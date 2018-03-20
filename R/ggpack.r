#' Handling of argument passing to ggplots
#'
#' A helper function to wrap ggplot calls, allowing for passing of a shared list
#' of arguments prefixed by an id string. For example, a call to a custom ggplot
#' template may allow for arguments 'line.color' and 'bar.color' to specify the
#' line and bar colors separately. These arguments are parsed appropriately and
#' passed to the appropriate sub-function.
#'
#' @param .call the \code{\link[ggplot2]{ggplot}} geom function to invoke
#' @param ... additional arguments to use when calling the provided function
#' @param id the prefix string to subset arguments by
#' @param dots arguments to subset
#' @param null_empty logical indicating whether \code{NULL} should be returned
#'   if no arguments are passed to call
#' @param auto_remove_aes logical indicating whether aesthetic mappings that
#' @param envir frame in which non-aesthetic arguments should be evaluated
#'
#' @return a call to the specified function with arguments subset for only those
#'   which match the specified prefix
#'
#' @note Ellipses arguments and the arguments passed through the `dots` args
#'   will overwrite previous arguments of the same name.
#'
#' @examples
#' library(ggplot2)
#'
#' # wrap layer calls in a ggpack to package them
#' ggpack(geom_bar(na.rm = TRUE))
#'
#' # or more explicitly define layer properties
#' ggpack(geom_bar,              # layer function to bundle
#'   id = 'bar',                 # name of layer - used for filter listed args
#'   fill = 'red',               # ...'s before 'args' used as defaults
#'   dots = list(fill = 'blue'), # named 'args' param will overwrite prior ...'s
#'   fill = 'green')             # ...'s after 'args' will overwrite any others
#'
#' @importFrom utils head tail
#' @importFrom tibble tibble
#'
#' @export
ggpack <- function(.call = NULL, ..., id = NULL, dots = NULL, 
     # warn = '...', (parameter relinquished to ... arg to allow multiple args)
     auto_remove_aes = is.null(id) || any(sapply(id, is.null)),
     null_empty = FALSE, envir = parent.frame()) {
  
  ## This function's principle purpose is to preprocess the arguments being
  ## passed to the ggplot call and store them for easier construction when the
  ## plot is being built. Outwardly, it's meant to appear like a ggplot call,
  ## but in actuality it only serves as an API to the ggpacked_layer constructor
  
  if (is.null(.call) && length(list(...)) == 0) return(ggpacket())
  
  expand <- list('...' = NULL, 'dots' = NULL)
  callname <- deparse(as.list(match.call())$'.call')
  excluded_args <- setdiff(names(formals()), names(expand))
  
  # get all call-specific args
  args <- as.list(sys.call()[-1])[-1]
  calldf <- call_df(args, source = 'call', auto_remove_aes)
  calldf <- exclude_calldf_args(calldf, excluded_args)
  
  # filter args passed as ellipses in parent frame from all ellipses arguments
  e_1 <- calldf@args[!names(calldf) %in% names(expand),]$val # parent ... args
  e_all <- safedequos(...)                                   # parent & grandparent+ ... args
  
  # prep lists of arguments that need to be added in
  expand$...  <- list_diff(e_1, e_all)                       # grandparent+ ... args
  expand$dots <- as.list(dots)
  expand <- lapply(expand, remove_by_prefix, id = id)
  
  # substitute ellipses args and dots args in place
  calldf <- do.call(substitute_calldf_args, c(calldf, expand))
  calldf <- bind_call(calldf, .call, callname, envir, match_args = TRUE)
  
  # unpack mapping aesthetics
  for (rows_from_end in length(calldf) - which(names(calldf) %in% 'mapping')) { 
    i <- length(calldf) - rows_from_end
    mapping <- eval(calldf@args[[i,'val']])
    if ('uneval' %in% class(mapping))
      arg_mapping <- call_df(mapping, paste(calldf@args[i,'source'], 'mapping'))
    else 
      arg_mapping <- calldf@args[i,]
    calldf <- append_calldf(calldf[-i,], arg_mapping, i-1)
  }
  
  names(calldf) <- to_ggplot(names(calldf))
  ggpacket() + ggpacked_layer(id, calldf, null_empty)
}



#' Capture all ellipses arguments using rlang::quos() with substitute() fallback
#'
#' @param ... arguments from a parent function to extract
#'
#' @return either an rlang list of quosures if rlang is installed or a named
#'   list of values otherwise
#'   
safedequos <- function(...) { 
  if (requireNamespace('rlang', quietly = TRUE)) {
    dequos(rlang::quos(...))
  } else 
    as.list(substitute(...()))
}



#' Attempt to identify which values were added in the transition from a to b
#'
#' @note This can cause issues if ambiguous terms are in b, making it impossible
#'   to distinguish which arguments were added. For example:
#'   
#'   \code{a = c('a', 'b', 'b', 'b', 'a')}
#'   \code{b = c('a', 'b', 'b', 'c', 'b', 'b', 'a')}
#'   
#'   makes it impossible to know whether c('b', 'c') was inserted after position
#'   2, or whether c('c', 'b') was inserted after position 3.
#'   
#' @param a a list of values
#' @param b another list of values
#'
#' @return the values that were added to a to arrive at b
#'   
list_diff <- function(a, b) { 
  # This function could possibly cause issues with regions of a list that have
  # ambiguity of arguments. Those situations are fairly rare, though can occur.
  
  if (!length(a)) return(b)
  if (length(a) > length(b)) return(list_diff(b, a))
  bm <- b[1:length(a)]
  
  # find first location of a difference in the list
  fstdiff <- min(head(which(
    unlist(Map(Negate(identical), a, bm)) | 
    tsnames(a) != tsnames(bm)), 1), length(a)+1)
  
  # put my thing down flip it and reverse it (and take a list_diff on that)
  rev(list_diff(
    rev(a[seq(length(a)) >= fstdiff]), 
    rev(b[seq(length(b)) >= fstdiff])))
}



#' Filter arguments based on a prefix id
#'
#' @param id a string specifying the prefix to be used to filter args
#' @param args a list of arguments to be subset by the prefix match
#' @param sep a regex joining string between prefix and args. 
#' Defaults to \code{"\\\\."}.
#'
#' @return a list of arguments that originally were prefaced by the
#' specified prefix, now with that prefix removed.
#'
#' @importFrom stats setNames
#' 
#' @export
remove_by_prefix <- function(id, args, sep = '\\.') {
  if (is.null(args)) return(list())
  
  # args same as id e.g. "xlab" in ggpack(xlab, id = 'xlab', xlab = 'x axis')
  id_idx <- which(names(args) %in% id)
  names(args)[id_idx] <- ''
  
  # args begin with id e.g. "xlab.label" in ggpack(xlab, id = 'xlab', xlab.label = 'x axis')
  ided_idx <- grep(paste0('^',id,sep, collapse = '|'), names(args))
  names(args)[ided_idx] <- gsub(paste0('^(',paste0(id,collapse='|'),')',sep,'(.*)$'), 
                                '\\2', names(args)[ided_idx])
  
  if (is.null(id) || 'NULL' %in% id) return(args)
  args[sort(union(id_idx, ided_idx))]
}