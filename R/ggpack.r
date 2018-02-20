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
#' @param warn a character vector of argument names that should yield a warning
#'   when parameters passed via that argument are overwritten. Can include
#'   values \code{'...'} and \code{'dots'} or \code{'call'} (to warn for
#'   arguments explicitly defined during call construction being overwritten).
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
  callname <- as.list(match.call())$'.call'
  excluded_args <- setdiff(names(formals()), names(expand))
  
  # get all call-specific args
  args <- as.list(sys.call()[-1])[-1]
  args <- tibble(name = tsnames(args, ''), val = args, source = 'call')
  args$name <- ifelse(args$val %in% '...', '...', args$name)
  args <- args[!args$name %in% excluded_args,]
  args <- match_argdf(args, .call)
  
  # filter args passed as ellipses in parent frame from all ellipses arguments
  e_1 <- args[!args$name %in% names(expand),]$val  # parent ellipses args
  e_all <- safedequos(...)                         # parent + grandparent+
  
  # prep lists of arguments that need to be added in
  expand$'...' <- list_diff(e_1, e_all)            # grandparent+ ellipses args
  expand$dots  <- as.list(dots)
  expand <- lapply(expand, remove_by_prefix, id = id)
  
  # substitute ellipses args and dots args in place
  for (rows_from_end in nrow(args) - which(args$name %in% names(expand))) { 
    i <- nrow(args) - rows_from_end
    e <- expand[[ a <- args[[i,'name']] ]]
    e_df <- match_argdf(tibble(val=e, name=names(e), source=a), .call)
    args <- append_df(args[-i,], e_df, i-1)
  }
  
  # unpack mapping aesthetics
  for (rows_from_end in nrow(args) - which(args$name %in% 'mapping')) { 
    i <- nrow(args) - rows_from_end
    mapping <- eval(args[[i,'val']])
    if ('uneval' %in% class(mapping))
      arg_mapping <- tibble(val = mapping, name = names(mapping), 
          source = paste(args[i,'source'], 'mapping'))
    else 
      arg_mapping <- args[i,]
    args <- append_df(args[-i,], arg_mapping, i-1)
  }
  
  # call ggproto construction with stripped args to determine geom
  # might cause issue if aesthetics are dependent on additional arguments
  ggproto_tmp <- with(args[args$name %in% c('geom', 'stat'),], 
      build_proto(.call, setNames(val, name)))
  
  args$name <- to_ggplot(args$name)
  ggpacket() + ggpacked_layer(
    id = id, ggcall = .call, ggargs = args, 
    geom = ggproto_tmp$geom, stat = ggproto_tmp$stat, 
    ggpackargs = list(null_empty = null_empty, auto_remove_aes = auto_remove_aes, 
      callname = callname, envir = envir))
}

match_argdf <- function(argdf, .call) { 
  if (!is.function(.call)) return(argdf)
  
  # filter for unamed args and named args appropriate for function call
  call_formals <- names(formals(.call))
  call_arg_l <- last_args(argdf$name) & argdf$name %in% c('', call_formals)
  dummy_args <- with(argdf[call_arg_l,], setNames(as.list(rep(NA, length(name))), name))
  
  # match args to call
  mcall <- do.call(call, c('.call', dummy_args))
  matched_args <- as.list(match.call(.call, mcall))[-1]
  
  # match new named args against previously unnamed args
  unmatched_args <- matched_args[!names(matched_args) %in% names(dummy_args)]
  unmatched_call_arg_l <- call_arg_l & argdf$name %in% ''
  argdf[unmatched_call_arg_l,'name'] <- tsnames(unmatched_args, '')
  
  argdf
}

build_proto <- function(.call, args) { 
  ggp <- tryCatch(do.call(.call, args), error = function(e) NULL)
  
  if (!'ggproto' %in% class(ggp)) 
    ggp <- list(geom = NULL, stat = NULL)
  else 
    ggp
}

  
append_df <- function(x, values, after = nrow(x)) {
  rbind(x[row(x[,1]) <= after,], values, x[row(x[,1]) > after,])
}


safedequos <- function(...) { 
  if (requireNamespace('rlang', quietly = TRUE)) 
    dequos(rlang::quos(...))
  else 
    as.list(substitute(...()))
}


#' Retrieve last named 'warn' value from list with default and restricted values
#'
#' @param args named list of values, potentially containing a named element
#'   'warn'
#' @param warn.default the default value if 'warn' is not found
#' @param warn.opts the options for possible values within the 'warn' element
#'
#' @return the values retrieved from the 'warn' named element in args
#' 
warn_arg <- function(args, warn.default = '...', 
    warn.opts = c('...', 'dots', 'call')) {
  
  warn <- unlist(tail(args[names(args) %in% 'warn'], 1), use.names = FALSE)
  if (is.null(warn)) warn <- warn.default
  
  if (isTRUE(warn)) 
    warn.opts
  else if (!warn %in% list(NULL, FALSE)) 
    match.arg(warn, warn.opts, several.ok = TRUE)
}


#' Filter argument list for only unnamed items and the last instance of each
#' named item
#'
#' @param args a list of arguments to filter
#' @param sources an optional list of names used to indicate the origin of each
#'   of the arguments
#' @param warn an optional character vector of sources for which a warning
#'   should be produced, should an argument from that source be overwritten
#' @param desc an optional named character vector of more verbose descriptions
#'   to use for each of the sources. (e.g. \code{c("..." = "ellipses
#'   arguments")})
#' @param call an optional object of type call to include with the warning if a
#'   statement associated with the arguments is desired to be included with the
#'   warning message.
#'
#' @return a named list of arguments with duplicated names omitted, leaving only
#'   the last occurence of each named value
#'   
last_args <- function(args) args == '' | !duplicated(args, fromLast = TRUE)


list_diff <- function(a, b) { 
  if (length(a) > length(b)) return(list_diff(b, a))
  anh <- `length<-`(tsnames(a), length(b))
  ant <- rev(`length<-`(rev(tsnames(a)), length(b)))
  bn <- names(b)
  if (is.null(bn)) b
  else b[as.logical(pmin(anh != bn, ant != bn, 1, na.rm = TRUE))]
}

#' Move items in args to 'mapping' key if found in aes_list
#'
#' @param args a list of arguments to parse for aesthetic arguments
#' @param aes_list a list of aesthetic names to consider
#' @param filter_mapping whether mapping key should be filtered for only those
#'   in the aes_list terms
#' @param envir environment in which to attempt evaluation of mapping term
#'
#' @importFrom utils tail modifyList
#' @importFrom ggplot2 aes_string
#'
#' @return a list of arguments with aesthetics matching values in
#'   \code{aes_list} flattened into a list stored within the mapping value
#'   
flatten_aes_to_mapping <- function(args, 
    aes_list = add_eqv_aes(.all_aesthetics), 
    filter_mapping = FALSE, envir = parent.frame()) { 
  
  # get 'mapping' arg position and any args position whose name in aes_list
  map_idx <- utils::tail(which(names(args) == 'mapping'), 1) %||% Inf
  aes_idx <- which(names(args) %in% aes_list)
  
  # split args into those pre- and post- mapping arugment
  aes_pre_idx <- aes_idx[aes_idx < map_idx]
  aes_pre_args <- Map(deparse, Filter(is_uneval, args[aes_pre_idx]))
  aes_pst_idx <- aes_idx[aes_idx > map_idx]
  aes_pst_args <- Map(deparse, Filter(is_uneval, args[aes_pst_idx]))
  
  # strip preceeding free aes vars if they appear in mapping
  mapped_aes <- if (is_uneval(args$mapping)) names(eval(args$mapping, envir))
                else names(args$mapping)
  mapped_aes <- add_eqv_aes(mapped_aes)
  preceeding_aes <- which(names(head(args, map_idx)) %in% mapped_aes)
  if (length(preceeding_aes)) args <- args[-preceeding_aes]
  
  # build mapping from unevaluated args before and after mapping param
  if (!is.null(names(args)))
    args <- args[!names(args) %in% names(c(aes_pre_args, aes_pst_args))]
  
  if (filter_mapping) mapped_aes <- mapped_aes[mapped_aes %in% aes_list]
  mapping <- Reduce(utils::modifyList, list(
    do.call(ggplot2::aes_string, aes_pre_args),
    as.list(eval(args$mapping))[mapped_aes],
    do.call(ggplot2::aes_string, aes_pst_args)
  )) %||% NULL
  
  if (length(mapping))
    args <- append(
      args[!names(args) %in% 'mapping'], 
      list(mapping = structure(mapping, class = 'uneval')), 0)
  
  args
}


replace_reserved_aesthetic_references <- function(args, re = '^\\.\\.(.+)\\.\\.$') { 
  Filter(Negate(is.null), Map(function(arg, argn, i) { 
    argd <- deparse(arg)
    if (length(m <- regmatches(argd, regexec(re, argd))[[1]])) {
      repn <- unlist(to_ggplot(m[[2]]))
      repi <- which(names(args) %in% repn)
      repi <- tail(repi[repi < i], 1)
      if (length(repi)) args[[repi]]
      else NULL
    } else arg
  }, args, tsnames(args, fill = ''), seq(length.out = length(args))))
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


#' Indices of unevaluated aesthetics not in aes_list
#'
#' @param args a list of arguments which may contain unevaluated values
#' @param expr a regular expression exclude unevaluated values with. default is
#'   NULL and will remove all unevaluated values that match aesthetic names
#' @param aes_list a list of aesthetic names to consider for removal
#'
#' @return a logical vector, with unevaluated values set as TRUE if the regex
#'   match matches an aesthetic name from aes_list, otherwise FALSE
#'   
invalid_uneval_aes_match <- function(args, expr = NULL, 
    aes_list = add_eqv_aes(.all_aesthetics)) { 
  
  excluded_args <- Map(function(k, v) {
    if (is.null(expr)) m <- k
    else m <- regmatches(k, regexpr(expr, k, perl = TRUE))
    length(m) && is_uneval(v) && m %in% aes_list
  }, names(args), args)
  as.logical(unlist(excluded_args))
}


#' Indices of any unevaluated aesthetics with a prefix
#'
#' @description A prefix comes in the form 'prefix.aesthetic_name'
#'
#' @param args a list of arguments which may contain unevaluated values
#' @param aes_list a list of aesthetic names to consider for removal
#'
#' @return a logical vector, with unevaluated values set as TRUE if aesthetic
#'   name from aes_list, otherwise FALSE
#'   
named_uneval_aes <- function(args, aes_list = add_eqv_aes(.all_aesthetics)) {
  invalid_uneval_aes_match(args, expr = '(?<=\\.)[^.]+$', aes_list)
}


#' Indices of any unevaluated aesthetics with no prefix
#'
#' @param args a list of arguments which may contain unevaluated values
#' @param aes_list a list of aesthetic names to consider for removal
#'
#' @return a logical vector, with unevaluated values set as TRUE if aesthetic
#'   name from aes_list, otherwise FALSE
#'   
unnamed_uneval_aes <- function(args, aes_list = add_eqv_aes(.all_aesthetics)) {
  invalid_uneval_aes_match(args, expr = '^[^.]+$', aes_list)
}


#' Indices of any unevaluated aesthetics with with or without prefix
#'
#' @param args a list of arguments which may contain unevaluated values
#' @param aes_list a list of aesthetic names to consider for removal
#'
#' @return a logical vector, with unevaluated values set as TRUE if aesthetic
#'   name from aes_list, otherwise FALSE
#'   
uneval_aes <- function(args, aes_list = add_eqv_aes(.all_aesthetics)) {
  invalid_uneval_aes_match(args, expr = '((?<=\\.)[^.]+$|^[^.]+$)', aes_list)
}