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
#' @param null_empty return \code{NULL} if no arguments are received
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
#' @importFrom utils modifyList head tail
#' @export
ggpack <- function(.call = NULL, ..., id = NULL, dots = NULL, warn = '...', 
    auto_remove_aes = (!is.null(id) && any(sapply(id, is.null))),
    null_empty = FALSE, envir = parent.frame()) {
  
  if (is.null(.call) && length(list(...)) == 0) return(ggpacket())
  
  warn <- match.arg(warn, c('call', '...', 'dots'), several.ok = TRUE)
  callfname <- deparse(as.list(match.call())$'.call')
  expand <- list('...' = NULL, 'dots' = as.list(dots))
  
  # get all call-specific args
  args <- as.list(sys.call()[-1])[-1]
  names(args)[args %in% '...'] <- rep('...', length(args[args %in% '...']))
  excluded_args <- setdiff(names(formals()), names(expand))
  if (!is.null(names(args))) args <- args[!names(args) %in% excluded_args]
  
  # filter args passed as ellipses in parent frame from all ellipses arguments
  e_1 <- args[!names(args) %in% names(expand)]      # parent ellipses args
  e_all <- if (requireNamespace('rlang', quietly = TRUE)) dequos(rlang::quos(...))
    else substitute(...())                          # all ellipses args
  expand$'...' <- if (is.null(names(e_all))) e_all  # all except parent
    else e_all[!(names(e_all) %in% names(e_1) & !duplicated(names(e_all)))]
  
  # remove any arguments from ellipses or dots that don't begin with id
  expand <- lapply(expand, remove_by_prefix, id = id)
  
  # substitute ellipses args and dots args in place, record origin and indices
  sources <- rep('call', length(args))
  for (i in length(args) - which(names(args) %in% names(expand))) {
    i <- length(args) - i
    args <- append(args[-i], e <- expand[[a <- names(args[i])]], i-1)
    sources <- append(sources[-i], rep(a, length(e)), i-1)
  }
  
  # strip args list of duplicate args, optionally warning during removal
  args <- rename_to_ggplot(args)
  args <- last_args(args, sources, warn, call = sys.call(),
    desc = list('...' = '"..."', dots = '"dots"', call = 'call construction'))
  if (null_empty && length(args) == 0) return(ggpacket(NULL))
  
  # call ggproto construction with stripped args to determine geom
  # might cause issue if aesthetics are dependent on additional arguments
  ggproto_tmp <- tryCatch({
      do.call(.call, args[names(args) %in% c('geom', 'stat')])
    }, error = function(e) NULL)
  geom <- if ('ggproto' %in% class(ggproto_tmp)) ggproto_tmp$geom else NULL
  stat <- if ('ggproto' %in% class(ggproto_tmp)) ggproto_tmp$stat else NULL
  
  # flatten args to mapping, remove extra aes
  args <- flatten_aes_to_mapping(args, allowed_aesthetics(geom), auto_remove_aes, envir)
  
  if (auto_remove_aes) # remove invalid argnames and unevaluated aesthetics
    args <- filter_args(callfname, geom, stat, args[!uneval_aes(args)])
  args <- Map(function(v) if (is_uneval(v)) eval(v, envir) else v, args)
  
  # ring ring, time to make a call!
  ggpacket_name <- if (is.null(id)) NULL else paste(id, collapse = '.')
  if (any(class(.call) == 'function'))
    ggpacket(tryCatch(withCallingHandlers(do.call(.call, args),
      warning = function(w) if (!auto_remove_aes) {
        message("In ggpack of call to ", callfname, ": \n", w$message, "\n")
        invokeRestart("muffleWarning")
      }),
      error = function(e) {
        message(sprintf('Error during call to "%s" with id%s %s: \n',
          callfname,
          if (length(id) > 1) 's' else '',
          paste0('"', id %||% 'NULL', '"', collapse = ', ')),
          e$message, '\n')
      }), ggpacket_name)
  else
    ggpacket(.call, ggpacket_name)
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
#'   statment associated with the arguments is desired to be included with the
#'   warning message.
#'
#' @return a named list of arguments with duplicated names omitted, leaving only
#'   the last occurance of each named value
#'   
last_args <- function(args, sources = list(), warn = c(), desc = c(), 
    call = NULL) {
  
  if (is.null(names(args))) args
  desc <- as.list(desc)
  
  # provide warning for overriden arguments
  dup_args <- unique(names(args[duplicated(names(args))]))
  dup_indx <- Map(function(a) which(names(args) %in% a), dup_args)
  msg <- Filter(Negate(is.null), Map(function(arg, i, arg_src = sources[i]) {
    if (any(arg_src[-length(arg_src)] %in% warn)) { 
      arg_src <- Map(function(i) desc[[i]] %||% i, arg_src)
      sprintf('argument "%s" defined within %s overwritten by last definition in %s', 
        arg, str_format_list(unique(arg_src[-length(arg_src)])),
        arg_src[length(arg_src)])
    }
  }, dup_args, dup_indx))

  # print warning if a warning message was produced
  if (length(msg)) {
    msg <- paste0(1:length(msg), '. ', msg)
    if (!is.null(call))
      msg <- c('', paste0(deparse(call), collapse = ''), '', msg)
    warning(call. = FALSE, immediate. = TRUE,
      'During ggpack call, some arguments were overwritten: \n',
      paste0(strwrap(msg, indent = 2, exdent = 5, width = getOption('width')), 
        collapse = '\n'), '\n\n')
  }
  
  args[names(args) == '' | !duplicated(names(args), fromLast = TRUE)]
}


#' Move items in args to 'mapping' key if found in aes_list
#'
#' @param args a list of arguments to parse for aesthetic arguments
#' @param aes_list a list of aesthetic names to consider
#' @param filter_mapping whether mapping key should be filtered for only those
#'   in the aes_list terms
#' @param envir environment in which to attempt evaluation of mapping term
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
  aes_pre_idx <- Filter(function(i) i < map_idx, aes_idx)
  aes_pre_args <- Map(deparse, Filter(is_uneval, args[aes_pre_idx]))
  aes_pst_idx <- Filter(function(i) i > map_idx, aes_idx)
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
  args$mapping <- Reduce(modifyList, list(
    do.call(ggplot2::aes_string, aes_pre_args),
    as.list(eval(args$mapping))[mapped_aes],
    do.call(ggplot2::aes_string, aes_pst_args)
  )) %||% NULL
  
  args
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