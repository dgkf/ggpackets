#' A class to contain arguments for lazy evaluation of calls in a ggplot
#' construction
#'
#' @slot args a tibble containing columns 'name', 'val', and 'source' for
#'   tracking the provenance of call arguments, handling of multiple arguments
#'   and communicating shadowing of duplicate arguments.
#' @slot call the call to evaluate during plot construction
#' @slot geom the resulting geom of the call, stored upon binding to aid in
#'   argument handling
#' @slot stat the resulting stat object of the call, stored upon binding to aid
#'   in argument handling.
#' @slot auto_remove_aes whether to automatically remove aesthetic arguments
#'   that aren't relevant to the given geom and stat objects.
#'
#' @rdname call_df-class
#'
#' @import methods
#'
#' @export call_df
call_df <- setClass("call_df",
  slots = list(
    args = "tbl_df",
    call = "ANY",
    callname = "character",
    envir = "ANY",
    geom = "ANY",
    stat = "ANY",
    auto_remove_aes = "logical"
  ),
  prototype = list(
    args = tibble::tibble(name = list(), val = list(), source = list()),
    call = NULL,
    callname = NULL, 
    envir = NULL,
    geom = NULL,
    stat = NULL,
    auto_remove_aes = TRUE
  )
)


#' Initialize empty call_df
#' @param .Object call_df object to be initialized
#' @rdname initialize,call_df,list,character
setMethod(f = "initialize",
  methods::signature(.Object = "call_df"),
  function(.Object, arglist = list(), 
      sources = list(NULL), 
      call,
      envir = parent.frame(),
      auto_remove_aes = TRUE) { 
    
    if (length(sources) != length(arglist))
      if (length(sources) != 1) 
        stop('arg sources must be the same length of provided arg list or 1.')
      else sources = rep(sources, length(arglist))
    
    args <- tibble::tibble(
      name   = tsnames(arglist, '')               %||% list(),
      val    = unlist(arglist, use.names = FALSE) %||% list(),
      source = sources                            %||% list())
    
    args$name <- ifelse(args$val %in% '...', '...', args$name)
    .Object@args <- args
    
    if (!missing(call)) .Object <- bind_call(.Object, call)
    
    .Object@envir <- envir
    .Object@auto_remove_aes <- auto_remove_aes
    
    .Object
  }
)

#' call_df equivalence
#' @param e1 lhs
#' @param e2 rhs
#' @rdname call_df-equivalence
setMethod("==", c("call_df", "call_df"), function(e1, e2) {
  conditions <- list(
    (nrow(e1@args) == 0 && nrow(e2@args) == 0) || all(e1@args == e2@args),
    deparse(e1@call) == deparse(e2@call),
    e1@callname == e2@callname,
    identical(e1@envir, e2@envir),
    identical(e1@geom, e2@geom),
    identical(e1@stat, e2@stat),
    e1@auto_remove_aes == e2@auto_remove_aes
  )
  
  all(do.call(c, conditions))
})

#' Caste call_df to list of arguments, optionally filtering duplicates
#' @param x call_df object
#' @param duplicates whether to allow duplicate arguments in list
#' @rdname call_df-as.list
setMethod("as.list", "call_df", 
  function(x, duplicates = FALSE) {
    
    args <- with(x@args, setNames(val, name))
    args <- replace_reserved_aesthetic_references(args)
    args[last_args(names(args))]
    
    # evaluate reserved aesthetics 
    # e.g. list(color = a, fill = ..color..) => list(color = a, fill = a)
    args <- replace_reserved_aesthetic_references(args)
    
    # strip last and flatten args to mapping, remove extra aes
    args <- args[last_args(names(args))]
    args <- flatten_aes_to_mapping(
        args, 
        allowed_aesthetics(x@geom), 
        x@auto_remove_aes, x@envir)
    
    if (is.function(x@call) && !'mapping' %in% names(formals(x@call)))
      args <- args[!names(args) %in% 'mapping']
    
    # remove invalid argnames and unevaluated aesthetics for ggplot functions
    if (!(is.null(x@geom) || is.null(x@stat)) && x@auto_remove_aes) {
      args <- args[!uneval_aes(args) | names(args) %in% '']
      args <- filter_args(x@call, x@geom, x@stat, args)
    }
    
    # filter out remaining named unevaluated arguments
    args <- Map(function(v, k) {
      if (is_uneval(v) && k != '') eval(v, x@envir) else v
    }, args, tsnames(args, ''))
    
    args
  }
)

#' Show call_df contents
#' @param object call_df object
#' @rdname call_df-show
setMethod("show", "call_df",
  function(object) {
    if (!is.null(object@call)) {
      ctext <- ''
      if (!is.null(object@callname))
        ctext <- sprintf('%s (%s):\n', object@callname, class(object@call)[[1]])
      else
        ctext <- sprintf('%s:\n', class(object@call)[[1]])
      cat(safecrayon('cyan', ctext))
    }
    
    sources <- Map(function(s) {
      if (is.null(s)) '<undefined>' else s 
    }, object@args$source)
    
    if (nrow(object@args)) { 
      src_chr <- max(nchar(sources), 0)
      cat(paste0('  ', 
        Map(function(a, an, i, s) {
          # pretty print source info
          src <- paste0(safecrayon('grey4', s), safecrayon('grey7', 
            if ((pad <- src_chr - nchar(s) + 3) > 2) 
              paste0(' ', paste0(rep('.', pad-1), collapse = ''))
            else 
              paste0(rep(' ', pad), collapse = '')))
          
          # pretty print argument info
          if (is_uneval(a)) {
           out <- paste0(gsub('^\\s+|\\s+$', '', (deparse(a))), collapse = ' ')
           if (grepl('^\\.\\..+\\.\\.$', out)) 
             out <- paste0(
               '..',
               safecrayon('cyan', gsub('^\\.\\.(.+)\\.\\.$', '\\1', out)),
               '..',
               safecrayon('cyan', ' ^'))
          } else 
           out <- deparse(a)
          
          # color code argument overloading
          if (an == '' || i == tail(which(object@args$name %in% an), 1)) 
            col <- 'grey2'
          else 
            col <- 'grey6'
          
          # format unnamed arguments appropriately
          if (is.na(an) || an == '') 
           out <- safecrayon(col, an, out, collapse = '', sep = '')
          else 
           out <- safecrayon(col, an, '=', out, collapse = '')
          
          out <- paste(src, out)
          
          # limit to line length
          if (safenchar(out) > (w <- getOption('width', default=80)))
           out <- paste0(safesubstr(out, 0, w-3), safecrayon('grey6', '...'))
          
          paste0(out, '\n')
        }, 
        object@args$val, 
        object@args$name, 
        seq(length.out = nrow(object@args)),
        sources), 
      collapse = ''))
    } else {
      safecrayon('grey4', '  <empty>')
    }
  }
)

#' Indexing into argument tibble rows by vector
#' @param x call_df object to index into
#' @param i row index of element to select
#' @param j column index of element to select
#' @param subset criteria to subset resulting calldf by
#' @rdname call_df-indexing
setMethod("[", "call_df", function(x, i, j, subset = c('named', 'unnamed')) {
  subset <- match.arg(subset, c('named', 'unnamed'), several.ok = TRUE)
  
  if (missing(i)) i <- 1:nrow(x@args)
  if (is.numeric(i)) {
    x@args <- x@args[i,j]
  } else if (!is.logical(i)) {
    i <- c(i, to_ggplot(i))
    x@args <- x@args[x@args$name %in% i,j]
  }
  
  subset <- Reduce(c, Map(function(s) {
    switch(s, 
      named   = which(x@args$name != ''),
      unnamed = which(x@args$name == ''))
  }, subset))
  
  x@args <- x@args[subset,]
  
  x
})

#' Indexing into arguments by name
#' @rdname call_df-indexing
setMethod("[[", "call_df", function(x, i) {
  if (is.numeric(i)) return(x@args[i,'val'])
  i <- to_ggplot(i)
  x@args[[tail(which(x@args$name %in% i),1),'val']]
  x
})

#' Indexing into arguments by name
#' @param name index
#' @rdname call_df-indexing
setMethod("$", "call_df", function(x, name) {
  name <- c(name, to_ggplot(name))
  i <- tail(which(x@args$name %in% name),1)
  if (length(i)) x@args[[i, 'val']] 
  else NULL
})

#' Overload names method to print calldf argument names
#' @param x the calldf object to show
#' @param value the new values to pass to names of call_df
#' @rdname names-call_df
setMethod("names", "call_df", function(x) x@args$name)

#' Overload names method to print calldf argument names
#' @rdname names-call_df
setMethod("names<-", "call_df", function(x, value) { 
  x@args$name <- value
  x
})

#' Overload length method to retrieve number of arguments from calldf
#' @param x the calldf object to show
#' @rdname length-call_df
setMethod("length", "call_df", function(x) nrow(x@args))

#' Sum of two ggpacked layers to return ggpacket object
#' @rdname ggpacked_layer-addition
setMethod("+", c("call_df", "call_df"), function(e1, e2) { 
  e1@args <- rbind(
    e1[, subset = 'unnamed']@args,
    e2[, subset = 'unnamed']@args,
    e1[, subset = 'named']@args,
    e2[, subset = 'named']@args)
  e1
})



#' Bind a call to a calldf
#' 
#' If the call produces a \code{ggplot2} \code{ggproto} object, the \code{geom}
#' and \code{stat} of the \code{ggproto} are cached in the calldf object for use
#' in ggplot construction.
#' 
#' @param x the calldf object to modify
#' @param .call the call to bind to the calldf
#' @param name a character vector to use for the call name
#' @param envir the environment in which the call is to be executed
#' @param match_args whether unnamed arguments should be matched against named
#'   call formals
#' 
#' @return the calldf object with the necessary call data added
#' 
#' @importFrom stats setNames
#' 
bind_call <- function(x, .call, name, 
    envir = parent.frame(), match_args = FALSE) {
  
  args <- x@args
  ggproto_tmp <- with(args[args$name %in% c('geom', 'stat'),],
    build_proto(.call, stats::setNames(val, name) %||% list()))
  
  if (!missing(name)) 
    x@callname <- name
  
  x@call  <- .call
  x@envir <- envir
  x@geom  <- ggproto_tmp$geom 
  x@stat  <- ggproto_tmp$stat
  
  if (match_args) 
    x <- match_calldf(x)
  
  x
}



#' Build a minimal ggproto object from desired arguments
#'
#' @param .call the function to use for ggproto construction
#' @param args arguments to use when calling the function
#'
#' @return a minimal ggproto object using only the geom and stat arguments of
#'   args, safely returning an empty list with geom and stat values if an error
#'   is encountered
#'   
build_proto <- function(.call, args) { 
  ggp <- tryCatch(do.call(.call, args), error = function(e) NULL)
  
  if (!'ggproto' %in% class(ggp)) 
    ggp <- list(geom = NULL, stat = NULL)
  else 
    ggp
}



#' Append dataframe to another after row index
#'
#' @param x an call_df to be appended to
#' @param values the call_df, whose arguments will be inserted
#' @param after the rowindex at which to insert the dataframe
#'
#' @return a combined call_df with call_df \code{values}'s args inserted into
#'   call_df \code{x} after row \code{after}
#'   
append_calldf <- function(x, values, after = nrow(x)) {
  x@args <- rbind(
    x@args[row(x@args[,1]) <= after,], 
    values@args, 
    x@args[row(x@args[,1]) > after,])
  x
}



#' Remove calldf arguments by name
#' 
#' @param calldf the calldf object to modify
#' @param exclude a vector of argument names to remove from the calldf arugment
#'   tibble
#' 
#' @return the modified calldf
#' 
exclude_calldf_args <- function(calldf, exclude) { 
  if (!"call_df" %in% class(calldf)) 
    stop("only valid for object of class 'call_df'")
  
  calldf@args <- calldf@args[!calldf@args$name %in% exclude,]
  calldf
}



#' Substitute calldf named arguments with new argument tibble
#' 
#' @param calldf the calldf object to substitute
#' @param ... named arguments where the name specifies the argument to replace
#'   and the value is a calldf to insert in that argument's place
#' 
#' @return the modified calldf
#' 
substitute_calldf_args <- function(calldf, ...) { 
  args <- list(...)
  for (i in names(args)) {
    calldf_i <- call_df(args[[i]], sources = i)
    idxs_i <- which(names(calldf) %in% i)
    for (idx in idxs_i)
      calldf <- append_calldf(calldf[-idx], calldf_i, idx-1)
  }
  calldf
}



#' Match arguments in the calldf against the formals of the call function
#' 
#' @description a \code{match.call} equivalent for the calldf object
#' 
#' @param calldf the calldf object for which arguments should be matched against
#'   call formals
#'
#' @return a calldf object of the same structure, with arugment names provided
#'   for unnamed arguments.
#'
#' @examples 
#' # intended for internal ggpackets use only
#' 
#' my_calldf <- call_df(call = print, arglist = list("a"), source = 'ex')
#' my_calldf
#' #> function:
#' #>   ex .. "a"
#' 
#' my_calldf <- ggpackets:::match_calldf(my_calldf)
#' #> function:
#' #>   ex .. x = "a"
#' 
#' @importFrom stats setNames
#' 
match_calldf <- function(calldf) { 
  if (!"call_df" %in% class(calldf)) 
    stop("only valid for object of class 'call_df'")
  
  if (!is.function(calldf@call)) return(calldf)
  .call <- calldf@call
  
  # filter for unamed args and named args appropriate for function call
  call_formals <- names(formals(.call))
  call_arg_l <- last_args(names(calldf)) & names(calldf) %in% c('', call_formals)
  dummy_args <- with(calldf@args[call_arg_l %||% NULL,], {
    stats::setNames(as.list(rep(NA, length(name))), name)
  })
  
  # match args to call
  mcall <- do.call(call, c('.call', dummy_args))
  matched_args <- as.list(match.call(.call, mcall))[-1]
  
  # match new named args against previously unnamed args
  unmatched_args <- matched_args[!names(matched_args) %in% names(dummy_args)]
  unmatched_call_arg_l <- call_arg_l & names(calldf) %in% ''
  calldf@args[unmatched_call_arg_l,'name'] <- tsnames(unmatched_args, '')
  
  calldf
}



#' do.call equivalent for calldf objects
#' 
#' @param what the calldf object to evaluate
#' @param envir the environment in which to evaluate the call
#'
do_calldf <- function(what, envir = parent.frame()) { 
  if (!"call_df" %in% class(what)) 
    stop("only valid for object of class 'call_df'")
  do.call(what@call, as.list(what), envir = envir)
}



#' Filter argument list for only unnamed items and the last instance of each
#' named item
#'
#' @param args a character vector of values
#'
#' @return a filtered vector of values with only the last instance of non-empty
#'   strings as well as any empty strings ('') remaining
#'   
last_args <- function(args) args == '' | !duplicated(args, fromLast = TRUE)



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



#' Filter reserved aesthetics of the form ..<aes>.. from values
#'
#' @param args named argument list potentially containing a reserved value
#' @param re the regular expression to match deparsed symbols against
#'
#' @return the argument list with reserved values replaced with the value from
#'   the name of the element if one occurs before the reserved value or removed
#'   otherwise
#'   
replace_reserved_aesthetic_references <- function(args, 
    re = '^\\.\\.(.+)\\.\\.$') { 
  
  argsd <- Map(deparse, args)
  Filter(Negate(is.null), Map(function(arg, argd, argn, i) { 
    if (length(m <- regmatches(argd, regexec(re, argd))[[1]])) {
      repn <- unlist(to_ggplot(m[[2]]))
      repi <- which(names(args) %in% repn & !grepl(re, argsd))
      repi <- tail(repi[repi < i], 1)
      if (length(repi)) args[[repi]]
      else NULL
    } else arg
  }, args, argsd, tsnames(args, fill = ''), seq(length.out = length(args))))
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