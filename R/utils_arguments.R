#' Expand dot arguments into named arguments
#'
#' @param expr An expression whose arguments need to be expanded.
#' @param envir An environment in which to evaluate expansion.
#'
expand_dots <- function(expr, envir = parent.frame(2L)) {
  f <- expr[[1]]
  fobj <- eval(f, envir = envir)
  if (!is.function(fobj) || is.primitive(fobj)) return(expr)
  fargs <- as.list(expr)[-1]

  # evaluate data argument greedily
  fargs$data <- eval(fargs$data, envir = envir)

  # replace dots with ellipsis args in envir
  dots <- eval(quote(substitute(...())), envir = envir)
  dots_idx <- utils::tail(which(fargs == quote(...)), 1)
  if (length(dots_idx)) {
    fargs <- append(fargs, dots, after = dots_idx)
    fargs <- fargs[fargs != quote(...)]
  }

  fargs <- collapse_mappings(fargs)
  fargs <- collapse_data(fargs)
  fargs <- deduplicate_params(fargs)
  fargs <- match_unnamed_args(fobj, fargs, envir = envir)

  as.call(append(f, fargs))
}



#' Filter a named list by ids
#'
#' Filter a named list (often an aesthetic mapping or argument list) for only
#' unprefixed elements, elements which don't match any of \code{all_ids} and 
#' those which match one of \code{call_ids}. For those that match, remove the
#' id prefix.
#'
#' @param x A named list to filter.
#' @param call_ids A character vector of valid ids.
#' @param all_ids A character vector of all possible ids.
#'
filter_by_ggcall_ids <- function(x, call_ids, all_ids) {
  if (is.null(x)) return(x)

  in_xnames <- names(x)
  names(x) <- gsub(
    sprintf("^(%s)\\.(.+)", paste0(call_ids, collapse = "|")), 
    "\\2", 
    names(x))

  x_name_matches <- matrix(
    apply(as.matrix(all_ids), 1L, function(id, xnames) {
        grepl(sprintf("^%s\\..+", id), xnames)
      }, names(x)), 
    nrow = length(x), 
    ncol = length(all_ids), 
    dimnames = list(names(x), all_ids))

  x <- x[!apply(x_name_matches, 1L, any)]

  # prefer arguments that have been explicitly prefixed with an id
  is_prefixed <- !in_xnames %in% names(x) 
  x[names(is_prefixed)] <- x[is_prefixed]

  x[!duplicated(names(x), fromLast = TRUE)]
}



#' Collapse aesthetic mappings arguments
#'
#' Reduce aesthetic mapping arguments using \code{update_mapping}
#'
#' @param args A list of arguments. If multiple elements are named "mapping"
#' they will be collapsed into a single mapping argument, appropriately
#' updating aesthetic mappings.
#'
collapse_mappings <- function(args) { 
  aes_args <- unname(args[names(args) %in% "mapping"])
  if (!length(aes_args)) return(args)
  args <- args[!names(args) %in% "mapping"]
  args$mapping <- do.call(update_mapping, aes_args)
  args
}



#' Collapse data arguments
#'
#' Reduce data arguments using \code{update_data}
#'
#' @param args A list of arguments. If multiple elements are named "data"
#' they will be collapsed into a single mapping argument, appropriately
#' updating datasets and applying functions as needed.
#'
collapse_data <- function(args) { 
  data_args <- unname(args[names(args) %in% "data"])
  if (!length(data_args)) return(args)
  args <- args[!names(args) %in% "data"]
  args$data <- do.call(update_data, data_args)
  args
}



#' Match unnamed arguments
#'
#' Similar to match.call, but without evaluating arguments, avoiding possible
#' syntactic errors that would arise due to ggpackets-specific syntax.
#'
#' @param f A function to match arguments against.
#' @param args A list of arguments to match.
#' @param envir An environment in which the function should be matched.
#'
match_unnamed_args <- function(f, args, envir = parent.frame()) {
  if (is.primitive(f)) return(args)

  # only match unnamed, possibly redundaant args
  idx_unnamed <- is.null(names(args)) | names(args) == ""
  matched_args <- match.call(
    f, 
    as.call(append(f, args[idx_unnamed])), 
    expand.dots = TRUE, 
    envir = envir)[-1]
  names(args)[idx_unnamed] <- names(matched_args)
  args
}



#' Mimic ggplot2 behavior of intelligently interpretting first layer argument
#'
#' Like ggplot, if the first argument doesn't appear to be an aesthetic
#' mapping, despite mappings being appropriately passed in the first argument,
#' swap the first two arguments.
#'
#' @param args a list of arguments to interpet
#'
smart_swap_mapping_data <- function(args) {
  if ("mapping" %in% names(args) && !inherits(args$mapping, "uneval")) {
    arg_mapping_in <- args$mapping
    args$mapping <- args$data
    args$data <- arg_mapping_in
  }
  args
}



#' Remove arguments with duplciated names
#'
#' @param args A list of arguments to deduplicate.
#'
deduplicate_params <- function(args) {
  if (is.null(names(args))) names(args) <- rep("", length(args))
  args[!duplicated(names(args), fromLast = TRUE)]
}



#' Filter for only arguments that can be accepted by a given function
#'
#' @param f A function to filter on.
#' @param args A list of arguments to filter.
#'
only_formals_and_dots <- function(f, args) {
  if ("..." %in% names(formals(f))) return(args)
  args[names(args) %in% names(formals(f))]
}



#' Evaluate an expression, ignoring warnings about unknown parameters
#'
#' @param expr An expression to evaluate.
#' @param envir An environment to evaluate the given expression in.
#'
with_ignore_unknown_params <- function(expr, envir = parent.frame()) {
  withCallingHandlers({
    eval(expr, envir = envir)
  }, warning = function(w) {
    if (grepl("^Ignoring unknown (parameters|aesthetics):", w$message))
      invokeRestart("muffleWarning")
  })
}
