expand_dots <- function(expr, envir = parent.frame(2L)) {
  f <- expr[[1]]
  fobj <- eval(f, envir = envir)
  if (!is.function(fobj) || is.primitive(fobj)) return(expr)
  fargs <- as.list(expr)[-1]
  dots <- eval(quote(substitute(...())), envir = envir)
  dots_idx <- utils::tail(which(fargs == quote(...)), 1)
  if (!length(dots_idx)) return(expr)
  fargs <- append(fargs, dots, after = dots_idx)
  fargs <- fargs[fargs != quote(...)]
  match.call(fobj, as.call(append(f, fargs)), envir = envir)
}

filter_by_ggcall_ids <- function(args, call_ids, all_ids) {
  names(args) <- gsub(
    sprintf("^(%s)\\.(.+)", 
    paste0(call_ids, collapse = "|")), "\\2", 
    names(args))
  
  argmatches <- matrix(
    apply(as.matrix(all_ids), 1L, function(id, argnames) {
        grepl(sprintf("^%s\\..+", id), argnames)
      }, names(args)), 
    nrow = length(args), 
    ncol = length(all_ids), 
    dimnames = list(names(args), all_ids))

  args[!apply(argmatches, 1L, any)]
}

smart_swap_mapping_data <- function(args) {
  if ("mapping" %in% names(args) && !inherits(args$mapping, "uneval")) {
    arg_mapping_in <- args$mapping
    args$mapping <- args$data
    args$data <- arg_mapping_in
  }

  args
}

deduplicate_params <- function(args) {
  args[!duplicated(names(args), fromLast = TRUE)]
}

with_ignore_unknown_params <- function(expr, envir = parent.frame()) {
  withCallingHandlers({
    eval(expr, envir = envir)
  }, warning = function(w) {
    if (grepl("^Ignoring unknown parameters:", w$message))
      invokeRestart("muffleWarning")
  })
}
