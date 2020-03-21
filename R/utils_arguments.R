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

filter_by_ggcall_id <- function(args, id, ids) {
  names(args) <- gsub(sprintf("^%s\\.(.+)", id), "\\1", names(args))
  argmatches <- as.matrix(apply(as.matrix(ids), 1L, function(id, names) {
    grepl(sprintf("^%s\\..+", id), names)
  }, names(args)))
  args[!apply(argmatches, 1L, any)]
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
