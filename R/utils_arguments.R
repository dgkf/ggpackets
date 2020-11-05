expand_dots <- function(expr, envir = parent.frame(2L)) {
  f <- expr[[1]]
  fobj <- eval(f, envir = envir)
  if (!is.function(fobj) || is.primitive(fobj)) return(expr)
  fargs <- as.list(expr)[-1]

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

filter_by_ggcall_ids <- function(x, call_ids, all_ids) {
  if (is.null(x)) return(x)

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

  x[!apply(x_name_matches, 1L, any)]
}

collapse_mappings <- function(args) { 
  aes_args <- unname(args[names(args) %in% "mapping"])
  if (!length(aes_args)) return(args)
  args <- args[!names(args) %in% "mapping"]
  args$mapping <- do.call(update_mapping, aes_args)
  args
}

collapse_data <- function(args) { 
  data_args <- unname(args[names(args) %in% "data"])
  if (!length(data_args)) return(args)
  args <- args[!names(args) %in% "data"]
  args$data <- do.call(update_data, data_args)
  args
}

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

smart_swap_mapping_data <- function(args) {
  if ("mapping" %in% names(args) && !inherits(args$mapping, "uneval")) {
    arg_mapping_in <- args$mapping
    args$mapping <- args$data
    args$data <- arg_mapping_in
  }
  args
}

deduplicate_params <- function(args) {
  if (is.null(names(args))) names(args) <- rep("", length(args))
  args[!duplicated(names(args), fromLast = TRUE)]
}

only_formals_and_dots <- function(f, args) {
  if ("..." %in% names(formals(f))) return(args)
  args[names(args) %in% names(formals(f))]
}

with_ignore_unknown_params <- function(expr, envir = parent.frame()) {
  withCallingHandlers({
    eval(expr, envir = envir)
  }, warning = function(w) {
    if (grepl("^Ignoring unknown parameters:", w$message))
      invokeRestart("muffleWarning")
  })
}
