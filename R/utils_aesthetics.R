.all_aesthetics <- tryCatch({
    # attempt to stay current with ggplot .all_aesthetics upstream
    get('.all_aesthetics', asNamespace('ggplot2'), inherits = FALSE)
  }, error = function(e) {
    # hard coded fallback in case upstream changes private variable name
    # #est for fallback viability included in testthat tests
    c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", "fg", 
      "fill", "group", "hjust", "label", "linetype", "lower", "lty", "lwd", 
      "max", "middle", "min", "pch", "radius", "sample", "shape", "size", "srt", 
      "upper", "vjust", "weight", "width", "x", "xend", "xmax", "xmin", 
      "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")
  })

handle_reset_mapping <- function(mapping) {
  mapping[!vapply(mapping, function(ai) {
    rlang::is_quosure(ai) && rlang::quo_get_expr(ai) == quote(..reset..)
  }, logical(1L))]
}

substitute_ggcall_dot_aes <- function(mapping, ggcall, envir = parent.frame()) {
  aess <- .all_aesthetics
  names(aess) <- ggplot2::standardise_aes_names(aess)

  # add in mappings for alternative naming conventions before substitution 
  for (name in names(mapping))
    mapping[aess[names(aess) %in% name]] <- mapping[name]

  names(mapping) <- sprintf("..%s..", names(mapping))
  substitute_quote(ggcall, env = mapping)
}

substitute_quote <- function(q, env = parent.frame()) {
  UseMethod("substitute_quote")
}

substitute_quote.default <- function(q, env = parent.frame()) {
  eval(bquote(substitute(.(q), .(env))))
}

substitute_quote.quosure <- function(q, env = parent.frame()) {
  # TODO: handle mixed quosure environments instead of retaining original
  eval(bquote(substitute(.(rlang::quo_get_expr(q)), .(env))))
}

