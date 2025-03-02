#' Extracted .all_aesthetics from internal ggplot2 with hardcoded fallback
.all_aesthetics <- function() {
  tryCatch({
    # attempt to stay current with ggplot .all_aesthetics upstream
    get(".all_aesthetics", asNamespace("ggplot2"), inherits = FALSE)
  }, error = function(e) {
    # hard coded fallback in case upstream changes private variable name
    # #est for fallback viability included in testthat tests
    c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", "fg",
      "fill", "group", "hjust", "label", "linetype", "lower", "lty", "lwd",
      "max", "middle", "min", "pch", "radius", "sample", "shape", "size", "srt",
      "upper", "vjust", "weight", "width", "x", "xend", "xmax", "xmin",
      "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")
  })
}



#' Specific handling of ..reset.. aesthetic
#'
#' @param mapping A ggplot2 aesthetic mapping.
#'
handle_reset_mapping <- function(mapping) {
  mapping[!vapply(mapping, function(ai) {
    rlang::is_quosure(ai) && rlang::quo_squash(ai) == quote(..reset..)
  }, logical(1L))]
}



#' Substitute a ggcall's dot aesthetics with their redirected values
#'
#' @param mapping A ggplot2 aesthetic mapping.
#' @param ggcall A ggcall list of expressions.
#' @param envir An environment in which the dot aesthetics should be evaluated.
#'
substitute_ggcall_dot_aes <- function(mapping, ggcall, envir = parent.frame()) {
  aess <- .all_aesthetics()
  names(aess) <- ggplot2::standardise_aes_names(aess)

  # add in mappings for alternative naming conventions before substitution
  for (name in names(mapping)) {
    mapping[aess[names(aess) %in% name]] <- mapping[name]
  }

  names(mapping) <- sprintf("..%s..", names(mapping))
  dot_names <- unlist(lapply(ggcall, all.names))
  dot_names <- dot_names[grepl("^\\.\\.(.*)\\.\\.$", dot_names)]
  mapping <- as.environment(mapping)

  for (var in setdiff(dot_names, names(mapping))) {
    mapping[[var]] <- NA
  }

  substitute_quote(ggcall, env = mapping)
}



#' Substitute a quoted expression in a given environmment
#'
#' @param q A quote to evaluate.
#' @param env An environment in which the quote should be evaluated.
#'
substitute_quote <- function(q, env = parent.frame()) {
  UseMethod("substitute_quote")
}

#' @export
substitute_quote.default <- function(q, env = parent.frame()) {
  eval(bquote(substitute(.(q), env)))
}

#' @export
substitute_quote.quosures <- function(q, env = parent.frame()) {
  rlang::as_quosures(lapply(q, function(...) substitute_quote(...), env = env))
}

#' @export
substitute_quote.quosure <- function(q, env = parent.frame()) {
  # TODO: handle mixed quosure environments instead of retaining original
  rlang::quo_set_expr(q, do.call(substitute, list(rlang::quo_squash(q), env)))
}
