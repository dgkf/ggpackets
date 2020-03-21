handle_reset_mapping <- function(mapping) {
  mapping[vapply(mapping, function(ai) {
    rlang::quo_get_expr(ai) != quote(..reset..)
  }, logical(1L))]
}

substitute_ggcall_dot_aes <- function(mapping, ggcall, envir = parent.frame()) {
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

