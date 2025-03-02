vcapply <- function(..., FUN.VALUE = character(1L)) {  # nolint
  vapply(..., FUN.VALUE = FUN.VALUE)
}

is_primitive_ggcall <- function(ggcall) {
  f <- rlang::eval_tidy(ggcall[[1]], env = rlang::quo_get_env(ggcall[[1]]))
  is.primitive(f)
}
