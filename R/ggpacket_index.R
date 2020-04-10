#' @export
`[.ggpacket` <- function(x, i, ...) {
  subset_ggpacket(x, i, ...)
}

subset_ggpacket <- function(x, i, ...) {
  UseMethod("subset_ggpacket", i)
}

subset_ggpacket.default <- function(x, i, ...) {
  x@ggcalls <- x@ggcalls[i, ...]
  x
}

subset_ggpacket.character <- function(x, i, ...) {
  xs <- vapply(x@ggcalls, function(xi) any(i %in% attr(xi, "ids")), logical(1L))
  x[xs, ...]
}

#' @export
`[[.ggpacket` <- function(x, i, ...) {
  x[i, ...]
}

