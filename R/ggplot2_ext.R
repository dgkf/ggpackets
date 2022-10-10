#' Retrieve ggplot2 internal gg addition method
#'
#' @return ggplot2's unexported `+.gg` handler
#'
#' @keywords internal
#'
.get_ggplot2_internal_plus_gg <- function() {
  getNamespace("ggplot2")[["+.gg"]]
}


#' Intercept ggplot2 ggproto plus operator
#'
#' @param e1 An object to add to a ggproto object.
#' @param e2 A ggproto object to add.
#'
#' @return A \code{ggplot2} object or \code{ggpacket}, dependent on whether
#'   \code{e1} is a materialized \code{ggproto} object or a \code{ggpacket}.
#'
#' @importFrom methods new
#' @export
"+.gg" <- function(e1, e2) {
  if (inherits(e2, "ggpacket"))
    return(gg_plus_ggpacket(e1, e2))

  if (!inherits(e1, "ggproto")) {
    plus_fn <- .get_ggplot2_internal_plus_gg()
    return(plus_fn(e1, e2))
  }

  methods::new(
    "ggpacket",
    ggpacket_call,
    ggcalls = list(as_gg_call(e1), as_gg_call(e2))
  )
}


#' Lazy handler for ggplot addition
#'
#' @param e1 Addition lhs.
#' @param e2 Addition rhs.
#'
#' @return A new \code{ggpacket} object with \code{e2} appended as an additional
#'   layer or layers.
#'
#' @export
`%+%` <- function(e1, e2) {
  if (inherits(e1, "ggpacket"))
    return(ggpacket_plus_ANY(e1, e2))
  ggplot2::`%+%`(e1, e2)
}
