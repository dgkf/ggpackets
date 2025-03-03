#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggpacket <- function(object, plot, object_name) {
  gg_plus_ggpacket(plot, object)
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
  if (inherits(e1, "ggpacket")) {
    return(ggpacket_plus_ANY(e1, e2))
  }

  ggplot2::`%+%`(e1, e2)
}
