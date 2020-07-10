#' ggplot2 internal gg addition method
#' 
#' @param e1 Addition lhs
#' @param e2 Addition rhs
#' 
.plus_gg <- getNamespace("ggplot2")[["+.gg"]]


#' Intercept ggplot2 ggproto plus operator
#' 
#' @param e1 An object to add to a ggproto object.
#' @param e2 A ggproto object to add.
#' 
#' @importFrom methods new
#' @export 
"+.gg" <- function(e1, e2) {
  if (inherits(e2, "ggpacket")) 
    return(gg_plus_ggpacket(e1, e2))
  if (!inherits(e1, "ggproto"))
   return(.plus_gg(e1, e2)) 
  methods::new(
    "ggpacket", 
    ggpacket_call, 
    ggcalls = list(as_gg_call(e1), as_gg_call(e2)))
}


#' Lazy handler for ggplot addition
#' 
#' @param e1 Addition lhs.
#' @param e2 Addition rhs.
#' 
#' @export
`%+%` <- function(e1, e2) {
  if (inherits(e1, "ggpacket")) 
    return(ggpacket_plus_ANY(e1, e2))
  ggplot2::`%+%`(e1, e2)
}
