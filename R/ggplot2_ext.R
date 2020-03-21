#' @export 
"+.gg" <- function(e1, e2) {
  if (inherits(e2, "ggpacket")) 
    return(gg_plus_ggpacket(e1, e2))
  if (!inherits(e1, "ggproto"))
   return(ggplot2:::`+.gg`(e1, e2)) 
  new("ggpacket", ggpacket_call, ggcalls = list(as_gg_call(e1), as_gg_call(e2)))
}


#' @export
`%+%` <- function(e1, e2) {
  if (inherits(e1, "ggpacket")) 
    return(ggpacket_plus_ANY(e1, e2))
  ggplot2:::`%+%`(e1, e2)
}
