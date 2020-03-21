#' Convert an expression into a call as a list of quosure components
#' 
#' @importFrom rlang quos enquo quo_get_expr quo_set_env
#' @importFrom ggplot2 standardise_aes_names
#'
as_gg_call <- function(x, which = -3L) {
  xexpr <- eval(bquote(
    substitute(.(substitute(x)))),
    envir = parent.frame(-which - 1L))

  if (is.call(xexpr)) {
    xexpr <- expand_dots(xexpr, parent.frame(-which))
    xcall <- do.call(rlang::quos, as.list(xexpr), envir = parent.frame(-which))
    names(xcall)[-1] <- ggplot2::standardise_aes_names(names(xcall)[-1])
    if (".id" %in% names(xcall)[-1]) {
      xid <- rlang::eval_tidy(xcall[[".id"]])
      xcall <- xcall[c(1, 1 + which(names(xcall[-1]) != ".id"))]
    } else {
      xid <- infer_ggcall_id(rlang::quo_get_expr(xcall[[1]]))
    }
  } else {
    xcall <- rlang::quo_set_env(rlang::enquo(xexpr), parent.frame(-which - 1L))
    xid <- infer_ggcall_id(rlang::quo_get_expr(xcall))
  }

  xcall <- list(xcall)
  names(xcall) <- xid
  xcall
}


#' Infer a ggpacket layer id from the call object
infer_ggcall_id <- function(expr) {
  # TODO: prohibit names ambiguous with gg args with dots 
  #       (inherit.aes, na.rm, show.legend, fun.data, label.r)
  if (is.name(expr)) gsub("^(geom|stat)_", "", as.character(expr))
  else "fn"
}
