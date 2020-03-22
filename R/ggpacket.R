setClass("ggpacket",
  contains = "function",
  slots = list(
    data = "ANY",
    mapping = "ANY",
    dots = "list",
    ggcalls = "list"))


#' Swallow calls when a ggpacket is added to any expression
ggpacket_plus_ANY <- function(e1, e2) {
  e1@ggcalls <- append(e1@ggcalls, as_gg_call(e2))
  e1
}


setMethod(`+`, signature("ggpacket", "ANY"), function(e1, e2) {
  ggpacket_plus_ANY(e1, e2)
})



#' Add a gg object to a ggpacket object
#'
#' @importFrom rlang eval_tidy
#'
gg_plus_ggpacket <- function(e1, e2) {
  e2@data <- update_data(e1$data, e2@data)
  all_ids <- unique(unlist(lapply(e2@ggcalls, attr, "ids")))

  Reduce(function(gg, ggcall) {
    ggcall <- ggcall[[1]]
    ggcall_ids <- attr(ggcall, "ids")

    # apply substitutions
    ggpk_mapping <- update_mapping(e1$mapping, e2@mapping)
    ggcall <- substitute_ggcall_dot_aes(ggpk_mapping, ggcall)

    # build gg call
    ggcallf <- rlang::eval_tidy(ggcall[[1]])
    ggcallargs <- append(e2@dots, as.list(ggcall)[-1])
    ggcallargs <- filter_by_ggcall_ids(ggcallargs, ggcall_ids, all_ids)
    ggcallargs <- deduplicate_params(ggcallargs)
    ggcallargs <- lapply(ggcallargs, rlang::eval_tidy)
    ggpk_i <- with_ignore_unknown_params(do.call(ggcallf, ggcallargs))

    # handle data and aesthetic propegation for geometry layers
    if (inherits(ggpk_i, "ggproto") && "data" %in% names(formals(ggcallf))) {
      # apply data scoping
      ggpk_i$data <- update_data(e1$data, e2@data, ggpk_i$data)
    }

    if (inherits(ggpk_i, "ggproto") && "mapping" %in% names(formals(ggcallf))) {
      # apply mapping scoping 
      if (!isFALSE(ggpk_i$inherit.aes)) {
        ggpk_i$mapping <- update_mapping(ggpk_mapping, ggpk_i$mapping)
        ggpk_i$inherit.aes <- FALSE
      }

      # unset any aesthetics that should be "reset"
      ggpk_i$mapping <- handle_reset_mapping(ggpk_i$mapping)
    }

    # add to gg plot construction
    gg + ggpk_i

  }, lapply(seq_along(e2@ggcalls), function(i) e2@ggcalls[i]), init = e1)
}


#' A container for lazy ggplot layers
#'
#' \code{ggpacket}s present a mechanism for easily collecting loose ggplot
#' layers into objects that can be reused across multiple plots, as well as
#' handling of hierarchical argument propegation, as well as data and aesthetic
#' scoping.
#' 
#' The \code{ggpacket} object behaves like a function, returning an updated
#' \code{ggpacket}. As well, it contains a few slots which can be used for
#' programmatically accessing the lazy ggplot calls.
#'
#' Within \code{ggpacket} and subsequent \code{ggplot} layer calls, aesthetic
#' mappings can contain references to previously mapped aesthetics using the
#' double-dot keywords (e.g. \code{..x..}). In addition, the keyword
#' \code{..reset..} can be used to revert aesthetics within the \code{ggpacket}
#' or layer to an un-specified state. 
#'
#' Because \code{ggpacket}s will tolerate a more flexible syntax for layer
#' specifications, it's preferrable to use the \code{ggplot} composition
#' operator \code{%+%} (instead of \code{+}). This allows for duplicate
#' argument names and non-standard aesthetics to be passed, which are both
#' handled internally within the \code{ggpacket} call, but will trigger
#' warnings when using a bare \code{+}.
#' 
#' @inheritDotParams ggpacket_call
#' 
#' @slot data A lazy reference to the \code{data} parameter of the
#' \code{ggpacket}, allowing for scoping the data used by a block of
#' \code{ggplot} layers.
#' @slot mapping A lazy reference to the \code{mapping} parameter of the
#' \code{ggpacket}, allowing for scoping of aesthetic mappings over a block of
#' \code{ggplot} layers.
#' @slot dots Quosures representing arguments to be passed to all of the
#' \code{ggpacket}'s \code{ggplot} layers.
#' @slot ggcalls A list containing the layers stored within the \code{ggpacket}
#' 
#' @examples
#' library(ggplot2)
#'
#' # create a ggpacket directly, setting some fixed argument settings
#' ggpk_simple <- ggpacket() %+% geom_line(color = "red") %+% geom_point()
#' ggplot(mtcars, aes(x = wt, y = mpg)) + ggpk_simple()
#'
#' # any non-fixed arguments can be modified in the ggpacket call
#' ggplot(mtcars, aes(x = wt, y = mpg)) + ggpk_simple(color = "green")
#'
#' # arguments can be specified for only specific layers by prefixing them
#' ggplot(mtcars, aes(x = wt, y = mpg)) + ggpk_simple(point.size = 5)
#'
#' # allow masking of preset arguments by wrapping the ggpacket in a function
#' ggpk_func <- function(...) {
#'   ggpacket() %+%
#'     geom_line(...) %+%
#'     geom_point(color = "red", ...)
#' }
#' 
#' ggplot(mtcars, aes(x = wt, y = mpg)) + 
#'   ggpk_func(color = "purple", size = 2, point.size = 4)
#'
#' @export
#' 
ggpacket <- function(...) {
  new_ggpacket <- new("ggpacket", ggpacket_call)
  new_ggpacket(...)
}


#' The function used when a \code{ggpacket} is called as a function
#' 
#' @param data The data argument to use for all layers within the
#' \code{ggpacket}
#' @param mapping The aesthetic mapping to use as a basis for all layers within
#' the \code{ggpacket}. Layer-specific mappings will update the value of the
#' mapping, and assigning \code{..reset..} to any aesthetic field will return
#' the propegated aesthetic mapping to a default, unmapped state for that
#' layer.
#' @param ... additional arguments passed to all bundled \code{ggplot} layers,
#' and will be overwritten by layer-specific arguments if provided.
#'
#' @return A new \code{ggpacket} object with the new defaults applied
#' 
#' @importFrom rlang enquos
#' @importFrom ggplot2 standardise_aes_names
#' 
ggpacket_call <- function(data = NULL, mapping = NULL, ...) {
  calling_ggpk <- self()

  if (inherits(data, "uneval")) {
    mapping <- data
    data <- NULL
  }

  dots <- as.list(rlang::enquos(...))
  names(dots) <- ggplot2::standardise_aes_names(names(dots))

  new("ggpacket",
    ggpacket_call,
    data = update_data(calling_ggpk@data, data),
    mapping = update_mapping(calling_ggpk@mapping, mapping),
    dots = dots,
    ggcalls = calling_ggpk@ggcalls)
}


#' Returning the calling object from within a function
self <- function(which = -1L) {
  calling_expr <- sys.call(which = which)
  eval(calling_expr[[1]], envir = parent.frame(-which + 1))
}


#' Reduce a list of mappings, iteratively routing aesthetics 
#'
#' @importFrom ggplot2 aes
#' 
update_mapping <- function(...) {
  Reduce(function(m1, m2) {
    m1 <- as.list(m1)
    m2 <- as.list(m2)
    m2 <- lapply(m2, substitute_ggcall_dot_aes, mapping = m1)
    m1[names(m2)] <- m2
    do.call(ggplot2::aes, m1)
  }, Filter(Negate(is.null), list(...)))
}


#' Reduce data parameters, iteratively applying functions or masking datasets
update_data <- function(...) {
  # TODO: handle function application
  Reduce(function(d1, d2) {
    d <- if (!is.null(d1) && (is.null(d2) || inherits(d2, "waiver"))) d1 else d2
    if (inherits(d, "waiver")) NULL else d
  }, list(...))
}
