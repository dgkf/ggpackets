#' A ggpacket object
#'
#' @slot data A dataset or waiver to use within the ggpacket ggcalls.
#' @slot mapping A ggplot aesthetic mapping to use within the ggpacket ggcalls.
#' @slot dots Arguments which should be passed before prespecified arguments to
#'   each ggcall.
#' @slot ggcalls A list of lazily evaluated ggplot layer construction
#'   expressions.
#'
#' @docType methods
#' @rdname ggpacket-methods
#'
setClass("ggpacket",
  contains = "function",
  slots = list(
    id = "character",
    data = "ANY",
    mapping = "ANY",
    dots = "list",
    ggcalls = "list"))


#' Swallow calls when a ggpacket is added to any expression
#'
#' @param e1 A ggpacket object.
#' @param e2 Any object.
#'
ggpacket_plus_ANY <- function(e1, e2) {
  e1@ggcalls <- append(e1@ggcalls, as_gg_call(e2))
  e1
}



#' Subset a ggpacket for a selection of ggcalls
#'
#' @param x A ggpacket to subset.
#' @param i A vector upon which to subset the ggpacket ggcalls.
#' @param ... Additional arguments unused.
#'
`[.ggpacket` <- function(x, i, ...) {
  subset_ggpacket(x, i, ...)
}



#' Subset a ggpacket for a selection of ggcalls
#'
#' @param x A ggpacket to subset.
#' @param i A vector upon which to subset the ggpacket ggcalls.
#' @param ... Additional arguments unused.
#'
`[[.ggpacket` <- function(x, i, ...) {
  x[i, ...]
}



length.ggpacket <- function(x) {
  length(x@ggcalls)
}



as.list.ggpacket <- function(x) {
  lapply(seq_along(x), function(i) x[[i]])
}



names.ggpacket <- function(x) {
  lapply(x@ggcalls, attr, "ids")
}



#' Add a ggpacket object to another, arbitrary object
#'
#' @param e1 A \code{ggpacket} object.
#' @param e2 Any object.
#'
#' @rdname ggpacket-methods
#' @aliases +,ggpacket,ANY-method
#'
#' @export
#' 
setMethod(`+`, signature("ggpacket", "ANY"), function(e1, e2) {
  ggpacket_plus_ANY(e1, e2)
})

#' Index into a ggpacket object
#' 
#' @param x A \code{ggpacket} object.
#' @param i A \code{character} or \code{numeric} vector for indexing.
#' @param j Unused.
#' @param drop Unused.
#' @param ... Unused.
#' 
#' @export
setMethod("[", c("ggpacket", "ANY", "ANY"), `[.ggpacket`)

#' Index into a ggpacket object
#' 
#' @param x A \code{ggpacket} object.
#' @param i A \code{character} or \code{numeric} value for indexing.
#' @param j Unused.
#' @param ... Unused.
#' 
#' @export
setMethod("[[", c("ggpacket", "ANY", "ANY"), `[[.ggpacket`)

#' Get the number of ggcalls within a ggpacket
#'
#' @param x A \code{ggpacket} object
#'
#' @export
setMethod("length", "ggpacket", length.ggpacket)

#' Convert a ggpacket to a list of ggcalls
#'
#' @param x A \code{ggpacket} object
#'
#' @export
setMethod("as.list", "ggpacket", as.list.ggpacket)

#' Fetch the ids associated with each ggcall
#'
#' @param x A \code{ggpacket} object
#'
#' @export
setMethod("names", "ggpacket", names.ggpacket)




#' Add a gg object to a ggpacket object
#'
#' @param e1 A ggplot ggproto object.
#' @param e2 A ggpacket object.
#'
#' @importFrom rlang eval_tidy
#' @importFrom ggplot2 waiver
#'
gg_plus_ggpacket <- function(e1, e2) {
  all_ids <- unique(unlist(lapply(e2@ggcalls, attr, "ids")))

  # aesthetic mapping and data for ggpacket scope
  ggpk_data <- update_data(e1$data, e2@data)
  ggpk_mapping <- update_mapping(e1$mapping, e2@mapping)

  Reduce(function(gg, ggcall) {
    ggcall_ids <- attr(ggcall, "ids")

    # apply substitutions for ..dot.. names
    ggcallf <- rlang::eval_tidy(ggcall[[1]])

    if (identical(ggcallf, .Primitive("(")) || 
        identical(ggcallf, .Primitive("{"))) {
      ggpk_i <- rlang::eval_tidy(as.call(ggcall))
    } else {
      ggcall <- substitute_ggcall_dot_aes(ggpk_mapping, ggcall)
      ggcallargs <- append(e2@dots, as.list(ggcall)[-1])

      # build gg call
      ggcallargs <- filter_by_ggcall_ids(ggcallargs, ggcall_ids, all_ids)
      ggcallargs <- lapply(ggcallargs, rlang::eval_tidy)
      ggcallargs <- match_unnamed_args(ggcallf, ggcallargs)
      ggcallargs <- smart_swap_mapping_data(ggcallargs)
      ggcallargs <- deduplicate_params(ggcallargs)
      ggcallargs <- only_formals_and_dots(ggcallf, ggcallargs)

      ggpk_i <- with_ignore_unknown_params(do.call(ggcallf, ggcallargs))
    }

    if (inherits(ggpk_i, "ggpacket")) {
      ggpk_i@data <- update_data(ggpk_data, ggpk_i@data)
      ggpk_i@mapping <- update_mapping(ggpk_mapping, ggpk_i@mapping)

      ggcall_ids <- ggpk_i@id
      all_ids <- unique(c(ggcall_ids, all_ids))

      ggpk_i@mapping <- filter_by_ggcall_ids(ggpk_i@mapping, ggcall_ids, all_ids)

    } else if (inherits(ggpk_i, "ggproto")) {
      # apply data scoping
      ggpk_i$data <- update_data(ggpk_data, ggpk_i$data)
      if (is.null(ggpk_i$data)) ggpk_i$data <- ggplot2::waiver()

      # apply mapping scoping
      if (!isFALSE(ggpk_i$inherit.aes)) {
        ggpk_i$mapping <- update_mapping(ggpk_mapping, ggpk_i$mapping)
        ggpk_i$mapping <- filter_by_ggcall_ids(ggpk_i$mapping, ggcall_ids, all_ids)
        ggpk_i$inherit.aes <- FALSE
      }

      # unset any aesthetics that should be "reset"
      ggpk_i$mapping <- handle_reset_mapping(ggpk_i$mapping)
    }

    # add to gg plot construction
    gg + ggpk_i

  }, e2@ggcalls, init = e1)
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
#' \dontrun{
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
#' }
#'
#' @importFrom methods new
#' @export
#'
ggpacket <- function(...) {
  new_ggpacket <- methods::new("ggpacket", ggpacket_call)
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
#' @param .id an optional identifier tag for the \code{ggpacket}, used for
#' filtering arguments and aesthetics that are propegated into the contained
#' \code{ggplot} layers.
#'
#' @return A new \code{ggpacket} object with the new defaults applied
#'
#' @importFrom rlang enquos
#' @importFrom methods new
#' @importFrom ggplot2 standardise_aes_names
#'
ggpacket_call <- function(mapping = NULL, data = NULL, ..., .id = character(0L)) {
  calling_ggpk <- self()

  if (!is.null(mapping) && !inherits(mapping, "uneval")) {
    mapping_in <- mapping
    mapping <- data
    data <- mapping_in
  }

  dots <- as.list(rlang::enquos(...))
  names(dots) <- ggplot2::standardise_aes_names(names(dots))

  methods::new("ggpacket",
    ggpacket_call,
    id = .id,
    data = update_data(calling_ggpk@data, data),
    mapping = update_mapping(calling_ggpk@mapping, mapping),
    dots = dots,
    ggcalls = calling_ggpk@ggcalls)
}


#' Returning the calling object from within a function
#'
#' Used for retrieving an S4 object being called as though it is a function
#'
#' @param which A relative environment offset in which to search for an object
#'   with a name of the calling expression.
#'
self <- function(which = -1L) {
  calling_expr <- sys.call(which = which)
  eval(calling_expr[[1]], envir = parent.frame(-which + 1))
}


#' Reduce a list of mappings, iteratively routing aesthetics
#'
#' @param ... A series of mappings to be sequentially collapsed
#'
#' @importFrom ggplot2 aes
#'
update_mapping <- function(...) {
  Reduce(function(m1, m2) {
    m1 <- as.list(m1)
    m2 <- as.list(m2)
    m2 <- lapply(m2, substitute_ggcall_dot_aes, mapping = m1)
    m1[names(m2)] <- m2
    handle_reset_mapping(do.call(ggplot2::aes, m1))
  }, Filter(Negate(is.null), list(...)))
}


#' Reduce data parameters, iteratively applying functions or masking
#'
#' @param d1 A plot data object to update
#' @param d2 A second plot data object with which to update \code{d1}
#' @param ... Additional objects to sequentially collapse.
#'
update_data <- function(d1, d2, ...) {
  if (missing(d2)) return(d1)
  UseMethod("update_data", d2)
}

update_data.default <- function(d1, d2, ...) {
  d <- update_data(d2, ...)
  if (inherits(d, "waiver")) NULL else d
}

update_data.NULL <- function(d1, d2, ...) {
  d <- update_data(d1, ...)
  if (inherits(d, "waiver")) NULL else d
}

update_data.waiver <- update_data.NULL

update_data.function <- function(d1, d2, ...) {
  d <- if (is.function(d1)) update_data(function(...) d2(d1(...)), ...)
       else if (is.null(d1) || inherits(d1, "waiver")) update_data(d2, ...)
       else update_data(d2(d1), ...)
  if (inherits(d, "waiver")) NULL else d
}

update_data.formula <- function(d1, d2, ...) {
  update_data(d1, rlang::as_function(d2), ...)
}


#' Check a ggpacket object for required aesthetic arguments
#' 
#' @param x A \code{ggpacket} object or related \code{ggplot} component
#' 
#' @export
required_aesthetics <- function(x) {
  UseMethod("required_aesthetics")
}

#' @export
required_aesthetics.default <- function(x) {
  character(0L)
}

#' @export
required_aesthetics.ggpacket <- function(x) {
  if (length(x@ggcalls) > 1L) {
    sort(unique(unlist(lapply(as.list(x), required_aesthetics))))
  } else {
    required_aesthetics(x@ggcalls)
  }
}

#' @export
required_aesthetics.LayerInstance <- function(x) {
  x$geom$required_aes
}

#' @export
required_aesthetics.quosures <- function(x) {
  aess <- .all_aesthetics
  names(aess) <- paste0("..", aess, "..")

  layer <- tryCatch(rlang::eval_tidy(x[[1]]), error = function(e) NULL)
  if (is.primitive(layer))
    layer <- tryCatch(rlang::eval_tidy(as.call(x)), error = function(e) NULL)
  if (is.function(layer)) 
    layer <- do.call(layer, list())

  layer_aes <- required_aesthetics(layer)

  named_aes_args <- intersect(names(x[-1]), layer_aes)

  dot_aes <- unlist(lapply(x[layer_aes], function(expr) {
    aess[match(all.names(expr), names(aess), nomatch = 0L)]
  }))

  mapping_dot_aes <- unlist(lapply(
    rlang::quo_squash(x$mapping)[layer_aes], 
    function(expr) {
      aess[match(all.names(expr), names(aess), nomatch = 0L)]
    }))

  mapped_aes <- names(rlang::quo_squash(x$mapping)[-1])

  setdiff(
    sort(unique(c(layer_aes, dot_aes, mapping_dot_aes))), 
    c(mapped_aes, named_aes_args))
}

#' @export
required_aesthetics.list <- function(x) {
  sort(unique(unlist(sapply(x, required_aesthetics))))
}


