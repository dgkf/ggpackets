#' A class for storing ggpack call contents for lazy evaluation
#'   
#' @examples 
#' 
#' @slot ggcall a constructor function to produce a ggproto layer object
#' @slot ggargs the arguments to use when calling the constructor function
#'   
#' @rdname ggpacked_layer-class
#'   
#' @import methods
#' @export ggpacked_layer
ggpacked_layer <- setClass("ggpacked_layer",
  slots = c(
    id = "list", 
    ggcall = "function", 
    ggargs = "list",
    geom = "ANY",
    stat = "ANY",
    ggpackargs = "list"),
  prototype = list(
    id = list(NULL), 
    ggcall = function(gg) gg, 
    ggargs = list(),
    geom = NULL,
    stat = NULL,
    ggpackargs = list())
)

#' Initialize empty ggpacket_layer
#' @param .Object ggpacked_layer object to be initialized
#' @rdname initialize,ggpacked_layer-method
setMethod(f = "initialize", methods::signature(.Object = "ggpacked_layer"), 
  function(.Object) { .Object })

#' Initialize new object by adding ggproto to list with name label
#' @param .Object ggpacket object to be initialized
#' @param ggproto_obj an object of class ggproto to add to the ggpacket's
#'   internal list
#' @param label an optional label to be able to index this component within the
#'   ggpacket
#' @rdname initialize,ggpacked_layer,function,list,character
#' @importFrom stats setNames
setMethod("initialize", "ggpacked_layer", 
  function(.Object, ggcall, ggargs = list(), id = NULL, 
      geom = NULL, stat = NULL, ggpackargs = list()) {
    .Object@id = as.list(id)
    .Object@ggcall = ggcall
    .Object@ggargs = ggargs
    .Object@geom = geom
    .Object@stat = stat
    .Object@ggpackargs = ggpackargs
    .Object
  }
)

#' Overload show method to show ggpacket
#' @param object the ggpacket object to show
#' @rdname show-ggpacked_layer
#' @importFrom utils capture.output
#' @importFrom ggplot2 ggplot ggplot_build
setMethod("show", "ggpacked_layer", function(object) {
  if (any(as.logical(Map(Negate(is.null), object@id)))) { 
    ids <- Map(function(i) if (is.null(i)) 'wildcard' else i, object@id)
    cat('ggpacket layer with id(s):', paste(ids, collapse = ', '), '\n')
  } else
    cat('Anonymous ggpacket layer\n')
    
  print(do.call(object@ggcall, object@ggargs))
})

#' Add NULL to a ggpacket to return the ggpacket
#' @rdname ggpacked_layer-addition
setMethod("+", c("NULL", "ggpacked_layer"), function(e1, e2) e2)

#' Sum of two ggpacked layers to return ggpacket object
#' @rdname ggpacked_layer-addition
setMethod("+", c("ggpacked_layer", "ggpacked_layer"), function(e1, e2) { 
  ggpacket() + e1 + e2
})

# register unexported gg class from ggplot2 so signature is accepted
setOldClass("gg")

#' Primitive methods for adding ggpacked layers to ggplot during construction
#' @param e1 left side argument for addition
#' @param e2 right side argument for addition
#' @rdname ggpacked_layer-addition
setMethod("+", c("gg", "ggpacked_layer"), function(e1, e2) {
  # extract parameters for ggpack constructions
  auto_remove_aes <- e2@ggpackargs$auto_remove_aes %||% FALSE
  envir           <- e2@ggpackargs$envir           %||% parent.frame()
  args            <- e2@ggargs
  
  if ('mapping' %in% names(args)) { 
    args$mapping <- last_args(c(e1$mapping, args$mapping))
    args$mapping <- replace_reserved_aesthetic_references(args$mapping)
  } else {
    args <- append(args, e1['mapping'], 0)
  }
  
  # flatten args to mapping, remove extra aes
  args <- flatten_aes_to_mapping(args, allowed_aesthetics(e2@geom), auto_remove_aes)
  args$mapping <- structure(args$mapping, class = 'uneval')
  
  if (auto_remove_aes) # remove invalid argnames and unevaluated aesthetics
    args <- filter_args(e2@ggcall, e2@geom, e2@stat, args[!uneval_aes(args)])
  args <- Map(function(v) if (is_uneval(v)) eval(v, envir) else v, args)
  
  e1 + do.call(e2@ggcall, args)
})