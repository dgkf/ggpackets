#' A class for storing ggpack call contents for lazy evaluation
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
    calldf = "ANY",
    null_empty = "logical"
  ),
  prototype = list(
    id = list(NULL), 
    calldf = NULL,
    null_empty = FALSE
  )
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
  function(.Object, id = NULL, calldf = call_df(), null_empty = FALSE) {
    .Object@id = as.list(id)
    .Object@calldf <- calldf
    .Object@null_empty <- null_empty
    .Object
  }
)



#' ggpacket_layer equivalence
#' @param e1 lhs
#' @param e2 rhs
#' @rdname ggpacked_layer-equivalence
setMethod("==", c("ggpacked_layer", "ggpacked_layer"), function(e1, e2) {
  do.call(all, Map(`==`, e1@id, e2@id)) &&
  e1@calldf     == e2@calldf            &&
  e1@null_empty == e2@null_empty
})



#' Index into layer arguments with vector
#' @param x ggpacked_layer object
#' @param i index
#' @rdname ggpacked_layer-indexing
setMethod("[", "ggpacked_layer", function(x, i) {
  with(x[i], setNames(val, name))
})



#' Index into layer arguments by numeric or character
#' @rdname ggpacked_layer-indexing
setMethod("[[", "ggpacked_layer", function(x, i) {
  with(x[[i]], setNames(val, name))
})



#' Index into layer arguments by name
#' @param name index
#' @rdname ggpacked_layer-indexing
setMethod("$", "ggpacked_layer", function(x, name) x[[name]])



#' Overload show method to show ggpacket
#' @param object the ggpacket object to show
#' @rdname show-ggpacked_layer
#' @importFrom utils capture.output
#' @importFrom ggplot2 ggplot ggplot_build
setMethod("show", "ggpacked_layer", function(object) {
  if (any(as.logical(Map(Negate(is.null), object@id)))) { 
    ids <- Map(function(i) if (is.null(i)) 'wildcard' else i, object@id)
    cat(safecrayon('darkblue', 
      'ggpacket layer with id(s):', paste(ids, collapse = ', '), '\n'))
  } else
    cat(safecrayon('darkblue', 
      'Anonymous ggpacket layer\n'))
  
  show(object@calldf)
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
#' 
#' @importFrom tibble tibble
#' 
#' @rdname ggpacked_layer-addition
setMethod("+", c("gg", "ggpacked_layer"), function(e1, e2) {
  # neglect construction if empty args means empty layer
  if (e2@null_empty && length(e2@calldf) == 0) return(e1)
  
  # flatten incoming mapping into ggplot aesthetics
  e1_mapping_calldf <- call_df(e1$mapping, 'inherited mapping')
  calldf <- e2@calldf + e1_mapping_calldf
  
  if (is.function(calldf@call)) e1 + do_calldf(calldf)
  else                          e1 + eval(calldf@call)
})