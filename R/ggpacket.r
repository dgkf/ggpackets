#' A class for wrapping ggplot layers.
#' 
#' ggplot ggproto objects can not be added together as they are and must be made
#' reusable by first adding to a list. This class aggregates ggproto objects
#' into a list and handles adding into a ggplot construction.
#'   
#' @examples 
#' library(ggplot2)
#' 
#' # a ggpacket can be created as an object by not passing arguments
#' # to the ggpack function. Any ggproto layers or ggpack'd ggpacket 
#' # layers can be added directly
#' bar_error_counts <- ggpacket() +
#'   stat_summary(fun.y = mean, geom = 'bar') + 
#'   stat_summary(fun.data = mean_se, width = 0.2, geom = "errorbar") + 
#'   stat_summary(fun.data = function(d) c(
#'                  y = mean(d) + sd(d)/sqrt(length(d)), 
#'                  label = length(d)), 
#'                vjust = -1, geom = 'text')
#' 
#' ggplot(mtcars, aes(x = gear, y = mpg)) + 
#'   bar_error_counts
#' 
#' # easier functionalization of subcomponents of a ggplot
#' custom_errorbars <- function(error_function, ...) {
#'   ggpack(stat_summary, id = 'bar', ..., fun.y = mean, geom = 'bar') + 
#'   ggpack(stat_summary, id = 'errorbar', ..., 
#'          fun.data = error_function, width = 0.2, geom = "errorbar")
#' } 
#' 
#' ggplot(mtcars, aes(x = gear, y = mpg)) + 
#'   custom_errorbars(mean_se, bar.fill = gear) + 
#'   facet_grid(. ~ am, scales = 'free')
#' 
#' @slot ggcalls a list of ggproto layer objects
#'   
#' @rdname ggpacket-class
#'   
#' @import methods
#' @export ggpacket
ggpacket <- setClass("ggpacket",
  slots = c(ggcalls = "list"),
  prototype = list(ggcalls = list())
)

#' Initialize empty ggpacket
#' @param .Object ggpacket object to be initialized
#' @rdname initialize,ggpacket-method
#' @importFrom methods signature
setMethod(f = "initialize", methods::signature(.Object = "ggpacket"), 
  function(.Object) { .Object })

#' Initialize new object by adding ggproto to list with name label
#' @param .Object ggpacket object to be initialized
#' @param ggproto_obj an object of class ggproto to add to the ggpacket's
#'   internal list
#' @param label an optional label to be able to index this component within the
#'   ggpacket
#' @rdname initialize,ggpacket,ggproto,character
setMethod("initialize", "ggpacket", 
  function(.Object, ggproto_obj = NULL, label = NULL) {
    if (is.null(ggproto_obj)) return(.Object)
    else if (all(class(ggproto_obj) == 'list'))
      .Object <- .Object + stats::setNames(
        ggproto_obj, 
        names(ggproto_obj) %||% label)
    else
      .Object <- .Object + stats::setNames(
        list(ggproto_obj), 
        label)
    
    .Object
  }
)

#' Overload show method to show ggpacket
#' @param object the ggpacket object to show
#' @rdname show-ggpacket
#' @importFrom utils capture.output
#' @importFrom ggplot2 ggplot ggplot_build
setMethod("show", "ggpacket", function(object) {
  ## attempt to build plot by calling packet with `ggplot() + ggpacket_obj`
  plt_output <- try(silent = TRUE, { 
    plt <- ggplot2::ggplot() + object
    utils::capture.output(type = 'message', {
      bld <- ggplot2::ggplot_build(plt)
    })
  })
  
  if (length(plt_output) == 0 && all(sapply(bld$data, nrow))) 
    return(show(plt))
  
  print(plt_output)
  
  cat("ggpacket\nA container for multiple ggplot ggproto objects\n\n")
  cat('standalone plotting status: \n')
  if (length(plt_output)) 
    cat(strwrap(
        attr(plt_output, 'condition')$message, 
        indent = 2, exdent = 2), 
      '\n')
  else if (!all(sapply(bld$data, nrow))) 
    cat(strwrap(
        'Not all layers have been passed sufficient data', 
        indent = 2, exdent = 2),
      '\n')
  
  cat('\n')
  if (length(object@ggcalls) == 0) cat("empty\n\n")
  else
    mapply(
      function(n, name, ggp) cat(sprintf("[[%s]] %s\n%s\n\n", n, name, ggp)),
      n = 1:length(object@ggcalls),
      ggp = Map(function(ggc) 
        paste(capture.output(suppressMessages(print(ggc))), 
              collapse = "\n", sep = "\n"), 
        object@ggcalls),
      name = names(object@ggcalls) %||% ""
    )
})

#' Overload names method to print ggpacket ggcall list names
#' @param x the ggpacket object to show
#' @rdname names-ggpacket
setMethod("names", "ggpacket", function(x) { names(x@ggcalls) })

#' Overload [ generic to index into ggcalls list
#' @param x ggpacket object
#' @param i index of the ggcall in the ggpacket object
#' @rdname ggpacket-indexing
setMethod("[", "ggpacket", function(x, i) { 
  if (any(sapply(x@ggcalls[i], function(i) 'ggproto' %in% class(i)))) 
    ggpacket(x@ggcalls[i]) 
  else 
    x@ggcalls[i]
})

#' Overload [[ generic to index into ggcalls list
#' @rdname ggpacket-indexing
setMethod("[[", "ggpacket", function(x, i) { 
  if ('ggproto' %in% class(x@ggcalls[[i]])) ggpacket(x@ggcalls[[i]]) 
  else x@ggcalls[[i]]
})

#' Primitive methods for adding ggpackets to various ggplot objects
#' @param e1 left side argument for addition
#' @param e2 right side argument for addition
#' @rdname ggpacket-addition
setMethod("+", c("ggpacket", "ANY"), function(e1, e2) {
  if      ("ggpacket" %in% class(e2)) e1@ggcalls <- append(e1@ggcalls, e2@ggcalls)
  else if ("theme" %in% class(e2))    e1@ggcalls <- append(e1@ggcalls, list(e2))
  else                                e1@ggcalls <- append(e1@ggcalls, e2)
  e1
})

#' Add NULL to a ggpacket to return the ggpacket
#' @rdname ggpacket-addition
setMethod("+", c("NULL", "ggpacket"), function(e1, e2) e2)

# register unexported gg class from ggplot2 so signature is accepted
setOldClass("gg")

#' Combine ggpacket contents with gg call to incorporate seamlessly with ggplot calls
#' @rdname ggpacket-addition
setMethod("+", c("gg", "ggpacket"), function(e1, e2) Reduce("+", e2@ggcalls, e1))