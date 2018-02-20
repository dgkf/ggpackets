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
setMethod(f = "initialize", methods::signature(.Object = "ggpacket"), 
  function(.Object) { .Object })


#' Overload show method to show ggpacket
#' @param object the ggpacket object to show
#' @rdname show-ggpacket
#' @importFrom utils capture.output
#' @importFrom ggplot2 ggplot ggplot_build
setMethod("show", "ggpacket", function(object) {
  ## attempt to build plot by calling packet with `ggplot() + ggpacket_obj`
  plt_output <- try(silent = TRUE, { 
    plt <- ggplot2::ggplot() + object
    utils::capture.output({
      bld <- ggplot2::ggplot_build(plt)
    })
  })
  
  if (length(plt_output) == 0 && all(sapply(bld$data, nrow))) 
    return(show(plt))
  
  cat(safecrayon('darkblue', 'ggpacket'), '\n')
  cat('standalone plotting status: \n')
  if (length(plt_output)) 
    cat(safecrayon('midred', strwrap(
        attr(plt_output, 'condition')$message, 
        indent = 2, exdent = 2)), '\n')
  else if (!all(sapply(bld$data, nrow))) 
    cat(safecrayon('midred', strwrap(
        'Not all layers have been passed sufficient data', 
        indent = 2, exdent = 2)), '\n')
  
  cat('\n')
  print(object@ggcalls)
})


#' @rdname ggpacket-equivalence
setMethod("==", c("ggpacket", "ggpacket"), function(e1, e2) {
  length(e1@ggcalls) == length(e2@ggcalls) && 
      do.call(all, Map(function(e1i, e2i) e1i == e2i, e1@ggcalls, e2@ggcalls))
})


#' Overload names method to print ggpacket ggcall list names
#' @param x the ggpacket object to show
#' @rdname names-ggpacket
setMethod("names", "ggpacket", function(x) { 
  Map(function(ggc) ggc@id, x@ggcalls)
})

#' Overload names method to print ggpacket ggcall list names
#' @param x the ggpacket object to show
#' @rdname names-ggpacket
setMethod("names<-", "ggpacket", function(x, value) {
  x@ggcalls <- Map(function(ggc, v) { 
    ggc@id <- as.list(v); ggc 
  }, x@ggcalls, value)
  x
})

#' Overload [ generic to index into ggcalls list
#' @param x ggpacket object
#' @param i index of the ggcall in the ggpacket object
#' @rdname ggpacket-indexing
setMethod("[", "ggpacket", function(x, i) { 
  if (is.numeric(i))
    ggpacket() + x@ggcalls[i]
  else {
    is <- which(sapply(x@ggcalls, function(ggc) any(i %in% ggc@id)))
    ggpacket() + x@ggcalls[is]
  }
})

#' Overload [[ generic to index into ggcalls list
#' @rdname ggpacket-indexing
setMethod("[[", "ggpacket", function(x, i) { 
  if (is.numeric(i)) return(x@ggcalls[[i]])
  i <- head(which(sapply(x@ggcalls, function(ggc) i %in% ggc@id)), 1)
  x@ggcalls[[i]]
})


#' Primitive methods for adding ggpackets to various ggplot objects
#' @param e1 left side argument for addition
#' @param e2 right side argument for addition
#' @rdname ggpacket-addition
setMethod("+", c("ggpacket", "ANY"), function(e1, e2) {
  e2call <- as.list(as.list(sys.call()[-1])[[2]])
  
  if ('ggpacked_layer' %in% class(e2)) { 
    e1@ggcalls <- append(e1@ggcalls, e2)
  } else if ('ggpacket' %in% class(e2))
    e1@ggcalls <- append(e1@ggcalls, e2@ggcalls)
  else if ('list' %in% class(e2))
    e1 <- Reduce("+", init = e1, e2)
  else
    e1@ggcalls <- append(e1@ggcalls, do.call(ggpack, e2call)@ggcalls)
    
  e1
})

#' Add NULL to a ggpacket to return the ggpacket
#' @rdname ggpacket-addition
setMethod("+", c("NULL", "ggpacket"), function(e1, e2) e2)

# register unexported gg class from ggplot2 so signature is accepted
setOldClass("gg")

#' Combine ggpacket contents with gg call to incorporate seamlessly with ggplot
#' calls
#' @rdname ggpacket-addition
setMethod("+", c("gg", "ggpacket"), function(e1, e2) 
  Reduce("+", e2@ggcalls, e1))

