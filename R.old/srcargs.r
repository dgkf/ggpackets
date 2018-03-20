#' Argument list with parallel source log
#'
#' @rdname argsrc-class
#'   
#' @import methods
#' @export argsrc
argsrc <- setClass("argsrc",
  slots = c(srcs = "list", args = "list"),
  prototype = list(srcs = list(), args = list())
)



#' Initialize empty ggpacket_layer
#' @param .Object argsrc object to be initialized
#' @rdname initialize,argsrc-method
setMethod(f = "initialize", methods::signature(.Object = "argsrc"), 
  function(.Object) { .Object })



#' Initialize empty ggpacket_layer
#' @param .Object argsrc object to be initialized
#' @rdname initialize,argsrc-method
setMethod(f = "initialize", methods::signature(.Object = "argsrc"), 
  function(.Object, ..., args = NULL, sources = NA) { 
    .Object@args <- c(list(...), as.list(args))
    .Object@srcs <- as.list(rep(sources, length.out = length(.Object@args)))
    attributes(.Object)$names <- names(.Object@args)
    .Object
  }
)



#' Overload show method to show ggpacket
#' @param object the ggpacket object to show
#' @rdname show-argsrc
#' @importFrom utils capture.output
#' @importFrom ggplot2 ggplot ggplot_build
setMethod("show", "argsrc", function(object) show(object@args))



#' Overload names method to print argsrc ggcall list names
#' @param x the argsrc object to show
#' @rdname names-argsrc
setMethod("names", "argsrc", function(x) names(x@args))



#' Overload names method to print argsrc ggcall list names
#' @param x the argsrc object to show
#' @rdname names-argsrc
setMethod("names<-", "argsrc", function(x, value) { 
  names(x@args) <- value
  attributes(x)$names <- value
  x
})



#' Overload [ generic to index into ggcalls list
#' @param x argsrc object
#' @param i index of the ggcall in the argsrc object
#' @rdname argsrc-indexing
setMethod("[", "argsrc", function(x, i, sources) { 
  out <- argsrc(args = x@args[i], sources = x@srcs[i])
  if (!missing(sources)) out <- out[out@srcs %in% sources]
  out
})



#' Overload [[ generic to index into ggcalls list
#' @rdname argsrc-indexing
setMethod("[[", "argsrc", function(x, i) {
  x@args[[i]]
})


#' Overload [[ generic to index into ggcalls list
#' @rdname argsrc-indexing
setMethod("[[<-", c(x = "argsrc", i = "ANY"), function(x, i, value) {
  if ('argsrc' %in% class(value)) { 
    x@args[[i]] <- value@args[[1]]
    x@srcs[[i]] <- value@srcs[[1]]
    attributes(x)$names[[i]] <- names(value@args[[1]])
  } else {
    x@args[[i]] <- value
  }
  x
})


#' @rdname argsrc-match
#' @export
setMethod("match", c(x = "ANY", table = "argsrc"),
  function(x, table, nomatch = NA_integer_, incomparables = NULL) {
    match(x, table@args, nomatch, incomparables)
  }
)


#' @rdname argsrc-match
#' @export
setMethod("match", c(x = "argsrc", table = "ANY"),
  function(x, table, nomatch = NA_integer_, incomparables = NULL) {
    match(x@args, table, nomatch, incomparables)
  }
)

#' @rdname argsrc-match
#' @export
setMethod("%in%", c(x = "ANY", table = "argsrc"),
  function(x, table) match(x, table@args, nomatch = 0L) > 0L)


#' @rdname argsrc-match
#' @export
setMethod("%in%", c(x = "argsrc", table = "ANY"),
  function(x, table) match(x@args, table, nomatch = 0L) > 0L)


#' @rdname argsrc-list
#' @export
setMethod("c", "argsrc", function(x, ...) {
  xs <- c(list(x), list(...))
  argsrc(args = c(unlist(Map(function(i) i@args, xs))), 
         sources = c(unlist(Map(function(i) i@srcs, xs))))
})


#' @rdname argsrc-list
setMethod("as.list", "argsrc", function(x, ...) x@args )

#' @rdname argsrc-list
#' @export
as.list.argsrc <- function(x, ...) x@args



