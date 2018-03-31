#' internal aes override to allow passing of !!args
#' 
#' @inheritParams ggplot2::aes
#'
#' @examples 
#' library(ggplot2)
#' 
#' data <- data.frame(list(
#'   x = rnorm(1000), 
#'   y = runif(1000), 
#'   value = rnorm(1000, 3)
#' ))
#' 
#' wrapping_function <- function(data, x, y, color) { 
#'   # create quosures from aesthetic args
#'   # enquos(x, y, color) # code removed from package
#'   x <- rlang::enquo(x)
#'   y <- rlang::enquo(y)
#'   color <- rlang::enquo(color)
#'   
#'   # build plot, unquoting e to build aes
#'   ggplot(data) + 
#'     .aes(x = !!x, y = !!y, color = !!color) + 
#'     geom_point()
#' }
#' 
#' wrapping_function(data, x, y, value > 3)
#' 
#' @importFrom ggplot2 aes
#' 
#' @export
#' 
.aes <- function(x, y, ...) { 
  if (!requireNamespace('rlang', quietly = TRUE)) 
    stop('\n.aes() used without package rlang installed. ',
         'To resolve, install package "rlang".')
  
  dots <- c(x = rlang::enquo(x), y = rlang::enquo(y), rlang::quos(...))
  do.call(ggplot2::aes, dequos(dots))
}


#' remove quoting of quoted item, returning only character name
#'
#' @param q argument to de-quote
#' 
dequo <- function(q) { as.list(q)[[-1]] }


#' remove quoting of quoted list items, returning only character names
#'
#' @param ... arguments to de-quote
#' 
dequos <- function(...) { Map(dequo, as.list(...)) }


#' rlang::quos equivalent to rlang::enquo for use within a function
#' 
#' @param ... arguments to encapsulate as a quosure
#' @param envir environment in which to perform forward quosure
#' 
# enquos <- function(..., envir = parent.frame()) {
#   if (!requireNamespace('rlang', quietly = TRUE)) return(substitute(...()))
#   for (dot in substitute(...())) {
#     capture <- rlang::lang(rlang:::captureArg, substitute(dot))
#     arg <- rlang::eval_bare(capture, rlang::caller_env())
#     expr <- rlang::expr_interp(arg$expr, arg$env)
#     assign(as.character(dot), rlang::enquo(expr), envir)
#   }
# }