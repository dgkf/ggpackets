#' Wrapper for common decorators to package
#'   
#' @param ... arguments to be passed to any of the bundled decorator plot 
#'   functions, blank, xlab, ylab, labs, theme or ggtitle prefaced by any of
#'   those strings ('labs.title').
#'   
#' @return a ggpacket object containing a collection of decorating plot 
#'   functions.
#'   
#' @examples 
#' library(ggplot2)
#' ggplot(mtcars) + 
#'   aes(x = carb) + 
#'   geom_bar() + 
#'   ggpk_decorators(
#'     xlab = 'Number of Carburetors',
#'     ylab = 'Count of Cars',
#'     labs.title = 'Distribution of Carburetors',
#'     labs.caption = '* taken from 1974 Motor Trends US')
#' 
#' @importFrom ggplot2 xlab ylab labs theme ggtitle
#' @export
ggpk_decorators <- function(...) {
  ggpack(ggplot2::xlab,   id = 'xlab',    ..., null_empty = TRUE) +
  ggpack(ggplot2::ylab,   id = 'ylab',    ..., null_empty = TRUE) +
  ggpack(ggplot2::labs,   id = 'labs',    ..., null_empty = TRUE) +
  ggpack(ggplot2::theme,  id = 'theme',   ..., null_empty = TRUE) + 
  ggpack(ggplot2::ggtitle,id = 'ggtitle', ..., null_empty = TRUE)
}

#' Say thanks!
#' 
#' @param ... aesthetic arguments to pass
#' @param xmin minimum x value to fill with text
#' @param xmax maximum x value to fill with text
#' @param ymin minimum y value to fill with text
#' @param ymax maximum y value to fill with text
#' 
#' @importFrom stats rnorm
#' @importFrom ggplot2 geom_path geom_point
#' @export
ggpk_ty <- function(..., xmin = 0, xmax = 1, ymin = 0, ymax = 1) { 
  points <- list(
    list(x = c(0.0, 0.8), y = c(1.0, 1.0), l = 'T'), 
    list(x = c(0.4, 0.4), y = c(0.0, 1.0), l = 'T', n = 10),
    list(x = c(1.0, 1.0), y = c(0.0, 1.0), l = 'H', n = 10),
    list(x = c(1.8, 1.8), y = c(0.0, 1.0), l = 'H', n = 10),
    list(x = c(1.0, 1.8), y = c(0.5, 0.5), l = 'H'),
    list(x = c(2.0, 2.4), y = c(0.0, 1.0), l = 'A', n = 10),
    list(x = c(2.4, 2.8), y = c(1.0, 0.0), l = 'A', n = 10),
    list(x = c(2.2, 2.6), y = c(0.5, 0.5), l = 'A', n = 3),
    list(x = c(3.0, 3.0), y = c(0.0, 1.0), l = 'N', n = 10),
    list(x = c(3.8, 3.8), y = c(0.0, 1.0), l = 'N', n = 10),
    list(x = c(3.0, 3.8), y = c(1.0, 0.0), l = 'N', n = 12),
    list(x = c(4.0, 4.0), y = c(0.0, 1.0), l = 'K', n = 10),
    list(x = c(4.0, 4.8), y = c(0.5, 1.0), l = 'K', n = 5),
    list(x = c(4.0, 4.8), y = c(0.5, 0.0), l = 'K', n = 5),
    
    list(x = c(6.4, 6.4), y = c(0.0, 0.5), l = 'Y', n = 5),
    list(x = c(6.4, 6.0), y = c(0.5, 1.0), l = 'Y', n = 5),
    list(x = c(6.4, 6.8), y = c(0.5, 1.0), l = 'Y', n = 5),
    list(x = c(7.0, 7.8), y = c(1.0, 1.0), l = 'O', n = 5),
    list(x = c(7.8, 7.8), y = c(1.0, 0.0), l = 'O', n = 10),
    list(x = c(7.8, 7.0), y = c(0.0, 0.0), l = 'O', n = 5),
    list(x = c(7.0, 7.0), y = c(0.0, 1.0), l = 'O', n = 10),
    list(x = c(8.8, 8.8), y = c(1.0, 0.0), l = 'U', n = 10),
    list(x = c(8.8, 8.0), y = c(0.0, 0.0), l = 'U', n = 5),
    list(x = c(8.0, 8.0), y = c(0.0, 1.0), l = 'U', n = 10)
  )
  
  ty <- do.call(rbind, Map(function(a, i) 
    data.frame(list(
      x = seq(a$x[1], a$x[2], length.out = a$n %||% 5), 
      y = seq(a$y[1], a$y[2], length.out = a$n %||% 5),
      i = rep(i, a$n %||% 5), 
      l = rep(a$l %||% '0', a$n %||% 5))), 
    points,
    i = 1:length(points)
  ))
  
  ty$x <- ty$x/9*(xmax-xmin) + xmin + stats::rnorm(nrow(ty), sd = max(xmax-xmin, ymax-ymin)/800)
  ty$y <- ty$y*(ymax-ymin) + ymin  + stats::rnorm(nrow(ty), sd = max(xmax-xmin, ymax-ymin)/800)
  ty$size <- stats::rnorm(nrow(ty), sd = max(xmax-xmin, ymax-ymin))
  
  ggpack(ggplot2::geom_path, id = 'path', size = 2, alpha = 0.8, ..., data = ty,
    mapping = ggplot2::aes_string(x = 'x', y = 'y', group = 'i', color = 'l'), inherit.aes = FALSE) + 
  ggpack(ggplot2::geom_point, id = 'point', alpha = 0.25, ..., data = ty,
    mapping = ggplot2::aes_string(x = 'x', y = 'y', group = 'i', color = 'l', size = 'size'), inherit.aes = FALSE)
}