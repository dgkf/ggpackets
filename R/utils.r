#' if not NULL else
#' @param a left hand side of operator
#' @param b right hand side of operator
#' @return b if a is null, otherwise a
#' 
#' @name if-not-null-else
#' @rdname if-not-null-else
#' 
`%||%` <- function(a, b) { if (is.null(a) || !length(a)) b else a }

#' Format a list of items as it would be in a sentence
#'
#' @param l a character vector list of strings to format
#' @param oxford whether to include an oxford comma
#'
#' @return the items of the input list, formatted as an articulation of a list
#'   in speech (e.g. 'a, b, c and d')
#' @export
#'
#' @examples
#' str_format_list(c('a', 'b', 'c'))
#' ## [1] "a, b and c"
str_format_list <- function(l = NULL, oxford = FALSE) {
  if (!length(l)) return('')
  junctions <- c(rep(', ', length(l)-1), paste0(if (oxford) ',', ' and '))
  paste(tail(c(rbind(junctions, l)), -1), collapse = '')
}