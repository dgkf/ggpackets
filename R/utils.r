#' if not NULL else
#' @param a left hand side of operator
#' @param b right hand side of operator
#' @return b if a is null, otherwise a
#' 
#' @name if-not-null-else
#' @rdname if-not-null-else
#' 
`%||%` <- function(a, b) { 
  if (missing(a) || is.null(a) || !length(a)) b else a 
}

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

#' Type-safe 'names()' call, always returning a character vector
#' (potentially containing NA's where list elements are unnamed)
#' 
tsnames <- function(l, fill = NA) {
  names(l) %||% rep(fill, length(l))
}

#' Safe use of package:crayon exports with fallback to paste(...)
#'
safecrayon <- function(fname, ...) { 
  if (require(crayon, quietly = TRUE)) { 
    if (fname %in% getNamespaceExports('crayon')) { 
      getExportedValue('crayon', fname)(...)
    } else { 
      switch(fname,
        darkblue = crayon::make_style(rgb(0/8, 0/8, 3/8))(...),
        midred = crayon::make_style(rgb(5/8, 0/8, 0/8))(...),
        grey2 = crayon::make_style(rgb(2/8, 2/8, 2/8))(...),
        grey4 = crayon::make_style(rgb(4/8, 4/8, 4/8))(...),
        grey6 = crayon::make_style(rgb(6/8, 6/8, 6/8))(...),
        grey7 = crayon::make_style(rgb(7/8, 7/8, 7/8))(...),
        paste(...))
    }
  } else { 
    paste(...)
  }
}

safenchar <- function(...) {
  if (require(crayon, quietly = TRUE)) crayon::col_nchar(...)
  else nchar(...)
}

safesubstr <- function(...) {
  if (require(crayon, quietly = TRUE)) crayon::col_substr(...)
  else substring(...)
}