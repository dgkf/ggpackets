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
#' @inheritParams base::names
#' 
#' @param l a list from which names should be pulled
#' @param fill a value to use for padding name values to length
#' 
tsnames <- function(l, fill = NA) {
  names(l) %||% rep(fill, length(l))
}



#' Safe use of package:crayon exports with fallback to paste(...)
#' 
#' @param fname package:crayon function name to call
#' @inheritParams base::paste
#'
#' @importFrom grDevices rgb
#'
safecrayon <- function(fname, ...) { 
  if (requireNamespace('crayon', quietly = TRUE)) { 
    if (fname %in% getNamespaceExports('crayon')) { 
      getExportedValue('crayon', fname)(...)
    } else { 
      make_style <- getExportedValue('crayon', 'make_style')
      switch(fname,
        darkblue = make_style(grDevices::rgb(0/8, 0/8, 3/8))(...),
        midred =   make_style(grDevices::rgb(5/8, 0/8, 0/8))(...),
        grey2 =    make_style(grDevices::rgb(2/8, 2/8, 2/8))(...),
        grey4 =    make_style(grDevices::rgb(4/8, 4/8, 4/8))(...),
        grey6 =    make_style(grDevices::rgb(6/8, 6/8, 6/8))(...),
        grey7 =    make_style(grDevices::rgb(7/8, 7/8, 7/8))(...),
        paste(...))
    }
  } else { 
    paste(...)
  }
}



#' Safe use of crayon's col_nchar() with base nchar() fallback
#' 
#' @param ... passed to \code{nchar}
#' @inheritParams base::nchar
#' 
safenchar <- function(...) {
  if (requireNamespace('crayon', quietly = TRUE)) 
    getExportedValue('crayon', 'col_nchar')(...)
  else nchar(...)
}



#' Safe use of crayon's col_substr() with base substring() fallback
#'
#' @param ... passed to \code{substring}
#' @inheritParams base::substring
#'
safesubstr <- function(...) {
  if (requireNamespace('crayon', quietly = TRUE)) 
    getExportedValue('crayon', 'col_substr')(...)
  else substring(...)
}