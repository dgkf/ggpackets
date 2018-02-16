#' Get Geom allowed aesthetics
#'
#' All ggplot2 \code{\link[ggplot2]{Geom}} objects have a set of allowable
#' aesthetics that are used for generating the layer of the plot. This function
#' returns those aesthetics, as well as any alternative localizations and base R
#' equivalent names which can all be used as inputs to specify aesthetics to a
#' ggplot object.
#'
#' @seealso [filter_aesthetics()], [remove_aesthetics()],
#'   [flatten_aesthetics_to_group()], [add_eqv_aes()], [split_aes_from_dots()],
#'   [is_uneval()]
#'
#' @param geom a ggplot2 \code{\link[ggplot2]{Geom}} object or NULL
#' @return a character vector of the names of aesthetics permitted by this
#'   geometry object (including alternative localizations of names and base R
#'   equivalent names also accepted by ggplot2. If \code{geom} is
#'   \code{NULL}, returns all of ggplot2's default allowed
#'   aesthetics.)
#'
#' @examples
#' library(ggplot2)
#' ggpackets:::allowed_aesthetics(GeomBar)
#'
#' @keywords aesthetics aes
#'   
allowed_aesthetics <- function(geom = NULL) {
  if (is.null(geom)) return(add_eqv_aes(.all_aesthetics))
  aes_names <- c('x', 'y', 'group', geom$required_aes, names(geom$default_aes))
  add_eqv_aes(aes_names)
}


#' Get allowed paramters for ggplot objects
#'
#' @param geom A Geom (or Stat) object
#'
#' @return the allowed paramters for the given object
#' @export
#'
allowed_params <- function(geom = NULL) {
    c(allowed_aesthetics(geom), geom$parameters())
}


#' Add equivalent aesthetics to vector
#' 
#' Many ggplot2 \code{\link[ggplot2]{Geom}} objects will accept a number of
#' alternative spellings and base R equivalent arguments as aesthetic
#' parameters. This function adds those equivalent values to a character vector
#' of aesthetic names.
#'   
#' @seealso [allowed_aesthetics()], [filter_aesthetics()],
#'   [remove_aesthetics()], [flatten_aesthetics_to_group()], [add_eqv_aes()],
#'   [split_aes_from_dots()], [is_uneval()]
#'   
#' @param aes_names a character vector of aesthetic names
#' @return a character vector of aesthetic names including any localized or base
#'   R equivalent argument names accepted by ggplot2.
#'   
#' @examples
#' ggpackets:::add_eqv_aes(c('color', 'fill'))
#' 
#' @keywords aesthetics aes
#'   
add_eqv_aes <- function(aes_names) {
  base_eqv_idx <- unlist(.base_to_ggplot) %in% aes_names |
                  names(.base_to_ggplot)  %in% aes_names
  base_eqv <- .base_to_ggplot[base_eqv_idx]
  aes_names_new <- unique(c(aes_names, names(base_eqv), unlist(base_eqv, use.names = F)))
  # attempt to add new equivalent mappings if anything new added 
  # (e.g. (color => colour) => fg)
  if (!(all(aes_names_new %in% aes_names))) add_eqv_aes(aes_names_new)
  else aes_names_new
}


#' Rename non-ggplot2 standard aesthetics to ggplot2 standard
#'
#' @param args a list of arguments which may or may not contain aesthetics
#'
#' @return the list with aesthetic names remapped to ggplot equivalents
#'   
rename_to_ggplot <- function(args) {
  names(args) <- to_ggplot(names(args))
  args
}

to_ggplot <- function(args) {
  ifelse(args %in% names(.base_to_ggplot), 
    unlist(.base_to_ggplot[args], use.names = FALSE), 
    args)
}

#' Filter aesthetics for Geom
#' 
#' Within ggpackets, often times many aesthetics will be passed to all packed 
#' layers without regard for whether or not their applicable. To accommodate 
#' this, it's often necessary to first filter down to only the applicable 
#' aesthetics for a given \code{\link[ggplot2]{Geom}} object to avoid erroneous 
#' warning messages.
#' 
#' @seealso [allowed_aesthetics()], [filter_aesthetics()],
#'   [remove_aesthetics()], [flatten_aesthetics_to_group()], [add_eqv_aes()],
#'   [split_aes_from_dots()], [is_uneval()]
#'   
#' @param geom a ggplot2 \code{\link[ggplot2]{Geom}} object
#' @param mapping a ggplot2 aesthetic mapping
#' @return the provided aesthetic mapping, filtered by accepted aesthetics for 
#'   the given \code{\link[ggplot2]{Geom}}
#'   
#' @examples
#' library(ggplot2)
#' filter_aesthetics(GeomBar, aes(x = test1, y = test2))
#'   
#' @keywords aesthetics aes
#'   
#' @export
filter_aesthetics <- function(geom, mapping) {
  allowed_aes <- allowed_aesthetics(geom)
  mapping_aes_names <- names(rename_to_ggplot(mapping))
  disallowed_aes <- setdiff(mapping_aes_names, allowed_aes)
  do.call(remove_aesthetics, c(list(mapping), disallowed_aes))
}



#' Filter arguments for given Geom and Stat objects
#' 
#' @param geom Geom object for which argument should be filtered
#' @param stat Stat object for which argument should be filtered
#' @param args argument list to be filtered
#' 
#' @importFrom ggplot2 layer
#' 
filter_args <- function(call, geom, stat, args) {
  if (any(c(class(geom), class(stat)) %in% 'ggproto')) {
    allowed_args <- c('',
      names(formals(ggplot2::layer)), 
      allowed_params(geom), 
      allowed_params(stat))
  } else {
    allowed_args <- c('', names(formals(call)))
  }
  
  argnames <- names(args) %||% rep('', length(args))
  args[argnames %in% allowed_args]
}


#' Flatten aesthetics into group
#' 
#' Occasionally, it may be necessary to enforce a group aesthetic from a 
#' selection of other aesthetics that are being passed to a packed layer. In 
#' this case, those aesthetics can be passed and preserved as an interaction 
#' term in the group aesthetic.
#' 
#' For example, if one would want to produce a line plot with an errorbar at 
#' every distinct point along the x-axis and use the linetype aesthetic to group
#' the lines of the line plot, this will be problematic because it may also 
#' affect the linetype of the errorbars. To accommodate this, the linetype might
#' be flattened into the group aesthetic, allowing for the linetype of the 
#' errorbars to be overridden without affecting the data groups.
#' 
#' @note the `x` and `y` aesthetics will not be pulled into the group aesthetic
#' 
#' @seealso [allowed_aesthetics()], [filter_aesthetics()],
#'   [remove_aesthetics()], [flatten_aesthetics_to_group()], [add_eqv_aes()],
#'   [split_aes_from_dots()], [is_uneval()]
#' 
#' @param mapping a ggplot2 aesthetic mapping
#' @param ... aesthetic handles to flatten into the group aesthetic as 
#'   interaction terms
#' @return the mapping aesthetics with specified aesthetics stored additionally
#'   as interaction terms in the group aesthetics with any preexisting group
#'   aesthetics
#'   
#' @examples 
#' library(ggplot2)
#' my.aes <- aes(x = mpg, y = wt, color = carb, fill = vs)
#' flatten_aesthetics_to_group(my.aes, 'color', 'fill')
#'   
#' @keywords aesthetics aes
#'   
#' @export
flatten_aesthetics_to_group <- function(mapping, ...) {
  .dots <- add_eqv_aes(unlist(list(...)))
  mapped_vars <- mapping[!(names(mapping) %in% c('x', 'y'))]
  if (length(.dots) > 0)
    mapped_vars <- mapped_vars[names(mapped_vars) %in% c(list('group'), .dots)]
  mapped_vals <- unique(unlist(mapped_vars, use.names=FALSE))
  if (length(mapped_vals) > 0)
    mapping$group <- as.call(c(list(as.symbol("interaction")), mapped_vals))
  mapping
}


#' Filter arguments into aesthetic and non-aesthetic
#' 
#' To accommodate passing of aesthetics as equivalent ellipses arguments without
#' the need for wrapping in an \code{\link[ggplot2]{aes}} call, it's often 
#' necessary to filter out any arguments which share the name of an acceptable 
#' aesthetic argument for the given \code{\link[ggplot2]{Geom}}. In this case,
#' arguments are split into two separate lists and returned within a list.
#' 
#' @seealso [allowed_aesthetics()], [filter_aesthetics()],
#'   [remove_aesthetics()], [flatten_aesthetics_to_group()], [add_eqv_aes()],
#'   [split_aes_from_dots()], [is_uneval()]
#' 
#' @param ... arguments containing aesthetics mapping
#' @param geom if specified, a ggplot2 \code{\link[ggplot2]{Geom}} object can be
#'   passed to filter aesthetics for only a specified
#'   \code{\link[ggplot2]{Geom}}.
#' @return a list containing two named elements: 'aes' which contains the 
#'   aesthetic mappings (of the \code{\link[ggplot2]{Geom}} object if specified)
#'   as a pairlist with class `uneval` akin to the result of an 
#'   \code{\link[ggplot2]{aes}} call, and 'not_aes' which contains a pairlist of
#'   all other arguments
#'   
#' @examples 
#' library(ggplot2)
#' split_aes_from_dots(x = aes_x, my.arg.2 = 'my arg', color = aes_color, 
#'   fill = 'aes_fill', my.arg.3 = list(1, 2, 3), geom = GeomBar)
#'   
#' @keywords aesthetics aes
#'   
#' @export
split_aes_from_dots <- function(..., geom = NULL) {
  if (is.null(geom)) aes_list <- .all_aesthetics
  else aes_list <- allowed_aesthetics(geom)
  aes_args     <- structure(substitute(...()), class = 'uneval')
  not_aes_args <- aes_args[!names(aes_args) %in% aes_list]
  aes_args     <- aes_args[names(aes_args) %in% aes_list]
  list(aes = do.call(ggplot2::aes, aes_args),
       not_aes = not_aes_args)
}


#' Remove specific aesthetics from mapping
#' 
#' Remove specified aesthetics by name from a ggplot2 aesthetic mapping,
#' returning the same mapping with the specified aesthetics removed an preserved
#' as an interaction term in the group aesthetic.
#'   
#' @seealso [allowed_aesthetics()], [filter_aesthetics()], 
#'   [remove_aesthetics()], [flatten_aesthetics_to_group()], [add_eqv_aes()], 
#'   [split_aes_from_dots()], [is_uneval()]
#'   
#' @param mapping aesthetic mapping to use for filtering
#' @param ... mapping names to filter out
#' @return an aesthetic mapping with filtered mappings removed and group mapping
#'   set as interaction terms of all non axial (non- x, y) terms.
#'   
#' @examples
#' library(ggplot2)
#' remove_aesthetics(aes(x = wt, y = mpg, color = carb, fill = wt), 'fill')
#' 
#' @keywords aesthetics aes
#'   
#' @export
remove_aesthetics <- function(mapping, ...) {
  mapping <- flatten_aesthetics_to_group(mapping, ...)
  mapping[!(names(mapping) %in% list(...))]
}


#' Determine if an argument is an unevaluated expression
#' 
#' An argument is considered unevaluated if it returns `TRUE` for any of 
#' `is.name()`, `is.call()` or `is.expression()`
#' 
#' @note This function may prove to be a heavy limitation of ggpackets if it is 
#'   unable to accommodate custom unevaluated expression classes. Input on how 
#'   best to handle such cases is welcome, but until a global solution is found,
#'   feedback on what other classes to consider is appreciated.
#'   
#' @seealso [allowed_aesthetics()], [filter_aesthetics()], 
#'   [remove_aesthetics()], [flatten_aesthetics_to_group()], [add_eqv_aes()], 
#'   [split_aes_from_dots()], [is_uneval()]
#'   
#' @param arg the input object to be tested
#' @return `TRUE` if the value is of type `name`, `call` or `expression`
#'   
#' @examples
#' ggpackets:::is_uneval(as.symbol('test'))
#' ggpackets:::is_uneval('test')
#' 
#' @keywords aesthetics aes
#'   
is_uneval <- function(arg) { 
  any(is.name(arg), is.call(arg), is.expression(arg)) 
}
