#' A class for storing ggpack call contents for lazy evaluation
#'   
#' @examples 
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
    ggcall = "ANY", 
    ggargs = "list",
    geom = "ANY",
    stat = "ANY",
    ggpackargs = "list"),
  prototype = list(
    id = list(NULL), 
    ggcall = NULL, 
    ggargs = list(),
    geom = NULL,
    stat = NULL,
    ggpackargs = list())
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
  function(.Object, ggcall, ggargs = list(), id = NULL, 
      geom = NULL, stat = NULL, ggpackargs = list()) {
    .Object@id = as.list(id)
    .Object@ggcall = ggcall
    .Object@ggargs = ggargs
    .Object@geom = geom
    .Object@stat = stat
    .Object@ggpackargs = ggpackargs
    .Object
  }
)


#' @rdname ggpacked_layer-equivalence
setMethod("==", c("ggpacked_layer", "ggpacked_layer"), function(e1, e2) {
  do.call(all, Map(`==`, e1@id, e2@id)) &&
  deparse(e1@ggcall) == deparse(e2@ggcall) &&
  class(e1@geom)     == class(e2@geom) &&
  class(e1@stat)     == class(e2@stat) &&
  
  # compare all ggpack args except for envir
  do.call(all, Map(`==`, 
  e1@ggpackargs[-which(names(e1@ggpackargs) %in% 'envir')], 
  e2@ggpackargs[-which(names(e2@ggpackargs) %in% 'envir')])) && 
  
  # compare cell-wise across entire ggargs tibble
  (!length(e1@ggargs) && !length(e2@ggargs)) || 
  do.call(all, Map(function(e1c, e2c) {
    (!length(e1c) && !length(e2c)) || 
    do.call(all, Map(`==`, e1c, e2c))
  }, e1@ggargs, e2@ggargs))
})


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
  
  args <- object@ggargs
  sources <- args$source
  args <- with(args, setNames(val, name))
  
  src_chr <- max(nchar(sources), 0)
  
  cat(safecrayon('cyan', 
    deparse(object@ggpackargs$callname),  
    ' (', class(object@ggcall)[[1]], ')',
    ': \n', sep = ''))
  
  cat(paste0('  ', 
    Map(function(a, an, i, s) {
      # pretty print source info
      src <- paste0(safecrayon('grey4', s), safecrayon('grey7', 
        if ((pad <- src_chr - nchar(s) + 3) > 2) 
          paste0(' ', paste0(rep('.', pad-1), collapse = ''))
        else 
          paste0(rep(' ', pad), collapse = '')))
      
      # pretty print argument info
      if (is_uneval(a)) {
        out <- paste0(trimws(deparse(a)), collapse = ' ')
        if (grepl('^\\.\\..+\\.\\.$', out)) 
          out <- paste0(
            '..',
            safecrayon('cyan', gsub('^\\.\\.(.+)\\.\\.$', '\\1', out)),
            '..',
            safecrayon('cyan', ' ⤴'))
      } else 
        out <- deparse(a)
      
      # color code argument overloading
      if (i == tail(which(names(args) %in% an), 1)) col <- 'grey2'
      else col <- 'grey6'
      
      # format unnamed arguments appropriately
      if (is.na(an) || an == '') 
        out <- safecrayon(col, an, out, collapse = '', sep = '')
      else 
        out <- safecrayon(col, an, '=', out, collapse = '')
      
      out <- paste(src, out)
      
      # limit to line length
      if (safenchar(out) > (w <- getOption('width', default=80)))
        out <- paste0(safesubstr(out, 0, w-3), safecrayon('grey6', '...'))
      
      paste0(out, '\n')
    }, 
    args, 
    tsnames(args, fill = ''), 
    seq(length.out = length(args)),
    sources), collapse = ''))
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
#' @rdname ggpacked_layer-addition
setMethod("+", c("gg", "ggpacked_layer"), function(e1, e2) {
  # extract parameters for ggpack constructions
  list2env(e2@ggpackargs, environment())
  args <- e2@ggargs
  
  e1mappingdf <- tibble(
      name = tsnames(e1$mapping, fill = ''), 
      val = e1$mapping, 
      source = 'inherited mapping')
    
  # flatten incoming mapping into ggplot aesthetics (also annotate their source)
  i <- max(head(which(names(args) != ''), 1) %||% length(args)-1, 0)
  args <- append_df(args, e1mappingdf, i)
  
  # unpack args tibble into argument list
  args <- with(args, setNames(val, name))
    
  # evaluate reserved aesthetics 
  # e.g. list(color = a, fill = ..color..) => list(color = a, fill = a)
  args <- replace_reserved_aesthetic_references(args)
  
  # strip last and flatten args to mapping, remove extra aes
  args <- args[last_args(names(args))]
  args <- flatten_aes_to_mapping(args, allowed_aesthetics(e2@geom), auto_remove_aes, envir)
  
  if (is.function(e2@ggcall) && !'mapping' %in% names(formals(e2@ggcall)))
    args <- args[!names(args) %in% 'mapping']
  
  # neglect construction if empty args means empty layer
  if (null_empty && length(args) == 0) return(e1)
  
  # remove invalid argnames and unevaluated aesthetics
  if (auto_remove_aes)
    args <- args[!uneval_aes(args) | names(args) %in% '']
    args <- filter_args(e2@ggcall, e2@geom, e2@stat, args)
  
  # filter out remaining named unevaluated arguments
  args <- Map(function(v, k)
    if (is_uneval(v) && k != '') eval(v, envir) else v,
    args, tsnames(args, ''))
  
  if (is.function(e2@ggcall)) e1 + do.call(e2@ggcall, args)
  else                        e1 + eval(e2@ggcall)
})


