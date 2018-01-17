#' Generate roxygen ellipses param documentation from function code
#'
#' @param f function to generate ellipses documentation for
#' @param header whether to include a brief intro, adding context to arg list
#' @param ... additional arugments used for substituting unknown variables
#' @param prefix prefix to insert before lines of documentation
#' @param dots_as_ellipses build documentation for arguments passed via dots
#'   argument
#'
#' @return a block of roxygen text for documenting the ellipses parameter
#'
#' @examples
#' document_ggpk(ggpk_ty)
#'
#' @importFrom utils capture.output tail getAnywhere
#' @export
document_ggpk <- function(f, header = TRUE, ..., prefix = "#' ", 
    dots_as_ellipses = FALSE) {
  
  dots <- list(...)
  exp <- as.expression(f)
  
  ## Parse calls for call data
  ggpacks <- Map(function(ggpack_call) { 
    call_args <- as.list(ggpack_call)[-1]
    id <- as.list(call_args$id)
    ggcall <- call_args[[1]]
    
    # package and call name
    if (is.call(ggcall) && ggcall[[1]] == '::') { 
      pkg <- as.character(ggcall[[2]])
      f <- as.character(ggcall[[3]])
    } else { 
      pkg <- utils::getAnywhere(as.character(ggcall))$where
      pkg <- gsub('^.*:', '', grep('^package:', pkg, value = TRUE))
      f <- as.character(ggcall)
    }
    
    # ids in call
    ids <- if (length(id) > 0 && id[1] %in% c('list', 'c')) id[-1] else id
    
    # reconcile ids
    ids <- Map(function(id) {
      if (is.character(id) || !length(id)) return(id)
      id <- as.character(id)
      if (id %in% names(dots)) return(dots[[id]])
      readline(sprintf(
        'Unknown name "%s" used for id argument. What id should be used? ', id))
    }, ids)
    
    # index of default id value if it's inside the ids list
    def <- which(ids %in% list(formals(ggpack)$id))
    
    # parameters passed after ellipses
    if (any(fix <- utils::tail(which(call_args %in% "..."), 1)) && fix < length(call_args)) {
      fix <- names(call_args[(fix+1):length(call_args)])
      fix <- setdiff(fix, names(formals(ggpack)))
    } else if (any(fix <- utils::tail(which(names(call_args) %in% 'dots'), 1))) {
      if (!dots_as_ellipses) {
        message('No ellipses args found in call to ggpack()')
        message(gsub('\\s+', ' ', paste(capture.output(as.call(ggpack_call)), collapse = ' ')))
        dots_as_ellipses_i <- tolower(readline('Consider arguments after dots argument fixed? [Y/n]: ')) %in% c('y', '')
      }
      if (dots_as_ellipses || dots_as_ellipses_i) {
        fix <- names(call_args[(fix+1):length(call_args)])
        fix <- setdiff(fix, names(formals(ggpack)))
      } else fix <- NULL
    }
    fix <- Filter(Negate(is.na), fix)
    
    list(call = call_args, f = f, pkg = pkg, ids = ids, def = def, fix = fix)
  }, find_calls(exp, "ggpack"))
  
  ## build @param roxygen tag from call data
  header_txt <- paste0(
    '@param ... ',
    if (length(f <- Filter(function(i) length(i$def), ggpacks))) paste0(
      'All captured arguments will be passed to calls to ',
      str_format_list(sprintf('\\code{\\link[%s]{%s}}', 
        sapply(f, "[[", "pkg"), sapply(f, "[[", "f"))),
      '.\n\n'),
    'Arguments prefixed with the following \\code{id}s will be parsed and passed ',
    'to the following calls: \n'
    , collapse = ' ')
  
  items_txt <- c(
    '\\itemize{\n',
    paste(Map(function(i) { sprintf(
      '\\item %s id%s: Passed to call to \\code{\\link%s{%s}}. %s\n',
      paste0('\\strong{', i$ids[(-i$def) %||% TRUE], '}', collapse = ', '),
      if (length(i$ids[(-i$def) %||% TRUE]) > 1) 's' else '',
      if (length(i$pkg)) paste0('[', i$pkg, ']') else '', 
      as.character(i$f),
      if (length(i$fix)) paste(str_format_list(sprintf('\\code{%s}', i$fix)), 'fixed.') else ''
    )}, ggpacks), 
    sep = '\n\n'),
    '}'
  )
  
  if (header) cat(paste(
    prefix, 
    strwrap(header_txt, width = 80-nchar(prefix), exdent = 2),
    collapse = '\n', sep = ''), '\n')
  cat(paste(
    prefix,
    paste0(rep(' ', 2*header), collapse = ''), 
    strwrap(items_txt, width = 80-nchar(prefix)-2, exdent = 2),
    collapse = '\n', sep = ''), '\n')
  
  invisible(ggpacks)
}


#' Coersion from function to expression
#'
#' @param x the function handle of the function to coerce
#' @param ... additional arguments will be ignored
#'
#' @return the expression representation of the function body
#' @export
#'
as.expression.function <- function(x, ...) {
  if (length(list(...))) warning('additional arguments will be ignored.')
  txt <- utils::capture.output(x)
  txt <- gsub('^<[^:]+: [^>]+>[\t\r\n\v\f]*$', '', txt)
  txt <- paste(txt, collapse = '\n')
  parse(text = txt)
}


#' Find calls to specified function name
#'
#' @param x expression to parse
#' @param call_name call name to search for
#'
#' @return calls introduced in expression
#' @export
#'
find_calls <- function(x, call_name = NULL) {
  if (is.expression(x)) {
    find_calls(x[[1]], call_name)
  } else if (is.atomic(x) || is.name(x)) {
    return()
  } else if (is.call(x)) {
    if (identical(x[[1]], as.name(call_name))) x
    else unlist(lapply(x, function(f) find_calls(f, call_name)))
  } else if (is.pairlist(x)) {
    unlist(lapply(x, function(f) find_calls(f, call_name)))
  } else {
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}
