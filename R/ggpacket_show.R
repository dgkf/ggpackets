#' Display contents of a ggpacket
#'
#' @param object A ggpacket object to show.
#'
#' @rdname ggpacket-methods
#' @aliases show, ggpacket-method
#'
#' @importFrom ggplot2 ggplot ggplot_build
#' @importFrom methods show
#'
setMethod("show", "ggpacket", function(object) {
  warnings <- list()
  ggout <- tryCatch(withCallingHandlers(
    ggplot2::ggplot_build(ggobj <- ggplot2::ggplot() + object),
    warning = function(w) {
      append(warnings, w)
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      e
    })

  if (!inherits(ggout, "error") && length(ggout$data) && nrow(ggout$data[[1]])) {
    for (w in warnings) warning(w)
    print(ggobj)
  } else {
    if (getOption("ggpackets.debug", FALSE)) stop(ggout)
    print(object)
  }
})

#' @export
print.ggpacket <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

#' @export
format.ggpacket <- function(x, ...) {
  req_aes <- required_aesthetics(x)
  missing_aes <- setdiff(req_aes, names(x@mapping))

  width <- getOption("width", 80) - 3L
  data_strs <- format_ggpacket_data(x@data, width)
  aes_strs <- format_ggpacket_mapping(x@mapping, width, missing_aes = missing_aes)
  layers_strs <- format_ggpacket_ggcalls(x@ggcalls)

  sprintf(multistr("
    <ggpacket>
    Data:
    %s
    Aesthetic Mapping:
    %s
    Layers:
    %s

    "),
    paste0("  ", data_strs, collapse = "\n"),
    paste0("  ", aes_strs, collapse = "\n"),
    paste0("  ", layers_strs, collapse = "\n"))
}

number_strings <- function(strs, width = getOption("width", 80) * 0.9,
    wrap = FALSE, ...) {

  if (!length(strs)) return(strs)
  nchars <- nchar(length(strs)) + 2 + 1

  if (wrap)
    strs <- lapply(strsplit(strs, "\n"), function(stri) {
      paste(strwrap(stri, ..., width = width - nchars), collapse = "\n")
    })

  unlist(strsplit(
    sprintf("%s[%d] %s",
      strrep(" ", nchar(length(strs)) - nchar(seq_along(strs))),
      seq_along(strs),
      gsub("\n", paste0("\n", strrep(" ", nchars)), strs)),
    "\n"))
}

format_ggpacket_data <- function(x,
    width = getOption("width", 80) * 0.9) {
  UseMethod("format_ggpacket_data")
}

format_ggpacket_data.NULL <- function(x,
    width = getOption("width", 80) * 0.9) {
  "awaiting data"
}

format_ggpacket_data.waiver <- function(x,
    width = getOption("width", 80) * 0.9) {
  "awaiting data"
}

#' @importFrom utils capture.output
format_ggpacket_data.default <- function(x,
    width = getOption("width", 80) * 0.9) {
  utils::capture.output(print(x, width = width))
}

#' @importFrom utils head
format_ggpacket_data.data.frame <- function(x,
    width = getOption("width", 80) * 0.9) {
  n <- 3L
  dfout <- format_ggpacket_data.default(utils::head(x, n), width = width)
  nr_omit <- nrow(x) - n
  c_omit <- length(dfout) - (n + 1L)
  c(dfout[1:(n + 1)],
    if (nr_omit > 0L || c_omit > 0L)
      sprintf("# \u2026 with %s%s%s",
        if (nr_omit) sprintf("%d more rows", nr_omit) else "",
        if (nr_omit && c_omit) ", " else "",
        if (c_omit) "columns omitted" else ""))
}

format_ggpacket_data.tbl <- function(x,
    width = getOption("width", 80) * 0.9) {
  format(x, n = 3L, width = width)
}

format_ggpacket_mapping <- function(x,
    width = getOption("width", 80) * 0.9, missing_aes = character(0L)) {
  UseMethod("format_ggpacket_mapping")
}

format_ggpacket_mapping.NULL <- function(x,
    width = getOption("width", 80) * 0.9, missing_aes = character(0L)) {
  format_ggpacket_mapping(aes(), width = width, missing_aes = missing_aes)
}

#' @importFrom utils capture.output
#' @importFrom crayon red
format_ggpacket_mapping.default <- function(x,
    width = getOption("width", 80) * 0.9, missing_aes = character(0L)) {

  x[missing_aes] <- " MISSING "
  gsub("\" MISSING \"", crayon::red("<missing>"), utils::capture.output(x)[-1])
}

format_ggpacket_ggcalls <- function(x,
    width = getOption("width", 80) * 0.9) {
  if (!length(x)) return("empty")
  UseMethod("format_ggpacket_ggcalls")
}

format_ggpacket_ggcalls.default <- function(x,
    width = getOption("width", 80) * 0.9) {
  number_strings(
    vapply(x, function(...) format_ggpacket_ggcall(...), character(1L)),
    width = width - nchar(length(x)) - 2,
    wrap = TRUE,
    exdent = 2L)
}

format_ggpacket_ggcall <- function(x,
    width = getOption("width", 80) * 0.9) {
  UseMethod("format_ggpacket_ggcall")
}

format_ggpacket_ggcall.default <- function(x,
    width = getOption("width", 80) * 0.9) {

  id_str <- if (length(attr(x, "ids"))) {
    paste0(paste0("#", attr(x, "ids"), collapse = " "), "\n")
  } else {
    character(0L)
  }

  x <- x[[1]] # unpack after using outer attributes
  if (!grepl("\\w", rlang::quo_get_expr(x[[1]])))
    return(paste(
      gsub("(^\\s*|\\s*$)", "",
        format(as.call(lapply(x, rlang::quo_get_expr)))),
      collapse = " "))

  non_breaking_space <- "\u00A0"

  args <- mapply(
    function(...) format_ggpacket_ggcall_arg(...),
    x,
    name = names(x),
    MoreArgs = list(width = width))

  sprintf("%s%s(%s)\n",
    id_str,
    args[[1]],
    paste(sprintf("%s%s",
        ifelse(names(args[-1]) == "", "",
          paste0(names(args[-1]), non_breaking_space, "=", non_breaking_space)),
        args[-1]),
      collapse = ", "))
}

format_ggpacket_ggcall_arg <- function(x,
    width = getOption("width", 80) * 0.9, name = NULL) {
  UseMethod("format_ggpacket_ggcall_arg", x)
}

format_ggpacket_ggcall_arg.quosure <- function(x,
    width = getOption("width", 80) * 0.9, name = NULL) {
  if (!is.null(name) && name == "mapping")
    return(format_ggpacket_ggcall_arg(rlang::eval_tidy(x)))
  xsq <- rlang::quo_squash(x)
  if (is.atomic(xsq)) return(deparse(xsq))
  tryCatch(format(xsq), error = function(e) as.character(xsq))
}

format_ggpacket_ggcall_arg.default <- function(x,
    width = getOption("width", 80) * 0.9, name = NULL) {
  format(x)
}

format_ggpacket_ggcall_arg.uneval <- function(x,
    width = getOption("width", 80) * 0.9, name = NULL) {
  sprintf("aes(%s)",
    paste0(
      sprintf("%s = %s", names(x), sapply(x, rlang::quo_squash)),
      collapse = ", "))
}

multistr <- function(s) {
  lines <- strsplit(s, "\n")[[1]]
  if (!length(lines)) return(s)
  if (!grepl("\\w", lines[[1]])) lines <- lines[-1]
  nchar_leader <- min(nchar(gsub("^(\\W*).*", "\\1", lines[grepl("\\w", lines)])))
  paste0(substring(lines, nchar_leader + 1L), collapse = "\n")
}
