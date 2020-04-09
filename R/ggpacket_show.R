#' @importFrom ggplot2 ggplot ggplot_build
#' @export
setMethod("show", "ggpacket", function(object) {
  ggout <<- tryCatch(
    ggplot2::ggplot_build(ggplot2::ggplot() + object), 
    error = function(e) e)

  if (!inherits(ggout, "error") && length(ggout$data) && nrow(ggout$data[[1]])) {
    show(ggplot2::ggplot() + object)
  } else {
    print(object)
  }


})

#' @export
print.ggpacket <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

format.ggpacket <- function(x, ...) {
  width <- getOption("width", 80) - 3L
  data_strs <- format_ggpacket_data(x@data, width)
  aes_strs <- format_ggpacket_mapping(x@mapping, width)
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

number_strings <- function(strs, width = getOption("width", 80) * 0.9) {
  sprintf("%s[%d] %s",
    strrep(" ", nchar(length(strs)) - nchar(seq_along(strs))),
    seq_along(strs),
    strs)
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

format_ggpacket_data.default <- function(x, 
    width = getOption("width", 80) * 0.9) {
  capture.output(print(x, width = width))
}

format_ggpacket_data.data.frame <- function(x,
    width = getOption("width", 80) * 0.9) {
  n <- 3L
  dfout <- format_ggpacket_data.default(head(x, n), width = width)
  nr_omit <- nrow(x) - n
  c_omit <- length(dfout) - (n + 1L)
  c(dfout[1:(n + 1)],
    if (nr_omit > 0L || c_omit > 0L)
      sprintf("# … with %s%s%s",
        if (nr_omit) sprintf("%d more rows", nr_omit) else "", 
        if (nr_omit && c_omit) ", " else "",
        if (c_omit) "columns omitted" else ""))
}

format_ggpacket_data.tbl <- function(x, 
    width = getOption("width", 80) * 0.9) {
  format(x, n = 3L, width = width)
}

format_ggpacket_mapping <- function(x, 
    width = getOption("width", 80) * 0.9) {
  UseMethod("format_ggpacket_mapping")
}

format_ggpacket_mapping.NULL <- function(x, 
    width = getOption("width", 80) * 0.9) {
  "awaiting aesthetics"
}

format_ggpacket_mapping.default <- function(x, 
    width = getOption("width", 80) * 0.9) {
  capture.output(print(x, width = width))[-1]
}

format_ggpacket_ggcalls <- function(x,
    width = getOption("width", 80) * 0.9) {
  UseMethod("format_ggpacket_ggcalls")
}

format_ggpacket_ggcalls.default <- function(x,
    width = getOption("width", 80) * 0.9) {
  strs <- number_strings(sapply(x, 
    function(...) format_ggpacket_ggcall(...), 
    width = width - nchar(length(x)) - 2))
  leading_chars <- gsub("^(\\s*).*", "\\1", strs)
  unlist(mapply(function(str, leading_chars) {
    paste0(
      leading_chars, 
      strwrap(str, exdent = nchar(length(strs)) + 4L, width = width))
  }, str = strs, leading_chars = leading_chars))
}

format_ggpacket_ggcall <- function(x,
    width = getOption("width", 80) * 0.9) {
  UseMethod("format_ggpacket_ggcall")
}

format_ggpacket_ggcall.default <- function(x,
    width = getOption("width", 80) * 0.9) {
  non_breaking_space <- "\u00A0"
  id_str <- if (length(attr(x, "ids")))
      paste0("#", attr(x, "ids"), collapse = " ")
    else
      character(0L)
  args <- lapply(x, function(x) format_ggpacket_ggcall_arg(x, width = width))
  sprintf("%s%s(%s)\n", 
    if (nchar(id_str)) paste0(id_str, "  ") else "",
    args[[1]], 
    paste(sprintf("%s%s=%s%s", 
        names(args[-1]), 
        non_breaking_space,
        non_breaking_space,
        args[-1]), 
      collapse = ", "))
}

format_ggpacket_ggcall_arg <- function(x,
    width = getOption("width", 80) * 0.9) {
  UseMethod("format_ggpacket_ggcall_arg")
}

format_ggpacket_ggcall_arg.quosure <- function(x, 
    width = getOption("width", 80) * 0.9) {
  xsq <- rlang::quo_squash(x)
  if (is.atomic(xsq)) return(deparse(xsq))
  tryCatch(format(xsq), error = function(e) as.character(xsq))
}

format_ggpacket_ggcall_arg.default <- function(x,
    width = getOption("width", 80) * 0.9) {
  format(x)
}

multistr <- function(s) {
  lines <- strsplit(s, "\n")[[1]]
  if (!length(lines)) return(s)
  if (!grepl("\\w", lines[[1]])) lines <- lines[-1]
  nchar_leader <- min(nchar(gsub("^(\\W*).*", "\\1", lines)))
  paste0(substring(lines, nchar_leader + 1L), collapse = "\n")
}
