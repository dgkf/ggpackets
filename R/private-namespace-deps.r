.all_aesthetics <- tryCatch({
    # attempt to stay current with ggplot .all_aesthetics upstream
    get('.all_aesthetics', asNamespace('ggplot2'), inherits = FALSE)
  
  }, error = function(e) {
    # hard coded fallback in case upstream changes private variable name
    # #est for fallback viability included in testthat tests
    c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", "fg", 
      "fill", "group", "hjust", "label", "linetype", "lower", "lty", "lwd", 
      "max", "middle", "min", "pch", "radius", "sample", "shape", "size", "srt", 
      "upper", "vjust", "weight", "width", "x", "xend", "xmax", "xmin", 
      "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")
  })

.base_to_ggplot <- tryCatch({
    # attempt to stay current with ggplot .base_to_ggplot upstream
    get('.base_to_ggplot', asNamespace('ggplot2'), inherits = FALSE)
  }, error = function(e) { 
    # hard coded fallback in case upstream changes private variable name
    # test for fallback viability included in testthat tests
    c(col = "colour", color = "colour", pch = "shape", cex = "size", 
      lty = "linetype", lwd = "size", srt = "angle", adj = "hjust", bg = "fill", 
      fg = "colour", min = "ymin", max = "ymax")
  })

# removed enquos function
# .captureArg <- get('captureArg', asNamespace('rlang'), inherits = FALSE)