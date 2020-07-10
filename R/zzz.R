.onLoad <- function(libname, pkgname) {
  requireNamespace("ggplot2", quietly = TRUE)

  setHook(packageEvent("ggplot2", "onLoad"), action = "append", function(...) {
    packageStartupMessage(
      "\nIt looks like you're loading `ggplot2` after `ggpackets`. ",
      "For `ggpackets` to work properly, it must be loaded after `ggplot2`. ",
      "You can correct this by running:\n\n",
      "    detach(\"package:ggpackets\", unload = TRUE)\n",
      "    library(ggpackets)")
  })
} 
