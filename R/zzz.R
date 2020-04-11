.onLoad <- function(libname, pkgname) {
  require("ggplot2")

  setHook(packageEvent("ggplot2", "onLoad"), action = "append", function(...) {
    message(
      "\nIt looks like you're loading `ggplot2` after `ggpackets`. ",
      "For `ggpackets` to work properly, it must be loaded after `ggplot2`. ",
      "You can correct this by running:\n\n",
      "    detach(\"package:ggpackets\", unload = TRUE)\n",
      "    library(ggpackets)")
  })
} 
