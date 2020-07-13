.onLoad <- function(libname, pkgname) {
  setHook(packageEvent("ggplot2", "onLoad"), action = "append", function(...) {
    packageStartupMessage(
      "\nIt looks like you're loading `ggplot2` after `ggpackets`. ",
      "For `ggpackets` to work properly, it must be loaded after `ggplot2`. ",
      "You can correct this by running:\n\n",
      "    detach(\"package:ggpackets\", unload = TRUE)\n",
      "    library(ggpackets)")
  })

  # Some current base functions are used which are not included in older
  # versions of R. These are provided through an 'Enhances' pacakge,
  # "backports" but this dependency is unnecessary otherwise. 
  if (package_version(R.Version()) < package_version("3.5") && 
      requireNamespace("backports")) {
    backports::import(pkgname, "isFALSE")
  }
} 
