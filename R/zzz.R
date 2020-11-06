.onLoad <- function(libname, pkgname) {
  if (requireNamespace("ggplot2") && !"ggplot2" %in% .packages()) {
    attachNamespace("ggplot2")
  } else {
    setHook(
      packageEvent("ggplot2", "attach"), 
      action = "append", 
      function(...) {
        packageStartupMessage("\n",
          paste(collapse = "\n", strwrap(indent = 2L, paste0(
            "It looks like you're attaching `ggplot2` after `ggpackets`. ",
            "For `ggpackets` to work properly, it must be attached after ", 
            "`ggplot2`. You can correct this by running:"))),
            "\n\n",
            "    detach(\"package:ggpackets\", unload = TRUE)\n",
            "    library(ggpackets)")
      })
  }

  # Some current base functions are used which are not included in older
  # versions of R. These are provided through an 'Enhances' pacakge,
  # "backports" but this dependency is unnecessary otherwise. 
  if (package_version(R.Version()) < package_version("3.5") && 
      requireNamespace("backports")) {
    backports::import(pkgname, "isFALSE")
  }
} 
