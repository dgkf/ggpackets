# ggpackets v0.2.1

* handle `+.gg` masking a bit more responsibly, avoiding building of 
  `ggplot2:::+.gg` into the `ggpackets` namespace and instead fetching it upon
  use.

* remove `crayon` package dependency, only used for console output of missing
  aesthetics

# ggpackets v0.2.0

* initial release on CRAN
