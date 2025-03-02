.onLoad <- function(libname, pkgname) {
  # Some current base functions are used which are not included in older
  # versions of R. These are provided through an 'Enhances' package,
  # "backports" but this dependency is unnecessary otherwise.
  if (
    package_version(R.Version()) < package_version("3.5") &&
      requireNamespace("backports")
  ) {
    backports::import(pkgname, "isFALSE")
  }
}
