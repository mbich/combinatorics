#' @import rJava
NULL

.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
}
