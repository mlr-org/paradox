#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom stats runif
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
} # nocov end
