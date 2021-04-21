#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6Class
#' @importFrom stats runif rnorm
#' @importFrom methods is
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
} # nocov end

# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c("id", "on"))

leanify_package()
