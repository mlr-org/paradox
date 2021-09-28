#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6Class
#' @importFrom stats runif rnorm
#' @importFrom methods is
"_PACKAGE"



# data.table-variables to announce:
# .init_given

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
} # nocov end

leanify_package()
