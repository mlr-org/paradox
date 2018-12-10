#' @import checkmate
#' @import mlr3misc
#' @importFrom data.table data.table as.data.table setnames
#' @importFrom R6 R6Class
#' @importFrom utils head tail
NULL

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
} #nocov end
