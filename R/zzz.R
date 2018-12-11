#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom stats runif
#' @importFrom utils head tail getFromNamespace
NULL

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
} #nocov end
