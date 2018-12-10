#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom utils head tail
NULL

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
} #nocov end
