#' @import checkmate
#' @importFrom R6 R6Class
#' @importFrom utils head
#' @import data.table
NULL

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
} #nocov end
