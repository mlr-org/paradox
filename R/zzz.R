#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6Class
#' @importFrom stats runif rnorm
#' @importFrom methods is
"_PACKAGE"



# data.table-variables to announce:
# .init_given, .trafo

utils::globalVariables(c("J", "id", "original_id", "owner_ps_index", ".tags", "tag", ".trafo", ".", "cargo", "default", "cls", "cond", "on"))

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)

  register_namespace_callback(pkgname, "ParamHelpers", function(...) {
    warning("Packages 'paradox' and 'ParamHelpers' are conflicting and should not be loaded in the same session")
  })
} # nocov end

leanify_package()
