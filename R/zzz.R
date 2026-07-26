#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6Class
#' @importFrom stats runif rnorm
#' @useDynLib paradox, .registration = TRUE, .fixes = "C_"
#'
#' @section Callback source references:
#' Paradox removes `srcref`, `srcfile`, and `wholeSrcref` attributes
#' recursively from stored `custom_check`, per-parameter `trafo`,
#' `extra_trafo`, `constraint`, aggregation, and internal-tuning callbacks.
#' This keeps serialized search spaces independent of the source file that
#' defined a callback. The callback's enclosing environment and ordinary
#' return values are preserved; source display, breakpoint, and introspection
#' behavior can change for a stripped copy. Source-reference normalization
#' never modifies function-valued parameter values or opaque value payloads.
#'
#' For source-level debugging, set
#' `options(paradox.strip_srcrefs = FALSE)` before constructing or assigning
#' the callbacks, then reconstruct the object. The option is read only at each
#' admission boundary and does not retroactively change existing objects.
"_PACKAGE"



# data.table-variables to announce:
# .init_given, .trafo

utils::globalVariables(c("J", "id", "original_id", "owner_ps_index", ".tags", "tag", ".trafo", "trafo", ".", "cargo", "default", "cls", "cond", "on", "required"))

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
  if (getRversion() < "4.0.0") {
    # backports 1.1.7 defines and exports deparse1() on old R, but its generic
    # import ledger predates that entry. Import this declared floor explicitly.
    backports::import(pkgname, "deparse1", force = TRUE)
  }

  register_namespace_callback(pkgname, "ParamHelpers", function(...) {
    warning("Packages 'paradox' and 'ParamHelpers' are conflicting and should not be loaded in the same session")
  })
} # nocov end

.paradox_leanify_package()
