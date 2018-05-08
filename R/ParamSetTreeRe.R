#' @title ParamSetTreeRe
#' @format \code{\link{R6Class}} object
#'
#' @description
#' ???
#'
#' @return [\code{\link{ParamSetTreeRe}}].
#' @family ParamSet
#' @export
ParamSetTreeRe = R6Class("ParamSetTreeRe",
  inherit = ParamSetTree,
  public = list(
    # member variables
    root.set = NULL,
    initialize = function(ns.id, ..., nr = 1L) {
      if (nr > 1) {
        self$root.set = recursiveParaFac(nr, ...)
      }
    },

    sample = function(n = 1L) {
      self$root.set$sample(n)
    },

    getRecursiveList = function() {
      self$root.set$getRecursiveList()
    }
  )
)