#' @title ParamSetTreeRe
#' @format \code{\link{R6Class}} object
#'
#' @description
#' Recursive ParamsetTree, used in situation where one want to define a deep learning network for example.
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
      else {
        stop("only use ParamSetTreeRe for more than 2 layer!")
      }
    },

    sample = function(n = 1L) {
      self$root.set$sample(n)
    },

    sampleList = function(prefer = "flat") {
      if (prefer == "flat") self$root.set$sampleList()
      else self$root.set$getRecursiveList()
    }
  ),
  private = list(),
)
