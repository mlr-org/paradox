#' @title GraghParamSet
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a graph form.
#'
#' @return [\code{\link{GraghParamSet}}].
#' @family ParamHelpers
#' @export
GraghParamSet = R6Class("GraghParamSet",
  inherit = ParamSet,
  public = list(
    nodes = list(),
    initialize = function(id = NULL, parents = NULL, children = NULL, nodes) {
      super$initialize(id, parents, children)
      self$nodes = nodes
    }
  ),
  private = list(
  )
)
