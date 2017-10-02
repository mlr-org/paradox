#' @title Base Class for ParamSet
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters.
#'
#' @return [\code{\link{ParamSet}}].
#' @family ParamHelpers
#' @export
ParamSet = R6Class("ParamSet",
  inherit = ParamNode,
  public = list(
    initialize = function(id = NULL, parents = NULL, children = NULL) {
      }
  ),
  private = list(
  )
)