#' @title Base Class for ParamSet
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters.
#'
#' @return [\code{\link{ParaSet}}].
#' @family ParamHelpers
#' @export
ParaSet = R6Class("ParaSet",
  public = list(
    initialize = function(id = NULL, parents = NULL) {
      }
  ),
  private = list(
  )
)
