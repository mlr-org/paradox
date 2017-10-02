#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent parameters.
#'
#' @return [\code{\link{ParaNode}}].
#' @family ParamHelpers
#' @export
ParamNode = R6Class("ParamNode",
  inherit = ParamBase,
  public = list(
  ),
  private = list(
  )
)

ParamNode$makeParam = function() {
  print("I am the factory method of ParamNode, I will generate Params of different type for you!")
}
