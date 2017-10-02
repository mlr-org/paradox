#' @title Simple parameter object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent simple parameters.
#'
#' @return [\code{\link{ParamSimple}}].
#' @family ParamHelpers
#' @export
ParamSimple = R6Class("ParamSimple",
  inherit = ParamNode,
  public = list(
    #initialize = function(id = NULL) {
    #  super$initialize(id)
    #}
  ),
  private = list(
  )
)
