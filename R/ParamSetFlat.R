
#' @title ParamSetFlat
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a flat form.
#'
#' @return [\code{\link{ParamSet}}].
#' @family ParamHelpers
#' @export
ParamSetFlat = R6Class("ParamSetFlat",
  inherit = ParamSet,
  public = list(
    val = list(),  # rewrite father
    initialize = function(id = NULL) {
      super$initialize(id)
    }
  ),
  private = list(
  )
)
