#' @title Simple parameter object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent simple parameters.
#'
#' @return [\code{\link{SimpleParam}}].
#' @family ParamHelpers
#' @export
SimpleParam = R6Class("SimpleParam",
  public = list(
    initialize = function(id = NULL, parents = NULL) {
    }
  ),
  private = list(
  )
)