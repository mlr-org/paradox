#' @title ParamSetTree
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a tree form.
#'
#' @return [\code{\link{ParamSetTree}}].
#' @family ParamHelpers
#' @export
ParamSetTree = R6Class("ParamSetTree",
  inherit = ParamSet,
  public = list(
    id = NULL,
    val = NULL,
    handle = NULL,
    flatval = NULL,
    initialize = function(id = NULL, val = NULL) {
      self$id = id
      self$val = val
      self$flatval = list()
      self$handle = NULL  #ParamHandle$new(self)
    }
  ),
  private = list(
  )
)
