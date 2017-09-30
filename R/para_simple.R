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
  public = list(
    initialize = function(id = NULL, parents = NULL) {
    },
    sample = function() {
      return(NULL)
    }
  ),
  private = list(
  )
)

ParamInt = R6Class("ParamInt",
  public = list(
    lower = NULL,
    upper = NULL,
    special.val = NULL,
    initialize = function(id = NULL, parents = NULL, lower, upper) {
      self$lower = lower
      self$upper = upper
    },
    sample = function() {
      as.integer(round(runif(1, min = self$lower-0.5, max = self$upper+0.5)))
    }
  ),
  private = list(
  )
)