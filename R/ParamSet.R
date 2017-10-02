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
    val = NULL,  # the flat version in the form of list(key = val)
    handle = NULL,
    initialize = function(id = "parset") {
    },
    sample = function() {
      print("I am the sampling function of ParamSet, I will call an iterator to go through each ParamNode in me and call their sample() function, then I will return you a val, you could also use this value to set my 'val' field")
    },
    toString = function() {
      print("I am Paramset, I have a field called 'val' which looks like {kernel:rbf}")
    }
  ),
  private = list(
  )
)
