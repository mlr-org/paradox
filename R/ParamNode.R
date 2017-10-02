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
    id = NULL,
    val = NULL,
    handle = NULL,
    initialize = function(id = NULL) {
      self$id = id
    },
    sample = function() {
      print("I am the sample function of ParamNode, actually I cannot do anything, I am waiting my subClass to overwrite this method")
    },
    toString = function() {
      print("I am ParamNode, an abstract class which could both represent an atomic Param and Tree Param, my 'val' and 'handle' field are always Null, if you want something, please construct a subClass of me!")
    }
  ),
  private = list(
  )
)

ParamNode$makeParam = function() {
  print("I am the factory method of ParamNode, I will generate Params of different type for you!")
}
