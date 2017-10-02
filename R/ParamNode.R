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
    id = NULL, # string to uniquely identify this param
    val = NULL, # ????
    handle = NULL, # additional stuff
    type = NULL, # of what R data type can values of this parameter be stored?
    check = NULL, # a checkmate check function to validate if a value is valid for this Param
    assert = NULL, # assertion generated from the above check
    test = NULL, # test generated from the above check
    initialize = function(id, type, check) {
      self$id = assertString(id)
      self$type = assertString(type)
      self$check = assertFunction(check)
      self$test = makeTestFunction(check)
      self$assert = makeAssertFunction(check)
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
