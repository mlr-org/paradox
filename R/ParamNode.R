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
   
    # member variables
    id = NULL, # string to uniquely identify this param
    val = NULL, # ????
    handle = NULL, # additional stuff
    type = NULL, # of what R data type can values of this parameter be stored?
    check = NULL, # a checkmate check function to validate if a value is valid for this Param
    assert = NULL, # assertion generated from the above check
    test = NULL, # test generated from the above check
    allowed = NULL, # expression that states if certain conditions have to be met
    
    # constructor
    initialize = function(id, type, check, handle = NULL, allowed = NULL) {
      handle = handle %??% ParamHandle$new()
      assertString(id)
      self$id = assertNames(id, type = "strict")
      self$type = assertString(type)
      self$check = assertFunction(check)
      self$test = makeTestFunction(check)
      self$assert = makeAssertionFunction(check)
      self$handle = assertClass(handle, "ParamHandle")
      self$allowed = substitute(allowed) %??% TRUE
    },
    
    # public methods
    sample = function(n = 1L) {
      print("I am the sample function of ParamNode, actually I cannot do anything, I am waiting my subClass to overwrite this method")
      stop("sample not implemented")
    },
    denorm = function(x) {
      stop("denorm not implemented")
    },
    transform = function(x) {
      stop("transform not implemented")
    },
    toString = function() {
      print("I am ParamNode, an abstract class which could both represent an atomic Param and Tree Param, my 'val' and 'handle' field are always Null, if you want something, please construct a subClass of me!")
    }
  ),
  active = list(
    is.finite = function() NA
  )
)

ParamNode$makeParam = function() {
  print("I am the factory method of ParamNode, I will generate Params of different type for you!")
}
