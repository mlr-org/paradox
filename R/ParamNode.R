#' @title Parameter Node Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent parameters.
#'
#' @return [\code{\link{ParaNode}}].
#' @family ParamHelpers
ParamNode = R6Class("ParamNode",
  inherit = ParamBase,
  public = list(
   
    # member variables
    id = NULL, # string to uniquely identify this param
    val = NULL, # ????
    handle = NULL, # additional stuff
    storage.type = NULL, # of what R data storage.type can values of this parameter be stored?
    check = NULL, # a checkmate check function to validate if a value is valid for this Param
    assert = NULL, # assertion generated from the above check
    test = NULL, # test generated from the above check
    tags = NULL, # additional properties like "on.train", "on.test" or "tunable" for mlr
    
    # constructor
    initialize = function(id, storage.type, check, handle = NULL, tags) {
      handle = handle %??% ParamHandle$new()
      assertString(id)
      self$id = assertNames(id, type = "strict")
      self$storage.type = assertString(storage.type)
      self$check = assertFunction(check)
      self$test = makeTestFunction(check)
      self$assert = makeAssertionFunction(check)
      self$handle = assertClass(handle, "ParamHandle")
      self$tags = assertCharacter(tags, null.ok = TRUE)
    },
    
    # public methods
    sample = function(n = 1L) {
      stop("sample not implemented")
    },
    denorm = function(x) {
      stop("denorm not implemented")
    }
  ),
  active = list(
    is.finite = function() NA
  )
)