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
    params = NULL,  # a list of all ParamSimple's
    initialize = function(id = "parset", type, check, handle = NULL, params, dictionary = NULL) {
      super$initialize(id = id, type = type, check = check, handle = handle)
      assertList(params, class = "ParamNode")
      for (i in seq_along(params)) {
        param$handle$root = self
      }
      self$params = assertList(params, class = "ParamNode")
    },
    sample = function(n = 1L) {
      print("I am the sampling function of ParamSet, I will call an iterator to go through each ParamNode in me and call their sample() function, then I will return you a val, you could also use this value to set my 'val' field")
      stopf("sample function not implemented")
    },
    toString = function() {
      print("I am Paramset, I have a field called 'val' which looks like {kernel:rbf}")
    }
  ),
  active = list(
    dictionary = function(x) {
      if (missing(x)) {
        return(self$priv.dictioary)
      } else if (is.list(x)) {
        x = as.environment(x)
      }
      self$priv.dictionary = x
    }
  ),
  private = list(
    priv.dictionary = NULL
  )
)
