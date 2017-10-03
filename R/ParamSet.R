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
    initialize = function(id = "parset", type, check, handle = NULL, params, dictionary) {
      super$initialize(id = id, type = type, check = check, handle = handle)
      assertList(params, types = "ParamNode")
      for (i in seq_along(params)) {
        params[[i]]$handle$root = self
      }
      self$params = params
    },

    # public methods
    sample = function(n = 1L) {
      print("I am the sampling function of ParamSet, I will call an iterator to go through each ParamNode in me and call their sample() function, then I will return you a val, you could also use this value to set my 'val' field")
      stop("sample function not implemented")
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
    },
    ids = function() stop("ids not implemented")
  ),
  private = list(
    priv.dictionary = NULL
  )
)
