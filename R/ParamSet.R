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
   
    # member variables
    params = NULL,  # a list of all ParamSimple's
    trafo = NULL, # function to transform the value before evaluation
    restriction = NULL, # quote that states if certain conditions have to be met
    
    # constructor
    initialize = function(id = "parset", type, check, handle = NULL, params, dictionary, tags, restriction, trafo) {
      
      # construct super class
      super$initialize(id = id, type = type, check = check, handle = handle, tags = tags)

      # set member variables
      assertList(params, types = "ParamNode")
      for (i in seq_along(params)) {
        params[[i]]$handle$root = self
      }
      self$params = params
      self$trafo = assertFunction(trafo, args = c("x", "dict"), null.ok = TRUE)
      self$restriction = assertClass(restriction, "call", null.ok = TRUE)
      self$dictionary = assertList(dictionary, names = "strict", null.ok = TRUE)
    },

    # public methods
    sample = function(n = 1L) {
      print("I am the sampling function of ParamSet, I will call an iterator to go through each ParamNode in me and call their sample() function, then I will return you a val, you could also use this value to set my 'val' field")
      stop("sample function not implemented")
    },
    transform = function(x) {
      stop("transform not implemented")
    },
    toString = function() {
      print("I am Paramset, I have a field called 'val' which looks like {kernel:rbf}")
    }
  ),
  active = list(
    dictionary = function(x) {
      if (missing(x)) {
        return(private$priv.dictionary)
      } else if (!is.null(x)) {
        x = as.environment(x)
        private$priv.dictionary = x   
      }
    },
    ids = function() stop("ids not implemented"),
    types = function() stop("types not implemented")
  ),
  private = list(
    priv.dictionary = NULL
  )
)
