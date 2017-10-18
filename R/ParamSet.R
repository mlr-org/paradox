#' @title Base Class for ParamSet
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters.
#' 
#' @section Member Variables:
#' 
#' \describe{
#'   \item{params}{[\code{list}] \cr
#'   List of the Params}
#'   \item{trafo}{[\code{function(x, dict, tags)}] \cr
#'     A function that returns a list of transformed x values. 
#'     Has to work vectorized and also return untransformed x values.
#'     The function takes a list \code{x} of all parameter values, additionally the dictionary linked to the \code{ParamSet}.
#'     \code{tags} is a named list that contains the tags for each Param in \code{x}.}
#'   \item{restriction}{[\code{quote}] \cr
#'     A quoted expression (\code{quote()}) that is evaluated on all parameter values to check if they are feasible.
#'     It has to be evaluated to \code{TRUE} so that the parameter value is valid.
#'     The expression has to work on vectors of values.}
#' }
#'
#' @inheritSection ParamNode Methods
#' 
#' @section Active Bindings:
#' 
#' \describe{
#'   \item{dictionary}{[\code{list|environment}] \cr
#'     A dictionary that additional values that might be important for the transformation function, like \code{n} for the number of observations.}
#' }
#'
#' @return [\code{\link{ParamSet}}].
#' @family ParamHelpers
ParamSet = R6Class("ParamSet",
  inherit = ParamNode,
  public = list(
   
    # member variables
    params = NULL,  # a list of all ParamSimple's
    trafo = NULL, # function to transform the value before evaluation
    restriction = NULL, # quote that states if certain conditions have to be met
    
    # constructor
    initialize = function(id = "parset", storage.type, check, handle = NULL, params, dictionary, tags, restriction, trafo) {
      
      # construct super class
      super$initialize(id = id, storage.type = storage.type, check = check, handle = handle, tags = tags)

      # set member variables
      assertList(params, types = "ParamNode")
      for (i in seq_along(params)) {
        params[[i]]$handle$root = self
      }
      self$params = params
      self$trafo = assertFunction(trafo, args = c("x", "dict", "tags"), null.ok = TRUE)
      self$restriction = assertClass(restriction, "call", null.ok = TRUE)
      self$dictionary = assertList(dictionary, names = "strict", null.ok = TRUE)
    },

    # public methods
    sample = function(n = 1L) {
      stop("sample function not implemented")
    },
    transform = function(x) {
      stop("transform not implemented")
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
    }
  ),
  private = list(
    priv.dictionary = NULL
  )
)
