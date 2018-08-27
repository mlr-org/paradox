#' @title Base Class for ParamSetBase
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
#' Inherited from \code{ParamNode}:
#' @inheritSection ParamNode Member Variables
#'
#' @section Methods:
#'   \emph{none}
#' 
#' Inherited from \code{ParamNode}:
#' @inheritSection ParamNode Methods
#' 
#' @section Active Bindings:
#' 
#' \describe{
#'   \item{dictionary}{[\code{list|environment}] \cr
#'     A dictionary that additional values that might be important for the transformation function, like \code{n} for the number of observations.}
#' }
#' 
#' Inherited from \code{ParamNode}
#' @inheritSection ParamNode Active Bindings
#'
#' @return [\code{\link{ParamSetBase}}].
#' @family ParamSetBase
ParamSetBase = R6Class("ParamSetBase",
  inherit = ParamNode,
  public = list(
   
    # member variables
    params = NULL,  # a list of ParamNodes
    trafo = NULL, # function to transform the value before evaluation
    restriction = NULL, # quote that states if certain conditions have to be met
    
    # constructor
    initialize = function(id = "parset", storage_type, check, handle = NULL, params, dictionary, tags, restriction, trafo) {
      
      # construct super class
      super$initialize(id = id, storage_type = storage_type, check = check, handle = handle, tags = tags)

      # set member variables
      assert_list(params, types = "ParamNode")
      for (i in seq_along(params)) {
        params[[i]]$handle$root = self
      }
      self$params = params
      self$trafo = assert_function(trafo, args = c("x", "dict", "tags"), null.ok = TRUE)
      self$restriction = assert_class(restriction, "call", null.ok = TRUE)
      self$dictionary = assert_list(dictionary, names = "strict", null.ok = TRUE)
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
        return(private$priv_dictionary)
      } else if (!is.null(x)) {
        x = as.environment(x)
        private$priv_dictionary = x   
      }
    }
  ),
  private = list(
    priv_dictionary = NULL
  )
)
