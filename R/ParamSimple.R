#' @title ParamSimple Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent a simple parameter.
#' 
#' @section Member Variables:
#' 
#' \describe{
#'   \item{default}{[\code{any}] \cr
#'     default value.}
#'   \item{special_vals}{[\code{any}] \cr
#'     Special values this parameter is allowed to take that are within the defined space.}
#' }
#' 
#' Inherited from \code{ParamNode}:
#' @inheritSection ParamNode Member Variables
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{sampleVector(n)}{[\code{function}] \cr
#'     samples \code{n} Parameter Values.}
#'   \item{denormVector(x)}{[\code{function}] \cr
#'     Takes a vector with values between \code{[0,1]} and maps them to values of the Parameter.}
#' }
#' 
#' Inherited from \code{ParamNode}:
#' @inheritSection ParamNode Methods
#' 
#' @section Active Bindings:
#'   \emph{none}
#' 
#' Inherited from \code{ParamNode}:
#' @inheritSection ParamNode Active Bindings
#' 
#' @family ParamSimple
ParamSimple = R6Class(
  "ParamSimple",
  inherit = ParamNode,
  public = list(
   
    # member variables
    default = NULL,
    special_vals = NULL, # special values as list, can not be changed after initialization

    # constructor
    initialize = function(id, storage.type, check, special_vals, default, tags) {

      if (!is.null(special_vals) && is.na(special_vals)) special_vals = list(special_vals)
      assertList(special_vals, null.ok = TRUE)

      # construct super class
      super$initialize(id = id, storage.type = storage.type, check = check, tags = tags)
      
      # set member variables
      self$default = self$assert(default, null.ok = TRUE)
      self$special_vals = special_vals
    },

    # public methods
    # Overwriting ParamNode Methods
    sample = function(n = 1L) asDtCols(self$sampleVector(n = n), self$id),
    denorm = function(x) asDtCols(self$denormVector(x[[self$id]]), self$id),
    
    # ParamSimpleMethods
    sampleVector = function(n = 1L) {
      # samples vector values without respecting what is 'restriction'
      stop("sampleVector not implemented")
    },
    denormVector = function(x) {
      stop("denorm function not implemented!")
    }
  ),
)
