#' @title Flag Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent Flag parameters.
#' 
#' @section Member Variables:
#'   \emph{none}
#' 
#' Inherited from \code{ParamSimple}:
#' @inheritSection ParamSimple Member Variables
#' 
#' @section Methods:
#'   \emph{none}
#' 
#' Inherited from \code{ParamSimple}
#' @inheritSection ParamSimple Methods
#' 
#' @section Active Bindings:
#'   \emph{none}
#' 
#' Inherited from \code{ParamSimple}
#' @inheritSection ParamSimple Active Bindings
#'
#' @return [\code{\link{ParamFlag}}].
#' @family ParamSimple
#' @export
ParamFlag = R6Class(
  "ParamFlag",
  inherit = ParamSimple,
  public = list(
    
    # constructor
    initialize = function(id, special_vals = NULL, default = NULL, tags = NULL) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (test_special_vals(self, x)) return(TRUE)
        checkFlag(x, na.ok, null.ok)
      }
      
      # construct super class
      super$initialize(id = id, storage_type = "logical", check = check, special_vals = special_vals, default = default, tags = tags)
    },

    # public methods
    sampleVector = function(n = 1L) {
      sample(c(TRUE, FALSE), size = n, replace = TRUE)
    },
    denormVector = function(x) {
      x < 0.5 #FIXME: Do we have to take care of x==0.5?
    }
  ),
  active = list(
    has.finite.bounds = function() TRUE,
    values = function() c(TRUE, FALSE),
    nlevels = function() 2L
  )
)
