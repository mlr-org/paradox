#' @title Untyped Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent untyped parameters.
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
#' @return [\code{\link{ParamUntyped}}].
#' @family ParamSimple
#' @export
ParamUntyped = R6Class(
  "ParamUntyped",
  inherit = ParamSimple,
  public = list(
    # member variables

    # constructor
    initialize = function(id, default = NULL, tags = NULL) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (!na.ok && length(x) == 1 && is.na(x)) "Value is NA"
        if (!null.ok && is.null(x)) "Value is NULL"
        return(TRUE)
      }

      # construct super class
      super$initialize(id = id, storage_type = "list", check = check, default = default, tags = tags, special_vals = NULL)
    },

    # public methods
    sampleVector = function(n = 1L) {
      stop("Untyped Param can not be sampled.")
    },
    denormVector = function(x) {
      stop("Untyped Param can not be denormed.")
    }
  ),

  active = list(
    has_finite_bounds = function() FALSE
  )
)
