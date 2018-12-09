#' @title Untyped Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent untyped parameters.
#'
#' @section Member Variables:
#'   \emph{none}
#'
#' Inherited from \code{Parameter}:
#' @inheritSection Parameter Member Variables
#'
#' @section Methods:
#'   \emph{none}
#'
#' Inherited from \code{Parameter}
#' @inheritSection Parameter Methods
#'
#' @section Active Bindings:
#'   \emph{none}
#'
#' Inherited from \code{Parameter}
#' @inheritSection Parameter Active Bindings
#'
#' @return [\code{\link{ParamUntyped}}].
#' @family Parameter
#' @export
ParamUntyped = R6Class(
  "ParamUntyped",
  inherit = Parameter,
  public = list(

    initialize = function(id, default = NULL, tags = NULL) {
      super$initialize(
        id = id,
        storage_type = "list",
        lower = NA_real_,
        upper = NA_real_,
        values = NULL,
        check = function(x) TRUE,  # values are always feasible
        special_vals = NULL,
        default = default,
        tags = tags
      )
    },

    # public methods
    denorm_vector = function(x) {
      stop("Untyped Param can not be denormed.")
    },

    value_to_string = function(x, ...) {
      class(x)
    }
  ),

  private = list(
    get_range_string = function() "",
    get_type_string = function() "u"
  )
)
