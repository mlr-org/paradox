#' @title Integer Parameter
#'
#' @description
#' A [Param] to describe integer parameters.
#'
#' @template param_id
#' @template param_lower
#' @template param_upper
#' @template param_special_vals
#' @template param_default
#' @template param_tags
#'
#' @section Methods:
#' See [Param].
#'
#' @family Params
#' @include Param.R
#' @export
#' @examples
#' ParamInt$new("count", lower = 0, upper = 10, default = 1)
ParamInt = R6Class("ParamInt", inherit = Param,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character()) {
      if (isTRUE(is.infinite(lower))) {
        private$.lower = lower
      } else {
        private$.lower = assert_int(lower)
      }
      if (isTRUE(is.infinite(upper))) {
        private$.upper = upper
      } else {
        private$.upper = assert_int(upper)
      }
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    },

    #' @description
    #' Converts a value to an integer.
    #' @param x (`numeric(1)`)\cr
    #'   Value to convert.
    convert = function(x) {
      as.integer(x)
    }
  ),

  active = list(
    #' @template field_lower
    lower = function() private$.lower,
    #' @template field_upper
    upper = function() private$.upper,
    #' @template field_levels
    levels = function() NULL,
    #' @template field_nlevels
    nlevels = function() (private$.upper - private$.lower) + 1L,
    #' @template field_is_bounded
    is_bounded = function() is.finite(private$.lower) && is.finite(private$.upper),
    #' @template field_storage_type
    storage_type = function() "integer"
  ),

  private = list(
    .check = function(x) checkInt(x, lower = private$.lower, upper = private$.upper, tol = 1e-300),
    .qunif = function(x) as.integer(floor(x * self$nlevels * (1 - 1e-16)) + private$.lower), # make sure we dont map to upper+1
    .lower = NULL,
    .upper = NULL
  )
)
