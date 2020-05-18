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
    #' @template field_lower
    lower = NULL,

    #' @template field_upper
    upper = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character()) {
      if (isTRUE(is.infinite(lower))) {
        self$lower = lower
      } else {
        self$lower = assert_int(lower)
      }
      if (isTRUE(is.infinite(upper))) {
        self$upper = upper
      } else {
        self$upper = assert_int(upper)
      }
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    #' @template field_levels
    levels = function() NULL,
    #' @template field_nlevels
    nlevels = function() (self$upper - self$lower) + 1L,
    #' @template field_is_bounded
    is_bounded = function() is.finite(self$lower) && is.finite(self$upper),
    #' @template field_storage_type
    storage_type = function() "integer"
  ),

  private = list(
    .check = function(x) checkInt(x, lower = self$lower, upper = self$upper),
    .qunif = function(x) floor(x * self$nlevels * (1 - 1e-16)) + self$lower # make sure we dont map to upper+1
  )
)
