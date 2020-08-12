#' @title Numerical Parameter
#'
#' @description
#' A [Param] to describe real-valued parameters.
#'
#' @template param_id
#' @template param_lower
#' @template param_upper
#' @template param_special_vals
#' @template param_default
#' @template param_tags
#'
#' @family Params
#' @include Param.R
#' @export
#' @examples
#' ParamDbl$new("ratio", lower = 0, upper = 1, default = 0.5)
ParamDbl = R6Class("ParamDbl", inherit = Param,
  public = list(
    #' @template field_lower
    lower = NULL,

    #' @template field_upper
    upper = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character()) {
      self$lower = assert_number(lower)
      self$upper = assert_number(upper)
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    },

    to_tune = function(lower = self$lower, upper = self$upper) {
      private$.to_tune_param = ParamDbl$new(id = self$id, lower = lower, upper = upper)
    }
  ),

  active = list(
    #' @template field_levels
    levels = function() NULL,
    #' @template field_nlevels
    nlevels = function() Inf,
    #' @template field_is_bounded
    is_bounded = function() is.finite(self$lower) && is.finite(self$upper),
    #' @template field_storage_type
    storage_type = function() "numeric"
  ),

  private = list(
    .check = function(x) checkNumber(x, lower = self$lower, upper = self$upper),
    .qunif = function(x) x * (self$upper - self$lower) + self$lower
  )
)
