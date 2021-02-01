#' @title Numerical Parameter
#'
#' @description
#' A [Param] to describe real-valued parameters.
#'
#' @note
#' The upper and lower bounds in `$check()` are expanded by
#' `sqrt(.Machine$double.eps)` to prevent errors due to the precision of double
#' values.
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

    #' @field tolerance (`numeric(1)`)\cr
    #' tolerance of values to accept beyond `$lower` and `$upper`.
    #' Used both for relative and absolute tolerance.
    tolerance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param tolerance (`numeric(1)`)\cr
    #'   Initializes the `$tolerance` field that determines the
    #    tolerance of the `lower` and `upper` values.
    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(), tolerance = sqrt(.Machine$double.eps)) {
      self$lower = assert_number(lower)
      self$upper = assert_number(upper)
      self$tolerance = assert_number(tolerance, lower = 0)
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    },

    #' @description
    #' Restrict the value to within the allowed range. This works
    #' in conjunction with `$tolerance`, which accepts values
    #' slightly out of this range.
    #'
    #' @param x (`numeric(1)`)\cr
    #'   Value to convert.
    convert = function(x) {
      min(max(x, self$lower), self$upper)
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
    .check = function(x) {
      # Accept numbers between lower and upper bound, with tolerance self$tolerance
      # Tolerance is both absolute & relative tolerance (if either tolerance is
      # undercut the value is accepted:
      # Values that go beyond the bound by less than `self$tolerance` are also
      #   accepted (absolute tolerance)
      # Values that go beyond the bound by less than `abs(<bound>) * self$tolerance`
      #   are also accepted (relative tolerance)
      checkNumber(x,
        lower = self$lower - self$tolerance * max(1, abs(self$lower)),
        upper = self$upper + self$tolerance * max(1, abs(self$upper))
      )
    },
    .qunif = function(x) x * self$upper - (x-1) * self$lower
  )
)
