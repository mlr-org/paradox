#' @title Numerical Parameter
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Param].
#'
#' @description
#' A [Param] to describe real-valued parameters.
#'
#' @section Construction:
#' ```
#' ParamDbl$new(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character())
#' ```
#' Arguments of [Param], and additionally:
#'
#' * `lower` :: `numeric(1)`\cr
#'   Lower bound, can be `-Inf`.
#' * `upper` :: `numeric(1)`\cr
#'   Upper bound can be `+Inf`.
#'
#' @section Fields:
#' Fields of [Param], and additionally:
#'
#' * `lower` :: `numeric(1)`\cr
#'   Lower bound.
#' * `upper` :: `numeric(1)`\cr
#'   Upper bound.
#' * `levels` :: `NULL`\cr
#'   Allowed levels.
#'   Always `NULL` for this parameter.
#' * `nlevels` :: `Inf` \cr
#'   Number of categorical levels.
#'   Always `Inf` for this parameter.
#' * `is_bounded` :: `logical(1)`\cr
#'   Are the bounds finite?
#'
#' @section Methods:
#' See [Param].
#'
#' @family Params
#' @export
ParamDbl = R6Class("ParamDbl", inherit = Param,
  public = list(
    lower = NULL,
    upper = NULL,

    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character()) {
      self$lower = assert_number(lower)
      self$upper = assert_number(upper)
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    levels = function() NULL,
    nlevels = function() Inf,
    is_bounded = function() is.finite(self$lower) && is.finite(self$upper),
    storage_type = function() "numeric"
  ),

  private = list(
    .check = function(x) checkNumber(x, lower = self$lower, upper = self$upper),
    .qunif = function(x) x * (self$upper - self$lower) + self$lower
  )
)
