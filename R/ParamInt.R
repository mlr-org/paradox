#' @title Integer Parameter
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Param].
#'
#' @description
#' A [Param] to describe integer parameters.
#'
#' @section Construction:
#' ```
#' ParamInt$new(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character())
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
#' * `nlevels` :: `integer(1)` \cr
#'   Number of categorical levels.
#'   Here, the number integers in the range `[lower, upper]`, or `Inf` if unbounded.
#' * `is_bounded` :: `logical(1)`\cr
#'   Are the bounds finite?
#'
#' @section Methods:
#' See [Param].
#'
#' @family Params
#' @export
#' @examples
#' ParamInt$new("count", lower = 0, upper = 10, default = 1)
ParamInt = R6Class("ParamInt", inherit = Param,
  public = list(
    lower = NULL,
    upper = NULL,

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
    levels = function() NULL,
    nlevels = function() (self$upper - self$lower) + 1L,
    is_bounded = function() is.finite(self$lower) && is.finite(self$upper),
    storage_type = function() "integer"
  ),

  private = list(
    .check = function(x) checkInt(x, lower = self$lower, upper = self$upper),
    .qunif = function(x) floor(x * self$nlevels * (1 - 1e-16)) + self$lower # make sure we dont map to upper+1
  )
)
