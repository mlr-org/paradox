#' @title Param: Double
#' @format [R6Class] object.
#'
#' @section Public members / active bindings:
#' * `range`            :: `numeric(2)` \cr
#'   Lower and upper bound as 2-dim-vector.
#' * `span`            :: `numeric(1)` \cr
#'   Difference of `upper - lower`.
#'
#' @section Public methods:
#' * `new(id, lower, upper, special_vals, default, tags)` \cr
#'   `character(1)`, `numeric(1)`, `numeric(1)`, `list`, `any`, `character` -> self
#'
#' @name ParamDbl
#' @family Param
#' @export
ParamDbl = R6Class("ParamDbl", inherit = Param,
  public = list(
    lower = NULL,
    upper = NULL,

    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NULL, tags = NULL) {
      assert_number(lower, na.ok = TRUE)
      assert_number(upper, na.ok = TRUE)
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
      self$lower = lower
      self$upper = upper
    }
  ),

  # FIXME: readd center again
  active = list(
    values = function() NULL,
    nlevels = function() Inf,
    is_bounded = function() all(is.finite(self$range)),
    storage_type = function() "numeric",
    range = function() c(self$lower, self$upper),
    span = function() self$upper - self$lower
  ),

  private = list(
    .check = function(x) checkNumber(x, lower = self$lower, upper = self$upper),

    # maps [0,1]*span + lower
    .map_unitint_to_values = function(x) {
      assert_true(self$is_bounded)
      self$lower + x * self$span
    }
  )
)
