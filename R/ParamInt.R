#' @title Param: Integer
#' @format [R6Class] object. Inherits from [Param]
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
#'   `lower` is set to its integer ceiling and 'upper' to its integer floor value.
#'
#' @name ParamInt
#' @family Param
#' @export
ParamInt = R6Class( "ParamInt", inherit = Param,
  public = list(
    lower = NULL,
    upper = NULL,

    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = NULL) {
      assert_number(lower)
      assert_number(upper)
      assert_true(lower <= upper)
      self$lower = ceiling(lower)
      self$upper = floor(upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    values = function() NULL,
    nlevels = function() diff(self$range) + 1L,
    is_bounded = function() all(is.finite(self$range)),
    storage_type = function() "integer",
    range = function() c(self$lower, self$upper),
    span = function() self$upper - self$lower
  ),

  private = list(
    .check = function(x) checkInt(x, lower = self$lower, upper = self$upper),
    .map_unitint_to_values = function(x) floor(x * self$nlevels * (1 - 1e-16)) + self$lower  # make sure we dont map to upper+1
  )
)
