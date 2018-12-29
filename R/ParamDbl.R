#' @title Param: Double
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
#'
#' @name ParamDbl
#' @family Param
#' @export
ParamDbl = R6Class("ParamDbl", inherit = Param,
  public = list(
    lower = NULL,
    upper = NULL,

    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(0L)) {
      assert_number(lower)
      assert_number(upper)
      assert_true(lower <= upper)
      self$lower = lower
      self$upper = upper
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

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
    .qunif = function(x) x*self$span + self$lower
  )
)
