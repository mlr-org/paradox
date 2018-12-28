#' @title Param: Integer
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
#'   `lower` is set to its integer ceiling and 'upper' to its integer floor value.
#'
#' @name ParamInt
#' @family Param
#' @export
ParamInt = R6Class( "ParamInt", inherit = Param,
  public = list(
    lower = NULL,
    upper = NULL,

    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NULL, tags = NULL) {
      assert_number(lower)
      assert_number(upper)
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
      self$lower = ceiling(lower)
      self$upper = floor(upper)
    }
  ),

  active = list(
    values = function() NULL,
    nlevels = function() diff(self$range),
    is_bounded = function() all(is.finite(self$range)),
    storage_type = function() "integer",
    range = function() c(self$lower, self$upper),
    span = function() self$upper - self$lower
  ),

  private = list(
    .check = function(x) checkInt(x, lower = self$lower, upper = self$upper),

    .map_unitint_to_values = function(x) {
      #FIXME: Can we make this shorter?
      assert_true(self$is_bounded)
      r = self$range + c(-0.5, 0.5)
      res = as.integer(round(r[1] + x * diff(r)))
      res = ifelse(res > self$upper, self$upper, res) #if we rounded up, we have to go down
      res = ifelse(res < self$lower, self$lower, res) #if we rounded down, we have to go up
      res
    }
  )
)
