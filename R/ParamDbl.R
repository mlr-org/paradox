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
    levels = function() NULL,
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
