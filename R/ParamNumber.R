ParamNumber = R6Class( "ParamNumber", inherit = Parameter,
  public = list(
    # member variables
    lower = NULL,
    upper = NULL,

    # constructor
    initialize = function(id, storage_type, special_vals = NULL, default = NULL, lower = -Inf, upper = Inf, tags = NULL, check) {
      assert_true(lower <= upper)
      self$lower = lower
      self$upper = upper
      super$initialize(id = id, storage_type = storage_type, check = check, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    range = function() c(self$lower, self$upper),
    has_finite_bounds = function() all(is.finite(self$range))
  )
)

