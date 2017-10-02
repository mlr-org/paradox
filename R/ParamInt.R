ParamInt = R6Class(
  "ParamInt",
  inherit = ParamSimple,
  public = list(
    lower = NULL,
    upper = NULL,
    special.val = NULL,
    initialize = function(id, special.vals = NULL, lower = -Inf, upper = Inf) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) checkInt(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok)
      super$initialize(id = id, type = "integer", check = check)
      self$lower = lower
      self$upper = upper
    },
    sample = function(n = 1L) {
      as.integer(round(runif(n, min = self$lower-0.5, max = self$upper+0.5)))
    }
  ),
  private = list(
  )
)
