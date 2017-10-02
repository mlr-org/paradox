ParamReal = R6Class(
  "ParamReal",
  inherit = ParamSimple,
  public = list(
    lower = NULL,
    upper = NULL,
    finite = NULL,
    initialize = function(id, special.vals = NULL, lower = -Inf, upper = Inf, finite = TRUE) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        checkNumber(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok, finite = finite)
      }
      super$initialize(id = id, type = "integer", check = check)
      self$lower = lower
      self$upper = upper
      self$finite = finite
    },
    sample = function(n = 1L) {
      runif(n, min = self$lower, max = self$upper)
    }
  ),
  private = list(
  )
)
