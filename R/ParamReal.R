ParamReal = R6Class(
  "ParamReal",
  inherit = ParamSimple,
  public = list(
   
    # member variables
    finite = NULL,
    lower.expr = NULL,
    upper.expr = NULL,
    
    # constructor
    initialize = function(id, special.vals = NULL, default = NULL, lower = -Inf, upper = Inf, finite = TRUE) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        checkNumber(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok, finite = finite)
      }
      super$initialize(id = id, type = "integer", check = check, special.vals = special.vals, default = default)
      self$lower.expr = assertPossibleExpr(lower, self$assert, null.ok = TRUE)
      self$upper.expr = assertPossibleExpr(upper, self$assert, null.ok = TRUE)
      self$finite = assertFlag(finite)
    },

    # public methods
    sample = function(n = 1L) {
      runif(n, min = self$lower, max = self$upper)
    },
    denorm = function(x) {
      BBmisc::normalize(x = x, method = "range", range = self$range)
    }
  ),
  active = list(
    lower = function() evalIfExpr(self$lower.expr, self),
    upper = function() evalIfExpr(self$upper.expr, self),
    range = function() c(self$lower, self$upper),
    is.finite = function() all(is.finite(self$range))
  )
)
