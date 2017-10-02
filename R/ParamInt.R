ParamInt = R6Class(
  "ParamInt",
  inherit = ParamSimple,
  public = list(
    special.val = NULL,
    initialize = function(id, special.vals = NULL, default = NULL, lower = -Inf, upper = Inf) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) checkInt(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok)
      super$initialize(id = id, type = "integer", check = check, special.vals = special.vals, default = default)
      self$lower = lower
      self$upper = upper
    },
    sample = function(n = 1L) {
      as.integer(round(runif(n, min = self$lower-0.5, max = self$upper+0.5)))
    },
    denorm = function(x) {
      as.integer(round(BBmisc::normalize(x = x, method = "range", range = self$range + c(-0.5, 0.5))))
    }
  ),
  active = list(
    lower = function() evalIfExpr(self$lower.expr, self),
    upper = function() evalIfExpr(self$upper.expr, self),
    range = function() c(self$lower, self$upper),
    is.finite = function() all(is.finite(self$range))
  ),
  private = list(
    lower.expr = NULL,
    upper.expr = NULL
  )
)
