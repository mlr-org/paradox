ParamInt = R6Class(
  "ParamInt",
  inherit = ParamSimple,
  public = list(
   
    # member variables
    lower.expr = NULL,
    upper.expr = NULL,
    
    # constructor
    initialize = function(id, special.vals = NULL, default = NULL, lower = -Inf, upper = Inf, trafo = NULL, allowed = NULL, tags = character()) {
      
      check = function(x, na.ok = FALSE, null.ok = FALSE) checkInt(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok)
      
      # construct super class
      super$initialize(id = id, type = "integer", check = check, special.vals = special.vals, default = default, trafo = trafo, allowed = allowed, tags = tags)
      
      # write member variables
      self$lower.expr = assertPossibleExpr(lower, self$assert, null.ok = TRUE)
      self$upper.expr = assertPossibleExpr(upper, self$assert, null.ok = TRUE)
    },

    # public methods
    sampleVector = function(n = 1L) {
      as.integer(round(runif(n, min = self$lower-0.5, max = self$upper+0.5)))
    },
    denormVector = function(x) {
      as.integer(round(BBmisc::normalize(x = x, method = "range", range = self$range + c(-0.5, 0.5))))
    },
    transformVector = function(x) {
      as.integer(round(self$trafo(x))) 
    }
  ),
  active = list(
    lower = function() evalIfExpr(self$lower.expr, self),
    upper = function() evalIfExpr(self$upper.expr, self),
    range = function() c(self$lower, self$upper),
    is.finite = function() all(is.finite(self$range))
  )
)
