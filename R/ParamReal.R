ParamReal = R6Class(
  "ParamReal",
  inherit = ParamSimple,
  public = list(
   
    # member variables
    finite = NULL,
    lower.varpar = NULL,
    upper.varpar = NULL,
    
    # constructor
    initialize = function(id, special.vals = NULL, default = NULL, lower = -Inf, upper = Inf, finite = TRUE, trafo = NULL, allowed = NULL, tags = character()) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        checkNumber(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok, finite = finite)
      }
      
      # construct super class
      super$initialize(id = id, type = "numeric", check = check, special.vals = special.vals, default = default, trafo = trafo, allowed = allowed, tags = tags)

      # write member variables
      self$lower.varpar = assertPossibleCall(lower, self$assert, null.ok = TRUE)
      self$upper.varpar = assertPossibleCall(upper, self$assert, null.ok = TRUE)
      self$finite = assertFlag(finite)
    },

    # public methods
    sampleVectorUnrestricted = function(n = 1L) {
      runif(n, min = self$lower, max = self$upper)
    },
    denormVector = function(x) {
      BBmisc::normalize(x = x, method = "range", range = self$range)
    }
  ),
  active = list(
    lower = function() evalIfCall(self$lower.varpar, self),
    upper = function() evalIfCall(self$upper.varpar, self),
    range = function() c(self$lower, self$upper),
    is.finite = function() all(is.finite(self$range))
  )
)
