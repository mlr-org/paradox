ParamInt = R6Class(
  "ParamInt",
  
  public = list(
    lower = NULL,
    upper = NULL,
    special.val = NULL,
    initialize = function(id = NULL, lower, upper) {
      self$lower = lower
      self$upper = upper
    },
    sample = function() {
      as.integer(round(runif(1, min = self$lower-0.5, max = self$upper+0.5)))
    }
  ),
  private = list(
  )
)
