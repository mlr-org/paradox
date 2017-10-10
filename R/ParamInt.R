#' @title Integer Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent Integer parameters.
#'
#' @return [\code{\link{ParamInteger}}].
#' @family ParamSimple
#' @export
ParamInt = R6Class(
  "ParamInt",
  inherit = ParamSimple,
  public = list(

    # member variables
    lower = NULL,
    upper = NULL,

    # constructor
    initialize = function(id, special.vals = NULL, default = NULL, lower = -Inf, upper = Inf, tags = NULL) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) checkInt(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok)

      # construct super class
      super$initialize(id = id, storage.type = "integer", check = check, special.vals = special.vals, default = default, tags = tags)
      
      # arg check lower and upper, we need handle Inf special case, that is not an int
      if (identical(lower, Inf) || identical(lower, -Inf))
        self$lower = lower
      else
        self$lower = asInt(lower)
      if (identical(upper, Inf) || identical(upper, -Inf))
        self$upper = upper
      else
        self$upper = asInt(upper)
      assert_true(lower <= upper)
    },

    # public methods
    sampleVector = function(n = 1L) {
      as.integer(round(runif(n, min = self$lower-0.5, max = self$upper+0.5)))
    },
    denormVector = function(x) {
      as.integer(round(normalize(x = x, method = "range", range = self$range + c(-0.5, 0.5))))
    }
  ),
  active = list(
    range = function() c(self$lower, self$upper),
    is.finite = function() all(is.finite(self$range))
  )
)
