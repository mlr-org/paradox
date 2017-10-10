#' @title Real Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent numeric, real valued parameters.
#'
#' @return [\code{\link{ParamReal}}].
#' @family ParamSimple
#' @export
ParamReal = R6Class(
  "ParamReal",
  inherit = ParamSimple,
  public = list(
   
    # member variables
    allow.inf = NULL,
    lower = NULL,
    upper = NULL,
    
    # constructor
    initialize = function(id, special.vals = NULL, default = NULL, lower = -Inf, upper = Inf, allow.inf = FALSE, tags = NULL) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        checkNumber(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok, finite = !allow.inf)
      }
      
      # construct super class
      super$initialize(id = id, storage.type = "numeric", check = check, special.vals = special.vals, default = default, tags = tags)

      # write member variables
      self$lower = assertNumber(lower, finite = !allow.inf)
      self$upper = assertNumber(upper, finite = !allow.inf)
      self$allow.inf = assertFlag(allow.inf)
      assert_true(lower <= upper)
    },

    # public methods
    sampleVector = function(n = 1L) {
      assert_true(self$has.finite.bounds)
      runif(n, min = self$lower, max = self$upper)
    },
    denormVector = function(x) {
      normalize(x = x, method = "range", range = self$range)
    }
  ),
  active = list(
    range = function() c(self$lower, self$upper),
    has.finite.bounds = function() all(is.finite(self$range))
  )
)
