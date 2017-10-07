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
    finite = NULL,
    lower = NULL,
    upper = NULL,
    
    # constructor
    initialize = function(id, special.vals = NULL, default = NULL, lower = -Inf, upper = Inf, finite = TRUE, tags = NULL) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        checkNumber(x, lower = lower, upper = upper, na.ok = na.ok, null.ok = null.ok, finite = finite)
      }
      
      # construct super class
      super$initialize(id = id, type = "numeric", check = check, special.vals = special.vals, default = default, tags = tags)

      # write member variables
      self$lower = self$assert(lower, null.ok = TRUE)
      self$upper = self$assert(upper, null.ok = TRUE)
      self$finite = assertFlag(finite)
    },

    # public methods
    sampleVector = function(n = 1L) {
      runif(n, min = self$lower, max = self$upper)
    },
    denormVector = function(x) {
      BBmisc::normalize(x = x, method = "range", range = self$range)
    }
  ),
  active = list(
    range = function() c(self$lower, self$upper),
    is.finite = function() all(is.finite(self$range))
  )
)
