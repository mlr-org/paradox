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
      super$initialize(id = id, type = "integer", check = check, special.vals = special.vals, default = default, tags = tags)
      
      # we need to allow INF here, thats not an int
      self$lower = assertNumber(lower, null.ok = TRUE)
      self$upper = assertNumber(upper, null.ok = TRUE)
    },

    # public methods
    sampleVector = function(n = 1L) {
      as.integer(round(runif(n, min = self$lower-0.5, max = self$upper+0.5)))
    },
    denormVector = function(x) {
      as.integer(round(BBmisc::normalize(x = x, method = "range", range = self$range + c(-0.5, 0.5))))
    }
  ),
  active = list(
    range = function() c(self$lower, self$upper),
    is.finite = function() all(is.finite(self$range))
  )
)
