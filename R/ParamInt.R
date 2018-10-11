#' @title Integer Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent Integer parameters.
#'
#' @section Member Variables:
#' \describe{
#'   \item{lower}{[\code{integer(1)|-Inf}] \cr
#'     Upper boundary.}
#'   \item{upper}{[\code{integer(1)|-Inf}] \cr
#'     Lower boundary.}
#' }
#'
#' Inherited from \code{ParamSimple}:
#' @inheritSection ParamSimple Member Variables
#'
#' @section Methods:
#'   \emph{none}
#'
#' Inherited from \code{ParamSimple}
#' @inheritSection ParamSimple Methods
#'
#' @section Active Bindings:
#'   \emph{none}
#'
#' Inherited from \code{ParamSimple}
#' @inheritSection ParamSimple Active Bindings
#'
#' @return [\code{\link{ParamInt}}].
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
    initialize = function(id, special_vals = NULL, default = NULL, lower = -Inf, upper = Inf, tags = NULL) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (test_special_vals(self, x)) return(TRUE)
        checkInt(x, lower = self$lower, upper = self$upper, na.ok = na.ok, null.ok = null.ok)
      }

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

      # construct super class
      super$initialize(id = id, storage_type = "integer", check = check, special_vals = special_vals, default = default, tags = tags)
    },

    # public methods
    sampleVector = function(n = 1L) {
      assert_true(self$has_finite_bounds)
      as.integer(round(runif(n, min = self$lower-0.5, max = self$upper+0.5)))
    },
    denorm_vector = function(x) {
      assert_true(self$has_finite_bounds)
      r = self$range + c(-0.5, 0.5)
      res = as.integer(round(r[1] + x * diff(r)))
      res = ifelse(res > self$upper, self$upper, res) #if we rounded up, we have to go down
      res = ifelse(res < self$lower, self$lower, res) #if we rounded down, we have to go up
      res
    },
    print = function(...) {
      super$print(newline = FALSE, ...)
      cat(sprintf(": {%i, ..., %i}\n", self$lower, self$upper))
    }
  ),
  active = list(
    nlevels = function() {
      if (self$has_finite_bounds) self$upper - self$lower + 1L
      else NA_integer_
    },
    values = function() {
      if (self$has_finite_bounds) seq(self$lower, self$upper)
      else NA
    },
    range = function() c(self$lower, self$upper),
    has_finite_bounds = function() all(is.finite(self$range))
  )
)
