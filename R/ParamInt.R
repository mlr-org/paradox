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
#' Inherited from \code{ParamBase}:
#' @inheritSection ParamBase Member Variables
#'
#' @section Methods:
#'   \emph{none}
#'
#' Inherited from \code{ParamBase}
#' @inheritSection ParamBase Methods
#'
#' @section Active Bindings:
#'   \emph{none}
#'
#' Inherited from \code{ParamBase}
#' @inheritSection ParamBase Active Bindings
#'
#' @return [\code{\link{ParamInt}}].
#' @family ParamBase
#' @export
ParamInt = R6Class(
  "ParamInt",
  inherit = ParamBase,
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
      self$lower = if (identical(lower, Inf) || identical(lower, -Inf)) lower else asInt(lower)
      self$upper = if (identical(upper, Inf) || identical(upper, -Inf)) upper else asInt(upper)
      stopifnot(lower <= upper)

      # construct super class
      super$initialize(id = id, storage_type = "integer", check = check, special_vals = special_vals, default = default, tags = tags)
    },

    # public methods
    sampleVector = function(n = 1L) {
      assert_true(self$has_finite_bounds)
      as.integer(runif(n, min = self$lower, max = self$upper + 1L))
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
      catf(": {%g, ..., %g}\n", self$lower, self$upper)
    },
    value_to_string = function(x, show.missing.values = FALSE, num.format = "%.3g", ...) {
      if (is.na(x)) {
        # Return "NA" or "", depending on show.missing.values.
        ifthenelse(show.missing.values, "NA", "")
      } else {
        sprintf(num.format, x)
      }
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
