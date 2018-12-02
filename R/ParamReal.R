#' @title Real Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent numeric, real valued parameters.
#'
#' @section Member Variables:
#' \describe{
#'   \item{lower}{[\code{numeric(1), \code{default = -Inf}] \cr
#'     Upper bound for feasible values.}
#'   \item{upper}{[\code{integer(1), \code{default = Inf}}] \cr
#'     Lower bound for feasible values.}
#'   \item{allow_inf}{[\code{logical(1)}, \code{default = TRUE}] \cr
#'     Are values \code{-Inf} and \code{Inf} feasible?}
#' }
#' @export
ParamReal = R6Class("ParamReal", inherit = ParamNumber,
  public = list(
    # member variables
    allow_inf = NULL,

    initialize = function(id, special_vals = NULL, default = NULL, lower = -Inf, upper = Inf, allow_inf = FALSE, tags = NULL) {
      self$allow_inf = assert_flag(allow_inf)
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (test_special_vals(self, x)) return(TRUE)
        checkNumber(x, lower = self$lower, upper = self$upper, na.ok = na.ok, null.ok = null.ok, finite = !self$allow_inf)
      }
      assert_number(lower)
      assert_number(upper)
      super$initialize(id = id, storage_type = "numeric", check = check, special_vals = special_vals,
        lower = lower, upper = upper, default = default, tags = tags)
    },

    # public methods
    denorm_vector = function(x) {
      assert_true(self$has_finite_bounds)
      self$range[1] + x * diff(self$range)
    },
    print = function(...) {
      super$print(newline = FALSE, ...)
      catf(": [%g, %g]\n", self$lower, self$upper)
    }
  ),
  active = list(
    center = function() {
      assert_true(self$has_finite_bounds)
      (self$lower + self$upper) / 2
    }
  )
)
