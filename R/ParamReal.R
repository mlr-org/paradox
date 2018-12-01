#' @title Real Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent numeric, real valued parameters.
#'
#' @section Member Variables:
#' \describe{
#'   \item{lower}{[\code{integer(1)|-Inf}] \cr
#'     Upper boundary.}
#'   \item{upper}{[\code{integer(1)|-Inf}] \cr
#'     Lower boundary.}
#'   \item{allow_inf}{[\code{logical(1)}] \cr
#'     Are the values \code{-Inf} and \code{Inf} feasible?}
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
#' @return [\code{\link{ParamReal}}].
#' @family ParamBase
#' @export
ParamReal = R6Class(
  "ParamReal",
  inherit = ParamBase,
  public = list(

    # member variables
    allow_inf = NULL,
    lower = NULL,
    upper = NULL,

    # constructor
    initialize = function(id, special_vals = NULL, default = NULL, lower = -Inf, upper = Inf, allow_inf = FALSE, tags = NULL) {

      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (test_special_vals(self, x)) return(TRUE)
        checkNumber(x, lower = self$lower, upper = self$upper, na.ok = na.ok, null.ok = null.ok, finite = !self$allow_inf)
      }

      # write member variables
      self$lower = assert_number(lower)
      self$upper = assert_number(upper)
      self$allow_inf = assert_flag(allow_inf)
      assert_true(lower <= upper)

      # construct super class
      super$initialize(id = id, storage_type = "numeric", check = check, special_vals = special_vals, default = default, tags = tags)

    },

    # public methods
    sampleVector = function(n = 1L) {
      assert_true(self$has_finite_bounds)
      runif(n, min = self$lower, max = self$upper)
    },
    denorm_vector = function(x) {
      assert_true(self$has_finite_bounds)
      self$range[1] + x * diff(self$range)
    },
    print = function(...) {
      super$print(newline = FALSE, ...)
      catf(": [%i, %i]\n", self$lower, self$upper)
    }
  ),
  active = list(
    range = function() c(self$lower, self$upper),
    has_finite_bounds = function() all(is.finite(self$range))
  )
)
