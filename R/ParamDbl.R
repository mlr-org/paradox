#' @title Float Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent numeric, real valued parameters.
#'
#' @section Member Variables:
#' \describe{
#'   \item{lower}{[\code{numeric(1)}, \code{default = -Inf}] \cr
#'     Upper bound for feasible values.}
#'   \item{upper}{[\code{integer(1)}, \code{default = Inf}] \cr
#'     Lower bound for feasible values.}
#'   \item{allow_inf}{[\code{logical(1)}, \code{default = TRUE}] \cr
#'     Are values \code{-Inf} and \code{Inf} feasible?}
#' }
#' @export
ParamDbl = R6Class("ParamDbl", inherit = Parameter,
  public = list(
    initialize = function(id, lower = -Inf, upper = Inf, special_vals = NULL, default = NULL, tags = NULL) {
      super$initialize(
        id = id,
        storage_type = "double",
        lower = lower,
        upper = upper,
        values = NULL,
        special_vals = special_vals,
        checker = function(x) checkNumber(x, lower = self$lower, upper = self$upper),
        default = default,
        tags = tags
      )
      assert_true(lower <= upper)
    },

    denorm_vector = function(x) {
      assert_true(self$has_finite_bounds)
      self$lower + x * self$span
    }

  ),
  active = list(
    range = function() c(self$lower, self$upper),
    has_finite_bounds = function() all(is.finite(self$range)),
    center = function() {
      assert_true(self$has_finite_bounds)
      (self$lower + self$upper) / 2
    },
    span = function() self$upper - self$lower
  ),

  private = list(
    get_range_string = function() sprintf("[%g, %g]", self$lower, self$upper),
    get_type_string = function() "d"
  )
)
