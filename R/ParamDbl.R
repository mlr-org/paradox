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
        storage_type = "numeric",
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

    # maps [0,1]*span + lower
    map_unitint_to_values = function(x) {
      assert_true(self$is_bounded)
      self$lower + x * self$span
    }
  ),

  active = list(
    range = function() c(self$lower, self$upper),
    is_bounded = function() all(is.finite(self$range)),
    span = function() self$upper - self$lower
  )
)
