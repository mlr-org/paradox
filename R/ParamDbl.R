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
    lower = NULL,
    upper = NULL,

    initialize = function(id, lower = -Inf, upper = Inf, special_vals = list(), default = NULL, tags = NULL) {
      assert_number(lower, na.ok = TRUE)
      assert_number(upper, na.ok = TRUE)
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
      self$lower = lower
      self$upper = upper
    }
  ),

  active = list(
    values = function() NULL,
    nlevels = function() ifelse(self$span == 0, 1, Inf),
    is_bounded = function() all(is.finite(self$range)),
    storage_type = function() "numeric",
    range = function() c(self$lower, self$upper),
    span = function() self$upper - self$lower
  ),

  private = list(
    .check = function(x) checkNumber(x, lower = self$lower, upper = self$upper),

    # maps [0,1]*span + lower
    .map_unitint_to_values = function(x) {
      assert_true(self$is_bounded)
      self$lower + x * self$span
    },

    .fix = function(x) {
      self$upper = self$lower = x
    }
  )
)
