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
#' @export
ParamInt = R6Class( "ParamInt", inherit = Parameter,
  public = list(
    initialize = function(id, lower = -Inf, upper = Inf, special_vals = NULL, default = NULL, tags = NULL) {
      # arg check lower and upper, we need handle Inf special case, that is not an int
      lower = if (identical(lower, Inf) || identical(lower, -Inf)) lower else asInt(lower)
      upper = if (identical(upper, Inf) || identical(upper, -Inf)) upper else asInt(upper)
      super$initialize(
        id = id,
        storage_type = "integer",
        lower = lower,
        upper = upper,
        values = NULL,
        special_vals = special_vals,
        default = default,
        checker = function(x) checkInt(x, lower = self$lower, upper = self$upper),
        tags = tags
      )
      assert_true(lower <= upper)
    },

    denorm_vector = function(x) {
      assert_true(self$has_finite_bounds)
      r = self$range + c(-0.5, 0.5)
      res = as.integer(round(r[1] + x * diff(r)))
      res = ifelse(res > self$upper, self$upper, res) #if we rounded up, we have to go down
      res = ifelse(res < self$lower, self$lower, res) #if we rounded down, we have to go up
      res
    },

    value_to_string = function(x, num.format = "%.3g", ...) {
        sprintf(num.format, x)
    }
  ),

  active = list(
    range = function() c(self$lower, self$upper),
    has_finite_bounds = function() all(is.finite(self$range))
  ),

  private = list(
    get_range_string = function() sprintf("[%g, %g]", self$lower, self$upper),
    get_type_string = function() "i"
  )
)
