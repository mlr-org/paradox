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

    map_unitint_to_values = function(x) {
      assert_true(self$is_bounded)
      r = self$range + c(-0.5, 0.5)
      res = as.integer(round(r[1] + x * diff(r)))
      res = ifelse(res > self$upper, self$upper, res) #if we rounded up, we have to go down
      res = ifelse(res < self$lower, self$lower, res) #if we rounded down, we have to go up
      res
    }
  ),

  active = list(
    range = function() c(self$lower, self$upper),
    is_bounded = function() all(is.finite(self$range))
  )
)
