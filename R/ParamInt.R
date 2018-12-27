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
    lower = NULL,
    upper = NULL,

    initialize = function(id, lower = -Inf, upper = Inf, special_vals = NULL, default = NULL, tags = NULL) {
      assert_number(lower, na.ok = TRUE)
      assert_number(upper, na.ok = TRUE)
      assert_true(lower <= upper)
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
      # FIXME: doc this
      self$lower = ceiling(lower)
      self$upper = floor(upper)
    }
  ),

  active = list(
    values = function() NULL,
    nlevels = function() diff(self$range),
    is_bounded = function() all(is.finite(self$range)),
    storage_type = function() "integer",
    range = function() c(self$lower, self$upper),
    span = function() self$upper - self$lower
  ),

  private = list(
    .check = function(x) checkInt(x, lower = self$lower, upper = self$upper),

    .map_unitint_to_values = function(x) {
      #FIXME: Can we make this shorter?
      assert_true(self$is_bounded)
      r = self$range + c(-0.5, 0.5)
      res = as.integer(round(r[1] + x * diff(r)))
      res = ifelse(res > self$upper, self$upper, res) #if we rounded up, we have to go down
      res = ifelse(res < self$lower, self$lower, res) #if we rounded down, we have to go up
      res
    },

    .fix = function(x) {
      self$lower = self$upper = x
    }
  )
)
