#' @title Boolean Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent boolean parameters.
#'
#' @export
ParamLgl = R6Class("ParamLgl",
  inherit = Parameter,
  public = list(

    initialize = function(id, special_vals = NULL, default = NULL, tags = NULL) {
      super$initialize(
        id = id,
        storage_type = "logical",
        lower = NA_real_,
        upper = NA_real_,
        values = NULL,
        special_vals = special_vals,
        default = default,
        tags = tags
      )
    }
  ),

  active = list(
    nlevels = function() ifelse(is.null(self$values), 2L, 1L),
    is_bounded = function() TRUE
  ),

  private = list(
    .check = function(x) {
      if (is.null(self$values)) {
        check_flag(x)
      } else {
        assert_true(identical(x, self$values))
      }
    },

    .map_unitint_to_values = function(x) {
      if (is.null(self$values)) {
        x < 0.5 #FIXME: Do we have to take care of x==0.5?
      } else {
        self$values
      }
    },

    .fix = function(x) {
      self$values = x
    }
  )
)
