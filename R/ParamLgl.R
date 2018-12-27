#' @title Boolean Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent boolean parameters.
#'
#' @export
# FIXME: doc them all on ?Parameter page?
ParamLgl = R6Class("ParamLgl", inherit = Parameter,
  public = list(
    initialize = function(id, special_vals = NULL, default = NULL, tags = NULL) {
      super$initialize(id, special_vals = special_vals, default = default, tags = tags)
    }
  ),

  active = list(
    lower = function() NA_real_,
    upper = function() NA_real_,
    # FIXME: what is this? has to do with fix?
    nlevels = function() ifelse(is.null(self$values), 2L, 1L),
    is_bounded = function() TRUE,
    storage_type = function() "logical"
  ),

  private = list(
    .check = function(x) {
      if (is.null(self$values)) {
        check_flag(x)
      } else {
        # FIXME: why do we call an asser in a check function?
        assert_true(identical(x, self$values))
      }
    },

    .map_unitint_to_values = function(x) {
      # FIXME: this is bullshot code? why should values be NULL?
      if (is.null(self$values)) {
        x < 0.5 #FIXME: Do we have to take care of x==0.5? # FIXME: no we dont
      } else {
        self$values
      }
    },

    .fix = function(x) {
      self$values = x
    }
  )
)
