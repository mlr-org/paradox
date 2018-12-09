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
        checker = function(x) check_flag(x),
        special_vals = special_vals,
        default = default,
        tags = tags
      )
    },

    denorm_vector = function(x) {
      x < 0.5 #FIXME: Do we have to take care of x==0.5?
    },

    value_to_string = function(x, ...) {
      as.character(x)
    }
  ),
  active = list(
    nlevels = function() 2L
  ),

  private = list(
    get_range_string = function() "",
    get_type_string = function() "l"
  )
)
