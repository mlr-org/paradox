#' @title Fixed Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent a fixed constant parameter.
#'
#' @section Member Variables:
#' \describe{
#'   \item{value}{[\code{any}] \cr
#'     All categorical values.}
#' }
#'
#' @export
ParamFix = R6Class(
  "ParamFix",
  inherit = Parameter,
  public = list(
    initialize = function(id, storage_type = NULL, default, tags = NULL) {
      assert_atomic(default)
      assert_true(length(default) == 1)
      if (is.null(storage_type)) {
        storage_type = class(default)[1]
      } else {
        assert_class(default, storage_type)
      }
      super$initialize(
        id = id,
        storage_type = storage_type,
        lower = NA_real_,
        upper = NA_real_,
        values = NULL,
        special_vals = NULL,
        checker = function(x) check_true(identical(x, default)),
        default = default,
        tags = tags
      )
    },

    map_unitint_to_values = function(x) {
      rep(self$default, length(x))
    }
  ),
   active = list(
    nlevels = function() 1,
    is_bounded = function() TRUE,
    values = function() self$default
  )
)
