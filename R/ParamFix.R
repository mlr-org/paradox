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
    initialize = function(id, storage_type, default, tags = NULL) {
      assert_true(length(default), 1)
      assert_class(default, storage_type)
      super$initialize(
        id = id,
        storage_type = storage_type,
        checker = function(x) assert_true(identical(x, default)),
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
