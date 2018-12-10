#' @title Categorical Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent categorical parameters.
#'
#' @section Member Variables:
#' \describe{
#'   \item{values}{[\code{character}] \cr
#'     All categorical values.}
#' }
#'
#' @export
ParamFct = R6Class(
  "ParamFct",
  inherit = Parameter,
  public = list(
    initialize = function(id, values, default = NULL, special_vals = NULL, tags = NULL) {
      super$initialize(
        id = id,
        storage_type = "character",
        lower = NA_real_,
        upper = NA_real_,
        values = values,
        special_vals = special_vals,
        checker = function(x) check_choice(x, choices = self$values),
        default = default,
        tags = tags
      )
    },

    # maps [0, 1/k] --> a, (1/k, 2/k] --> b, ..., ((k-1)/k, 1] --> z
    # NB: this is a bit inconsistent for the first part
    map_unitint_to_values = function(x) {
      res = cut(x, breaks = seq(0, 1, length.out = self$nlevels+1), include.lowest = TRUE)
      levels(res) = self$values
      as.character(res)
    }
  ),

  active = list(
    nlevels = function() length(self$values)
  ),

  private = list(
    get_range_string = function() sprintf("{%s}", paste0(self$values, collapse = ",")),

    get_type_string = function() "f"
  )
)
