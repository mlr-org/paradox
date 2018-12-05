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

    # member variables
    values = NULL,

    # constructor
    initialize = function(id, values, default = NULL, special_vals = NULL, tags = NULL) {

      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (na.ok && is.na(x) || test_special_vals(self, x)) return(TRUE)
        checkChoice(x, choices = self$values, null.ok = null.ok)
      }

      # write member variables
      self$values = assert_character(values, any.missing = FALSE, unique = TRUE)

      # construct super class
      super$initialize(id = id, storage_type = "character", check = check, default = default, special_vals = special_vals, tags = tags)
    },

    # public methods
    denorm_vector = function(x) {
      res = cut(x, breaks = seq(0, 1, length.out = self$nlevels+1), include.lowest = TRUE)
      levels(res) = self$values
      as.character(res)
    }
  ),
  active = list(
    nlevels = function() length(self$values),
    has_finite_bounds = function() TRUE
  ),
  private = list(
    get_range_string = function() sprintf("{%s}", paste0(self$values, collapse = ",")),
    get_type_string = function() "f"
  )
)
