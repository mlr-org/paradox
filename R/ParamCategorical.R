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
#' Inherited from \code{ParamSimple}:
#' @inheritSection ParamSimple Member Variables
#'
#' @section Methods:
#'   \emph{none}
#'
#' Inherited from \code{ParamSimple}
#' @inheritSection ParamSimple Methods
#'
#' @section Active Bindings:
#'   \emph{none}
#'
#' Inherited from \code{ParamSimple}
#' @inheritSection ParamSimple Active Bindings
#'
#' @return [\code{\link{ParamCategorical}}].
#' @family ParamSimple
#' @export
ParamCategorical = R6Class(
  "ParamCategorical",
  inherit = ParamSimple,
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
    sampleVector = function(n = 1L) {
      sample(self$values, n, replace = TRUE)
    },
    denorm_vector = function(x) {
      res = cut(x, breaks = seq(0, 1, length.out = self$nlevels+1), include.lowest = TRUE)
      levels(res) = self$values
      as.character(res)
    },
    print = function(...) {
      super$print(newline = FALSE, ...)
      cat(sprintf(": {%s}\n", paste(self$values, collapse = ", ")))
    }
  ),
  active = list(
    nlevels = function() length(self$values),
    has_finite_bounds = function() TRUE
  ),
  private = list(
  )
)
