#' @title Categorical Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent categorical parameters.
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
    initialize = function(id, values, default = NULL, special.vals = NULL, tags = NULL) {

      check = function(x, na.ok = FALSE, null.ok = FALSE) checkChoice(x, choices = values, null.ok = null.ok)

      # construct super class
      super$initialize(id = id, storage.type = "character", check = check, default = default, special.vals = special.vals, tags = tags)

      # write member variables
      self$values = assertCharacter(values, any.missing = FALSE, unique = TRUE)
    },

    # public methods
    sampleVector = function(n = 1L) {
      sample(self$values, n, replace = TRUE)
    },
    denormVector = function(x) {
      res = cut(x, breaks = seq(0, 1, length.out = self$nlevels+1))
      levels(res) = self$values
      as.character(res)
    }
  ),
  active = list(
    nlevels = function() length(self$values),
    is.finite = function() TRUE
  ),
  private = list(
  )
)
