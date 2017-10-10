#' @title Unstorage.typed Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent unstorage.typed parameters.
#'
#' @return [\code{\link{ParamUnstorage.typed}}].
#' @family ParamSimple
#' @export
ParamUnstorage.typed = R6Class(
  "ParamUnstorage.typed",
  inherit = ParamSimple,
  public = list(
    # member variables

    # constructor
    initialize = function(id, default = NULL, tags = NULL) {
      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (!na.ok && identical(x, NA)) "Value is NA"
        if (!null.ok && is.null(x)) "Value is NULL"
        return(TRUE)
      }

      # construct super class
      super$initialize(id = id, storage.type = "list", check = check, default = default, tags = tags)
    },

    # public methods
    sampleVector = function(n = 1L) {
      stop("Untped Param can not be sampled")
    },
    denormVector = function(x) {
      stop("Unstorage.typed Param can not be denormed")
    }
  )
)
