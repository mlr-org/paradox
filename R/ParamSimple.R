#' @title Simple parameter object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent simple parameters.
#'
#' @return [\code{\link{ParamSimple}}].
#' @family ParamHelpers
#' @export
ParamSimple = R6Class(
  "ParamSimple",
  inherit = ParamNode,
  public = list(
   
    # member variables
    default = NULL,
    special.vals = NULL, # special values as list, can not be changed after initialization

    # constructor
    initialize = function(id, type, check, special.vals, default, tags) {
      # wrap the underlaying check to allow speical.vals.
      assertList(special.vals, null.ok = TRUE)
      if (!is.null(special.vals)) {
        check.wrap = function(x) {
          # TRUE, if value is one of special.vals
          if (!is.null(special.vals) && x %in% special.vals) TRUE
          else check(x)
        }
      } else {
        check.wrap = check
      }
      
      # construct super class
      super$initialize(id = id, type = type, check = check.wrap, tags = tags)
      
      # set member variables
      self$default = self$assert(default, null.ok = TRUE)
      self$special.vals = assertList(special.vals, null.ok = TRUE)
    },

    # public methods
    # Overwriting ParamNode Methods
    sample = function(n = 1L) asDtCols(self$sampleVector(n = n), self$id),
    denorm = function(x) asDtCols(self$denormVector(x[[self$id]]), self$id),
    
    # ParamSimpleMethods
    sampleVector = function(n = 1L) {
      # samples vector values without respecting what is 'restriction'
      stop("sampleVector not implemented")
    },
    denormVector = function(x) {
      stop("denorm function not implemented!")
    }
  ),
)
