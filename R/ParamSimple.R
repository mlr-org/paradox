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
    default.expr = NULL,
    special.vals = NULL, # special values as list, can not be changed after initialization
    trafo = NULL, # function to transform the value before evaluation

    # constructor
    initialize = function(id, type, check, special.vals, default, trafo) {
      # handle check for speical.vals
      assertList(special.vals, null.ok = TRUE)
      if (!is.null(special.vals)) {
        check.wrap = function(x) {
          if (x %in% special.vals) TRUE
          else check(x)
        }
      } else {
        check.wrap = check
      }

      # init
      super$initialize(id = id, type = type, check = check.wrap)
      
      self$default.expr = assertPossibleExpr(default, self$assert, null.ok = TRUE)
      self$special.vals = special.vals

      # handle trafo
      if (is.null(trafo)) trafo = identity
      self$trafo = assertFunction(trafo)
    },

    # public methods
    # Overwriting ParamNode Methods
    sample = function(n = 1L) asDtCols(self$sampleVector(n = n), self$id),
    denorm = function(x) asDtCols(self$denormVector(x[[self$id]]), self$id),
    transform = function(x) asDtCols(self$transformVector(x[[self$id]]), self$id),
    
    # ParamSimpleMethods
    sampleVector = function(n = 1L) stop("sample function not implemented!"),
    denormVector = function(x) stop("denorm function not implemented!"),
    transformVector = function(x) {
      self$trafo(x)
    }
    # Do we want the following? The user can call ps$transform(ps$sample())
    #,
    #sampleTransformedVector = function(n = 1L) self$trasformValue(self$sample(n = n)),
    #denormTransformedVector = function(x) self$transform(self$denorm(x = x))
  ),
  active = list(
    default = function() evalIfExpr(self$default.expr, self)
  )
)
