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
    default.varpar = NULL,
    special.vals = NULL, # special values as list, can not be changed after initialization
    trafo = NULL, # function to transform the value before evaluation

    # constructor
    initialize = function(id, type, check, special.vals, default, trafo, allowed, tags) {
      # wrap the underlaying check to allow speical.vals and return an error for when the allowed quote is not TRUE.
      assertList(special.vals, null.ok = TRUE)
      if (!is.null(special.vals) || !is.null(allowed)) {
        allowed.varpar = substitute(allowed)
        check.wrap = function(x) {
          # TRUE, if value is one of special.vals
          if (!is.null(special.vals) && x %in% special.vals) TRUE
          # character if value is not allowed
          if (!is.null(allowed.varpar)) {
            if (!isTRUE(eval(x, envir = setNames(list(x), self$id)))) {
              sprintf("Value %s is not allowed by %s.", as.character(x), deparse(x))
            }
          }
          else check(x)
        }
      } else {
        check.wrap = check
      }

      # assert allowed to only contain the variable of self
      if (!is.null(allowed))
        assertSubset(all.vars(substitute(allowed)), self$id)

      # init
      
      # construct super class
      super$initialize(id = id, type = type, check = check.wrap, allowed = allowed, tags = tags)
      
      self$default.varpar = assertPossibleCall(default, self$assert, null.ok = TRUE)
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
    sampleVector = function(n = 1L) {
      if (!is.null(self$allowed)) {
        stop("WHY am i?")
        oversampleForbiddenVector(n = n, param = self)
      } else {
        self$sampleVectorUnrestricted(n = n)
      }
    },
    sampleVectorUnrestricted = function(n = 1L) {
      # samples vector values without respecting what is 'allowed'
      stop("sampleVectorUnrestricted not implemented")
    },
    denormVector = function(x) {
      stop("denorm function not implemented!")
    },
    transformVector = function(x) {
      self$trafo(x)
    }
    # Do we want the following? The user can call ps$transform(ps$sample())
    #,
    #sampleTransformedVector = function(n = 1L) self$trasformValue(self$sample(n = n)),
    #denormTransformedVector = function(x) self$transform(self$denorm(x = x))
  ),
  active = list(
    default = function() evalIfCall(self$default.varpar, self)
  )
)
