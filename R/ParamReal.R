#' @title Real Parameter Object
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent numeric, real valued parameters.
#' 
#' @section Member Variables:
#' \describe{
#'   \item{lower}{[\code{integer(1)|-Inf}] \cr
#'     Upper boundary.}
#'   \item{upper}{[\code{integer(1)|-Inf}] \cr
#'     Lower boundary.}
#'   \item{allow.inf}{[\code{logical(1)}] \cr
#'     Are the values \code{-Inf} and \code{Inf} feasible?}
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
#' @return [\code{\link{ParamReal}}].
#' @family ParamSimple
#' @export
ParamReal = R6Class(
  "ParamReal",
  inherit = ParamSimple,
  public = list(
   
    # member variables
    allow.inf = NULL,
    lower = NULL,
    upper = NULL,
    
    # constructor
    initialize = function(id, special.vals = NULL, default = NULL, lower = -Inf, upper = Inf, allow.inf = FALSE, tags = NULL) {

      check = function(x, na.ok = FALSE, null.ok = FALSE) {
        if (testSpecialVals(self, x)) return(TRUE)
        checkNumber(x, lower = self$lower, upper = self$upper, na.ok = na.ok, null.ok = null.ok, finite = !self$allow.inf)
      }
     
      # write member variables
      self$lower = assertNumber(lower)
      self$upper = assertNumber(upper)
      self$allow.inf = assertFlag(allow.inf)
      assert_true(lower <= upper)
       
      # construct super class
      super$initialize(id = id, storage.type = "numeric", check = check, special.vals = special.vals, default = default, tags = tags)

    },

    # public methods
    sampleVector = function(n = 1L) {
      assert_true(self$has.finite.bounds)
      runif (n, min = self$lower, max = self$upper)
    },
    denormVector = function(x) {
      assert_true(self$has.finite.bounds)
      self$range[1] + x * diff(self$range)
    }
  ),
  active = list(
    range = function() c(self$lower, self$upper),
    has.finite.bounds = function() all(is.finite(self$range))
  )
)
