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
    special.vals = NULL, # special values as list, can not be changed after initialization
    initialize = function(id, type, check, special.vals, default) {
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
      
      self$default.expr = assertPossibleExpr(default, self$assert)

      self$special.vals = special.vals
    },
    sample = function(n = 1L) stop("sample function not implemented!"),
    denorm = function(x) stop("denorm function not implemented!")
  ),
  active = list(
    default = function() evalIfExpr(self$default.expr, self)
  ),
  private = list(
    default.expr = NULL
  )
)
