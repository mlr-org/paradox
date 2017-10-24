#' @title ParamTree
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a tree form.
#'
#' @return [\code{\link{ParamTree}}].
#' @family ParamHelpers
#' @export
ParamTree = R6Class("ParamSetTree",
  inherit = ParamSet,
  public = list(

    # member variables
    id = NULL,
    handle = NULL,
    flatval = NULL,
    depend = NULL,

    # constructor
    initialize = function(id = NULL, handle = NULL, depend = NULL) {
      self$id = id
      self$handle = handle
      self$flatval = list()
      self$depend = depend
    },

    # public methods
    sample = function() {
      self$handle$sample()
    }
  ),
  private = list(
  )
)
ParamTree$fac = function(...) {
  input = list(...)
  ps = ParamHandle$new(id = "Root")
  ps$visitor$parseFlat(input)
  return(ps$getFirstMandChild)
}
ParamTree$dn = function(node, depend = NULL) {
  return(list(node = node, depend = depend))
}

