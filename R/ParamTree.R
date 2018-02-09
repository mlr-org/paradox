#' @title ParamSetTree
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a tree form.
#'
#' @return [\code{\link{ParamSetTree}}].
#' @family ParamHelpers
#' @export
ParamSetTree = R6Class("ParamSetTree",  ##FIXME: this class is still under development, the difference between ParamSetTree and ParamHandle is that one could "hang" a tree(of nodes) directly(instead of only one node) to a ParamHandle or another tree
  inherit = ParamSet,
  public = list(

    # member variables
    id = NULL,
    handle = NULL,
    flatval = NULL,
    depend = NULL,

    # constructor
    initialize = function(id = NULL, handle = NULL, depend = NULL) {
      self$id = assertNames(id)
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

