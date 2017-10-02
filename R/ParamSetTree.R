#' @title ParamSetTree
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a tree form.
#'
#' @return [\code{\link{ParamSetTree}}].
#' @family ParamHelpers
#' @export
ParamSetTree = R6Class("ParamSetTree",
  inherit = ParamSet,
  public = list(
    val = NULL,
    handle = NULL,
    depend = NULL,  # by default no dependency
    children = list(),
    initialize = function(id = NULL) {
    },
    addChild = function(cnode) {
      self$children[cnode$id] = cnode
    },
    addChildren = function(flatnodes) {
    },
    setParent = function(pnode) {
      self$depend = pnode
    }
  ),
  private = list(
  )
)
