#' @title ParamSetTree
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a tree form.
#' The difference between ParamSetTree and Paramroot.handle is that one could "hang" a ParamSetTree(tree of nodes, instead of only one node) directly to a ParamHandle or another tree.
#' This class serves the situation when one set of parameters decide on another set of parameters.
#'
#' @return [\code{\link{ParamSetTree}}].
#' @family ParamHelpers
#' @export

#FIXME: this class is still under development, see description above of this file
ParamSetTree = R6Class("ParamSetTree",
  inherit = ParamSet,
  public = list(
    # member variables
    ns.id = NULL,  # namespace id
    root.handle = NULL,
    parent.set = NULL,
    child.set = NULL,

    # constructor
    initialize = function(ns.id = NULL, root.handle = NULL) {
      self$ns.id = assertNames(ns.id)
      self$root.handle = root.handle
    },

    # public methods
    # after a tree factory is called, directly set the root
    setRootHandle = function(handle) {
      self$root.handle = handle
    },

    setChild = function(child.set) {
      self$child.set = child.set
      child.set$parent.set = self
    },

    sample = function() {
      self$root.handle$sample()
      if (!is.null(self$child.set)) {
        self$child.set$sample()
      }
    },

    toStringVal = function() {
      self$root.handle$toStringVal()
      if (!is.null(self$child.set)) {
        self$child.set$toStringVal()
      }
    }
  ),
  private = list(
  )
)

