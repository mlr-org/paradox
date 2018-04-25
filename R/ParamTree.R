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

ParamSetTree = R6Class("ParamSetTree",
  inherit = ParamSet,
  public = list(
    # member variables
    ns.id = NULL,  # namespace id
    root.handle = NULL,
    parent.set = NULL,
    child.set = NULL,

    initialize = function(ns.id = NULL, ...) {
      self$ns.id = assertNames(ns.id)
      self$root.handle = ParamTreeFac(...)
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

    asample = function() {
      self$root.handle$asample()
      if (!is.null(self$child.set)) {
        self$child.set$asample()
      }
    },

    toStringVal = function() {
      self$root.handle$toStringVal()
      if (!is.null(self$child.set)) {
        self$child.set$toStringVal()
      }
    },

    render2str = function() {
      self$asample()
      self$toStringVal()
    },

    sample = function(n = 1) {
      if (n == 1L) {
        res = self$root.handle$sample(1)
        return(res)
      }
      res.list = lapply(1:n, function(i) {
        res = self$root.handle$sample(1)
      if (!is.null(self$child.set)) {
        res = plyr::rbind.fill(res, self$child.set$sample(1))
      }
        return(res)
      })
      rbindlist(res.list)
    }
  ),
  private = list(
  )
)

