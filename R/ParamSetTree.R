#' @title ParamSetTree
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent set of parameters in a tree form.
#' The difference between ParamSetTree and Paramrt.hinge is that one could "hang" a ParamSetTree(tree of nodes, instead of only one node) directly to a ParamHandle or another tree.
#' This class serves the situation when one set of parameters decide on another set of parameters.
#'
#' @return [\code{\link{ParamSetTree}}].
#' @family ParamSet
#' @export
ParamSetTree = R6Class("ParamSetTree",
  inherit = ParamSet,
  public = list(
    # member variables
    ns.id = NULL,  # namespace id
    rt.hinge = NULL,
    parent.set = NULL,
    child.set = NULL,
    context = NULL,

    #initialize = function(ns.id, ..., nr = 1L) {
    #  self$ns.id = assertNames(ns.id)
    #  if (nr == 1L) {
    #  self$rt.hinge = ParamTreeFac(ns.id, ...)
    #  }
    #  if (nr > 1) {
    #    root = recursiveParaFac(nr, ...)
    #    self$rt.hinge = root$rt.hinge
    #    self$ns.id = root$ns.id
    #    self$child.set = root$child.set
    #    self$parent.set = root$parent.set
    #  }
    #},

    initialize = function(ns.id, ..., context = NULL) {
      self$ns.id = assertNames(ns.id)
      self$rt.hinge = ParamTreeFac(ns.id, ...)
      self$context = context
    },



    # public methods
    # after a tree factory is called, directly set the root
    setRootHandle = function(handle) {
      self$rt.hinge = handle
    },

    setChild = function(child.set) {
      self$child.set = child.set
      child.set$parent.set = self
    },

    asample = function() {
      self$rt.hinge$asample()
      if (!is.null(self$child.set)) {
        self$child.set$asample()
      }
    },

    toStringVal = function() {
      self$rt.hinge$toStringVal()
      if (!is.null(self$child.set)) {
        self$child.set$toStringVal()
      }
    },

    asample.render2str = function() {
      self$asample()
      self$toStringVal()
    },

    sample = function(n = 1) {
      res.list = lapply(1:n, function(i) {
        res = self$rt.hinge$sample(1L)
        if (!is.null(self$child.set)) {
          temp = self$child.set$sample(1L)
          res = cbind(res, temp)  # combine the subspace(colums) of current tree and the child tree
        }
        return(res)
      })
      rbindlist(res.list, fill = TRUE)
    },

    getFlatList = function() {
      res = self$rt.hinge$getList()
      if (!is.null(self$child.set)) {
        temp = self$child.set$getFlatList()
        res = c(res, temp)
      }
      res
    },

    getRecursiveList = function(res = list()) {
      if (is.null(self$child.set)) {
        temp = self$rt.hinge$getList()
        n = length(res)
        res[[n + 1L]] = temp
        return(res)
      } else {
        res = self$child.set$getRecursiveList(res = res)
        temp = self$rt.hinge$getList()
        n = length(res)
        res[[n + 1L]] = temp
      }
      res
    }
  ),
  private = list(
  )
)
