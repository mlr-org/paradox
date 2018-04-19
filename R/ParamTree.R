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
    depend = NULL,

    # constructor
    initialize = function(ns.id = NULL, root.handle = NULL, depend = NULL) {
      self$ns.id = assertNames(ns.id)
      self$root.handle = handle
      self$depend = depend
    },

    # public methods
    attachTo = function(depend) {
      stop("attach operation from one tree to another not implemented yet")
    },

    sample = function() {
      self$root.handle$sample()
    }
  ),
  private = list(
  )
)

