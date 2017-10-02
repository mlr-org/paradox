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
    id = NULL,
    val = NULL,
    handle = NULL,
    depend = NULL,  # by default no dependency
    children = NULL,
    initialize = function(id = NULL) {
      self$id = id
      self$children = new.env()  # parent = self
    },
    addChild = function(cnode) {
      assign(cnode$id, cnode, self$children)
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

ParamHandle = R6Class("ParamHandle",
  inherit = ParamBase,
  public = list(
  ),
  private = list(
  )
)

ps = ParamSetTree$new("SVM")

ps$addChild(ParamSetTree$new("Kernel"))
