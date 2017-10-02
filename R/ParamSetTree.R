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
    allowedVal = NULL,
    initialize = function(id = NULL, depend = NULL) {
      self$id = id
      self$val = NULL
      self$children = new.env()  # parent = self
    },
    addChild = function(cnode) {
      if(is.character(cnode)) cnode = ParamSetTree$new(cnode)
      assign(cnode$id, cnode, self$children)
      self$val = names(self$children)
      return(cnode)
    },
    addCondChild = function() {

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

ps = ParamSetTree$new("SVM")

res = ps$addChild(ParamSetTree$new("Kernel"))
#$addChild("rbf")
