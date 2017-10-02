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
    flatval = NULL,
    handle = NULL,
    depend = NULL,  # by default no dependency
    mand.children = NULL,
    cond.children = NULL,
    initialize = function(id = NULL, depend = NULL) {
      self$id = id
      self$val = ParamInt$new("demo")
      self$handle = NULL
      self$depend = depend
      self$flatval = list()
      self$mand.children = new.env()
      self$cond.children = new.env()
    },
    addMandChild = function(cnode) {
      #if(is.character(cnode)) cnode = ParamSetTree$new(cnode)
      assign(cnode$id, cnode, self$mand.children)
      self$flatval$mand = names(self$mand.children)
      return(cnode)
    },
    addCondChild = function() {

    },
    addChildren = function(flatnodes) {

    },
    setParent = function(pnode) {
      self$depend = pnode
    },
    sampleCurrentNode = function() {

    },
    sampleMandChild = function() {

    },
    sampleCondChildChain = function(expr) {

    },
    sample = function() {
      sampleCurrentNode()
      sampleMandChild()
      sampleCondChildChain()
    }
  ),
  private = list(
  )
)


