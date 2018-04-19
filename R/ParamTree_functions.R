#' @title  ParamTree Factory method
#'
#' @description Define a list of Node in the hyper-parameter definition
#'
#' @param ... set of ParamTreeDn
#' @return the root node of the ParamTree
#' @export
ParamTreeFac = function(...) {
  input = list(...)
  lapply(input, function(x) {
    assertTRUE(test_class(x, "ParamSimple") | test_class(x, "NodeWithDependency")) })
  ps = ParamHandle$new(id = "Root")  #FIXME: id should be defined outside the code
  ps$visitor$parseFlat(input)
  return(ps$getFirstMandChild)
}

#' @title Add dependent Node
#'
#' @description Define a node in the hyper parameter tree with dependencies
#'
#' @param node ParamSimple
#' @param did dependent node id
#' @param expr expression that provides the dependency
#' @return List of class NodeParamSetTree
#' @export
addDep = function(node, did, expr) {
  makeCondTreeNode(node = node, depend = list(id = did, fun = expr))
}

#' @title make conditional tree node
#'
#' @description Define a node in the hyper parameter tree with dependencies
#'
#' @param node ParamSimple
#' @param depend A list of field c("id", "val", "fun")
#' @return List of class NodeParamSetTree
#' @export
makeCondTreeNode = function(node, depend = NULL) {
  node = list(node = node, depend = depend)
  class(node) = "NodeWithDependency"
  return(node)
}

