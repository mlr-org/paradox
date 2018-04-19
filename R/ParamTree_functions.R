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

#' @title make conditional tree node
#'
#' @description Define a node in the hyper parameter tree with dependencies
#'
#' @param node ParamSimple
#' @param depend A list with id and val representing the dependency for the current node
#' @return List of class NodeParamSetTree
#' @export
makeCondTreeNode = function(node, depend = NULL) {
  node = list(node = node, depend = depend)
  class(node) = "NodeWithDependency"
  return(node)
}

#' @export
makeCondTreeNode2 = function(node, did, expr) {
  makeCondTreeNode(node = node, depend = list(id = did, fun = fun))
}

