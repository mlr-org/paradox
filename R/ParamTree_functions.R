#' @title  ParamTree Factory method
#' 
#' @description Define a list of Node in the hyper-parameter definition
#' 
#' @param ... set of ParamTreeDn
#' @return the root node of the ParamTree
#' @export 
ParamTreeFac = function(...) {
  input = list(...)
  ps = ParamHandle$new(id = "Root")
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
#' @examples
#' makeCondTreeNode(node = ParamCategorical$new(id = "model", values = c("SVM", "RF")))
#' makeCondTreeNode(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", val = "SVM"))
#' @export 
makeCondTreeNode = function(node, depend = NULL) {
  node = list(node = node, depend = depend)
  class(node) = "NodeParamSetTree"
  return(node)
}

