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
    assertTRUE(test_class(x, "ParamSimple") | test_class(x, "NodeWithDependency"))
  })
  ps = PHinge$new(id = "Root")  #FIXME: do we need a hard coded id here?
  ps$visitor$parseFlat(input)
  return(ps)
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

#' @title make recursive ParamsetTree
#'
#' @description Repeat the same structure
#'
#' @param nr The number of repetitiveness
#' @param ... Params to add
#' @return A ParamSetTree
#' @export
recursiveParaFac = function(nr, ...) {
  root = ParamSetTree$new("l0", ...)  # the first layer
  root$rt.hinge$setNamePrefix("l0")
  cc = root
  for (i in 1:nr) {
    psn = ParamSetTree$new(as.character(i), ...)
    psn$rt.hinge$setNamePrefix(paste0("l", as.character(i)))
    cc$setChild(psn)
    cc = psn
  }
  return(root)
}

keras_helper = function() {
  expr = sprintf("model = keras_model_sequential();model %%>%%")
  input.shape = input.shape
  {
  sprintf("layer_dense(units = %d, activation = '%s', input_shape = c(%d), kernel_regularizer = %s, bias_regularizer = %s)", nhidden, act1, input_shape, kernel_regularizer, bias_regularizer)
  }
  sprintf("%%>%%layer_dense(units = %d, activation = '%s');", output_shape, act2)
  sprintf("model$compile(loss = '%s', optimizer = optimizer_rmsprop(lr = %f)); model", loss, lr)
}

