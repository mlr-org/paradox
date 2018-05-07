#' @title  ParamTree Factory method
#'
#' @description Define a list of Node in the hyper-parameter definition
#' @param id The name of the Hinge node
#' @param ... set of ParamTreeDn
#' @return the root node of the ParamTree
#' @export
ParamTreeFac = function(id, ...) {
  input = list(...)
  lapply(input, function(x) {
    assertTRUE(test_class(x, "ParamSimple") | test_class(x, "NodeWithDependency"))
  })
  ps = PHinge$new(id = id)
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
#' @param depend A list of field c("id", "val", "fun") #' @return List of class NodeParamSetTree
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
  root = ParamSetTree$new("L0", ...)  # the first layer
  root$rt.hinge$setNamePrefix("L0")
  cc = root
  for (i in 1:nr) {
    name = paste0("L", as.character(i))
    psn = ParamSetTree$new(name, ...)
    psn$rt.hinge$setNamePrefix(name)
    cc$setChild(psn)
    cc = psn
  }
  return(root)
}

keras_helper = function(input.shape, output.shape, output.act, loss, lr, list.par.val) {
  hh = function(reg_type, val) {
    paste0(reg_type, "(l=", as.character(val), ")")
  }
  s0 = sprintf("model = keras_model_sequential();model ")
  input.shape = input.shape
  list.str = lapply(list.par.val, function(x) {
  sprintf("%%>%%layer_dense(units = %d, activation = '%s', input_shape = c(%d), kernel_regularizer = %s, bias_regularizer = %s)", x$layer_dense.units, x$activation_fun, input.shape, hh(x$reg_type, x$kernel_regularizer), hh(x$reg_type, x$bias_regularizer))
  })
  ss = Reduce(paste0, list.str)
  ss = paste0(s0, ss)
  s1 = sprintf("%%>%%layer_dense(units = %d, activation = '%s');", output.shape, output.act)
  ss = paste0(ss, s1)
  s2 = sprintf("model$compile(loss = '%s', optimizer = optimizer_rmsprop(lr = %f)); model", loss, lr)
  ss = paste0(ss, s2)
  ss
}

