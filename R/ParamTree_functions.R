#' @title  ParamTree Factory method
#' 
#' @description
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

#' @title 
#' 
#' @description
#' 
#' @param node Elementary ParamNode
#' @param depend value
#' @return returndes
#' @examples
#' ParamTreeDn(node = ParamCategorical$new(id = "model", values = c("SVM", "RF"))),
#' ParamTreeDn(node = ParamReal$new(id = "C", lower = 0, upper = 100), depend = list(id = "model", val = "SVM")),
#' @export 
ParamTreeDn = function(node, depend = NULL) {
  return(list(node = node, depend = depend))
}

