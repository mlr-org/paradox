#' title 
#' 
#' description
#' 
#' @param ... value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
ParamTreeFac = function(...) {
  input = list(...)
  ps = ParamHandle$new(id = "Root")
  ps$visitor$parseFlat(input)
  return(ps$getFirstMandChild)
}
#' title 
#' 
#' description
#' 
#' @param node value
#' @param depend value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 

ParamTreeDn = function(node, depend = NULL) {
  return(list(node = node, depend = depend))
}

