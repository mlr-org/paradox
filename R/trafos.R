
#' @family trafo
#' @export
trafo_exp2 = function(id) {
  function(x, param_set) {
    force(id)
    for (i in id) {
      x[[i]] = 2^x[[i]]
    }
    return(x)
  }
}
