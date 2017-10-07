# usefull trafo objects
#' @export
trafo.2exp = function(x) 2^x
#' @export
trafo.exp = function(x) exp(x)
#' @export
trafo.10exp = function(x) 10^x
#' @export
trafo.log = function(x) log(x)
#' @export
trafo.getfun = function(x) get(x, mode = "function")
#' @export
trafo.getdict = function(x, dict) dict[x]
