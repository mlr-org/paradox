#' @import checkmate
#' 
# Evaluates x if it is an expression. Otherwise just return the value.
evalIfExpr = function(x, param) {
  if (is.expression(x)) {
    eval(x, dict = param$handle$root$dictionary)
  } else {
    x
  }
}

# assert for possible expressions
assertPossibleExpr = function(x, assert, ...) {
  if (!is.expression(x)) { x = assert(x, ...) }
  invisible(x)
}
