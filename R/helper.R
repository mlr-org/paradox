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

# unify value output
asDtCols = function(x, names) {
  dt = as.data.table(x)
  dt = setnames(dt, names)
  return(dt)
}

# just a thought
oversampleForbiddenVector = function(n = 1L, param, oversample.rate = 2, max.tries = 10L) {
  x = param$sampleVector(n = round(oversample.rate * n)) 
  ind.allowed = BBmisc::vlapply(x, function(x) param$test(x))
  this.try = 1
  good.ones = sum(ind.allowed)
  x = x[good.ones]
  while (this.try <= max.tries && good.ones < n) {
    x.new = param$sampleVector(n = round(oversample.rate * n))
    ind.allowed = BBmisc::vlapply(x, function(x) param$test(x))
    good.ones = good.ones = sum(ind.allowed)
    x = c(x, head(x.new, n - good.ones))
  }
  if (good.ones < n) {
    BBmisc::stopf("Not enough valid param values for %s sampled (%i from %i)", param$id, good.ones, n)
  }
  return(x)
}