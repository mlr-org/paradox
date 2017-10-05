#' @import checkmate
#' 
# Evaluates x if it is an call Otherwise just return the value.
evalIfCall = function(x, param) {
  if (is.call(x)) {
    eval(x, dict = param$handle$root$dictionary)
  } else {
    x
  }
}

# assert for possible call
assertPossibleCall = function(x, assert, ...) {
  if (!is.call(x)) { x = assert(x, ...) }
  invisible(x)
}

# unify value output
asDtCols = function(x, names) {
  dt = as.data.table(x)
  dt = setnames(dt, names)
  return(dt)
}

# just a thought

oversampleForbidden2 = function(n, param, oversample.rate, max.tries, sample.generator, sample.validator, sample.combine) {
  x = sample.generator(n = round(oversample.rate * n)) 
  ind.restriction = sample.validator(x)
  this.try = 1
  good.ones = sum(ind.restriction)
  x = x[good.ones]
  while (this.try <= max.tries && good.ones < n) {
    x.new = sample.generator(n = round(oversample.rate * n))
    ind.restriction = sample.validator(x.new)
    good.ones = sum(ind.restriction)
    x = sample.combine(x, head(x.new, n - good.ones))
  }
  if (good.ones < n) {
    BBmisc::stopf("Not enough valid param values for %s sampled (%i from %i)", param$id, good.ones, n)
  }
  return(x)

}
oversampleForbiddenVector = function(n = 1L, param, oversample.rate = 2, max.tries = 10L) {
  sample.generator = param$sampleVector
  sample.validator = function(x) BBmisc::vlapply(x, function(z) param$test(x))
  oversampleForbidden2(n, param = param, oversample.rate, max.tries,sample.generator, sample.validator, sample.combine = c)
}
 
oversampleForbidden = function(n = 1L, param, oversample.rate = 2L, max.tries = 10L) {
  sample.generator = param$sample
  sample.validator = function(x) .mapply(function(x) pram$test(x), xdf, list())
  oversampleForbidden2(n, param = param, oversample.rate, max.tries,sample.generator, sample.validator, sample.combine = cbind)
}

# vectorizeTrafo = function(trafo) {
# 
# }