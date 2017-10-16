# unify value output
asDtCols = function(x, names) {
  dt = as.data.table(x)
  dt = setnames(dt, names)
  return(dt)
}

oversampleForbidden2 = function(n, param, oversample.rate = 2, max.tries = 100, sample.generator, sample.validator) {
  x = sample.generator(n = round(oversample.rate * n))
  ind.restriction = sample.validator(x)
  # select the first n elements, that are valid
  ind.restriction = ind.restriction & cumsum(ind.restriction) <= n 
  this.try = 1
  x = x[ind.restriction,]
  while (this.try <= max.tries && nrow(x) < n) {
    x.new = sample.generator(n = round(oversample.rate * n), old.x = x)
    ind.restriction = sample.validator(x.new)
    # select the first n elements, that are valid
    ind.restriction = ind.restriction & cumsum(ind.restriction) <= n - nrow(x) 
    x.new = x.new[ind.restriction, ]
    x = rbind(x, x.new)
    this.try = this.try + 1
  }
  if (nrow(x) < n) {
    warning("Not enough valid param values for %s sampled (%i from %i)", param$id, nrow(x), n)
  }
  return(x)
}

# x list of columns (or data.table)
# fun function that accepts a list of the same structure as x, but each list element has just one item, meaning this function works just on one row.
vectorizedForParamSetFlat = function(x, fun) {
  fn = function(...) {fun(list(...))}
  unlist(.mapply(fn, x, list()))
}

testSpecialVals = function(param, x) {
  if (!is.null(param$special.vals) && any(vlapply(param$special.vals, identical, x))) {
    # TRUE, if value is one of special.vals
    TRUE
  } else {
    FALSE
  }
} 