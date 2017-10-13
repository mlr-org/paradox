# unify value output
asDtCols = function(x, names) {
  dt = as.data.table(x)
  dt = setnames(dt, names)
  return(dt)
}

oversampleForbidden2 = function(n, param, oversample.rate, max.tries, sample.generator, sample.validator, sample.combine) {
  x = sample.generator(n = round(oversample.rate * n))
  ind.restriction = sample.validator(x)
  this.try = 1
  good.ones = sum(ind.restriction)
  x = x[ind.restriction]
  while (this.try <= max.tries && good.ones < n) {
    x.new = sample.generator(n = round(oversample.rate * n))
    ind.restriction = sample.validator(x.new)
    good.ones = sum(ind.restriction)
    browser()
    x = sample.combine(x, head(x.new, n - good.ones))
  }
  if (good.ones < n) {
    stopf("Not enough valid param values for %s sampled (%i from %i)", param$id, good.ones, n)
  }
  return(head(x, n))
}
