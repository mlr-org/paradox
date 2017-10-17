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

# res int(1) - aimed at resolution
# nlevels int() - number of levels per param or NA if continuous.
# return: int vector that gives the resolution for each param leading to 
optGridRes = function(n, nlevels) {
  nnames = names(nlevels)
  p = length(nlevels) # number of params
  x = ceiling(n^(1/p)) # upper bound for factor levels
  consumed = !is.na(nlevels) & nlevels < x # find out which params we can completely use
  n.rest = n / prod(nlevels[consumed]) # remove them from the n
  p.rest = p - sum(consumed) # remaining number of parameters we have to optimize for
  x.rest = ceiling(n.rest^(1/p.rest)) # recalculate uper bound for number of params for each level
  optFun = function(k) {
    x.rest^k * (x.rest-1)^(p.rest-k) - n.rest
  }
  opt.k = 0:p.rest
  k = opt.k[which.min(abs(sapply(opt.k, optFun) - 0))]
  # build result vector
  res = rep(NA_integer_, p)
  names(res) = nnames
  # define the values where we can use all levels
  res[consumed] = nlevels[consumed]
  # use the first appearing params to use the higher resolution
  empty = which(is.na(res))
  res[empty[seq_len(k)]] = x.rest
  # fill the rest with the lower available resolution
  res[is.na(res)] = x.rest - 1
  return(res)
}