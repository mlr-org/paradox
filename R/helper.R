# unify value output
as_dt_cols = function(x, names) {
  dt = as.data.table(x)
  dt = setnames(dt, names)
  return(dt)
}

oversample_forbidden2 = function(n, param, oversample_rate = 2, max_tries = 100, sample_generator, sample_validator) {
  x = sample_generator(n = round(oversample_rate * n))
  ind_restriction = sample_validator(x)
  # select the first n elements, that are valid
  ind_restriction = ind_restriction & cumsum(ind_restriction) <= n
  this_try = 1
  x = x[ind_restriction,]
  while (this_try <= max_tries && nrow(x) < n) {
    x_new = sample_generator(n = round(oversample_rate * n), old_x = x)
    ind_restriction = sample_validator(x_new)
    # select the first n elements, that are valid
    ind_restriction = ind_restriction & cumsum(ind_restriction) <= n - nrow(x)
    x_new = x_new[ind_restriction, ]
    x = rbind(x, x_new)
    this_try = this_try + 1
  }
  if (nrow(x) < n) {
    warning("Not enough valid param values for %s sampled (%i from %i)", param$id, nrow(x), n)
  }
  return(x)
}

# x list of columns (or data.table)
# fun function that accepts a list of the same structure as x, but each list element has just one item, meaning this function works just on one row.
vectorized_for_param_set_flat = function(x, fun) {
  fn = function(...) {fun(list(...))}
  unlist(.mapply(fn, x, list()))
}

test_special_vals = function(param, x) {
  !is.null(param$special_vals) && any(map_lgl(param$special_vals, identical, x))
}

could_list_be_data_table = function(x) {
  is.list(x) && length(unique(lengths(x))) == 1L && test_named(x, type = "strict")
}

ensure_data_table = function(x, ...) {
  if (test_data_frame(x) || could_list_be_data_table(x)) {
    x = as.data.table(x)
  } else {
    assert_data_table(x, ...)
  }
}

# res int(1) - aimed at resolution
# nlevels int() - number of levels per param or NA if continuous.
# return: int vector that gives the resolution for each param leading to
opt_grid_res = function(n, nlevels) {
  nnames = names(nlevels)
  p = length(nlevels) # number of params
  x = ceiling(n^(1/p)) # upper bound for factor levels
  consumed = !is.na(nlevels) & nlevels < x # find out which params we can completely use
  n_rest = n / prod(nlevels[consumed]) # remove them from the n
  p_rest = p - sum(consumed) # remaining number of parameters we have to optimize for
  x_rest = ceiling(n_rest^(1/p_rest)) # recalculate uper bound for number of params for each level
  optFun = function(k) {
    x_rest^k * (x_rest-1)^(p_rest-k) - n_rest
  }
  opt_k = 0:p_rest
  k = opt_k[which.min(abs(map_dbl(opt_k, optFun)))]
  # build result vector
  res = rep(NA_integer_, p)
  names(res) = nnames
  # define the values where we can use all levels
  res[consumed] = nlevels[consumed]
  # use the first appearing params to use the higher resolution
  empty = which(is.na(res))
  res[empty[seq_len(k)]] = x_rest
  # fill the rest with the lower available resolution
  res[is.na(res)] = x_rest - 1
  return(res)
}
