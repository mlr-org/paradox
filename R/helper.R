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
  if (!is.null(param$special_vals) && any(vapply(param$special_vals, identical, x, FUN.VALUE = NA))) {
    # TRUE, if value is one of special_vals
    TRUE
  } else {
    FALSE
  }
}

could_list_be_data_table = function(x) {
  is.list(x) && length(unique(lengths(x))) == 1L && test_named(x, type = "strict")
}

ensure_data_table = function(x, ...) {
  if (test_data_frame(x) || could_list_be_data_table(x)) {
    x = as.data.table(x)
  }
  assert_data_table(x, ...)
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
  k = opt_k[which.min(abs(sapply(opt_k, optFun) - 0))]
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


convert_to_string = function (x, num.format = "%.4g", clip.len = 15L) {
  names2 = function (x, missing.val = NA_character_) {
      n = names(x)
      if (is.null(n))
        return(rep.int(missing.val, length(x)))
      replace(n, is.na(n) | n == "", missing.val)
  }

  convObj = function(x) {
    string = if (is.atomic(x) && !is.null(x) && length(x) == 0L) {
      sprintf("%s(0)", cl)
    } else if (cl == "numeric") {
      paste0(sprintf(num.format, x), collapse = ",")
    } else if (cl %in% c("integer", "logical", "expression", "character")) {
      paste0(as.character(x), collapse = ",")
    } else {
      sprintf("<%s>", cl)
    }

    if (nchar(string) > clip.len)
      string = paste0(substr(string, 1L, clip.len - 3L), "...")
    string
  }

  cl = class(x)[1L]

  if (cl == "list") {
    if (length(x) == 0L)
      return("list()")
    ns = names2(x, missing.val = "<unnamed>")
    ss = lapply(x, convObj)
    paste0(paste0(ns, "=", ss), collapse = ", ")
  } else {
    convObj(x)
  }
}
