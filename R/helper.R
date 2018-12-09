# unify value output
as_dt_cols = function(x, names) {
  dt = as.data.table(x)
  dt = setnames(dt, names)
  return(dt)
}

# x list of columns (or data.table)
# fun function that accepts a list of the same structure as x, but each list element has just one item, meaning this function works just on one row.
vectorized_for_param_set_flat = function(x, fun) {
  fn = function(...) {fun(list(...))}
  unlist(.mapply(fn, x, list()))
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

