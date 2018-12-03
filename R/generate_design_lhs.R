generate_design_lhs = function(param_set, n, lhs_function = lhs::maximinLHS) {
  assert_int(n, lower = 1L)
  assert_function(lhs_function, args = c("n", "k"))
  lhs_des = lhs_function(n, k = param_set$length)
  # converts the LHS output to values of the parameters
  sample_converter = function(lhs_des) {
    vec_cols = lapply(seq_len(ncol(lhs_des)), function(z) lhs_des[,z])
    names(vec_cols) = param_set$ids
    param_set$denorm(vec_cols)
  }
  if (!is.null(param_set$restriction)) {
    # we work on the matrix that is the LHS output to be able to use augmentLHS to sample additional values.
    sample_generator = function(n, old_x = NULL) {
      if (is.null(old_x)) return(lhs_des)
      lhs_des = lhs::augmentLHS(lhs = old_x, m = n)
      tail(lhs_des, n)
    }
    # validates the LHS output, according to the param restrictions
    sample_validator = function(lhs_des) {
      vectorized_for_param_set_flat(sample_converter(lhs_des), param_set$test)
    }
    lhs_des = oversample_forbidden2(n = n, param_set = param_set, oversample_rate = 1, sample_generator = sample_generator, sample_validator = sample_validator)
  }
  sample_converter(lhs_des)
}

