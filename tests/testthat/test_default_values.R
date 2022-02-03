test_that("default_values", {
  param_set = ps(x = p_int(default = 3), y = p_dbl(default = 2.5))
  values = default_values(param_set)
  expect_names(names(values), permutation.of = c("x", "y"))
})
