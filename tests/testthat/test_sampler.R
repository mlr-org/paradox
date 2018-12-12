context("sampling")

test_that("sampling 1d unif", {
  p = th_param_dbl()
  s = Sampler1DDblUnif$new(p)
  x = s$sample(20)
  expect_data_table(x, ncols = 1L, nrows = 20L)
  expect_numeric(x$th_param_dbl, lower = -10, upper = 10)
})

test_that("sampling 1d cat", {
  p = th_param_fct()
  s = Sampler1DFct$new(p)
  x = s$sample(20)
  expect_data_table(x, ncols = 1L, nrows = 20L)
  expect_character(x$th_param_fct)
})


test_that("multivariate", {
  p1 = th_param_fct()
  p2 = th_param_dbl()
  s1 = Sampler1DFct$new(p1)
  s2 = Sampler1DDblUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))
  x = s$sample(20)
  expect_data_table(x, ncols = 2L, nrows = 20L)
  expect_numeric(x$th_param_dbl, lower = -10, upper = 10)
  expect_character(x$th_param_fct)
})

test_that("sampling of number requires finite bounds", {
  p = ParamInt$new(id = "x", lower = 1)
  s = expect_error(Sampler1DIntUnif$new(p), "is_bounded")

  p = ParamDbl$new(id = "x", lower = 1)
  s = expect_error(Sampler1DDblUnif$new(p), "is_bounded")
})



