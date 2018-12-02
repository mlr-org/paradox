context("sampling")

test_that("sampling 1d unif", {
  p = th_param_real
  s = Sampler1DFloatUnif$new(p)
  x = s$sample(20)
  expect_data_table(x, ncols = 1L, nrows = 20L)
  expect_numeric(x$th_param_real, lower = -10, upper = 10)
})

test_that("sampling 1d cat", {
  p = th_param_categorical
  s = Sampler1DCat$new(p)
  x = s$sample(20)
  expect_data_table(x, ncols = 1L, nrows = 20L)
  expect_character(x$th_param_categorical)
})


test_that("multivariate", {
  p1 = th_param_categorical
  p2 = th_param_real
  s1 = Sampler1DCat$new(p1)
  s2 = Sampler1DFloatUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))
  x = s$sample(20)
  expect_data_table(x, ncols = 2L, nrows = 20L)
  expect_numeric(x$th_param_real, lower = -10, upper = 10)
  expect_character(x$th_param_categorical)
})

test_that("sampling of number requires finite bounds", {
  p = ParamInt$new(id = "x", lower = 1)
  s = expect_error(Sampler1DIntUnif$new(p), "has_finite_bounds")

  p = ParamFloat$new(id = "x", lower = 1)
  s = expect_error(Sampler1DFloatUnif$new(p), "has_finite_bounds")
})



