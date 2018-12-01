context("sampling")

test_that("sampling 1d unif", {
  p = th_param_real
  s = Sampler1DRealUnif$new(p)
  x = s$sample(20)
  expect_data_table(x, ncols = 1L, nrows = 20L)
  expect_numeric(x$th_param_real, lower = -10, upper = 10)
})

test_that("sampling 1d cat", {
  p = th_param_categorical
  s = Sampler1DCat$new(p)
  x = s$sample(20)
  expect_data_table(x, ncols = 1L, nrows = 20L)
  expect_factor(x$th_param_categorical, levels = letters[1:3])
})


test_that("multivariate", {
  p1 = th_param_categorical
  p2 = th_param_real
  s1 = Sampler1DCat$new(p1)
  s2 = Sampler1DRealUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))
  x = s$sample(20)
  expect_data_table(x, ncols = 2L, nrows = 20L)
  expect_numeric(x$th_param_real, lower = -10, upper = 10)
  expect_factor(x$th_param_categorical, levels = letters[1:3])
})




