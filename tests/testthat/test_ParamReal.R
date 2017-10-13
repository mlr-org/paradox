context("ParamReal")

test_that("constructor works", {
  p = ParamReal$new(id = "test", lower = 1, upper = 10)
  expect_data_table(p$sample())

  # check that we can create param with Inf bounds
  p = ParamReal$new(id = "test", lower = 1)
  expect_equal(p$lower, 1)
  expect_equal(p$upper, Inf)
  
  # check some invalid arg settings
  expect_error(ParamReal$new(id = "x", lower = NULL), "not 'NULL'")
  expect_error(ParamReal$new(id = "x", lower = 1, upper = 0), "lower <= upper")
  expect_error(ParamReal$new(id = "x", lower = Inf, upper = 0), "lower <= upper")
})

test_that("allow.inf works", {
  p = ParamReal$new(id = "x", lower = 1, upper = 10, allow.inf = FALSE)
  expect_true(p$test(1))
  expect_false(p$test(Inf))
  
  p = ParamReal$new(id = "x", lower = 1, allow.inf = FALSE)
  expect_true(p$test(1))
  expect_false(p$test(Inf))
  
  p = ParamReal$new(id = "x", lower = 1, allow.inf = TRUE)
  expect_true(p$test(1))
  expect_true(p$test(Inf))
})


test_that("has.finite.bounds works", {
  expect_true(ParamReal$new(id = "x", lower = 1, upper = 10)$has.finite.bounds)
  expect_false(ParamReal$new(id = "x", lower = 1)$has.finite.bounds)
  expect_false(ParamReal$new(id = "x")$has.finite.bounds)
})


test_that("sample requires finite bounds", {
  p = ParamReal$new(id = "x", lower = 1)
  expect_error(p$sample(), "has.finite.bounds")
})


