context("ParamReal")

test_that("test if ParamReal constructor works", {
  p = ParamReal$new(id = "test", lower = 1L, upper = 10L)
  expect_data_table(p$sample())

  # check that we can create param with Inf bounds
  p = ParamReal$new(id = "test", lower = 1L)
  expect_equal(p$lower, 1L)
  expect_equal(p$upper, Inf)
  
  # check some invalid arg settings
  expect_error(ParamReal$new(id = "x", lower = NULL), "not 'NULL'")
  expect_error(ParamReal$new(id = "x", lower = 1, upper = 0), "lower <= upper")
  expect_error(ParamReal$new(id = "x", lower = Inf, upper = 0), "lower <= upper")
})

test_that("ParamReal allow.inf works", {
  p = ParamReal$new(id = "x", lower = 1L, upper = 10L, allow.inf = FALSE)
  expect_true(p$test(1))
  expect_false(p$test(Inf))
  
  p = ParamReal$new(id = "x", lower = 1L, allow.inf = FALSE)
  expect_true(p$test(1))
  expect_false(p$test(Inf))
  
  p = ParamReal$new(id = "x", lower = 1L, allow.inf = TRUE)
  expect_true(p$test(1))
  expect_true(p$test(Inf))
})



