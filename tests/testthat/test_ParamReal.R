context("ParamReal")

test_that("test if ParamReal constructor works", {
  p = ParamReal$new(id = "test", lower = 1L, upper = 10L)
  expect_data_table(p$sample())

  # check that we can create param with Inf bounds
  p = ParamReal$new(id = "test", lower = 1L)
  expect_equal(p$lower, 1L)
  expect_equal(p$upper, Inf)
  
  # check some invalid arg settings
  expect_error(ParamInt$new(id = "x", lower = NULL), "not 'NULL'")
})


