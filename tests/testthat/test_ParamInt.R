context("ParamInt")

test_that("constructor works", {
  p = ParamInt$new(id = "test", lower = 1L, upper = 10L)
  expect_equal(p$id, "test")
  expect_equal(p$lower, 1L)
  expect_equal(p$upper, 10L)

  # check that we can create param with Inf bounds
  p = ParamInt$new(id = "test", lower = 1L)
  expect_equal(p$lower, 1L)
  expect_equal(p$upper, Inf)

  # check some invalid arg settings
  expect_error(ParamInt$new(id = "x", lower = NULL), "not 'NULL'")
  expect_error(ParamInt$new(id = "x", lower = 1, upper = 0), "lower <= upper")
  expect_error(ParamInt$new(id = "x", lower = Inf, upper = 0), "lower <= upper")
})

test_that("is_bounded works", {
  expect_true(ParamInt$new(id = "x", lower = 1, upper = 10)$is_bounded)
  expect_false(ParamInt$new(id = "x", lower = 1)$is_bounded)
  expect_false(ParamInt$new(id = "x")$is_bounded)
})

