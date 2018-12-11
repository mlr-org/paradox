context("ParamDbl")

test_that("constructor works", {
  p = ParamDbl$new(id = "test", lower = 1, upper = 10)
  expect_equal(p$id, "test")
  expect_equal(p$lower, 1)
  expect_equal(p$upper, 10)

  # check that we can create param with Inf bounds
  p = ParamDbl$new(id = "test", lower = 1)
  expect_equal(p$lower, 1)
  expect_equal(p$upper, Inf)

  # check some invalid arg settings
  expect_error(ParamDbl$new(id = "x", lower = NULL), "not 'NULL'")
  expect_error(ParamDbl$new(id = "x", lower = 1, upper = 0), "lower <= upper")
  expect_error(ParamDbl$new(id = "x", lower = Inf, upper = 0), "lower <= upper")
})

test_that("allowing inf as feasible value works", {
  p = ParamDbl$new(id = "x", lower = 1, upper = 10)
  expect_true(p$test(1))
  expect_false(p$test(Inf))

  p = ParamDbl$new(id = "x", lower = 1, special_vals = list(Inf))
  expect_true(p$test(1))
  expect_true(p$test(Inf))
})


test_that("has_finite_bounds works", {
  expect_true(ParamDbl$new(id = "x", lower = 1, upper = 10)$has_finite_bounds)
  expect_false(ParamDbl$new(id = "x", lower = 1)$has_finite_bounds)
  expect_false(ParamDbl$new(id = "x")$has_finite_bounds)
})


