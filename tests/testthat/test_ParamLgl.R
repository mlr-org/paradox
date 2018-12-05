context("ParamLgl")

test_that("constructor works", {
  p = ParamLgl$new(id = "test")
  expect_equal(p$id, "test")
  expect_equal(p$values, c(TRUE, FALSE))
  expect_true(p$has_finite_bounds)
  expect_equal(p$nlevels, 2L)
})


test_that("printer works", {
  p = ParamLgl$new(id = "x")
  expect_output(print(p), "x l")
})



