context("ParamCateg")

test_that("test if ParamCateg constructor works", {
  p = ParamCateg$new(id = "test", values = c("a", "b"))
  expect_equal(p$values, c("a", "b"))
  expect_equal(p$nlevels, 2L)
  expect_true(p$has_finite_bounds)

  # we dont allow NAs as values
  expect_error(ParamCateg$new(id = "test", values = c("a", NA)))
})

test_that("printer works", {
  p = ParamCateg$new(id = "x", values = c("a", "b"))
  expect_output(print(p), "x \\[character\\]")
  expect_output(print(p), "\\{a, b\\}")
})




