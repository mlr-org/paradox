context("ParamFct")

test_that("test if ParamFct constructor works", {
  p = ParamFct$new(id = "test", values = c("a", "b"))
  expect_equal(p$values, c("a", "b"))
  expect_equal(p$nlevels, 2L)

  # we dont allow NAs as values
  expect_error(ParamFct$new(id = "test", values = c("a", NA)))
})

test_that("printer works", {
  p = ParamFct$new(id = "x", values = c("a", "b"))
  expect_output(print(p), "x f \\{a,b\\}")
})




