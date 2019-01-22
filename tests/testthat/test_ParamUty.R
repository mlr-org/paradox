context("ParamUty")

test_that("ParamUty", {
  p = ParamUty$new(id = "x")
  expect_true(p$check(FALSE))
  expect_true(p$check(NULL))
  expect_true(p$check(NA))

  p = ParamUty$new(id = "x", custom_check = function(x)
    if(is.null(x)) "foo" else TRUE)
  expect_true(p$check(FALSE))
  expect_string(p$check(NULL), "foo")
  expect_true(p$check(NA))
})

