context("ParamSet")

test_that("test if ParamSet constructor works", {
  params = list(x = ParamInt$new('x', lower = 0, upper = 10))
  ps = ParamSet$new(params = params, type = "list", check = function(x) return(TRUE))
  expect_error(ps$sample(), "not implemented")
  ps$toString()
})
