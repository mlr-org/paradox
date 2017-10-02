context("ParamSet")

test_that("test if ParamSet constructor works", {
  ps = ParamSet$new()
  ps$sample()
  ps$toString()
})
