context("ParamSetFlat")

test_that("test if ParamSetFlat constructor works", {
  ps = ParamSetFlat$new(id = "flatParamSetEx")
  ps$sample()
  ps$toString()
})

