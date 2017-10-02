context("ParamNode")


test_that("test if ParamNode constructor works", {
  pn = ParamNode$new()
  pn$toString()
  pn$sample()
  ParamNode$makeParam()
  })



