context("ParamNode")


test_that("test if ParamNode constructor works", {
  pn = ParamNode$new(id = "test", type = "list", check = checkList)
  pn$toString()
  pn$sample()
  ParamNode$makeParam()
  })



