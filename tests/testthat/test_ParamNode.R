context("ParamNode")


test_that("test if ParamNode constructor works", {
  pn = ParamNode$new(id = "test", type = "list", check = checkList, tags = character())
  pn$toString()
  expect_error(pn$sample(), "not implemented")
  ParamNode$makeParam()
  })



