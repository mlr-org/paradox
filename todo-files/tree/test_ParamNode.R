context("ParamNode")


test_that("test if ParamNode constructor works", {
  pn = ParamNode$new(id = "test", storage.type = "list", check = checkList, tags = NULL)
  pn$toString()
  expect_error(pn$sample(), "not implemented")
  ParamNode$makeParam()
  })



