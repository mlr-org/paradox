context("ParamNode")


test_that("test if ParamNode constructor works", {
  pn = ParamNode$new(id = "test", storage.type = "list", check = checkList, tags = NULL)
  expect_error(pn$sample(), "not implemented")
  ParamNode$makeParam()
  })



