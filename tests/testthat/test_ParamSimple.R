context("ParamSimple")

test_that("test if ParamSimple constructor works", {
  pint = ParamSimple$new(id = "test", type = "character", check = checkString, default = "keks", special.vals = NULL)
  expect_error(pint$sample(), "not implemented")
})


