context("ParamSimple")

test_that("test if ParamSimple constructor works", {
  pint = ParamSimple$new(id = "test", type = "character", check = checkString)
  pint$sample()
})


