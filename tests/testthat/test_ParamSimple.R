context("ParamSimple")

test_that("test if ParamSimple constructor works", {
  pint = ParamSimple$new(id = "test", type = "character", check = checkString, default = "keks", special.vals = NULL, tags = NULL)
  expect_error(pint$sample(), "not implemented")

  pint = ParamSimple$new(id = "test", type = "matrix", check = function(x, na.ok = FALSE, null.ok = TRUE) {checkMatrix(x, null.ok = null.ok)}, default = NULL, special.vals = NULL, tags = NULL)
})


