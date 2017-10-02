context("ParamFactor")

test_that("test if ParamFactor constructor works", {
  pint = ParamFactor$new(id = "test", values = list(a = 1))
  print(pint$sample())
})


