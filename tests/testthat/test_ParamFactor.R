context("ParamFactor")

test_that("test if ParamFactor constructor works", {
  pint = ParamFactor$new(id = "test", values = c("a", "b"))
  print(pint$sample())
})


