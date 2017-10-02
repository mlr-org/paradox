context("ParamInt")

test_that("test if ParamInt constructor works", {
  pint = ParamInt$new(id = "test", lower = 1L, upper = 10L)
  print(pint$sample())
})


