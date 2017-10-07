context("ParamInt")

test_that("test if ParamInt constructor works", {
  pint = ParamInt$new(id = "test", lower = 1L, upper = 10L)
  expect_data_table(pint$sample())
})


