context("ParamReal")

test_that("test if ParamReal constructor works", {
  pint = ParamReal$new(id = "test", lower = 1L, upper = 10L)
  expect_data_table(pint$sample())
})


