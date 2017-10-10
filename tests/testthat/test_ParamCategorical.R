context("ParamCategorical")

test_that("test if ParamCategorical constructor works", {
  pint = ParamCategorical$new(id = "test", values = c("a", "b"))
  expect_data_table(pint$sample())
})
