context("ParamCategorical")

test_that("test if ParamCategorical constructor works", {
  p = ParamCategorical$new(id = "test", values = c("a", "b"))
  expect_data_table(p$sample())
})
