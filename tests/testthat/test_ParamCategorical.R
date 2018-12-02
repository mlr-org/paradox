context("ParamCateg")

test_that("test if ParamCateg constructor works", {
  p = ParamCateg$new(id = "test", values = c("a", "b"))
  expect_equal(p$values, c("a", "b"))
})
