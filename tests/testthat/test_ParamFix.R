context("ParamLFix")

test_that("constructor works", {
  p = ParamFix$new(id = "test", default = 3L, storage_type = "integer")
  expect_equal(p$id, "test")
  expect_equal(p$values, 3L)
})


