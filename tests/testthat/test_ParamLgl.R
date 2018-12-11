context("ParamLgl")

test_that("constructor works", {
  p = ParamLgl$new(id = "test")
  expect_equal(p$id, "test")
  expect_equal(p$nlevels, 2L)
})


