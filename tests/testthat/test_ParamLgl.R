context("ParamLgl")

test_that("constructor works", {
  p = ParamLgl$new(id = "test")
  expect_equal(p$ids(), "test")
  expect_equal(p$nlevels[["test"]], 2L)
  expect_equal(p$levels[["test"]], c(TRUE, FALSE))
})

test_that("qunif", {
  n = 100000L
  testit = function() {
    p = ParamLgl$new("x")
    u = runif(n)
    v1 = p$qunif(data.frame(x = u))$x
    expect_logical(v1, any.missing = FALSE, len = n)
    expect_setequal(unique(v1), p$levels[["x"]]) # check we see all levels
    # check that empirical frequencies are pretty much uniform
    freqs = prop.table(table(v1))
    p = c(1 / 2, 1 / 2)
    expect_lte(max(abs(freqs - p)), 0.01)
  }
  testit()
})
