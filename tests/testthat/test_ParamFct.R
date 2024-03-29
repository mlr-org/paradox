context("ParamFct")

test_that("test if ParamFct constructor works", {
  p = ParamFct$new(id = "test", levels = c("a", "b"))
  expect_equal(p$levels$test, c("a", "b"))
  expect_equal(p$nlevels[["test"]], 2L)

  # we dont allow NAs as levels
  expect_error(ParamFct$new(id = "test", levels = c("a", NA)))
})

test_that("qunif", {
  n = 100000L
  testit = function(vals) {
    p = ParamFct$new("x", levels = vals)
    k = p$nlevels
    u = runif(n)
    v1 = p$qunif(data.frame(x = u))$x
    expect_character(v1, any.missing = FALSE, len = n)
    expect_setequal(unique(v1), p$levels$x) # check we see all levels
    # check that empirical frequencies are pretty much uniform
    freqs = prop.table(table(v1))
    p = rep(1 / k, k)
    expect_lte(max(abs(freqs - p)), 0.01)
  }
  testit(c("f", "g"))
  testit(letters[2:9])
})
