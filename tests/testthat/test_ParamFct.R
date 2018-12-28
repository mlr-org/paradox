context("ParamFct")

test_that("test if ParamFct constructor works", {
  p = ParamFct$new(id = "test", values = c("a", "b"))
  expect_equal(p$values, c("a", "b"))
  expect_equal(p$nlevels, 2L)

  # we dont allow NAs as values
  expect_error(ParamFct$new(id = "test", values = c("a", NA)))
})

test_that("map_unitint_to_values", {
  n = 100000L
  testit = function(vals) {
    p = ParamFct$new("x", values = vals)
    k = p$nlevels
    u = runif(n)
    v1 = p$map_unitint_to_values(u)
    expect_setequal(unique(v1), p$values) # check we see all levels
    # check that empirical frequencies are pretty much uniform
    freqs = prop.table(table(v1))
    p = rep(1/k, k)
    expect_lte(max(abs(freqs - p)), 0.01)
  }
  testit(c("f", "g"))
  testit(letters[2:9])
})


