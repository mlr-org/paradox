context("ParamLgl")

test_that("constructor works", {
  p = ParamLgl$new(id = "test")
  expect_equal(p$id, "test")
  expect_equal(p$nlevels, 2L)
})

test_that("map_unitint_to_values", {
  n = 100000L
  testit = function() {
    p = ParamLgl$new("x")
    u = runif(n)
    v1 = p$map_unitint_to_values(u)
    expect_setequal(unique(v1), p$values) # check we see all levels
    # check that empirical frequencies are pretty much uniform
    freqs = prop.table(table(v1))
    p = c(1/2, 1/2)
    expect_lte(max(abs(freqs - p)), 0.01)
  }
  testit()
})



