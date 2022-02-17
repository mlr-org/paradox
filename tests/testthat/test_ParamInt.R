context("ParamInt")

test_that("constructor works", {
  p = ParamInt$new(id = "test", lower = 1L, upper = 10L)
  expect_equal(p$id, "test")
  expect_equal(p$lower, 1L)
  expect_equal(p$upper, 10L)
  expect_equal(p$nlevels, 10L)

  # check that we can create param with Inf bounds
  p = ParamInt$new(id = "test", lower = 1L)
  expect_equal(p$lower, 1L)
  expect_equal(p$upper, Inf)

  # check some invalid arg settings
  expect_error(ParamInt$new(id = "x", lower = NULL), "not 'NULL'")
  expect_error(ParamInt$new(id = "x", lower = 1.5), "not 'double'")
  expect_error(ParamInt$new(id = "x", upper = NULL), "not 'NULL'")
  expect_error(ParamInt$new(id = "x", upper = 1.5), "not 'double'")
  expect_error(ParamInt$new(id = "x", lower = 1, upper = 0), "lower <= upper")
  expect_error(ParamInt$new(id = "x", lower = Inf, upper = 0), "lower <= upper")
})

test_that("is_bounded works", {
  expect_true(ParamInt$new(id = "x", lower = 1, upper = 10)$is_bounded)
  expect_false(ParamInt$new(id = "x", lower = 1)$is_bounded)
  expect_false(ParamInt$new(id = "x")$is_bounded)
})

test_that("qunif", {
  n = 100000L
  testit = function(a, b) {

    p = ParamInt$new("x", lower = a, upper = b)
    k = p$nlevels
    expect_equal(k, b - a + 1)
    u = runif(n)
    v1 = p$qunif(u)
    expect_integer(v1, any.missing = FALSE, len = n)
    expect_setequal(unique(v1), a:b) # check we see all levels
    # check that empirical frequencies are pretty much uniform
    freqs = prop.table(table(v1))
    p = rep(1 / k, k)
    expect_lte(max(abs(freqs - p)), 0.01)
  }
  testit(1, 12)
  testit(-2, 1)
})

test_that("assigning integer value results in int", {

  p = ParamSet$new(list(ParamInt$new("x")))
  p$values$x = 0
  expect_equal(typeof(p$values$x), "integer")
  expect_error({p$values$x = 1e-10}, "be of type.*integerish")

})
