context("ParamUty")

test_that("ParamUty", {
  p = ParamUty$new(id = "x")
  expect_true(p$check(list(x = FALSE)))
  expect_true(p$check(list(x = NULL)))
  expect_true(p$check(list(x = NA)))

  p = ParamUty$new(id = "x", custom_check = function(x)
    if (is.null(x)) "foo" else TRUE)
  expect_true(p$check(list(x = FALSE)))
  expect_string(p$check(list(x = NULL)), fixed = "foo")
  expect_true(p$check(list(x = NA)))

  p = ParamUty$new(id = "x", default = Inf)
})

test_that("R6 values of ParamUty are cloned", {
  ps = ParamSet_legacy$new(list(ParamUty$new("x")))
  ps$values$x = R6Class("testclass", public = list(x = NULL))$new()

  psclone = ps$clone(deep = TRUE)
  psunclone = ps$clone(deep = FALSE)

  ps$values$x$x = TRUE

  expect_true(ps$values$x$x)  # was changed to TRUE
  expect_true(psunclone$values$x$x)  # reference check: value was not cloned
  expect_null(psclone$values$x$x)  # was cloned before change --> should still be null
})

test_that("default NULL works", {
  expect_equal(p_uty(default = NULL)$cargo[[1]]$repr, "NULL")
})
