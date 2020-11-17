
context("FunctionParamValue")

test_that("FunctionParamValue construction", {
  y = 2
  fpv = FunctionParamValue(function(x) x * y, y)
  expect_equal(fpv(10), 20)
  expect_output(print(fpv), "x \\* y.*\\$y.*2")
})

test_that("FunctionParamValue applied in get_values()", {
  p = ParamSet$new(list(ParamInt$new("x")))
  y = 2
  p$values$x = FunctionParamValue(function(x) x * y, y)
  expect_output(print(p$values$x), "x \\* y.*\\$y.*2")

  expect_equal(p$get_values(env = 10), list(x = 20))
  expect_equal(p$get_values(env = 30), list(x = 60))

})

test_that("FunctionParamValue checks range", {
  p = ParamSet$new(list(ParamInt$new("x", lower = 0, upper = 30)))
  y = 2
  p$values$x = FunctionParamValue(function(x) x * y, y)

  expect_equal(p$get_values(env = 10), list(x = 20))
  expect_error(p$get_values(env = 20), " x resulted in infeasible value.*is not <= 30")

  expect_error(p$get_values(env = 10.25), " x resulted in infeasible value.*not 'double'")

})

test_that("FunctionParamValue may not be in variable with dependency", {
  p = ParamSet$new(list(
    ParamInt$new("x"),
    ParamInt$new("y")
  ))
  p$add_dep("x", "y", CondEqual$new(0))

  expect_error({p$values = list(x = 1, y = 1)}, "can only be set if the following.*y = 0")

  expect_error({p$values = list(x = 1, y = 0)}, NA)
  expect_error({p$values = list(y = 1)}, NA)

  fpv = FunctionParamValue(function(x) x * y, y)

  expect_error({p$values = list(y = fpv)}, NA)

  expect_error({p$values = list(x = 1, y = fpv)}, "y is a FunctionParamValue")
})

test_that("FunctionParamValue in Tuning PS", {
  p = ParamSet$new(list(
    ParamInt$new("x"),
    ParamInt$new("y")
  ))

  p2 = p$tune_ps(list(
    x = to_tune(0, 10),
    y = to_tune(p_dbl(0, 1,
      trafo = function(x) FunctionParamValue(function(env) env$scale * x, x)))
  ))

  paramval = generate_design_grid(p2, 3)$transpose()[[5]]  # 5, 0.5
  p$values = paramval
  expect_equal(p$get_values(env = list(scale = 100)), list(x = 5, y = 50))
  expect_equal(p$get_values(env = list(scale = 10)), list(x = 5, y = 5))
  expect_error(p$get_values(env = list(scale = 1)), "infeasible value.*not 'double'")

})
