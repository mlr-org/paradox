
context("ContextPV")

test_that("ContextPV construction", {
  y = 2
  fpv = ContextPV(function(x) x * y, y)
  expect_equal(fpv(10), 20)
  expect_output(print(fpv), "ContextPV function.*x \\* y.*\\$y.*2")
})

test_that("ContextPV applied in get_values()", {
  p = ParamSet$new(list(ParamInt$new("x")))
  y = 2
  expect_error({p$values$x = ContextPV(function(x) x * y, y)}, "Argument names of ContextPV Must be a subset of set \\{\\}")

  p$context_available = c("a", "x")

  p$values$x = ContextPV(function(x) x * y, y)
  expect_output(print(p$values$x), "x \\* y.*\\$y.*2")

  expect_error(p$get_values(), "context.*Must include the elements \\{a,x\\}")

  expect_equal(p$get_values(context = list(x = 10, a = 20)), list(x = 20))

  x = 30
  a = 20
  expect_equal(p$get_values(), list(x = 60))  # default context is parent.frame

})

test_that("ContextPV checks range", {
  p = ParamSet$new(list(ParamInt$new("x", lower = 0, upper = 30)))
  p$context_available = "x"

  y = 2
  p$values$x = ContextPV(function(x) x * y, y)

  expect_equal(p$get_values(context = list(x = 10)), list(x = 20))
  expect_error(p$get_values(context = list(x = 20)), " x resulted in infeasible value.*is not <= 30")

  expect_error(p$get_values(context = list(x = 10.25)), " x resulted in infeasible value.*not 'double'")

})

test_that("ContextPV convert", {

  p = ParamSet$new(list(ParamDbl$new("x", lower = 0, upper = 30)))
  p$context_available = "x"

  y = 2
  p$values$x = ContextPV(function(x) x * y, y)

  expect_equal(p$get_values(context = list(x = 10)), list(x = 20))
  # convert to within range
  expect_equal(p$get_values(context = list(x = 15.0000000001)), list(x = 30), tolerance = 1e-100)
})

test_that("ContextPV may not be in variable with dependency", {
  p = ParamSet$new(list(
    ParamInt$new("x"),
    ParamInt$new("y")
  ))
  p$add_dep("x", "y", CondEqual$new(0))
  y = 2
  expect_error({p$values = list(x = 1, y = 1)}, "can only be set if the following.*y = 0")

  expect_error({p$values = list(x = 1, y = 0)}, NA)
  expect_error({p$values = list(y = 1)}, NA)

  fpv = ContextPV(function(x) x * y, y)
  p$context_available = "x"
  expect_error({p$values = list(y = fpv)}, NA)

  expect_error({p$values = list(x = 1, y = fpv)}, "y is a ContextPV")
})

test_that("ContextPV in Tuning PS", {
  p = ParamSet$new(list(
    ParamInt$new("x"),
    ParamInt$new("y")
  ))
  p$context_available = "scale"
  p2 = p$search_space(list(
    x = to_tune(0, 10),
    y = to_tune(p_dbl(0, 1,
      trafo = function(x) ContextPV(function(scale) scale * x, x)))
  ))

  paramval = generate_design_grid(p2, 3)$transpose()[[5]]  # 5, 0.5
  p$values = paramval
  expect_equal(p$get_values(context = list(scale = 100)), list(x = 5, y = 50))
  expect_equal(p$get_values(context = list(scale = 10)), list(x = 5, y = 5))
  expect_error(p$get_values(context = list(scale = 1)), "infeasible value.*not 'double'")

})


test_that("ContextPV with PSC", {
  p = ps(x = p_dbl())
  p$context_available = "x"
  p$set_id = "n"
  psc = ParamSetCollection$new(list(p))
  expect_error({psc$values$n.x = ContextPV(function(y) x * 2)}, "Must be a subset of set \\{x\\}")
  psc$values$n.x = ContextPV(function(x) x * 2)

  expect_equal(p$get_values(context = list(x = 10)), list(x = 20))

})
