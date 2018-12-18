context("ParamSet Conditions")

test_that("basic example works", {
  ps = th_paramset_full()
  ps$add_dependency(
    Dependency$new(
      node_id = "th_param_int",
      parent_id = "th_param_fct",
      condition = cond_equal("a"))
  )
  x1 = list(th_param_int = 1) # should fail
  expect_character(ps$check(x1), fixed = "condition")
  x2 = list(th_param_int = 1, th_param_fct = "a") # should pass
  expect_true(ps$check(x2))
  x3 = list(th_param_int = 1, th_param_fct = "b") # should fail
  expect_character(ps$check(x3), fixed = "condition")
  x4 = list(th_param_fct = "a") # pass
  expect_true(ps$check(x4))
  x5 = list(th_param_fct = "b") # pass
  expect_true(ps$check(x5))
  x6 = list(th_param_dbl = 1.3) # pass
  expect_true(ps$check(x6))

})
