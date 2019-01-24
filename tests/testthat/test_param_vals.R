context("param_vals")

test_that("param_vals", {
  ps = ParamSet$new(list(
    ParamDbl$new(id = "d", lower = 0, upper = 1),
    ParamInt$new(id = "i", lower = 1, upper = 3),
    ParamFct$new(id = "f", values = letters[1:3])
  ))
  ps$param_vals = list(d = 1, f = "a")
  expect_true(ps$check(list(d = 0, f = "a")))
  ps2 = ps$clone()
  ps2$subset(ids = c("d", "i"))
  expect_equal(ps2$param_vals, list(d = 1))
  ps2$param_vals = list(d = 0.5)
  expect_true(ps$check(list(d = 1, f = "a")))
  expect_equal(ps2$param_vals, list(d = 0.5))
})


