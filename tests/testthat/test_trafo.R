context("trafo")

test_that("trafo", {
  ps = ParamSet_legacy$new(list(
    ParamDbl$new("x", lower = -3, upper = 3),
    ParamDbl$new("w1", lower = 7, upper = 9),
    ParamDbl$new("w2", lower = 7, upper = 9),
    ParamFct$new("f", levels = c("a", "b"))
  ))
  expect_false(ps$has_trafo)
  ps$extra_trafo = function(x, param_set) {
    x$x = 2^x$x
    s = x$w1 + x$w2
    x$w1 = x$w1 / s
    x$w2 = x$w2 / s
    return(x)
  }
  expect_true(ps$has_trafo)
  expect_output(print(ps), "Trafo is set")
  d1 = generate_design_grid(ps, resolution = 4)
  dd1 = d1$data
  d2 = map_dtr(seq_len(nrow(dd1)), function(i) ps$trafo(as.list(dd1[i])))
  expect_numeric(d2$x, lower = 0)
  expect_numeric(d2$w1, lower = 0, upper = 1)
  expect_numeric(d2$w2, lower = 0, upper = 1)
  expect_equal(d2$w1 + d2$w2, rep(1, nrow(d2)))
})
