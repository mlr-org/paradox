context("Design")

test_that("transpose works", {
  ps = ParamSet$new(list(
    ParamFct$new("f", values = c("a", "b")),
    ParamInt$new("i", lower = 1, upper = 5)
  ))
  ps$add_dep("i", on = "f", CondEqual$new("a"))
  data = data.table(f = c("a", "b"), i = c(1L, NA))
  d = Design$new(ps, data)
  xs = d$transpose(filter_na = FALSE)
  expect_equal(xs, list(list(f = "a", i = 1L), list(f = "b", i = NA_integer_)))
  xs = d$transpose(filter_na = TRUE)
  expect_equal(xs, list(list(f = "a", i = 1L), list(f = "b")))

  # now a trafo, with a dep
  ps$trafo = function(x, param_set) {
    if (!is.null(x$i))
      x$i = x$i + 10
    return(x)
  }
  xs = d$transpose(trafo = TRUE, filter_na = FALSE)
  expect_equal(xs, list(list(f = "a", i = 11L), list(f = "b", i = NA_integer_)))
  xs = d$transpose(trafo = TRUE, filter_na = TRUE)
  expect_equal(xs, list(list(f = "a", i = 11L), list(f = "b")))
})

