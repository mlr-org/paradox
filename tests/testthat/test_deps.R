context("Dependencies")

test_that("basic example works", {
  ps = th_paramset_full()
  expect_false(ps$has_deps)
  ps$add_dep("th_param_int", on = "th_param_fct", CondEqual$new("a"))
  expect_true(ps$has_deps)
  x = list(th_param_int = 1)
  expect_string(ps$check(x, check_strict = TRUE), fixed = "The parameter 'th_param_int' can only be set")
  x = list(th_param_int = 1, th_param_fct = "a")
  expect_true(ps$check(x, check_strict = TRUE))
  x = list(th_param_int = 1, th_param_fct = "b")
  expect_string(ps$check(x, check_strict = TRUE), fixed = "The parameter 'th_param_int' can only be set")
  x = list(th_param_int = NA, th_param_fct = "b")
  expect_string(ps$check(x, check_strict = TRUE), fixed = "May not be NA")
  x = list(th_param_fct = "a")
  expect_true(ps$check(x, check_strict = TRUE))
  x = list(th_param_fct = "b")
  expect_true(ps$check(x, check_strict = TRUE))
  x = list(th_param_dbl = 1.3)
  expect_true(ps$check(x, check_strict = TRUE))

  # test printer, with 2 deps
  ps = th_paramset_full()
  ps$add_dep("th_param_int", on = "th_param_fct", CondEqual$new("a"))
  ps$add_dep("th_param_int", on = "th_param_lgl", CondEqual$new(TRUE))
  expect_output(print(ps), "th_param_fct,th_param_lgl")

  # test that we can remove deps
  ps$deps = ps$deps[-1, ]
  expect_true(ps$has_deps)
  ps$deps = ps$deps[-1, ]
  expect_false(ps$has_deps)
})

test_that("nested deps work", {
  ps = th_paramset_full()
  ps$add_dep("th_param_int", on = "th_param_fct", CondAnyOf$new(c("a", "b")))
  ps$add_dep("th_param_dbl", on = "th_param_lgl", CondEqual$new(TRUE))
  ps$add_dep("th_param_lgl", on = "th_param_fct", CondEqual$new("c"))

  x1 = list(th_param_int = 1)
  expect_string(ps$check(x1, check_strict = TRUE), fixed = "The parameter 'th_param_int' can only be set")
  x2 = list(th_param_int = 1, th_param_fct = "b")
  expect_true(ps$check(x2, check_strict = TRUE))
  x3 = list(th_param_int = 1, th_param_fct = "c")
  expect_string(ps$check(x3, check_strict = TRUE), fixed = "The parameter 'th_param_int' can only be set")
  x4 = list(th_param_fct = "a")
  expect_true(ps$check(x4, check_strict = TRUE))
  x5 = list(th_param_dbl = 1.3)
  expect_string(ps$check(x5, check_strict = TRUE), fixed = "The parameter 'th_param_dbl' can only be set")
  x6 = list(th_param_fct = "c", th_param_lgl = TRUE, th_param_dbl = 3)
  expect_true(ps$check(x6, check_strict = TRUE))
})


test_that("adding 2 sets with deps works", {
  ps1 = ParamSet$new(list(
    ParamFct$new("x1", levels = c("a", "b")),
    ParamDbl$new("y1")
  ))
  ps1$add_dep("y1", on = "x1", CondEqual$new("a"))

  ps2 = ParamSet$new(list(
    ParamFct$new("x2", levels = c("a", "b")),
    ParamDbl$new("y2")
  ))
  ps2$add_dep("y2", on = "x2", CondEqual$new("a"))

  ps1$add(ps2)
  expect_equal(ps1$length, 4L)
  expect_true(ps1$has_deps)
  expect_data_table(ps1$deps, nrows = 2)
  # do a few feasibility checks on larger set
  expect_true(ps1$test(list(x1 = "a", y1 = 1, x2 = "a", y2 = 1), check_strict = TRUE))
  expect_true(ps1$test(list(x1 = "a", y1 = 1), check_strict = TRUE))
  expect_false(ps1$test(list(x1 = "b", y1 = 1), check_strict = TRUE))
  expect_true(ps1$test(list(x2 = "a", y2 = 1), check_strict = TRUE))
  expect_false(ps1$test(list(x2 = "b", y2 = 1), check_strict = TRUE))
})

test_that("subsetting with deps works", {
  ps = ParamSet$new(list(
    ParamFct$new("a", levels = c("a", "b")),
    ParamFct$new("b", levels = c("a", "b")),
    ParamFct$new("c", levels = c("a", "b")),
    ParamFct$new("d", levels = c("a", "b"))
  ))
  ps$add_dep("a", on = "b", CondEqual$new("a"))
  ps$add_dep("a", on = "c", CondEqual$new("a"))
  ps$add_dep("b", on = "c", CondEqual$new("a"))

  ps$clone(deep = TRUE)$subset("d")
  ps$clone(deep = TRUE)$subset(c("a", "b", "c"))
  expect_error(ps$clone(deep = TRUE)$subset(c("a", "c")), "Subsetting so that dependencies")
  expect_error(ps$clone(deep = TRUE)$subset(c("a")), "Subsetting so that dependencies")
})

test_that("cannot add a dep on yourself", {
  ps = ParamSet$new(list(ParamFct$new("x", levels = c("a"))))
  expect_error(ps$add_dep("x", on = "x", CondEqual$new("a")), "depend on itself")
})


test_that("we can also dep on integer", {
  ps = ParamSet$new(list(
    ParamInt$new("i", lower = 0, upper = 9),
    ParamDbl$new("d", lower = 0, upper = 9)
  ))
  ps$add_dep("d", on = "i", CondAnyOf$new(1:3))

  expect_true(ps$check(list(i = 2, d = 5), check_strict = TRUE))
  expect_string(ps$check(list(i = 5, d = 5), check_strict = TRUE))
})

test_that("deps make sense", {
  ps = th_paramset_full()
  expect_error(ps$add_dep("th_param_lgl", "th_param_fct", CondEqual$new("d")),
    "Condition has infeasible values for th_param_fct")
  expect_error(ps$add_dep("th_param_lgl", "th_param_int", CondAnyOf$new(5:15)),
    "Condition has infeasible values for th_param_int")
})
