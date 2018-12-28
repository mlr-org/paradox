context("ParamSet")

test_that("simple active bindings work", {
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_untyped(),
    th_paramset_numeric()
  )
  for (ps in ps_list) {
    info = ps$id
    if (ps$id == "th_paramset_full") {
      expect_equal(ps$ids, c('th_param_int', 'th_param_dbl', 'th_param_fct', 'th_param_lgl'))
      expect_equal(ps$lowers, c(th_param_int=-10, th_param_dbl=-10, th_param_fct=NA_real_, th_param_lgl=NA_real_))
      expect_equal(ps$uppers, c(th_param_int=10, th_param_dbl=10, th_param_fct=NA_real_, th_param_lgl=NA_real_))
    }
    expect_class(ps, "ParamSet", info = info)
    expect_int(ps$length, lower = 0L, info = info)
    expect_character(ps$ids, info = info)
    expect_character(ps$pclasses, info = info)
    expect_names(names(ps$pclasses), identical.to = ps$ids, info = info)
    expect_character(ps$storage_types, info = info)
    expect_names(names(ps$storage_types), identical.to = ps$ids, info = info)
    expect_numeric(ps$lowers, any.missing = TRUE, info = info)
    expect_names(names(ps$lowers), identical.to = ps$ids, info = info)
    expect_numeric(ps$uppers, any.missing = TRUE, info = info)
    expect_names(names(ps$uppers), identical.to = ps$ids, info = info)
    expect_list(ps$values, info = info)
    expect_names(names(ps$values), identical.to = ps$ids, info = info)
    expect_flag(ps$is_bounded, info = info)
    expect_numeric(ps$nlevels, any.missing = FALSE, lower = 1, info = info)
    expect_list(ps$tags, names = "strict", any.missing = TRUE, info = info)
    expect_list(ps$defaults, names = "strict", any.missing = TRUE, info = info)
  }
})

test_that("ParamSet$subset", {
  ids = th_paramset_full()$ids
  getps = function() th_paramset_full()$clone(deep = TRUE) # give us a fresh clone of the fullset
  # we can subset to an empty set
  ps = getps()
  ps$subset(character(0L))
  expect_true(ps$is_empty)
  ps = getps()
  # subsetting to 2 params make the set smaller
  ps$subset(ids[2:3])
  expect_equal(ps$ids, ids[2:3])
  expect_equal(ps$length, 2)
  # subsetting to all ids does not change anything
  ps = getps()
  ps$subset(ids)
  expect_equal(as.data.table(ps), as.data.table(getps()))
  # subset full set to 2 numeric params
  ps = getps()
  ps$subset(c("th_param_int", "th_param_dbl"))
  expect_equal(as.data.table(ps), as.data.table(th_paramset_numeric()))
})

test_that("ParamSet$add_param_set", {
  # adding with the empty set
  ps1 = th_paramset_numeric()$clone(deep = TRUE)
  n1 = ps1$length
  ps2 = th_paramset_empty()$clone(deep = TRUE)
  ps1$add(ps2)
  expect_equal(ps1$length, n1)
  ps2$add(ps1)
  expect_equal(ps2$length, n1)

  # adding 2 sets, full and numeric, results in a clash
  ps1 = th_paramset_numeric()$clone(deep = TRUE)
  ps2 = th_paramset_full()$clone(deep = TRUE)
  expect_error(ps1$add(ps2), "Name clash")

  # adding 2 sets, numeric and untyped, makes them larger
  ps1 = th_paramset_numeric()$clone(deep = TRUE)
  ps2 = th_paramset_untyped()$clone(deep = TRUE)
  ps1$add(ps2)
  expect_equal(ps2$length, 1L)
  expect_equal(ps1$ids, c("th_param_int", "th_param_dbl", "th_param_uty"))
  ps1 = th_paramset_numeric()$clone(deep = TRUE)
  ps2 = th_paramset_untyped()$clone(deep = TRUE)
  ps2$add(ps1)
  expect_equal(ps2$ids, c("th_param_uty", "th_param_int", "th_param_dbl"))
  expect_equal(ps1$length, 2L)
})

test_that("empty paramset", {
  ps = ParamSet$new()
  expect_r6(ps, "ParamSet")
  expect_equal(ps$length, 0)
  expect_equal(ps$ids, character(0L))
  expect_equal(ps$lowers, set_names(numeric(0L), character(0L)))
})


test_that("ParamSet$check", {
  ps = th_paramset_numeric()
  expect_true(ps$check(list()))
  expect_true(ps$check(list(th_param_int = 5, th_param_dbl = 5)))
  expect_true(ps$check(list(th_param_dbl = 5, th_param_int = 5)))
  expect_character(ps$check(list(th_param_dbl = 5, new_param = 5)), fixed = "subset of")
  expect_match(ps$check(list(th_param_dbl = 5, th_param_int = 15)), "not <= 10")
  expect_true(ps$check(list(th_param_dbl = 5)))
  expect_true(ps$check(list(th_param_int = 5)))
})

test_that("we cannot create ParamSet with non-strict R names", {
  expect_error(ParamSet$new(id = "$foo") , "naming convention")
})

test_that("ParamSet$print", {
  ps = th_paramset_empty()
  expect_output(print(ps), "ParamSet: th_paramset_empty")
  expect_output(print(ps), "Empty")
  ps = th_paramset_numeric()
  expect_output(print(ps), "ParamSet:")
  s = capture_output(print(ps))
  expect_true(stri_detect_fixed(s, "ParamInt"))
  expect_true(stri_detect_fixed(s, "ParamDbl"))
  s = capture_output(print(ps, hide.cols = c("pclass")))
  expect_false(stri_detect_fixed(s, "ParamInt"))

  # iterate through more complex PS and check that printer works by at least calling it
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_untyped(),
    th_paramset_trafo()
  )
  for (ps in ps_list) {
    expect_output(print(ps), "ParamSet:")
  }
})

test_that("ParamSet does a deep copy of params on construction", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps = ParamSet$new(list(p))
  p$lower = 2
  expect_equal(p$lower, 2)
  expect_equal(ps$lowers, c(x = 1))
  expect_equal(ps$params[["x"]]$lower, 1)
})

test_that("ParamSet does a deep copy of param on add", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps = ParamSet$new(list())$add(p)
  p$lower = 2
  expect_equal(p$lower, 2)
  expect_equal(ps$lowers, c(x = 1))
  expect_equal(ps$params[["x"]]$lower, 1)
})

test_that("ParamSet$clone can be deep", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps1 = ParamSet$new(list(p))
  ps2 = ps1$clone(deep = TRUE)
  ps2$params[[1]]$id = "foo"
  expect_equal(ps2$ids, "foo")
  expect_equal(ps1$ids, "x")
})

test_that("ParamSet$is_bounded", {
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3)
  ))
  expect_true(ps$is_bounded)
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3),
    ParamLgl$new("y")
  ))
  expect_true(ps$is_bounded)
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1),
    ParamLgl$new("y")
  ))
  expect_false(ps$is_bounded)
})

test_that("ParamSet$add_param", {
  ps = ParamSet$new(list())
  ps$add(ParamDbl$new("x", lower = 1))
  expect_equal(ps$length, 1L)
  expect_equal(ps$ids, "x")
  expect_equal(ps$lowers, c(x = 1))
  ps$add(ParamFct$new("y", values = c("a")))
  expect_equal(ps$length, 2L)
  expect_equal(ps$ids, c("x", "y"))
  expect_equal(ps$lowers, c(x = 1, y = NA))
})

test_that("as.data.table", {
  d = as.data.table(th_paramset_empty())
  expect_data_table(d, nrow = 0)
  ps = th_paramset_full()
  d = as.data.table(ps)
  expect_data_table(d, nrow = 4, ncol = 11)
  expect_equal(ps$ids, d$id)
  expect_equal(unname(ps$lower), d$lowers)
  expect_equal(unname(ps$values), d$values)
})

test_that("ParamSet$defaults", {
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3, default = 2),
    ParamInt$new("y", lower = 1, upper = 3)
  ))
  expect_equal(ps$defaults, list(x = 2))
  expect_error(ParamDbl$new("x", lower = 1, upper = 3, default = 4))
  expect_error(ParamDbl$new("x", lower = 1, upper = 3, default = NULL))
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3, special_vals = list(NULL), default = NULL),
    ParamInt$new("y", lower = 1, upper = 3)
  ))
  expect_equal(ps$defaults, list(x = NULL))
})




