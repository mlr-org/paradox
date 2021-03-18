context("ParamSet")

test_that("simple active bindings work", {
  ps_list = list(
    th_paramset_dbl1(),
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_untyped(),
    th_paramset_numeric()
  )
  for (ps in ps_list) {
    info = ps$set_id
    expect_class(ps, "ParamSet", info = info)
    expect_int(ps$length, lower = 0L, info = info)
    expect_character(ps$ids(), info = info)
    expect_character(ps$class, info = info)
    expect_names(names(ps$class), identical.to = ps$ids(), info = info)
    expect_character(ps$storage_type, info = info)
    expect_names(names(ps$storage_type), identical.to = ps$ids(), info = info)
    expect_numeric(ps$lower, any.missing = TRUE, info = info)
    expect_names(names(ps$lower), identical.to = ps$ids(), info = info)
    expect_numeric(ps$upper, any.missing = TRUE, info = info)
    expect_names(names(ps$upper), identical.to = ps$ids(), info = info)
    expect_list(ps$levels, info = info)
    expect_names(names(ps$levels), identical.to = ps$ids(), info = info)
    expect_flag(ps$is_bounded, info = info)
    expect_numeric(ps$nlevels, any.missing = FALSE, lower = 1, info = info)
    expect_list(ps$tags, types = "character", info = info)
    expect_names(names(ps$tags), identical.to = ps$ids(), info = info)
    expect_list(ps$default, names = "strict", info = info)
    expect_names(names(ps$default), subset.of = ps$ids(), info = info)
  }
  ps = th_paramset_full()
  expect_output(print(ps), "<ParamSet>")
  expect_equal(ps$ids(), c("th_param_int", "th_param_dbl", "th_param_fct", "th_param_lgl"))
  expect_equal(ps$lower, c(th_param_int = -10, th_param_dbl = -10, th_param_fct = NA_real_, th_param_lgl = NA_real_))
  expect_equal(ps$upper, c(th_param_int = 10, th_param_dbl = 10, th_param_fct = NA_real_, th_param_lgl = NA_real_))
})

test_that("ParamSet$subset", {
  ids = th_paramset_full()$ids()
  getps = function() th_paramset_full()$clone(deep = TRUE) # give us a fresh clone of the fullset
  # we can subset to an empty set
  ps = getps()
  ps$subset(character(0L))
  expect_true(ps$is_empty)
  ps = getps()
  # subsetting to 2 params make the set smaller
  ps$subset(ids[2:3])
  expect_equal(ps$ids(), ids[2:3])
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
  ps1 = ParamSet$new()
  n1 = ps1$length
  ps2 = ParamSet$new()
  ps1$add(ps2)
  expect_equal(ps1$length, n1)
  ps2$add(ps1)
  expect_equal(ps2$length, n1)

  # adding 2 sets, numeric and untyped, makes them larger
  ps1 = th_paramset_numeric()$clone(deep = TRUE)
  ps2 = th_paramset_untyped()$clone(deep = TRUE)
  ps1$add(ps2)
  expect_equal(ps2$length, 1L)
  expect_equal(ps1$ids(), c("th_param_int", "th_param_dbl", "th_param_uty"))
  ps1 = th_paramset_numeric()$clone(deep = TRUE)
  ps2 = th_paramset_untyped()$clone(deep = TRUE)
  ps2$add(ps1)
  expect_equal(ps2$ids(), c("th_param_uty", "th_param_int", "th_param_dbl"))
  expect_equal(ps1$length, 2L)
})

test_that("empty paramset", {
  ps = ParamSet$new()
  expect_r6(ps, "ParamSet")
  expect_equal(ps$length, 0)
  expect_equal(ps$ids(), character(0L))
  expect_equal(ps$lower, set_names(numeric(0L), character(0L)))
  expect_data_table(ps$deps, nrows = 0L, ncols = 3L)
})

test_that("ParamSet$check", {
  ps = th_paramset_numeric()
  expect_true(ps$check(list()))
  expect_true(ps$check(list(th_param_int = 5, th_param_dbl = 5)))
  expect_true(ps$check(list(th_param_dbl = 5, th_param_int = 5)))
  expect_character(ps$check(list(th_param_dbl = 5, new_param = 5)), fixed = "not available")
  expect_character(ps$check(list(th_param_dbl = 5, th_param_intx = 5)), fixed = "Did you mean")
  expect_match(ps$check(list(th_param_dbl = 5, th_param_int = 15)), "not <= 10")
  expect_true(ps$check(list(th_param_dbl = 5)))
  expect_true(ps$check(list(th_param_int = 5)))

  ps = ParamLgl$new("x")$rep(2)
  ps$add_dep("x_rep_1", "x_rep_2", CondEqual$new(TRUE))
  expect_string(ps$check(list(x_rep_1 = FALSE, x_rep_2 = FALSE), check_strict = TRUE), fixed = "x_rep_2 = TRUE")
})

test_that("we cannot create ParamSet with non-strict R names", {
  ps = ParamSet$new()
  expect_error(ps$set_id <- "$foo", "Must comply")
})

test_that("ParamSets cannot have duplicated ids", {
  p1 = ParamDbl$new("x1")
  p2 = ParamDbl$new("x1")
  expect_error(ParamSet$new(list(p1, p2)), "duplicated")
  ps = ParamSet$new(list(p1))
  expect_error(ps$add(p2), "duplicated")
  expect_error(ps$add(ParamSet$new(list(p2))), "duplicated")
})

test_that("ParamSet$print", {
  ps = ParamSet$new()
  ps$set_id = "foo"
  expect_output(print(ps), "<ParamSet:foo>")
  expect_output(print(ps), "Empty")
  ps = th_paramset_numeric()
  expect_output(print(ps), "<ParamSet>")
  s = capture_output(print(ps))
  expect_true(grepl("ParamInt", s, fixed = TRUE))
  expect_true(grepl("ParamDbl", s, fixed = TRUE))
  s = capture_output(print(ps, hide_cols = c("class")))
  expect_false(grepl("ParamInt", s, fixed = TRUE))

  # iterate through more complex PS and check that printer works by at least calling it
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_untyped()
  )
  for (ps in ps_list) {
    expect_output(print(ps), "<ParamSet>")
  }
})

test_that("ParamSet does a deep copy of params on construction", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps = ParamSet$new(list(p))
  p$lower = 2
  expect_equal(p$lower, 2)
  expect_equal(ps$lower, c(x = 1))
  expect_equal(ps$params[["x"]]$lower, 1)
})

test_that("ParamSet does a deep copy of param on add", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps = ParamSet$new(list())$add(p)
  p$lower = 2
  expect_equal(p$lower, 2)
  expect_equal(ps$lower, c(x = 1))
  expect_equal(ps$params[["x"]]$lower, 1)
})

test_that("ParamSet$clone can be deep", {
  p1 = ParamDbl$new("x", lower = 1, upper = 3)
  p2 = ParamFct$new("y", levels = c("a", "b"))
  ps1 = ParamSet$new(list(p1, p2))
  ps2 = ps1$clone(deep = TRUE)
  pp = ps2$params[["x"]]
  pp$lower = 9
  expect_equal(ps2$lower, c(x = 9, y = NA))
  expect_equal(ps1$lower, c(x = 1, y = NA))

  # now lets add a dep, see if that gets clones properly
  ps1$add_dep("x", on = "y", CondEqual$new("a"))
  ps2 = ps1$clone(deep = TRUE)
  d = ps2$deps$id[1] = "foo"
  expect_equal(ps2$deps$id[1], "foo")
  expect_equal(ps1$deps$id[1], "x")

  ps = ParamSet$new()
  expect_equal(ps, ps$clone(deep = TRUE))
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
  expect_equal(ps$ids(), "x")
  expect_equal(ps$lower, c(x = 1))
  ps$add(ParamFct$new("y", levels = c("a")))
  expect_equal(ps$length, 2L)
  expect_equal(ps$ids(), c("x", "y"))
  expect_equal(ps$lower, c(x = 1, y = NA))
})

test_that("as.data.table", {
  d = as.data.table(ParamSet$new())
  expect_data_table(d, nrows = 0)
  ps = th_paramset_full()
  d = as.data.table(ps)
  expect_data_table(d, nrows = 4, ncols = 11)
  expect_equal(ps$ids(), d$id)
  expect_equal(unname(ps$lower), d$lower)
  expect_equal(unname(ps$levels), d$levels)
})

test_that("ParamSet$default", {
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3, default = 2),
    ParamInt$new("y", lower = 1, upper = 3)
  ))
  expect_equal(ps$default, list(x = 2))
  expect_error(ParamDbl$new("x", lower = 1, upper = 3, default = 4))
  expect_error(ParamDbl$new("x", lower = 1, upper = 3, default = NULL))
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3, special_vals = list(NULL), default = NULL),
    ParamInt$new("y", lower = 1, upper = 3)
  ))
  expect_equal(ps$default, list(x = NULL))
})

test_that("is_number / is_categ / all_numeric / all_categoric", {
  expect_equal(th_paramset_full()$is_number,
    c(th_param_int = TRUE, th_param_dbl = TRUE, th_param_fct = FALSE, th_param_lgl = FALSE))
  expect_equal(th_paramset_full()$is_categ,
    c(th_param_int = FALSE, th_param_dbl = FALSE, th_param_fct = TRUE, th_param_lgl = TRUE))
  expect_equal(th_paramset_numeric()$all_numeric, TRUE)
  expect_equal(th_paramset_full()$all_numeric, FALSE)
  expect_equal(th_paramset_categorical()$all_categorical, TRUE)
  expect_equal(th_paramset_full()$all_categorical, FALSE)
})

test_that("ParamSet$ids", {
  ps = ParamSet$new(list(
    ParamDbl$new(id = "x", lower = 1, tags = c("t1")),
    ParamInt$new(id = "y", lower = 1, upper = 2),
    ParamFct$new(id = "z", levels = letters[1:3], tags = c("t1"))
  ))
  expect_equal(ps$ids(), c("x", "y", "z"))
  expect_equal(ps$ids(class = c("ParamInt", "ParamFct")), c("y", "z"))
  expect_equal(ps$ids(class = c("ParamInt", "ParamFct"), tags = "t1"), c("z"))
  expect_equal(ps$ids(is_bounded = TRUE), c("y", "z"))
})

test_that("ParamSet$get_values", {
  ps = ParamSet$new(list(
    ParamDbl$new(id = "x", lower = 1, tags = c("t1")),
    ParamInt$new(id = "y", lower = 1, upper = 2),
    ParamFct$new(id = "z", levels = letters[1:3], tags = c("t1"))
  ))
  expect_equal(ps$get_values(), named_list())
  expect_equal(ps$get_values(class = c("ParamInt", "ParamFct")), named_list())
  ps$values$x = 1
  expect_equal(ps$get_values(class = c("ParamInt", "ParamFct")), named_list())
  expect_equal(ps$get_values(is_bounded = TRUE), named_list())
  ps$values$y = 2
  expect_equal(ps$get_values(), list(x = 1, y = 2))
  expect_equal(ps$get_values(class = c("ParamInt", "ParamFct")), list(y = 2))
  expect_equal(ps$get_values(is_bounded = TRUE), list(y = 2))

  # 2 dependencies
  pss = ps(
    a = p_fct(c("b", "c")),
    b = p_int(depends = a == "b"),
    c = p_int(depends = a == "c")
  )

  pss$values$b = 1
  expect_list(pss$get_values(), len = 0)
  expect_equal(pss$get_values(remove_dependencies = FALSE), list(b = 1))

  pss$values$a = "c"
  expect_equal(pss$get_values(), list(a = "c"))
  expect_equal(pss$get_values(remove_dependencies = FALSE), list(b = 1, a = "c"))

  pss$values$a = "b"
  expect_equal(pss$get_values(), list(b = 1, a = "b"))

  pss$values$a = "b"
  pss$values$b = 1
  pss$values$c = 1

  expect_equal(pss$get_values(), list(b = 1, a = "b"))
  expect_equal(pss$get_values(remove_dependencies = FALSE), list(b = 1, a = "b", c = 1))

  # 2 dependencies and tune token
  pss = ps(
    a = p_fct(c("b", "c")),
    b = p_int(depends = a == "b"),
    c = p_int(depends = a == "c")
  )

  pss$values$a = to_tune()
  pss$values$b = 1
  pss$values$c = 1

  expect_equal(pss$get_values(), list(a = to_tune(), b = 1L, c = 1L))

  # 3 dependencies
  pss = ps(
    a = p_fct(c("b", "c")),
    b = p_int(depends = a == "b"),
    c = p_int(depends = a == "c"),
    d = p_lgl(),
    e = p_int(depends = d == TRUE)
  )

  pss$values$a = "b"
  pss$values$b = 1
  pss$values$c = 1

  expect_equal(pss$get_values(), list(a = "b", b = 1L))
  expect_equal(pss$get_values(remove_dependencies = FALSE), list(a = "b", b = 1L, c = 1L))

  pss$values$e = 1

  expect_equal(pss$get_values(), list(a = "b", b = 1L))

  pss$values$d = FALSE

  expect_equal(pss$get_values(), list(a = "b", b = 1L, d = FALSE))

  pss$values$d = TRUE

  expect_equal(pss$get_values(), list(a = "b", b = 1L, e = 1, d = TRUE))

  # nested dependencies
  pss = ps(
    a = p_fct(c("b", "c")),
    b = p_int(depends = a == "b"),
    c = p_int(depends = b == 1)
  )

  pss$values$c = 1
  expect_list(pss$get_values(), len = 0)

  pss$values$b = 1
  expect_list(pss$get_values(), len = 0)

  pss$values$a = "b"
  expect_equal(pss$get_values(), list(c = 1, b = 1, a = "b"))

  pss$values = list()
  pss$values$b = 1
  expect_list(pss$get_values(), len = 0)

  pss$values$a = "b"
  expect_equal(pss$get_values(), list(b = 1, a = "b"))
})

test_that("required tag", {
  ps = ParamSet$new(list(
    ParamDbl$new(id = "x", tags = c("required")),
    ParamInt$new(id = "y")
  ))
  expect_equal(ps$ids(), c("x", "y"))
  expect_equal(ps$ids(tags = "required"), "x")
  ps$values = list(y = 1)
  expect_error(ps$get_values(),
    regexp = "Missing required parameters: x",
    fixed = TRUE)

  ps$values = list()
    expect_error(ps$get_values(),
    regexp = "Missing required parameters: x",
    fixed = TRUE)
})

test_that("required tag, empty param set (#219)", {
  ps = ParamSet$new()
  ps$ids()
  expect_identical(ps$ids(tags = "required"), character(0))
})

test_that("paramset clones properly", {
  ps = ParamSet$new()
  ps$add(ParamFct$new("a", levels = letters[1:3]))
  ps$add(ParamFct$new("b", levels = letters[1:3]))
  ps$add_dep("a", "b", CondAnyOf$new(letters[1:2]))
  ps2 = ps$clone(deep = TRUE)

  expect_equal(ps, ps2)

  ps$deps$cond[[1]]$rhs = c("b", "c")

  expect_equal(ps$deps$cond[[1]]$rhs, c("b", "c"))
  expect_equal(ps2$deps$cond[[1]]$rhs, c("a", "b"))
})

test_that("ParamSet$check_dt", {
  ps = th_paramset_numeric()
  xdt = data.table()
  expect_true(ps$check_dt(xdt))
  xdt = data.table(th_param_int = c(1, 1), th_param_dbl = c(1, 1))
  expect_true(ps$check_dt(xdt))
  xdt = data.table(th_param_dbl = c(1, 1), th_param_int = c(1, 1))
  expect_true(ps$check_dt(xdt))
  xdt = data.table(th_param_dbl = c(20, 20), th_param_int = c(1, 1))
  expect_character(ps$check_dt(xdt), fixed = "th_param_dbl: Element 1 is not <= 10")
  xdt = data.table(th_param_dbl = c(1, 1), th_param_int = c(1, 20))
  expect_character(ps$check_dt(xdt), fixed = "th_param_int: Element 1 is not <= 10")
  xdt = data.table(th_param_dbl = c(1, 1), new_param = c(1, 20))
  expect_character(ps$check_dt(xdt), fixed = "not available")
  ps = ParamLgl$new("x")$rep(2)
  ps$add_dep("x_rep_2", "x_rep_1", CondEqual$new(TRUE))
  xdt = data.table(x_rep_1 = c(TRUE, TRUE), x_rep_2 = c(FALSE, TRUE))
  expect_true(ps$check_dt(xdt, check_strict = TRUE))
  xdt = data.table(x_rep_1 = c(TRUE, TRUE, FALSE), x_rep_2 = c(FALSE, TRUE, FALSE))
  expect_character(ps$check_dt(xdt, check_strict = TRUE), fixed = "x_rep_1 = TRUE")
  xdt = data.table(x_rep_1 = c(TRUE, TRUE, FALSE), x_rep_2 = c(FALSE, TRUE, NA))
  expect_true(ps$check_dt(xdt, check_strict = TRUE))
})

test_that("rd_info.ParamSet", {
  ps = ParamSet$new()
  expect_character(rd_info(ps))
  ps$add(ParamFct$new("a", levels = letters[1:3]))
  expect_character(rd_info(ps))
})


test_that("ParamSet$values convert nums to ints for ParamInt", {
  pp = ParamInt$new("x")
  ps = ParamSet$new(list(pp))
  ps$values$x = 2
  expect_class(ps$values$x, "integer")
})
