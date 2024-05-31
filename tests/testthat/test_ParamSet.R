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
    info = str_collapse(ps$class)
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
    expect_logical(ps$is_bounded, any.missing = FALSE, info = info)
    expect_names(names(ps$is_bounded), identical.to = ps$ids(), info = info)
    expect_flag(ps$all_bounded, info = info)
    expect_numeric(ps$nlevels, any.missing = FALSE, lower = 1, info = info)
    expect_list(ps$tags, types = "character", info = info)
    expect_names(names(ps$tags), identical.to = ps$ids(), info = info)
    expect_list(ps$default, names = "strict", info = info)
    expect_names(names(ps$default), subset.of = ps$ids(), info = info)
  }
  ps = th_paramset_full()
  expect_output(print(ps), fixed = "<ParamSet(4)>")
  expect_equal(ps$ids(), c("th_param_int", "th_param_dbl", "th_param_fct", "th_param_lgl"))
  expect_equal(ps$lower, c(th_param_int = -10, th_param_dbl = -10, th_param_fct = NA_real_, th_param_lgl = NA_real_))
  expect_equal(ps$upper, c(th_param_int = 10, th_param_dbl = 10, th_param_fct = NA_real_, th_param_lgl = NA_real_))
})

test_that("ParamSet$subset", {
  ids = th_paramset_full()$ids()
  getps = function() th_paramset_full()$clone(deep = TRUE) # give us a fresh clone of the fullset
  # we can subset to an empty set
  ps = getps()
  ps = ps$subset(character(0L))
  expect_true(ps$is_empty)
  ps = getps()
  # subsetting to 2 params make the set smaller
  ps = ps$subset(ids[2:3])
  expect_equal(ps$ids(), ids[2:3])
  expect_equal(ps$length, 2)
  # subsetting to all ids does not change anything
  ps = getps()
  ps = ps$subset(ids)
  expect_equal(as.data.table(ps), as.data.table(getps()))
  # subset full set to 2 numeric params
  ps = getps()
  ps = ps$subset(c("th_param_int", "th_param_dbl"))
  expect_equal(as.data.table(ps), as.data.table(th_paramset_numeric()))
})

test_that("ParamSet$add_param_set", {
  # adding with the empty set
  ps1 = ParamSet$new()
  n1 = ps1$length
  ps2 = ParamSet$new()
  ps1 = ps_union(list(ps1, ps2))
  expect_equal(ps1$length, n1)
  ps2 = ps_union(list(ps2, ps1))
  expect_equal(ps2$length, n1)

  # adding 2 sets, numeric and untyped, makes them larger
  ps1 = th_paramset_numeric()$clone(deep = TRUE)
  ps2 = th_paramset_untyped()$clone(deep = TRUE)
  ps1 = ps_union(list(ps1, ps2))
  expect_equal(ps2$length, 1L)
  expect_equal(ps1$ids(), c("th_param_int", "th_param_dbl", "th_param_uty"))
  ps1 = th_paramset_numeric()$clone(deep = TRUE)
  ps2 = th_paramset_untyped()$clone(deep = TRUE)
  ps2 = ps_union(list(ps2, ps1))
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

  ps = ps_replicate(ParamLgl$new("x"), 2)
  ps$add_dep("rep1.x", "rep2.x", CondEqual(TRUE))
  expect_string(ps$check(list(rep1.x = FALSE, rep2.x = FALSE), check_strict = TRUE), fixed = "rep2.x == TRUE")
})

test_that("we cannot create ParamSet with non-strict R names", {
  expect_error(ParamDbl$new("$foo"), "does not comply")
})

test_that("ParamSets cannot have duplicated ids", {
  p1 = ParamDbl$new("x1")
  p2 = ParamDbl$new("x1")
  expect_error(ParamSet_legacy$new(list(p1, p2)), "duplicated")
  ps = ParamSet_legacy$new(list(p1))
  expect_error(ps_union(list(ps, p2)), "duplicated")
  expect_error(ps_union(list(ps, ParamSet_legacy$new(list(p2)))), "duplicated")
})

test_that("ParamSet$print", {
  ps = ParamSet_legacy$new()
  expect_output(print(ps), fixed = "<ParamSet(0)>")
  expect_output(print(ps), "Empty")
  ps = th_paramset_numeric()
  expect_output(print(ps), fixed = sprintf("<ParamSet(%s)>", ps$length))
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
    expect_output(print(ps), fixed = sprintf("<ParamSet(%s)>", ps$length))
  }
})

test_that("ParamSet does a deep copy of params on construction", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps = ParamSet_legacy$new(list(y = p))
  p$values = list(x = 1)
  ps$values = list(y.x = 2)
  expect_equal(p$values, list(x = 1))
  expect_equal(ps$values, list(y.x = 2))
})

test_that("ParamSet does a deep copy of param on add", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps = ps_union(list(ParamSet_legacy$new(list()), ParamSet_legacy$new(list(y = p))))
  p$values = list(x = 1)
  ps$values = list(y.x = 2)
  expect_equal(p$values, list(x = 1))
  expect_equal(ps$values, list(y.x = 2))
})

test_that("ParamSet$clone can be deep", {

  p1 = c(ParamDbl$new("x", lower = 1, upper = 3), ParamDbl$new("foo", lower = -10, upper = 10))
  p2 = ParamFct$new("y", levels = c("a", "b"))
  ps1 = ParamSet_legacy$new(list(p1, p2))
  ps2 = ps1$clone(deep = TRUE)

  p3 = ParamFct$new("z", levels = c("a", "b"))
  ps2 = ps_union(list(ps2, p3))

  expect_equal(ps2$params, ps_union(list(p1, p2, p3))$params)
  expect_equal(ps1$params, ps_union(list(p1, p2))$params)

  # now lets add a dep, see if that gets clones properly
  ps1$add_dep("x", on = "y", CondEqual("a"))
  ps2 = ps1$clone(deep = TRUE)
  d = ps2$deps$id[1] = "foo"
  expect_equal(ps2$deps$id[1], "foo")
  expect_equal(ps1$deps$id[1], "x")

  ps = ParamSet_legacy$new()
  expect_equal(ps, ps$clone(deep = TRUE))
})

test_that("ParamSet$is_bounded", {
  ps = ParamSet_legacy$new(list(
    ParamDbl$new("x", lower = 1, upper = 3)
  ))
  expect_true(ps$all_bounded)
  ps = ParamSet_legacy$new(list(
    ParamDbl$new("x", lower = 1, upper = 3),
    ParamLgl$new("y")
  ))
  expect_true(ps$all_bounded)
  ps = ParamSet_legacy$new(list(
    ParamDbl$new("x", lower = 1),
    ParamLgl$new("y")
  ))
  expect_false(ps$all_bounded)
})

test_that("ParamSet$add_param", {
  ps = ParamSet_legacy$new(list())
  ps = ps_union(list(ps, ParamDbl$new("x", lower = 1)))
  expect_equal(ps$length, 1L)
  expect_equal(ps$ids(), "x")
  expect_equal(ps$lower, c(x = 1))
  ps = ps_union(list(ps, ParamFct$new("y", levels = c("a"))))
  expect_equal(ps$length, 2L)
  expect_equal(ps$ids(), c("x", "y"))
  expect_equal(ps$lower, c(x = 1, y = NA))
})

test_that("as.data.table", {
  d = as.data.table(ParamSet_legacy$new())
  expect_data_table(d, nrows = 0)
  ps = th_paramset_full()
  d = as.data.table(ps)
  expect_data_table(d, nrows = 4, ncols = 11)
  expect_equal(ps$ids(), d$id)
  expect_equal(unname(ps$lower), d$lower)
  expect_equal(unname(ps$levels), d$levels)
})

test_that("ParamSet$default", {
  ps = ParamSet_legacy$new(list(
    ParamDbl$new("x", lower = 1, upper = 3, default = 2),
    ParamInt$new("y", lower = 1, upper = 3)
  ))
  expect_equal(ps$default, list(x = 2))
  expect_error(ParamDbl$new("x", lower = 1, upper = 3, default = 4))
  expect_error(ParamDbl$new("x", lower = 1, upper = 3, default = NULL))
  ps = ParamSet_legacy$new(list(
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
  ps = ParamSet_legacy$new(list(
    ParamDbl$new(id = "x", lower = 1, tags = c("t1")),
    ParamInt$new(id = "y", lower = 1, upper = 2),
    ParamFct$new(id = "z", levels = letters[1:3], tags = c("t1"))
  ))
  expect_equal(ps$ids(), c("x", "y", "z"))
  expect_equal(ps$ids(class = c("ParamInt", "ParamFct")), c("y", "z"))
  expect_equal(ps$ids(class = c("ParamInt", "ParamFct"), tags = "t1"), c("z"))
  expect_equal(ps$ids(class = c("ParamInt", "ParamFct"), any_tags = c("t1", "t2")), c("z"))
  expect_equal(ps$ids(class = c("ParamInt", "ParamFct"), tags = c("t1", "t2")), character(0))
})

test_that("ParamSet$get_values", {
  ps = ParamSet_legacy$new(list(
    ParamDbl$new(id = "x", lower = 1, tags = c("t1")),
    ParamInt$new(id = "y", lower = 1, upper = 2),
    ParamFct$new(id = "z", levels = letters[1:3], tags = c("t1"))
  ))
  expect_equal(ps$get_values(), named_list())
  expect_equal(ps$get_values(class = c("ParamInt", "ParamFct")), named_list())
  ps$values$x = 1
  expect_equal(ps$get_values(class = c("ParamInt", "ParamFct")), named_list())
  ps$values$y = 2
  expect_equal(ps$get_values(), list(x = 1, y = 2))
  expect_equal(ps$get_values(class = c("ParamInt", "ParamFct")), list(y = 2))
})

test_that("required tag", {
  ps = ParamSet_legacy$new(list(
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
  ps = ParamSet_legacy$new()
  ps$ids()
  expect_identical(ps$ids(tags = "required"), character(0))
})

test_that("setting empty tags on empty paramset", {
  param_set = ps()
  nl <- structure(list(), names = character(0))
  expect_identical(param_set$tags, nl)
  param_set$tags = nl
  expect_identical(param_set$tags, nl)
  param_set$tags = list()
  expect_identical(param_set$tags, nl)

})

test_that("paramset clones properly", {
  ps = ParamSet_legacy$new()
  ps = ps_union(list(ps, ParamFct$new("a", levels = letters[1:3])))
  ps = ps_union(list(ps, ParamFct$new("b", levels = letters[1:3])))
  ps$add_dep("a", "b", CondAnyOf(letters[1:2]))
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
  ps = ps_replicate(ParamLgl$new("x"), 2)
  ps$add_dep("rep2.x", "rep1.x", CondEqual(TRUE))
  xdt = data.table(rep1.x = c(TRUE, TRUE), rep2.x = c(FALSE, TRUE))
  expect_true(ps$check_dt(xdt, check_strict = TRUE))
  xdt = data.table(rep1.x = c(TRUE, TRUE, FALSE), rep2.x = c(FALSE, TRUE, FALSE))
  expect_character(ps$check_dt(xdt, check_strict = TRUE), fixed = "rep1.x == TRUE")
  xdt = data.table(rep1.x = c(TRUE, TRUE, FALSE), rep2.x = c(FALSE, TRUE, NA))
  expect_true(ps$check_dt(xdt, check_strict = TRUE))
})

test_that("rd_info.ParamSet", {
  ps = ParamSet_legacy$new()
  expect_character(rd_info(ps), pattern = "empty", ignore.case = TRUE)
  ps = ps_union(list(ps, ParamFct$new("a", levels = letters[1:3])))
  expect_character(rd_info(ps), len = 1L)
})


test_that("ParamSet$values convert nums to ints for ParamInt", {
  pp = ParamInt$new("x")
  ps = ParamSet_legacy$new(list(pp))
  ps$values$x = 2
  expect_class(ps$values$x, "integer")
})

test_that("Empty ParamSets are named (#351)", {
  ps = ps_union(list(ps(), ps(x = p_lgl())))
  expect_names(names(ps$values), type = "strict")
  expect_is(ps$search_space(), "ParamSet")
})

test_that("set_values checks inputs correctly", {
  param_set = ps(a = p_dbl(), b = p_dbl())
  expect_error(param_set$set_values(a = 2, .values = list(a = 1)))
  expect_error(param_set$set_values(2))
  expect_error(param_set$set_values(.values = list(1)))
})

test_that("set_values works for ... with correct inputs", {
  param_set = ps(a = p_dbl(), b = p_dbl())
  param_set$values$a = 1
  param_set$set_values(b = 2, .insert = FALSE)
  expect_true(is.null(param_set$values$a))
  expect_true(param_set$values$b == 2)
  param_set$values = list(a = 1)
  param_set$set_values(b = 2, .insert = TRUE)
  expect_true(param_set$values$b == 2)
  expect_true(param_set$values$a == 1)
})

test_that("set_values works for .values with correct inputs", {
  param_set = ps(a = p_dbl(), b = p_dbl())
  param_set$values$a = 1
  param_set$set_values(.values = list(b = 2), .insert = FALSE)
  expect_true(is.null(param_set$values$a))
  expect_true(param_set$values$b == 2)
  param_set$values = list(a = 1)
  param_set$set_values(.values = list(b = 2), .insert = TRUE)
  expect_true(param_set$values$b == 2)
  expect_true(param_set$values$a == 1)
})

test_that("set_values works for .values and ... with correct inputs", {
  param_set = ps(a = p_dbl(), b = p_dbl(), c = p_dbl())
  param_set$values$a = 1
  param_set$set_values(b = 2, .values = list(c = 3), .insert = TRUE)
  expect_true(param_set$values$a == 1)
  expect_true(param_set$values$b == 2)
  expect_true(param_set$values$c == 3)

  param_set$values = list(a = 1)
  param_set$set_values(b = 2, .values = list(c = 3), .insert = FALSE)
  expect_true(is.null(param_set$values$a))
  expect_true(param_set$values$b == 2)
  expect_true(param_set$values$c == 3)
})

test_that("set_values allows to unset parameters by setting them to NULL", {
  param_set = ps(a = p_int())
  param_set$set_values(a = 1)
  param_set$set_values(a = NULL)
  expect_true(length(param_set$values) == 0)

  param_set = ps(a = p_int())
  param_set$set_values(a = 1)
  param_set$set_values(.values = list(a = NULL))
  expect_true(length(param_set$values) == 0)

  param_set = ps(a = p_int())
  param_set$set_values(a = 1)
  # .insert = FALSE can also set values to NULL
  expect_error(param_set$set_values(.values = list(a = NULL), .insert = FALSE), "not 'NULL'")
  param_set = ps(a = p_int(special_vals = list(NULL)))
  param_set$set_values(a = 1)
  param_set$set_values(.values = list(a = NULL), .insert = FALSE)
  expect_identical(param_set$values, list(a = NULL))
})

test_that("aggr", {
  param_set = ps(
    a = p_uty(aggr = function(x) "a"),
    b = p_fct(levels = c("a", "b"), aggr = function(x) "b"),
    c = p_lgl(aggr = function(x) "c"),
    d = p_int(aggr = function(x) "d"),
    e = p_dbl(aggr = function(x) "e")
  )
  expect_class(param_set, "ParamSet")

  vals = param_set$aggr_internal_tuned_values(
  list(a = list(1), b = list(1), c = list(1), d = list(1), e = list(1)))
  expect_equal(vals, list(a = "a", b = "b", c = "c", d = "d", e = "e"))

  expect_error(param_set$aggr_internal_tuned_values(1), "list")
  expect_error(param_set$aggr_internal_tuned_values(list(1)), "list")
  expect_error(param_set$aggr_internal_tuned_values(list(y = list())), "subset")
  expect_error(param_set$aggr_internal_tuned_values(
    list(a = list(), b = list(), c = list(), d = list(), e = list())), "but there are no")
})

test_that("convert_internal_search_space", {
  param_set = ps(
    a = p_int(lower = 1, upper = 100, tags = "internal_tuning", in_tune_fn = function(domain, param_vals) domain$upper,
      aggr = function(x) round(mean(unlist(x))), disable_in_tune = list(a = 1))
  )
  param_set$set_values(a = to_tune(internal = TRUE))
  expect_identical(param_set$convert_internal_search_space(param_set$search_space()), list(a = 100))
  param_set$set_values(a = to_tune(internal = TRUE, upper = 99))
  expect_identical(param_set$convert_internal_search_space(param_set$search_space()), list(a = 99))
})

test_that("get_values works with internal_tune", {
  param_set = ps(
    a = p_int(lower = 1, upper = 100, tags = "internal_tuning", in_tune_fn = function(domain, param_vals) domain$upper,
      aggr = function(x) round(mean(unlist(x))), disable_in_tune = list(a = 1))
  )
  param_set$set_values(a = to_tune(internal = TRUE))
  expect_list(param_set$get_values(type = "with_internal"), len = 1L)
  param_set$set_values(a = to_tune())
  expect_list(param_set$get_values(type = "with_internal"), len = 0L)
})

test_that("InternalTuneToken is translated to 'internal_tuning' tag when creating search space", {
  param_set = ps(
    a = p_int(0, Inf, tags = "internal_tuning", in_tune_fn = function(domain, param_vals) domain$upper, aggr = function(x) round(mean(unlist(x)), aggr = function(x) 1),
      disable_in_tune = list())
  )

  param_set$set_values(
    a = to_tune(upper = 100, internal = TRUE)
  )

  ss = param_set$search_space()
  expect_true("internal_tuning" %in% ss$tags$a)
})

test_that("disable internal tuning", {
  param_set = ps(
    a = p_dbl(tags = "internal_tuning", in_tune_fn = function(domain, param_vals) domain$upper, disable_in_tune = list(b = FALSE), aggr = function(x) 1),
    b = p_lgl()
  )

  expect_equal(param_set$values$b, NULL)
  param_set$disable_internal_tuning("a")
  expect_equal(param_set$values$b, FALSE)

  expect_error(param_set$disable_internal_tuning("c"))
  expect_error(param_set$disable_internal_tuning("b"))
})
