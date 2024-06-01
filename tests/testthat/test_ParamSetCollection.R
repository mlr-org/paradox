context("ParamSetCollection")

test_that("ParamSet basic stuff works", {
  ps1 = th_paramset_dbl1()
  ps2 = th_paramset_full()
  ps3 = th_paramset_dbl1()
  psc = ParamSetCollection$new(list(s1 = ps1, s2 = ps2, ps3))

  ps1clone = ps1$clone(deep = TRUE)
  ps2clone = ps2$clone(deep = TRUE)

  my_c = function(xs1, xs2, xs3) {
    # littler helper to join to ps-result and prefix names
    ns = c(paste0("s1.", names(xs1)), paste0("s2.", names(xs2)), names(xs3))
    set_names(c(xs1, xs2, xs3), ns)
  }

  expect_class(psc, "ParamSetCollection")
  expect_equal(psc$length, ps1$length + ps2$length + ps3$length)
  # check that param internally in collection is constructed correctly
  p = psc$params[2L]
  p$id = "th_param_int"

  expect_equal(p, ps2$params[1L])
  expect_equal(psc$ids(), c(paste0("s1.", ps1$ids()), paste0("s2.", ps2$ids()), ps3$ids()))
  expect_equal(psc$lower, my_c(ps1$lower, ps2$lower, ps3$lower))
  d = as.data.table(psc)
  expect_data_table(d, nrows = 6)
  expect_false(psc$has_deps)
  expect_false(psc$has_trafo)

  d = as.data.table(psc)
  expect_equal(d$id, c(paste0("s1.", ps1$ids()), paste0("s2.", ps2$ids()), ps3$ids()))

  expect_true(psc$check(list(s1.th_param_dbl = 1, s2.th_param_int = 2)))
  expect_string(psc$check(list(th_param_int = 2)), fixed = "not avail")
  expect_true(psc$check(list(th_param_dbl = 1)))

  d = generate_design_random(psc, n = 10L)
  expect_data_table(d$data, nrows = 10, ncols = 6L)

  psflat = psc$flatten()
  psflat$extra_trafo = function(x, param_set) {
    x$s2.th_param_int = 99 # nolint
    return(x)
  }
  expect_true(psflat$has_trafo)
  d = generate_design_random(psflat, n = 10L)
  expect_data_table(d$data, nrows = 10, ncols = 6L)
  xs = d$transpose(trafo = TRUE)
  for (i in 1:10) {
    x = xs[[i]]
    expect_list(x, len = 6)
    expect_names(names(x), permutation.of = psc$ids())
    expect_equal(x$s2.th_param_int, 99)
  }

  # ps1 and ps2 should not be changed
  expect_equal(ps1, ps1clone)
  expect_equal(ps2, ps2clone)

  expect_output(print(psc), "s1\\.th_param_dbl.*s2\\.th_param_int.*s2\\.th_param_dbl.*s2\\.th_param_fct.*s2\\.th_param_lgl.*th_param_dbl") # nolint

  # ps1 and ps2 should not be changed by printing
  expect_equal(ps1, ps1clone)
  expect_equal(ps2, ps2clone)

  # adding a set
  ps4 = ParamSet_legacy$new(list(ParamDbl$new("x")))
  psc = psc$add(ps4, n = "s4")
  expect_equal(psc$length, ps1$length + ps2$length + ps3$length + ps4$length)
  expect_equal(psc$ids(), c(paste0("s1.", ps1$ids()), paste0("s2.", ps2$ids()), ps3$ids(), paste0("s4.", ps4$ids())))
})

test_that("some operations are not allowed", {
  ps1 = th_paramset_dbl1()
  ps2 = th_paramset_full()
  psc = ParamSetCollection$new(list(s1 = ps1, s2 = ps2))

  expect_error(psc$subset("foo"), "Must be a subset of")
})

test_that("deps", {
  ps1 = ParamSet_legacy$new(list(
    ParamFct$new("f", levels = c("a", "b")),
    ParamDbl$new("d")
  ))
  ps1$add_dep("d", on = "f", CondEqual("a"))

  ps2 = ParamSet_legacy$new(list(
    ParamFct$new("f", levels = c("a", "b")),
    ParamDbl$new("d")
  ))

  ps1clone = ps1$clone(deep = TRUE)
  ps2clone = ps2$clone(deep = TRUE)

  psc = ParamSetCollection$new(list(ps1 = ps1, ps2 = ps2))
  d = psc$deps
  expect_data_table(d, nrows = 1, ncols = 3)
  expect_equal(d$id, c("ps1.d"))

  # check deps across sets
  psc$add_dep("ps2.d", on = "ps1.f", CondEqual("a"))
  expect_data_table(psc$deps, nrows = 2, ncols = 3)
  expect_true(psc$check(list(ps1.f = "a", ps1.d = 0, ps2.d = 0)))
  expect_string(psc$check(list(ps2.d = 0), check_strict = TRUE))

  # ps1 and ps2 should not be changed
  expect_equal(ps1clone, ps1)
  expect_equal(ps2clone, ps2)
})

test_that("values", {
  ps1 = ParamSet_legacy$new(list(
    ParamFct$new("f", levels = c("a", "b")),
    ParamDbl$new("d", lower = 1, upper = 8)
  ))
  ps2 = ParamSet_legacy$new(list(
    ParamFct$new("f", levels = c("a", "b")),
    ParamDbl$new("d", lower = 1, upper = 8)
  ))
  ps3 = ParamSet_legacy$new(list(
    ParamDbl$new("x", lower = 1, upper = 8)
  ))
  ps4 = ParamSet_legacy$new(list(
    ParamDbl$new("y", lower = 1, upper = 8)
  ))

  ps1clone = ps1$clone(deep = TRUE)
  ps2clone = ps2$clone(deep = TRUE)

  pcs = ParamSetCollection$new(list(foo = ps1, bar = ps2, ps3, ps4))
  expect_equal(pcs$values, named_list())
  ps2$values = list(d = 3)
  expect_equal(pcs$values, list(bar.d = 3))
  pcs$values = list(foo.d = 8)
  expect_equal(pcs$values, list(foo.d = 8))
  expect_equal(ps1$values, list(d = 8))
  expect_equal(ps2$values, named_list())
  pcs$values = list(x = 1)
  expect_equal(pcs$values, list(x = 1))
  expect_equal(ps3$values, list(x = 1))

  ps1clone$values$d = 8
  pcs$values = list(foo.d = 8)
  ps2$values = list()

  # data table adds indexes at will and comparisons fail because of that, so we have to remove them here.
  setindex(ps1clone$deps, NULL)
  setindex(ps2clone$deps, NULL)
  setindex(ps1$deps, NULL)
  setindex(ps2$deps, NULL)

  expect_equal(ps1clone, ps1)
  expect_equal(ps2clone, ps2)

  # resetting pcs values
  pcs$values = list()
  expect_list(pcs$values, len = 0)
})

test_that("empty collections", {
  # no paramsets
  psc = ParamSetCollection$new(list())
  expect_equal(psc$length, 0L)
  expect_equal(psc$subspaces(), named_list())
  expect_equal(psc$ids(), character(0L))
  expect_data_table(as.data.table(psc), nrows = 0L)

  # 1 empty paramset
  psc = ParamSetCollection$new(list(ParamSet_legacy$new()))
  expect_equal(psc$length, 0L)
  expect_equal(psc$subspaces(), named_list())
  expect_equal(psc$ids(), character(0L))
  expect_data_table(as.data.table(psc), nrows = 0L)
})


test_that("no problems if we name the list of sets", {
  ps = ParamSet_legacy$new(list(ParamDbl$new("test1")))
  psc = ParamSetCollection$new(list(paramset = ps))
  expect_equal(names(psc$subspaces()), "paramset.test1")
})

test_that("no warning in printer, see issue 208", {
  ps = ParamSet_legacy$new(list(ParamDbl$new("test1")))

  psc = ParamSetCollection$new(list(paramset = ps))
  psc$values = list(paramset.test1 = 1)
  expect_warning(capture_output(print(ps)), NA)
})

test_that("collection allows state-change setting of paramvals, see issue 205", {
  ps1 = ParamSet_legacy$new(list(ParamDbl$new("d1")))
  ps2 = ParamSet_legacy$new(list(ParamDbl$new("d2")))
  ps3 = ParamSet_legacy$new(list(ParamDbl$new("d3")))

  psc = ParamSetCollection$new(list(s1 = ps1, s2 = ps2, ps3))
  expect_equal(psc$values, named_list())
  psc$values$s1.d1 = 1 # nolint
  expect_equal(psc$values, list(s1.d1 = 1))
  psc$values$s2.d2 = 2 # nolint
  expect_equal(psc$values, list(s1.d1 = 1, s2.d2 = 2))
  psc$values$d3 = 3
  expect_equal(psc$values, list(s1.d1 = 1, s2.d2 = 2, d3 = 3))
})

test_that("set_id inference in values assignment works now", {
  psa = ParamSet_legacy$new(list(ParamDbl$new("parama")))

  psb = ParamSet_legacy$new(list(ParamDbl$new("paramb")))

  psc = ParamSet_legacy$new(list(ParamDbl$new("paramc")))

  pscol1 = ParamSetCollection$new(list(b = psb, c = psc))

  pscol2 = ParamSetCollection$new(list(a.b = psa, a = pscol1))

  pstest = ParamSet_legacy$new(list(ParamDbl$new("paramc")))

  expect_error(pscol2$add(pstest, n = "a.c"), "would lead to nameclashes.*a\\.c\\.paramc")

  pstest = ParamSet_legacy$new(list(ParamDbl$new("a.c.paramc")))

  expect_error(pscol2$add(pstest), "would lead to nameclashes.*a\\.c\\.paramc")

  pscol2$values = list(a.c.paramc = 3, a.b.parama = 1, a.b.paramb = 2)

  expect_equal(psa$values, list(parama = 1))
  expect_equal(psb$values, list(paramb = 2))
  expect_equal(psc$values, list(paramc = 3))
  expect_equal(pscol1$values, list(b.paramb = 2, c.paramc = 3))
  expect_equal(pscol2$values, list(a.b.parama = 1, a.b.paramb = 2, a.c.paramc = 3))

  expect_error(ParamSetCollection$new(list(a = pscol1, pstest)),
    "duplicated parameter.* a\\.c\\.paramc")
})

test_that("disable internal tuning works", {
  param_set = psc(prefix = ps(
    a = p_dbl(aggr = function(x) 1, tags = "internal_tuning", in_tune_fn = function(domain, param_vals) domain$upper, disable_in_tune = list(b = FALSE)),
    b = p_lgl()
  ))

  param_set$disable_internal_tuning("prefix.a")
  expect_equal(param_set$values$prefix.b, FALSE)
  expect_error(param_set$disable_internal_tuning("b"))

  expect_equal(named_list(), psc(ps())$disable_internal_tuning(character(0))$values)
})

test_that("convert_internal_search_space: depends on other parameter", {
  param_set = psc(a = ps(
    b = p_int(tags = "internal_tuning", in_tune_fn = function(domain, param_vals) param_vals$c * domain$upper,
      aggr = function(x) 1, disable_in_tune = list()),
    c = p_int()
  ))
  param_set$values$a.c = -1

  search_space = ps(
    a.b = p_int(upper = 1000, tags = "internal_tuning", aggr = function(x) 1)
  )

  expect_equal(
    param_set$convert_internal_search_space(search_space)$a.b, 
    -1000
  )
})

test_that("convert_internal_search_space: nested collections", {
  param_set = psc(a = psc(b = ps(param = p_int(
    in_tune_fn = function(domain, param_vals) domain$upper, tags = "internal_tuning", disable_in_tune = list(), aggr = function(x) 1
  ))))

  search_space = ps(
    a.b.param = p_int(upper = 99, tags = "internal_tuning", aggr = function(x) 1)
  )

  expect_equal(
    param_set$convert_internal_search_space(search_space),
    list(a.b.param = 99)
  )
})

test_that("convert_internal_search_space: flattening", {
  param_set = psc(a = psc(b = ps(
    param = p_int(
    in_tune_fn = function(domain, param_vals) domain$upper * param_vals$other_param, tags = "internal_tuning",
    disable_in_tune = list(), aggr = function(x) 1),
    other_param = p_int()
  )))

  param_set$values$a.b.other_param = -1

  search_space = ps(
    a.b.param = p_int(upper = 99, tags = "internal_tuning", aggr = function(x) 1)
  )

  expect_equal(
    param_set$flatten()$convert_internal_search_space(search_space),
    list(a.b.param = -99)
  )
})

test_that("disable internal tuning: single collection", {
  param_set = psc(a = ps(
    b = p_int(
      in_tune_fn = function(domain, param_vals) domain$upper, tags = "internal_tuning",
      disable_in_tune = list(c = TRUE), aggr = function(x) 1
    ),
    c = p_lgl()
  ))

  param_set$disable_internal_tuning("a.b")
  expect_equal(param_set$values$a.c, TRUE)
})

test_that("disable internal tuning: nested collection", {
  param_set = ps(
    a = p_int(
      in_tune_fn = function(domain, param_vals) domain$upper, tags = "internal_tuning",
      disable_in_tune = list(), aggr = function(x) 1
    )
  )
})
test_that("disable internal tuning: nested flattening", {
  param_set = psc(a = ps(
    b = p_int(
      in_tune_fn = function(domain, param_vals) domain$upper, tags = "internal_tuning",
      disable_in_tune = list(c = 1), aggr = function(x) 1
    ),
    c = p_int()
  ))$flatten()

  expect_equal(
    param_set$disable_internal_tuning("a.b")$values$a.c,
    1
  )

  # now with no set id
  param_set = psc(ps(
    b = p_int(
      in_tune_fn = function(domain, param_vals) domain$upper, tags = "internal_tuning",
      disable_in_tune = list(c = 1), aggr = function(x) 1
    ),
    c = p_int()
  ))$flatten()

  expect_equal(
    param_set$disable_internal_tuning("b")$values$c,
    1
  )
})
