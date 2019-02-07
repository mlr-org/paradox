context("ParamSetCollection")

test_that("simple active bindings work", {

  ps1 = th_paramset_dbl1()
  ps1$set_id = "s1"
  ps2 = th_paramset_full()
  ps2$set_id = "s2"
  psc = ParamSetCollection$new(list(ps1, ps2))

  ps1clone = ps1$clone(deep = TRUE)
  ps2clone = ps2$clone(deep = TRUE)

  my_c = function(xs1, xs2) {  # littler helper to join to ps-result and prefix names
    ns = c(paste0("s1.", names(xs1)), paste0("s2.", names(xs2)))
    set_names(c(xs1, xs2), ns)
  }

  expect_class(psc, "ParamSetCollection")
  expect_equal(psc$length, ps1$length + ps2$length)
  # FIXME: der cloning krams sieht hier komisch aus und sollte wohl in einen etxra test?
  # p = psc$params[[2L]]; p = p$clone()
  # p$id = "th_param_int"
  # expect_equal(p, ps2$params[[1L]])
  expect_equal(psc$ids(), c(paste0("s1.", ps1$ids()), paste0("s2.", ps2$ids())))
  expect_equal(psc$lower, my_c(ps1$lower, ps2$lower))
  d = as.data.table(psc)
  expect_data_table(d, nrows = 5)
  expect_false(psc$has_deps)
  expect_false(psc$has_trafo)

  d = as.data.table(psc)
  expect_equal(d$id, c(paste0("s1.", ps1$ids()), paste0("s2.", ps2$ids())))

  expect_true(psc$check(list(s1.th_param_dbl = 1, s2.th_param_int = 2)))
  expect_string(psc$check(list(th_param_dbl = 1, th_param_int = 2)), fixed = "not avail")

  d = generate_design_random(psc, n = 10L)
  expect_data_table(d$data, nrows = 10, ncols = 5L)

  psc$trafo = function(x, param_set) {
    x$s2.th_param_int = 99
    return(x)
  }
  expect_true(psc$has_trafo)
  d = generate_design_random(psc, n = 10L)
  expect_data_table(d$data, nrows = 10, ncols = 5L)
  xs = d$transpose(trafo = TRUE)
  for (i in 1:10) {
    x = xs[[i]]
    expect_list(x, len = 5)
    expect_names(names(x), permutation.of = psc$ids())
    expect_equal(x$s2.th_param_int, 99)
  }

  # ps1 and ps2 should not be changed
  expect_equal(ps1, ps1clone)
  expect_equal(ps2, ps2clone)

  expect_output(print(psc), "s1\\.th_param_dbl.*s2\\.th_param_int.*s2\\.th_param_dbl.*s2\\.th_param_fct.*s2\\.th_param_lgl.*")

  # ps1 and ps2 should not be changed by printing
  expect_equal(ps1, ps1clone)
  expect_equal(ps2, ps2clone)

})


test_that("some operations are not allowed", {

  ps1 = th_paramset_dbl1()
  ps1$set_id = "s1"
  ps2 = th_paramset_full()
  ps2$set_id = "s2"
  psc = ParamSetCollection$new(list(ps1, ps2))

  expect_error(psc$subset("foo"), "not allowed")
  expect_error(psc$add(th_param_dbl), "not allowed")
  expect_error(psc$add(th_paramset_dbl1), "not allowed")
})

test_that("deps", {

  ps1 = ParamSet$new(list(
    ParamFct$new("f", values = c("a", "b")),
    ParamDbl$new("d")
  ))
  ps1$set_id = "ps1"
  ps1$add_dep("d", on = "f", CondEqual$new("a"))

  ps2 = ParamSet$new(list(
    ParamFct$new("f", values = c("a", "b")),
    ParamDbl$new("d")
  ))
  ps2$set_id = "ps2"

  ps1clone = ps1$clone(deep = TRUE)
  ps2clone = ps2$clone(deep = TRUE)

  psc = ParamSetCollection$new(list(ps1, ps2))
  d = psc$deps
  expect_data_table(d, nrows = 1, ncols = 3)
  expect_equal(d$id, c("ps1.d"))

  # check deps across sets
  psc$add_dep("ps2.d", on = "ps1.f", CondEqual$new("a"))
  expect_data_table(psc$deps, nrows = 2, ncols = 3)
  expect_true(psc$check(list(ps1.f = "a", ps1.d = 0, ps2.d = 0)))
  expect_string(psc$check(list(ps2.d = 0)))

  # ps1 and ps2 should not be changed
  expect_equal(ps1clone, ps1)
  expect_equal(ps2clone, ps2)
})

test_that("param_vals", {

  ps1 = ParamSet$new(list(
    ParamFct$new("f", values = c("a", "b")),
    ParamDbl$new("d", lower = 1, upper = 8)
  ))
  ps1$set_id = "foo"
  ps2 = ParamSet$new(list(
    ParamFct$new("f", values = c("a", "b")),
    ParamDbl$new("d", lower = 1, upper = 8)
  ))
  ps2$set_id = "bar"

  ps1clone = ps1$clone(deep = TRUE)
  ps2clone = ps2$clone(deep = TRUE)

  pcs = ParamSetCollection$new(list(ps1, ps2))
  expect_equal(pcs$param_vals, named_list())
  ps2$param_vals = list(d = 3)
  expect_equal(pcs$param_vals, list(bar.d = 3))
  pcs$param_vals = list(foo.d = 8)
  expect_equal(pcs$param_vals, list(foo.d = 8))
  expect_equal(ps1$param_vals, list(d = 8))
  expect_equal(ps2$param_vals, named_list())

  ps1clone$param_vals$d = 8
  ps2$param_vals = list()
  expect_equal(ps1clone, ps1)
  expect_equal(ps2clone, ps2)

})

test_that("empty collections", {
  # no paramsets
  psc = ParamSetCollection$new(list())
  expect_equal(psc$length, 0L)
  expect_equal(psc$params, named_list())
  expect_equal(psc$ids(), character(0L))
  expect_data_table(as.data.table(psc), nrow = 0L)

  # 1 empty paramset
  psc = ParamSetCollection$new(list(ParamSet$new()))
  expect_equal(psc$length, 0L)
  expect_equal(psc$params, named_list())
  expect_equal(psc$ids(), character(0L))
  expect_data_table(as.data.table(psc), nrow = 0L)
})


test_that("no problems if we name the list of sets", {
  ps = ParamSet$new(list(ParamDbl$new("test1")))
  psc = ParamSetCollection$new(list(prefix = ps))
  expect_equal(names(psc$params), "paramset.test1")
})

test_that("no warning in printer, see issue 208", {
  ps = ParamSet$new(list(ParamDbl$new("test1")))
  psc = ParamSetCollection$new(list(ps))
  psc$param_vals = list(paramset.test1 = 1)
  expect_warning(capture_output(print(ps)), NA)
})


test_that("collection reflects direct paramset$set_id change", {
  ps = ParamSet$new(list(ParamDbl$new("d")))
  psc = ParamSetCollection$new(list(ps))
  ps$param_vals = list(d = 1)
  expect_equal(psc$param_vals, list(paramset.d = 1))
  ps$set_id = "foo"
  expect_equal(psc$param_vals, list(foo.d = 1))
  expect_equal(psc$params, list(foo.d = ParamDbl$new("foo.d")))
})


test_that("collection allows state-change setting of paramvals, see issue 205", {
  ps1 = ParamSet$new(list(ParamDbl$new("d1")))
  ps1$set_id = "s1"
  ps2 = ParamSet$new(list(ParamDbl$new("d2")))
  ps2$set_id = "s2"
  psc = ParamSetCollection$new(list(ps1, ps2))
  expect_equal(psc$param_vals, named_list())
  psc$param_vals$s1.d1 = 1
  expect_equal(psc$param_vals, list(s1.d1 = 1))
  psc$param_vals$s2.d2 = 2
  expect_equal(psc$param_vals, list(s1.d1 = 1, s2.d2 = 2))
})

