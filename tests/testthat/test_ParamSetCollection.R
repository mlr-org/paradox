context("ParamSetCollection")

test_that("simple active bindings work", {
  ps1 = th_paramset_dbl1()
  ps1$set_id = "s1"
  ps2 = th_paramset_full()
  ps2$set_id = "s2"
  psc = ParamSetCollection$new(list(ps1, ps2))

  my_c = function(xs1, xs2) {  # littler helper to join to ps-result and prefix names
    ns = c(paste0("s1.", names(xs1)), paste0("s2.", names(xs2)))
    set_names(c(xs1, xs2), ns)
  }

  expect_class(psc, "ParamSetCollection")
  expect_equal(psc$length, ps1$length + ps2$length)
  p = psc$params[[2L]]; p = p$clone()
  p$id = "th_param_int"
  expect_equal(p, ps2$params[[1L]])
  expect_equal(psc$ids(), c(paste0("s1.", ps1$ids()), paste0("s2.", ps2$ids())))
  expect_equal(psc$lower, my_c(ps1$lower, ps2$lower))
  d = as.data.table(psc)
  expect_data_table(d, nrows = 5)
  expect_false(psc$has_deps)
  expect_false(psc$has_trafo)
  expect_true(psc$check(list(s1.th_param_dbl = 1, s2.th_param_int = 2)))
  expect_string(psc$check(list(th_param_dbl = 1, th_param_int = 2)), "must be subset")

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

