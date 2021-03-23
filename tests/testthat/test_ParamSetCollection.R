context("ParamSetCollection")

test_that("ParamSet basic stuff works", {
  ps1 = th_paramset_dbl1()
  ps1$set_id = "s1"
  ps2 = th_paramset_full()
  ps2$set_id = "s2"
  ps3 = th_paramset_dbl1()
  ps3$set_id = ""
  psc = ParamSetCollection$new(list(ps1, ps2, ps3))

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
  p = psc$params[[2L]]
  p = p$with_id("th_param_int")
  expect_equal(p, ps2$params[[1L]])
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

  psc$trafo = function(x, param_set) {
    x$s2.th_param_int = 99 # nolint
    return(x)
  }
  expect_true(psc$has_trafo)
  d = generate_design_random(psc, n = 10L)
  expect_data_table(d$data, nrows = 10, ncols = 6L)
  xs = d$transpose(trafo = TRUE)
  for (i in 1:10) {
    x = xs[[i]]
    expect_list(x, len = 6)
    expect_names(names(x), permutation.of = psc$ids())
    expect_equal(x$s2.th_param_int, 99)
  }

  # Generate cached values for comparisons
  ps1$params
  ps2$params
  ps1clone$params
  ps2clone$params
  ps1$tags
  ps2$tags
  ps1clone$tags
  ps2clone$tags

  # ps1 and ps2 should not be changed
  expect_equal(ps1, ps1clone)
  expect_equal(ps2, ps2clone)

  expect_output(print(psc), "s1\\.th_param_dbl.*s2\\.th_param_int.*s2\\.th_param_dbl.*s2\\.th_param_fct.*s2\\.th_param_lgl.*th_param_dbl") # nolint

  # ps1 and ps2 should not be changed by printing
  expect_equal(ps1, ps1clone)
  expect_equal(ps2, ps2clone)

  # adding a set
  ps4 = ParamSet$new(list(ParamDbl$new("x")))
  ps4$set_id = "s4"
  psc$add(ps4)
  expect_equal(psc$length, ps1$length + ps2$length + ps3$length + ps4$length)
  expect_equal(psc$ids(), c(paste0("s1.", ps1$ids()), paste0("s2.", ps2$ids()), ps3$ids(), paste0("s4.", ps4$ids())))
  psc$remove_sets("s1")
  expect_equal(psc$length, ps2$length + ps3$length + ps4$length)
  expect_equal(psc$ids(), c(paste0("s2.", ps2$ids()), ps3$ids(), paste0("s4.", ps4$ids())))
})

test_that("some operations are not allowed", {
  ps1 = th_paramset_dbl1()
  ps1$set_id = "s1"
  ps2 = th_paramset_full()
  ps2$set_id = "s2"
  psc = ParamSetCollection$new(list(ps1, ps2))

  expect_error(psc$subset("foo"), "not allowed")
  expect_error(psc$add(th_param_dbl()), "ParamSet")
})

test_that("deps", {
  ps1 = ParamSet$new(list(
    ParamFct$new("f", levels = c("a", "b")),
    ParamDbl$new("d")
  ))
  ps1$set_id = "ps1"
  ps1$add_dep("d", on = "f", CondEqual$new("a"))

  ps2 = ParamSet$new(list(
    ParamFct$new("f", levels = c("a", "b")),
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

  ps1clone$tags
  ps2clone$tags
  ps1$tags
  ps2$tags

  # ps1 and ps2 should not be changed
  expect_equal(ps1clone, ps1)
  expect_equal(ps2clone, ps2)
})

test_that("values", {
  ps1 = ParamSet$new(list(
    ParamFct$new("f", levels = c("a", "b")),
    ParamDbl$new("d", lower = 1, upper = 8)
  ))
  ps1$set_id = "foo"
  ps2 = ParamSet$new(list(
    ParamFct$new("f", levels = c("a", "b")),
    ParamDbl$new("d", lower = 1, upper = 8)
  ))
  ps2$set_id = "bar"
  ps3 = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 8)
  ))
  ps4 = ParamSet$new(list(
    ParamDbl$new("y", lower = 1, upper = 8)
  ))

  ps1clone = ps1$clone(deep = TRUE)
  ps2clone = ps2$clone(deep = TRUE)

  pcs = ParamSetCollection$new(list(ps1, ps2, ps3, ps4))
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

  ps1$params
  ps2$params
  ps1clone$params
  ps2clone$params
  ps1$tags
  ps2$tags
  ps1clone$tags
  ps2clone$tags

  expect_equal(ps1clone, ps1)
  expect_equal(ps2clone, ps2)
})

test_that("empty collections", {
  # no paramsets
  psc = ParamSetCollection$new(list())
  expect_equal(psc$length, 0L)
  expect_equal(psc$params, named_list())
  expect_equal(psc$ids(), character(0L))
  expect_data_table(as.data.table(psc), nrows = 0L)

  # 1 empty paramset
  psc = ParamSetCollection$new(list(ParamSet$new()))
  expect_equal(psc$length, 0L)
  expect_equal(psc$params, named_list())
  expect_equal(psc$ids(), character(0L))
  expect_data_table(as.data.table(psc), nrows = 0L)
})


test_that("no problems if we name the list of sets", {
  ps = ParamSet$new(list(ParamDbl$new("test1")))
  ps$set_id = "paramset"
  psc = ParamSetCollection$new(list(prefix = ps))
  expect_equal(names(psc$params), "paramset.test1")
})

test_that("no warning in printer, see issue 208", {
  ps = ParamSet$new(list(ParamDbl$new("test1")))
  ps$set_id = "paramset"
  psc = ParamSetCollection$new(list(ps))
  psc$values = list(paramset.test1 = 1)
  expect_warning(capture_output(print(ps)), NA)
})


test_that("collection reflects direct paramset$set_id change", {
  ps = ParamSet$new(list(ParamDbl$new("d")))
  ps$set_id = "paramset"
  psc = ParamSetCollection$new(list(ps))
  ps$values = list(d = 1)
  expect_equal(psc$values, list(paramset.d = 1))
  ps$set_id = "foo"
  expect_equal(psc$values, list(foo.d = 1))
  expect_equal(psc$params, list(foo.d = ParamDbl$new("foo.d")))

  ps$set_id = ""
  expect_equal(psc$values, list(d = 1))
  expect_equal(psc$params, list(d = ParamDbl$new("d")))
})


test_that("collection allows state-change setting of paramvals, see issue 205", {
  ps1 = ParamSet$new(list(ParamDbl$new("d1")))
  ps1$set_id = "s1"
  ps2 = ParamSet$new(list(ParamDbl$new("d2")))
  ps2$set_id = "s2"
  ps3 = ParamSet$new(list(ParamDbl$new("d3")))
  ps3$set_id = ""

  psc = ParamSetCollection$new(list(ps1, ps2, ps3))
  expect_equal(psc$values, named_list())
  psc$values$s1.d1 = 1 # nolint
  expect_equal(psc$values, list(s1.d1 = 1))
  psc$values$s2.d2 = 2 # nolint
  expect_equal(psc$values, list(s1.d1 = 1, s2.d2 = 2))
  psc$values$d3 = 3
  expect_equal(psc$values, list(s1.d1 = 1, s2.d2 = 2, d3 = 3))
})

test_that("set_id inference in values assignment works now", {
  psa = ParamSet$new(list(ParamDbl$new("parama")))
  psa$set_id = "a.b"

  psb = ParamSet$new(list(ParamDbl$new("paramb")))
  psb$set_id = "b"

  psc = ParamSet$new(list(ParamDbl$new("paramc")))
  psc$set_id = "c"

  pscol1 = ParamSetCollection$new(list(psb, psc))
  pscol1$set_id = "a"

  pscol2 = ParamSetCollection$new(list(psa, pscol1))

  pstest = ParamSet$new(list(ParamDbl$new("paramc")))
  pstest$set_id = "a.c"

  expect_error(pscol2$add(pstest), "nameclashes.* a\\.c\\.paramc")

  pstest = ParamSet$new(list(ParamDbl$new("a.c.paramc")))
  pstest$set_id = ""
  expect_error(pscol2$add(pstest), "nameclashes.* a\\.c\\.paramc")

  pscol2$values = list(a.c.paramc = 3, a.b.parama = 1, a.b.paramb = 2)

  expect_equal(psa$values, list(parama = 1))
  expect_equal(psb$values, list(paramb = 2))
  expect_equal(psc$values, list(paramc = 3))
  expect_equal(pscol1$values, list(b.paramb = 2, c.paramc = 3))
  expect_equal(pscol2$values, list(a.b.parama = 1, a.b.paramb = 2, a.c.paramc = 3))

  expect_error(ParamSetCollection$new(list(pscol1, pstest)),
    "duplicated parameter.* a\\.c\\.paramc")
})

test_that("cloning and changing underlying params works", {
  ps1 = th_paramset_dbl1()
  ps1$set_id = "s1"
  ps2 = th_paramset_full()
  ps2$set_id = "s2"
  ps3 = th_paramset_dbl1()
  ps3$set_id = ""
  psc1 = ParamSetCollection$new(list(ps1, ps2))
  psc2 = ParamSetCollection$new(list(psc1, ps3))

  expect_length(psc1$.__enclos_env__$private$.params, 0)  # has not created .params in any psc
  expect_length(psc2$.__enclos_env__$private$.params, 0)

  expect_equal(psc2$params_unid[1], list(s1.th_param_dbl = ps1$params[[1]]))
  expect_equal(psc2$params_unid[6], ps3$params[1])
  expect_length(psc1$.__enclos_env__$private$.params, 0)  # has not created .params in psc1
  expect_length(psc2$.__enclos_env__$private$.params, 0)

  pe = th_paramset_dbl1()$params[[1]]$with_id("s1.th_param_dbl")
  expect_equal(psc2$params[1], list(s1.th_param_dbl = pe))

  expect_length(psc1$.__enclos_env__$private$.params, 0)  # has not created .params in psc1
  expect_length(psc2$.__enclos_env__$private$.params, 6)  # but psc2 has .params now

  ps1$params[[1]]$.__enclos_env__$private$.id = "test"
  expect_equal(psc2$params_unid[1], list(s1.th_param_dbl = ps1$params[[1]]))
  expect_equal(psc2$params[1], list(s1.th_param_dbl = pe))

  psc2_clone = psc2$clone(deep = TRUE)

  psc1$remove_sets("s2")
  expect_equal(psc2$params_unid, list(s1.th_param_dbl = ps1$params[[1]], th_param_dbl = ps3$params[[1]]))
  expect_equal(psc2$params, list(s1.th_param_dbl = pe, th_param_dbl = ps3$params[[1]]))

  ps1$add(ParamInt$new("x"))

  expect_equal(psc2$params_unid, list(s1.th_param_dbl = ps1$params[[1]], s1.x = ps1$params[[2]], th_param_dbl = ps3$params[[1]]))
  expect_equal(psc2$params, list(s1.th_param_dbl = pe, s1.x = ParamInt$new("s1.x"), th_param_dbl = ps3$params[[1]]))

  expect_equal(psc2_clone$params_unid[1], list(s1.th_param_dbl = ps1$params[[1]]))
  expect_equal(psc2_clone$params_unid[6], ps3$params[1])
  expect_equal(psc2_clone$params[1], list(s1.th_param_dbl = pe))
  expect_equal(psc2_clone$params[6], ps3$params[1])

})

test_that("tags shadowing works", {
  ps1 = ParamSet$new(list(ParamInt$new("x", tags = c("a", "b")), ParamInt$new("y", tags = c("c"))))
  ps1$set_id = "s1"

  expect_equal(ps1$tags, list(x = c("a", "b"), y = "c"))
  expect_error({ps1$tags$x = 2}, "May only contain.*character.*numeric")
  expect_error({ps1$tags$x = NULL}, "Must be.*identical to.*x,y")
  expect_error({ps1$tags = list(y = "c", x = "a")}, "Must be.*identical to.*x,y")

  ps1$tags$x = "z"

  expect_equal(ps1$params$x$param_tags, c("a", "b"))

  ps2 = th_paramset_full()
  ps2$set_id = "s2"

  ps2$tags$th_param_int = "xx"

  expect_equal(ps2$params$th_param_int$param_tags, character(0))

  ps3 = th_paramset_dbl1()
  ps3$set_id = ""
  psc1 = ParamSetCollection$new(list(ps1, ps2))
  psc2 = ParamSetCollection$new(list(psc1, ps3))

  expect_equal(psc2$tags, list(s1.x = "z", s1.y = "c", s2.th_param_int = "xx",
    s2.th_param_dbl = character(0), s2.th_param_fct = character(0), s2.th_param_lgl = character(0), th_param_dbl = character(0)))

  pscc = psc1$clone(deep = TRUE)
  psc1$tags$s1.y = "d"


  expect_equal(psc2$tags, list(s1.x = "z", s1.y = "c", s2.th_param_int = "xx",
    s2.th_param_dbl = character(0), s2.th_param_fct = character(0), s2.th_param_lgl = character(0), th_param_dbl = character(0)))
  expect_equal(psc1$tags, list(s1.x = "z", s1.y = "d", s2.th_param_int = "xx",
    s2.th_param_dbl = character(0), s2.th_param_fct = character(0), s2.th_param_lgl = character(0)))
  expect_equal(pscc$tags, list(s1.x = "z", s1.y = "c", s2.th_param_int = "xx",
    s2.th_param_dbl = character(0), s2.th_param_fct = character(0), s2.th_param_lgl = character(0)))

  expect_equal(ps2$params$th_param_int$param_tags, character(0))
  expect_equal(ps1$params$x$param_tags, c("a", "b"))

})
