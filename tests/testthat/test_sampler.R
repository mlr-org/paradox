context("sampling")

test_that("1d samplers: basic tests", {
  samplers = list(
    ParamDbl = list(Sampler1DUnif, Sampler1DNormal),
    ParamInt = list(Sampler1DUnif),
    ParamFct = list(Sampler1DUnif, Sampler1DCateg),
    ParamLgl = list(Sampler1DUnif, Sampler1DCateg)
  )
  ps = th_paramset_full()
  for (p in ps$params) {
    ss = samplers[[p$class]]
    for (s in ss) {
      expect_error(s$new(ps()), "exactly 1 Param, but contains 0")
      expect_error(s$new(ps(x = p, y = p)), "exactly 1 Param, but contains 2")
      expect_class(s$new(ps(x = p)), "Sampler1D")
      s = s$new(p)
      info = paste(p$id, "-", class(s)[[1L]])
      n = 5L
      x = s$sample(n)
      d = x$data
      expect_data_table(d, ncols = 1L, nrows = n, info = info)
      d1 = d[[1]]
      expect_is(d1, p$storage_type, info = info)
      if (p$class %in% c("ParamInt", "ParamDbl")) {
        expect_true(all(d1 >= p$lower & d1 <= p$upper), info = info)
      }
      if (p$class %in% c("ParamFct")) {
        expect_true(all(d1 %in% p$levels), info = info)
      }
      expect_output(print(s), "<Sampler")
    }
  }
})

test_that("sampling of unif requires finite bounds", {
  p = ParamInt$new(id = "x", lower = 1)
  s = expect_error(Sampler1DUnif$new(p), "bounded")

  p = ParamDbl$new(id = "x", lower = 1)
  s = expect_error(Sampler1DUnif$new(p), "bounded")
})

test_that("SamplerJointIndep", {
  p1 = th_param_fct()
  p2 = th_param_dbl()
  ps = ParamSet$new(list(p1, p2))
  s1 = Sampler1DCateg$new(p1)
  s2 = Sampler1DUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))
  # as the ps is constructed in the sampler, we cannot expect ps$id to be the same
  expect_equal(s$param_set$params, ps$params)
  d = s$sample(20)
  dd = d$data
  expect_data_table(dd, ncols = 2L, nrows = 20L)
  expect_equal(colnames(dd), ps$ids())
  expect_numeric(dd$th_param_dbl, lower = -10, upper = 10)
  expect_character(dd$th_param_fct)
  expect_true(all(map_lgl(d$transpose(), ps$test)))
  expect_output(print(s), "<SamplerJointIndep>")
  expect_output(print(s), "Independent comps: 2")
})

test_that("SamplerUnif", {
  ps_list = list(
    th_paramset_dbl1(),
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric()
  )

  for (ps in ps_list) {
    info = ps$set_id
    s = SamplerUnif$new(ps)
    # as the ps is constructed in the sampler, we cannot expect ps$id to be the same
    expect_equal(s$param_set$params, ps$params)
    d = s$sample(10)
    dd = d$data
    expect_data_table(dd, nrows = 10, any.missing = FALSE, info = info)
    expect_equal(colnames(dd), ps$ids(), info = info)
    expect_true(all(map_lgl(d$transpose(), ps$test)), info = info)
    expect_output(print(s), "<SamplerUnif>")
    expect_output(print(s), str_collapse(ps$ids()[1])) # check that we at least see an id
  }
})

test_that("SamplerUnif works with deps", {
  ps = th_paramset_deps()
  s = SamplerUnif$new(ps)
  d = s$sample(1000)
  dd = d$data
  expect_data_table(dd, nrows = 1000, ncols = 4L, any.missing = TRUE)
  expect_true(anyNA(dd))
  expect_true(all((dd$th_param_fct %in% c("c", NA_character_) & is.na(dd$th_param_dbl))
  | (dd$th_param_fct %in% c("a", "b") & !is.na(dd$th_param_dbl))))
  expect_names(names(dd), permutation.of = c("th_param_int", "th_param_dbl", "th_param_lgl", "th_param_fct"))
  expect_true(all(map_lgl(d$transpose(filter_na = TRUE), ps$test)))
})

test_that("we had a bug where creating the joint sampler changed the ps-ref of the 1d samplers", {
  p1 = th_param_fct()
  p2 = th_param_dbl()
  ps = ParamSet$new(list(p1, p2))
  s1 = Sampler1DCateg$new(p1)
  s2 = Sampler1DUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))

  s1_expected = ParamSet$new(list(th_param_fct()))
  s1_expected$params
  s1$param_set$params
  expect_equal(s1$param_set, s1_expected)

  s2_expected = ParamSet$new(list(th_param_dbl()))
  s2_expected$params
  s2_expected$tags
  s2$param_set$params
  s2$param_set$tags
  expect_equal(s2$param_set, s2_expected)
})

test_that("Sampler1DRfun with 0 samples (#338)", {
  s = Sampler1DRfun$new(param = ParamDbl$new("x", 0, 10), rfun = function(n) numeric(0))
  x = s$sample(0)
  expect_data_table(x$data, nrows = 0L, ncols = 1L)
})
