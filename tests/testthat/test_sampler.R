context("sampling")

test_that("1d samplers: basic tests", {
  samplers = list(
    ParamDbl = list(Sampler1DUnif, Sampler1DTruncNorm),
    ParamInt = list(Sampler1DUnif),
    ParamFct = list(Sampler1DUnif, Sampler1DCateg),
    ParamLgl = list(Sampler1DUnif, Sampler1DCateg)
  )
  ps = th_paramset_full()
  for (p in ps$params) {
    ss = samplers[[p$class]]
    for (s in ss) {
      s = s$new(p)
      info = paste(p$id, "-", class(s)[[1L]])
      n = 5L
      x = s$sample(n)
      expect_data_table(x, ncols = 1L, nrows = n, info = info)
      x1 = x[[1]]
      expect_is(x1, p$storage_type, info = info)
      if (p$class %in% c("ParamInt", "ParamDbl"))
        expect_true(all(x1 >= p$lower & x <= p$upper), info = info)
      if (p$class %in% c("ParamFct"))
        expect_true(all(x1 %in% p$values), info = info)
      expect_output(print(s), "Sampler:")
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
  expect_data_table(d, ncols = 2L, nrows = 20L)
  expect_equal(colnames(d), ps$ids())
  expect_numeric(d$th_param_dbl, lower = -10, upper = 10)
  expect_character(d$th_param_fct)
  expect_true(all(map_lgl(transpose(d), ps$test)))
  expect_output(print(s), "Sampler:")
  expect_output(print(s), "Independent comps: 2")
})

test_that("SamplerUnif", {
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric()
  )

  for (ps in ps_list) {
    info = ps$id
    s = SamplerUnif$new(ps)
    # as the ps is constructed in the sampler, we cannot expect ps$id to be the same
    expect_equal(s$param_set$params, ps$params)
    d = s$sample(10)
    expect_data_table(d, nrows = 10, any.missing = FALSE, info = info)
    expect_equal(colnames(d), ps$ids(), info = info)
    expect_true(all(map_lgl(transpose(d), ps$test)), info = info)
    expect_output(print(s), "Sampler:")
    expect_output(print(s), str_collapse(ps$ids()[2])) # check that we at least see an id
  }
})


