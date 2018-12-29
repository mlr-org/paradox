context("sampling")

test_that("1d samplers: basic tests", {
  samplers = list(
    ParamDbl = list(Sampler1DUnif, Sampler1DDblNorm),
    ParamInt = list(Sampler1DUnif),
    ParamFct = list(Sampler1DUnif, Sampler1DFct),
    ParamLgl = list(Sampler1DUnif, Sampler1DFct)
  )
  ps = th_paramset_full()
  for (p in ps$params) {
    ss = samplers[[p$pclass]]
    for (s in ss) {
      s = s$new(p)
      info = paste(p$id, "-", class(s)[[1L]])
      n = 5L
      x = s$sample(n)
      expect_data_table(x, ncols = 1L, nrows = n, info = info)
      x1 = x[[1]]
      expect_is(x1, p$storage_type, info = info)
      if (p$pclass %in% c("ParamInt", "ParamDbl"))
        expect_true(all(x1 >= p$lower & x <= p$upper), info = info)
      if (p$pclass %in% c("ParamFct"))
        expect_true(all(x1 %in% p$values), info = info)
    }
  }
})

test_that("sampling of unif requires finite bounds", {
  p = ParamInt$new(id = "x", lower = 1)
  s = expect_error(Sampler1DUnif$new(p), "bounded")

  p = ParamDbl$new(id = "x", lower = 1)
  s = expect_error(Sampler1DUnif$new(p), "bounded")
})

test_that("multivariate", {
  p1 = th_param_fct()
  p2 = th_param_dbl()
  s1 = Sampler1DFct$new(p1)
  s2 = Sampler1DUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))
  x = s$sample(20)
  expect_data_table(x, ncols = 2L, nrows = 20L)
  expect_numeric(x$th_param_dbl, lower = -10, upper = 10)
  expect_character(x$th_param_fct)
})

test_that("sampling works", {
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    s = SamplerUnif$new(ps)
    #x = s$sample(10)
    #expect_data_table(x, nrows = 10, any.missing = FALSE)
    #expect_equal(colnames(x), ps$ids)
    #expect_true(all(x[, ps$test(as.list(.SD)), by = seq_len(nrow(x))]$V1))
    #xt = ps$transform(x)
    #expect_data_table(xt, nrows = 10)

    #x = lapply(ps$ids, function(x) runif(10))
    #names(x) = ps$ids
    #xd = ps$qunif(x)
    #expect_data_table(xd, nrows = 10, any.missing = FALSE)
    #expect_equal(colnames(xd), ps$ids)
    # denorm can produce infeasible settings
    # expect_true(all(x[, ps$test(.SD), by = seq_len(nrow(x))]$V1))
    #xdt = ps$transform(xd)
    #expect_data_table(xdt, nrows = 10)
  }
})
