context("sampling")

test_that("sampling 1d unif", {
  p = th_param_dbl()
  s = Sampler1DDblUnif$new(p)
  x = s$sample(20)
  expect_data_table(x, ncols = 1L, nrows = 20L)
  expect_numeric(x$th_param_dbl, lower = -10, upper = 10)
})

test_that("sampling 1d cat", {
  p = th_param_fct()
  s = Sampler1DFct$new(p)
  x = s$sample(20)
  expect_data_table(x, ncols = 1L, nrows = 20L)
  expect_character(x$th_param_fct)
})


test_that("multivariate", {
  p1 = th_param_fct()
  p2 = th_param_dbl()
  s1 = Sampler1DFct$new(p1)
  s2 = Sampler1DDblUnif$new(p2)
  s = SamplerJointIndep$new(list(s1, s2))
  x = s$sample(20)
  expect_data_table(x, ncols = 2L, nrows = 20L)
  expect_numeric(x$th_param_dbl, lower = -10, upper = 10)
  expect_character(x$th_param_fct)
})

test_that("sampling of number requires finite bounds", {
  p = ParamInt$new(id = "x", lower = 1)
  s = expect_error(Sampler1DIntUnif$new(p), "is_bounded")

  p = ParamDbl$new(id = "x", lower = 1)
  s = expect_error(Sampler1DDblUnif$new(p), "is_bounded")
})


test_that("samping works", {
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
