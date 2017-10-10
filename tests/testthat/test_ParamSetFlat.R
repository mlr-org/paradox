context("ParamSetFlat")

test_that("test if ParamSetFlat constructor works", {
  ps = th.paramset.flat.full
  expect_class(ps, "ParamSetFlat")
  expect_equal(ps$ids, c('th.param.int', 'th.param.real', 'th.param.categorical', 'th.param.flag'))
  expect_equal(ps$lower, c(th.param.int=-10, th.param.real=-10, th.param.categorical=NA_real_, th.param.flag=NA_real_))
  expect_equal(ps$upper, c(th.param.int=10, th.param.real=10, th.param.categorical=NA_real_, th.param.flag=NA_real_))
  expect_equal(ps$storage.type, "list")

  sampled = ps$sample(10)
  expect_data_table(sampled, nrows = 10)
  expect_equal(colnames(sampled), ps$ids)
  denormed = ps$denorm(list(th.param.int=runif(5), th.param.real=runif(5), th.param.categorical=runif(5), th.param.flag=runif(5)))
  expect_data_table(denormed, nrows = 5)
  expect_equal(colnames(denormed), ps$ids)
})
