context("ParamSetFlat")

test_that("test if ParamSetFlat constructor works", {
  ps = th.paramset.flat.full
  expect_class(ps, "ParamSetFlat")
  expect_equal(ps$ids, c('th.param.int', 'th.param.real', 'th.param.factor', 'th.param.flag'))
  expect_equal(ps$lower, c(th.param.int=-10, th.param.real=-10, th.param.factor=NA_real_, th.param.flag=NA_real_))
  expect_equal(ps$upper, c(th.param.int=10, th.param.real=10, th.param.factor=NA_real_, th.param.flag=NA_real_))
  expect_equal(ps$type, "list")

  sampled = ps$sample(10)
  expect_data_table(sampled, nrows = 10)
  expect_equal(colnames(sampled), ps$ids)
  denormed = ps$denorm(list(x = runif(5), y = runif(5), c = runif(5)))
  expect_data_table(denormed, nrows = 5)
  expect_equal(colnames(denormed), ps$ids)
  ps$toString()
})
