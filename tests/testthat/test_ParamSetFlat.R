context("ParamSetFlat")

test_that("test if ParamSetFlat constructor works", {
  params = list(
    ParamInt$new('x', lower = 0, upper = 10),
    ParamReal$new('y', lower = -1, upper = 5, default = 2.5),
    ParamFactor$new('c', values = c('a','b'))
  )
  ps = ParamSetFlat$new(id = "flatParamSetEx", params = params)
  expect_class(ps, "ParamSetFlat")
  ps$ids
  expect_equal(ps$ids, c('x', 'y', 'c'))
  expect_equal(ps$lower, c(x=0, y=-1, c=NA_real_))
  expect_equal(ps$upper, c(x=10, y=5, c=NA_real_))
  expect_equal(ps$type, "list")

  sampled = ps$sample(10)
  expect_data_table(sampled, nrows = 10)
  expect_equal(colnames(sampled), ps$ids)
  denormed = ps$denorm(list(x = runif(5), y = runif(5), c = runif(5)))
  expect_data_table(denormed, nrows = 5)
  expect_equal(colnames(denormed), ps$ids)
  ps$toString()
})

