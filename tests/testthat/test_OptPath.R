context("OptPath")

test_that("test if OptPath works", {
  op = th.opt.path.full

  if (FALSE) {
  # test active bindings
  expect_equal(op$x.names, names(th.paramset.flat.full$params))
  expect_equal(op$length, 10)
  expect_data_table(op$x)
  expect_equal(colnames(op$x), names(th.paramset.flat.full$params))
  expect_data_table(op$y)
  expect_equal(op$y$y, 1:10)
  expect_equal(op$dim, 1)

  # contents
  op.df = as.data.frame(op)
  expect_set_equal(colnames(op.df), c("dob", "message", "error", "exec.time", "timestamp", "th.param.int", "th.param.real", "th.param.factor", "th.param.logical", "th.param.int.trafo", "th.param.real.trafo", "th.param.factor.trafo", "y", 'th.ex1', 'th.ex2'))
  expect_equal(op.df$y, 1:10)
  expect_class(op.df$timestamp, "POSIXct")
  }

  
})


