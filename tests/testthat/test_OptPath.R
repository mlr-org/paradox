context("OptPath")

test_that("active bindings works", {
  op = th.opt.path.full

  # test active bindings
  expect_equal(op$x.names, names(th.paramset.flat.full$params))
  expect_equal(op$length, 10)
  expect_data_table(op$x)
  expect_equal(colnames(op$x), names(th.paramset.flat.full$params))
  expect_data_table(op$y)
  expect_equal(op$y$y, 1:10)
  expect_equal(op$dim, 1)

  # sub-setting
  expect_data_table(op[dob < 6], nrows = 5)
  expect_class(op[["timestamp"]], "POSIXct")

  # contents after data.frame conversion
  op.df = as.data.frame(op)
  expect_data_frame(op.df, nrow = 10, ncol = 13)
  expect_set_equal(colnames(op.df), c("dob", "message", "error", "exec.time", "timestamp", "th.param.int", "th.param.real", "th.param.categorical", "th.param.flag", "y", 'th.ex1', 'th.ex2'))
  expect_equal(op.df$y, 1:10)
  expect_equal(op.df$th.ex1, 1:10)
  expect_class(op.df$timestamp, "POSIXct")
})

test_that("multi-objective with trafos works", {
  op = th.opt.path.multiobjective

  # test active bindings
  expect_equal(op$x.names, names(th.paramset.flat.collection$params))
  expect_equal(op$length, 10)
  expect_data_table(op$x)
  expect_equal(colnames(op$x), names(th.paramset.flat.collection$params))
  expect_data_table(op$y)
  expect_equal(op$y$y1.min, 10:1)
  expect_equal(op$y$y2.max, 1:10)
  expect_equal(op$dim, 2)

  # contents after data.frame conversion
  op.df = as.data.frame(op)
  expect_data_frame(op.df, nrow = 10, ncol = 21)
  expect_list(op.df$transformed.x, len = 10)
})


