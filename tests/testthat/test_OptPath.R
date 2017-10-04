context("OptPath")

test_that("test if OptPath works", {
  op = th.opt.path.full

  # test active bindings
  expect_equal(op$x.names, names(th.paramset.flat.full$params))
  expect_equal(op$length, 10)
  expect_data_table(op$x)
  op.df = as.data.frame(op)
  expect_equal(colnames(op$x), names(th.paramset.flat.full$params))
  expect_data_table(op$y)
  expect_equal(op$y$y, 1:10)
  expect_equal(op$dim, 1)
})


