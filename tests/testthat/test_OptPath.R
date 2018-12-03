context("OptPath")

test_that("active bindings works", {
  op = th_opt_path_full()

  # test active bindings
  expect_equal(op$x_names, names(th_paramset_full()$params))
  expect_equal(op$length, 10)
  expect_data_table(op$x)
  expect_equal(colnames(op$x), names(th_paramset_full()$params))
  expect_data_table(op$y)
  expect_equal(op$y$y, 1:10)
  expect_equal(op$dim, 1)

  # sub-setting
  expect_class(op[dob < 6], "OptPath")
  expect_class(op[["timestamp"]], "POSIXct")

  # contents after data.frame conversion
  op_df = as.data.frame(op)
  expect_data_frame(op_df, nrow = 10, ncol = 13)
  expect_set_equal(colnames(op_df), c("dob", "message", "error", "transformed_x", "exec_time", "timestamp", "th_param_int", "th_param_dbl", "th_param_fct", "th_param_lgl", "y", "th_ex1", "th_ex2"))
  expect_equal(op_df$y, 1:10)
  expect_equal(op_df$th_ex1, 1:10)
  expect_class(op_df$timestamp, "POSIXct")
})

test_that("multi-objective with trafos works", {
  op = th_opt_path_multiobjective()

  # test active bindings
  expect_equal(op$x_names, names(th_paramset_repeated()$params))
  expect_equal(op$length, 10)
  expect_data_table(op$x)
  expect_equal(colnames(op$x), names(th_paramset_repeated()$params))
  expect_data_table(op$y)
  expect_equal(op$y$y1.min, 10:1)
  expect_equal(op$y$y2.max, 1:10)
  expect_equal(op$dim, 2)

  # contents after data.frame conversion
  op_df = as.data.frame(op, include_extras = FALSE)
  expect_data_frame(op_df, nrow = 10, ncol = 7 + op$param_set$length + op$dim)
  expect_list(op_df$transformed_x, len = 10)
})

test_that("printer works", {
  op = th_opt_path_full()
  expect_output(print(op), "x = 4")
  expect_output(print(op), "y = 1")
  expect_output(print(op), "Length = 10")
  expect_output(print(op), "errors: 5")
})

