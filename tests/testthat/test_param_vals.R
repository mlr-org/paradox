context("values")

test_that("values", {
  ps = ParamSet$new(list(
    ParamDbl$new(id = "d", lower = 0, upper = 1),
    ParamInt$new(id = "i", lower = 1, upper = 3),
    ParamFct$new(id = "f", levels = letters[1:3])
  ))
  # make sure we accept empty list, and not only a "named list"
  ps$values = list()
  expect_equal(ps$values, named_list())
  ps$values = list(d = 1, f = "a")
  expect_true(ps$check(list(d = 0, f = "a")))
  ps2 = ps$clone()
  ps2$subset(ids = c("d", "i"))
  expect_equal(ps2$values, list(d = 1))
  ps2$values = list(d = 0.5)
  expect_true(ps$check(list(d = 1, f = "a")))
  expect_equal(ps2$values, list(d = 0.5))
  # check printer
  expect_output(print(ps2), "d.*<NoDefault>.*0.5")

  ps2 = ps$clone()
  ps2$subset(ids = c("i"))
  expect_equal(ps2$values, set_names(list(), character(0)))

  ps3 = ParamSet$new(list(
    ParamDbl$new(id = "x", lower = 0, upper = 9)
  ))
  ps3$values = list(x = 7)
  ps2 = ps$clone()
  ps2$add(ps3)
  expect_equal(ps2$values, list(d = 1, f = "a", x = 7))

  # designs
  ps$values = list(f = "a")
  d = generate_design_grid(ps, resolution = 3)
  dd = d$data
  expect_data_table(dd, nrows = 9, ncols = 3)
  expect_true(all(dd$f == "a"))

  d = generate_design_random(ps, n = 100)
  dd = d$data
  expect_data_table(dd, nrows = 100, ncols = 3)
  expect_true(all(dd$f == "a"))

  d = generate_design_lhs(ps, n = 10)
  dd = d$data
  expect_data_table(dd, nrows = 10, ncols = 3)
  expect_true(all(dd$f == "a"))

  # sampler
  s = SamplerUnif$new(ps)
  d = s$sample(9)
  dd = d$data
  expect_data_table(dd, nrows = 9, ncols = 3)
  expect_true(all(dd$f == "a"))
})

test_that("values calls assert", {
  # most of the tests should be done for ParamSet$check, so we simply
  # check here, that paramvals calls assert
  ps = ParamSet$new(list(
    ParamDbl$new(id = "d", lower = 0, upper = 1),
    ParamInt$new(id = "i", lower = 1, upper = 3),
    ParamFct$new(id = "f", levels = letters[1:3])
  ))
  expect_error(ps$values <- list(xxx = 1), "not available")
  expect_error(ps$values <- list(d = 9), "not <= 1")
})

test_that("required params are checked", {
  ps = ParamSet$new(list(
    ParamDbl$new(id = "d", lower = 0, upper = 1, tags = "required"),
    ParamInt$new(id = "i", lower = 1, upper = 3)
  ))
  expect_error(ps$values <- list(i = 2), "Missing required")
})


