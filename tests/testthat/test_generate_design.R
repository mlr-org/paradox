context("generate_design")

test_that("generate_design_random", {
  ps_list = list(
    th_paramset_dbl1(),
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric()
  )

  for (ps in ps_list) {
    info = ps$set_id
    d = generate_design_random(ps, n = 5L)
    dd = d$data
    expect_data_table(dd, any.missing = FALSE, nrows = 5L, ncols = ps$length, info = info)
    expect_true(all(map_lgl(d$transpose(), ps$test)), info = info) # check that all rows are feasible
  }
})

test_that("generate_design_grid", {
  ps_list = list(
    th_paramset_dbl1(),
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric()
  )

  for (ps in ps_list) {
    info = ps$set_id
    reso = 3L
    d = generate_design_grid(ps, resolution = reso)
    dd = d$data
    expect_data_table(d$data, any.missing = FALSE, info = info)
    # compute length of design as product of resolution (for all numbers) * product of nlevels
    nrows = ps$nlevels
    nrows[ps$is_number] = reso
    nrows = prod(nrows)
    expect_equal(nrow(dd), nrows, info = info)
    expect_true(all(map_lgl(d$transpose(), ps$test)), info = info) # check that all rows are feasible
  }
})

test_that("generate_design_grid with different resolutions and egde cases", {
  ps = ParamSet$new(list(ParamFct$new("f", levels = letters[1:2])))
  d = generate_design_grid(ps)
  expect_data_table(d$data, any.missing = FALSE, nrows = 2, ncols = 1)

  ps = ParamSet$new(list(
    ParamFct$new("f", levels = letters[1:2]),
    ParamDbl$new("d", lower = 0, upper = 1)
  ))
  d = generate_design_grid(ps, param_resolutions = c(d = 3))
  expect_data_table(d$data, any.missing = FALSE, nrows = 6, ncols = 2)

  ps = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamInt$new("y", lower = 0, upper = 10)
  ))
  d = generate_design_grid(ps, resolution = 2, param_resolutions = c(y = 3))
  dd = d$data
  expect_data_table(dd, any.missing = FALSE, nrows = 6, ncols = 2)
  expect_equal(length(unique(dd$x)), 2)
  expect_equal(length(unique(dd$y)), 3)

  d = generate_design_grid(ps, resolution = 2, param_resolutions = c(x = 4, y = 3))
  dd = d$data
  expect_data_table(dd, any.missing = FALSE, nrows = 12, ncols = 2)
  expect_equal(length(unique(dd$x)), 4)
  expect_equal(length(unique(dd$y)), 3)

  ps = ParamSet$new(list(
    ParamInt$new("x", lower = 0, upper = 10),
    ParamInt$new("y", lower = 0, upper = 10),
    ParamLgl$new("z")
  ))
  d = generate_design_grid(ps, resolution = 2, param_resolutions = c(y = 3))
  dd = d$data
  expect_data_table(dd, any.missing = FALSE, nrows = 12, ncols = 3)
  expect_equal(length(unique(dd$x)), 2)
  expect_equal(length(unique(dd$y)), 3)
  expect_equal(length(unique(dd$z)), 2)
})

test_that("check generate_design_grid against concrete expectation", {
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3),
    ParamFct$new("y", levels = c("a", "b"))
  ))
  d = generate_design_grid(ps, resolution = 3)
  expect_equal(d$data, data.table(x = c(1, 1, 2, 2, 3, 3), y = c("a", "b", "a", "b", "a", "b")))
})


test_that("generate_design_lhs", {
  ps_list = list(
    th_paramset_dbl1(),
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric()
  )

  for (ps in ps_list) {
    info = ps$set_id
    d = generate_design_lhs(ps, 10)
    dd = d$data
    expect_data_table(d$data, nrows = 10, any.missing = FALSE, info = info)
    expect_true(all(map_lgl(d$transpose(), ps$test)), info = info)
  }
})

test_that("generate_design_lhs does not work with deps", {
  ps = th_paramset_deps()
  expect_error(generate_design_lhs(ps, n = 5L), "dependencies")
})

#
test_that("generate_design_random and grid works with deps", {
  # call is a very lightweight wrapper around Sampler, so we test it briefly and test SamplerUnif in its test-file
  ps = th_paramset_deps()

  d = generate_design_random(ps, n = 5L)
  dd = d$data
  expect_data_table(dd, nrows = 5L, ncols = 4L)
  expect_names(names(dd), permutation.of = c("th_param_int", "th_param_dbl", "th_param_lgl", "th_param_fct"))

  d = generate_design_grid(ps, resolution = 4L)
  dd = d$data
  # 4 (i) * ( (TRUE) (2 * 4 + 1) + (FALSE) (1))
  expect_data_table(dd, nrows = 40L, ncols = 4L)
  expect_names(names(dd), permutation.of = c("th_param_int", "th_param_dbl", "th_param_lgl", "th_param_fct"))
  expect_true(all(ifelse(dd$th_param_fct %in% c("a", "b", NA) & dd$th_param_lgl,
    !is.na(dd$th_param_dbl), is.na(dd$th_param_dbl))))
})

test_that("generate_design_random with zero rows", {
  ps = th_paramset_full()
  d = generate_design_random(ps, n = 0)
  expect_data_table(d$data, any.missing = FALSE, nrows = 0, ncols = ps$length, info = ps$set_id)
})

test_that("generate_design_lhs with zero rows", {
  ps = th_paramset_full()
  d = generate_design_lhs(ps, n = 0)
  expect_data_table(d$data, any.missing = FALSE, nrows = 0, ncols = ps$length, info = ps$set_id)
})

test_that("generate_design_grid with zero rows", {
  ps = th_paramset_full()
  d = generate_design_grid(ps, resolution = 0)
  expect_data_table(d$data, any.missing = FALSE, nrows = 0, ncols = ps$length, info = ps$set_id)
})

