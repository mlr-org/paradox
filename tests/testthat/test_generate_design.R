context("generate_design")

test_that("generate_design_random", {
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    info = ps$id
    d = generate_design_random(ps, n = 5L)
    expect_data_table(d, any.missing = FALSE, nrow = 5L, ncol = ps$length, info = info)
    expect_true(all(map_lgl(transpose(d), ps$test)), info = info) # check that all rows are feasible
  }
})

test_that("generate_design_grid", {
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    info = ps$id
    reso = 3L
    d = generate_design_grid(ps, resolution = reso)
    expect_data_table(d, any.missing = FALSE, info = info)
    # compute length of design as product of resolution (for all numbers) * product of nlevels
    nrows = ps$nlevels
    nrows[ps$is_number] = reso
    nrows = prod(nrows)
    expect_equal(nrow(d), nrows, info = info)
    expect_true(all(map_lgl(transpose(d), ps$test)), info = info) # check that all rows are feasible
  }
})

test_that("check generate_design_grid against concrete expectation", {

  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3),
    ParamFct$new("y", values = c("a", "b"))
  ))
  d = generate_design_grid(ps, resolution = 3)
  expect_equal(d, data.table(x = c(1, 1, 2, 2, 3, 3), y = c("a", "b", "a", "b", "a", "b")))
})


test_that("generate_design_lhs", {
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    info = ps$id
    d = generate_design_lhs(ps, 10)
    expect_data_table(d, nrows = 10, any.missing = FALSE, info = info)
    xs = mlr3misc::transpose(d)
    expect_true(all(map_lgl(xs, ps$test)), info = info)
  }
})

test_that("generate_design does not work with deps", {
  ps = th_paramset_deps()
  expect_error(generate_design_grid(ps, resolution = 2), "dependencies")
  expect_error(generate_design_random(ps, n = 5L), "dependencies")
  expect_error(generate_design_lhs(ps, n = 5L), "dependencies")
})

