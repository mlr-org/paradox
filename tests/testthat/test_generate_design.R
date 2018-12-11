context("generate_design")

test_that("generate_design_grid", {
  ps_list = list(
    th_paramset_full(),
    # th_paramset_repeated(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    info = ps$id
    print(info)
    d = generate_design_grid(ps, resolution = 3)
    expect_data_table(d, any.missing = FALSE, info = info)
    # xgt = ps$transform(xg)
    # expect_data_table(xgt, nrows = nrow(xg), info = info)

    # p_res = ps$nlevels
    # p_res[is.na(p_res)] = 2
    # xgp = generate_design_grid(ps, param_resolutions = p_res)
    # expect_data_table(xgp, any.missing = FALSE, info = info)
    # expect_true(nrow(xgp) == prod(p_res), info = info)
  }
})


test_that("generate_design_lhs", {
  ps_list = list(
    th_paramset_full(),
    # th_paramset_repeated(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    info = ps$id
    d = generate_design_lhs(ps, 10)
    expect_data_table(d, nrows = 10, any.missing = FALSE, info = info)
    xs = mlr3misc::transpose(d)
    # FIXME: the next test seesm unfinished
    all(map_lgl(xs, ps$test))
    # FIXME: the next lines should not be here, they test transform and design_to_list
    # xlt = ps$transform(xl)
    # expect_data_table(xlt, nrows = 10)
    # xltl = design_to_list(xlt)
    # expect_list(xltl, len = 10)
  }
})

