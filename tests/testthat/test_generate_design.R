context("generate_design")

test_that("generate_design_grid", {
  ps_list = list(
    th_paramset_full(),
    # th_paramset_repeated(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    xg = generate_design_grid(ps, resolution = 3)
    expect_data_table(xg, any.missing = FALSE)
    # expect_equal(nrow(xg), 3^ps$length)
    # expect_true(all(xg[, ps$test(.SD), by = seq_len(nrow(xg))]$V1))
    # xgt = ps$transform(xg)
    # expect_data_table(xgt, nrows = nrow(xg))

    # p_res = ps$nlevels
    # p_res[is.na(p_res)] = 2
    # xgp = generate_design_grid(ps, param_resolutions = p_res)
    # expect_data_table(xgp, any.missing = FALSE)
    # expect_true(nrow(xgp) == prod(p_res))
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
    d = generate_design_lhs(ps, 10)
    expect_data_table(d, nrows = 10, any.missing = FALSE)
    xs = design_to_list(d)
    all(map_lgl(xs, ps$test))
    # FIXME: the next lines should not be here, they test transform and design_to_list
    # xlt = ps$transform(xl)
    # expect_data_table(xlt, nrows = 10)
    # xltl = design_to_list(xlt)
    # expect_list(xltl, len = 10)
  }
})

