context("generate_design")

test_that("generate_design_grid", {
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_restricted(),
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
    th_paramset_repeated(),
    th_paramset_restricted(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    xl = generate_design_lhs(ps, 10)
    expect_data_table(xl, nrows = 10, any.missing = FALSE)
    expect_true(all(xl[, ps$test(.SD), by = seq_len(nrow(xl))]$V1))
    xlt = ps$transform(xl)
    expect_data_table(xlt, nrows = 10)
    xltl = design_to_list(xlt)
    expect_list(xltl, len = 10)
  }
})

