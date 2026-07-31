# Extracted from test_ParamSet.R:80

# prequel ----------------------------------------------------------------------
context("ParamSet")

# test -------------------------------------------------------------------------
ps_list = list(
    th_paramset_dbl1(),
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_untyped(),
    th_paramset_numeric()
  )
for (ps in ps_list) {
    info = str_collapse(ps$class)
    expect_class(ps, "ParamSet", info = info)
    expect_int(ps$length, lower = 0L, info = info)
    expect_character(ps$ids(), info = info)
    expect_character(ps$class, info = info)
    expect_names(names(ps$class), identical.to = ps$ids(), info = info)
    expect_character(ps$storage_type, info = info)
    expect_names(names(ps$storage_type), identical.to = ps$ids(), info = info)
    expect_numeric(ps$lower, any.missing = TRUE, info = info)
    expect_names(names(ps$lower), identical.to = ps$ids(), info = info)
    expect_numeric(ps$upper, any.missing = TRUE, info = info)
    expect_names(names(ps$upper), identical.to = ps$ids(), info = info)
    expect_list(ps$levels, info = info)
    expect_names(names(ps$levels), identical.to = ps$ids(), info = info)
    expect_logical(ps$is_bounded, any.missing = FALSE, info = info)
    expect_names(names(ps$is_bounded), identical.to = ps$ids(), info = info)
    expect_flag(ps$all_bounded, info = info)
    expect_numeric(ps$nlevels, any.missing = FALSE, lower = 1, info = info)
    expect_list(ps$tags, types = "character", info = info)
    expect_names(names(ps$tags), identical.to = ps$ids(), info = info)
    expect_list(ps$default, names = "strict", info = info)
    expect_names(names(ps$default), subset.of = ps$ids(), info = info)
  }
