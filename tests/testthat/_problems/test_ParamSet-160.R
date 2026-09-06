# Extracted from test_ParamSet.R:160

# prequel ----------------------------------------------------------------------
context("ParamSet")

# test -------------------------------------------------------------------------
ps = ParamSet$new()
expect_r6(ps, "ParamSet")
expect_equal(ps$length, 0)
expect_equal(ps$ids(), character(0L))
expect_equal(ps$lower, set_names(numeric(0L), character(0L)))
