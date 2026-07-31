# Extracted from test_ParamSet.R:559

# prequel ----------------------------------------------------------------------
context("ParamSet")

# test -------------------------------------------------------------------------
skip_if_not_installed("knitr")
ps = ParamSet_legacy$new()
expect_character(rd_info(ps), pattern = "empty", ignore.case = TRUE)
