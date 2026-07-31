# Extracted from test_ParamSet.R:350

# prequel ----------------------------------------------------------------------
context("ParamSet")

# test -------------------------------------------------------------------------
ps = ParamSet_legacy$new(list(
    ParamDbl$new(id = "x", lower = 1, tags = c("t1")),
    ParamInt$new(id = "y", lower = 1, upper = 2),
    ParamFct$new(id = "z", levels = letters[1:3], tags = c("t1"))
  ))
expect_equal(ps$get_values(), named_list())
