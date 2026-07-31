# Extracted from test_ParamSet.R:573

# prequel ----------------------------------------------------------------------
context("ParamSet")

# test -------------------------------------------------------------------------
skip_if_not_installed("knitr")
set = ps(zz = p_uty(default = "ZDEF"), aa = p_uty(default = "ADEF"), mm = p_dbl(0, 1))
described = strsplit(
    rd_info(set, descriptions = c(zz = "Zdesc", aa = "Adesc", mm = "Mdesc")),
    "\n"
  )[[1L]]
