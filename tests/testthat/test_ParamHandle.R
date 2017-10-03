context("ParamHandle")

test_that("test if ParamHandle constructor works", {
  ps = ParamHandle$new(val = 3)
  ps$addMandChild(ParamHandle$new())$addMandChild(ParamHandle$new())
})
