context("Interface")

test_that("interface use case works", {
  ps_orig = ParamSet$new(list(ParamInt$new("mtry", 0)))
  ps = ps_orig$clone()
  ps$subset(setdiff(ps$ids(), "mtry"))
  ps$add(ParamDbl$new("mtry.pexp", 0, 1))
  ps$trafo = function(x, env) {
    x$mtry = round(env$task$ncol ^ x$mtry.pexp)
    x$mtry.pexp = NULL
    x
  }

  ps_orig$values$mtry = 3
  ps_orig$add_interface(ps)
  ps_orig$values$mtry.pexp = 0.7
  expect_error({ps_orig$values$mtry = 3})

  expect_equal(ps_orig$get_values(), list(mtry.pexp = 0.7))
  expect_equal(ps_orig$get_values(learnerside = TRUE,
    env = list(task = list(ncol = 200))), list(mtry = round(200^0.7)))
})
