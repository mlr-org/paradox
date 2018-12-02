context("ParamBase")

test_that("special_vals work for all ancestors of ParamBase", {
  param_ancestors = list(ParamCategorical, ParamFlag, ParamInt, ParamFloat)
  special_vals_list = list(
    list(1),
    list('a'),
    list(1:10),
    list('a', 1, 1:10, as.environment(list(a = 10, b = 100, c = mean))),
    list(mean, sum, function(x) x^10))
  for (param in param_ancestors) {
    for (special_vals in special_vals_list) {
      if (param$classname == "ParamCategorical") {
        po = param$new(id = paste0('test.', param$classname), special_vals = special_vals, values = letters[11:20])
      } else {
        po = param$new(id = paste0('test.', param$classname), special_vals = special_vals)
      }
      for (special_val in special_vals) {
        expect_true(po$test(special_val))
        expect_false(po$test('never valid'))
        expect_false(po$test(NA))
        expect_false(po$test(NULL))
      }
    }
  }
})


