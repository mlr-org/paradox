context("ParamSimple")

test_that("special.vals work for all ancestors of ParamSimple", {
  param.ancestors = list(ParamCategorical, ParamFlag, ParamInt, ParamReal)
  special.vals.list = list(
    list(1), 
    list('a'), 
    list(1:10), 
    list('a', 1, 1:10, as.environment(list(a = 10, b = 100, c = mean))),
    list(mean, sum, function(x) x^10))
  for (param in param.ancestors) {
    for (special.vals in special.vals.list) {
      if (param$classname == "ParamCategorical") {
        po = param$new(id = paste0('test.', param$classname), special.vals = special.vals, values = letters[11:20])
      } else {
        po = param$new(id = paste0('test.', param$classname), special.vals = special.vals)  
      }
      for (special.val in special.vals) {
        expect_true(po$test(special.val))
        expect_false(po$test('never valid'))
        expect_false(po$test(NA))
        expect_false(po$test(NULL))
      }
    }
  }
})


