context("Parameter")

test_that("special_vals work for all Parameter subclasses", {
  pclasses = list(ParamFct, ParamLgl, ParamInt, ParamDbl)
  special_vals_list = list(
    list(1),
    list('a'),
    list(1:10),
    list('a', 1, 1:10, as.environment(list(a = 10, b = 100, c = mean))),
    list(mean, sum, function(x) x^10)
  )
  for (cl in pclasses) {
    for (special_vals in special_vals_list) {
      if (cl$classname == "ParamFct") {
        p = cl$new(id = paste0('test.', cl$classname), special_vals = special_vals, values = letters[11:20])
      } else {
        p = cl$new(id = paste0('test.', cl$classname), special_vals = special_vals)
      }
      for (special_val in special_vals) {
        expect_true(p$test(special_val))
        expect_false(p$test('never valid'))
        expect_false(p$test(NA))
        expect_false(p$test(NULL))
      }
    }
  }
})

test_that("we cannot create Params with non-strict R names", {
  expect_error(ParamInt$new(id = "$foo") , "naming convention")
})

