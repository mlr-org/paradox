context("Param")


test_that("basic properties", {
  p1 = ParamDbl$new("x", default = 4)
  p2 = ParamFct$new("y", values = c("a", "b"))
  expect_true(p1$has_default)
  expect_false(p2$has_default)
  expect_true(p1$is_number)
  expect_false(p2$is_number)
  expect_false(p1$is_categ)
  expect_true(p2$is_categ)
})

test_that("check and assert work", {
  # test-funcion should be tested in individual test_Param<type> files
  # here we briefly check all 3 to see if they work in principle
  p = ParamDbl$new("x", lower = 1, upper = 2)
  p$assert(1)
  expect_error(p$assert(3))
  expect_true(p$check(1))
  expect_string(p$check(3), fixed = "<= 2")
  expect_true(p$test(1))
  expect_false(p$test(3))
})


test_that("special_vals work for all Param subclasses", {
  class = list(ParamFct, ParamLgl, ParamInt, ParamDbl)
  special_vals_list = list(
    list(1),
    list('a'),
    list(1:10),
    list('a', 1, 1:10, as.environment(list(a = 10, b = 100, c = mean))),
    list(mean, sum, function(x) x^10)
  )
  for (cl in class) {
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
  expect_error(ParamInt$new(id = "$foo") , "Must comply")
})

test_that("printer works", {
  for (p in th_paramset_full()$params) {
    info = p$id
    s = capture_output(print(p))
    expect_true(grepl(p$id, s, fixed = TRUE), info = info)
    expect_true(grepl(class(p)[1L], s, fixed = TRUE), info = info)
  }
})
