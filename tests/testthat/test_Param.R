context("Param")


test_that("basic properties", {
  p1 = ParamDbl$new("x", default = 4)
  p2 = ParamFct$new("y", levels = c("a", "b"))
  expect_equal(p1$default, list(x = 4))
  expect_equal(p2$default, named_list())
  expect_true(p1$is_number)
  expect_false(p2$is_number)
  expect_false(p1$is_categ)
  expect_true(p2$is_categ)
})

test_that("check and assert work", {
  # test-funcion should be tested in individual test_Param<type> files
  # here we briefly check all 3 to see if they work in principle
  p = ParamDbl$new("x", lower = 1, upper = 2)
  p$assert(list(x = 1))
  expect_error(p$assert(list(x = 3)))
  expect_true(p$check(list(x = 1)))
  expect_string(p$check(list(x = 3)), fixed = "<= 2")
  expect_true(p$test(list(x = 1)))
  expect_false(p$test(list(x = 3)))
})


test_that("special_vals work for all Param subclasses", {
  class = list(ParamFct, ParamLgl, ParamInt, ParamDbl)
  special_vals_list = list(
    list(1),
    list("a"),
    list(1:10),
    list("a", 1, 1:10, as.environment(list(a = 10, b = 100, c = mean))),
    list(mean, sum, function(x) x^10)
  )
  for (cl in class) {
    for (special_vals in special_vals_list) {
      if (cl$classname == "ParamFct") {
        p = cl$new(id = paste0("test.", cl$classname), special_vals = special_vals, levels = letters[11:20])
      } else {
        p = cl$new(id = paste0("test.", cl$classname), special_vals = special_vals)
      }
      for (special_val in special_vals) {
        expect_true(p$test(set_names(list(special_val), paste0("test.", cl$classname))))
        expect_false(p$test(set_names(list("never valid"), paste0("test.", cl$classname))))
        expect_false(p$test(set_names(list(NA), paste0("test.", cl$classname))))
        expect_false(p$test(set_names(list(NULL), paste0("test.", cl$classname))))
      }
    }
  }
})

test_that("we cannot create Params with non-strict R names", {
  expect_error(ParamInt$new(id = "$foo"), "does not comply")
})

test_that("printer works", {
  for (p in th_paramset_full()$params) {
    info = p$id
    s = capture_output(print(p))
    expect_true(grepl(p$id, s, fixed = TRUE), info = info)
    expect_true(grepl(class(p)[1L], s, fixed = TRUE), info = info)
  }
})
