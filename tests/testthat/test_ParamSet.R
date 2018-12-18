context("ParamSet")

test_that("methods and active bindings work", {
  ps_list = list(
    th_paramset_empty(),
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_untyped(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )
  for (ps in ps_list) {
    info = ps$id
    if (ps$id == "th_paramset_full") {
      expect_equal(ps$ids, c('th_param_int', 'th_param_dbl', 'th_param_fct', 'th_param_lgl'))
      expect_equal(ps$lowers, c(th_param_int=-10, th_param_dbl=-10, th_param_fct=NA_real_, th_param_lgl=NA_real_))
      expect_equal(ps$uppers, c(th_param_int=10, th_param_dbl=10, th_param_fct=NA_real_, th_param_lgl=NA_real_))
    }
    expect_class(ps, "ParamSet", info = info)
    expect_int(ps$length, lower = 0L, info = info)
    expect_character(ps$ids, info = info)
    expect_character(ps$pclasses, info = info)
    expect_names(names(ps$pclasses), identical.to = ps$ids, info = info)
    expect_character(ps$storage_types, info = info)
    expect_names(names(ps$storage_types), identical.to = ps$ids, info = info)
    expect_numeric(ps$lowers, any.missing = TRUE, info = info)
    expect_names(names(ps$lowers), identical.to = ps$ids, info = info)
    expect_numeric(ps$uppers, any.missing = TRUE, info = info)
    expect_names(names(ps$uppers), identical.to = ps$ids, info = info)
    expect_list(ps$values, info = info)
    expect_names(names(ps$values), identical.to = ps$ids, info = info)
    if (ps$id == "th_paramset_untyped") {
      expect_error(ps$is_bounded, regexp = "undefined", info = info)
    } else {
      expect_flag(ps$is_bounded, info = info)
    }
    expect_integer(ps$nlevels, any.missing = TRUE, info = info)
    expect_list(ps$tags, names = "strict", any.missing = TRUE, info = info)
    expect_list(ps$defaults, names = "strict", any.missing = TRUE, info = info)
    expect_output(print(ps), "ParamSet:", info = info)
    expect_true(ps$check(list()))
  }
})

test_that("advanced methods work", {
  ps_list = list(
    th_paramset_full(),
    th_paramset_repeated(),
    th_paramset_numeric(),
    th_paramset_trafo()
  )

  for (ps in ps_list) {
    s = SamplerUnif$new(ps)
    x = s$sample(10)
    expect_data_table(x, nrows = 10, any.missing = FALSE)
    expect_equal(colnames(x), ps$ids)
    expect_true(all(x[, ps$test(as.list(.SD)), by = seq_len(nrow(x))]$V1))
    xt = ps$transform(x)
    expect_data_table(xt, nrows = 10)

    x = lapply(ps$ids, function(x) runif(10))
    names(x) = ps$ids
    xd = ps$map_unitint_to_values(x)
    expect_data_table(xd, nrows = 10, any.missing = FALSE)
    expect_equal(colnames(xd), ps$ids)
    # denorm can produce infeasible settings
    # expect_true(all(x[, ps$test(.SD), by = seq_len(nrow(x))]$V1))
    xdt = ps$transform(xd)
    expect_data_table(xdt, nrows = 10)
  }
})


# test_that("param subset in ParamSet works", {
#   # Define all the different subsets we want to try:
#   configs = list(
#     list(
#       ps = th_paramset_full(),
#       ids = c("th_param_int", "th_param_lgl"),
#       expected_ids = c("th_param_int", "th_param_lgl"),
#       fix = NULL
#     ),
#     list(
#       ps = th_paramset_full(),
#       expected_ids = c("th_param_dbl", "th_param_fct", "th_param_lgl"),
#       fix = list("th_param_int" = 1L)
#     ),
#     list(
#       ps = th_paramset_trafo(),
#       ids = c("th_param_int"),
#       expected_ids = c("th_param_int"),
#       fix = list("th_param_dbl" = 1)
#     ),
#     list(
#       ps = th_paramset_trafo(),
#       ids = NULL,
#       expected_ids = c("th_param_int"),
#       fix = list("th_param_dbl" = 1)
#     )
#   )
#   # Test the different combinations:
#   for (conf in configs) {
#     paramset_sub = conf$ps$subset(ids = conf$ids, fix = conf$fix)
#     expect_equal(paramset_sub$ids, conf$expected_ids)
#     s = SamplerUnif$new(paramset_sub)
#     x = s$sample(2)
#     expect_set_equal(colnames(x), conf$expected_ids)
#     expect_true(paramset_sub$check(x[1,]))
#     expect_true(paramset_sub$check(x[2,]))
#     x_trafo = paramset_sub$transform(x)


#     x = s$sample(1)
#     expect_set_equal(colnames(x), conf$expected_ids)
#     expect_true(paramset_sub$check(x))
#     x_trafo = paramset_sub$transform(x)
#   }
# })

# test_that("Combine of ParamSet work", {
#   # define some ParamSets we will join to the th_ ones
#   new_param_sets = list(
#     normal = ParamSet$new(
#       id = "new_param_set",
#       params = list(
#         ParamDbl$new("new_int", lower = 0L, upper = 10L)
#       )
#     ),
#     trafo = ParamSet$new(
#       id = "new_param_set_trafo",
#       params = list(
#         ParamDbl$new("new_real", lower = 0, upper = 10)
#       ),
#       trafo = function(x, tags) {
#         x$new_real = sqrt(x$new_real)
#         return(x)
#       }
#     )
#   )

#   ps_list = list(
#     th_paramset_empty(),
#     th_paramset_full(),
#     th_paramset_repeated(),
#     th_paramset_numeric(),
#     th_paramset_trafo()
#   )

#   for (ps in ps_list) {
#     for (ps_new in new_param_sets) {
#       info = paste0("parset = ", ps$id, "-", ps_new$id)
#       ps_comb1 = ps$combine(ps_new)
#       ps_comb2 = ps_new$combine(ps)
#       expect_set_equal(ps_comb1$ids, ps_comb1$ids, info = info)
#       expect_set_equal(ps_comb1$ids, c(ps$ids, ps_new$ids), info = info)
#       s = SamplerUnif$new(ps_comb1)
#       x = s$sample(1)
#       expect_data_table(x, info = info)
#       expect_true(ps_comb1$check(x), info = info)
#       expect_true(ps_comb2$check(x), info = info)
#       xt1 = ps_comb1$transform(x)
#       xt2 = ps_comb2$transform(x)[, colnames(xt1), with = FALSE]
#       expect_equal(xt1, xt2, info = info)
#     }
#   }

# })

test_that("empty paramset", {
  ps = ParamSet$new()
  expect_r6(ps, "ParamSet")
  expect_equal(ps$length, 0)
  expect_null(ps$lower)
})


test_that("ParamSet$check", {
  ps = th_paramset_numeric()
  expect_true(ps$check(list(th_param_int = 5, th_param_dbl = 5)))
  expect_true(ps$check(list(th_param_dbl = 5, th_param_int = 5)))
  expect_error(ps$check(list(th_param_dbl = 5)), "permutation of") # incomplete list
  expect_match(ps$check(list(th_param_dbl = 5, th_param_int = 15)), "not <= 10")
})

test_that("we cannot create ParamSet with non-strict R names", {
  expect_error(ParamSet$new(id = "$foo") , "naming convention")
})

test_that("ParamSet$print", {
  ps = th_paramset_empty()
  expect_output(print(ps), "ParamSet: th_paramset_empty")
  expect_output(print(ps), "Empty")
  ps = th_paramset_numeric()
  expect_output(print(ps), "ParamSet:")
  s = capture_output(print(ps))
  expect_true(stri_detect_fixed(s, "ParamInt"))
  expect_true(stri_detect_fixed(s, "ParamDbl"))
  s = capture_output(print(ps, hide.cols = c("pclass")))
  expect_false(stri_detect_fixed(s, "ParamInt"))
})

test_that("ParamSet does a deep copy of params on construction", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps = ParamSet$new(list(p))
  p$data[, lower := 2]
  expect_equal(p$lower, 2)
  expect_equal(ps$lowers, c(x = 1))
  expect_equal(ps$get_param("x")$lower, 1)
})

test_that("ParamSet does a deep copy of param on add", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps = ParamSet$new(list())$add_param(p)
  p$data[, lower := 2]
  expect_equal(p$lower, 2)
  expect_equal(ps$lowers, c(x = 1))
  expect_equal(ps$get_param("x")$lower, 1)
})

test_that("ParamSet$clone can be deep", {
  p = ParamDbl$new("x", lower = 1, upper = 3)
  ps1 = ParamSet$new(list(p))
  ps2 = ps1$clone(deep = TRUE)
  ps2$data[, id := "foo"]
  expect_equal(ps2$ids, "foo")
  expect_equal(ps1$ids, "x")
})

test_that("ParamSet$is_bounded", {
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3)
  ))
  expect_true(ps$is_bounded)
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1, upper = 3),
    ParamLgl$new("y")
  ))
  expect_true(ps$is_bounded)
  ps = ParamSet$new(list(
    ParamDbl$new("x", lower = 1),
    ParamLgl$new("y")
  ))
  expect_false(ps$is_bounded)
})

test_that("ParamSet$add_param", {
  ps = ParamSet$new(list())
  ps$add_param(ParamDbl$new("x", lower = 1))
  expect_equal(ps$length, 1L)
  expect_equal(ps$ids, "x")
  expect_equal(ps$lowers, c(x = 1))
  ps$add_param(ParamFct$new("y", values = c("a")))
  expect_equal(ps$length, 2L)
  expect_equal(ps$ids, c("x", "y"))
  expect_equal(ps$lowers, c(x = 1, y = NA))
})



