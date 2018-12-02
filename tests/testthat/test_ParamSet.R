context("ParamSet")

test_that("methods and active bindings work", {
  ps_list = list(
    th_paramset_empty,
    th_paramset_full,
    th_paramset_repeated,
    th_paramset_restricted,
    th_paramset_untyped,
    th_paramset_numeric,
    th_paramset_trafo
  )
  for (ps in ps_list) {
    if (ps$id == "th_paramset_full") {
      expect_equal(ps$ids, c('th_param_int', 'th_param_real', 'th_param_categorical', 'th_param_flag'))
      expect_equal(ps$lower, c(th_param_int=-10, th_param_real=-10, th_param_categorical=NA_real_, th_param_flag=NA_real_))
      expect_equal(ps$upper, c(th_param_int=10, th_param_real=10, th_param_categorical=NA_real_, th_param_flag=NA_real_))
    }
    expect_class(ps, "ParamSet")
    expect_numeric(ps$lower, any.missing = TRUE, names = "strict")
    expect_numeric(ps$upper, any.missing = TRUE, names = "strict")
    expect_character(ps$storage_types, names = "strict")
    expect_character(ps$ids)
    expect_list(ps$values, any.missing = TRUE, names = "strict")
    expect_character(ps$param_classes, names = "strict")
    expect_data_table(ps$range)
    expect_flag(ps$has_finite_bounds)
    expect_int(ps$length, lower = 0L)
    expect_integer(ps$nlevels, any.missing = TRUE)
    expect_list(ps$member_tags, names = "strict", any.missing = TRUE)
    expect_output(print(th_paramset_full), "ParamSet:")
  }
})

test_that("advanced methods work", {
  ps_list = list(
    th_paramset_full,
    th_paramset_repeated,
    th_paramset_restricted,
    th_paramset_numeric,
    th_paramset_trafo
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
    xd = ps$denorm(x)
    expect_data_table(xd, nrows = 10, any.missing = FALSE)
    expect_equal(colnames(xd), ps$ids)
    # denorm can produce infeasible settings
    # expect_true(all(x[, ps$test(.SD), by = seq_len(nrow(x))]$V1))
    xdt = ps$transform(xd)
    expect_data_table(xdt, nrows = 10)

  }
})

test_that("repeated params in ParamSet works", {
  ps = th_paramset_repeated
  expect_class(ps, "ParamSet")
  expect_equal(sum(sapply(ps$member_tags, function(z) "th_param_real_na_repeated" %in% z)), 4)
  s = SamplerUnif$new(ps)
  xs = s$sample(10)
  expect_true("th_param_categorical" %in% names(xs))
  # xs_t = ps$transform(xs)
  # expect_false("th_param_nat" %in% names(xs_t))
  # expect_list(xs_t$vector_param)
  # xs_l = design_to_list(xs_t)
  # expect_list(xs_l, len = 10)
})

test_that("param subset in ParamSet works", {
  # Define all the different subsets we want to try:
  configs = list(
    list(
      ps = th_paramset_full,
      ids = c("th_param_int", "th_param_flag"),
      expected_ids = c("th_param_int", "th_param_flag"),
      fix = NULL
    ),
    list(
      ps = th_paramset_full,
      expected_ids = c("th_param_real", "th_param_categorical", "th_param_flag"),
      fix = list("th_param_int" = 1L)
    ),
    list(
      ps = th_paramset_trafo,
      ids = c("th_param_int"),
      expected_ids = c("th_param_int"),
      fix = list("th_param_real" = 1)
    ),
    list(
      ps = th_paramset_trafo,
      ids = NULL,
      expected_ids = c("th_param_int"),
      fix = list("th_param_real" = 1)
    ),
    list(
      ps = th_paramset_restricted,
      ids = NULL,
      expected_ids = c("th_param_int", "th_param_categorical"),
      fix = list("th_param_real" = 1)
    )
  )
  # Test the different combinations:
  for (conf in configs) {
    paramset_sub = conf$ps$subset(ids = conf$ids, fix = conf$fix)
    expect_equal(paramset_sub$ids, conf$expected_ids)
    s = SamplerUnif$new(paramset_sub)
    x = s$sample(2)
    expect_set_equal(colnames(x), conf$expected_ids)
    expect_true(paramset_sub$check(x[1,]))
    expect_true(paramset_sub$check(x[2,]))
    x_trafo = paramset_sub$transform(x)


    x = s$sample(1)
    expect_set_equal(colnames(x), conf$expected_ids)
    expect_true(paramset_sub$check(x))
    x_trafo = paramset_sub$transform(x)
  }
})

test_that("Combine of ParamSet work", {
  # define some ParamSets we will join to the th_ ones
  new_param_sets = list(
    normal = ParamSet$new(
      id = "new_param_set",
      params = list(
        ParamReal$new("new_int", lower = 0L, upper = 10L)
      )
    ),
    trafo = ParamSet$new(
      id = "new_param_set_trafo",
      params = list(
        ParamReal$new("new_real", lower = 0, upper = 10)
      ),
      trafo = function(x, tags) {
        x$new_real = sqrt(x$new_real)
        return(x)
      }
    ),
    restriction = ParamSet$new(
      id = "new_param_set_requires",
      params = list(
        ParamReal$new("new_real", lower = 0, upper = 10),
        ParamReal$new("new_int", lower = 0L, upper = 10L)
      ),
      restriction = quote(new_real>=new_int)
    )
  )

  ps_list = list(
    th_paramset_empty,
    th_paramset_full,
    th_paramset_repeated,
    th_paramset_restricted,
    th_paramset_numeric,
    th_paramset_trafo
  )

  for (ps in ps_list) {
    for (ps_new in new_param_sets) {
      info = paste0("parset = ", ps$id, "-", ps_new$id)
      ps_comb1 = ps$combine(ps_new)
      ps_comb2 = ps_new$combine(ps)
      expect_set_equal(ps_comb1$ids, ps_comb1$ids, info = info)
      expect_set_equal(ps_comb1$ids, c(ps$ids, ps_new$ids), info = info)
      s = SamplerUnif$new(ps_comb1)
      x = s$sample(1)
      expect_data_table(x, info = info)
      expect_true(ps_comb1$check(x), info = info)
      expect_true(ps_comb2$check(x), info = info)
      xt1 = ps_comb1$transform(x)
      xt2 = ps_comb2$transform(x)[, colnames(xt1), with = FALSE]
      expect_equal(xt1, xt2, info = info)
    }
  }

})
