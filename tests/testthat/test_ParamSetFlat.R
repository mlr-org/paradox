context("ParamSetFlat")

test_that("methods and active bindings work", {
  ps.list = list(
    th.paramset.flat.empty,
    th.paramset.flat.full,
    th.paramset.flat.repeated,
    th.paramset.flat.restricted,
    th.paramset.flat.untyped,
    th.paramset.flat.numeric,
    th.paramset.flat.trafo,
    th.paramset.flat.trafo.dictionary
    )
  for (ps in ps.list) {
    if (ps$id == "th.paramset.flat.full") {
      expect_equal(ps$ids, c('th.param.int', 'th.param.real', 'th.param.categorical', 'th.param.flag'))
      expect_equal(ps$lower, c(th.param.int=-10, th.param.real=-10, th.param.categorical=NA_real_, th.param.flag=NA_real_))
      expect_equal(ps$upper, c(th.param.int=10, th.param.real=10, th.param.categorical=NA_real_, th.param.flag=NA_real_))
    }
    expect_class(ps, "ParamSetFlat")
    expect_numeric(ps$lower, any.missing = TRUE, names = "strict")
    expect_numeric(ps$upper, any.missing = TRUE, names = "strict")
    expect_character(ps$storage.types, names = "strict")
    expect_character(ps$ids)
    expect_list(ps$values, any.missing = TRUE, names = "strict")
    expect_character(ps$param.classes, names = "strict")
    expect_data_table(ps$range)
    expect_flag(ps$has.finite.bounds)
    expect_int(ps$length, lower = 0L)
    expect_integer(ps$nlevels, any.missing = TRUE)
    expect_list(ps$member.tags, names = "strict", any.missing = TRUE)
  }
})

test_that("advanced methods work", {
  ps.list = list(
    th.paramset.flat.full,
    th.paramset.flat.repeated,
    th.paramset.flat.restricted,
    th.paramset.flat.numeric,
    th.paramset.flat.trafo,
    th.paramset.flat.trafo.dictionary
  )
  
  for (ps in ps.list) {

    x = ps$sample(10)
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

    xl = ps$generateLHSDesign(10)
    expect_data_table(xl, nrows = 10, any.missing = FALSE)
    expect_true(all(xl[, ps$test(.SD), by = seq_len(nrow(xl))]$V1))
    xlt = ps$transform(xl)
    expect_data_table(xlt, nrows = 10)

    xg = ps$generateGridDesign(5)
    expect_data_table(xg, any.missing = FALSE)
    expect_true(nrow(xg) <= 5^ps$length)
    expect_true(all(xg[, ps$test(.SD), by = seq_len(nrow(xg))]$V1))
    xgt = ps$transform(xg)
    expect_data_table(xgt, nrows = nrow(xg))

    p.res = ps$nlevels
    p.res[is.na(p.res)] = 2
    xgp = ps$generateGridDesign(param.resolutions = p.res)
    expect_data_table(xgp, any.missing = FALSE)
    expect_true(nrow(xgp) <= prod(p.res))

    xgn = ps$generateGridDesign(n = 100)
    expect_data_table(xgn, any.missing = FALSE)
    expect_true(nrow(xgn) <= 100)
  }
})

test_that("repeated params in ParamSetFlat works", {
  ps = th.paramset.flat.repeated
  expect_class(ps, "ParamSetFlat")
  expect_equal(sum(sapply(ps$member.tags, function(z) "th.param.real.na.repeated" %in% z)), 4)
  xs = ps$sample(10)
  expect_true("th.param.categorical" %in% names(xs))
  xs.t = ps$transform(xs)
  expect_false("th.param.nat" %in% names(xs.t))
  expect_list(xs.t$vector.param)
})