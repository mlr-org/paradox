test_that("infinite double bounds remain valid with zero tolerance", {
  cases = list(
    lower_finite = list(p_dbl(0, Inf, tolerance = 0), c(0, 1, Inf), -1),
    upper_finite = list(p_dbl(-Inf, 1, tolerance = 0), c(-Inf, 0, 1), 2),
    positive_point = list(p_dbl(Inf, Inf, tolerance = 0), Inf, -Inf),
    negative_point = list(p_dbl(-Inf, -Inf, tolerance = 0), -Inf, Inf)
  )

  for (case in cases) {
    domain = case[[1L]]
    parameter_set = ParamSet$new(list(x = domain))
    valid = case[[2L]]
    invalid = case[[3L]]

    for (value in valid) {
      expect_true(domain_check(domain, list(value)), info = deparse(domain))
      expect_true(parameter_set$check(list(x = value)), info = deparse(domain))
      expect_true(
        parameter_set$check_dt(data.frame(x = value)),
        info = deparse(domain)
      )
    }
    expect_false(domain_test(domain, list(invalid)), info = deparse(domain))
    expect_false(parameter_set$test(list(x = invalid)), info = deparse(domain))
    expect_false(
      parameter_set$test_dt(data.frame(x = invalid)),
      info = deparse(domain)
    )
  }
})

test_that("fixed infinite double Domains accept their point at default tolerance", {
  for (point in c(-Inf, Inf)) {
    domain = p_dbl(point, point)
    parameter_set = ParamSet$new(list(x = domain))

    expect_true(domain_check(domain, list(point)))
    expect_true(parameter_set$check(list(x = point)))
    expect_true(parameter_set$check_dt(data.frame(x = point)))
  }
})

test_that("quantile mapping preserves defined infinite endpoints", {
  units = c(0, 0.5, 1)
  cases = list(
    bounded = list(p_dbl(0, 1), c(0, 0.5, 1)),
    lower_finite = list(p_dbl(0, Inf), c(0, Inf, Inf)),
    upper_finite = list(p_dbl(-Inf, 1), c(-Inf, -Inf, 1)),
    two_sided = list(p_dbl(-Inf, Inf), c(-Inf, NaN, Inf)),
    positive_point = list(p_dbl(Inf, Inf), rep(Inf, 3L)),
    negative_point = list(p_dbl(-Inf, -Inf), rep(-Inf, 3L))
  )

  for (name in names(cases)) {
    domain = cases[[name]][[1L]]
    expected = cases[[name]][[2L]]
    parameter_set = ParamSet$new(list(x = domain))
    matrix_input = matrix(
      units,
      ncol = 1L,
      dimnames = list(NULL, "x")
    )

    expect_identical(domain_qunif(domain, units), expected, info = name)
    expect_identical(parameter_set$qunif(matrix_input)$x, expected, info = name)
  }
})

test_that("integer quantiles retain representable unbounded endpoints", {
  units = c(0, 0.5, 1)
  cases = list(
    lower_finite = list(p_int(0, Inf), c(0L, NA_integer_, NA_integer_)),
    upper_finite = list(p_int(-Inf, 1), c(NA_integer_, NA_integer_, 1L))
  )

  for (name in names(cases)) {
    domain = cases[[name]][[1L]]
    expected = cases[[name]][[2L]]
    parameter_set = ParamSet$new(list(x = domain))
    matrix_input = matrix(
      units,
      ncol = 1L,
      dimnames = list(NULL, "x")
    )

    expect_warning(
      direct <- domain_qunif(domain, units),
      "NAs introduced by coercion"
    )
    expect_warning(
      bulk <- parameter_set$qunif(matrix_input)$x,
      "NAs introduced by coercion"
    )
    expect_identical(direct, expected, info = name)
    expect_identical(bulk, expected, info = name)
  }
})
