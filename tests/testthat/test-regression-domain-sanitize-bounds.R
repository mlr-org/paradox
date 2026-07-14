test_that("double sanitization applies each parameter's own bounds", {
  param_set = ps(
    first = p_dbl(0, 1, tolerance = 0.1),
    second = p_dbl(-2, 2, tolerance = 0.2),
    third = p_dbl(10, 20, tolerance = 0.1)
  )

  result = expect_no_warning(param_set$check(
    list(first = -0.05, second = 2.3, third = 20.5),
    sanitize = TRUE
  ))

  expect_identical(
    attr(result, "sanitized"),
    list(first = 0, second = 2, third = 20)
  )
})

test_that("direct double-domain sanitization is pairwise and type stable", {
  param_set = ps(
    first = p_dbl(0, 1),
    second = p_dbl(-2, 2),
    third = p_dbl(10, 20)
  )
  domain = recover_domain(data.table::rbindlist(
    lapply(param_set$ids(), param_set$get_domain),
    use.names = TRUE
  ))

  expect_identical(
    domain_sanitize(domain, list(-0.1, 2.1, 21)),
    list(0, 2, 20)
  )
  expect_identical(
    domain_sanitize(domain, list(0.5, -1.5, 15)),
    list(0.5, -1.5, 15)
  )
})
