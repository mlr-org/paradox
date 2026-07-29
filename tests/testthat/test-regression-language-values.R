# A `p_uty()` value may be any R object, including a symbol or an unevaluated
# call. Documented one-argument callbacks must receive that object, not the
# result of evaluating it: R only self-evaluates the other value types, so
# splicing a language object straight into the callback call would substitute a
# different value, fail on a name the callback never mentions, or run the call.

test_that("custom_check receives a language value instead of its evaluation", {
  observed = NULL
  set = ps(u = p_uty(custom_check = function(x) {
    observed <<- x
    TRUE
  }))

  set$values = list(u = quote(1 + 1))
  expect_identical(observed, quote(1 + 1))
  expect_identical(set$values$u, quote(1 + 1))

  set$values = list(u = as.symbol("a_name_no_frame_defines"))
  expect_identical(observed, as.symbol("a_name_no_frame_defines"))
  expect_identical(set$values$u, as.symbol("a_name_no_frame_defines"))

  expect_true(set$check(list(u = quote(f(g(1))))))
  expect_identical(observed, quote(f(g(1))))
})

test_that("standalone Domain checking passes a language value through", {
  observed = NULL
  domain = p_uty(custom_check = function(x) {
    observed <<- x
    TRUE
  })

  expect_true(domain_check(domain, list(quote(z * 2))))
  expect_identical(observed, quote(z * 2))
})

test_that("checking a language value runs no part of it", {
  previous = Sys.getenv("PARADOX_LANGUAGE_VALUE_PROBE", unset = NA_character_)
  on.exit(
    if (is.na(previous)) {
      Sys.unsetenv("PARADOX_LANGUAGE_VALUE_PROBE")
    } else {
      do.call(Sys.setenv, set_names(list(previous), "PARADOX_LANGUAGE_VALUE_PROBE"))
    },
    add = TRUE
  )
  Sys.setenv(PARADOX_LANGUAGE_VALUE_PROBE = "untouched")

  set = ps(u = p_uty(custom_check = function(x) {
    force(x)
    TRUE
  }))
  set$values = list(
    u = quote(Sys.setenv(PARADOX_LANGUAGE_VALUE_PROBE = "executed"))
  )

  expect_identical(
    Sys.getenv("PARADOX_LANGUAGE_VALUE_PROBE"),
    "untouched"
  )
})

test_that("per-parameter transformations receive a language value", {
  observed = NULL
  set = ps(u = p_uty(trafo = function(x) {
    observed <<- x
    deparse(x)
  }))
  set$values = list(u = quote(m + n))

  expect_identical(set$trafo(set$get_values())$u, "m + n")
  expect_identical(observed, quote(m + n))
})

test_that("self-evaluating values still reach callbacks unchanged", {
  observed = NULL
  set = ps(u = p_uty(custom_check = function(x) {
    observed <<- x
    TRUE
  }))

  formula_value = y ~ x
  environment(formula_value) = new.env()
  ordinary = list(
    1.5, "text", list(1, 2), NULL, TRUE, expression(a + b),
    formula_value, sum
  )
  for (value in ordinary) {
    set$values = list(u = value)
    expect_identical(observed, value)
    expect_identical(set$values$u, value)
  }
})
