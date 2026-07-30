# Public Domain operations route the complete outward table through the single
# canonical Domain-row admission owner before any operation-specific work.
# These regressions pin that every public operation rejects the same corrupt
# nested state, and that nothing an ordinary Domain can express changed.

nested_corrupt = function(domain, column, value) {
  attrs = attributes(domain)
  result = unclass(domain)
  result[[column]] = value
  attributes(result) = attrs
  result
}

# Every public entry point of the closed Domain kernel family.
nested_operations = function(domain, value) {
  list(
    check = function() domain_check(domain, list(value)),
    nlevels = function() domain_nlevels(domain),
    is_bounded = function() domain_is_bounded(domain),
    is_categ = function() domain_is_categ(domain),
    sanitize = function() domain_sanitize(domain, list(value)),
    qunif = function() domain_qunif(domain, 0.5)
  )
}

expect_corrupt_everywhere = function(domain, value, pattern = "Corrupt Domain storage",
    label = "case") {
  operations = nested_operations(domain, value)
  for (name in names(operations)) {
    expect_error(
      operations[[name]](),
      pattern,
      info = paste(label, name, sep = "/")
    )
  }
}

test_that("a special-value match does not suppress factor-level admission", {
  # The special-value fast path may skip a row's *value* check; it may never
  # skip that row's schema admission.
  domain = p_fct(c("a", "b"), special_vals = list("special"))
  expect_true(domain_check(domain, list("special")))

  missing_levels = nested_corrupt(domain, "levels", list(c("a", NA_character_)))
  expect_error(
    domain_check(missing_levels, list("special")),
    "`levels` may not contain missing values",
    fixed = TRUE
  )
  expect_error(
    domain_check(missing_levels, list("a")),
    "`levels` may not contain missing values",
    fixed = TRUE
  )

  wrong_type = nested_corrupt(domain, "levels", list(1:2))
  expect_error(
    domain_check(wrong_type, list("special")),
    "each `levels` element must be character",
    fixed = TRUE
  )

  duplicated = nested_corrupt(domain, "levels", list(c("a", "a")))
  expect_error(domain_check(duplicated, list("special")), "Corrupt Domain storage")
})

test_that("factor levels are admitted identically by every public operation", {
  domain = p_fct(c("a", "b"))
  cases = list(
    missing = c("a", NA_character_),
    duplicated = c("a", "a"),
    duplicated_tail = c("a", "b", "a"),
    attributed = structure(c("a", "b"), extra = 1),
    classed = structure(c("a", "b"), class = "myclass"),
    named = c(x = "a", y = "b"),
    s4 = asS4(c("a", "b")),
    wrong_type = 1:2
  )
  for (case in names(cases)) {
    expect_corrupt_everywhere(
      nested_corrupt(domain, "levels", list(cases[[case]])),
      "a",
      label = case
    )
  }

  # A structural ALTREP level vector is rejected without observing an element.
  observations = 0L
  altrep_levels = native_stateful_altrep(
    c("a", "b"),
    c("a", "b"),
    callback = function() observations <<- observations + 1L,
    callback_after = 0L
  )
  expect_corrupt_everywhere(
    nested_corrupt(domain, "levels", list(altrep_levels)),
    "a",
    label = "altrep"
  )
  expect_identical(observations, 0L)
})

test_that("logical levels are admitted identically by every public operation", {
  domain = p_lgl()
  cases = list(
    reversed = c(FALSE, TRUE),
    padded = c(TRUE, FALSE, NA),
    classed = structure(c(TRUE, FALSE), class = "myclass"),
    named = c(a = TRUE, b = FALSE),
    attributed = structure(c(TRUE, FALSE), extra = 1),
    s4 = asS4(c(TRUE, FALSE)),
    wrong_type = c("TRUE", "FALSE")
  )
  for (case in names(cases)) {
    expect_corrupt_everywhere(
      nested_corrupt(domain, "levels", list(cases[[case]])),
      TRUE,
      label = case
    )
  }
})

test_that("special-value row shells are ordinary unclassed lists", {
  domain = p_dbl(0, 1, special_vals = list("auto"))
  expect_true(domain_check(domain, list("auto")))
  cases = list(
    classed = structure(list("auto"), class = "myclass"),
    s4 = asS4(list("auto")),
    attributed = structure(list("auto"), extra = 1),
    wrong_type = "auto"
  )
  for (case in names(cases)) {
    corrupt = nested_corrupt(domain, "special_vals", list(cases[[case]]))
    expect_error(domain_check(corrupt, list("auto")), "Corrupt Domain storage",
      info = case)
    expect_error(domain_check(corrupt, list(0.5)), "Corrupt Domain storage",
      info = case)
    expect_error(domain_qunif(corrupt, 0.5), "Corrupt Domain storage", info = case)
    expect_error(domain_nlevels(corrupt), "Corrupt Domain storage", info = case)
    expect_error(domain_sanitize(corrupt, list(0.5)), "Corrupt Domain storage",
      info = case)
  }
})

test_that("a typed ALTREP special leaf is rejected before it is observed", {
  observations = 0L
  leaf = native_stateful_altrep(1, 2, callback = function() {
    observations <<- observations + 1L
  }, callback_after = 0L)
  corrupt = nested_corrupt(
    p_dbl(0, 1, special_vals = list(1)),
    "special_vals",
    list(list(leaf))
  )
  expect_error(domain_check(corrupt, list(1)), "Corrupt Domain storage")
  expect_identical(observations, 0L)

  # A ParamUty special leaf stays opaque by contract: admission neither
  # rejects nor observes it.
  utility = nested_corrupt(p_uty(), "special_vals", list(list(leaf)))
  expect_identical(domain_nlevels(utility), Inf)
  expect_identical(observations, 0L)
})

test_that("Domain cargo is admitted by the canonical owner", {
  utility = p_uty()
  utility_cases = list(
    absent = NULL,
    missing_repr = list(custom_check = NULL),
    wrong_repr = list(custom_check = NULL, repr = 1L),
    unknown_name = list(custom_check = NULL, repr = "x", bogus = 1),
    classed = structure(list(custom_check = NULL, repr = "x"), class = "cfg"),
    stray_logscale = list(custom_check = NULL, repr = "x", logscale = TRUE),
    unpaired_tuning = list(
      custom_check = NULL,
      repr = "x",
      in_tune_fn = function(domain, values) domain$upper
    )
  )
  for (case in names(utility_cases)) {
    corrupt = nested_corrupt(utility, "cargo", list(utility_cases[[case]]))
    expect_error(domain_check(corrupt, list(1)), "Corrupt Domain storage",
      info = case)
    expect_error(domain_nlevels(corrupt), "Corrupt Domain storage", info = case)
    expect_error(domain_qunif(corrupt, 0.5), "Corrupt Domain storage", info = case)
    expect_error(domain_sanitize(corrupt, list(1)), "Corrupt Domain storage",
      info = case)
  }

  # Cargo of the wrong kind used never to be fetched at all, so a stray
  # `custom_check` on a numeric Domain was silently inert.
  numeric_cases = list(
    stray_check = list(custom_check = function(x) FALSE),
    stray_repr = list(repr = "x"),
    logscale_without_trafo = list(logscale = TRUE)
  )
  for (case in names(numeric_cases)) {
    expect_corrupt_everywhere(
      nested_corrupt(p_dbl(0, 1), "cargo", list(numeric_cases[[case]])),
      0.5,
      label = case
    )
  }
})

test_that("numeric bounds and tolerance are admitted by every operation", {
  expect_error(
    domain_check(nested_corrupt(p_dbl(0, 1), "tolerance", Inf), list(1e300)),
    "invalid numeric bounds",
    fixed = TRUE
  )
  expect_error(
    domain_check(nested_corrupt(p_dbl(0, 1), "tolerance", NaN), list(0.5)),
    "invalid numeric bounds",
    fixed = TRUE
  )
  expect_error(
    domain_check(nested_corrupt(p_int(0L, 10L), "tolerance", 0.9), list(0.6)),
    "invalid numeric bounds",
    fixed = TRUE
  )
  # Non-integerish integer bounds used to be caught by the property path only.
  for (corrupt in list(
      nested_corrupt(p_int(0L, 10L), "lower", 0.5),
      nested_corrupt(p_int(0L, 10L), "upper", 9.5))) {
    expect_error(domain_check(corrupt, list(1L)), "invalid numeric bounds",
      fixed = TRUE)
    expect_error(domain_qunif(corrupt, 0), "invalid numeric bounds", fixed = TRUE)
    expect_error(domain_nlevels(corrupt), "invalid numeric bounds", fixed = TRUE)
  }
  # Kind-inappropriate schema entries are corrupt, not ignored.
  expect_corrupt_everywhere(
    nested_corrupt(p_dbl(0, 1), "levels", list(c("a", "b"))),
    0.5,
    label = "numeric_levels"
  )
  expect_corrupt_everywhere(
    nested_corrupt(p_fct(c("a", "b")), "lower", 0),
    "a",
    label = "categorical_bounds"
  )
  # Identity and grouping likewise.
  expect_corrupt_everywhere(
    nested_corrupt(p_dbl(0, 1), "id", ""),
    0.5,
    label = "empty_id"
  )
  expect_corrupt_everywhere(
    nested_corrupt(p_dbl(0, 1), "grouping", "bogus"),
    0.5,
    label = "grouping"
  )
})

test_that("nested admission preserves every canonical Domain operation", {
  # Zero-level ParamFct stays a canonical, typed-empty Domain.
  empty_levels = p_fct(character())
  expect_identical(domain_nlevels(empty_levels), 0)
  expect_identical(domain_qunif(empty_levels, numeric()), character())
  expect_error(
    domain_qunif(empty_levels, 0.5),
    "Cannot map quantiles for a factor Domain with no levels",
    fixed = TRUE
  )
  expect_identical(domain_sanitize(empty_levels, list("a")), list("a"))
  expect_type(domain_check(empty_levels, list("a")), "character")

  # The empty Domain and a zero-row typed Domain remain no-ops.
  expect_true(domain_check(paradox:::empty_domain, list()))
  expect_identical(domain_nlevels(paradox:::empty_domain), integer())
  expect_identical(domain_qunif(paradox:::empty_domain, c(-1, 2)), logical())
  expect_true(domain_check(p_dbl(0, 1)[0], list()))
  expect_identical(domain_nlevels(p_dbl(0, 1)[0]), integer())

  # Unbounded numeric and ParamUty Domains are supported operations, not
  # rejected shapes -- unlike the ObjectTuneToken candidate snapshot.
  expect_true(domain_check(p_dbl(), list(1e300)))
  expect_identical(domain_qunif(p_dbl(0, Inf), 0), 0)
  expect_identical(domain_nlevels(p_uty()), Inf)
  expect_true(domain_check(p_uty(), list(quote(z * 2))))
  expect_identical(domain_sanitize(p_uty(), list(1)), list(1))

  # Multi-row Domain tables remain admitted, including through the ParamSet
  # projection and a bound table.
  bound = paradox:::recover_domain(data.table::rbindlist(
    list(p_dbl(0, 1), p_dbl(2, 3)),
    use.names = TRUE
  ))
  expect_true(domain_check(bound, list(0.5, 2.5)))
  expect_identical(domain_qunif(bound, c(0, 1)), c(0, 3))
  expect_identical(domain_sanitize(bound, list(-1, 9)), list(0, 3))
  expect_identical(domain_is_bounded(bound), c(TRUE, TRUE))

  # A categorical Domain groups by its level fingerprint, so a multi-row
  # factor table shares one level set.
  factors = paradox:::recover_domain(data.table::rbindlist(
    list(p_fct(c("a", "b")), p_fct(c("a", "b"))),
    use.names = TRUE
  ))
  expect_identical(domain_nlevels(factors), c(2, 2))
  expect_identical(domain_qunif(factors, c(0, 0.99)), c("a", "b"))

  # And ordinary valid Domains still answer, including the special-value and
  # custom-check paths.
  expect_true(domain_check(p_dbl(0, 1, special_vals = list("auto")), list("auto")))
  expect_true(domain_check(
    p_uty(custom_check = function(x) if (is.numeric(x)) TRUE else "not numeric"),
    list(1)
  ))
  expect_match(
    domain_check(
      p_uty(custom_check = function(x) if (is.numeric(x)) TRUE else "not numeric"),
      list("a")
    ),
    "not numeric"
  )
})
