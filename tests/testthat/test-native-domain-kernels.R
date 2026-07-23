native_domain_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"))
}

native_domain_bind = function(domains) {
  paradox:::recover_domain(data.table::rbindlist(
    domains,
    use.names = TRUE,
    fill = TRUE
  ))
}

test_that("closed Domain kernels have forced registered entry points", {
  routines = c(
    domain_check_builtin = 3L,
    domain_property_builtin = 2L,
    domain_qunif_builtin = 2L,
    domain_sanitize_builtin = 2L
  )
  for (routine in names(routines)) {
    symbol = native_domain_symbol(routine)
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, routines[[routine]])
    expect_error(
      .Call(routine, list(), list(), PACKAGE = "paradox"),
      "not available"
    )
  }
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("empty Domain semantics are owned by the native kernels", {
  empty = paradox:::empty_domain
  expect_true(domain_check(empty, list()))
  expect_true(domain_check(p_dbl(0, 1), list()))
  expect_identical(domain_nlevels(empty), integer())
  expect_identical(domain_is_bounded(empty), logical())
  expect_identical(domain_is_number(empty), logical())
  expect_identical(domain_is_categ(empty), logical())
  expect_identical(domain_qunif(empty, c(-1, 2)), logical())
  expect_identical(domain_sanitize(empty, numeric()), numeric())
  expect_identical(domain_sanitize(empty, list()), list())

  zero_double = p_dbl(0, 1)[0]
  expect_true(domain_check(zero_double, list()))
  expect_identical(domain_nlevels(zero_double), integer())
  expect_identical(domain_qunif(zero_double, 0.5), logical())

  check = native_domain_symbol("domain_check_builtin")
  property = native_domain_symbol("domain_property_builtin")
  qunif = native_domain_symbol("domain_qunif_builtin")
  sanitize = native_domain_symbol("domain_sanitize_builtin")
  expect_true(.Call(check, empty, list(), FALSE))
  expect_identical(.Call(property, empty, 0L), integer())
  expect_identical(.Call(qunif, empty, 0.5), logical())
  expect_identical(.Call(sanitize, empty, list()), list())

  malformed = data.table::copy(empty)
  data.table::set(malformed, j = "storage_type", value = NULL)
  expect_error(.Call(property, malformed, 0L), "Corrupt empty Domain")
  unknown = structure(empty, class = c("UnknownDomain", "Domain", "data.table", "data.frame"))
  expect_error(.Call(check, unknown, list(), FALSE), "Unsupported Domain class")
})

test_that("built-in Domain checks are authoritative", {
  expect_true(domain_check(p_dbl(-1, 1), list(0.25)))
  expect_true(domain_check(p_int(-2, 2), list(1L)))
  expect_true(domain_check(p_fct(c("slow", "fast")), list("fast")))
  expect_true(domain_check(p_lgl(), list(FALSE)))
  expect_true(domain_check(p_uty(), list(list(payload = 1))))

  expect_match(
    domain_check(p_dbl(0, 1), list(TRUE)),
    "Must be of type 'number', not 'logical'",
    fixed = TRUE
  )
  expect_match(
    domain_check(p_dbl(0, 1), list(NA_real_)),
    "May not be NA",
    fixed = TRUE
  )
  expect_match(
    domain_check(p_int(0, 2), list(0.5)),
    "single integerish value",
    fixed = TRUE
  )
  expect_match(
    domain_check(p_fct(c("a", "b")), list("c")),
    "Must be element of set {'a','b'}, but is 'c'",
    fixed = TRUE
  )
  expect_match(
    domain_check(p_lgl(), list(NA)),
    "May not be NA",
    fixed = TRUE
  )
  expect_error(domain_check(p_dbl(), 1), "ordinary list")
  expect_error(domain_check(p_dbl(), list(1), internal = 1), "TRUE or FALSE")

  special = p_dbl(0, 1, special_vals = list("automatic", NULL))
  expect_true(domain_check(special, list("automatic")))
  expect_true(domain_check(special, list(NULL)))
  expect_match(
    domain_check(special, list("automatic"), internal = TRUE),
    "Must be of type 'number', not 'character'",
    fixed = TRUE
  )
})

test_that("ParamUty is the supported custom validation callback", {
  calls = 0L
  utility = p_uty(custom_check = function(value) {
    calls <<- calls + 1L
    if (is.character(value)) TRUE else "must be character"
  })
  # p_uty() validates the callback once at construction.
  expect_identical(calls, 1L)
  expect_true(domain_check(utility, list("ok")))
  expect_match(domain_check(utility, list(1)), "must be character")
  expect_gte(calls, 3L)

  malformed_result = p_uty(custom_check = function(value) TRUE)
  malformed_result$cargo[[1L]]$custom_check = function(value) FALSE
  expect_match(
    domain_check(malformed_result, list(1)),
    "must return TRUE or one non-missing string"
  )
})

test_that("translated ParamUty diagnostics survive forced collection", {
  skip_on_cran()
  utf8_reason = enc2utf8("caf\u00e9 required")
  latin1_reason = iconv(utf8_reason, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(latin1_reason),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(latin1_reason) = "latin1"
  utility = p_uty(custom_check = function(value) latin1_reason)
  expected = paste0(enc2utf8(utility$id), ": ", utf8_reason)

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = domain_check(utility, list(NULL))
  gctorture(previous)

  expect_identical(observed, expected)
  expect_identical(Encoding(observed), "UTF-8")
})

test_that("callback and argument snapshots bound one Domain operation", {
  observed = character()
  domain = p_uty(custom_check = function(value) {
    observed <<- c(observed, paste0("old-", value))
    TRUE
  })
  observed = character()
  combined = native_domain_bind(rep(list(domain), 2L))
  old = combined$cargo[[2L]]$custom_check
  combined$cargo[[1L]]$custom_check = function(value) {
    observed <<- c(observed, paste0("mutating-", value))
    data.table::set(
      combined,
      i = 2L,
      j = "cargo",
      value = list(list(custom_check = function(value) {
        observed <<- c(observed, paste0("new-", value))
        TRUE
      }, repr = "NoDefault"))
    )
    TRUE
  }
  combined$cargo[[2L]]$custom_check = old

  expect_true(domain_check(combined, list("a", "b")))
  expect_identical(observed, c("mutating-a", "old-b"))
  expect_true(domain_check(combined, list("c", "d")))
  expect_identical(observed, c("mutating-a", "old-b", "mutating-c", "new-d"))
})

test_that("Domain checks re-admit row shape after ALTREP materialization", {
  callbacks = 0L
  domain = p_dbl(0, 1)
  value = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      callbacks <<- callbacks + 1L
      pointer = .Call(
        get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
        domain,
        match("id", names(domain)) - 1L,
        c("x", "y")
      )
      rm(pointer)
      for (index in 1:3) invisible(gc(full = TRUE))
    },
    callback_after = 0L
  )

  expect_error(
    domain_check(domain, list(value)),
    "Domain shape changed|must have type .* length 2"
  )
  expect_identical(callbacks, 1L)
  expect_identical(domain$id, c("x", "y"))
})

test_that("Domain properties are closed native operations", {
  domains = list(
    p_dbl(0, 1),
    p_int(0, 2),
    p_fct(c("a", "b")),
    p_lgl(),
    p_uty()
  )
  expected = list(
    c(Inf, TRUE, TRUE, FALSE),
    c(3, TRUE, TRUE, FALSE),
    c(2, TRUE, FALSE, TRUE),
    c(2, TRUE, FALSE, TRUE),
    c(Inf, FALSE, FALSE, FALSE)
  )
  for (index in seq_along(domains)) {
    observed = c(
      domain_nlevels(domains[[index]]),
      domain_is_bounded(domains[[index]]),
      domain_is_number(domains[[index]]),
      domain_is_categ(domains[[index]])
    )
    expect_equal(observed, expected[[index]])
  }

  grouped = native_domain_bind(list(p_int(0, 1), p_int(10, 12)))
  expect_identical(domain_nlevels(grouped), c(2, 3))
  expect_identical(domain_is_bounded(grouped), c(TRUE, TRUE))
  expect_identical(domain_is_number(grouped), TRUE)
})

test_that("Domain sanitization has no R replay path", {
  domain = native_domain_bind(list(
    p_dbl(0, 1),
    p_dbl(-2, 2),
    p_dbl(10, 20)
  ))
  expect_identical(
    domain_sanitize(domain, list(-0.1, 2.1, 21)),
    list(0, 2, 20)
  )
  expect_warning(
    expect_identical(
      domain_sanitize(domain[1:2], list(-2, 20, 3)),
      list(0, 2, 1)
    ),
    "not a multiple"
  )
  expect_identical(
    domain_sanitize(
      p_int(),
      list(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5, TRUE, NA_real_, NaN)
    ),
    list(-2L, -2L, 0L, 0L, 2L, 2L, 1L, NA_integer_, NA_integer_)
  )
  expect_warning(
    expect_identical(domain_sanitize(p_int(), list(-Inf, Inf)), list(NA_integer_, NA_integer_)),
    "NAs introduced"
  )
  opaque = list(list(payload = 1))
  expect_identical(domain_sanitize(p_uty(), opaque), opaque)
  expect_error(domain_sanitize(p_dbl(), c("1", "2")), "numeric vector or list")
})

test_that("quantile kernels preserve formulas and portable rounding barriers", {
  double_domain = native_domain_bind(list(p_dbl(-1, 1), p_dbl(10, 20)))
  integer_domain = native_domain_bind(list(p_int(-2, 2), p_int(10, 12)))
  factor_domain = native_domain_bind(rep(list(p_fct(c("a", "b", "c"))), 2L))
  logical_domain = native_domain_bind(rep(list(p_lgl()), 2L))
  x = c(0, 0, 0.25, 0.5, 0.75, 1)

  expect_identical(domain_qunif(double_domain, x), c(-1, 10, -0.5, 15, 0.5, 20))
  expect_identical(domain_qunif(integer_domain, x), c(-2L, 10L, -1L, 11L, 1L, 12L))
  expect_identical(domain_qunif(factor_domain, x), c("a", "a", "a", "b", "c", "c"))
  expect_identical(domain_qunif(logical_domain, x), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))

  expect_identical(domain_qunif(p_dbl(4, 4), c(0, 0.5, 1)), rep(4, 3L))
  expect_identical(domain_qunif(p_int(4, 4), c(0, 0.5, 1)), rep(4L, 3L))
  expect_identical(domain_qunif(p_dbl(), c(0, 0.5, 1)), c(-Inf, NaN, Inf))
  expect_identical(domain_qunif(p_int(), c(0, 0.5, 1)), rep(NA_integer_, 3L))

  named = structure(c(0, 0.5, 1), names = letters[1:3], extra = "kept")
  expect_identical(attributes(domain_qunif(p_dbl(0, 1), named)), attributes(named))
  expect_identical(names(domain_qunif(p_lgl(), named)), names(named))
  expect_error(domain_qunif(p_dbl(), c(-0.1, 1)), "between zero and one")
  expect_error(domain_qunif(p_uty(), 0.5), "undefined for ParamUty")

  expect_identical(
    domain_qunif(p_dbl(-10, 10), 0.499),
    -0x1.47ae147ae14p-6
  )
  expect_identical(
    domain_qunif(p_int(-2, 2), 0x1.9999999999999p-3),
    -1L
  )
})

test_that("zero-level factor Domains map only empty quantile inputs", {
  domain = p_fct(character())

  expect_identical(domain_nlevels(domain), 0)
  expect_identical(domain_qunif(domain, numeric()), character())
  expect_error(
    domain_qunif(domain, 0.5),
    "Cannot map quantiles for a factor Domain with no levels",
    fixed = TRUE
  )
})

test_that("common ALTREP inputs are consumed safely once", {
  expect_true(domain_check(p_int(1, 1), as.list(1:1)))
  expect_identical(domain_qunif(p_int(0, 9), 0:1), c(0L, 9L))
  expect_identical(domain_sanitize(p_int(), 1:4), as.list(1:4))
})

test_that("third-party Domain seams are closed", {
  forged = p_dbl(0, 1)
  class(forged)[[1L]] = "ParamExtension"
  for (operation in list(
    function() domain_check(forged, list(0.5)),
    function() domain_nlevels(forged),
    function() domain_qunif(forged, 0.5),
    function() domain_sanitize(forged, list(0.5))
  )) {
    expect_error(operation(), "Unsupported Domain class")
  }

  registerS3method(
    "domain_check",
    "ParamExtension",
    function(...) stop("third-party method ran"),
    envir = asNamespace("paradox")
  )
  expect_error(domain_check(forged, list(0.5)), "Unsupported Domain class")
})

test_that("malformed built-in Domains error instead of restarting in R", {
  corrupt = function(domain, column, value) {
    domain = data.table::copy(domain)
    data.table::set(domain, j = column, value = value)
    domain
  }
  cases = list(
    corrupt(p_dbl(0, 1), "storage_type", "integer"),
    corrupt(p_dbl(0, 1), "lower", NA_real_),
    corrupt(p_fct(c("a", "b")), "levels", list(1:2)),
    corrupt(p_lgl(), "levels", list(c(FALSE, TRUE))),
    corrupt(p_uty(), "cargo", list(list(custom_check = 1)))
  )
  operations = list(
    function(domain) domain_check(domain, list(1)),
    function(domain) domain_nlevels(domain)
  )
  for (domain in cases) {
    for (operation in operations) {
      expect_error(operation(domain), "Corrupt Domain storage")
    }
  }
})

test_that("large closed Domain loops remain interruptible and correct", {
  size = 65537L
  domain = native_domain_bind(rep(list(p_dbl(0, 1, tolerance = 0)), size))
  expect_true(domain_check(domain, as.list(rep(0.5, size))))
  expect_identical(domain_qunif(domain, rep(0.5, size)), rep(0.5, size))
  expect_identical(domain_sanitize(domain, rep(0.5, size)), as.list(rep(0.5, size)))
})
