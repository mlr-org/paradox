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

native_domain_capture = function(expr) {
  warnings = character()
  value = withCallingHandlers(
    force(expr),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

test_that("native Domain kernels are registered with forced symbols", {
  for (routine in c(
    "domain_check_builtin",
    "domain_qunif_builtin",
    "domain_sanitize_builtin"
  )) {
    symbol = native_domain_symbol(routine)
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, 2L)
    expect_error(
      .Call(routine, list(), list(), PACKAGE = "paradox"),
      "not available"
    )
  }

  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native validity gates recognize common built-in scalar values", {
  symbol = native_domain_symbol("domain_check_builtin")

  cases = list(
    list(p_dbl(-1, 1), list(0.25), list(TRUE)),
    list(p_int(-2, 2), list(1L), list(1.25)),
    list(p_fct(c("slow", "fast")), list("fast"), list("absent")),
    list(p_lgl(), list(FALSE), list(1L))
  )
  for (case in cases) {
    expect_identical(.Call(symbol, case[[1L]], case[[2L]]), TRUE)
    expect_identical(.Call(symbol, case[[1L]], case[[3L]]), FALSE)
  }

  # The fast path is deliberately narrower than the accepted R API. Values
  # outside that common representation fall through to the exact S3 method.
  expect_false(.Call(symbol, p_int(0, 2), list(1 + 0i)))
  expect_true(domain_check(p_int(0, 2), list(1 + 0i)))
  expect_false(.Call(symbol, p_dbl(0, 1), list(structure(0.5, class = "x"))))
  expect_true(domain_check(p_dbl(0, 1), list(structure(0.5, class = "x"))))

  # NULL is not a vector for which XLENGTH() may be queried.  The native gate
  # must reject it without raising so the R layer can either report the usual
  # diagnostic or recognize it as a declared special value.
  expect_identical(.Call(symbol, p_dbl(0, 1), list(NULL)), FALSE)
  expect_type(domain_check(p_dbl(0, 1), list(NULL)), "character")
  expect_identical(
    domain_check(p_dbl(0, 1, special_vals = list(NULL)), list(NULL)),
    TRUE
  )
  special = p_dbl(0, 1, special_vals = list("automatic"))
  expect_false(.Call(symbol, special, list(0.5)))
  expect_true(domain_check(special, list(0.5)))
})

test_that("invalid built-in values retain exact R fallback diagnostics", {
  dbl = p_dbl(0, 1, tolerance = 0.1)
  int = p_int(0, 2, tolerance = 0.1)
  fct = p_fct(c("a", "b"))
  lgl = p_lgl()

  expect_identical(
    domain_check(dbl, list(TRUE)),
    paste0(
      "p_dbl(lower = 0, upper = 1, tolerance = 0.1): ",
      "Must be of type 'number', not 'logical'"
    )
  )
  expect_identical(
    domain_check(dbl, list(NA_real_)),
    "p_dbl(lower = 0, upper = 1, tolerance = 0.1): May not be NA"
  )
  expect_identical(
    domain_check(dbl, list(Inf)),
    paste0(
      "p_dbl(lower = 0, upper = 1, tolerance = 0.1): ",
      "Element 1 is not <= 1.1"
    )
  )
  expect_identical(
    domain_check(int, list(0.5)),
    paste0(
      "p_int(lower = 0, upper = 2, tolerance = 0.1): ",
      "Must be of type 'single integerish value', not 'double'"
    )
  )
  expect_identical(
    domain_check(fct, list(factor("a"))),
    paste0(
      "p_fct(levels = c(\"a\", \"b\")): ",
      "Must be element of set {'a','b'}, but types do not match ",
      "(factor != character)"
    )
  )
  expect_identical(domain_check(lgl, list(NA)), "p_lgl(): May not be NA")
  expect_identical(domain_check(dbl, 0.5), "values must be a list")
  expect_identical(domain_check(dbl, NULL), TRUE)
})

test_that("native checks decline dispatch-sensitive Domain metadata", {
  symbol = native_domain_symbol("domain_check_builtin")
  domain = p_dbl(0, 2)
  data.table::setattr(domain$lower, "class", "NativeDomainCheckAudit")
  method = "Ops.NativeDomainCheckAudit"
  assign(
    method,
    function(...) stop("NATIVE DOMAIN CHECK OPS DISPATCH", call. = FALSE),
    envir = .GlobalEnv
  )
  on.exit(rm(list = method, envir = .GlobalEnv), add = TRUE)

  expect_false(.Call(symbol, domain, list(1)))
  expect_error(
    domain_check(domain, list(1)),
    "NATIVE DOMAIN CHECK OPS DISPATCH",
    fixed = TRUE
  )

  factor = p_fct(c("a", "b"))
  data.table::setattr(factor$levels[[1L]], "note", "dispatch-sensitive")
  expect_false(.Call(symbol, factor, list("a")))
  expect_true(domain_check(factor, list("a")))

  coerced_factor = p_fct(c("1", "2"))
  coerced_factor$levels[[1L]] = 1:2
  expect_false(.Call(symbol, coerced_factor, list("1")))
  expect_true(domain_check(coerced_factor, list("1")))
})

test_that("validity kernels preserve tolerance, alignment, and special values", {
  expect_true(domain_check(p_dbl(0, 1, tolerance = 0.1), list(-0.1)))
  expect_true(domain_check(p_dbl(0, 1, tolerance = 0.1), list(1.1)))
  expect_false(domain_test(p_dbl(0, 1, tolerance = 0.1), list(-0.1000001)))
  expect_false(domain_test(p_dbl(0, 1, tolerance = 0.1), list(1.1000001)))

  expect_true(domain_check(p_int(0, 2, tolerance = 0.1), list(-0.1)))
  expect_true(domain_check(p_int(0, 2, tolerance = 0.1), list(2.1)))
  expect_false(domain_test(p_int(0, 2, tolerance = 0.1), list(-0.1000001)))
  expect_false(domain_test(p_int(0, 2, tolerance = 0.1), list(2.1000001)))

  double_domain = native_domain_bind(list(
    p_dbl(-1, 1, tolerance = 0.01),
    p_dbl(10, 20, tolerance = 0.1),
    p_dbl(-100, -50, tolerance = 0)
  ))
  expect_true(domain_check(double_domain, list(0, 21, -75)))
  expect_false(domain_test(double_domain, list(0, 21, -49)))

  factor_domain = native_domain_bind(rep(list(p_fct(c("a", "b"))), 3L))
  expect_true(domain_check(factor_domain, list("a", "b", "a")))
  expect_false(domain_test(factor_domain, list("a", "missing", "a")))

  special = p_dbl(0, 1, special_vals = list("automatic"))
  expect_true(domain_check(special, list("automatic")))
  expect_type(domain_check(special, list("automatic"), internal = TRUE), "character")
})

test_that("native checks preserve internal argument forcing and diagnostics", {
  domain = p_lgl()
  expect_error(
    domain_check(domain, list(TRUE), internal = NA),
    "missing value where TRUE/FALSE needed",
    fixed = TRUE
  )
  expect_error(
    domain_check(domain, list(TRUE), internal = c(TRUE, FALSE)),
    "the condition has length > 1",
    fixed = TRUE
  )
  expect_error(
    domain_check(domain, list(TRUE), internal = NULL),
    "invalid argument type",
    fixed = TRUE
  )
  expect_true(domain_check(domain, list(TRUE), internal = 1))

  events = new.env(parent = emptyenv())
  events$seen = character()
  expect_true(domain_check(
    domain,
    list(),
    internal = {
      events$seen = c(events$seen, "internal")
      FALSE
    }
  ))
  expect_identical(events$seen, character())
  expect_true(domain_check(
    domain,
    list(TRUE),
    internal = {
      events$seen = c(events$seen, "internal")
      FALSE
    }
  ))
  expect_identical(events$seen, "internal")
})

test_that("native validity kernels agree with vectorized reference formulas", {
  symbol = native_domain_symbol("domain_check_builtin")
  set.seed(20260713)
  size = 257L

  lower = runif(size, -1000, 1000)
  upper = lower + rexp(size, rate = 0.02)
  tolerance = runif(size, 0, 0.2)
  double_domain = native_domain_bind(Map(
    function(lo, hi, tol) p_dbl(lo, hi, tolerance = tol),
    lower,
    upper,
    tolerance
  ))
  double_values = runif(
    size,
    lower - tolerance * pmax(1, abs(lower)),
    upper + tolerance * pmax(1, abs(upper))
  )
  double_reference = all(
    double_values >= lower - tolerance * pmax(1, abs(lower)) &
      double_values <= upper + tolerance * pmax(1, abs(upper))
  )
  expect_identical(
    .Call(symbol, double_domain, as.list(double_values)),
    double_reference
  )
  double_values[[size %/% 2L]] = upper[[size %/% 2L]] * 2 + 2000
  expect_false(.Call(symbol, double_domain, as.list(double_values)))

  lower = sample(-1000:0, size, replace = TRUE)
  upper = lower + sample(0:100, size, replace = TRUE)
  tolerance = runif(size, 0, 0.5)
  integer_domain = native_domain_bind(Map(
    function(lo, hi, tol) p_int(lo, hi, tolerance = tol),
    lower,
    upper,
    tolerance
  ))
  integer_values = round(runif(size, lower, upper)) +
    runif(size, -tolerance, tolerance)
  rounded = round(integer_values)
  integer_reference = all(
    abs(integer_values - rounded) <= tolerance &
      rounded >= lower & rounded <= upper
  )
  expect_identical(
    .Call(symbol, integer_domain, as.list(integer_values)),
    integer_reference
  )

  levels = c("red", "green", "blue")
  factor_domain = native_domain_bind(rep(list(p_fct(levels)), size))
  factor_values = as.list(sample(levels, size, replace = TRUE))
  expect_true(.Call(symbol, factor_domain, factor_values))
  factor_values[[size]] = "ultraviolet"
  expect_false(.Call(symbol, factor_domain, factor_values))

  logical_domain = native_domain_bind(rep(list(p_lgl()), size))
  expect_true(.Call(symbol, logical_domain, as.list(rep(c(TRUE, FALSE), length.out = size))))
})

test_that("double sanitization is pairwise, recycling-compatible, and type stable", {
  domain = native_domain_bind(list(
    p_dbl(0, 1),
    p_dbl(-2, 2),
    p_dbl(10, 20)
  ))

  expect_identical(domain_sanitize(domain, list(-0.1, 2.1, 21)), list(0, 2, 20))
  expect_identical(domain_sanitize(domain, c(-0.1, 2.1, 21)), list(0, 2, 20))
  expect_identical(
    domain_sanitize(domain, structure(c(-0.1, 2.1, 21), names = letters[1:3])),
    list(0, 2, 20)
  )
  expect_identical(
    domain_sanitize(p_dbl(0, 1), list(-Inf, NA_real_, NaN, Inf, TRUE)),
    list(0, NA_real_, NaN, 1, 1)
  )

  two = native_domain_bind(rep(list(p_dbl(0, 10)), 2L))
  three = native_domain_bind(rep(list(p_dbl(0, 10)), 3L))
  warning_text = "an argument will be fractionally recycled"
  expect_identical(
    native_domain_capture(domain_sanitize(two, list(-2, 20, 3))),
    list(value = list(0, 10, 3), warnings = rep(warning_text, 2L))
  )
  expect_null(.Call(
    native_domain_symbol("domain_sanitize_builtin"),
    two,
    list(-2, 20, 3)
  ))
  expect_identical(
    native_domain_capture(domain_sanitize(three, list(-2, 20))),
    list(value = list(0, 10, 0), warnings = warning_text)
  )

  # Unsupported list elements must fall back before the native routine emits
  # recycling warnings; otherwise users would see the native and R warnings.
  for (values in list(
    list(structure(-2, class = "numeric_subclass"), 20, 3),
    list("-2", "20", "3")
  )) {
    expect_identical(
      native_domain_capture(domain_sanitize(two, values)),
      native_domain_capture(paradox:::domain_sanitize.ParamDbl(two, values))
    )
  }

  character_values = c("-1", "0.5", "bad", "2")
  expect_identical(
    native_domain_capture(domain_sanitize(p_dbl(0, 1), character_values)),
    native_domain_capture(paradox:::domain_sanitize.ParamDbl(
      p_dbl(0, 1),
      character_values
    ))
  )
})

test_that("integer sanitization preserves R rounding and coercion semantics", {
  expect_identical(
    domain_sanitize(
      p_int(),
      list(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5, TRUE, NA_real_, NaN)
    ),
    list(-2L, -2L, 0L, 0L, 2L, 2L, 1L, NA_integer_, NA_integer_)
  )
  expect_identical(
    native_domain_capture(domain_sanitize(p_int(), list(-Inf, Inf))),
    list(
      value = list(NA_integer_, NA_integer_),
      warnings = "NAs introduced by coercion to integer range"
    )
  )
  expect_null(.Call(
    native_domain_symbol("domain_sanitize_builtin"),
    p_int(),
    list(-Inf, Inf)
  ))
  expect_identical(
    domain_sanitize(p_int(), structure(c(1.2, 1.8), names = c("x", "y"))),
    list(1L, 2L)
  )

  character_values = c("1.2", "bad", "2.8")
  expect_identical(
    native_domain_capture(domain_sanitize(p_int(), character_values)),
    native_domain_capture(paradox:::domain_sanitize.ParamInt(
      p_int(),
      character_values
    ))
  )
})

test_that("native quantile kernels preserve formulas, endpoints, and attributes", {
  double_domain = native_domain_bind(list(p_dbl(-1, 1), p_dbl(10, 20)))
  integer_domain = native_domain_bind(list(p_int(-2, 2), p_int(10, 12)))
  factor_domain = native_domain_bind(rep(list(p_fct(c("a", "b", "c"))), 2L))
  logical_domain = native_domain_bind(rep(list(p_lgl()), 2L))
  x = c(0, 0, 0.25, 0.5, 0.75, 1)

  expect_identical(
    domain_qunif(double_domain, x),
    paradox:::domain_qunif.ParamDbl(double_domain, x)
  )
  expect_identical(
    domain_qunif(integer_domain, x),
    paradox:::domain_qunif.ParamInt(integer_domain, x)
  )
  expect_identical(
    domain_qunif(factor_domain, x),
    paradox:::domain_qunif.ParamFct(factor_domain, x)
  )
  expect_identical(
    domain_qunif(logical_domain, x),
    paradox:::domain_qunif.ParamLgl(logical_domain, x)
  )

  named = structure(c(0, 0.5, 1), names = letters[1:3], extra = "kept")
  matrix = structure(
    matrix(c(0, 0.25, 0.75, 1), nrow = 2L),
    dimnames = list(c("r1", "r2"), c("c1", "c2")),
    extra = "kept"
  )
  for (value in list(named, matrix)) {
    expect_identical(
      domain_qunif(p_dbl(0, 1), value),
      paradox:::domain_qunif.ParamDbl(p_dbl(0, 1), value)
    )
    expect_identical(
      domain_qunif(p_lgl(), value),
      paradox:::domain_qunif.ParamLgl(p_lgl(), value)
    )
  }

  expect_type(domain_qunif(p_dbl(0, 1), 0:1), "double")
  expect_type(domain_qunif(p_int(0, 2), c(0, 0.5, 1)), "integer")
  expect_type(domain_qunif(p_fct(c("a", "b")), c(0, 1)), "character")
  expect_type(domain_qunif(p_lgl(), c(0, 1)), "logical")
  qunif_symbol = native_domain_symbol("domain_qunif_builtin")
  builtins = list(
    p_dbl(0, 1),
    p_int(0, 2),
    p_fct(c("a", "b")),
    p_lgl()
  )
  invalid_units = c(NA_real_, NaN, -Inf, Inf, -0.1, 1.1)
  for (domain in builtins) {
    for (unit in invalid_units) {
      expect_null(
        .Call(qunif_symbol, domain, unit),
        info = paste(class(domain)[[1L]], deparse(unit))
      )
    }
  }
  expect_identical(domain_qunif(p_dbl(4, 4), c(0, 0.5, 1)), rep(4, 3L))
  expect_identical(domain_qunif(p_int(4, 4), c(0, 0.5, 1)), rep(4L, 3L))
  expect_identical(
    domain_qunif(p_dbl(), c(0, 0.5, 1)),
    c(-Inf, NaN, Inf)
  )
  expect_identical(domain_qunif(p_int(), c(0, 0.5, 1)), rep(NA_integer_, 3L))

  expect_identical(
    domain_qunif(p_dbl(0, Inf), c(0, 0.5, 1)),
    c(0, Inf, Inf)
  )
  expect_identical(
    domain_qunif(p_dbl(-Inf, 1), c(0, 0.5, 1)),
    c(-Inf, -Inf, 1)
  )
  expect_identical(
    domain_qunif(p_dbl(Inf, Inf), c(0, 0.5, 1)),
    rep(Inf, 3L)
  )
  expect_identical(
    domain_qunif(p_dbl(-Inf, -Inf), c(0, 0.5, 1)),
    rep(-Inf, 3L)
  )

  expect_identical(.Call(qunif_symbol, p_int(0, Inf), 0), 0L)
  expect_identical(.Call(qunif_symbol, p_int(-Inf, 1), 1), 1L)
  expect_null(.Call(qunif_symbol, p_int(0, Inf), c(0, 0.5, 1)))
  expect_null(.Call(qunif_symbol, p_int(-Inf, 1), c(0, 0.5, 1)))

  wide_integer = p_int(0, 1)
  data.table::set(wide_integer, j = "upper", value = 3e9)
  expect_null(.Call(
    native_domain_symbol("domain_qunif_builtin"),
    wide_integer,
    1
  ))
  expect_identical(
    native_domain_capture(domain_qunif(wide_integer, 1)),
    native_domain_capture(paradox:::domain_qunif.ParamInt(wide_integer, 1))
  )
})

test_that("numeric quantile kernels preserve historical rounding boundaries", {
  symbol = native_domain_symbol("domain_qunif_builtin")

  # Apple clang contracts the unseparated affine expression to an ARM64 FMA.
  # This interior value distinguishes that result from the historical sequence
  # of R vector primitives by 64 ulps.
  double_domain = p_dbl(-10, 10)
  double_unit = 0.499
  double_expected = paradox:::domain_qunif.ParamDbl(
    double_domain,
    double_unit
  )
  expect_identical(double_expected, -0x1.47ae147ae14p-6)
  expect_identical(.Call(symbol, double_domain, double_unit), double_expected)

  # At an integer bucket boundary the same contraction is behavioral rather
  # than cosmetic: the fused value is one ulp below -1 and floor() selects -2.
  integer_domain = p_int(-2, 2)
  integer_unit = 0x1.9999999999999p-3
  integer_expected = paradox:::domain_qunif.ParamInt(
    integer_domain,
    integer_unit
  )
  expect_identical(integer_expected, -1L)
  expect_identical(.Call(symbol, integer_domain, integer_unit), integer_expected)
})

test_that("quantile dimension and empty-domain contracts remain explicit", {
  domain = native_domain_bind(rep(list(p_dbl(0, 1)), 2L))

  expect_identical(domain_qunif(domain, c(0, 0.25, 0.5, 1)), c(0, 0.25, 0.5, 1))
  expect_error(domain_qunif(domain, c(0, 0.25, 0.5)), "Must be TRUE")
  expect_identical(domain_qunif(domain[FALSE], numeric()), logical())
  expect_identical(domain_sanitize(p_dbl(0, 1), numeric()), numeric())
})

test_that("unknown Domain classes and ParamUty retain R and S3 dispatch", {
  class_name = "ParamNativeDomainKernelFallback"
  calls = new.env(parent = emptyenv())
  calls$check = 0L
  calls$qunif = 0L
  calls$sanitize = 0L

  registerS3method(
    "domain_check",
    class_name,
    function(param, values, internal = FALSE) {
      calls$check = calls$check + 1L
      "custom-check-result"
    },
    envir = asNamespace("paradox")
  )
  registerS3method(
    "domain_qunif",
    class_name,
    function(param, x) {
      calls$qunif = calls$qunif + 1L
      x + 10
    },
    envir = asNamespace("paradox")
  )
  registerS3method(
    "domain_sanitize",
    class_name,
    function(param, values) {
      calls$sanitize = calls$sanitize + 1L
      rev(values)
    },
    envir = asNamespace("paradox")
  )

  custom_domain = function() {
    paradox:::Domain(
      cls = class_name,
      grouping = class_name,
      storage_type = "numeric"
    )
  }
  custom = custom_domain()
  check_symbol = native_domain_symbol("domain_check_builtin")
  qunif_symbol = native_domain_symbol("domain_qunif_builtin")
  sanitize_symbol = native_domain_symbol("domain_sanitize_builtin")

  expect_false(.Call(check_symbol, custom, list(1)))
  expect_null(.Call(qunif_symbol, custom, c(0, 1)))
  expect_null(.Call(sanitize_symbol, custom, list(1, 2)))
  expect_identical(domain_check(custom, list(1)), "custom-check-result")
  expect_identical(domain_qunif(custom, c(0, 1)), c(10, 11))
  expect_identical(domain_sanitize(custom, list(1, 2)), list(2, 1))
  expect_identical(
    c(calls$check, calls$qunif, calls$sanitize),
    c(1L, 1L, 1L)
  )

  utility = p_uty()
  expect_false(.Call(check_symbol, utility, list(list(payload = 1))))
  expect_null(.Call(qunif_symbol, utility, 0.5))
  expect_null(.Call(sanitize_symbol, utility, list(list(payload = 1))))
  expect_true(domain_check(utility, list(list(payload = 1))))
  expect_identical(
    domain_sanitize(utility, list(list(payload = 1))),
    list(list(payload = 1))
  )
})

test_that("native Domain kernels reject corrupt built-in storage safely", {
  check_symbol = native_domain_symbol("domain_check_builtin")
  qunif_symbol = native_domain_symbol("domain_qunif_builtin")
  sanitize_symbol = native_domain_symbol("domain_sanitize_builtin")
  valid = structure(
    list(
      id = "x",
      cls = "ParamDbl",
      grouping = "ParamDbl",
      lower = 0,
      upper = 1,
      tolerance = 0.1
    ),
    class = "ParamDbl"
  )
  missing_id = unclass(valid)
  missing_id$id = NULL
  class(missing_id) = "ParamDbl"

  expect_false(.Call(check_symbol, valid, 0.5))
  expect_error(
    .Call(check_symbol, missing_id, list(0.5)),
    "has no `id` column",
    fixed = TRUE
  )
  expect_error(
    .Call(check_symbol, modifyList(valid, list(id = NA_character_)), list(0.5)),
    "`id` contains a missing value",
    fixed = TRUE
  )
  expect_error(
    .Call(check_symbol, modifyList(valid, list(cls = "ParamInt")), list(0.5)),
    "`cls` is inconsistent with its class",
    fixed = TRUE
  )
  expect_error(
    .Call(check_symbol, modifyList(valid, list(lower = TRUE)), list(0.5)),
    "`lower` must be numeric and have length 1",
    fixed = TRUE
  )
  expect_error(
    .Call(check_symbol, modifyList(valid, list(lower = NA_real_)), list(0.5)),
    "invalid numeric bounds or tolerance",
    fixed = TRUE
  )
  expect_error(
    .Call(check_symbol, modifyList(valid, list(tolerance = -1)), list(0.5)),
    "invalid numeric bounds or tolerance",
    fixed = TRUE
  )
  expect_error(
    .Call(qunif_symbol, modifyList(valid, list(upper = NA_real_)), 0.5),
    "invalid numeric bounds",
    fixed = TRUE
  )
  expect_error(
    .Call(sanitize_symbol, modifyList(valid, list(lower = NA_real_)), list(0.5)),
    "invalid numeric bounds",
    fixed = TRUE
  )

  factor = structure(
    list(
      id = "x",
      cls = "ParamFct",
      grouping = "factor",
      levels = list(c("a", "b"))
    ),
    class = "ParamFct"
  )
  expect_error(
    .Call(check_symbol, modifyList(factor, list(levels = "a")), list("a")),
    "`levels` must have type `list` and length 1",
    fixed = TRUE
  )
})

test_that("native Domain loops cross their interrupt-check interval", {
  size = 65537L
  domain = structure(
    list(
      id = sprintf("parameter_%05d", seq_len(size)),
      cls = rep("ParamDbl", size),
      grouping = rep("ParamDbl", size),
      lower = rep(0, size),
      upper = rep(1, size),
      tolerance = rep(0, size)
    ),
    class = "ParamDbl"
  )
  check_symbol = native_domain_symbol("domain_check_builtin")
  qunif_symbol = native_domain_symbol("domain_qunif_builtin")
  sanitize_symbol = native_domain_symbol("domain_sanitize_builtin")

  expect_true(.Call(check_symbol, domain, as.list(rep(0.5, size))))
  expect_identical(.Call(qunif_symbol, domain, rep(0.5, size)), rep(0.5, size))
  sanitized = .Call(sanitize_symbol, domain, rep(0.5, size))
  expect_length(sanitized, size)
  expect_true(all(vapply(sanitized, identical, logical(1L), 0.5)))

  # Neither nested loop reaches the interval on its own. Their combined work
  # does, which guards against resetting an interrupt counter for every row.
  nested_size = 257L
  choices = sprintf("choice_%03d", seq_len(nested_size))
  selected = choices[[nested_size]]
  factor_domain = structure(
    list(
      id = sprintf("factor_%03d", seq_len(nested_size)),
      cls = rep("ParamFct", nested_size),
      grouping = rep("factor-group", nested_size),
      levels = rep(list(choices), nested_size)
    ),
    class = "ParamFct"
  )
  expect_true(.Call(
    check_symbol,
    factor_domain,
    rep(list(selected), nested_size)
  ))
  expect_identical(
    .Call(qunif_symbol, factor_domain, rep(1, nested_size)),
    rep(selected, nested_size)
  )
})
