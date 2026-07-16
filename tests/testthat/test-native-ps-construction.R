native_ps_domains_symbol = function() {
  get("C_ps_builtin_domains", envir = asNamespace("paradox"))
}

native_ps_runtime_symbol = function() {
  get("C_ps_builtin_runtime", envir = asNamespace("paradox"))
}

native_ps_domains = function(...) {
  .Call(
    native_ps_domains_symbol(),
    substitute(list(...)),
    parent.frame()
  )
}

native_ps_private_snapshot = function(param_set) {
  private = param_set$.__enclos_env__$private
  normalize_table = function(table) {
    attr(table, ".internal.selfref") = NULL
    table
  }
  list(
    params = normalize_table(private$.params),
    tags = normalize_table(private$.tags),
    trafos = normalize_table(private$.trafos),
    deps = normalize_table(private$.deps),
    values = private$.values
  )
}

test_that("native ps construction is registered and runtime-sealed", {
  domains_symbol = native_ps_domains_symbol()
  runtime_symbol = native_ps_runtime_symbol()
  expect_s3_class(domains_symbol, "NativeSymbolInfo")
  expect_identical(domains_symbol$numParameters, 2L)
  expect_s3_class(runtime_symbol, "NativeSymbolInfo")
  expect_identical(runtime_symbol$numParameters, 2L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])

  ns = asNamespace("paradox")
  constructors = list(
    p_dbl = get("p_dbl", ns),
    p_int = get("p_int", ns),
    p_fct = get("p_fct", ns),
    p_lgl = get("p_lgl", ns)
  )
  expect_true(.Call(runtime_symbol, constructors, get("NO_DEF", ns)))
  changed = constructors
  changed$p_dbl = function(...) stop("replacement must not be admitted")
  expect_false(.Call(runtime_symbol, changed, get("NO_DEF", ns)))
  expect_true(.Call(runtime_symbol, constructors, get("NO_DEF", ns)))
})

test_that("literal built-ins produce anonymous canonical Domain rows", {
  domains = native_ps_domains(
    alpha = p_dbl(-2, upper = 3, tolerance = 1e-6,
      tags = c("numeric", "bounded")),
    beta = p_int(lower = -4L, 2L, tolerance = 0L,
      tags = "integer"),
    mode = p_fct(c("z", "a", "m"), tags = c("choice", "required")),
    flag = p_lgl(tags = "logical")
  )

  expect_named(domains, c("alpha", "beta", "mode", "flag"))
  expect_identical(
    vapply(domains, function(domain) class(domain)[[1L]], character(1L)),
    c(
      alpha = "ParamDbl", beta = "ParamInt",
      mode = "ParamFct", flag = "ParamLgl"
    )
  )
  expect_true(all(vapply(domains, inherits, logical(1L), "Domain")))
  expect_true(all(vapply(domains, function(domain) {
    is.null(attr(domain, "repr", exact = TRUE))
  }, logical(1L))))
  expect_identical(domains$alpha$id, "alpha")
  expect_identical(domains$alpha$lower, -2)
  expect_identical(domains$beta$lower, -4L)
  expect_identical(domains$beta$tolerance, 0L)
  expect_identical(domains$mode$levels[[1L]], c("z", "a", "m"))
  expect_identical(domains$flag$levels[[1L]], c(TRUE, FALSE))
})

test_that("native and historical ps construction agree exactly", {
  fast = ps(
    alpha = p_dbl(-2, upper = 3, tolerance = 1e-6,
      tags = c("numeric", "bounded")),
    beta = p_int(lower = -4L, 2L, tolerance = 0L,
      tags = "integer"),
    mode = p_fct(c("z", "a", "m"), tags = c("choice", "required")),
    empty = p_fct(character()),
    flag = p_lgl(tags = "logical")
  )
  historical = ps(
    alpha = paradox::p_dbl(-2, upper = 3, tolerance = 1e-6,
      tags = c("numeric", "bounded")),
    beta = paradox::p_int(lower = -4L, 2L, tolerance = 0L,
      tags = "integer"),
    mode = paradox::p_fct(c("z", "a", "m"),
      tags = c("choice", "required")),
    empty = paradox::p_fct(character()),
    flag = paradox::p_lgl(tags = "logical")
  )

  expect_identical(fast$ids(), historical$ids())
  expect_identical(fast$class, historical$class)
  expect_identical(fast$lower, historical$lower)
  expect_identical(fast$upper, historical$upper)
  expect_identical(fast$levels, historical$levels)
  expect_identical(fast$storage_type, historical$storage_type)
  expect_identical(fast$special_vals, historical$special_vals)
  expect_identical(fast$default, historical$default)
  expect_identical(fast$tags, historical$tags)
  expect_identical(fast$values, historical$values)
  expect_identical(as.list(fast$deps), as.list(historical$deps))
  expect_identical(
    native_ps_private_snapshot(fast),
    native_ps_private_snapshot(historical)
  )
})

test_that("unsupported call surfaces decline before argument evaluation", {
  calls = 0L
  expect_null(native_ps_domains(
    x = p_dbl(lower = {
      calls = calls + 1L
      0
    })
  ))
  expect_identical(calls, 0L)

  result = ps(
    x = p_dbl(lower = {
      calls = calls + 1L
      0
    })
  )
  expect_identical(result$lower, c(x = 0))
  expect_identical(calls, 1L)

  calls = 0L
  expect_null(native_ps_domains(
    x = p_dbl(
      lower = {
        calls = calls + 1L
        0
      },
      default = 0
    )
  ))
  expect_null(native_ps_domains(
    x = p_dbl(
      lower = {
        calls = calls + 1L
        0
      },
      special_vals = list(2)
    )
  ))
  expect_null(native_ps_domains(
    x = p_dbl(
      lower = {
        calls = calls + 1L
        1
      },
      logscale = TRUE
    )
  ))
  expect_null(native_ps_domains(
    x = p_dbl(
      lower = {
        calls = calls + 1L
        0
      },
      trafo = exp
    )
  ))
  expect_null(native_ps_domains(
    x = p_dbl(
      lower = {
        calls = calls + 1L
        0
      },
      depends = y == 1
    )
  ))
  expect_identical(calls, 0L)
})

test_that("constructor overrides and active bindings are never bypassed", {
  counter = new.env(parent = emptyenv())
  counter$constructor = 0L
  counter$argument = 0L
  override = new.env(parent = environment())
  override$p_dbl = function(...) {
    counter$constructor = counter$constructor + 1L
    paradox::p_dbl(...)
  }

  expect_null(evalq(native_ps_domains(
    x = p_dbl(lower = {
      counter$argument = counter$argument + 1L
      0
    })
  ), override))
  expect_identical(counter$constructor, 0L)
  expect_identical(counter$argument, 0L)

  result = evalq(ps(
    x = p_dbl(lower = {
      counter$argument = counter$argument + 1L
      0
    })
  ), override)
  expect_identical(result$lower, c(x = 0))
  expect_identical(counter$constructor, 1L)
  expect_identical(counter$argument, 1L)

  counter$active = 0L
  active = new.env(parent = environment())
  makeActiveBinding("p_dbl", function() {
    counter$active = counter$active + 1L
    paradox::p_dbl
  }, active)
  expect_null(evalq(native_ps_domains(x = p_dbl(0, 1)), active))
  expect_identical(counter$active, 0L)
  expect_identical(evalq(ps(x = p_dbl(0, 1))$lower, active), c(x = 0))
  expect_identical(counter$active, 1L)
})

test_that("fallback evaluates parameter calls once from left to right", {
  seen = character()
  result = ps(
    first = p_dbl(lower = {
      seen = c(seen, "first")
      0
    }),
    second = p_int(lower = {
      seen = c(seen, "second")
      0L
    })
  )
  expect_identical(seen, c("first", "second"))
  expect_identical(result$lower, c(first = 0, second = 0))
})

test_that("invalid, partial, and feature-rich calls retain R behavior", {
  expect_identical(
    suppressWarnings(ps(x = p_dbl(low = 0, upp = 1))$lower),
    suppressWarnings(ps(x = paradox::p_dbl(low = 0, upp = 1))$lower)
  )
  expect_identical(
    ps(x = p_lgl(default = TRUE))$default,
    ps(x = paradox::p_lgl(default = TRUE))$default
  )
  expect_identical(
    ps(x = p_int(0, 4, init = 2L))$values,
    ps(x = paradox::p_int(0, 4, init = 2L))$values
  )
  expect_identical(
    ps(x = p_dbl(1, 10, logscale = TRUE))$lower,
    ps(x = paradox::p_dbl(1, 10, logscale = TRUE))$lower
  )

  capture = function(expr) {
    tryCatch(
      {
        force(expr)
        NA_character_
      },
      error = conditionMessage
    )
  }
  expect_identical(
    capture(ps(x = p_dbl(2, 1))),
    capture(ps(x = paradox::p_dbl(2, 1)))
  )
  expect_identical(
    capture(ps(x = p_fct(c("a", "a")))),
    capture(ps(x = paradox::p_fct(c("a", "a"))))
  )
})

test_that("ps control fields still use the normal R6 instance", {
  extra = function(x, param_set) x
  constraint = function(x) TRUE
  space = ps(
    x = p_dbl(0, 1),
    y = p_lgl(),
    .extra_trafo = extra,
    .constraint = constraint
  )
  expect_s3_class(space, "ParamSet")
  expect_identical(space$extra_trafo, extra)
  expect_identical(space$constraint, constraint)
  expect_identical(class(space), c("ParamSet", "R6"))
})
