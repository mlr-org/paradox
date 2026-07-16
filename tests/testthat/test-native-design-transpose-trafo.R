design_logscale_trafo_symbol = function() {
  get(
    "C_design_transpose_logscale_builtin",
    envir = asNamespace("paradox")
  )
}

design_logscale_rows = function(data, filter_na) {
  native = get("C_design_transpose", envir = asNamespace("paradox"))
  rows = .Call(native, data, filter_na)
  if (is.null(rows)) {
    rows = transpose_list(data)
    if (filter_na) {
      rows = map(rows, function(row) Filter(Negate(is_scalar_na), row))
    }
  }
  rows
}

design_logscale_legacy = function(data, param_set, filter_na) {
  map(
    design_logscale_rows(data, filter_na),
    function(row) param_set$trafo(row)
  )
}

test_that("native Design logscale trafo is registered and exact", {
  routines = getDLLRegisteredRoutines("paradox")$.Call
  expect_identical(
    routines$design_transpose_logscale_builtin$numParameters,
    2L
  )

  parameter_set = ps(
    double = p_dbl(1e-6, 1e3, logscale = TRUE),
    integer = p_int(0, 100, logscale = TRUE),
    plain = p_lgl()
  )
  data = data.table(
    double = c(log(1e-6), 0, log(1e3), NA_real_, NaN, Inf),
    integer = c(log(0.5), log(2), log(101), NA_real_, NaN, log(5)),
    plain = c(TRUE, FALSE, TRUE, FALSE, NA, TRUE)
  )
  design = Design$new(parameter_set, copy(data), remove_dupl = FALSE)
  symbol = design_logscale_trafo_symbol()

  for (filter_na in c(FALSE, TRUE)) {
    rows = design_logscale_rows(design$data, filter_na)
    before = unserialize(serialize(rows, NULL))
    native = .Call(symbol, rows, parameter_set)
    expected = design_logscale_legacy(
      design$data,
      parameter_set,
      filter_na
    )
    expect_identical(native, expected)
    expect_identical(rows, before)
    expect_identical(
      design$transpose(filter_na = filter_na, trafo = TRUE),
      expected
    )
  }

  rows = design_logscale_rows(design$data, FALSE)
  native = .Call(symbol, rows, parameter_set)
  names(native[[1L]])[[1L]] = "changed"
  expect_identical(names(rows[[1L]]), c("double", "integer", "plain"))
  expect_identical(names(native[[2L]]), c("double", "integer", "plain"))
})

test_that("native Design logscale lane never evaluates arbitrary callbacks", {
  symbol = design_logscale_trafo_symbol()
  calls = 0L
  custom = function(value) {
    calls <<- calls + 1L
    value + 1
  }
  parameter_set = ps(
    logscale = p_dbl(1, 10, logscale = TRUE),
    custom = p_dbl(0, 1, trafo = custom)
  )
  data = data.table(logscale = c(0, 1), custom = c(0.25, 0.75))
  design = Design$new(parameter_set, copy(data), remove_dupl = FALSE)
  rows = design_logscale_rows(design$data, FALSE)

  expect_null(.Call(symbol, rows, parameter_set))
  expect_identical(calls, 0L)
  expected = design_logscale_legacy(design$data, parameter_set, FALSE)
  expect_identical(calls, 2L)
  calls = 0L
  expect_identical(design$transpose(), expected)
  expect_identical(calls, 2L)

  user_exp = ps(x = p_dbl(-1, 1, trafo = exp))
  user_rows = list(list(x = 0))
  expect_null(.Call(symbol, user_rows, user_exp))

  extra = ps(
    x = p_dbl(1, 10, logscale = TRUE),
    .extra_trafo = function(x) c(x, list(extra = TRUE))
  )
  expect_null(.Call(symbol, list(list(x = 0)), extra))
  expect_identical(
    Design$new(extra, data.table(x = 0), remove_dupl = FALSE)$transpose(),
    list(list(x = 1, extra = TRUE))
  )
})

test_that("native integer logscale admission rejects executable bindings", {
  symbol = design_logscale_trafo_symbol()

  active = ps(x = p_int(0, 100, logscale = TRUE))
  active_callback = active$.__enclos_env__$private$.trafos$trafo[[1L]]
  active_environment = environment(active_callback)
  active_calls = 0L
  rm("lower", envir = active_environment)
  makeActiveBinding("lower", function(value) {
    active_calls <<- active_calls + 1L
    if (!missing(value)) stop("unexpected active-binding write")
    0
  }, active_environment)
  expect_null(.Call(symbol, list(list(x = 0)), active))
  expect_identical(active_calls, 0L)

  delayed = ps(x = p_int(0, 100, logscale = TRUE))
  delayed_callback = delayed$.__enclos_env__$private$.trafos$trafo[[1L]]
  delayed_environment = environment(delayed_callback)
  delayed_calls = 0L
  rm("upper", envir = delayed_environment)
  delayedAssign("upper", {
    delayed_calls <<- delayed_calls + 1L
    100
  }, assign.env = delayed_environment)
  expect_null(.Call(symbol, list(list(x = 0)), delayed))
  expect_identical(delayed_calls, 0L)
})

test_that("native Design logscale lane fails closed on altered surfaces", {
  symbol = design_logscale_trafo_symbol()
  parameter_set = ps(x = p_dbl(1, 10, logscale = TRUE))
  rows = list(list(x = 0))

  cargo_changed = parameter_set$clone(deep = TRUE)
  cargo_changed$.__enclos_env__$private$.params$cargo[[1L]]$logscale = NULL
  expect_null(.Call(symbol, rows, cargo_changed))
  expect_identical(cargo_changed$trafo(rows[[1L]]), list(x = 1))

  callback_changed = parameter_set$clone(deep = TRUE)
  callback_changed$.__enclos_env__$private$.trafos$trafo[[1L]] = identity
  expect_null(.Call(symbol, rows, callback_changed))

  method_changed = parameter_set$clone(deep = TRUE)
  unlockBinding("trafo", method_changed)
  method_changed$trafo = function(x, param_set = method_changed) {
    list(overridden = TRUE)
  }
  lockBinding("trafo", method_changed)
  expect_null(.Call(symbol, rows, method_changed))

  expect_null(.Call(symbol, structure(rows, note = TRUE), parameter_set))
  expect_null(.Call(
    symbol,
    list(structure(list(x = 0), class = "custom")),
    parameter_set
  ))
  expect_null(.Call(
    symbol,
    list(list(x = structure(0, class = "custom"))),
    parameter_set
  ))

  unicode_rows = list(setNames(list(0), "xθ"))
  expect_null(.Call(symbol, unicode_rows, parameter_set))
  expect_identical(
    map(unicode_rows, function(row) parameter_set$trafo(row)),
    unicode_rows
  )
})

test_that("integer logscale warning cases retain the R callback", {
  symbol = design_logscale_trafo_symbol()
  parameter_set = ps(x = p_int(0, Inf, logscale = TRUE))
  data = data.table(x = 1000)
  design = Design$new(parameter_set, data, remove_dupl = FALSE)
  rows = design_logscale_rows(design$data, FALSE)

  expect_null(.Call(symbol, rows, parameter_set))
  warnings = character()
  result = withCallingHandlers(
    design$transpose(),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(warnings, "NAs introduced by coercion to integer range")
  expect_identical(result, list(list(x = NA_integer_)))
})
