native_paramset_check_symbol = function(table = FALSE) {
  get(
    if (table) "C_param_set_check_dt_builtin" else "C_param_set_check_builtin",
    envir = asNamespace("paradox")
  )
}

native_paramset_check_complete_symbol = function(all_params = FALSE) {
  get(
    if (all_params) {
      "C_param_set_check_dt_all_builtin"
    } else {
      "C_param_set_check_dt_complete_builtin"
    },
    envir = asNamespace("paradox")
  )
}

native_paramset_check_plan_symbol = function() {
  get("C_param_set_check_dt_plan_builtin", envir = asNamespace("paradox"))
}

native_paramset_check_params = function(param_set) {
  param_set$.__enclos_env__$private$.params
}

native_paramset_check_space = function(special = FALSE) {
  ParamSet$new(list(
    double = p_dbl(
      -10,
      10,
      tolerance = 1e-6,
      special_vals = if (special) list("AUTO") else list()
    ),
    integer = p_int(-20L, 20L, tolerance = 1e-6),
    factor = p_fct(c("slow", "fast")),
    logical = p_lgl()
  ))
}

test_that("native ParamSet validity gates are registered with forced symbols", {
  routines = c(
    "param_set_check_builtin",
    "param_set_check_dt_builtin",
    "param_set_check_dt_plan_builtin",
    "param_set_check_dt_complete_builtin",
    "param_set_check_dt_all_builtin"
  )
  arities = c(3L, 2L, 2L, 2L, 2L)

  for (index in seq_along(routines)) {
    symbol = get(paste0("C_", routines[[index]]), envir = asNamespace("paradox"))
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, arities[[index]])
    if (index == 1L) {
      expect_error(
        .Call(routines[[index]], list(), list(), FALSE, PACKAGE = "paradox"),
        "not available"
      )
    } else {
      expect_error(
        .Call(routines[[index]], list(), list(), PACKAGE = "paradox"),
        "not available"
      )
    }
  }
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native scalar gate validates and sanitizes canonical built-ins", {
  param_set = native_paramset_check_space()
  params = native_paramset_check_params(param_set)
  symbol = native_paramset_check_symbol()

  values = list(
    factor = "fast",
    double = -10 - 5e-7,
    logical = FALSE,
    integer = 10.0000005
  )
  expect_identical(.Call(symbol, params, values, FALSE), TRUE)

  before = TRUE
  result = .Call(symbol, params, values, TRUE)
  expected = param_set$check(values, sanitize = TRUE)
  expect_identical(result, expected)
  expect_identical(
    attr(result, "sanitized"),
    list(factor = "fast", double = -10, logical = FALSE, integer = 10L)
  )
  # R may implement ScalarLogical(TRUE) with a shared singleton. The native
  # attributed result must never mutate it.
  expect_identical(before, TRUE)
  expect_null(attributes(before))
  expect_null(attributes(TRUE))

  integer_input = list(integer = 3L, double = 2L)
  integer_result = .Call(symbol, params, integer_input, TRUE)
  expect_type(attr(integer_result, "sanitized")$integer, "integer")
  expect_type(attr(integer_result, "sanitized")$double, "double")
})

test_that("scalar boundaries and unsupported inputs retain the R fallback", {
  param_set = native_paramset_check_space()
  params = native_paramset_check_params(param_set)
  symbol = native_paramset_check_symbol()

  fallback_cases = list(
    list(double = NA_real_),
    list(double = NaN),
    list(double = NULL),
    list(double = TRUE),
    list(double = structure(0.5, class = "native_check_number")),
    list(integer = Inf),
    list(integer = 1.5),
    list(factor = factor("slow")),
    list(factor = "absent"),
    list(logical = NA),
    list(logical = 1L),
    list(unknown = 1)
  )
  for (values in fallback_cases) {
    expect_null(.Call(symbol, params, values, FALSE), info = deparse(values))
  }

  expect_identical(
    param_set$check(list(double = NA_real_)),
    "double: May not be NA"
  )
  expect_match(
    param_set$check(list(integer = 1.5)),
    "single integerish value",
    fixed = TRUE
  )
  expect_match(
    param_set$check(list(factor = "absent")),
    "Must be element of set",
    fixed = TRUE
  )
  expect_match(
    param_set$check(list(unknown = 1)),
    "not available",
    fixed = TRUE
  )
  expect_false(isTRUE(param_set$check(list(double = NULL))))

  # Exact decimal tolerance endpoints can require checkmate's diagnostic path
  # because of binary rounding. Returning the sentinel remains correct.
  tolerance_edge = list(integer = 20.000001)
  expect_true(param_set$check(tolerance_edge))
  native_edge = .Call(symbol, params, tolerance_edge, FALSE)
  expect_true(isTRUE(native_edge) || is.null(native_edge))
})

test_that("special values, utility values, custom Domains, and tokens fall back", {
  symbol = native_paramset_check_symbol()
  special_set = native_paramset_check_space(special = TRUE)
  expect_null(.Call(
    symbol,
    native_paramset_check_params(special_set),
    list(double = 0.5),
    FALSE
  ))
  expect_true(special_set$check(list(double = 0.5)))
  expect_true(special_set$check(list(double = "AUTO")))

  null_special_set = ps(double = p_dbl(0, 1, special_vals = list(NULL)))
  expect_null(.Call(
    symbol,
    native_paramset_check_params(null_special_set),
    list(double = NULL),
    FALSE
  ))
  expect_true(null_special_set$check(list(double = NULL)))

  utility_calls = 0L
  utility_set = ps(value = p_uty(custom_check = function(value) {
    utility_calls <<- utility_calls + 1L
    if (identical(value, list(ok = TRUE))) TRUE else "utility rejected"
  }))
  # p_uty() probes the checker once at construction time.
  utility_calls = 0L
  expect_null(.Call(
    symbol,
    native_paramset_check_params(utility_set),
    list(value = list(ok = TRUE)),
    FALSE
  ))
  expect_true(utility_set$check(list(value = list(ok = TRUE))))
  expect_identical(utility_calls, 1L)

  custom_calls = 0L
  make_custom_domain = function() {
    paradox:::Domain(
      cls = "ParamNativeCheckCustom",
      grouping = "native-check-custom",
      storage_type = "numeric"
    )
  }
  registerS3method(
    "domain_check",
    "ParamNativeCheckCustom",
    function(param, values, internal = FALSE) {
      custom_calls <<- custom_calls + 1L
      TRUE
    },
    envir = asNamespace("paradox")
  )
  custom_set = ParamSet$new(list(custom = make_custom_domain()))
  expect_null(.Call(
    symbol,
    native_paramset_check_params(custom_set),
    list(custom = 1),
    FALSE
  ))
  expect_true(custom_set$check(list(custom = 1)))
  expect_identical(custom_calls, 1L)

  token_set = ps(double = p_dbl(0, 1))
  token = to_tune(0, 1)
  expect_null(.Call(
    symbol,
    native_paramset_check_params(token_set),
    list(double = token),
    FALSE
  ))
  expect_true(token_set$check(list(double = token)))
})

test_that("strict constraints and dependencies are never bypassed", {
  constrained = ps(x = p_dbl(0, 1), y = p_lgl())
  constraint_calls = 0L
  constrained$constraint = function(x) {
    constraint_calls <<- constraint_calls + 1L
    x$x <= 0.5
  }

  expect_identical(
    constrained$check(list(x = 0.75, y = TRUE), check_strict = TRUE),
    "Constraint not fulfilled."
  )
  expect_identical(constraint_calls, 1L)
  expect_true(constrained$check(list(x = 0.75, y = TRUE), check_strict = FALSE))
  expect_identical(constraint_calls, 1L)
  expect_identical(
    constrained$check_dt(data.frame(x = 0.75, y = TRUE)),
    "Constraint not fulfilled."
  )
  expect_identical(constraint_calls, 2L)

  dependent = ps(
    parent = p_lgl(),
    child = p_int(0, 2, depends = parent == TRUE)
  )
  expect_match(
    dependent$check(list(parent = FALSE, child = 1L), check_strict = TRUE),
    "parent == TRUE",
    fixed = TRUE
  )
  expect_true(dependent$check(
    list(parent = FALSE, child = 1L),
    check_strict = FALSE
  ))

  NativeCheckSubclass = R6::R6Class(
    "NativeCheckSubclass",
    inherit = ParamSet,
    public = list(
      test_constraint = function(x, assert_value = TRUE) FALSE
    )
  )
  subclass = NativeCheckSubclass$new(list(x = p_dbl(0, 1)))
  expect_identical(
    subclass$check(list(x = 0.5), check_strict = TRUE),
    "Constraint not fulfilled."
  )
  expect_identical(
    subclass$check_dt(data.frame(x = 0.5), check_strict = TRUE),
    "Constraint not fulfilled."
  )
})

test_that("base constraints retain their established call and callback frame", {
  observed = NULL
  param_set = ps(x = p_dbl(0, 1))
  param_set$constraint = function(x) {
    callback_frame = parent.frame()
    observed <<- list(
      call = sys.call(),
      parent_names = sort(ls(callback_frame, all.names = TRUE)),
      argument_expression = substitute(x, callback_frame)
    )
    stop("constraint introspection sentinel")
  }

  condition = tryCatch(
    param_set$test_constraint(list(x = 0.5), assert_value = FALSE),
    error = identity
  )
  expect_s3_class(condition, "error")
  expect_identical(conditionCall(condition), quote(private$.constraint(x)))
  expect_identical(observed, list(
    call = quote(private$.constraint(x)),
    parent_names = c("assert_value", "private", "self", "super", "x"),
    argument_expression = quote(x)
  ))
})

test_that("ordinary constraint checks ignore replaced public getters", {
  param_set = ps(x = p_dbl(0, 1))
  private = param_set$.__enclos_env__$private
  calls = 0L
  original = activeBindingFunction("constraint", param_set)
  on.exit(makeActiveBinding("constraint", original, param_set), add = TRUE)
  makeActiveBinding("constraint", function(value) {
    if (!missing(value)) stop("replacement constraint is read-only")
    calls <<- calls + 1L
    function(x) FALSE
  }, param_set)

  expect_false(param_set$has_constraint)
  expect_true(param_set$test_constraint(
    list(x = 0.5),
    assert_value = FALSE
  ))
  expect_identical(calls, 0L)

  private$.constraint = function(x) TRUE
  expect_true(param_set$has_constraint)
  expect_true(param_set$test_constraint(
    list(x = 0.5),
    assert_value = FALSE
  ))
  expect_identical(calls, 0L)
})

test_that("native check admission rejects replaced public methods", {
  replace_method = function(param_set, name, replacement) {
    unlockBinding(name, param_set)
    assign(name, replacement, envir = param_set)
    lockBinding(name, param_set)
    invisible(param_set)
  }

  constrained = ps(x = p_dbl(0, 1))
  replace_method(
    constrained,
    "test_constraint",
    function(x, assert_value = TRUE) FALSE
  )
  expect_identical(
    constrained$check(list(x = 0.5)),
    "Constraint not fulfilled."
  )

  dependent = ps(x = p_dbl(0, 1))
  replace_method(
    dependent,
    "check_dependencies",
    function(xs) "replacement dependency result"
  )
  expect_identical(
    dependent$check(list(x = 0.5)),
    "replacement dependency result"
  )

  table_checked = ps(x = p_dbl(0, 1))
  replace_method(
    table_checked,
    "check",
    function(...) "replacement row check"
  )
  expect_identical(
    table_checked$check_dt(data.frame(x = 0.5)),
    "replacement row check"
  )
})

test_that("surface admission rejects delayed methods without forcing them", {
  symbol = getDLLRegisteredRoutines(
    getLoadedDLLs()[["paradox"]]
  )$.Call[["param_set_surface_auth"]]

  scalar = ps(x = p_dbl(0, 1))
  scalar_state = new.env(parent = emptyenv())
  scalar_state$forced = 0L
  unlockBinding("test_constraint", scalar)
  delayedAssign(
    "test_constraint",
    {
      scalar_state$forced = scalar_state$forced + 1L
      function(x, assert_value = TRUE) FALSE
    },
    assign.env = scalar
  )
  lockBinding("test_constraint", scalar)
  expect_false(.Call(symbol, scalar, 2L))
  expect_identical(scalar_state$forced, 0L)
  expect_true(scalar$check(list(x = 0.5), check_strict = FALSE))
  expect_identical(scalar_state$forced, 0L)
  expect_identical(scalar$check(list(x = 0.5)), "Constraint not fulfilled.")
  expect_identical(scalar_state$forced, 1L)

  table = ps(x = p_dbl(0, 1))
  table_state = new.env(parent = emptyenv())
  table_state$forced = 0L
  unlockBinding("check", table)
  delayedAssign(
    "check",
    {
      table_state$forced = table_state$forced + 1L
      function(...) "replacement delayed check"
    },
    assign.env = table
  )
  lockBinding("check", table)
  expect_false(.Call(symbol, table, 3L))
  expect_identical(table_state$forced, 0L)
  expect_true(table$check_dt(data.frame(x = numeric())))
  expect_identical(table_state$forced, 0L)
  expect_identical(
    table$check_dt(data.frame(x = 0.5)),
    "replacement delayed check"
  )
  expect_identical(table_state$forced, 1L)
})

test_that("private check stores retain their established fallback force order", {
  delay_private = function(param_set, name, events) {
    private = param_set$.__enclos_env__$private
    value = private[[name]]
    evaluation_environment = new.env(parent = baseenv())
    evaluation_environment$events = events
    evaluation_environment$name = name
    evaluation_environment$value = value
    delayedAssign(
      name,
      {
        events$value = c(events$value, name)
        value
      },
      assign.env = private,
      eval.env = evaluation_environment
    )
    invisible(param_set)
  }
  delayed_check_space = function(events) {
    param_set = ps(x = p_dbl(0, 1))
    for (name in c(".params", ".constraint", ".deps")) {
      delay_private(param_set, name, events)
    }
    param_set
  }
  symbol = getDLLRegisteredRoutines(
    getLoadedDLLs()[["paradox"]]
  )$.Call[["param_set_surface_auth"]]

  empty_events = new.env(parent = emptyenv())
  empty_events$value = character()
  empty = ps(x = p_dbl(0, 1, tags = "required"))
  delay_private(empty, ".params", empty_events)
  expect_true(empty$check(list(), presence = "none"))
  expect_identical(empty_events$value, character())
  expect_identical(
    empty$check(list(), presence = "all"),
    "All parameters must be present. Missing parameters: x"
  )
  expect_identical(empty_events$value, ".params")

  required_events = new.env(parent = emptyenv())
  required_events$value = character()
  required = ps(x = p_dbl(0, 1, tags = "required"))
  delay_private(required, ".params", required_events)
  expect_identical(
    required$check(list(), presence = "required"),
    "All parameters must be present. Missing parameters: x"
  )
  expect_identical(required_events$value, ".params")

  scalar_events = new.env(parent = emptyenv())
  scalar_events$value = character()
  scalar = delayed_check_space(scalar_events)
  expect_false(.Call(symbol, scalar, 2L))
  expect_identical(scalar_events$value, character())
  expect_true(scalar$check(list(x = 0.5), check_strict = FALSE))
  expect_identical(scalar_events$value, ".params")
  expect_true(scalar$check(list(x = 0.5), check_strict = TRUE))
  expect_identical(
    scalar_events$value,
    c(".params", ".constraint", ".deps")
  )

  table_events = new.env(parent = emptyenv())
  table_events$value = character()
  table = delayed_check_space(table_events)
  expect_false(.Call(symbol, table, 3L))
  expect_identical(table_events$value, character())
  expect_true(table$check_dt(data.frame(x = numeric())))
  expect_identical(table_events$value, character())
  expect_true(table$check_dt(data.frame(x = 0.5)))
  expect_identical(
    table_events$value,
    c(".params", ".constraint", ".deps")
  )
})

test_that("native table gate validates column-wise and skips missing cells", {
  param_set = native_paramset_check_space()
  params = native_paramset_check_params(param_set)
  symbol = native_paramset_check_symbol(table = TRUE)

  values = data.frame(
    logical = c(TRUE, NA, FALSE),
    factor = c("slow", NA, "fast"),
    integer = c(-20, NA, 20.0000005),
    double = c(-10 - 5e-7, NaN, 10 + 5e-7),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_identical(.Call(symbol, params, values), TRUE)
  expect_identical(param_set$check_dt(values), TRUE)

  all_missing_wrong_storage = data.frame(
    double = c(NA_character_, NA_character_),
    integer = c(NA_character_, NA_character_),
    factor = c(NA_integer_, NA_integer_),
    logical = c(NA_character_, NA_character_),
    check.names = FALSE
  )
  expect_identical(.Call(symbol, params, all_missing_wrong_storage), TRUE)
  expect_identical(param_set$check_dt(all_missing_wrong_storage), TRUE)

  expect_identical(.Call(symbol, params, data.frame()), TRUE)
  zero_rows = values[FALSE, , drop = FALSE]
  zero_rows$unknown = numeric()
  expect_identical(.Call(symbol, params, zero_rows), TRUE)
  expect_identical(param_set$check_dt(zero_rows), TRUE)
})

test_that("native table completeness gates distinguish cells and parameters", {
  param_set = native_paramset_check_space()
  params = native_paramset_check_params(param_set)
  complete_symbol = native_paramset_check_complete_symbol()
  all_symbol = native_paramset_check_complete_symbol(all_params = TRUE)
  values = data.frame(
    logical = c(TRUE, FALSE),
    factor = c("slow", "fast"),
    integer = c(-20L, 20L),
    double = c(-10, 10),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_identical(.Call(complete_symbol, params, values), TRUE)
  expect_identical(.Call(all_symbol, params, values), TRUE)
  expect_identical(param_set$check_dt(values), TRUE)
  expect_identical(param_set$check_dt(values, presence = "all"), TRUE)
  expect_identical(
    param_set$check_dt(values[c("double", "logical")]),
    TRUE
  )

  subset = values[c("double", "logical")]
  expect_identical(.Call(complete_symbol, params, subset), TRUE)
  expect_null(.Call(all_symbol, params, subset))

  missing = values
  missing$integer[[2L]] = NA_integer_
  expect_null(.Call(complete_symbol, params, missing))
  expect_null(.Call(all_symbol, params, missing))
  expect_identical(param_set$check_dt(missing), TRUE)
  expect_match(
    param_set$check_dt(missing, presence = "all"),
    "All parameters must be present",
    fixed = TRUE
  )
})

test_that("native table plan records completeness in its first value pass", {
  param_set = native_paramset_check_space()
  params = native_paramset_check_params(param_set)
  symbol = native_paramset_check_plan_symbol()
  values = data.frame(
    double = c(0, NA_real_),
    integer = c(0L, 1L),
    factor = c("slow", "fast"),
    logical = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  expect_identical(.Call(symbol, params, values), 2L)
  expect_identical(.Call(symbol, params, values[-1L]), 1L)
  values$double[[2L]] = 1
  expect_identical(.Call(symbol, params, values), 3L)
})

test_that("invalid and exotic tables fall back with first-row diagnostics", {
  param_set = native_paramset_check_space()
  params = native_paramset_check_params(param_set)
  symbol = native_paramset_check_symbol(table = TRUE)

  bad = data.frame(
    double = c(0, 0),
    integer = c(0, 21),
    factor = c("absent", "slow"),
    logical = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  expect_null(.Call(symbol, params, bad))
  # Row-major fallback must report the first row's factor failure, not the
  # later integer failure found earlier by a column scan.
  expect_match(
    param_set$check_dt(bad),
    "factor: Must be element of set",
    fixed = TRUE
  )

  exotic = transform(bad, factor = factor(factor))
  expect_null(.Call(symbol, params, exotic))
  expect_match(param_set$check_dt(exotic), "types do not match", fixed = TRUE)

  extra = data.frame(double = 0, unknown = 1)
  expect_null(.Call(symbol, params, extra))
  expect_match(param_set$check_dt(extra), "not available", fixed = TRUE)

  duplicated = data.frame(double = 0, integer = 1L, check.names = FALSE)
  names(duplicated) = c("double", "double")
  expect_null(.Call(symbol, params, duplicated))
  expect_match(param_set$check_dt(duplicated), "unique", fixed = TRUE)

  special_set = native_paramset_check_space(special = TRUE)
  special_table = data.frame(double = c(0.5, NA))
  expect_null(.Call(
    symbol,
    native_paramset_check_params(special_set),
    special_table
  ))
  expect_true(special_set$check_dt(special_table))
})

test_that("empty and malformed tables retain the permissive upstream contract", {
  param_set = ps(x = p_dbl(0, 1))

  zero_column_three_rows = structure(
    list(),
    class = "data.frame",
    row.names = .set_row_names(3L)
  )
  zero_row_duplicate = structure(
    list(numeric(), numeric()),
    names = c("x", "x"),
    class = "data.frame",
    row.names = integer()
  )
  zero_row_extra = data.frame(extra = numeric())

  for (input in list(
    data.frame(),
    list(),
    structure(list(), names = character()),
    zero_column_three_rows,
    data.frame(x = numeric()),
    zero_row_extra,
    zero_row_duplicate,
    list(extra = numeric())
  )) {
    expect_identical(param_set$check_dt(input), TRUE)
  }

  for (arguments in list(
    list(check_strict = NA),
    list(check_strict = "invalid"),
    list(presence = "invalid"),
    list(presence = c("none", "all")),
    list(allow_token = NA),
    list(allow_token = "invalid")
  )) {
    expect_identical(
      do.call(param_set$check_dt, c(list(data.frame()), arguments)),
      TRUE
    )
  }

  expect_error(param_set$check_dt(NULL), "check_list")
  expect_error(param_set$check_dt(numeric()), "check_list")
  expect_error(
    param_set$check_dt(matrix(numeric(), nrow = 0, ncol = 1)),
    "check_list"
  )

  malformed_mismatched = structure(
    list(x = c(0.5, 0.6), extra = 0.1),
    class = "data.frame",
    row.names = .set_row_names(2L)
  )
  expect_match(
    param_set$check_dt(malformed_mismatched),
    "not available",
    fixed = TRUE
  )
})

test_that("native check_dt preserves optional-argument laziness and order", {
  param_set = ps(x = p_dbl(0, 1))

  error_call = function(expr) {
    condition = tryCatch(
      {
        force(expr)
        NULL
      },
      error = identity
    )
    expect_s3_class(condition, "error")
    paste(deparse(conditionCall(condition)), collapse = "\n")
  }

  observe = function(table) {
    state = new.env(parent = emptyenv())
    state$events = character()
    mark = function(name, value) {
      state$events = c(state$events, name)
      value
    }
    result = param_set$check_dt(
      table,
      check_strict = mark("check_strict", TRUE),
      presence = mark("presence", "none"),
      allow_token = mark("allow_token", TRUE)
    )
    list(result = result, events = state$events)
  }

  expect_identical(
    observe(data.frame()),
    list(result = TRUE, events = character())
  )
  expect_identical(
    observe(data.frame(x = numeric())),
    list(result = TRUE, events = character())
  )
  expect_identical(
    observe(data.frame(x = 0.5)),
    list(
      result = TRUE,
      events = c("presence", "check_strict", "allow_token")
    )
  )

  expect_error(
    param_set$check_dt(
      data.frame(x = 0.5),
      check_strict = stop("forced check_strict"),
      presence = stop("forced presence"),
      allow_token = stop("forced allow_token")
    ),
    "forced presence",
    fixed = TRUE
  )
  expect_identical(
    error_call(param_set$check_dt(
      data.frame(x = 0.5),
      check_strict = TRUE,
      presence = stop("forced presence"),
      allow_token = TRUE
    )),
    "checkChoice(x, choices, null.ok, fmatch)"
  )
  expect_identical(
    error_call(param_set$check_dt(
      data.frame(x = 0.5),
      check_strict = stop("forced check_strict"),
      presence = "none",
      allow_token = TRUE
    )),
    "assert_flag(check_strict)"
  )
  expect_match(
    error_call(param_set$check_dt(
      data.frame(x = 0.5),
      check_strict = TRUE,
      presence = "none",
      allow_token = stop("forced allow_token")
    )),
    "^\\.__ParamSet__check\\(",
    perl = TRUE
  )
})

test_that("check_dt observes allow_token side effects before later rows", {
  param_set = ps(x = p_dbl(0, 10))
  private = param_set$.__enclos_env__$private

  result = param_set$check_dt(
    data.frame(x = c(1, 9)),
    allow_token = {
      data.table::set(private$.params, j = "upper", value = 5)
      TRUE
    }
  )

  expect_identical(result, "x: Element 1 is not <= 5")
})

test_that("native identifier matching follows R string encoding semantics", {
  utf8_id = enc2utf8("caf\u00e9")
  latin1_id = iconv(utf8_id, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1_id), "Latin-1 conversion is unavailable")
  Encoding(latin1_id) = "latin1"

  param_set = ps(cafe = p_dbl(0, 1))
  params = native_paramset_check_params(param_set)
  data.table::set(params, j = "id", value = utf8_id)
  values = setNames(list(0.5), latin1_id)
  table = setNames(data.frame(0.5), latin1_id)

  expect_identical(.Call(
    native_paramset_check_symbol(),
    params,
    values,
    FALSE
  ), TRUE)
  expect_identical(param_set$check(values), TRUE)
  expect_identical(.Call(
    native_paramset_check_symbol(table = TRUE),
    params,
    table
  ), TRUE)
  expect_identical(param_set$check_dt(table), TRUE)
})

test_that("native gates reject malformed storage without unsafe reads", {
  param_set = native_paramset_check_space()
  params = as.list(native_paramset_check_params(param_set))
  scalar_symbol = native_paramset_check_symbol()
  table_symbol = native_paramset_check_symbol(table = TRUE)
  scalar = list(double = 0)
  table = data.frame(double = 0)
  replace_column = function(name, value) {
    result = params
    result[[name]] = value
    result
  }

  corruptions = list(
    unname(params),
    within(params, rm(id)),
    replace_column("id", 1:4),
    replace_column("cls", "ParamDbl"),
    replace_column("lower", c(0, 1)),
    replace_column("levels", letters[1:4]),
    replace_column("special_vals", rep(list(NULL), 4)),
    replace_column("storage_type", rep(NA_character_, 4))
  )
  for (corrupt in corruptions) {
    expect_null(.Call(scalar_symbol, corrupt, scalar, FALSE))
    expect_null(.Call(table_symbol, corrupt, table))
  }

  expect_null(.Call(scalar_symbol, params, 1, FALSE))
  expect_null(.Call(scalar_symbol, params, list(0), FALSE))
  expect_null(.Call(scalar_symbol, params, scalar, NA))
  expect_null(.Call(table_symbol, params, list(double = 0)))
  expect_null(.Call(table_symbol, params, matrix(0, nrow = 1)))

  factor_set = ps(factor = p_fct(c("a", "b")))
  factor_params = data.table::copy(native_paramset_check_params(factor_set))
  data.table::set(
    factor_params,
    i = 1L,
    j = "levels",
    value = list(c("a", NA_character_))
  )
  expect_null(.Call(
    table_symbol,
    factor_params,
    data.frame(factor = NA_character_)
  ))
})

test_that("random canonical scalar and table values agree with public checks", {
  set.seed(20260713)
  param_set = native_paramset_check_space()
  params = native_paramset_check_params(param_set)
  scalar_symbol = native_paramset_check_symbol()
  table_symbol = native_paramset_check_symbol(table = TRUE)

  for (iteration in seq_len(200L)) {
    values = list(
      double = runif(1L, -10, 10),
      integer = sample(-20:20, 1L),
      factor = sample(c("slow", "fast"), 1L),
      logical = sample(c(FALSE, TRUE), 1L)
    )
    values = values[sample.int(length(values))]
    expect_identical(.Call(scalar_symbol, params, values, FALSE), TRUE)
    expect_identical(param_set$check(values), TRUE)
  }

  rows = 257L
  table = data.frame(
    double = runif(rows, -10, 10),
    integer = sample(c(-20:20, NA_integer_), rows, replace = TRUE),
    factor = sample(c("slow", "fast", NA_character_), rows, replace = TRUE),
    logical = sample(c(FALSE, TRUE, NA), rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
  table$double[sample.int(rows, 20L)] = NA_real_
  expect_identical(.Call(table_symbol, params, table), TRUE)
  expect_identical(param_set$check_dt(table), TRUE)

  for (column in names(table)) {
    invalid = table
    invalid[[column]][[sample.int(rows, 1L)]] = switch(
      column,
      double = 11,
      integer = 21L,
      factor = "absent",
      logical = 1L
    )
    expect_null(.Call(table_symbol, params, invalid))
    expect_false(isTRUE(param_set$check_dt(invalid)))
  }
})

test_that("factor table validation accumulates nested interrupt work", {
  nested_size = 257L
  choices = sprintf("choice_%03d", seq_len(nested_size))
  selected = choices[[nested_size]]
  param_set = ps(factor = p_fct(c("placeholder", "other")))
  params = data.table::copy(native_paramset_check_params(param_set))
  factor_row = match("factor", params$id)
  data.table::set(params, i = factor_row, j = "levels", value = list(choices))

  values = data.frame(
    factor = rep(selected, nested_size),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_identical(
    .Call(native_paramset_check_symbol(table = TRUE), params, values),
    TRUE
  )
})
