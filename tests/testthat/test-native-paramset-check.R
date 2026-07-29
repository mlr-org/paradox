native_check_private = function(param_set) {
  param_set$.__enclos_env__$private
}

native_check_call = function(param_set, values, check_strict = TRUE,
    sanitize = FALSE, presence = "none", allow_token = TRUE) {
  .Call(
    get("C_param_set_check_builtin", envir = asNamespace("paradox")),
    native_check_private(param_set),
    param_set,
    values,
    check_strict,
    sanitize,
    presence,
    allow_token
  )
}

native_check_dt_call = function(param_set, table, check_strict = TRUE,
    presence = "none", allow_token = TRUE) {
  .Call(
    get("C_param_set_check_dt_builtin", envir = asNamespace("paradox")),
    native_check_private(param_set),
    param_set,
    table,
    check_strict,
    presence,
    allow_token
  )
}

native_check_space = function() {
  ps(
    double = p_dbl(-10, 10, tolerance = 1e-6),
    integer = p_int(-20, 20, tolerance = 1e-6),
    factor = p_fct(c("slow", "fast")),
    logical = p_lgl()
  )
}

test_that("the unified check operations are registered and forced", {
  routines = list(
    param_set_check_builtin = 7L,
    tune_token_snapshot_list = 3L,
    test_tune_token_gc_mutation_snapshot = 3L,
    param_set_check_dependencies_builtin = 3L,
    param_set_test_constraint_builtin = 4L,
    param_set_test_constraint_dt_builtin = 4L,
    param_set_check_dt_builtin = 6L
  )
  for (name in names(routines)) {
    symbol = get(paste0("C_", name), envir = asNamespace("paradox"))
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, routines[[name]])
  }
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
  expect_error(
    .Call(
      "param_set_check_builtin", new.env(), new.env(), list(), TRUE,
      FALSE, "none", TRUE, PACKAGE = "paradox"
    ),
    "not available"
  )
})

test_that("check_dependencies is the strict native dependency-only boundary", {
  param_set = ps(
    switch = p_lgl(),
    dependent = p_int(depends = switch == TRUE),
    other = p_int()
  )

  expect_identical(param_set$check_dependencies(list()), TRUE)
  expect_identical(
    param_set$check_dependencies(list(switch = FALSE)),
    TRUE
  )
  expect_identical(
    param_set$check_dependencies(list(
      dependent = to_tune(),
      switch = FALSE
    )),
    TRUE
  )
  expect_identical(
    param_set$check_dependencies(list(
      dependent = 1L,
      switch = to_tune()
    )),
    TRUE
  )
  expect_match(
    param_set$check_dependencies(list(dependent = 1L)),
    "switch.*not set at all"
  )
  expect_match(
    param_set$check_dependencies(list(dependent = 1L, switch = FALSE)),
    "dependent:.*switch == TRUE.*switch == FALSE"
  )
  expect_identical(
    param_set$check_dependencies(list(dependent = 1L, switch = TRUE)),
    TRUE
  )

  # The 2.0 dependency-only boundary admits the same identifier universe as
  # `$check()`, even when there happen to be no dependency rows.
  expect_identical(
    ps(x = p_int())$check_dependencies(list(unknown = 1L)),
    "Parameter 'unknown' not available"
  )
  expect_match(
    param_set$check_dependencies(structure(
      list(switch = TRUE),
      class = "configuration"
    )),
    "ordinary named list"
  )
  expect_match(param_set$check_dependencies(1L), "ordinary named list")
  expect_match(
    param_set$check_dependencies(list(1L)),
    "Must be a named list"
  )
  expect_match(
    param_set$check_dependencies(list(switch = TRUE, switch = FALSE)),
    "Names must be unique"
  )

  multiple = ps(
    on = p_lgl(),
    first = p_int(depends = on == TRUE),
    second = p_int(depends = on == TRUE)
  )
  diagnostic = multiple$check_dependencies(list(
    on = FALSE,
    first = 1L,
    second = 2L
  ))
  expect_match(diagnostic, "^first:")
  expect_false(grepl("second:|\\n", diagnostic))
})

test_that("check_dependencies traverses live Collection and Shadow graphs", {
  left = ps(on = p_lgl(), value = p_int(depends = on == TRUE))
  right = ps(other = p_int())
  collection = ParamSetCollection$new(list(left = left, right = right))

  expect_match(
    collection$check_dependencies(list(left.value = 1L)),
    "left.value:.*left.on.*not set at all"
  )
  expect_identical(
    collection$check_dependencies(list(left.on = TRUE, left.value = 1L)),
    TRUE
  )

  origin = ps(hidden = p_int(), on = p_lgl(), value = p_int())
  shadow = ParamSetShadow$new(origin, "hidden")
  origin$add_dep("value", "on", CondEqual$new(TRUE))
  expect_match(
    shadow$check_dependencies(list(value = 1L, on = FALSE)),
    "value:.*on == TRUE.*on == FALSE"
  )
  expect_identical(
    shadow$check_dependencies(list(value = 1L, on = TRUE)),
    TRUE
  )

  unlockBinding("check_dependencies", left)
  left$check_dependencies = function(...) {
    stop("overridden dependency reader executed", call. = FALSE)
  }
  lockBinding("check_dependencies", left)
  expect_identical(
    collection$check_dependencies(list(left.on = TRUE, left.value = 1L)),
    TRUE
  )
})

test_that("check_dependencies rejects corrupt capsule dependency state", {
  param_set = ps(on = p_lgl(), value = p_int(depends = on == TRUE))
  private = native_check_private(param_set)
  state = paradox:::param_set_core_state(private)
  state$.deps$id[[1L]] = "forged"
  private$.core = .Call(paradox:::C_param_set_core_new, 1L, state)

  expect_error(
    param_set$check_dependencies(list()),
    "dependency target is unknown"
  )
})

test_that("BASE checking validates and sanitizes all built-in kinds", {
  param_set = native_check_space()
  values = list(
    factor = "fast",
    double = -10 - 5e-7,
    logical = FALSE,
    integer = 10.0000005
  )

  expect_identical(native_check_call(param_set, values), TRUE)
  result = native_check_call(param_set, values, sanitize = TRUE)
  expect_identical(result, param_set$check(values, sanitize = TRUE))
  expect_identical(
    attr(result, "sanitized"),
    list(factor = "fast", double = -10, logical = FALSE, integer = 10L)
  )

  expect_match(param_set$check(list(double = 11)), "double:.*not <=")
  expect_match(param_set$check(list(integer = 1.5)), "integer:.*integerish")
  expect_match(
    param_set$check(list(factor = "absent")),
    "factor:.*element of set"
  )
  expect_match(param_set$check(list(logical = 1)), "logical:.*logical")
  expect_match(param_set$check(list(unknown = 1)), "not available")
  expect_match(param_set$check(list(double = NA_real_)), "double:")
  expect_true(param_set$check(list()))

  for (values in list(
    list(double = NA_real_),
    list(integer = Inf),
    list(factor = factor("slow")),
    list(logical = 1L),
    list(unknown = 1)
  )) {
    result = native_check_call(param_set, values)
    expect_false(is.null(result), info = deparse(values))
    expect_true(is.character(result), info = deparse(values))
  }
})

test_that("special values, ParamUty callbacks, and TuneTokens use the engine", {
  special = ps(
    number = p_dbl(0, 1, special_vals = list("AUTO", NULL)),
    opaque = p_uty(special_vals = list(environment()))
  )
  expect_true(special$check(list(number = "AUTO")))
  expect_true(special$check(list(number = NULL)))
  expect_true(special$check(list(number = 0.5)))

  calls = 0L
  utility = ps(value = p_uty(custom_check = function(value) {
    calls <<- calls + 1L
    if (identical(value, list(ok = TRUE))) TRUE else "utility rejected"
  }))
  # p_uty() validates the callback during construction.
  calls = 0L
  expect_true(utility$check(list(value = list(ok = TRUE))))
  expect_identical(calls, 1L)
  expect_identical(
    utility$check(list(value = list(ok = FALSE))),
    "value: utility rejected"
  )
  expect_identical(calls, 2L)

  numeric = ps(value = p_dbl(0, 1))
  expect_true(numeric$check(list(value = to_tune())))
  expect_true(numeric$check(list(value = to_tune(0, 1))))
  expect_match(
    numeric$check(list(value = to_tune(-1, 1))),
    "not compatible.*lower"
  )
  expect_identical(
    numeric$check(list(value = to_tune()), allow_token = FALSE),
    "TuneTokens are not allowed to be present."
  )

  categorical = ps(value = p_fct(c("a", "b")))
  expect_true(categorical$check(list(value = to_tune(c("a", "b")))))
  expect_identical(
    categorical$check(list(value = to_tune("outside"))),
    TRUE
  )
  expect_error(
    categorical$search_space(list(value = to_tune("outside"))),
    "outside.*not compatible"
  )

  internal = ps(value = p_int(0, 5))
  internal$tags = list(value = "internal_tuning")
  expect_true(internal$check(list(
    value = to_tune(upper = 5, internal = TRUE)
  )))
  ordinary = ps(value = p_int(0, 5))
  expect_error(
    ordinary$check(list(value = to_tune(upper = 5, internal = TRUE))),
    "not tagged with 'internal_tuning'"
  )
})

test_that("built-in checks reject structural S4 while retaining opaque leaves", {
  special = asS4(0.5)
  numeric = ps(value = p_dbl(0, 1, special_vals = list(special)))
  expect_identical(numeric$check(list(value = special)), TRUE)
  expect_identical(
    numeric$check(list(value = asS4(0.75))),
    "value: Must be of type 'number', not 'double'"
  )

  opaque = asS4(list(payload = 1L))
  utility = ps(value = p_uty())
  expect_identical(utility$check(list(value = opaque)), TRUE)

  point = list(value = 0.5)
  expect_identical(
    numeric$check(structure(point, class = "configuration")),
    TRUE
  )
  expect_match(numeric$check(asS4(point)), "ordinary named list")

  s4_names = point
  attr(s4_names, "names") = asS4(names(s4_names))
  expect_match(numeric$check(s4_names), "named list")

  s4_classes = structure(point, class = "configuration")
  attr(s4_classes, "class") = asS4(class(s4_classes))
  expect_match(numeric$check(s4_classes), "ordinary named list")

  for (arguments in list(
    list(check_strict = asS4(TRUE)),
    list(sanitize = asS4(FALSE)),
    list(presence = asS4("none")),
    list(allow_token = asS4(TRUE))
  )) {
    expect_error(
      do.call(numeric$check, c(list(xs = point), arguments)),
      "must be TRUE or FALSE|presence",
      info = names(arguments)
    )
  }
})

test_that("presence, closed Conditions, constraints, and strictness agree", {
  param_set = ps(
    switch = p_fct(c("on", "off"), tags = "required"),
    always = p_dbl(0, 1, tags = "required"),
    dependent = p_int(
      0,
      5,
      depends = switch %in% c("on"),
      tags = "required"
    )
  )
  expect_match(
    param_set$check(list(switch = "on"), presence = "required"),
    "Missing parameters: always"
  )
  expect_match(
    param_set$check(
      list(switch = "on", always = 0.5),
      presence = "required"
    ),
    "satisfied dependencies: dependent"
  )
  expect_true(param_set$check(
    list(switch = "off", always = 0.5),
    presence = "required"
  ))
  expect_match(
    param_set$check(list(switch = "off", dependent = 1L)),
    "switch %in%"
  )
  expect_true(param_set$check(
    list(switch = "off", dependent = 1L),
    check_strict = FALSE
  ))

  calls = 0L
  param_set$constraint = function(x) {
    calls <<- calls + 1L
    is.null(x$always) || x$always <= 0.5
  }
  expect_identical(
    param_set$check(list(always = 0.75)),
    "Constraint not fulfilled."
  )
  expect_identical(calls, 1L)
  expect_true(param_set$check(list(always = 0.75), check_strict = FALSE))
  expect_identical(calls, 1L)
})

test_that("ordinary arguments are forced once from left to right", {
  events = character()
  mark = function(name, value) {
    events <<- c(events, name)
    value
  }
  param_set = ps(value = p_dbl(0, 1))
  expect_true(param_set$check(
    mark("values", list(value = 0.5)),
    check_strict = mark("strict", TRUE),
    sanitize = mark("sanitize", FALSE),
    presence = mark("presence", "none"),
    allow_token = mark("token", TRUE)
  ))
  expect_identical(
    events,
    c("values", "strict", "sanitize", "presence", "token")
  )

  events = character()
  expect_true(param_set$check_dt(
    mark("table", data.frame(value = 0.5)),
    check_strict = mark("strict", TRUE),
    presence = mark("presence", "none"),
    allow_token = mark("token", TRUE)
  ))
  expect_identical(events, c("table", "strict", "presence", "token"))
})

test_that("callback and table state is snapshotted without replay", {
  param_set = NULL
  calls = character()
  table = data.frame(value = I(list("first", "second")))
  checker = function(value) {
    calls <<- c(calls, value)
    if (!is.null(param_set) && identical(value, "first")) {
      param_set$constraint = function(x) FALSE
      table$value[[2L]] <<- "mutated"
    }
    TRUE
  }
  param_set = ps(value = p_uty(custom_check = checker))
  calls = character()
  param_set$constraint = function(x) TRUE

  expect_true(param_set$check_dt(table))
  expect_identical(calls, c("first", "second"))
  expect_identical(table$value[[2L]], "mutated")
  expect_identical(
    param_set$check(list(value = "later")),
    "Constraint not fulfilled."
  )

  armed = FALSE
  error_calls = 0L
  failing = ps(value = p_uty(custom_check = function(value) {
    if (!armed) return(TRUE)
    error_calls <<- error_calls + 1L
    stop("callback failed once", call. = FALSE)
  }))
  armed = TRUE
  expect_error(
    failing$check(list(value = "x")),
    "callback failed once"
  )
  expect_identical(error_calls, 1L)
})

test_that("opaque ParamUty leaves retain identity and are not inspected", {
  altrep_accesses = 0L
  nested = native_stateful_altrep(
    c(1L, 2L, 3L),
    c(4L, 5L, 6L),
    callback = function() altrep_accesses <<- altrep_accesses + 1L,
    callback_after = c(NA_integer_, 0L)
  )
  payload = list(nested = nested)
  utility_calls = 0L
  param_set = ps(value = p_uty(custom_check = function(value) {
    if (is.list(value) && identical(names(value), "nested")) {
      utility_calls <<- utility_calls + 1L
      expect_identical(value$nested, nested)
    }
    TRUE
  }))
  param_set$values = list(value = payload)

  native_stateful_altrep_rearm(nested, c(NA_integer_, 0L))
  altrep_accesses = 0L
  utility_calls = 0L
  expect_true(param_set$check(list(value = payload)))
  expect_identical(utility_calls, 1L)
  expect_identical(altrep_accesses, 0L)
  expect_identical(param_set$values$value$nested, nested)
  expect_identical(altrep_accesses, 0L)
})

test_that("check_dt uses the scalar row kernel and snapshots its columns", {
  param_set = native_check_space()
  values = data.frame(
    logical = c(TRUE, NA, FALSE),
    factor = c("slow", NA, "fast"),
    integer = c(-20, NA, 20.0000005),
    double = c(-10 - 5e-7, NaN, 10 + 5e-7),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_true(native_check_dt_call(param_set, values))
  expect_true(param_set$check_dt(values))

  bad = data.frame(
    double = c(0, 0),
    integer = c(0L, 21L),
    factor = c("absent", "slow"),
    logical = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  # Row-major behavior reports the first row before inspecting a later row.
  expect_match(param_set$check_dt(bad), "factor:.*element of set")

  expect_true(param_set$check_dt(data.frame()))
  expect_true(param_set$check_dt(data.frame(unknown = numeric())))
  three_empty_rows = structure(
    list(),
    class = "data.frame",
    row.names = .set_row_names(3L)
  )
  expect_match(
    param_set$check_dt(three_empty_rows, presence = "all"),
    "Missing parameters"
  )
})

test_that("COLLECTION supports shared DAGs and current child semantics", {
  shared = ps(value = p_int(0, 5))
  collection = ParamSetCollection$new(list(left = shared, right = shared))
  expect_true(collection$check(
    list(left.value = 1L, right.value = 2L),
    presence = "all"
  ))
  expect_true(collection$check_dt(data.frame(
    left.value = c(1L, 2L),
    right.value = c(2L, 3L)
  ), presence = "all"))

  shared$constraint = function(x) is.null(x$value) || x$value < 3L
  expect_identical(
    collection$check(list(left.value = 4L, right.value = 1L)),
    "Constraint not fulfilled."
  )
})

test_that("SHADOW and collection-to-shadow checks use one live snapshot", {
  origin = ps(
    switch = p_lgl(),
    value = p_int(0, 5),
    hidden = p_int(0, 9)
  )
  origin$values = list(hidden = 8L)
  shadow = ParamSetShadow$new(origin, "hidden")
  collection = ParamSetCollection$new(list(view = shadow))

  expect_true(collection$check(list(
    view.switch = TRUE,
    view.value = 1L
  ), presence = "all"))
  origin$add_dep("value", "switch", CondEqual$new(TRUE))
  expect_match(
    collection$check(list(view.switch = FALSE, view.value = 1L)),
    "view.switch == TRUE"
  )
  origin$constraint = function(x) is.null(x$value) || x$value < 2L
  expect_identical(
    collection$check(list(view.switch = TRUE, view.value = 3L)),
    "Constraint not fulfilled."
  )
  expect_identical(origin$values$hidden, 8L)

  origin$extra_trafo = function(x, param_set) {
    x$value = x$value + 10L
    x
  }
  expect_true(shadow$check(list(switch = TRUE, value = 1L)))
  expect_true(shadow$has_trafo)
  expect_identical(
    shadow$trafo(list(switch = TRUE, value = 1L))$value,
    11L
  )
  origin$extra_trafo = NULL
  expect_true(shadow$check(list(switch = TRUE, value = 1L)))
  expect_false(shadow$has_trafo)
})

test_that("current-path cycles error while repeated children remain valid", {
  collection = ParamSetCollection$new(list())
  shadow = ParamSetShadow$new(collection, character())
  paradox:::param_set_core_replace(
    collection$.__enclos_env__$private,
    sets = list(cycle = shadow)
  )
  expect_error(collection$check(list()), "graph contains a cycle")
  expect_error(shadow$check(list()), "graph contains a cycle")
  expect_error(
    collection$check_dependencies(list()),
    "graph contains a cycle"
  )
  expect_error(shadow$check_dependencies(list()), "graph contains a cycle")
})
