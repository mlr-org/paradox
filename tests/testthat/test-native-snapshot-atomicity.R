snapshot_atomicity_skip_without_helpers = function() {
  namespace = asNamespace("paradox")
  skip_if_not(
    exists("C_test_stateful_altrep", namespace, inherits = FALSE) &&
      exists(
        "C_test_stateful_altrep_row_names_rearm",
        namespace,
        inherits = FALSE
      ),
    "the internal stateful ALTREP fixtures are unavailable"
  )
}

snapshot_atomicity_frame = function(columns) {
  state = new.env(parent = emptyenv())
  rows = length(columns[[1L]])
  labels = sprintf("row-%d", seq_len(rows))
  row_names = native_stateful_altrep(
    labels,
    labels,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setcolorder(
        state$frame,
        rev(seq_along(state$frame))
      )
      invisible(gc())
    }
  )
  state$callbacks = 0L
  state$frame = structure(
    columns,
    names = names(columns),
    row.names = row_names,
    class = "data.frame"
  )
  invisible(.Call(
    get(
      "C_test_stateful_altrep_row_names_rearm",
      envir = asNamespace("paradox")
    ),
    state$frame,
    c(NA_integer_, 0L)
  ))
  state
}

test_that("public table consumers keep names paired with captured columns", {
  skip_on_cran()
  snapshot_atomicity_skip_without_helpers()

  checked = ps(a = p_dbl(0, 1), b = p_dbl(10, 20))
  check_state = snapshot_atomicity_frame(list(
    a = c(0.25, 0.75),
    b = c(12, 18)
  ))
  expect_identical(checked$check_dt(check_state$frame), TRUE)
  expect_identical(check_state$callbacks, 1L)
  expect_identical(names(check_state$frame), c("b", "a"))

  trafo_set = ps(a = p_uty(), b = p_uty())
  trafo_state = snapshot_atomicity_frame(list(
    a = c(1L, 2L),
    b = c(10L, 20L)
  ))
  transformed = trafo_set$trafo(trafo_state$frame)
  expect_identical(trafo_state$callbacks, 1L)
  expect_identical(
    transformed,
    list(a = c(1L, 2L), b = c(10L, 20L))
  )

  transpose_state = snapshot_atomicity_frame(list(
    a = c(1L, 2L),
    b = c(10L, 20L)
  ))
  rows = .Call(
    paradox:::C_design_transpose,
    transpose_state$frame,
    FALSE
  )
  expect_identical(transpose_state$callbacks, 1L)
  expect_identical(rows, list(
    list(a = 1L, b = 10L),
    list(a = 2L, b = 20L)
  ))

  dependent = ps(parent = p_lgl(), child = p_int())
  dependent$add_dep("child", "parent", CondEqual(TRUE))
  dependency_state = snapshot_atomicity_frame(list(
    parent = c(TRUE, FALSE),
    child = c(11L, 22L)
  ))
  plan = .Call(
    paradox:::C_design_dependency_plan,
    dependency_state$frame,
    dependent
  )
  expect_identical(dependency_state$callbacks, 1L)
  expect_identical(plan$rows, list(2L))
  expect_identical(plan$columns, "child")
  expect_identical(plan$values, list(NA_integer_))
})

test_that("ALTREP table row-name reentry cannot precede spine capture", {
  skip_on_cran()
  skip_if_no_list_altrep()
  snapshot_atomicity_skip_without_helpers()

  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  labels = c("first", "second")
  row_names = native_stateful_altrep(
    labels,
    labels,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setcolorder(state$frame, c("b", "a"))
      invisible(gc())
    }
  )
  state$frame = structure(
    list(a = c(1L, 2L), b = c(10L, 20L)),
    names = c("a", "b"),
    row.names = c(1L, 2L),
    class = "data.frame"
  )
  state$outer = native_stateful_altrep(state$frame, state$frame)
  # R 4.3--4.5's deep attribute duplicator asks a no-DATAPTR ALTREP row-name
  # value for its data pointer. Give that constructor ordinary source row
  # names, then install the exact callback-capable fixture by reference before
  # rearming it. The outer ALTREP still serves elements from `state$frame`, so
  # the callback retains the same spine-reordering hazard. `setattr()` cannot
  # install the fixture here because it duplicates a referenced replacement
  # on some runtimes.
  installer = .Call(
    get("C_test_gc_attribute_mutator", envir = asNamespace("paradox")),
    state$outer,
    "row.names",
    row_names
  )
  rm(installer)
  invisible(gc())
  invisible(.Call(
    get(
      "C_test_stateful_altrep_row_names_rearm",
      envir = asNamespace("paradox")
    ),
    state$outer,
    c(NA_integer_, 0L)
  ))

  rows = .Call(paradox:::C_design_transpose, state$outer, FALSE)
  expect_identical(state$callbacks, 1L)
  expect_identical(names(state$frame), c("b", "a"))
  expect_identical(rows, list(
    list(a = 1L, b = 10L),
    list(a = 2L, b = 20L)
  ))
})

test_that("ParamSet construction captures each name with its Domain", {
  skip_on_cran()
  snapshot_atomicity_skip_without_helpers()

  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  deferred_default = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setcolorder(
        state$domains,
        rev(seq_along(state$domains))
      )
      data.table::setcolorder(
        state$domain,
        rev(seq_along(state$domain))
      )
      invisible(gc())
    },
    callback_after = c(0L, NA_integer_)
  )
  domain = p_dbl(0, 1)
  domain_attributes = attributes(domain)
  domain = unclass(domain)
  domain[[match("default", names(domain))]] = list(deferred_default)
  attributes(domain) = domain_attributes
  state$domain = domain
  state$domains = list(first = state$domain, second = p_int(10L, 20L))

  bundle = .Call(
    paradox:::C_param_set_construct,
    state$domains,
    FALSE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(bundle$params$id, c("first", "second"))
  expect_identical(bundle$params$lower, c(0, 10))
  expect_identical(bundle$params$upper, c(1, 20))
  expect_identical(bundle$params$default[[1L]], 0.5)
})

test_that("public Domain snapshots retain cargo and special-value pairings", {
  custom_check = function(x) TRUE
  aggregate = function(x) x[[1L]]
  in_tune = function(domain, param_vals) TRUE
  first = new.env(parent = emptyenv())
  second = new.env(parent = emptyenv())

  domain = p_uty(
    custom_check = custom_check,
    special_vals = list(first = first, second = second),
    tags = "internal_tuning",
    aggr = aggregate,
    in_tune_fn = in_tune,
    disable_in_tune = list(blocked = TRUE)
  )

  cargo = domain$cargo[[1L]]
  expect_identical(cargo$custom_check, custom_check)
  expect_identical(cargo$aggr, aggregate)
  expect_identical(cargo$in_tune_fn, in_tune)
  expect_identical(cargo$disable_in_tune, list(blocked = TRUE))
  expect_identical(names(domain$special_vals[[1L]]), c("first", "second"))
  expect_identical(domain$special_vals[[1L]][[1L]], first)
  expect_identical(domain$special_vals[[1L]][[2L]], second)
})

test_that("tag assignment captures each name with its value vector", {
  skip_on_cran()
  snapshot_atomicity_skip_without_helpers()

  set = ps(first = p_int(), second = p_lgl())
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  first_tags = native_stateful_altrep(
    "numeric",
    "numeric",
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setcolorder(
        state$tags,
        rev(seq_along(state$tags))
      )
      invisible(gc())
    },
    callback_after = c(0L, NA_integer_)
  )
  state$tags = list(first = first_tags, second = "switch")

  set$tags = state$tags
  expect_identical(state$callbacks, 1L)
  expect_identical(
    set$tags,
    list(first = "numeric", second = "switch")
  )
})

test_that("dependency assignment captures names with exact columns", {
  skip_on_cran()
  snapshot_atomicity_skip_without_helpers()

  set = ps(parent = p_lgl(), child = p_int())
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  ids = native_stateful_altrep(
    "child",
    "child",
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setcolorder(state$deps, c(2L, 1L, 3L))
      invisible(gc())
    },
    callback_after = c(NA_integer_, 0L)
  )
  state$deps = structure(
    list(
      id = ids,
      on = "parent",
      cond = list(CondEqual(TRUE))
    ),
    names = c("id", "on", "cond"),
    row.names = c(NA_integer_, -1L),
    class = "data.frame"
  )
  native_stateful_altrep_rearm(ids, c(NA_integer_, 0L))

  set$deps = state$deps
  expect_identical(state$callbacks, 1L)
  expect_identical(set$deps$id, "child")
  expect_identical(set$deps$on, "parent")
  expect_identical(set$deps$cond[[1L]]$rhs, TRUE)
})

test_that("dependency snapshots capture every Condition row before RHS reentry", {
  skip_on_cran()
  snapshot_atomicity_skip_without_helpers()

  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  deferred_rhs = native_stateful_altrep(
    TRUE,
    TRUE,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setcolorder(state$conditions, c(2L, 1L))
      invisible(gc())
    },
    callback_after = c(0L, NA_integer_)
  )
  first = CondEqual(TRUE)
  first[[1L]] = deferred_rhs
  state$conditions = structure(
    list(first, CondEqual(FALSE)),
    names = c("first", "second")
  )
  dependencies = structure(
    list(
      id = c("a", "b"),
      on = c("x", "y"),
      cond = state$conditions
    ),
    names = c("id", "on", "cond"),
    row.names = c(NA_integer_, -2L),
    class = "data.frame"
  )

  snapshot = .Call(
    paradox:::C_param_set_dependency_table_snapshot,
    dependencies
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(
    lapply(snapshot$cond, `[[`, "rhs"),
    list(TRUE, FALSE)
  )
})

test_that("dependency assignment rejects S4 shells", {
  set = ps(parent = p_lgl(), child = p_int())
  s4_dependencies = asS4(data.frame(
    id = "child",
    on = "parent",
    cond = I(list(CondEqual(TRUE)))
  ))
  expect_error(
    { set$deps = s4_dependencies },
    "three-column table or an empty table",
    fixed = TRUE
  )

  ordinary = data.frame(
    id = "child",
    on = "parent",
    cond = I(list(CondEqual(TRUE)))
  )
  for (column in names(ordinary)) {
    malformed = ordinary
    malformed[[column]] = asS4(malformed[[column]])
    expect_error(
      { set$deps = malformed },
      "columns have unsupported types",
      fixed = TRUE,
      info = column
    )
  }
})

test_that("public check tables reject S4 list-column shells, not S4 leaves", {
  set = ps(value = p_uty())
  set$constraint = function(x) TRUE

  table = data.table::data.table(value = I(list(1L)))
  table[[1L]] = asS4(table[[1L]])
  expect_true(isS4(table[[1L]]))
  expect_match(
    set$check_dt(table),
    "Table list-column shells must not be S4",
    fixed = TRUE
  )
  expect_error(
    set$assert_dt(table),
    "Table list-column shells must not be S4",
    fixed = TRUE
  )
  expect_error(
    set$test_constraint_dt(table, assert_value = FALSE),
    "Table list-column shells must not be S4",
    fixed = TRUE
  )

  leaf = asS4(1L)
  ordinary = data.table::data.table(value = I(list(leaf)))
  expect_identical(set$check_dt(ordinary), TRUE)
})

test_that("detached collection callback plans reject S4 structure", {
  make_adapter = function() {
    child = ps(value = p_lgl())
    child$constraint = function(x) TRUE
    ParamSetCollection$new(list(owner = child))$flatten()$constraint
  }

  outer = make_adapter()
  environment(outer)$plan = asS4(environment(outer)$plan)
  expect_error(
    outer(list()),
    "Corrupt detached ParamSetCollection callback plan",
    fixed = TRUE
  )

  named = make_adapter()
  plan = environment(named)$plan
  attr(plan, "names") = asS4(names(plan))
  environment(named)$plan = plan
  expect_error(
    named(list()),
    "Corrupt detached ParamSetCollection callback plan",
    fixed = TRUE
  )
})

test_that("data.table finalization rejects S4 structural carriers", {
  table = data.table::data.table(value = 1L)
  expect_error(
    .Call(paradox:::C_finalize_data_table, asS4(table)),
    "expected an ordinary data.table shell",
    fixed = TRUE
  )

  malformed_names = data.table::copy(table)
  # data.table::setattr() clears the S4 bit of an atomic attribute value.
  # Base attribute replacement preserves the deliberately malformed carrier.
  attr(malformed_names, "names") = asS4(names(malformed_names))
  expect_true(isS4(attr(malformed_names, "names")))
  expect_error(
    .Call(paradox:::C_finalize_data_table, malformed_names),
    "expected bounded data.table metadata",
    fixed = TRUE
  )
})

test_that("data.table finalization captures one column/name generation", {
  skip_on_cran()

  state = new.env(parent = emptyenv())
  state$table = data.table::data.table(
    first = 1L,
    second = 2L
  )
  state$fired = FALSE
  symbol = paradox:::C_finalize_data_table
  source = state$table
  old_names = names(source)[seq_along(source)]
  old_columns = unname(as.list(source))

  trigger = new.env(parent = emptyenv())
  reg.finalizer(trigger, function(unused) {
    state$fired = TRUE
    data.table::setcolorder(state$table, c("second", "first"))
  })
  trigger = NULL
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)

  finalized = .Call(symbol, source)
  gctorture(previous)
  gc(full = TRUE)
  expect_true(state$fired)
  selected_old = identical(names(finalized), old_names) &&
    identical(unname(as.list(finalized)), old_columns)
  selected_new = identical(names(finalized), names(state$table)) &&
    identical(
      unname(as.list(finalized)),
      unname(as.list(state$table))
    )
  expect_true(
    selected_old || selected_new,
    info = "finalization must select one complete pre/post-finalizer generation"
  )
  expect_identical(data.table:::selfrefok(finalized, verbose = FALSE), 1L)
})
