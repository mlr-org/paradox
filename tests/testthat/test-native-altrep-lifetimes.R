altrep2_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"), inherits = FALSE)
}

altrep2_helpers_available = function() {
  namespace = asNamespace("paradox")
  exists("C_test_stateful_altrep", namespace, inherits = FALSE) &&
    exists("C_test_stateful_altrep_rearm", namespace, inherits = FALSE)
}

altrep2_skip_without_helpers = function() {
  skip_if_not(
    altrep2_helpers_available(),
    "the internal stateful ALTREP test class is unavailable"
  )
}

altrep2_private = function(param_set) {
  param_set$.__enclos_env__$private
}

altrep2_state = function(param_set) {
  paradox:::param_set_core_state(altrep2_private(param_set))
}

altrep2_replace_table_column = function(table, name, value) {
  result = unclass(table)
  result[[name]] = value
  attributes(result) = attributes(table)
  result
}

altrep2_replace_core_field = function(param_set, field, value) {
  .Call(
    paradox:::C_param_set_core_replace,
    altrep2_private(param_set),
    setNames(list(value), paste0(".", field))
  )
  invisible(param_set)
}

altrep2_set_upper = function(param_set, upper) {
  params = unserialize(serialize(altrep2_state(param_set)$.params, NULL, version = 3L))
  params$upper[] = upper
  paradox:::param_set_core_replace(altrep2_private(param_set), params = params)
  invisible(param_set)
}

test_that("ALTREP-sensitive native routines use their current fixed arities", {
  routines = c(
    domain_check_builtin = 3L,
    domain_qunif_builtin = 2L,
    domain_sanitize_builtin = 2L,
    param_set_construct = 1L,
    param_set_check_builtin = 7L,
    param_set_test_constraint_builtin = 4L,
    param_set_test_constraint_dt_builtin = 4L,
    param_set_check_dt_builtin = 6L,
    condition_test_builtin = 2L,
    param_set_qunif_builtin = 3L,
    generate_design_grid_builtin = 2L,
    design_transpose = 2L,
    design_transpose_trafos = 2L,
    param_set_trafo = 4L,
    param_set_values_merge = 4L
  )
  for (name in names(routines)) {
    symbol = altrep2_symbol(name)
    expect_identical(symbol$numParameters, routines[[name]], info = name)
  }
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("ordinary compact sequences work at every applicable boundary", {
  expect_identical(
    .Call(altrep2_symbol("domain_sanitize_builtin"), p_int(), 1:4),
    as.list(1:4)
  )
  param_set = ps(x = p_int(0L, 4L))
  private = altrep2_private(param_set)
  expect_identical(.Call(
    altrep2_symbol("param_set_check_dt_builtin"),
    private,
    param_set,
    data.frame(x = 1:4),
    TRUE,
    "none",
    TRUE
  ), TRUE)

  units = matrix(0:1, ncol = 1L, dimnames = list(NULL, "x"))
  mapped = .Call(
    altrep2_symbol("param_set_qunif_builtin"),
    private,
    param_set,
    units
  )
  expect_identical(mapped$x, c(0L, 4L))

  grid_set = ps(a = p_int(0L, 0L), b = p_int(0L, 1L))
  resolutions = 1:2
  names(resolutions) = c("a", "b")
  grid = .Call(
    altrep2_symbol("generate_design_grid_builtin"),
    altrep2_state(grid_set)$.params,
    resolutions
  )
  expect_identical(as.list(grid), list(a = c(0L, 0L), b = c(0L, 1L)))

  transposed = .Call(altrep2_symbol("design_transpose"), list(x = 1:3), FALSE)
  expect_identical(transposed, list(list(x = 1L), list(x = 2L), list(x = 3L)))
})

test_that("Domain vectors materialize once and constructor shells stay structural", {
  altrep2_skip_without_helpers()
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    invisible(gc())
  }

  values = native_stateful_altrep(
    c(-1, 11),
    c(100, 100),
    elt_switch_after = 2L,
    callback = callback,
    callback_after = 0L
  )
  expect_identical(
    .Call(altrep2_symbol("domain_sanitize_builtin"), p_dbl(0, 10), values),
    list(0, 10)
  )

  units = native_stateful_altrep(
    c(0.25, 0.75),
    c(0.1, 0.1),
    elt_switch_after = 2L,
    callback = callback,
    callback_after = 0L
  )
  expect_identical(
    .Call(altrep2_symbol("domain_qunif_builtin"), p_dbl(0, 10), units),
    c(2.5, 7.5)
  )

  scalar = native_stateful_altrep(
    0.5,
    99,
    elt_switch_after = 1L,
    callback = callback,
    callback_after = 0L
  )
  expect_identical(.Call(
    altrep2_symbol("domain_check_builtin"),
    p_dbl(0, 1),
    list(scalar),
    FALSE
  ), TRUE)

  first = structure(list(a = p_int(0L, 1L), b = p_dbl(10, 20)), names = c("a", "b"))
  later = structure(list(a = p_int(100L, 101L), b = p_dbl(30, 40)), names = c("a", "b"))
  domains = native_stateful_altrep(
    first,
    later,
    elt_switch_after = 2L,
    callback = callback,
    callback_after = 0L
  )
  callbacks_before = callbacks
  expect_error(
    .Call(altrep2_symbol("param_set_construct"), domains),
    "ordinary named list",
    fixed = TRUE
  )
  expect_identical(callbacks, callbacks_before)
})

test_that("constructor rejects ALTREP nested structure before observation", {
  altrep2_skip_without_helpers()
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
  }

  cargo_shell = native_stateful_altrep(
    list(logscale = TRUE),
    list(logscale = FALSE),
    callback = callback,
    callback_after = 0L
  )
  domain = altrep2_replace_table_column(
    p_dbl(1, 10, logscale = TRUE),
    "cargo",
    list(cargo_shell)
  )
  native_stateful_altrep_rearm(domain$cargo[[1L]], 0L)
  callbacks = 0L
  expect_error(
    .Call(altrep2_symbol("param_set_construct"), list(x = domain)),
    "cargo",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)

  logscale = native_stateful_altrep(
    TRUE,
    FALSE,
    callback = callback,
    callback_after = 0L
  )
  domain = altrep2_replace_table_column(
    p_dbl(1, 10, logscale = TRUE),
    "cargo",
    list(list(logscale = logscale))
  )
  native_stateful_altrep_rearm(domain$cargo[[1L]]$logscale, 0L)
  callbacks = 0L
  expect_error(
    .Call(altrep2_symbol("param_set_construct"), list(x = domain)),
    "cargo",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)

  special_shell = native_stateful_altrep(
    list(Inf),
    list(-Inf),
    callback = callback,
    callback_after = 0L
  )
  domain = altrep2_replace_table_column(
    p_dbl(special_vals = list(Inf)),
    "special_vals",
    list(special_shell)
  )
  native_stateful_altrep_rearm(domain$special_vals[[1L]], 0L)
  callbacks = 0L
  expect_error(
    .Call(altrep2_symbol("param_set_construct"), list(x = domain)),
    "special",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("Domain special-value and value checks share one ALTREP snapshot", {
  altrep2_skip_without_helpers()
  callbacks = 0L
  scalar = native_stateful_altrep(
    0.5,
    99,
    elt_switch_after = 1L,
    callback = function() {
      callbacks <<- callbacks + 1L
      invisible(gc())
    },
    callback_after = 0L
  )

  expect_identical(
    domain_check(p_dbl(0, 1, special_vals = list(-1)), list(scalar)),
    TRUE
  )
  expect_identical(callbacks, 1L)
})

test_that("check rejects structural ALTREP before selecting capsule state", {
  altrep2_skip_without_helpers()

  point_set = ps(x = p_int(0L, 1L))
  point_callbacks = 0L
  point = native_stateful_altrep(
    structure(list(2L), names = "x"),
    structure(list(99L), names = "x"),
    elt_switch_after = 1L,
    callback = function() {
      point_callbacks <<- point_callbacks + 1L
      altrep2_set_upper(point_set, 2)
    },
    callback_after = 0L
  )
  expect_identical(.Call(
    altrep2_symbol("param_set_check_builtin"),
    altrep2_private(point_set),
    point_set,
    point,
    TRUE,
    FALSE,
    "none",
    TRUE
  ), "Must be an ordinary named list.")
  expect_identical(point_callbacks, 0L)
  expect_identical(altrep2_state(point_set)$.params$upper, 1L)

  name_callbacks = 0L
  hostile_names = native_stateful_altrep(
    "x",
    "changed",
    callback = function() name_callbacks <<- name_callbacks + 1L,
    callback_after = 0L
  )
  named_point = list(1L)
  attr(named_point, "names") = hostile_names
  expect_identical(
    point_set$check(named_point),
    paste(
      "Must be a named list.",
      "Names must be an ordinary character vector."
    )
  )

  named_domains = list(p_dbl())
  attr(named_domains, "names") = hostile_names
  expect_error(
    .Call(altrep2_symbol("param_set_construct"), named_domains),
    "ordinary character names",
    fixed = TRUE
  )

  token_values = list(to_tune())
  attr(token_values, "names") = hostile_names
  expect_error(.Call(
    altrep2_symbol("tune_token_snapshot_list"),
    altrep2_private(point_set),
    point_set,
    token_values
  ), "ordinary character names", fixed = TRUE)
  expect_identical(name_callbacks, 0L)

  table_set = ps(x = p_int(0L, 1L))
  table_callbacks = 0L
  column = native_stateful_altrep(
    c(2L, 2L),
    c(99L, 99L),
    elt_switch_after = 2L,
    callback = function() {
      table_callbacks <<- table_callbacks + 1L
      altrep2_set_upper(table_set, 2)
    },
    callback_after = 0L
  )
  table = structure(
    list(x = column),
    names = "x",
    row.names = c(NA_integer_, -2L),
    class = "data.frame"
  )
  expect_identical(.Call(
    altrep2_symbol("param_set_check_dt_builtin"),
    altrep2_private(table_set),
    table_set,
    table,
    TRUE,
    "none",
    TRUE
  ), TRUE)
  expect_identical(table_callbacks, 1L)
})

test_that("bulk qunif snapshots matrix values and metadata", {
  altrep2_skip_without_helpers()
  param_set = ps(x = p_dbl(0, 10))
  first = matrix(c(0, 0.5, 1), ncol = 1L, dimnames = list(NULL, "x"))
  later = matrix(rep(0.25, 3L), ncol = 1L, dimnames = dimnames(first))
  callbacks = 0L
  units = native_stateful_altrep(
    first,
    later,
    elt_switch_after = length(first),
    callback = function() {
      callbacks <<- callbacks + 1L
      invisible(gc())
    },
    callback_after = 0L
  )

  observed = .Call(
    altrep2_symbol("param_set_qunif_builtin"),
    altrep2_private(param_set),
    param_set,
    units
  )
  expect_identical(observed$x, c(0, 5, 10))
  expect_identical(callbacks, 1L)

  malformed = first
  attr(malformed, "dimnames") = asS4(dimnames(first))
  expect_error(.Call(
    altrep2_symbol("param_set_qunif_builtin"),
    altrep2_private(param_set),
    param_set,
    malformed
  ), "column name", fixed = TRUE)
})

test_that("Design columns materialize while trafo shells stay structural", {
  altrep2_skip_without_helpers()

  design_set = ps(
    x = p_dbl(),
    .extra_trafo = function(x) c(x, list(source = "old"))
  )
  design_callbacks = 0L
  column = native_stateful_altrep(
    c(1, 2),
    c(9, 9),
    elt_switch_after = 2L,
    callback = function() {
      design_callbacks <<- design_callbacks + 1L
      design_set$extra_trafo = function(x) c(x, list(source = "new"))
    },
    callback_after = 0L
  )
  rows = .Call(altrep2_symbol("design_transpose"), list(x = column), FALSE)
  transformed = .Call(
    altrep2_symbol("design_transpose_trafos"),
    rows,
    design_set
  )
  expect_identical(vapply(transformed, `[[`, character(1L), "source"), c("new", "new"))
  expect_identical(vapply(transformed, `[[`, numeric(1L), "x"), c(1, 2))
  expect_identical(design_callbacks, 1L)

  point_set = ps(
    x = p_dbl(),
    .extra_trafo = function(x) c(x, list(source = "old"))
  )
  point_callbacks = 0L
  point = native_stateful_altrep(
    structure(list(1), names = "x"),
    structure(list(99), names = "x"),
    elt_switch_after = 1L,
    callback = function() {
      point_callbacks <<- point_callbacks + 1L
      point_set$extra_trafo = function(x) c(x, list(source = "new"))
    },
    callback_after = 0L
  )
  expect_error(
    .Call(
      altrep2_symbol("param_set_trafo"),
      altrep2_private(point_set),
      point_set,
      point,
      point_set
    ),
    "ordinary named list",
    fixed = TRUE
  )
  expect_identical(point_callbacks, 0L)
  expect_identical(
    point_set$trafo(list(x = 1)),
    list(x = 1, source = "old")
  )
})

test_that("public tables snapshot ALTREP shells while other shells reject", {
  altrep2_skip_without_helpers()
  param_set = ps(x = p_dbl(0, 1))
  table = structure(
    list(x = c(0.25, 0.75)),
    names = "x",
    row.names = c(NA_integer_, -2L),
    class = "data.frame"
  )
  table_callbacks = 0L
  wrapped_table = function(value = table) native_stateful_altrep(
    value, value,
    callback = function() table_callbacks <<- table_callbacks + 1L,
    callback_after = 0L
  )

  expect_identical(.Call(
    altrep2_symbol("param_set_check_dt_builtin"),
    altrep2_private(param_set),
    param_set,
    wrapped_table(),
    TRUE,
    "none",
    TRUE
  ), TRUE)
  expect_identical(table_callbacks, 1L)
  expect_identical(.Call(
    altrep2_symbol("param_set_qunif_builtin"),
    altrep2_private(param_set),
    param_set,
    wrapped_table()
  )$x, c(0.25, 0.75))
  expect_identical(table_callbacks, 2L)
  expect_identical(
    param_set$trafo(wrapped_table()),
    list(x = c(0.25, 0.75))
  )
  expect_identical(table_callbacks, 3L)
  expect_identical(
    .Call(altrep2_symbol("design_transpose"), wrapped_table(), FALSE),
    list(list(x = 0.25), list(x = 0.75))
  )
  expect_identical(table_callbacks, 4L)
  expect_length(.Call(
    altrep2_symbol("design_dependency_plan"),
    wrapped_table(),
    param_set
  )$rows, 0L)
  expect_identical(table_callbacks, 5L)

  data_table = data.table::as.data.table(table)
  param_set$constraint = function(x) TRUE
  expect_identical(
    param_set$test_constraint_dt(wrapped_table(data_table)),
    c(TRUE, TRUE)
  )
  expect_identical(table_callbacks, 6L)

  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("structural shell was observed", call. = FALSE)
  }

  rows = native_stateful_altrep(
    list(list(x = 0.5)),
    list(list(x = 0.75)),
    callback = callback,
    callback_after = 0L
  )
  expect_error(
    .Call(altrep2_symbol("design_transpose_trafos"), rows, param_set),
    "ordinary list",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)

  result_callbacks = 0L
  result_set = ps(
    x = p_dbl(),
    .extra_trafo = function(x) native_stateful_altrep(
      list(x = x$x),
      list(x = 99),
      callback = function() result_callbacks <<- result_callbacks + 1L,
      callback_after = 0L
    )
  )
  expect_error(
    result_set$trafo(list(x = 0.5)),
    "ordinary list",
    fixed = TRUE
  )
  expect_identical(result_callbacks, 0L)
})

test_that("trafo result and public structural shells reject S4 or custom classes", {
  plain = ps(x = p_dbl(), .extra_trafo = identity)
  tabular = data.table::data.table(x = 1)
  data.table::setindexv(tabular, "x")
  expect_identical(plain$trafo(tabular), list(x = 1))
  expect_identical(
    .Call(altrep2_symbol("design_transpose"), tabular, FALSE),
    list(list(x = 1))
  )

  expect_error(
    .Call(altrep2_symbol("param_set_construct"),
      asS4(list(x = p_dbl()))),
    "ordinary named list",
    fixed = TRUE
  )
  expect_error(
    .Call(altrep2_symbol("param_set_construct"),
      structure(list(x = p_dbl()), class = "custom")),
    "ordinary named list",
    fixed = TRUE
  )
  expect_error(plain$trafo(asS4(list(x = 1))), "ordinary named list")
  expect_error(
    plain$trafo(structure(list(x = 1), class = "custom")),
    "ordinary named list"
  )

  classed = ps(
    x = p_dbl(),
    .extra_trafo = function(x) structure(list(x = x$x), class = "custom")
  )
  expect_error(classed$trafo(list(x = 1)), "ordinary")
  s4_result = ps(
    x = p_dbl(),
    .extra_trafo = function(x) asS4(list(x = x$x))
  )
  expect_error(s4_result$trafo(list(x = 1)), "ordinary")
  child = ps(
    x = p_dbl(),
    .extra_trafo = function(x) structure(list(x = x$x), class = "custom")
  )
  detached = psc(component = child)$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )
  expect_error(
    detached$trafo(list(component.x = 1)),
    "ordinary named list"
  )

  param_set = ps(x = p_dbl(0, 1))
  expect_identical(param_set$check_dt(tabular), TRUE)
  expect_identical(param_set$test_constraint_dt(tabular), TRUE)
  expect_identical(param_set$qunif(tabular)$x, 1)
  expect_length(.Call(
    altrep2_symbol("design_dependency_plan"),
    tabular,
    param_set
  )$rows, 0L)
  s4_table = asS4(data.frame(x = 0.5))
  expect_identical(.Call(
    altrep2_symbol("param_set_check_dt_builtin"),
    altrep2_private(param_set),
    param_set,
    s4_table,
    TRUE,
    "none",
    TRUE
  ), "Must be a data.frame or data.table.")
  expect_error(
    .Call(altrep2_symbol("param_set_qunif_builtin"),
      altrep2_private(param_set), param_set, s4_table),
    "numeric matrix or data.frame",
    fixed = TRUE
  )
  expect_error(
    .Call(altrep2_symbol("design_transpose"), asS4(list(x = 0.5)), FALSE),
    "list-like data frame",
    fixed = TRUE
  )
  expect_error(
    .Call(altrep2_symbol("design_dependency_plan"),
      asS4(list(x = 0.5)), param_set),
    "list-like data frame",
    fixed = TRUE
  )

  subclass = structure(
    data.frame(x = 0.5),
    class = c("custom", "data.frame")
  )
  expect_error(plain$trafo(subclass), "ordinary named list")
  expect_identical(.Call(
    altrep2_symbol("param_set_check_dt_builtin"),
    altrep2_private(param_set),
    param_set,
    subclass,
    TRUE,
    "none",
    TRUE
  ), "Must be a data.frame or data.table.")
  expect_error(.Call(
    altrep2_symbol("param_set_qunif_builtin"),
    altrep2_private(param_set),
    param_set,
    subclass
  ), "numeric matrix or data.frame", fixed = TRUE)
  expect_error(
    .Call(altrep2_symbol("design_transpose"), subclass, FALSE),
    "list-like data frame",
    fixed = TRUE
  )
  expect_error(
    .Call(altrep2_symbol("design_dependency_plan"), subclass, param_set),
    "list-like data frame",
    fixed = TRUE
  )

  zero_columns = data.frame(row.names = integer())
  expect_identical(param_set$check_dt(zero_columns), TRUE)
  attr(zero_columns, "row.names") = asS4(integer())
  expect_identical(
    param_set$check_dt(zero_columns),
    "Invalid data.frame row names."
  )
})

test_that("value merge alone snapshots shells while preserving opaque leaves", {
  altrep2_skip_without_helpers()
  marker = new.env(parent = emptyenv())
  callbacks = 0L
  updates = native_stateful_altrep(
    structure(list(a = 1L, payload = marker), names = c("a", "payload")),
    structure(list(a = 99L, payload = new.env()), names = c("a", "payload")),
    elt_switch_after = 2L,
    callback = function() {
      callbacks <<- callbacks + 1L
      invisible(gc())
    },
    callback_after = 0L
  )
  merged = .Call(
    altrep2_symbol("param_set_values_merge"),
    updates,
    list(b = 2L),
    NULL,
    FALSE
  )
  expect_identical(merged$a, 1L)
  expect_identical(merged$b, 2L)
  expect_identical(merged$payload, marker)
  expect_identical(callbacks, 1L)

  seen = NULL
  utility = ps(payload = p_uty(custom_check = function(value) {
    if (is.environment(value)) seen <<- value
    TRUE
  }))
  point = native_stateful_altrep(
    structure(list(payload = marker), names = "payload"),
    structure(list(payload = new.env()), names = "payload"),
    elt_switch_after = 1L
  )
  expect_identical(utility$check(point), "Must be an ordinary named list.")
  expect_null(seen)
  expect_identical(
    .Call(altrep2_symbol("design_transpose"), list(payload = list(marker)), FALSE)[[1L]]$payload,
    marker
  )
})

test_that("exotic ALTREP state is rejected before dispatch", {
  altrep2_skip_without_helpers()
  callbacks = 0L
  trap = function() {
    callbacks <<- callbacks + 1L
    stop("malformed ALTREP was evaluated", call. = FALSE)
  }

  bad_params_set = ps(x = p_int())
  params = altrep2_state(bad_params_set)$.params
  bad_ids = native_stateful_altrep(
    params$id,
    "changed",
    callback = trap,
    callback_after = 0L
  )
  altrep2_replace_core_field(
    bad_params_set,
    "params",
    altrep2_replace_table_column(params, "id", bad_ids)
  )
  expect_error(
    bad_params_set$params,
    "Corrupt ParamSet parameter state capsule",
    fixed = TRUE
  )

  bad_values_set = ps(x = p_int())
  bad_values = native_stateful_altrep(
    structure(list(x = 1L), names = "x"),
    structure(list(x = 2L), names = "x"),
    callback = trap,
    callback_after = 0L
  )
  altrep2_replace_core_field(bad_values_set, "values", bad_values)
  expect_error(
    bad_values_set$params,
    "Corrupt ParamSet parameter state capsule",
    fixed = TRUE
  )

  grid_set = ps(a = p_int(0L, 0L), b = p_int(0L, 1L))
  resolutions = native_stateful_altrep(
    structure(c(a = 1L, b = 2L)),
    structure(c(a = 1L, b = 2L)),
    callback = trap,
    callback_after = 0L
  )
  expect_error(.Call(
    altrep2_symbol("generate_design_grid_builtin"),
    altrep2_state(grid_set)$.params,
    resolutions
  ), "ordinary named numeric vector", fixed = TRUE)
  expect_identical(callbacks, 0L)
})

test_that("materialized and rejected inputs remain safe under forced collection", {
  skip_on_cran()
  altrep2_skip_without_helpers()

  callback = function() invisible(gc())
  unit = native_stateful_altrep(
    c(0.25, 0.75),
    c(0.25, 0.75),
    callback = callback,
    callback_after = 0L
  )
  param_set = ps(x = p_int(0L, 2L))
  point = native_stateful_altrep(
    structure(list(x = 1L), names = "x"),
    structure(list(x = 1L), names = "x"),
    callback = callback,
    callback_after = 0L
  )
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  quantiles = .Call(
    altrep2_symbol("domain_qunif_builtin"),
    p_dbl(0, 10),
    unit
  )
  checked = .Call(
    altrep2_symbol("param_set_check_builtin"),
    altrep2_private(param_set),
    param_set,
    point,
    TRUE,
    FALSE,
    "none",
    TRUE
  )
  gctorture(previous)

  expect_identical(quantiles, c(2.5, 7.5))
  expect_identical(checked, "Must be an ordinary named list.")
})
