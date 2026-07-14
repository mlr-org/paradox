native_altrep_replace_table_column = function(table, name, value) {
  table_class = class(table)
  table = unclass(table)
  table[[name]] = value
  class(table) = table_class
  table
}

test_that("ids materialize a changing required-tag filter once", {
  params = list(
    id = c("a", "b"),
    cls = c("ParamInt", "ParamInt")
  )
  tag_table = list(
    id = c("a", "a", "b"),
    tag = c("red", "blue", "red")
  )
  changing_filter = function() {
    native_stateful_altrep(
      c("red", "blue"),
      c("red", "red"),
      elt_switch_after = 2L
    )
  }

  observed = .Call(
    get("C_param_set_ids", envir = asNamespace("paradox")),
    params,
    tag_table,
    NULL,
    changing_filter(),
    NULL
  )

  expect_identical(observed, "a")

  param_set = ps(
    a = p_int(tags = c("red", "blue")),
    b = p_int(tags = "red")
  )
  expect_identical(param_set$ids(tags = changing_filter()), "a")
})

test_that("exact ids and trafo columns decline before ALTREP Length", {
  callbacks = 0L
  length_trap = function(value) {
    native_stateful_altrep(
      value,
      value,
      callback = function() callbacks <<- callbacks + 1L,
      callback_after = c(NA_integer_, 0L)
    )
  }
  ids_symbol = get("C_param_set_ids", envir = asNamespace("paradox"))
  params = list(id = c("a", "b"), cls = c("ParamInt", "ParamInt"))
  tags = list(id = c("a", "b"), tag = c("red", "blue"))

  expect_error(
    .Call(
      ids_symbol,
      list(id = length_trap(params$id), cls = params$cls),
      tags,
      NULL,
      NULL,
      NULL
    ),
    "ordinary character representation",
    fixed = TRUE
  )
  expect_error(
    .Call(
      ids_symbol,
      params,
      list(id = length_trap(tags$id), tag = tags$tag),
      NULL,
      "red",
      NULL
    ),
    "ordinary character representation",
    fixed = TRUE
  )

  trafo_table = structure(
    list(id = length_trap("x"), trafo = list(identity)),
    names = c("id", "trafo"),
    class = c("data.table", "data.frame"),
    sorted = "id"
  )
  expect_null(.Call(
    get("C_param_set_trafo_plan", envir = asNamespace("paradox")),
    list(x = 1),
    trafo_table
  ))
  expect_identical(callbacks, 0L)
})

test_that("ParamSet checks decline exact ALTREP state before Length", {
  callbacks = 0L
  length_trap = function(value, armed = TRUE) {
    native_stateful_altrep(
      value,
      value,
      callback = function() callbacks <<- callbacks + 1L,
      callback_after = if (armed) {
        c(NA_integer_, 0L)
      } else {
        c(NA_integer_, NA_integer_)
      }
    )
  }
  without_callback = function(value) {
    before = callbacks
    result = force(value)
    expect_identical(callbacks, before)
    result
  }
  symbol = get("C_param_set_check_builtin", envir = asNamespace("paradox"))
  param_set = ps(x = p_int(0L, 2L))
  original = param_set$.__enclos_env__$private$.params
  params = unclass(data.table::copy(original))
  values = list(x = 1L)

  stateful_params = length_trap(original, armed = FALSE)
  native_stateful_altrep_rearm(stateful_params, c(NA_integer_, 0L))
  expect_null(without_callback(
    .Call(symbol, stateful_params, values, FALSE)
  ))

  changed = params
  attr(changed, "names") = length_trap(names(changed), armed = FALSE)
  native_stateful_altrep_rearm(
    attr(changed, "names"),
    c(NA_integer_, 0L)
  )
  expect_null(without_callback(.Call(symbol, changed, values, FALSE)))

  for (column in c(
    "id", "cls", "lower", "upper", "tolerance", "levels",
    "special_vals", "storage_type"
  )) {
    changed = params
    changed[[column]] = length_trap(changed[[column]], armed = FALSE)
    native_stateful_altrep_rearm(
      changed[[column]],
      c(NA_integer_, 0L)
    )
    expect_null(
      without_callback(.Call(symbol, changed, values, FALSE)),
      info = column
    )
  }

  factor_params = unclass(data.table::copy(
    ps(x = p_fct(c("a", "b")))$.__enclos_env__$private$.params
  ))
  factor_params$levels[[1L]] = length_trap(
    factor_params$levels[[1L]],
    armed = FALSE
  )
  native_stateful_altrep_rearm(
    factor_params$levels[[1L]],
    c(NA_integer_, 0L)
  )
  expect_null(without_callback(
    .Call(symbol, factor_params, list(x = "a"), FALSE)
  ))

  changed = params
  changed$special_vals[[1L]] = length_trap(list(), armed = FALSE)
  native_stateful_altrep_rearm(
    changed$special_vals[[1L]],
    c(NA_integer_, 0L)
  )
  expect_null(without_callback(.Call(symbol, changed, values, FALSE)))

  forged_sanitize = length_trap(FALSE)
  expect_null(without_callback(
    .Call(symbol, params, values, forged_sanitize)
  ))
  expect_identical(callbacks, 0L)
})

test_that("ParamSet checks take callback-zero fallbacks for public ALTREP", {
  callbacks = 0L
  length_trap = function(value, armed = TRUE) {
    native_stateful_altrep(
      value,
      value,
      callback = function() callbacks <<- callbacks + 1L,
      callback_after = if (armed) {
        c(NA_integer_, 0L)
      } else {
        c(NA_integer_, NA_integer_)
      }
    )
  }
  without_callback = function(value) {
    before = callbacks
    result = force(value)
    expect_identical(callbacks, before)
    result
  }
  param_set = ps(x = p_int(0L, 2L))
  params = param_set$.__enclos_env__$private$.params
  scalar_symbol = get(
    "C_param_set_check_builtin",
    envir = asNamespace("paradox")
  )
  table_symbol = get(
    "C_param_set_check_dt_builtin",
    envir = asNamespace("paradox")
  )

  stateful_values = length_trap(list(x = 1L), armed = FALSE)
  native_stateful_altrep_rearm(stateful_values, c(NA_integer_, 0L))
  expect_null(without_callback(
    .Call(scalar_symbol, params, stateful_values, FALSE)
  ))
  values = list(x = 1L)
  attr(values, "names") = length_trap("x", armed = FALSE)
  native_stateful_altrep_rearm(
    attr(values, "names"),
    c(NA_integer_, 0L)
  )
  expect_null(without_callback(.Call(scalar_symbol, params, values, FALSE)))
  expect_null(without_callback(.Call(
    scalar_symbol,
    params,
    list(x = length_trap(1L)),
    FALSE
  )))

  table = data.frame(x = 1L)
  stateful_table = length_trap(table, armed = FALSE)
  native_stateful_altrep_rearm(stateful_table, c(NA_integer_, 0L))
  expect_null(without_callback(
    .Call(table_symbol, params, stateful_table)
  ))
  changed = table
  attr(changed, "names") = length_trap("x", armed = FALSE)
  native_stateful_altrep_rearm(
    attr(changed, "names"),
    c(NA_integer_, 0L)
  )
  expect_null(without_callback(.Call(table_symbol, params, changed)))
  changed = table
  changed[[1L]] = length_trap(changed[[1L]], armed = FALSE)
  native_stateful_altrep_rearm(changed[[1L]], c(NA_integer_, 0L))
  expect_null(without_callback(.Call(table_symbol, params, changed)))
  expect_identical(callbacks, 0L)
})

test_that("Domain construction declines ALTREP before every Length probe", {
  callbacks = 0L
  length_trap = function(value, armed = TRUE) {
    native_stateful_altrep(
      value,
      value,
      callback = function() callbacks <<- callbacks + 1L,
      callback_after = if (armed) {
        c(NA_integer_, 0L)
      } else {
        c(NA_integer_, NA_integer_)
      }
    )
  }
  without_callback = function(value) {
    before = callbacks
    result = force(value)
    expect_identical(callbacks, before)
    result
  }
  symbol = get("C_domain_construct", envir = asNamespace("paradox"))
  baseline = list(
    "ParamDbl", "ParamDbl", NULL, 0, 1, 0, NULL, list(),
    paradox:::NO_DEF, character(), NULL, "numeric", FALSE, NULL
  )
  call_construct = function(arguments) {
    do.call(.Call, c(list(symbol), arguments))
  }

  for (index in c(1L, 2L, 4L, 5L, 6L, 8L, 10L, 12L, 13L)) {
    changed = baseline
    changed[[index]] = length_trap(changed[[index]], armed = FALSE)
    native_stateful_altrep_rearm(
      changed[[index]],
      c(NA_integer_, 0L)
    )
    expect_null(without_callback(call_construct(changed)), info = index)
  }
  changed = baseline
  changed[[3L]] = length_trap(list(), armed = FALSE)
  native_stateful_altrep_rearm(changed[[3L]], c(NA_integer_, 0L))
  expect_null(without_callback(call_construct(changed)))

  factor_arguments = baseline
  factor_arguments[[1L]] = "ParamFct"
  factor_arguments[[2L]] = "ParamFct"
  factor_arguments[[7L]] = length_trap(c("a", "b"), armed = FALSE)
  native_stateful_altrep_rearm(
    factor_arguments[[7L]],
    c(NA_integer_, 0L)
  )
  factor_arguments[[12L]] = "character"
  expect_null(without_callback(call_construct(factor_arguments)))

  disable_in_tune = length_trap(list(disabled = TRUE), armed = FALSE)
  internal_arguments = baseline
  internal_arguments[[3L]] = list(
    aggr = identity,
    in_tune_fn = identity,
    disable_in_tune = disable_in_tune
  )
  native_stateful_altrep_rearm(
    internal_arguments[[3L]]$disable_in_tune,
    c(NA_integer_, 0L)
  )
  internal_arguments[[10L]] = "internal_tuning"
  expect_null(without_callback(call_construct(internal_arguments)))
  expect_identical(callbacks, 0L)
})

test_that("get_domain declines ALTREP ids before callbacks", {
  param_set = ps(x = p_dbl(0, 1, tags = "before"))
  private = param_set$.__enclos_env__$private
  callbacks = 0L
  request = native_stateful_altrep(
    "x",
    "x",
    callback = function() {
      callbacks <<- callbacks + 1L
      data.table::set(private$.tags, i = 1L, j = "tag", value = "after")
      invisible(gc())
    },
    callback_after = 0L
  )

  expect_null(.Call(
    get("C_param_set_get_domain", envir = asNamespace("paradox")),
    private,
    param_set,
    request
  ))

  expect_identical(callbacks, 0L)
  expect_identical(private$.tags$tag, "before")

  observed = param_set$get_domain(request)

  expect_identical(callbacks, 1L)
  expect_identical(observed$id, "x")
  expect_identical(observed$.tags[[1L]], "after")

  length_callbacks = 0L
  length_request = native_stateful_altrep(
    "x",
    "x",
    callback = function() {
      length_callbacks <<- length_callbacks + 1L
      stop("native get_domain observed ALTREP Length", call. = FALSE)
    },
    callback_after = c(NA_integer_, 0L)
  )
  expect_null(.Call(
    get("C_param_set_get_domain", envir = asNamespace("paradox")),
    private,
    param_set,
    length_request
  ))
  expect_identical(length_callbacks, 0L)
})

test_that("exact ParamSet fast paths reject callback-capable private shells", {
  param_set = ps(x = p_dbl(0, 1, tags = "tag", init = 0.5))
  private = param_set$.__enclos_env__$private
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("exact private state invoked an ALTREP callback")
  }
  get_domain_symbol = get(
    "C_param_set_get_domain",
    envir = asNamespace("paradox")
  )

  for (field in c(".params", ".tags", ".trafos", ".deps", ".values")) {
    original = private[[field]]
    private[[field]] = native_stateful_altrep(
      original,
      original,
      elt_switch_after = 0L,
      length_switch_after = 0L,
      callback = callback,
      callback_after = 0L
    )
    expect_null(
      .Call(get_domain_symbol, private, param_set, "x"),
      info = field
    )
    private[[field]] = original
  }
  expect_identical(callbacks, 0L)

  original_params = private$.params
  levels = original_params$levels
  levels[[1L]] = native_stateful_altrep(
    c("first", "second"),
    c("changed"),
    elt_switch_after = 0L,
    length_switch_after = 0L,
    callback = callback,
    callback_after = 0L
  )
  private$.params = native_altrep_replace_table_column(
    original_params,
    "levels",
    levels
  )
  on.exit(private$.params <- original_params, add = TRUE)
  expect_null(.Call(get_domain_symbol, private, param_set, "x"))
  expect_identical(callbacks, 0L)
})

test_that("batch domains and subset decline ALTREP exact columns", {
  param_set = ps(x = p_fct(c("first", "second"), init = "first"))
  private = param_set$.__enclos_env__$private
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("native exact-state validator invoked ALTREP")
  }
  original_params = private$.params
  levels = original_params$levels
  levels[[1L]] = native_stateful_altrep(
    levels[[1L]],
    character(),
    elt_switch_after = 0L,
    length_switch_after = 0L,
    callback = callback,
    callback_after = 0L
  )
  private$.params = native_altrep_replace_table_column(
    original_params,
    "levels",
    levels
  )
  on.exit(private$.params <- original_params)

  expect_null(.Call(
    get("C_param_set_domains", envir = asNamespace("paradox")),
    private,
    param_set
  ))
  expect_null(.Call(
    get("C_param_set_subset_state", envir = asNamespace("paradox")),
    private,
    param_set,
    "x",
    FALSE
  ))
  expect_identical(callbacks, 0L)
  })

test_that("property kernels reject ALTREP shells, names, and pointer columns", {
  symbol = get("C_param_set_property", envir = asNamespace("paradox"))
  valid = list(
    id = "x",
    cls = "ParamInt",
    lower = 0,
    upper = 2,
    levels = list(NULL)
  )
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("property validation invoked ALTREP")
  }
  stateful = function(first, later = first) {
    native_stateful_altrep(
      first,
      later,
      elt_switch_after = 0L,
      length_switch_after = 0L,
      callback = callback,
      callback_after = 0L
    )
  }

  expect_error(
    .Call(symbol, stateful(valid), 0L),
    "ordinary list representation",
    fixed = TRUE
  )
  altrep_names = valid
  attr(altrep_names, "names") = stateful(names(valid), rev(names(valid)))
  expect_error(
    .Call(symbol, altrep_names, 0L),
    "ordinary character representation",
    fixed = TRUE
  )
  altrep_numeric = valid
  altrep_numeric$lower = stateful(0, numeric())
  expect_error(
    .Call(symbol, altrep_numeric, 0L),
    "ordinary numeric representation",
    fixed = TRUE
  )
  altrep_levels = valid
  altrep_levels$levels = stateful(list(NULL), list())
  expect_error(
    .Call(symbol, altrep_levels, 0L),
    "ordinary representations",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("Domain and bulk qunif reject ids before ALTREP Length dispatch", {
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("native admission invoked ALTREP Length", call. = FALSE)
  }
  changing_ids = function(ids) {
    native_stateful_altrep(
      ids,
      rev(ids),
      callback = callback,
      callback_after = c(NA_integer_, 0L)
    )
  }

  domain = p_int(0L, 3L)
  # R materializes an ALTREP value while assigning the special `class`
  # attribute, so no callback-free adversarial class fixture can be created
  # through the public API. The ID-column probes below retain ALTREP and cover
  # the same native admission order for Domain and bulk qunif.
  fancy_domain = native_altrep_replace_table_column(
    domain,
    "id",
    changing_ids(domain$id)
  )
  expect_error(
    .Call(
      get("C_domain_check_builtin", envir = asNamespace("paradox")),
      fancy_domain,
      list(1L)
    ),
    "ordinary character representation",
    fixed = TRUE
  )

  param_set = ps(x = p_int(0L, 3L))
  params = param_set$.__enclos_env__$private$.params
  fancy_params = native_altrep_replace_table_column(
    params,
    "id",
    changing_ids(params$id)
  )
  units = matrix(0.5, nrow = 1L, dimnames = list(NULL, "x"))
  expect_null(.Call(
    get("C_param_set_qunif_builtin", envir = asNamespace("paradox")),
    fancy_params,
    units
  ))
  expect_identical(callbacks, 0L)
})

test_that("wrapper authentication rejects ALTREP defaults before Length", {
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("wrapper authentication invoked ALTREP Length", call. = FALSE)
  }
  stateful = function(value) {
    native_stateful_altrep(
      value,
      value,
      callback = callback,
      callback_after = c(NA_integer_, 0L)
    )
  }
  replace_locked = function(environment, name, replacement) {
    unlockBinding(name, environment)
    assign(name, replacement, envir = environment)
    lockBinding(name, environment)
  }

  namespace = asNamespace("paradox")
  frame = list2env(list(
    class = NULL,
    tags = NULL,
    any_tags = NULL,
    type = "with_token",
    check_required = TRUE,
    remove_dependencies = TRUE
  ), parent = namespace)
  get_values_symbol = get("C_param_set_get_values", envir = namespace)
  for (case in list(
      list(name = "type", value = stateful("with_token")),
      list(name = "check_required", value = stateful(TRUE)),
      list(name = "remove_dependencies", value = stateful(TRUE))
    )) {
    param_set = ps(x = p_int(init = 1L))
    private = param_set$.__enclos_env__$private
    forged = param_set$get_values
    defaults = formals(forged)
    defaults[[case$name]] = case$value
    formals(forged) = defaults
    replace_locked(param_set, "get_values", forged)
    expect_null(.Call(get_values_symbol, private, param_set, frame))
  }

  assign_symbol = get(
    "C_param_set_assign_values_checked",
    envir = namespace
  )
  for (case in list(
      list(name = "check_strict", value = stateful(TRUE)),
      list(name = "presence", value = stateful("none"))
    )) {
    param_set = ps(x = p_int())
    private = param_set$.__enclos_env__$private
    forged = param_set$assert
    defaults = formals(forged)
    defaults[[case$name]] = case$value
    formals(forged) = defaults
    replace_locked(param_set, "assert", forged)
    expect_null(.Call(assign_symbol, private, param_set, list(x = 1L)))
  }
  expect_identical(callbacks, 0L)
})

test_that("collection values decline ALTREP root ids and leaf value lists", {
  child = ps(x = p_int(init = 1L))
  collection = ParamSetCollection$new(list(owner = child))
  symbol = get(
    "C_param_set_collection_values",
    envir = asNamespace("paradox")
  )
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("collection values invoked ALTREP")
  }
  stateful = function(first, later = first) {
    native_stateful_altrep(
      first,
      later,
      elt_switch_after = 0L,
      length_switch_after = 0L,
      callback = callback,
      callback_after = 0L
    )
  }

  private = collection$.__enclos_env__$private
  original_params = private$.params
  private$.params = native_altrep_replace_table_column(
    original_params,
    "id",
    stateful(original_params$id, character())
  )
  expect_null(.Call(symbol, private, collection))
  private$.params = original_params

  child_private = child$.__enclos_env__$private
  original_values = child_private$.values
  child_private$.values = stateful(original_values, list())
  on.exit(child_private$.values <- original_values)
  expect_null(.Call(symbol, private, collection))
  expect_identical(callbacks, 0L)
})

test_that("transpose declines a changing container before any callback", {
  callbacks = 0L
  data = native_stateful_altrep(
    structure(
      list(c(1L, 2L), c(3, 4)),
      names = c("left", "right")
    ),
    structure(
      list(c(10L, 20L), c(30, 40)),
      names = c("left", "right")
    ),
    length_switch_after = 0L,
    elt_switch_after = 0L,
    callback = function() {
      callbacks <<- callbacks + 1L
      invisible(gc())
    },
    callback_after = 0L
  )
  observed = .Call(
    get("C_design_transpose", envir = asNamespace("paradox")),
    data,
    FALSE
  )
  expect_null(observed)
  expect_identical(callbacks, 0L)
})

test_that("ParamSet construction sizes and fills from one Domain snapshot", {
  callbacks = 0L
  first = structure(list(
    a = p_int(0L, 1L, tags = "first", init = 0L),
    b = p_dbl(10, 20, tags = "stale", init = 11)
  ), names = c("a", "b"))
  later = structure(list(
    a = p_int(100L, 101L, tags = "unused", init = 100L),
    b = p_dbl(30, 40, tags = c("later", "second"), init = 31)
  ), names = c("a", "b"))
  domains = native_stateful_altrep(
    first,
    later,
    elt_switch_after = 1L,
    callback = function() {
      callbacks <<- callbacks + 1L
      invisible(gc())
    },
    callback_after = 0L
  )

  observed = .Call(
    get("C_param_set_construct", envir = asNamespace("paradox")),
    domains
  )
  expect_identical(callbacks, 1L)
  expect_identical(observed$params$id, c("a", "b"))
  expect_identical(observed$params$lower, c(0, 30))
  expect_identical(observed$params$upper, c(1, 40))
  expect_identical(observed$tags$id, c("a", "b", "b"))
  expect_identical(observed$tags$tag, c("first", "later", "second"))
  expect_identical(observed$init_values, list(a = 0L, b = 31))
})

test_that("ParamSet construction bounds changing outer ALTREP traversal", {
  callbacks = 0L
  first = list(x = p_int(0L, 1L))
  later = rep(list(p_int(10L, 20L)), 10000L)
  domains = native_stateful_altrep(
    first,
    later,
    length_switch_after = 1L,
    callback = function() {
      callbacks <<- callbacks + 1L
    },
    callback_after = c(NA_integer_, 1L)
  )

  observed = .Call(
    get("C_param_set_construct", envir = asNamespace("paradox")),
    domains
  )
  # VECTOR_ELT must ask an ALTLIST for its current Length. Allocation and the
  # loop remain bounded by the first observation even when that later grows.
  expect_identical(callbacks, 1L)
  expect_identical(observed$params$id, "x")
  expect_identical(observed$params$lower, 0L)
  expect_identical(observed$params$upper, 1L)

  shrinking = native_stateful_altrep(
    list(x = p_int(), y = p_int()),
    list(x = p_int()),
    length_switch_after = 1L
  )
  expect_error(.Call(
    get("C_param_set_construct", envir = asNamespace("paradox")),
    shrinking
  ))
})

test_that("ParamSet construction authenticates Domain storage before ALTREP", {
  symbol = get("C_param_set_construct", envir = asNamespace("paradox"))
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("Domain authentication invoked ALTREP", call. = FALSE)
  }
  stateful_unarmed = function(value) {
    fixture = native_stateful_altrep(
      value,
      value,
      callback = callback,
      callback_after = c(NA_integer_, NA_integer_)
    )
    fixture
  }
  base = p_int(0L, 1L, tags = "tag", depends = parent == 1L)

  outer_names = list(x = base)
  outer_name_fixture = stateful_unarmed("x")
  attr(outer_names, "names") = outer_name_fixture
  native_stateful_altrep_rearm(outer_name_fixture, c(0L, 0L))
  expect_null(.Call(symbol, outer_names))

  domain_fixture = stateful_unarmed(base)
  shell_domains = list(x = domain_fixture)
  native_stateful_altrep_rearm(domain_fixture, c(0L, 0L))
  expect_null(.Call(symbol, shell_domains))

  domain_names = data.table::copy(base)
  domain_name_fixture = stateful_unarmed(names(domain_names))
  attr(domain_names, "names") = domain_name_fixture
  native_stateful_altrep_rearm(domain_name_fixture, c(0L, 0L))
  expect_null(.Call(symbol, list(x = domain_names)))

  for (column in names(base)) {
    fixture = stateful_unarmed(base[[column]])
    changed = native_altrep_replace_table_column(
      base,
      column,
      fixture
    )
    native_stateful_altrep_rearm(fixture, c(0L, 0L))
    expect_null(.Call(symbol, list(x = changed)), info = column)
  }

  nested_tags = base$.tags
  tag_fixture = stateful_unarmed(nested_tags[[1L]])
  nested_tags[[1L]] = tag_fixture
  changed = native_altrep_replace_table_column(base, ".tags", nested_tags)
  native_stateful_altrep_rearm(tag_fixture, c(0L, 0L))
  expect_null(.Call(symbol, list(x = changed)))

  nested_requirements = base$.requirements
  requirement_fixture = stateful_unarmed(nested_requirements[[1L]])
  nested_requirements[[1L]] = requirement_fixture
  changed = native_altrep_replace_table_column(
    base,
    ".requirements",
    nested_requirements
  )
  native_stateful_altrep_rearm(requirement_fixture, c(0L, 0L))
  expect_null(.Call(symbol, list(x = changed)))
  expect_identical(callbacks, 0L)
})

test_that("value merge and store reuse one changing-list observation", {
  callbacks = 0L
  changing = native_stateful_altrep(
    list(a = 1L, b = 2L),
    list(a = 10L, b = 20L),
    elt_switch_after = 1L,
    callback = function() {
      callbacks <<- callbacks + 1L
      invisible(gc())
    },
    callback_after = 0L
  )
  merge = get(
    "C_param_set_values_merge",
    envir = asNamespace("paradox")
  )
  expect_identical(
    .Call(merge, changing, list(c = 3L), NULL, FALSE),
    list(a = 1L, b = 20L, c = 3L)
  )
  expect_identical(callbacks, 1L)

  param_set = ps(a = p_int(), b = p_int())
  private = param_set$.__enclos_env__$private
  changing = native_stateful_altrep(
    list(a = 4L, b = 5L),
    list(a = 40L, b = 50L),
    elt_switch_after = 1L
  )
  stored = .Call(
    get("C_param_set_store_values", envir = asNamespace("paradox")),
    private,
    param_set,
    changing
  )
  expect_identical(stored, list(a = 4L, b = 50L))
  expect_identical(private$.values, stored)
})

test_that("value snapshots avoid hidden post-snapshot ALTREP Length", {
  merge = get("C_param_set_values_merge", envir = asNamespace("paradox"))
  store = get("C_param_set_store_values", envir = asNamespace("paradox"))
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("value snapshot made an extra ALTREP Length observation", call. = FALSE)
  }
  length_fixture = function(value) {
    native_stateful_altrep(
      value,
      value,
      callback = callback,
      callback_after = c(NA_integer_, NA_integer_)
    )
  }

  merge_values = length_fixture(list(a = 1L))
  native_stateful_altrep_rearm(
    merge_values,
    c(NA_integer_, 2L)
  )
  expect_identical(
    .Call(merge, merge_values, list(b = 2L), NULL, FALSE),
    list(a = 1L, b = 2L)
  )

  param_set = ps(a = p_int())
  private = param_set$.__enclos_env__$private
  store_values = length_fixture(list(a = 3L))
  native_stateful_altrep_rearm(
    store_values,
    c(NA_integer_, 2L)
  )
  expect_identical(
    .Call(store, private, param_set, store_values),
    list(a = 3L)
  )

  named_values = list(a = 4L)
  name_fixture = native_stateful_altrep(
    "a",
    rep("changed", 100L),
    callback = callback,
    callback_after = c(NA_integer_, NA_integer_)
  )
  attr(named_values, "names") = name_fixture
  native_stateful_altrep_rearm(
    name_fixture,
    c(NA_integer_, 2L)
  )
  expect_identical(
    .Call(store, private, param_set, named_values),
    list(a = 4L)
  )
  expect_identical(callbacks, 0L)
})

test_that("bulk qunif snapshots matrix units and nested factor levels", {
  param_set = ps(
    factor = p_fct(c("slow", "fast")),
    double = p_dbl(0, 10)
  )
  params = param_set$.__enclos_env__$private$.params
  first = matrix(
    c(0, 1, 0.25, 0.75),
    nrow = 2L,
    dimnames = list(NULL, c("factor", "double"))
  )
  later = matrix(
    rep(2, 4L),
    nrow = 2L,
    dimnames = dimnames(first)
  )
  units = native_stateful_altrep(
    first,
    later,
    elt_switch_after = length(first)
  )
  symbol = get(
    "C_param_set_qunif_builtin",
    envir = asNamespace("paradox")
  )
  observed = .Call(symbol, params, units)
  expect_identical(observed$factor, c("slow", "fast"))
  expect_identical(observed$double, c(2.5, 7.5))

  params = data.table::copy(params)
  levels = params$levels
  factor_row = match("factor", params$id)
  levels[[factor_row]] = native_stateful_altrep(
    c("slow", "fast"),
    c("changed-slow", "changed-fast"),
    elt_switch_after = 3L
  )
  params = native_altrep_replace_table_column(params, "levels", levels)
  factor_units = matrix(
    c(0, 1),
    nrow = 2L,
    dimnames = list(NULL, "factor")
  )
  expect_identical(
    .Call(symbol, params, factor_units)$factor,
    c("slow", "fast")
  )
})

test_that("bulk qunif retains rooted ParamSet columns across input callbacks", {
  param_set = ps(
    amount = p_dbl(0, 10),
    mode = p_fct(c("slow", "fast"))
  )
  params = param_set$.__enclos_env__$private$.params
  callbacks = 0L
  first = matrix(
    c(0.5, 0.75),
    nrow = 1L,
    dimnames = list(NULL, c("amount", "mode"))
  )
  units = native_stateful_altrep(
    first,
    first,
    callback = function() {
      callbacks <<- callbacks + 1L
      data.table::set(
        params,
        j = "id",
        value = c("changed-amount", "changed-mode")
      )
      data.table::set(params, j = "cls", value = rep("ParamLgl", 2L))
      data.table::set(params, j = "lower", value = c(100, 100))
      data.table::set(params, j = "upper", value = c(200, 200))
      data.table::set(
        params,
        j = "levels",
        value = list(c(TRUE, FALSE), c(TRUE, FALSE))
      )
      data.table::set(
        params,
        j = "storage_type",
        value = rep("logical", 2L)
      )
      invisible(gc())
    },
    callback_after = 0L
  )

  observed = .Call(
    get("C_param_set_qunif_builtin", envir = asNamespace("paradox")),
    params,
    units
  )
  expect_identical(callbacks, 1L)
  expect_identical(observed$amount, 5)
  expect_identical(observed$mode, "fast")
})

test_that("bulk qunif bounds a growing public ALTREP matrix", {
  param_set = ps(
    amount = p_dbl(0, 10),
    mode = p_fct(c("slow", "fast"))
  )
  params = param_set$.__enclos_env__$private$.params
  first = matrix(
    c(0.5, 0.75),
    nrow = 1L,
    dimnames = list(NULL, c("amount", "mode"))
  )
  units = native_stateful_altrep(
    first,
    matrix(rep(0.5, 100L), nrow = 50L),
    length_switch_after = 1L
  )

  observed = .Call(
    get("C_param_set_qunif_builtin", envir = asNamespace("paradox")),
    params,
    units
  )
  expect_identical(observed$amount, 5)
  expect_identical(observed$mode, "fast")
})

test_that("Domain kernels never reread changing scalar observations", {
  sanitize = get(
    "C_domain_sanitize_builtin",
    envir = asNamespace("paradox")
  )
  changing_double = native_stateful_altrep(
    c(-1, 2),
    c(100, 100),
    elt_switch_after = 2L
  )
  expect_identical(
    .Call(sanitize, p_dbl(0, 1), changing_double),
    list(0, 1)
  )
  changing_integer = native_stateful_altrep(
    c(1.2, 1.8),
    c(9.2, 9.8),
    elt_switch_after = 2L
  )
  expect_identical(
    .Call(sanitize, p_int(), changing_integer),
    list(1L, 2L)
  )

  check = get("C_domain_check_builtin", envir = asNamespace("paradox"))
  selected = native_stateful_altrep(
    "slow",
    "missing",
    elt_switch_after = 1L
  )
  expect_identical(
    .Call(check, p_fct(c("slow", "fast")), list(selected)),
    TRUE
  )
})

test_that("Domain raw views retain independently rooted storage", {
  check = get("C_domain_check_builtin", envir = asNamespace("paradox"))
  domain = p_dbl(0, 1, tolerance = 0.1)
  changing = native_stateful_altrep(
    -0.05,
    -0.05,
    callback = function() {
      # Row-qualified `set()` mutates the existing column vectors in place.
      # The native kernel must already have copied their values, not merely
      # retained protected pointers to those R vectors.
      data.table::set(domain, i = 1L, j = "lower", value = 100)
      data.table::set(domain, i = 1L, j = "upper", value = 200)
      data.table::set(domain, i = 1L, j = "tolerance", value = 0)
      invisible(gc())
    },
    callback_after = 0L
  )
  expect_identical(.Call(check, domain, list(changing)), TRUE)

  sanitize = get("C_domain_sanitize_builtin", envir = asNamespace("paradox"))
  domain = p_dbl(0, 10)
  values = native_stateful_altrep(
    -1,
    -1,
    callback = function() {
      data.table::set(domain, i = 1L, j = "lower", value = 100)
      data.table::set(domain, i = 1L, j = "upper", value = 200)
      invisible(gc())
    },
    callback_after = 0L
  )
  expect_identical(.Call(sanitize, domain, values), list(0))

  check = get("C_domain_check_builtin", envir = asNamespace("paradox"))
  domain = p_fct(c("slow", "fast"))
  values = native_stateful_altrep(
    list("slow"),
    list("slow"),
    callback = function() {
      data.table::set(domain, j = "levels", value = list("changed"))
      invisible(gc())
    },
    callback_after = 0L
  )
  expect_identical(.Call(check, domain, values), TRUE)

  qunif = get("C_domain_qunif_builtin", envir = asNamespace("paradox"))
  domain = p_dbl(0, 10)
  unit = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      data.table::set(domain, i = 1L, j = "lower", value = 100)
      data.table::set(domain, i = 1L, j = "upper", value = 200)
      invisible(gc())
    },
    callback_after = 0L
  )
  expect_identical(.Call(qunif, domain, unit), 5)
})

test_that("logical Domain qunif snapshots structure before public callbacks", {
  qunif = get("C_domain_qunif_builtin", envir = asNamespace("paradox"))
  original = matrix(
    c(0.25, 0.75),
    nrow = 1L,
    dimnames = list("old-row", c("old-a", "old-b"))
  )
  unit = NULL
  unit = native_stateful_altrep(
    original,
    original,
    callback = function() {
      data.table::setattr(
        unit,
        "dimnames",
        list("new-row", c("new-a", "new-b"))
      )
      invisible(gc())
    },
    callback_after = 0L
  )

  observed = .Call(qunif, p_lgl(), unit)
  expect_identical(observed, original < 0.5)
})

test_that("Domain qunif bounds growing public ALTREP input", {
  symbol = get("C_domain_qunif_builtin", envir = asNamespace("paradox"))
  cases = list(
    list(domain = p_dbl(0, 10), unit = 0.5, expected = 5),
    list(domain = p_int(0L, 10L), unit = 0.5, expected = 5L),
    list(
      domain = p_fct(c("slow", "fast")),
      unit = 0.75,
      expected = "fast"
    ),
    list(domain = p_lgl(), unit = 0.25, expected = TRUE)
  )

  for (case in cases) {
    unit = native_stateful_altrep(
      case$unit,
      rep(case$unit, 100L),
      length_switch_after = 1L
    )
    expect_identical(
      .Call(symbol, case$domain, unit),
      case$expected,
      info = class(case$domain)[[1L]]
    )
  }
})

test_that("get_values bounds post-callback ALTSTRING names snapshots", {
  class_name = "NativeAltRepGetValuesCondition"
  registerS3method(
    "condition_test",
    class_name,
    function(cond, x) cond$callback(x),
    envir = asNamespace("paradox")
  )
  param_set = ps(
    first = p_int(init = 1L),
    second = p_int(init = 2L),
    third = p_int(init = 3L)
  )
  private = param_set$.__enclos_env__$private
  calls = integer()
  first = CondEqual(1L)
  first$callback = function(x) {
    calls <<- c(calls, 1L)
    stateful_names = native_stateful_altrep(
      c("first", "second", "third"),
      c("first", "second", "third", rep("overflow", 10000L)),
      length_switch_after = 2L
    )
    data.table::setattr(private$.values, "names", stateful_names)
    TRUE
  }
  class(first) = c(class_name, class(first))
  second = CondEqual(1L)
  second$callback = function(x) {
    calls <<- c(calls, 2L)
    data.table::setattr(
      private$.values,
      "names",
      c("first", "second", "third")
    )
    TRUE
  }
  class(second) = c(class_name, class(second))
  param_set$add_dep("second", "first", first)
  param_set$add_dep("third", "first", second)

  expect_identical(
    param_set$get_values(check_required = FALSE),
    list(first = 1L, second = 2L, third = 3L)
  )
  expect_identical(calls, c(1L, 2L))
})

test_that("checked assignment declines callback-capable values before auth", {
  param_set = ps(a = p_int())
  private = param_set$.__enclos_env__$private
  callbacks = 0L
  changing = native_stateful_altrep(
    list(a = 1L),
    list(a = 1L),
    callback = function() {
      callbacks <<- callbacks + 1L
      data.table::set(private$.params, j = "id", value = "changed")
      invisible(gc())
    },
    callback_after = 0L
  )

  observed = .Call(
    get(
      "C_param_set_assign_values_checked",
      envir = asNamespace("paradox")
    ),
    private,
    param_set,
    changing
  )
  expect_null(observed)
  expect_identical(callbacks, 0L)
  expect_identical(private$.params$id, "a")
  expect_identical(private$.values, structure(list(), names = character()))
})

test_that("value storage revalidates IDs after input callbacks", {
  param_set = ps(a = p_int())
  private = param_set$.__enclos_env__$private
  callbacks = 0L
  changing = native_stateful_altrep(
    list(a = 1L),
    list(a = 1L),
    callback = function() {
      callbacks <<- callbacks + 1L
      data.table::set(private$.params, j = "id", value = "changed")
      invisible(gc())
    },
    callback_after = 0L
  )

  observed = .Call(
    get("C_param_set_store_values", envir = asNamespace("paradox")),
    private,
    param_set,
    changing
  )
  expect_null(observed)
  expect_identical(callbacks, 1L)
  expect_identical(private$.params$id, "changed")
  expect_identical(private$.values, structure(list(), names = character()))
})

test_that("collection planning declines replaced live child lists", {
  first = ps(a = p_int())
  second = ps(b = p_int())
  collection = ParamSetCollection$new(list(first = first, second = second))
  private = collection$.__enclos_env__$private
  original_sets = private$.sets
  callbacks = 0L
  changing = native_stateful_altrep(
    list(first.a = 1L),
    list(first.a = 1L),
    callback = function() {
      callbacks <<- callbacks + 1L
      private$.sets = list(replacement = ps(z = p_int()))
      invisible(gc())
    },
    callback_after = 0L
  )

  observed = .Call(
    get(
      "C_param_set_collection_store_plan",
      envir = asNamespace("paradox")
    ),
    private,
    collection,
    original_sets,
    changing
  )
  expect_null(observed)
  expect_identical(callbacks, 1L)
  expect_identical(first$values, structure(list(), names = character()))
  expect_identical(second$values, structure(list(), names = character()))
})
