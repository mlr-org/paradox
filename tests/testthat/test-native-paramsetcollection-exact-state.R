collection_exact_params_symbol = function() {
  get(
    "C_param_set_collection_params",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

collection_exact_values_symbol = function() {
  get(
    "C_param_set_collection_values",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

collection_exact_check_symbol = function() {
  get(
    "C_param_set_collection_check_builtin",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

collection_exact_check = function(
    collection,
    values,
    sanitize = FALSE,
    check_strict = TRUE) {
  .Call(
    collection_exact_check_symbol(),
    collection_exact_private(collection),
    collection,
    values,
    sanitize,
    check_strict
  )
}

collection_exact_fixture = function() {
  child = ps(
    first = p_int(init = 1L),
    second = p_lgl(init = FALSE)
  )
  child$values = list(first = 2L, second = TRUE)
  ParamSetCollection$new(list(owner = child))
}

collection_exact_private = function(collection) {
  collection$.__enclos_env__$private
}

collection_exact_replace_column = function(table, name, value) {
  table_attributes = attributes(table)
  result = unclass(table)
  result[[name]] = value
  attributes(result) = table_attributes
  result
}

collection_exact_replace_attribute = function(object, name, value) {
  object_attributes = attributes(object)
  object_attributes[[name]] = value
  attributes(object) = object_attributes
  object
}

collection_exact_expect_both_null = function(collection, info = NULL) {
  private = collection_exact_private(collection)
  expect_null(
    .Call(collection_exact_params_symbol(), private, collection),
    info = info
  )
  expect_null(
    .Call(collection_exact_values_symbol(), private, collection),
    info = info
  )
}

test_that("collection exact-state shells decline before ALTREP Length", {
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("collection exact-state validation invoked ALTREP Length", call. = FALSE)
  }
  trap = function(value) {
    native_stateful_altrep(
      value,
      value,
      callback = callback,
      callback_after = NA_integer_
    )
  }
  arm_length = function(value) {
    native_stateful_altrep_rearm(value, c(NA_integer_, 0L))
    callbacks <<- 0L
    invisible(NULL)
  }

  cases = list(
    sets_shell = function(collection) {
      private = collection_exact_private(collection)
      changed = trap(private$.sets)
      private$.sets = changed
      arm_length(changed)
    },
    set_names = function(collection) {
      private = collection_exact_private(collection)
      sets = private$.sets
      changed = trap(names(sets))
      private$.sets = collection_exact_replace_attribute(
        sets,
        "names",
        changed
      )
      arm_length(changed)
    },
    postfix = function(collection) {
      private = collection_exact_private(collection)
      changed = trap(private$.postfix)
      private$.postfix = changed
      arm_length(changed)
    },
    translation_shell = function(collection) {
      private = collection_exact_private(collection)
      changed = trap(private$.translation)
      private$.translation = changed
      arm_length(changed)
    },
    translation_names = function(collection) {
      private = collection_exact_private(collection)
      table = private$.translation
      changed = trap(names(table))
      private$.translation = collection_exact_replace_attribute(
        table,
        "names",
        changed
      )
      arm_length(changed)
    },
    translation_sorted = function(collection) {
      private = collection_exact_private(collection)
      table = private$.translation
      changed = trap(attr(table, "sorted", exact = TRUE))
      private$.translation = collection_exact_replace_attribute(
        table,
        "sorted",
        changed
      )
      arm_length(changed)
    },
    translation_index = function(collection) {
      private = collection_exact_private(collection)
      table = private$.translation
      changed = trap(attr(table, "index", exact = TRUE))
      private$.translation = collection_exact_replace_attribute(
        table,
        "index",
        changed
      )
      arm_length(changed)
    },
    translation_index_cache = function(collection) {
      private = collection_exact_private(collection)
      table = private$.translation
      index = attr(table, "index", exact = TRUE)
      changed = trap(attr(index, "__original_id", exact = TRUE))
      index = collection_exact_replace_attribute(
        index,
        "__original_id",
        changed
      )
      private$.translation = collection_exact_replace_attribute(
        table,
        "index",
        index
      )
      arm_length(changed)
    },
    params_shell = function(collection) {
      private = collection_exact_private(collection)
      changed = trap(private$.params)
      private$.params = changed
      arm_length(changed)
    },
    params_names = function(collection) {
      private = collection_exact_private(collection)
      table = private$.params
      changed = trap(names(table))
      private$.params = collection_exact_replace_attribute(
        table,
        "names",
        changed
      )
      arm_length(changed)
    },
    params_index = function(collection) {
      private = collection_exact_private(collection)
      table = private$.params
      changed = trap(attr(table, "index", exact = TRUE))
      private$.params = collection_exact_replace_attribute(
        table,
        "index",
        changed
      )
      arm_length(changed)
    },
    params_index_cache = function(collection) {
      private = collection_exact_private(collection)
      table = private$.params
      index = attr(table, "index", exact = TRUE)
      changed = trap(attr(index, "__id__cls__grouping", exact = TRUE))
      index = collection_exact_replace_attribute(
        index,
        "__id__cls__grouping",
        changed
      )
      private$.params = collection_exact_replace_attribute(
        table,
        "index",
        index
      )
      arm_length(changed)
    },
    values_shell = function(collection) {
      child_private = collection$sets[[1L]]$.__enclos_env__$private
      changed = trap(child_private$.values)
      child_private$.values = changed
      arm_length(changed)
    },
    value_names = function(collection) {
      child_private = collection$sets[[1L]]$.__enclos_env__$private
      values = child_private$.values
      changed = trap(names(values))
      child_private$.values = collection_exact_replace_attribute(
        values,
        "names",
        changed
      )
      arm_length(changed)
    }
  )

  for (case_name in names(cases)) {
    collection = collection_exact_fixture()
    cases[[case_name]](collection)
    collection_exact_expect_both_null(collection, case_name)
    expect_identical(callbacks, 0L, info = case_name)
  }

  for (column in names(collection_exact_private(
      collection_exact_fixture()
    )$.translation)) {
    collection = collection_exact_fixture()
    private = collection_exact_private(collection)
    changed = trap(private$.translation[[column]])
    private$.translation = collection_exact_replace_column(
      private$.translation,
      column,
      changed
    )
    arm_length(changed)
    case_name = paste0("translation column `", column, "`")
    collection_exact_expect_both_null(collection, case_name)
    expect_identical(callbacks, 0L, info = case_name)
  }

  for (column in names(collection_exact_private(
      collection_exact_fixture()
    )$.params)) {
    collection = collection_exact_fixture()
    private = collection_exact_private(collection)
    changed = trap(private$.params[[column]])
    private$.params = collection_exact_replace_column(
      private$.params,
      column,
      changed
    )
    arm_length(changed)
    case_name = paste0("params column `", column, "`")
    collection_exact_expect_both_null(collection, case_name)
    expect_identical(callbacks, 0L, info = case_name)
  }
})

test_that("collection values ignores hostile row-name representation", {
  callbacks = 0L
  collection = collection_exact_fixture()
  private = collection_exact_private(collection)
  expected = .Call(collection_exact_values_symbol(), private, collection)
  row_names = native_stateful_altrep(
    c(1L, 2L),
    c(2L, 1L),
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("collection values invoked row-name ALTREP", call. = FALSE)
    },
    callback_after = NA_integer_
  )
  private$.params = collection_exact_replace_attribute(
    private$.params,
    "row.names",
    row_names
  )
  native_stateful_altrep_rearm(row_names, c(NA_integer_, 0L))

  observed = .Call(collection_exact_values_symbol(), private, collection)
  expect_identical(observed, expected)
  expect_identical(callbacks, 0L)
})

test_that("collection values admits only inert invalidated params indices", {
  collection = collection_exact_fixture()
  private = collection_exact_private(collection)
  expected = .Call(collection_exact_values_symbol(), private, collection)

  saved_attributes = attributes(private$.params)
  data.table::set(
    private$.params,
    1L,
    "id",
    private$.params$id[[1L]]
  )
  attributes(private$.params) = saved_attributes
  expect_identical(attr(private$.params, "index", exact = TRUE), integer())
  expect_null(attributes(attr(private$.params, "index", exact = TRUE)))
  expect_identical(
    .Call(collection_exact_values_symbol(), private, collection),
    expected
  )

  malformed = list(
    nonempty = 1L,
    wrong_attribute = structure(integer(), wrong = integer()),
    extra_attribute = structure(
      integer(),
      `__id__cls__grouping` = integer(),
      extra = integer()
    )
  )
  for (case_name in names(malformed)) {
    collection = collection_exact_fixture()
    private = collection_exact_private(collection)
    private$.params = collection_exact_replace_attribute(
      private$.params,
      "index",
      malformed[[case_name]]
    )
    expect_null(
      .Call(collection_exact_values_symbol(), private, collection),
      info = case_name
    )
  }

  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("collection values invoked malformed index ALTREP", call. = FALSE)
  }
  collection = collection_exact_fixture()
  private = collection_exact_private(collection)
  index = native_stateful_altrep(
    integer(),
    1L,
    callback = callback,
    callback_after = NA_integer_
  )
  private$.params = collection_exact_replace_attribute(
    private$.params,
    "index",
    index
  )
  native_stateful_altrep_rearm(index, c(NA_integer_, 0L))
  expect_null(.Call(collection_exact_values_symbol(), private, collection))
  expect_identical(callbacks, 0L)

  collection = collection_exact_fixture()
  private = collection_exact_private(collection)
  cache = native_stateful_altrep(
    integer(),
    1L,
    callback = callback,
    callback_after = NA_integer_
  )
  index = integer()
  index = collection_exact_replace_attribute(
    index,
    "__id__cls__grouping",
    cache
  )
  private$.params = collection_exact_replace_attribute(
    private$.params,
    "index",
    index
  )
  native_stateful_altrep_rearm(cache, c(NA_integer_, 0L))
  expect_null(.Call(collection_exact_values_symbol(), private, collection))
  expect_identical(callbacks, 0L)
})

test_that("deep collection snapshots survive root and graph growth", {
  skip_on_cran()

  marker = new.env(parent = emptyenv())
  marker$value = 42L
  leaves = lapply(seq_len(12L), function(index) {
    result = ps(
      number = p_int(init = index),
      marker = p_uty(init = marker)
    )
    result$values = list(number = index, marker = marker)
    result
  })
  collection = ParamSetCollection$new(setNames(
    leaves,
    sprintf("leaf%02d", seq_along(leaves))
  ))
  for (depth in seq_len(18L)) {
    collection = ParamSetCollection$new(setNames(
      list(collection),
      sprintf("depth%02d", depth)
    ))
  }
  private = collection_exact_private(collection)
  expected_params = .Call(
    collection_exact_params_symbol(),
    private,
    collection
  )
  expected_values = .Call(
    collection_exact_values_symbol(),
    private,
    collection
  )
  expect_false(is.null(expected_params))
  expect_false(is.null(expected_values))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed_params = .Call(
    collection_exact_params_symbol(),
    private,
    collection
  )
  observed_values = .Call(
    collection_exact_values_symbol(),
    private,
    collection
  )
  gctorture(previous)

  expect_identical(observed_params, expected_params)
  expect_identical(observed_values, expected_values)
  expect_true(any(vapply(
    observed_values,
    identical,
    logical(1L),
    marker
  )))
})

test_that("collection scalar check is registered and reached publicly", {
  symbol = collection_exact_check_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 5L)

  collection = psc(component = ps(x = p_int(0, 2)))
  values = list(component.x = 1L)
  expect_error(
    .Call(
      "param_set_collection_check_builtin",
      collection_exact_private(collection),
      collection,
      values,
      FALSE,
      TRUE,
      PACKAGE = "paradox"
    ),
    "not available",
    fixed = TRUE
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])

  namespace = asNamespace("paradox")
  binding = "C_param_set_collection_check_builtin"
  registered = get(binding, envir = namespace, inherits = FALSE)
  unlockBinding(binding, namespace)
  on.exit({
    assign(binding, registered, envir = namespace)
    lockBinding(binding, namespace)
  }, add = TRUE)
  assign(binding, NULL, envir = namespace)
  expect_error(
    collection$check(values),
    "first argument must be a string",
    fixed = TRUE
  )
})

test_that("exact collection scalar checks preserve values and sanitization", {
  child = ps(
    number = p_dbl(-1, 1, tolerance = 1e-6),
    integer = p_int(-2, 2, tolerance = 1e-6),
    flag = p_lgl(),
    factor = p_fct(c("slow", "fast"))
  )
  collection = psc(left = child, right = ps(other = p_int(0, 9)))
  values = list(
    left.factor = "fast",
    right.other = 2L,
    left.number = -1 - 5e-7,
    left.flag = FALSE,
    left.integer = 1.0000005
  )
  observed = collection_exact_check(
    collection,
    values,
    sanitize = TRUE
  )
  expect_identical(observed, collection$check(values, sanitize = TRUE))
  expect_identical(attr(observed, "sanitized"), list(
    left.factor = "fast",
    right.other = 2L,
    left.number = -1,
    left.flag = FALSE,
    left.integer = 1L
  ))

  inner = psc(component = ps(x = p_int(0, 2)))
  nested = psc(left = inner, right = inner)
  nested_values = list(
    left.component.x = 1L,
    right.component.x = 2L
  )
  expect_identical(
    collection_exact_check(nested, nested_values),
    TRUE
  )
  expect_identical(nested$check(nested_values), TRUE)

  evaluation_state = new.env(parent = emptyenv())
  evaluation_state$count = 0L
  expect_identical(collection$check({
    evaluation_state$count = evaluation_state$count + 1L
    list(left.integer = 1L)
  }), TRUE)
  expect_identical(evaluation_state$count, 1L)
})

test_that("strict collection features retain the established fallback", {
  constraint_calls = 0L
  constrained_child = ps(x = p_dbl(0, 1))
  constrained_child$constraint = function(x) {
    constraint_calls <<- constraint_calls + 1L
    x$x <= 0.5
  }
  constrained = psc(component = constrained_child)
  constrained_values = list(component.x = 0.75)
  expect_null(collection_exact_check(constrained, constrained_values))
  expect_identical(constraint_calls, 0L)
  expect_identical(
    constrained$check(constrained_values),
    "Constraint not fulfilled."
  )
  expect_identical(constraint_calls, 1L)
  expect_identical(
    collection_exact_check(
      constrained,
      constrained_values,
      check_strict = FALSE
    ),
    TRUE
  )
  expect_identical(
    constrained$check(constrained_values, check_strict = FALSE),
    TRUE
  )
  expect_identical(constraint_calls, 1L)

  dependent_child = ps(
    parent = p_lgl(),
    child = p_int(0, 2)
  )
  dependent_child$add_dep("child", "parent", CondEqual(TRUE))
  dependent = psc(component = dependent_child)
  dependent_values = list(
    component.parent = FALSE,
    component.child = 1L
  )
  expect_null(collection_exact_check(dependent, dependent_values))
  expect_match(
    dependent$check(dependent_values),
    "component.parent == TRUE",
    fixed = TRUE
  )
  expect_identical(collection_exact_check(
    dependent,
    dependent_values,
    check_strict = FALSE
  ), TRUE)

  cross = psc(
    left = ps(parent = p_lgl()),
    right = ps(child = p_int(0, 2))
  )
  cross$add_dep("right.child", "left.parent", CondEqual(TRUE))
  cross_values = list(left.parent = FALSE, right.child = 1L)
  expect_null(collection_exact_check(cross, cross_values))
  expect_match(
    cross$check(cross_values),
    "left.parent == TRUE",
    fixed = TRUE
  )
})

test_that("unsupported collection checks decline without extension effects", {
  special = psc(component = ps(
    x = p_dbl(0, 1, special_vals = list("AUTO"))
  ))
  expect_null(collection_exact_check(special, list(component.x = 0.5)))
  expect_identical(special$check(list(component.x = 0.5)), TRUE)
  expect_identical(special$check(list(component.x = "AUTO")), TRUE)

  ordinary = psc(component = ps(x = p_int(0, 2)))
  expect_null(collection_exact_check(
    ordinary,
    list(component.x = factor("1"))
  ))
  expect_match(
    ordinary$check(list(component.x = factor("1"))),
    "single integerish value",
    fixed = TRUE
  )
  expect_null(collection_exact_check(ordinary, list(unknown = 1L)))
  expect_match(
    ordinary$check(list(unknown = 1L)),
    "not available",
    fixed = TRUE
  )

  ChildSubclass = R6::R6Class(
    "NativeCollectionCheckChildSubclass",
    inherit = ParamSet
  )
  subclassed = psc(component = ChildSubclass$new(list(x = p_int())))
  subclass_values = list(component.x = 1L)
  expect_null(collection_exact_check(subclassed, subclass_values))
  expect_identical(collection_exact_check(
    subclassed,
    subclass_values,
    check_strict = FALSE
  ), TRUE)
  expect_identical(subclassed$check(subclass_values), TRUE)

  utility = psc(component = ps(payload = p_uty()))
  utility_values = list(
    function() 1,
    new.env(parent = emptyenv()),
    quote(left + right),
    as.name("payload"),
    pairlist(value = 1L),
    new("externalptr")
  )
  for (value in utility_values) {
    input = list(component.payload = value)
    expect_null(collection_exact_check(utility, input))
    expect_identical(utility$check(input), TRUE)
  }
})

test_that("strict collection graph admission is inert and fail closed", {
  collection = psc(component = ps(x = p_int()))
  private = collection_exact_private(collection)
  original = private$.children_with_constraints
  forced = 0L
  unlockBinding(".children_with_constraints", private)
  delayedAssign(
    ".children_with_constraints",
    {
      forced <<- forced + 1L
      original
    },
    assign.env = private
  )
  lockBinding(".children_with_constraints", private)
  on.exit({
    unlockBinding(".children_with_constraints", private)
    assign(".children_with_constraints", original, envir = private)
    lockBinding(".children_with_constraints", private)
  }, add = TRUE)
  expect_null(collection_exact_check(
    collection,
    list(component.x = 1L)
  ))
  expect_identical(forced, 0L)

  cyclic = ParamSetCollection$new(list())
  cyclic_private = collection_exact_private(cyclic)
  cyclic_private$.sets = list(self = cyclic)
  empty_values = structure(list(), names = character())
  expect_null(collection_exact_check(cyclic, empty_values))
  expect_identical(collection_exact_check(
    cyclic,
    empty_values,
    check_strict = FALSE
  ), TRUE)
})

test_that("collection scalar check rejects ALTREP before duplication", {
  callbacks = 0L
  collection = psc(component = ps(x = p_int()))
  private = collection_exact_private(collection)
  changed = native_stateful_altrep(
    private$.params$id,
    private$.params$id,
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("collection check invoked ALTREP", call. = FALSE)
    },
    callback_after = NA_integer_
  )
  private$.params = collection_exact_replace_column(
    private$.params,
    "id",
    changed
  )
  native_stateful_altrep_rearm(changed, c(NA_integer_, 0L))

  expect_null(collection_exact_check(
    collection,
    list(component.x = 1L)
  ))
  expect_identical(callbacks, 0L)
})

test_that("collection check snapshots reject non-vector parameter columns", {
  malformed = list(
    lower = new.env(parent = emptyenv()),
    upper = function() NULL,
    tolerance = quote(left + right),
    levels = list(new.env(parent = emptyenv()))
  )
  for (column in names(malformed)) {
    collection = psc(component = ps(x = p_dbl()))
    private = collection_exact_private(collection)
    private$.params = collection_exact_replace_column(
      private$.params,
      column,
      malformed[[column]]
    )
    expect_null(
      collection_exact_check(collection, list(component.x = 0)),
      info = column
    )
  }
})

test_that("collection check snapshots decline dispatch-sensitive metadata", {
  collection = psc(component = ps(x = p_dbl(0, 2)))
  private = collection_exact_private(collection)
  private$.params = collection_exact_replace_column(
    private$.params,
    "lower",
    structure(private$.params$lower, class = "CollectionCheckAudit")
  )
  method_name = "Ops.CollectionCheckAudit"
  assign(
    method_name,
    function(...) stop("COLLECTION CHECK OPS DISPATCH", call. = FALSE),
    envir = .GlobalEnv
  )
  on.exit(rm(list = method_name, envir = .GlobalEnv), add = TRUE)

  values = list(component.x = 1)
  expect_null(collection_exact_check(collection, values))
  expect_error(
    collection$check(values),
    "COLLECTION CHECK OPS DISPATCH",
    fixed = TRUE
  )

  attributed = list(
    levels = structure(c(TRUE, FALSE), note = "dispatch-sensitive"),
    special_vals = structure(list(), note = "dispatch-sensitive")
  )
  for (column in names(attributed)) {
    candidate = if (column == "levels") {
      psc(component = ps(x = p_lgl()))
    } else {
      psc(component = ps(x = p_dbl()))
    }
    candidate_private = collection_exact_private(candidate)
    candidate_private$.params = collection_exact_replace_column(
      candidate_private$.params,
      column,
      list(attributed[[column]])
    )
    input = list(component.x = if (column == "levels") TRUE else 1)
    expect_null(collection_exact_check(candidate, input), info = column)
  }
})
