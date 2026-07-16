native_design_dependencies_available = function() {
  exists(
    "C_design_dependency_plan_builtin",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_design_dependencies_symbol = function() {
  get(
    "C_design_dependency_plan_builtin",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_design_dependencies_call = function(data, param_set) {
  .Call(native_design_dependencies_symbol(), data, param_set)
}

native_design_dependencies_apply = function(data, plan) {
  stopifnot(
    identical(names(plan), c("rows", "columns", "values")),
    is.list(plan$rows),
    is.character(plan$columns),
    is.list(plan$values),
    length(plan$rows) == length(plan$columns),
    length(plan$rows) == length(plan$values)
  )
  for (edge in seq_along(plan$rows)) {
    data.table::set(
      data,
      i = plan$rows[[edge]],
      j = plan$columns[[edge]],
      value = plan$values[[edge]]
    )
  }
  data
}

# Frozen copy of Design's pre-native private dependency loop. In addition to
# performing the historical by-reference updates, it records exactly the three
# operands passed to each data.table::set() call. This makes the differential
# cover topo/edge order and empty updates, rather than only the final table.
native_design_dependencies_reference = function(param_set, data) {
  ps = param_set
  graph = ps$deps[, 1:2]
  colnames(graph) = c("id", "parents")
  fillin = data.table::data.table(
    id = ps$ids(),
    parents = list(character(0L))
  )
  graph = rbind(
    graph,
    fillin[mlr3misc::`%nin%`(fillin$id, graph$id), ]
  )
  graph = graph[, list(
    parents = list(unlist(get("parents"), use.names = FALSE))
  ), by = "id"]
  topo = mlr3misc::topo_sort(graph)
  pids_sorted = topo$id
  storage_types = ps$storage_type

  rows = list()
  columns = character()
  values = list()
  edge = 0L
  for (param_id in pids_sorted) {
    dd = ps$deps[get("id") == param_id, ]
    for (j in mlr3misc::seq_row(dd)) {
      pcol = data[[dd$on[j]]]
      not_ok = which(is.na(pcol) | !condition_test(dd$cond[[j]], pcol))
      value = paradox:::as_type(NA, storage_types[[param_id]])
      edge = edge + 1L
      rows[[edge]] = not_ok
      columns[[edge]] = param_id
      values[[edge]] = value
      data.table::set(data, not_ok, j = param_id, value = value)
    }
  }
  list(
    data = data,
    plan = list(rows = rows, columns = columns, values = values)
  )
}

native_design_dependencies_simple_set = function() {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(0L, 9L)
  )
  param_set$add_dep("child", "parent", CondEqual(TRUE))
  param_set
}

native_design_dependencies_simple_data = function() {
  data.table::data.table(
    parent = c(TRUE, FALSE),
    child = c(1L, 2L)
  )
}

native_design_dependencies_manual_table = function(columns, nrow) {
  structure(
    columns,
    names = names(columns),
    row.names = .set_row_names(nrow),
    class = c("data.table", "data.frame")
  )
}

native_design_dependencies_replace_method = function(
  param_set,
  name,
  replacement
) {
  unlockBinding(name, param_set)
  assign(name, replacement, envir = param_set)
  lockBinding(name, param_set)
  invisible(param_set)
}

native_design_dependencies_replace_active = function(
  param_set,
  name,
  replacement
) {
  if (bindingIsLocked(name, param_set)) unlockBinding(name, param_set)
  makeActiveBinding(name, replacement, param_set)
  invisible(param_set)
}

native_design_dependencies_capture_error = function(expression) {
  tryCatch(
    {
      force(expression)
      NULL
    },
    error = function(condition) conditionMessage(condition)
  )
}

test_that("native Design dependency routines have forced registered arities", {
  skip_if_not(native_design_dependencies_available())
  symbol = native_design_dependencies_symbol()
  runtime = get(
    "C_design_dependency_runtime",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_s3_class(runtime, "NativeSymbolInfo")
  expect_identical(runtime$numParameters, 1L)
  expect_error(
    .Call("design_dependency_plan_builtin", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native plans retain DAG cascades, topo ties, and edge order", {
  skip_if_not(native_design_dependencies_available())
  param_set = ps(
    root = p_int(0L, 3L),
    gate = p_lgl(),
    left = p_int(0L, 20L),
    right = p_int(0L, 20L),
    empty = p_int(0L, 20L),
    joined = p_dbl(0, 2),
    leaf = p_int(0L, 20L)
  )
  # right and left are a deliberate topo tie in dependency insertion order,
  # which differs from their order in the ParamSet.
  param_set$add_dep("right", "gate", CondEqual(TRUE))
  param_set$add_dep("left", "root", CondAnyOf(c(1L, 3L)))
  param_set$add_dep("empty", "root", CondAnyOf(1:3))
  param_set$add_dep("joined", "left", CondEqual(10L))
  param_set$add_dep("joined", "right", CondEqual(20L))
  param_set$add_dep("leaf", "joined", CondEqual(1))

  input = data.table::data.table(
    root = c(1L, 2L, 3L, 1L),
    gate = c(TRUE, TRUE, FALSE, TRUE),
    left = rep(10L, 4L),
    right = rep(20L, 4L),
    empty = 11:14,
    joined = rep(1, 4L),
    leaf = 15:18
  )
  expected = native_design_dependencies_reference(
    param_set,
    data.table::copy(input)
  )
  actual = data.table::copy(input)
  before = data.table::copy(actual)
  plan = native_design_dependencies_call(actual, param_set)

  expect_false(is.null(plan))
  expect_identical(actual, before)
  expect_identical(plan, expected$plan)
  expect_identical(
    plan$columns,
    c("right", "left", "empty", "joined", "joined", "leaf")
  )
  expect_identical(plan$rows, list(3L, 2L, integer(), 2L, 3L, 2:3))
  expect_identical(
    native_design_dependencies_apply(actual, plan),
    expected$data
  )
  expect_true(all(is.na(actual$leaf[2:3])))
  expect_identical(actual$leaf[c(1L, 4L)], c(15L, 18L))
})

test_that("fixed values are installed before native dependency masking", {
  skip_if_not(native_design_dependencies_available())
  param_set = ps(
    parent = p_lgl(),
    child = p_int(0L, 9L)
  )
  param_set$values = list(parent = FALSE)
  param_set$add_dep("child", "parent", CondEqual(TRUE))
  caller = data.table::data.table(
    parent = rep(TRUE, 3L),
    child = 1:3
  )
  caller_address = data.table::address(caller)

  expected = data.table::copy(caller)
  data.table::set(expected, j = "parent", value = FALSE)
  expected = native_design_dependencies_reference(param_set, expected)$data
  design = Design$new(param_set, caller, remove_dupl = FALSE)

  expect_identical(data.table::address(design$data), caller_address)
  expect_identical(data.table::address(caller), caller_address)
  expect_identical(design$data, expected)
  expect_identical(caller, expected)
  expect_identical(caller$parent, rep(FALSE, 3L))
  expect_true(all(is.na(caller$child)))
})

test_that("typed missing values and by-reference column aliases are retained", {
  skip_if_not(native_design_dependencies_available())
  param_set = ps(
    parent = p_lgl(),
    dbl = p_dbl(),
    int = p_int(0L, 9L),
    fct = p_fct(c("a", "b")),
    lgl = p_lgl()
  )
  for (id in c("dbl", "int", "fct", "lgl")) {
    param_set$add_dep(id, "parent", CondEqual(TRUE))
  }
  actual = data.table::data.table(
    parent = c(FALSE, TRUE),
    dbl = c(1, 2),
    int = c(1L, 2L),
    fct = c("a", "b"),
    lgl = c(TRUE, FALSE)
  )
  held_ids = c("dbl", "int", "fct", "lgl")
  held = setNames(lapply(held_ids, function(id) actual[[id]]), held_ids)
  held_addresses = vapply(held, data.table::address, character(1L))
  plan = native_design_dependencies_call(actual, param_set)

  expect_false(is.null(plan))
  expect_identical(plan$rows, rep(list(1L), 4L))
  expect_identical(
    plan$values,
    list(NA_real_, NA_integer_, NA_character_, NA)
  )
  native_design_dependencies_apply(actual, plan)
  expect_identical(vapply(held, typeof, character(1L)), c(
    dbl = "double", int = "integer", fct = "character", lgl = "logical"
  ))
  expect_identical(
    vapply(held, data.table::address, character(1L)),
    held_addresses
  )
  expect_true(all(vapply(held, function(column) is.na(column[[1L]]), TRUE)))
  expect_identical(
    held,
    setNames(lapply(held_ids, function(id) actual[[id]]), held_ids)
  )
})

test_that("empty edge plans still perform the historical set update", {
  skip_if_not(native_design_dependencies_available())
  param_set = native_design_dependencies_simple_set()
  data = data.table::data.table(
    parent = rep(TRUE, 3L),
    child = 1:3
  )
  plan = native_design_dependencies_call(data, param_set)

  expect_false(is.null(plan))
  expect_identical(plan$rows, list(integer()))
  native_design_dependencies_apply(data, plan)
  expect_identical(data.table::.Last.updated, 0L)
  expect_identical(data$child, 1:3)
})

test_that("numeric dependency comparisons retain R coercion and missing rules", {
  skip_if_not(native_design_dependencies_available())
  real_set = ps(
    parent = p_dbl(),
    equal_integer = p_int(0L, 100L),
    membership = p_int(0L, 100L),
    infinity = p_int(0L, 100L),
    zero = p_int(0L, 100L)
  )
  real_set$add_dep("equal_integer", "parent", CondEqual(2L))
  real_set$add_dep("membership", "parent", CondAnyOf(c(Inf, 0)))
  real_set$add_dep("infinity", "parent", CondEqual(Inf))
  real_set$add_dep("zero", "parent", CondEqual(0))
  real_data = data.table::data.table(
    parent = c(2, NaN, Inf, -0, NA_real_, -Inf),
    equal_integer = 1:6,
    membership = 11:16,
    infinity = 21:26,
    zero = 31:36
  )
  real_expected = native_design_dependencies_reference(
    real_set,
    data.table::copy(real_data)
  )
  real_plan = native_design_dependencies_call(real_data, real_set)

  expect_false(is.null(real_plan))
  expect_identical(real_plan, real_expected$plan)
  expect_identical(real_plan$rows, list(
    2:6,
    c(1L, 2L, 5L, 6L),
    c(1L, 2L, 4L, 5L, 6L),
    c(1L, 2L, 3L, 5L, 6L)
  ))
  expect_identical(
    native_design_dependencies_apply(real_data, real_plan),
    real_expected$data
  )

  integer_set = ps(
    parent = p_int(0L, 3L),
    any_logical = p_int(0L, 100L),
    equal_double = p_int(0L, 100L)
  )
  integer_set$deps = data.table::data.table(
    id = c("any_logical", "equal_double"),
    on = c("parent", "parent"),
    cond = list(CondAnyOf(c(FALSE, TRUE)), CondEqual(2))
  )
  integer_data = data.table::data.table(
    parent = c(0L, 1L, 2L, NA_integer_),
    any_logical = 1:4,
    equal_double = 5:8
  )
  integer_expected = native_design_dependencies_reference(
    integer_set,
    data.table::copy(integer_data)
  )
  integer_plan = native_design_dependencies_call(integer_data, integer_set)

  expect_false(is.null(integer_plan))
  expect_identical(integer_plan, integer_expected$plan)
  expect_identical(integer_plan$rows, list(3:4, c(1L, 2L, 4L)))
  expect_identical(
    native_design_dependencies_apply(integer_data, integer_plan),
    integer_expected$data
  )
})

test_that("character conditions retain encoding equality and bytes fallback", {
  skip_if_not(native_design_dependencies_available())
  utf8 = enc2utf8("caf\u00e9")
  other_utf8 = enc2utf8("na\u00efve")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(is.na(latin1))
  Encoding(latin1) = "latin1"

  param_set = ps(
    parent = p_fct(c(utf8, other_utf8)),
    child = p_int(0L, 9L)
  )
  param_set$add_dep("child", "parent", CondEqual(utf8))
  encoded = data.table::data.table(
    parent = c(utf8, other_utf8),
    child = c(1L, 2L)
  )
  expected = native_design_dependencies_reference(
    param_set,
    data.table::copy(encoded)
  )
  plan = native_design_dependencies_call(encoded, param_set)

  expect_false(is.null(plan))
  expect_identical(plan, expected$plan)
  expect_identical(plan$rows, list(2L))
  expect_identical(
    native_design_dependencies_apply(encoded, plan),
    expected$data
  )

  mixed = data.table::data.table(
    parent = c(latin1, other_utf8),
    child = c(1L, 2L)
  )
  mixed_before = data.table::copy(mixed)
  expect_null(native_design_dependencies_call(mixed, param_set))
  expect_identical(mixed, mixed_before)
  mixed_expected = native_design_dependencies_reference(
    param_set,
    data.table::copy(mixed)
  )$data
  mixed_design = Design$new(param_set, mixed, remove_dupl = FALSE)
  expect_identical(mixed_design$data, mixed_expected)

  bytes = "caf\u00e9"
  Encoding(bytes) = "bytes"
  bytes_other = "na\u00efve"
  Encoding(bytes_other) = "bytes"
  byte_set = ps(
    parent = p_fct(c(utf8, other_utf8)),
    child = p_int(0L, 9L)
  )
  # The public dependency setter intentionally performs only its documented
  # structural checks.  Use it to retain a bytes-marked historical condition
  # without asking ParamFct's level sorter to translate that string first.
  byte_set$deps = data.table::data.table(
    id = "child",
    on = "parent",
    cond = list(CondEqual(bytes))
  )
  byte_data = data.table::data.table(
    parent = c(bytes, bytes_other),
    child = c(1L, 2L)
  )
  before = data.table::copy(byte_data)

  expect_null(native_design_dependencies_call(byte_data, byte_set))
  expect_identical(byte_data, before)
  byte_expected = native_design_dependencies_reference(
    byte_set,
    data.table::copy(byte_data)
  )$data
  byte_design = Design$new(byte_set, byte_data, remove_dupl = FALSE)
  expect_identical(byte_design$data, byte_expected)

  # A missing CHARSXP must never enter the direct comparator as if it were the
  # literal string "NA". This malformed operand belongs to the R fallback.
  missing_rhs = CondEqual("placeholder")
  missing_rhs$rhs = NA_character_
  missing_set = ps(
    parent = p_fct(c("NA", "other")),
    child = p_int(0L, 9L)
  )
  missing_set$deps = data.table::data.table(
    id = "child",
    on = "parent",
    cond = list(missing_rhs)
  )
  missing_data = data.table::data.table(
    parent = c("NA", NA_character_),
    child = c(1L, 2L)
  )
  missing_before = data.table::copy(missing_data)
  expect_null(native_design_dependencies_call(missing_data, missing_set))
  expect_identical(missing_data, missing_before)
})

test_that("cycles and dangling dependency nodes decline to historical errors", {
  skip_if_not(native_design_dependencies_available())

  cyclic = ps(a = p_lgl(), b = p_lgl())
  cyclic$add_dep("a", "b", CondEqual(TRUE))
  cyclic$add_dep("b", "a", CondEqual(TRUE))
  cyclic_data = data.table::data.table(
    a = c(TRUE, FALSE),
    b = c(TRUE, FALSE)
  )
  cyclic_before = data.table::copy(cyclic_data)
  expect_null(native_design_dependencies_call(cyclic_data, cyclic))
  expect_identical(cyclic_data, cyclic_before)
  reference_cycle = native_design_dependencies_capture_error(
    native_design_dependencies_reference(
      cyclic,
      data.table::copy(cyclic_data)
    )
  )
  design_cycle = native_design_dependencies_capture_error(
    Design$new(cyclic, cyclic_data, remove_dupl = FALSE)
  )
  expect_identical(design_cycle, reference_cycle)
  expect_identical(design_cycle, "Cycle detected, this is not a DAG!")
  expect_identical(cyclic_data, cyclic_before)

  dangling_parent = ps(a = p_lgl(), b = p_lgl())
  dangling_parent$deps = data.table::data.table(
    id = "b",
    on = "missing",
    cond = list(CondEqual(TRUE))
  )
  dangling_data = data.table::data.table(
    a = c(TRUE, FALSE),
    b = c(TRUE, FALSE)
  )
  dangling_before = data.table::copy(dangling_data)
  expect_null(native_design_dependencies_call(
    dangling_data,
    dangling_parent
  ))
  expect_identical(dangling_data, dangling_before)
  reference_dangling = native_design_dependencies_capture_error(
    native_design_dependencies_reference(
      dangling_parent,
      data.table::copy(dangling_data)
    )
  )
  design_dangling = native_design_dependencies_capture_error(
    Design$new(dangling_parent, dangling_data, remove_dupl = FALSE)
  )
  expect_identical(design_dangling, reference_dangling)
  expect_false(is.null(design_dangling))
  expect_identical(dangling_data, dangling_before)

  dangling_id = ps(a = p_lgl(), b = p_lgl())
  dangling_id$.__enclos_env__$private$.deps = data.table::data.table(
    id = "foreign",
    on = "a",
    cond = list(CondEqual(TRUE))
  )
  id_data = data.table::data.table(
    a = c(FALSE, TRUE),
    b = c(TRUE, TRUE)
  )
  id_before = data.table::copy(id_data)
  expect_null(native_design_dependencies_call(id_data, dangling_id))
  expect_identical(id_data, id_before)
  reference_id = native_design_dependencies_capture_error(
    native_design_dependencies_reference(
      dangling_id,
      data.table::copy(id_data)
    )
  )
  design_id = native_design_dependencies_capture_error(
    Design$new(dangling_id, id_data, remove_dupl = FALSE)
  )
  expect_identical(design_id, reference_id)
  expect_false(is.null(design_id))
})

test_that("custom conditions fall back once and retain partial mutation order", {
  skip_if_not(native_design_dependencies_available())
  calls = 0L
  class_name = "NativeDesignDependencyErrorCondition"
  registerS3method(
    "condition_test",
    class_name,
    function(cond, x) {
      calls <<- calls + 1L
      stop("custom Design dependency exploded", call. = FALSE)
    },
    envir = asNamespace("paradox")
  )
  custom = structure(
    list(rhs = TRUE, condition_format_string = "%s custom %s"),
    class = c(class_name, "Condition")
  )
  param_set = ps(
    root = p_lgl(),
    first = p_int(0L, 9L),
    second = p_int(0L, 9L)
  )
  param_set$deps = data.table::data.table(
    id = c("first", "second"),
    on = c("root", "root"),
    cond = list(CondEqual(TRUE), custom)
  )
  data = data.table::data.table(
    root = c(FALSE, TRUE),
    first = c(1L, 2L),
    second = c(3L, 4L)
  )
  before = data.table::copy(data)

  expect_null(native_design_dependencies_call(data, param_set))
  expect_identical(calls, 0L)
  expect_identical(data, before)
  expect_error(
    Design$new(param_set, data, remove_dupl = FALSE),
    "custom Design dependency exploded",
    fixed = TRUE
  )
  expect_identical(calls, 1L)
  expect_identical(data$first, c(NA_integer_, 2L))
  expect_identical(data$second, c(3L, 4L))
})

test_that("replaced ids, deps, and storage surfaces decline without forcing", {
  skip_if_not(native_design_dependencies_available())

  ids_calls = 0L
  ids_set = native_design_dependencies_simple_set()
  inherited_ids = ids_set$ids
  native_design_dependencies_replace_method(
    ids_set,
    "ids",
    function(class = NULL, tags = NULL, any_tags = NULL) {
      ids_calls <<- ids_calls + 1L
      inherited_ids(class = class, tags = tags, any_tags = any_tags)
    }
  )
  ids_data = native_design_dependencies_simple_data()
  expect_null(native_design_dependencies_call(ids_data, ids_set))
  expect_identical(ids_calls, 0L)
  ids_design = Design$new(ids_set, ids_data, remove_dupl = FALSE)
  expect_gt(ids_calls, 0L)
  expect_identical(ids_design$data$child, c(1L, NA_integer_))

  deps_calls = 0L
  deps_set = native_design_dependencies_simple_set()
  stored_deps = deps_set$deps
  native_design_dependencies_replace_active(
    deps_set,
    "deps",
    function(value) {
      if (!missing(value)) stop("deps is read-only in this fixture")
      deps_calls <<- deps_calls + 1L
      stored_deps
    }
  )
  deps_data = native_design_dependencies_simple_data()
  expect_null(native_design_dependencies_call(deps_data, deps_set))
  expect_identical(deps_calls, 0L)
  deps_design = Design$new(deps_set, deps_data, remove_dupl = FALSE)
  expect_gt(deps_calls, 0L)
  expect_identical(deps_design$data$child, c(1L, NA_integer_))

  storage_calls = 0L
  storage_set = native_design_dependencies_simple_set()
  stored_types = storage_set$storage_type
  native_design_dependencies_replace_active(
    storage_set,
    "storage_type",
    function(value) {
      if (!missing(value)) stop("storage_type is read-only in this fixture")
      storage_calls <<- storage_calls + 1L
      stored_types
    }
  )
  storage_data = native_design_dependencies_simple_data()
  expect_null(native_design_dependencies_call(storage_data, storage_set))
  expect_identical(storage_calls, 0L)
  storage_design = Design$new(
    storage_set,
    storage_data,
    remove_dupl = FALSE
  )
  expect_identical(storage_calls, 1L)
  expect_identical(storage_design$data$child, c(1L, NA_integer_))
})

test_that("ParamSet subclasses retain the complete R dependency fallback", {
  skip_if_not(native_design_dependencies_available())
  Subclass = R6::R6Class(
    "NativeDesignDependencyParamSetSubclass",
    inherit = ParamSet
  )
  param_set = Subclass$new(list(
    parent = p_lgl(),
    child = p_int(0L, 9L)
  ))
  param_set$add_dep("child", "parent", CondEqual(TRUE))
  data = native_design_dependencies_simple_data()
  before = data.table::copy(data)

  expect_null(native_design_dependencies_call(data, param_set))
  expect_identical(data, before)
  expected = native_design_dependencies_reference(
    param_set,
    data.table::copy(data)
  )$data
  design = Design$new(param_set, data, remove_dupl = FALSE)
  expect_identical(design$data, expected)
})

test_that("replaced built-in condition dispatch declines before invocation", {
  skip_if_not(native_design_dependencies_available())
  namespace = asNamespace("paradox")
  name = "condition_test.CondEqual"
  original = get(name, envir = namespace, inherits = FALSE)
  was_locked = bindingIsLocked(name, namespace)
  restore = function() {
    if (bindingIsLocked(name, namespace)) unlockBinding(name, namespace)
    assign(name, original, envir = namespace)
    if (was_locked) lockBinding(name, namespace)
  }
  on.exit(restore(), add = TRUE)

  calls = 0L
  if (bindingIsLocked(name, namespace)) unlockBinding(name, namespace)
  assign(name, function(cond, x) {
    calls <<- calls + 1L
    rep(TRUE, length(x))
  }, envir = namespace)
  lockBinding(name, namespace)

  param_set = native_design_dependencies_simple_set()
  data = native_design_dependencies_simple_data()
  before = data.table::copy(data)
  expect_null(native_design_dependencies_call(data, param_set))
  expect_identical(calls, 0L)
  expect_identical(data, before)

  design = Design$new(param_set, data, remove_dupl = FALSE)
  expect_identical(calls, 1L)
  expect_identical(design$data$child, c(1L, 2L))
})

test_that("registered built-in methods retain namespace fallback dispatch", {
  skip_if_not(native_design_dependencies_available())
  namespace = asNamespace("paradox")

  for (case in list(
    list(class = "CondEqual", condition = CondEqual(TRUE)),
    list(class = "CondAnyOf", condition = CondAnyOf(TRUE))
  )) {
    original = getS3method(
      "condition_test",
      case$class,
      optional = FALSE,
      envir = namespace
    )
    calls = 0L
    replacement = function(cond, x) {
      calls <<- calls + 1L
      rep(FALSE, length(x))
    }
    registerS3method(
      "condition_test",
      case$class,
      replacement,
      envir = namespace
    )

    tryCatch(
      {
        param_set = ps(
          parent = p_lgl(),
          child = p_int(0L, 9L)
        )
        param_set$add_dep("child", "parent", case$condition)
        data = native_design_dependencies_simple_data()
        before = data.table::copy(data)

        expect_null(native_design_dependencies_call(data, param_set))
        expect_identical(calls, 0L)
        expect_identical(data, before)

        design = Design$new(param_set, data, remove_dupl = FALSE)
        # Calls originating inside the namespace still find its canonical
        # method binding before this registered table entry.  The historical
        # fallback therefore retains canonical dispatch as well.
        expect_identical(calls, 0L)
        expect_identical(
          design$data$child,
          c(1L, NA_integer_)
        )
      },
      finally = registerS3method(
        "condition_test",
        case$class,
        original,
        envir = namespace
      )
    )
  }
})

test_that("a same-environment replacement as_type closure declines exactly", {
  skip_if_not(native_design_dependencies_available())
  namespace = asNamespace("paradox")
  name = "as_type"
  original = get(name, envir = namespace, inherits = FALSE)
  was_locked = bindingIsLocked(name, namespace)
  restore = function() {
    if (bindingIsLocked(name, namespace)) unlockBinding(name, namespace)
    assign(name, original, envir = namespace)
    if (was_locked) lockBinding(name, namespace)
  }
  on.exit(restore(), add = TRUE)

  replacement = function(x, type) {
    stop("replacement as_type ran", call. = FALSE)
  }
  environment(replacement) = namespace
  if (bindingIsLocked(name, namespace)) unlockBinding(name, namespace)
  assign(name, replacement, envir = namespace)
  lockBinding(name, namespace)

  param_set = native_design_dependencies_simple_set()
  data = native_design_dependencies_simple_data()
  before = data.table::copy(data)
  expect_null(native_design_dependencies_call(data, param_set))
  expect_identical(data, before)
  expect_error(
    Design$new(param_set, data, remove_dupl = FALSE),
    "replacement as_type ran",
    fixed = TRUE
  )
  expect_identical(data, before)
})

test_that("duplicate data columns decline because historical set observes aliases", {
  skip_if_not(native_design_dependencies_available())
  param_set = ps(
    parent = p_lgl(),
    child = p_lgl(),
    leaf = p_int(0L, 99L)
  )
  param_set$add_dep("child", "parent", CondEqual(FALSE))
  param_set$add_dep("leaf", "parent", CondEqual(TRUE))
  make_data = function() {
    shared = c(TRUE, FALSE)
    native_design_dependencies_manual_table(
      list(parent = shared, child = shared, leaf = c(11L, 12L)),
      2L
    )
  }
  data = make_data()
  before = make_data()
  expect_identical(
    data.table::address(data$parent),
    data.table::address(data$child)
  )

  expect_null(native_design_dependencies_call(data, param_set))
  expect_identical(data, before)
  expected = native_design_dependencies_reference(param_set, make_data())$data
  design = Design$new(param_set, data, remove_dupl = FALSE)

  expect_identical(design$data, expected)
  expect_true(is.na(data$parent[[1L]]))
  expect_true(all(is.na(data$leaf)))
})

test_that("data columns aliased to planner inputs retain historical set order", {
  skip_if_not(native_design_dependencies_available())
  make_case = function() {
    param_set = ps(
      parent = p_lgl(),
      first = p_fct(c("first", "second")),
      second = p_int(0L, 9L)
    )
    param_set$add_dep("first", "parent", CondEqual(TRUE))
    param_set$add_dep("second", "parent", CondEqual(TRUE))

    # setDT installs an ordinary data.table shell without copying its column
    # vectors.  Updating row two of `first` therefore also removes the second
    # dependency id before the historical loop looks that dependency up.
    shared_ids = param_set$.__enclos_env__$private$.deps$id
    data = list(
      parent = c(TRUE, FALSE),
      first = shared_ids,
      second = c(1L, 2L)
    )
    data.table::setDT(data)
    stopifnot(identical(
      data.table::address(data$first),
      data.table::address(param_set$.__enclos_env__$private$.deps$id)
    ))
    list(param_set = param_set, data = data)
  }

  reference = make_case()
  expected = native_design_dependencies_reference(
    reference$param_set,
    reference$data
  )
  expected_dependency_ids = reference$param_set$deps$id[]

  actual = make_case()
  data_before = data.table::copy(actual$data)
  dependency_ids_before = actual$param_set$deps$id[]
  expect_null(native_design_dependencies_call(
    actual$data,
    actual$param_set
  ))
  expect_identical(actual$data, data_before)
  expect_identical(actual$param_set$deps$id, dependency_ids_before)

  design = Design$new(
    actual$param_set,
    actual$data,
    remove_dupl = FALSE
  )
  expect_identical(design$data, expected$data)
  expect_identical(actual$param_set$deps$id, expected_dependency_ids)
  expect_identical(design$data$first, c("first", NA_character_))
  expect_identical(design$data$second, c(1L, 2L))
  expect_identical(actual$param_set$deps$id, c("first", NA_character_))

  make_condition_case = function() {
    later_condition = CondEqual(TRUE)
    param_set = ps(
      gate = p_lgl(),
      first = p_fct(c("CondEqual", "Condition")),
      second = p_int(0L, 9L)
    )
    param_set$deps = data.table::data.table(
      id = c("first", "second"),
      on = c("gate", "gate"),
      cond = list(CondEqual(TRUE), later_condition)
    )
    stored_later = param_set$.__enclos_env__$private$.deps$cond[[2L]]
    shared_class = attr(stored_later, "class", exact = TRUE)
    data = list(
      gate = c(FALSE, TRUE),
      first = shared_class,
      second = c(1L, 2L)
    )
    data.table::setDT(data)
    stopifnot(identical(
      data.table::address(data$first),
      data.table::address(attr(stored_later, "class", exact = TRUE))
    ))
    list(
      param_set = param_set,
      data = data,
      later_condition = stored_later
    )
  }

  condition_reference = make_condition_case()
  reference_error = native_design_dependencies_capture_error(
    native_design_dependencies_reference(
      condition_reference$param_set,
      condition_reference$data
    )
  )
  expect_false(is.null(reference_error))

  condition_actual = make_condition_case()
  condition_data_before = data.table::copy(condition_actual$data)
  condition_class_before = attr(
    condition_actual$later_condition,
    "class",
    exact = TRUE
  )[]
  expect_null(native_design_dependencies_call(
    condition_actual$data,
    condition_actual$param_set
  ))
  expect_identical(condition_actual$data, condition_data_before)
  expect_identical(
    attr(condition_actual$later_condition, "class", exact = TRUE),
    condition_class_before
  )

  actual_error = native_design_dependencies_capture_error(
    Design$new(
      condition_actual$param_set,
      condition_actual$data,
      remove_dupl = FALSE
    )
  )
  expect_identical(actual_error, reference_error)
  expect_identical(condition_actual$data, condition_reference$data)
  expect_identical(
    attr(condition_actual$later_condition, "class", exact = TRUE),
    attr(condition_reference$later_condition, "class", exact = TRUE)
  )
  expect_true(is.na(condition_actual$data$first[[1L]]))
  expect_true(is.na(attr(
    condition_actual$later_condition,
    "class",
    exact = TRUE
  )[[1L]]))
  expect_identical(condition_actual$data$second, c(1L, 2L))
})

test_that("dependency mappings are revalidated across GC finalizers", {
  skip_if_not(native_design_dependencies_available())
  param_set = ps(
    left = p_lgl(),
    right = p_lgl(),
    child = p_int(0L, 9L)
  )
  param_set$add_dep("child", "left", CondEqual(TRUE))
  private = param_set$.__enclos_env__$private
  data = data.table::data.table(
    left = c(TRUE, FALSE),
    right = c(FALSE, TRUE),
    child = c(1L, 2L)
  )
  before = data.table::copy(data)

  planner = native_design_dependencies_symbol()
  mutator = get(
    "C_test_gc_column_mutator",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  arm_mutator = function() {
    invisible(.Call(mutator, private$.deps, 1L, "right"))
    invisible(NULL)
  }
  arm_mutator()
  invisible(gc(FALSE))
  expect_identical(private$.deps$on, "right")

  previous = gctorture2(1L, wait = 0L)
  on.exit(gctorture2(previous), add = TRUE)
  plan = .Call(planner, data, param_set)
  gctorture2(previous)

  expect_identical(data, before)
  post_gc = native_design_dependencies_reference(
    param_set,
    data.table::copy(data)
  )$plan
  expect_false(is.null(plan))
  expect_identical(plan, post_gc)
  expect_identical(plan$rows, list(1L))
})

test_that("data-column and condition-RHS aliases decline without mutation", {
  skip_if_not(native_design_dependencies_available())
  rhs = c("a", "b")
  condition = CondAnyOf(rhs)
  param_set = ps(
    parent = p_fct(c("a", "x")),
    child = p_fct(c("a", "b"))
  )
  param_set$deps = data.table::data.table(
    id = "child",
    on = "parent",
    cond = list(condition)
  )
  stored_condition = param_set$.__enclos_env__$private$.deps$cond[[1L]]
  data = native_design_dependencies_manual_table(
    list(parent = c("x", "a"), child = stored_condition$rhs),
    2L
  )
  expect_identical(
    data.table::address(data$child),
    data.table::address(stored_condition$rhs)
  )
  before = lapply(data, identity)
  rhs_before = stored_condition$rhs[]

  expect_null(native_design_dependencies_call(data, param_set))
  expect_identical(lapply(data, identity), before)
  expect_identical(stored_condition$rhs, rhs_before)
  design = Design$new(param_set, data, remove_dupl = FALSE)

  expect_true(is.na(design$data$child[[1L]]))
  expect_true(is.na(stored_condition$rhs[[1L]]))
  expect_identical(stored_condition$rhs[[2L]], "b")
})

test_that("classed, ALTREP, type-mismatched, and malformed data decline", {
  skip_if_not(native_design_dependencies_available())
  param_set = native_design_dependencies_simple_set()

  classed = data.table::data.table(
    parent = structure(c(TRUE, FALSE), class = "NativeDesignParent"),
    child = c(1L, 2L)
  )
  classed_before = data.table::copy(classed)
  expect_null(native_design_dependencies_call(classed, param_set))
  expect_identical(classed, classed_before)

  mismatch = data.table::data.table(
    parent = c(TRUE, FALSE),
    child = c(1, 2)
  )
  mismatch_before = data.table::copy(mismatch)
  expect_null(native_design_dependencies_call(mismatch, param_set))
  expect_identical(mismatch, mismatch_before)

  malformed = native_design_dependencies_manual_table(
    list(parent = c(TRUE, FALSE), child = 1L),
    2L
  )
  malformed_before = lapply(malformed, identity)
  expect_null(native_design_dependencies_call(malformed, param_set))
  expect_identical(lapply(malformed, identity), malformed_before)

  duplicate_names = native_design_dependencies_manual_table(
    setNames(list(c(TRUE, FALSE), c(1L, 2L)), c("parent", "parent")),
    2L
  )
  duplicate_before = lapply(duplicate_names, identity)
  expect_null(native_design_dependencies_call(duplicate_names, param_set))
  expect_identical(lapply(duplicate_names, identity), duplicate_before)

  callbacks = 0L
  altrep_parent = native_stateful_altrep(
    c(TRUE, FALSE),
    c(FALSE, TRUE),
    elt_switch_after = 0L,
    length_switch_after = 0L,
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = 0L
  )
  altrep = native_design_dependencies_manual_table(
    list(parent = altrep_parent, child = c(1L, 2L)),
    2L
  )
  callbacks = 0L
  expect_null(native_design_dependencies_call(altrep, param_set))
  expect_identical(callbacks, 0L)
})

test_that("R set application preserves key and secondary-index semantics", {
  skip_if_not(native_design_dependencies_available())
  param_set = native_design_dependencies_simple_set()

  keyed = data.table::data.table(
    parent = c(TRUE, FALSE, TRUE, FALSE),
    child = c(4L, 3L, 2L, 1L)
  )
  data.table::setkeyv(keyed, c("parent", "child"))
  keyed_before = data.table::copy(keyed)
  keyed_key = data.table::key(keyed)
  keyed_plan = native_design_dependencies_call(keyed, param_set)
  expect_false(is.null(keyed_plan))
  expect_identical(keyed, keyed_before)
  expect_identical(data.table::key(keyed), keyed_key)

  keyed_expected = native_design_dependencies_reference(
    param_set,
    data.table::copy(keyed)
  )$data
  native_design_dependencies_apply(keyed, keyed_plan)
  expect_identical(keyed, keyed_expected)
  expect_identical(data.table::key(keyed), data.table::key(keyed_expected))
  expect_identical(
    data.table::indices(keyed),
    data.table::indices(keyed_expected)
  )

  indexed = data.table::data.table(
    parent = c(TRUE, FALSE, TRUE, FALSE),
    child = c(1L, 2L, 3L, 4L)
  )
  data.table::setindexv(indexed, "parent")
  data.table::setindexv(indexed, "child")
  indexed_before = data.table::copy(indexed)
  indexed_indices = data.table::indices(indexed)
  indexed_plan = native_design_dependencies_call(indexed, param_set)
  expect_false(is.null(indexed_plan))
  expect_identical(indexed, indexed_before)
  expect_identical(data.table::indices(indexed), indexed_indices)

  indexed_expected = native_design_dependencies_reference(
    param_set,
    data.table::copy(indexed)
  )$data
  native_design_dependencies_apply(indexed, indexed_plan)
  expect_identical(indexed, indexed_expected)
  expect_identical(
    data.table::indices(indexed),
    data.table::indices(indexed_expected)
  )
})
