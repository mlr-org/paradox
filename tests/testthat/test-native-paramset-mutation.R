mutation2_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"), inherits = FALSE)
}

test_that("ParamSet mutators have fixed registered native interfaces", {
  expected = c(
    param_set_set_tags = 3L,
    param_set_get_tags = 2L,
    param_set_dependency_table_snapshot = 1L,
    param_set_dependencies = 2L,
    param_set_has_dependencies = 2L,
    param_set_set_dependencies = 3L,
    param_set_add_dependency = 6L,
    param_set_set_callback = 4L
  )
  for (name in names(expected)) {
    symbol = mutation2_symbol(name)
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, expected[[name]])
  }
})

test_that("has_deps is one native scalar read across every ParamSet kind", {
  symbol = mutation2_symbol("param_set_has_dependencies")
  base = ps(parent = p_int(0L, 2L), child = p_lgl())
  expect_identical(.Call(symbol, base$.__enclos_env__$private, base), FALSE)
  expect_identical(base$has_deps, FALSE)

  collection = ParamSetCollection$new(list(left = base, right = base))
  expect_identical(collection$has_deps, FALSE)

  origin = ps(
    hidden = p_int(),
    parent = p_int(0L, 2L),
    child = p_lgl()
  )
  shadow = ParamSetShadow$new(origin, "hidden")
  expect_identical(shadow$has_deps, FALSE)

  collection_origin = ParamSetCollection$new(list(owner = origin))
  collection_shadow = ParamSetShadow$new(collection_origin, character())
  expect_identical(collection_shadow$has_deps, FALSE)

  base$add_dep("child", "parent", CondEqual(1L))
  origin$add_dep("child", "parent", CondEqual(1L))
  expect_identical(base$has_deps, TRUE)
  expect_identical(collection$has_deps, TRUE)
  expect_identical(shadow$has_deps, TRUE)
  expect_identical(collection_shadow$has_deps, TRUE)
})

test_that("has_deps fails closed on malformed dependency graphs", {
  forge_dependencies = function(set, replacement) {
    .Call(
      paradox:::C_param_set_core_replace,
      set$.__enclos_env__$private,
      setNames(list(replacement), ".deps")
    )
    invisible(set)
  }

  base = ps(parent = p_int(), child = p_lgl())
  forge_dependencies(base, list(id = "child"))
  expect_error(base$has_deps, "Corrupt ParamSet dependency capsule")

  bytes_base = ps(parent = p_int(), child = p_lgl())
  bytes_base$add_dep("child", "parent", CondEqual(1L))
  bytes_dependencies = paradox:::param_set_core_state(
    bytes_base$.__enclos_env__$private
  )$.deps
  bytes_id = rawToChar(as.raw(255L))
  Encoding(bytes_id) = "bytes"
  expect_identical(Encoding(bytes_id), "bytes")
  bytes_dependencies$id[[1L]] = bytes_id
  forge_dependencies(bytes_base, bytes_dependencies)
  expect_true(bytes_base$has_deps)

  origin = ps(
    hidden = p_int(),
    parent = p_int(),
    child = p_lgl()
  )
  shadow = ParamSetShadow$new(origin, "hidden")
  forge_dependencies(origin, list(id = "child"))
  expect_error(shadow$has_deps, "Corrupt ParamSetShadow|Corrupt ParamSet")

  child = ps(parent = p_int(), child = p_lgl())
  collection = ParamSetCollection$new(list(owner = child))
  forge_dependencies(child, list(id = "child"))
  expect_error(collection$has_deps, "Corrupt ParamSetCollection")

  bytes_child = ps(parent = p_int(), child = p_lgl())
  bytes_child$add_dep("child", "parent", CondEqual(1L))
  bytes_collection = ParamSetCollection$new(list(owner = bytes_child))
  bytes_dependencies = paradox:::param_set_core_state(
    bytes_child$.__enclos_env__$private
  )$.deps
  bytes_dependencies$id[[1L]] = bytes_id
  forge_dependencies(bytes_child, bytes_dependencies)
  expect_true(bytes_collection$has_deps)

  cycle = ParamSetCollection$new(list())
  paradox:::param_set_core_replace(
    cycle$.__enclos_env__$private,
    sets = list(self = cycle)
  )
  expect_error(cycle$has_deps, "cycle", ignore.case = TRUE)
})

test_that("tags are admitted once and stored as an owned canonical table", {
  set = ps(first = p_int(), second = p_lgl())
  supplied = structure(
    list(second = c("switch", "required"), first = "numeric"),
    class = c("tag_configuration", "list")
  )
  set$tags = supplied

  expect_identical(
    set$tags,
    list(first = "numeric", second = c("switch", "required"))
  )
  state = paradox:::param_set_core_state(set$.__enclos_env__$private)
  expect_identical(class(state$.tags), "data.frame")
  expect_null(attr(state$.tags, ".internal.selfref", exact = TRUE))

  observed = set$tags
  observed$first[[1L]] = "changed"
  expect_identical(set$tags$first, "numeric")

  set$tags = list()
  expect_identical(
    set$tags,
    list(first = character(), second = character())
  )
  expect_error(
    { set$tags = list(first = "only") },
    "name every parameter|permutation"
  )
  expect_error(
    { set$tags = list(first = NA_character_, second = character()) },
    "missing"
  )
  # The permutation contract names its offender: an unknown name gets the
  # standard unavailable-parameter diagnostic, a repeated one is named too.
  expect_error(
    { set$tags = list(first = character(), sceond = character()) },
    "Parameter 'sceond' not available. Did you mean 'second'?",
    fixed = TRUE
  )
  expect_error(
    { set$tags = setNames(list(character(), character()), c("first", "first")) },
    "'first' appears more than once"
  )
  expect_error(
    { set$tags = setNames(list(character(), character()), c("first", NA)) },
    "`tags` names may not be missing or bytes-encoded",
    fixed = TRUE
  )
})

test_that("dependency setters snapshot structure while add_dep checks feasibility", {
  set = ps(parent = p_int(0L, 2L), child = p_lgl())
  condition = CondAnyOf(c(1L, 2L))
  set$deps = data.frame(
    id = "child",
    on = "parent",
    cond = I(list(condition)),
    stringsAsFactors = FALSE
  )
  condition$rhs[[1L]] = 0L
  expect_identical(set$deps$cond[[1L]]$rhs, c(1L, 2L))

  detached = set$deps
  detached$cond[[1L]]$rhs[[1L]] = 0L
  expect_identical(set$deps$cond[[1L]]$rhs, c(1L, 2L))

  expect_error(
    {
      set$deps = data.frame(
        id = "parent", on = "parent", cond = I(list(CondEqual(1L))),
        stringsAsFactors = FALSE
      )
    },
    "depend on itself"
  )
  expect_error(
    {
      set$deps = data.frame(
        id = "absent", on = "parent", cond = I(list(CondEqual(1L))),
        stringsAsFactors = FALSE
      )
    },
    "child is not a parameter"
  )
  factor_dependencies = data.frame(
    id = "child",
    on = "parent",
    cond = I(list(CondEqual(1L))),
    stringsAsFactors = TRUE
  )
  expect_error(
    { set$deps = factor_dependencies },
    "`deps` columns have unsupported types",
    fixed = TRUE
  )
  set$deps = data.frame(
    id = "child",
    on = "parent",
    cond = I(list(CondAnyOf(c(1L, 9L)))),
    stringsAsFactors = FALSE
  )
  expect_identical(set$deps$cond[[1L]]$rhs, c(1L, 9L))
  expect_true(set$test(list(parent = 1L, child = TRUE)))
  expect_false(set$test(list(parent = 0L, child = TRUE)))

  # A graph restricted to a narrower parent Domain may legitimately make an
  # existing predicate constantly false.  Bulk assignment preserves that
  # graph exactly; the interactive authoring operation still catches typos.
  set$deps = data.frame(
    id = "child", on = "parent", cond = I(list(CondEqual(9L))),
    stringsAsFactors = FALSE
  )
  expect_identical(set$deps$cond[[1L]]$rhs, 9L)
  expect_true(set$test(list(parent = 0L)))
  expect_false(set$test(list(parent = 0L, child = TRUE)))

  set$deps = data.table::data.table()
  expect_error(
    set$add_dep("child", "parent", CondEqual(9L)),
    "infeasible values"
  )
  expect_error(
    set$add_dep("child", "parent", Condition(1L, "%s == %s")),
    "Unsupported Condition class"
  )

  set$deps = data.table::data.table()
  expect_identical(nrow(set$deps), 0L)
  set$add_dep("child", "foreign", CondEqual(1L),
    allow_dangling_dependencies = TRUE)
  expect_identical(set$deps$on, "foreign")
})

test_that("BASE and SHADOW dependency reads return detached native facades", {
  origin = ps(
    hidden = p_int(),
    parent = p_int(0L, 2L),
    child = p_lgl()
  )
  origin$add_dep("child", "parent", CondAnyOf(c(1L, 2L)))
  shadow = ParamSetShadow$new(origin, "hidden")

  for (set in list(origin, shadow)) {
    first = set$deps
    second = set$deps
    expect_identical(first, second)
    expect_identical(class(first), c("data.table", "data.frame"))
    expect_identical(names(first), c("id", "on", "cond"))
    expect_identical(data.table::key(first), NULL)
    expect_identical(data.table::indices(first), NULL)
    expect_identical(data.table:::selfrefok(first, FALSE), 1L)
    expect_false(identical(
      data.table::address(first),
      data.table::address(second)
    ))
    for (column in names(first)) {
      expect_false(identical(
        data.table::address(first[[column]]),
        data.table::address(second[[column]])
      ), info = column)
    }
    expect_false(identical(
      data.table::address(first$cond[[1L]]),
      data.table::address(second$cond[[1L]])
    ))

    state = paradox:::param_set_core_state(
      set$.__enclos_env__$private
    )$.deps
    expect_identical(class(state), "data.frame")
    expect_null(attr(state, ".internal.selfref", exact = TRUE))

    data.table::set(first, i = 1L, j = "id", value = "changed")
    first$cond[[1L]]$rhs[[1L]] = 0L
    expect_identical(set$deps, second)
  }
})

test_that("dependency admission materializes Condition RHS values once", {
  condition = CondAnyOf(1:2)
  dependencies = data.frame(
    id = "child",
    on = "parent",
    cond = I(list(condition)),
    stringsAsFactors = FALSE
  )

  snapshot = .Call(
    mutation2_symbol("param_set_dependency_table_snapshot"),
    dependencies
  )
  set = ps(parent = p_int(0L, 2L), child = p_lgl())
  set$deps = dependencies

  condition$rhs[[1L]] = 9L
  expect_identical(snapshot$cond[[1L]]$rhs, c(1L, 2L))
  expect_identical(set$deps$cond[[1L]]$rhs, c(1L, 2L))

  callbacks = 0L
  condition = CondAnyOf(c(1L, 2L))
  condition[[1L]] = native_stateful_altrep(
    c(1L, 2L),
    c(9L, 9L),
    elt_switch_after = 2L,
    callback = function() {
      callbacks <<- callbacks + 1L
      condition[[1L]] <<- c(9L, 9L)
      invisible(gc())
    },
    callback_after = 0L
  )
  dependencies$cond[[1L]] = condition
  snapshot = .Call(
    mutation2_symbol("param_set_dependency_table_snapshot"),
    dependencies
  )
  expect_identical(snapshot$cond[[1L]]$rhs, c(1L, 2L))
  expect_identical(condition[[1L]], c(9L, 9L))
  expect_identical(callbacks, 1L)
})

test_that("dependency materialization cannot overwrite a nested mutation", {
  callbacks = 0L
  set = ps(parent = p_int(0L, 2L), child = p_lgl())
  condition = CondAnyOf(c(1L, 2L))
  condition[[1L]] = native_stateful_altrep(
    c(1L, 2L),
    c(1L, 2L),
    callback = function() {
      callbacks <<- callbacks + 1L
      set$tags = list(parent = "nested", child = character())
      invisible(gc())
    },
    callback_after = 0L
  )
  dependencies = data.frame(
    id = "child",
    on = "parent",
    cond = I(list(condition)),
    stringsAsFactors = FALSE
  )

  expect_error({
    set$deps = dependencies
  }, "changed while")
  expect_identical(callbacks, 1L)
  expect_identical(set$tags, list(parent = "nested", child = character()))
  expect_identical(nrow(set$deps), 0L)
})

test_that("dependency admission rejects duplicate AnyOf snapshots", {
  duplicated = CondAnyOf(c(1L, 2L))
  duplicated[[1L]] = c(1L, 1L)
  set = ps(parent = p_int(0L, 2L), child = p_lgl())

  expect_error(
    set$add_dep("child", "parent", duplicated),
    "Malformed built-in dependency Condition"
  )
  expect_identical(nrow(set$deps), 0L)
})

test_that("dependency callback reentry cannot overwrite a newer generation", {
  holder = new.env(parent = emptyenv())
  set = ps(
    parent = p_uty(custom_check = function(x) {
      holder$set$assert_values = FALSE
      holder$set$values = list(parent = x)
      holder$set$assert_values = TRUE
      TRUE
    }),
    child = p_lgl()
  )
  holder$set = set

  expect_error(
    set$add_dep("child", "parent", CondEqual("allowed")),
    "changed while dependency values were being validated"
  )
  expect_identical(nrow(set$deps), 0L)
  expect_identical(set$values, list(parent = "allowed"))
})

test_that("dependency assignment is callback-free and add_dep roots callbacks", {
  checks = 0L
  set = ps(
    parent = p_uty(custom_check = function(x) {
      checks <<- checks + 1L
      garbage = lapply(seq_len(64L), function(index) raw(4096L + index))
      invisible(garbage)
      invisible(gc())
      TRUE
    }),
    first = p_lgl(),
    second = p_lgl()
  )
  checks = 0L
  dependencies = data.frame(
    id = c("first", "second"),
    on = c("parent", "parent"),
    cond = I(list(CondEqual("one"), CondEqual("two"))),
    stringsAsFactors = FALSE
  )

  set$deps = dependencies

  expect_identical(checks, 0L)
  expect_identical(set$deps$id, c("first", "second"))
  expect_identical(set$deps$cond[[1L]]$rhs, "one")
  expect_identical(set$deps$cond[[2L]]$rhs, "two")

  set$deps = data.table::data.table()
  set$add_dep("first", "parent", CondEqual("one"))
  set$add_dep("second", "parent", CondEqual("two"))
  expect_identical(checks, 2L)
  expect_identical(set$deps$id, c("first", "second"))
})

test_that("callback setters are native and keep established formal admission", {
  set = ps(x = p_int())
  one = function(x) x
  two = function(x, param_set) x
  set$extra_trafo = one
  expect_identical(set$extra_trafo, one)
  set$extra_trafo = two
  expect_identical(set$extra_trafo, two)
  set$constraint = function(x, ignored = NULL) TRUE
  expect_true(set$test_constraint(list(x = 1L)))

  expect_error(
    { set$constraint = function(value) TRUE },
    "formal argument named `x`"
  )
  expect_error(
    { set$extra_trafo = sum },
    "formal argument named `x`"
  )
  set$constraint = NULL
  set$extra_trafo = NULL
  expect_null(set$constraint)
  expect_null(set$extra_trafo)
})

test_that("Shadow add_dep routes natively and never crosses its bounds", {
  origin = ps(hidden = p_int(), parent = p_int(0L, 2L), child = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  condition = CondEqual(1L)
  shadow$add_dep("child", "parent", condition)
  condition$rhs = 2L
  expect_identical(origin$deps$cond[[1L]]$rhs, 1L)
  expect_identical(shadow$deps$cond[[1L]]$rhs, 1L)

  # A hidden parent is refused whatever the dangling flag says: the flag is
  # about a parent that does not exist, not about one this view cannot see.
  expect_error(
    shadow$add_dep(
      "child", "hidden", CondEqual(1L),
      allow_dangling_dependencies = TRUE
    ),
    "crosses the ParamSetShadow boundary",
    fixed = TRUE
  )
  expect_identical(nrow(origin$deps), 1L)
})

test_that("Shadow add_dep keeps its origin selection through Condition admission", {
  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )

  origin = ps(parent = p_int(), child = p_int())
  shadow = ParamSetShadow$new(origin, character())
  rhs = native_stateful_altrep(
    1L,
    1L,
    callback = function() origin$values = list(parent = 1L),
    callback_after = NA_integer_
  )
  condition = structure(
    list(rhs = rhs, condition_format_string = "%s == %s"),
    class = c("CondEqual", "Condition")
  )
  native_stateful_altrep_rearm(rhs, 0L)

  expect_error(
    shadow$add_dep("child", "parent", condition),
    "origin changed while a dependency was being constructed",
    fixed = TRUE
  )
  expect_identical(origin$values, list(parent = 1L))
  expect_false(shadow$has_deps)
})
