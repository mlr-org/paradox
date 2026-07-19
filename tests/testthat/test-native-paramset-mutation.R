mutation2_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"), inherits = FALSE)
}

test_that("ParamSet mutators have fixed registered native interfaces", {
  expected = c(
    param_set_set_tags = 3L,
    param_set_get_tags = 2L,
    param_set_dependency_table_snapshot = 1L,
    param_set_dependencies = 2L,
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
})

test_that("dependency setters share one native admission and snapshot engine", {
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
        id = "parent", on = "parent", cond = I(list(CondEqual(1L)))
      )
    },
    "depend on itself"
  )
  expect_error(
    {
      set$deps = data.frame(
        id = "absent", on = "parent", cond = I(list(CondEqual(1L)))
      )
    },
    "child is not a parameter"
  )
  expect_error(
    {
      set$deps = data.frame(
        id = "child", on = "parent", cond = I(list(CondEqual(9L)))
      )
    },
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

test_that("dependency snapshots stay rooted through feasibility callbacks", {
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

  expect_identical(checks, 2L)
  expect_identical(set$deps$id, c("first", "second"))
  expect_identical(set$deps$cond[[1L]]$rhs, "one")
  expect_identical(set$deps$cond[[2L]]$rhs, "two")
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

test_that("Shadow add_dep routes natively and stays inside its visible schema", {
  origin = ps(hidden = p_int(), parent = p_int(0L, 2L), child = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  condition = CondEqual(1L)
  shadow$add_dep("child", "parent", condition)
  condition$rhs = 2L
  expect_identical(origin$deps$cond[[1L]]$rhs, 1L)
  expect_identical(shadow$deps$cond[[1L]]$rhs, 1L)

  expect_error(
    shadow$add_dep(
      "child", "hidden", CondEqual(1L),
      allow_dangling_dependencies = TRUE
    ),
    "visible schema"
  )
})
