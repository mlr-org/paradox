context("dormant values and default-aware dependencies")

test_that("T1 activity is transitive and independent of dependency row order", {
  param_set = ps(
    a = p_fct(c("off", "on")),
    b = p_int(),
    c = p_int()
  )
  param_set$add_dep("b", "a", CondEqual("on"))
  param_set$add_dep("c", "b", CondEqual(1L))

  param_set$values = list(a = "off", b = 1L, c = 2L)
  expect_identical(param_set$values, list(a = "off", b = 1L, c = 2L))
  expect_identical(param_set$get_values(), list(a = "off"))

  param_set$deps = param_set$deps[c(2L, 1L)]
  expect_identical(param_set$get_values(), list(a = "off"))
})

test_that("T2-T5 absent parents consult defaults and explicit values win", {
  satisfying = ps(
    parent = p_lgl(default = TRUE),
    child = p_int(depends = parent == TRUE)
  )
  satisfying$values = list(child = 1L)
  expect_identical(satisfying$get_values(), list(child = 1L))
  expect_identical(satisfying$check(list(child = 1L)), TRUE)
  expect_identical(
    satisfying$check_dt(data.table::data.table(child = 1L)),
    TRUE
  )
  expect_identical(satisfying$check_dependencies(list(child = 1L)), TRUE)

  unsatisfying = ps(
    parent = p_lgl(default = FALSE),
    child = p_int(depends = parent == TRUE)
  )
  unsatisfying$values = list(child = 1L)
  expect_identical(unsatisfying$values, list(child = 1L))
  expect_identical(
    unsatisfying$get_values(),
    setNames(list(), character())
  )
  for (diagnostic in list(
    unsatisfying$check(list(child = 1L)),
    unsatisfying$check_dt(data.table::data.table(child = 1L)),
    unsatisfying$check_dependencies(list(child = 1L))
  )) {
    expect_match(diagnostic, "child: can only be set")
    expect_match(diagnostic, "parent.*not set at all")
  }

  no_default = ps(
    parent = p_lgl(),
    child = p_int(depends = parent == TRUE)
  )
  no_default$values = list(child = 1L)
  expect_identical(no_default$get_values(), setNames(list(), character()))
  expect_match(
    no_default$check(list(child = 1L)),
    "child: can only be set.*parent.*not set at all"
  )
  expect_match(
    no_default$check_dependencies(list(child = 1L)),
    "child: can only be set.*parent.*not set at all"
  )

  explicit_unsatisfying = ps(
    parent = p_lgl(default = TRUE),
    child = p_int(depends = parent == TRUE)
  )
  explicit_unsatisfying$values = list(parent = FALSE, child = 1L)
  expect_identical(
    explicit_unsatisfying$get_values(),
    list(parent = FALSE)
  )

  explicit_satisfying = ps(
    parent = p_lgl(default = FALSE),
    child = p_int(depends = parent == TRUE)
  )
  explicit_satisfying$values = list(parent = TRUE, child = 1L)
  expect_identical(
    explicit_satisfying$get_values(),
    list(parent = TRUE, child = 1L)
  )
})

test_that("T6 recursive defaults do not reactivate an inactive parent", {
  puzzle = ps(
    a = p_lgl(default = TRUE),
    b = p_lgl(default = TRUE, depends = a == FALSE),
    c = p_int(depends = b == TRUE)
  )
  puzzle$values = list(b = TRUE, c = 1L)

  expect_identical(puzzle$get_values(), setNames(list(), character()))
  expect_match(
    puzzle$check(list(), presence = "all"),
    "Missing parameters: a"
  )
  expect_identical(
    puzzle$check(list(a = TRUE), presence = "all"),
    TRUE
  )
  expect_match(puzzle$check(list(b = TRUE)), "b: can only be set")
  expect_match(puzzle$check(list(c = 1L)), "c: can only be set")
})

test_that("T7 diamond dependencies remain conjunctive with CondAnyOf", {
  diamond = ps(
    gate = p_fct(c("go", "also", "stop"), default = "go"),
    left = p_lgl(),
    right = p_int(0L, 2L),
    leaf = p_int()
  )
  diamond$add_dep("left", "gate", CondAnyOf(c("go", "also")))
  diamond$add_dep("right", "gate", CondEqual("go"))
  diamond$add_dep("leaf", "left", CondEqual(TRUE))
  diamond$add_dep("leaf", "right", CondAnyOf(c(1L, 2L)))

  diamond$values = list(left = TRUE, right = 1L, leaf = 9L)
  expect_identical(
    diamond$get_values(),
    list(left = TRUE, right = 1L, leaf = 9L)
  )

  diamond$values = list(
    gate = "stop",
    left = TRUE,
    right = 1L,
    leaf = 9L
  )
  expect_identical(diamond$get_values(), list(gate = "stop"))

  diamond$values = list(
    gate = "go",
    left = TRUE,
    right = 0L,
    leaf = 9L
  )
  expect_identical(
    diamond$get_values(),
    list(gate = "go", left = TRUE, right = 0L)
  )
})

test_that("T7 every list-basis activity consumer rejects cycles safely", {
  cyclic = ps(a = p_lgl(), b = p_lgl())
  cyclic$add_dep("a", "b", CondEqual(TRUE))
  cyclic$add_dep("b", "a", CondEqual(TRUE))
  cyclic$assert_values = FALSE
  cyclic$values = list(a = TRUE, b = TRUE)
  cyclic$assert_values = TRUE

  expect_error(cyclic$get_values(), "cycle", ignore.case = TRUE)
  expect_error(
    cyclic$get_values(check_required = TRUE),
    "cycle",
    ignore.case = TRUE
  )
  expect_error(
    cyclic$check(list(a = TRUE, b = TRUE)),
    "cycle",
    ignore.case = TRUE
  )
  expect_error(
    cyclic$check(list(), presence = "all"),
    "cycle",
    ignore.case = TRUE
  )
  expect_error(
    cyclic$check_dt(data.table::data.table(a = TRUE, b = TRUE)),
    "cycle",
    ignore.case = TRUE
  )
  expect_error(
    cyclic$check_dependencies(list(a = TRUE, b = TRUE)),
    "cycle",
    ignore.case = TRUE
  )

  cyclic$constraint = function(x) TRUE
  expect_error(
    cyclic$test_constraint(
      list(a = TRUE, b = TRUE),
      assert_value = FALSE
    ),
    "cycle",
    ignore.case = TRUE
  )
  expect_error(
    cyclic$values <- list(a = TRUE, b = TRUE),
    "cycle",
    ignore.case = TRUE
  )
})

test_that("T8 point checks never consult the stored value state", {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(depends = parent == TRUE)
  )
  param_set$values = list(parent = TRUE, child = 1L)

  expect_identical(param_set$get_values(), list(parent = TRUE, child = 1L))
  expect_match(
    param_set$check(list(child = 1L)),
    "child: can only be set.*parent.*not set at all"
  )
  expect_match(
    param_set$check_dependencies(list(child = 1L)),
    "child: can only be set.*parent.*not set at all"
  )
})

test_that("T9 TuneToken children and parents skip dependency edges", {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(0L, 10L, depends = parent == TRUE)
  )
  parent_token = to_tune()
  parent_point = list(parent = parent_token, child = 1L)
  param_set$values = parent_point

  expect_identical(param_set$get_values(), parent_point)
  expect_identical(param_set$check(parent_point), TRUE)
  expect_identical(param_set$test(parent_point), TRUE)
  expect_identical(param_set$assert(parent_point), parent_point)
  expect_identical(param_set$check_dependencies(parent_point), TRUE)
  expect_identical(
    param_set$check_dt(data.table::data.table(
      parent = list(parent_token),
      child = 1L
    )),
    TRUE
  )

  child_token = to_tune()
  child_point = list(parent = FALSE, child = child_token)
  param_set$values = child_point
  expect_identical(param_set$values, child_point)
  expect_identical(param_set$get_values(), child_point)
  expect_identical(param_set$check(child_point), TRUE)
  expect_identical(param_set$check_dependencies(child_point), TRUE)
})

test_that("T10 checked assignment stores dormant values but stays domain-sound", {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(0L, 10L, depends = parent == TRUE)
  )

  param_set$values$child = 1L
  expect_identical(param_set$values, list(child = 1L))
  expect_identical(param_set$get_values(), setNames(list(), character()))

  param_set$set_values(parent = FALSE, .insert = TRUE)
  expect_identical(param_set$values, list(parent = FALSE, child = 1L))
  expect_identical(param_set$get_values(), list(parent = FALSE))

  param_set$set_values(child = 2L, .insert = FALSE)
  expect_identical(param_set$values, list(child = 2L))
  expect_identical(param_set$get_values(), setNames(list(), character()))

  param_set$values = list(parent = FALSE, child = 3L)
  before = param_set$values
  expect_error(
    param_set$values <- list(parent = FALSE, child = 11L),
    "child:.*not <="
  )
  expect_identical(param_set$values, before)
  expect_error(
    param_set$values <- list(parent = FALSE, child = "wrong"),
    "child:.*integer"
  )
  expect_identical(param_set$values, before)
  expect_error(
    param_set$values <- list(parent = FALSE, unknown = 1L),
    "Parameter 'unknown' not available"
  )
  expect_identical(param_set$values, before)

  calls = 0L
  custom = ps(
    gate = p_lgl(),
    payload = p_uty(
      depends = gate == TRUE,
      custom_check = function(value) {
        calls <<- calls + 1L
        if (identical(value, "ok")) TRUE else "payload rejected"
      }
    )
  )
  calls = 0L
  custom$values = list(gate = FALSE, payload = "ok")
  expect_identical(calls, 1L)
  expect_identical(custom$get_values(), list(gate = FALSE))
  expect_error(
    custom$values <- list(gate = FALSE, payload = "bad"),
    "payload: payload rejected"
  )
  expect_identical(calls, 2L)
  expect_identical(custom$values, list(gate = FALSE, payload = "ok"))
})

test_that("T11 dormant values update in place and reactivate automatically", {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(depends = parent == TRUE)
  )
  param_set$values = list(parent = FALSE, child = 1L)
  expect_identical(param_set$get_values(), list(parent = FALSE))

  param_set$values$child = 2L
  expect_identical(param_set$values, list(parent = FALSE, child = 2L))
  expect_identical(param_set$get_values(), list(parent = FALSE))

  param_set$values$parent = TRUE
  expect_identical(
    param_set$get_values(),
    list(parent = TRUE, child = 2L)
  )
  param_set$values$parent = FALSE
  expect_identical(param_set$get_values(), list(parent = FALSE))
})

test_that("T12 dormant writes traverse Collection and Shadow graphs", {
  left = ps(gate = p_lgl())
  right = ps(value = p_int())
  collection = ParamSetCollection$new(list(left = left, right = right))
  collection$add_dep(
    "right.value",
    "left.gate",
    CondEqual(TRUE)
  )

  collection$values = list(left.gate = FALSE, right.value = 1L)
  expect_identical(
    collection$values,
    list(left.gate = FALSE, right.value = 1L)
  )
  expect_identical(collection$get_values(), list(left.gate = FALSE))
  expect_identical(right$get_values(), list(value = 1L))

  raw = collection$values
  collection$values = raw
  expect_identical(collection$values, raw)
  expect_identical(collection$get_values(), list(left.gate = FALSE))

  origin = ps(
    hidden_gate = p_lgl(),
    hidden_value = p_int(depends = hidden_gate == TRUE),
    gate = p_lgl(),
    value = p_int(depends = gate == TRUE)
  )
  origin$values = list(
    hidden_gate = FALSE,
    hidden_value = 9L
  )
  shadow = ParamSetShadow$new(origin, c("hidden_gate", "hidden_value"))
  shadow$values = list(gate = FALSE, value = 2L)
  expect_identical(
    origin$values,
    list(
      hidden_gate = FALSE,
      hidden_value = 9L,
      gate = FALSE,
      value = 2L
    )
  )
  expect_identical(shadow$values, list(gate = FALSE, value = 2L))
  expect_identical(shadow$get_values(), list(gate = FALSE))

  shadow$values$value = 3L
  expect_identical(origin$values$value, 3L)
  expect_identical(shadow$get_values(), list(gate = FALSE))
})

test_that("T13 BASE constraints receive one dependency-filtered snapshot", {
  calls = 0L
  observed = list()
  param_set = ps(
    parent = p_lgl(),
    child = p_int(depends = parent == TRUE)
  )
  param_set$constraint = function(x) {
    calls <<- calls + 1L
    observed[[calls]] <<- x
    !"child" %in% names(x)
  }

  param_set$values = list(parent = FALSE, child = 1L)
  expect_identical(calls, 1L)
  expect_identical(observed[[1L]], list(parent = FALSE))

  expect_true(param_set$test_constraint(
    list(parent = FALSE, child = 2L),
    assert_value = FALSE
  ))
  expect_identical(calls, 2L)
  expect_identical(observed[[2L]], list(parent = FALSE))

  row_inputs = list()
  param_set$constraint = function(x) {
    row_inputs[[length(row_inputs) + 1L]] <<- x
    TRUE
  }
  expect_identical(
    param_set$test_constraint_dt(
      data.table::data.table(
        parent = c(FALSE, TRUE),
        child = c(1L, 2L)
      ),
      assert_value = FALSE
    ),
    c(TRUE, TRUE)
  )
  expect_identical(row_inputs[[1L]], list(parent = FALSE))
  expect_identical(row_inputs[[2L]], list(parent = TRUE, child = 2L))
})

test_that("T13 Collection and Shadow constraints filter before slicing", {
  collection_inputs = list()
  left = ps(gate = p_lgl())
  right = ps(value = p_int(), static = p_int())
  right$constraint = function(x) {
    collection_inputs[[length(collection_inputs) + 1L]] <<- x
    TRUE
  }
  collection = ParamSetCollection$new(list(left = left, right = right))
  collection$add_dep("right.value", "left.gate", CondEqual(TRUE))

  dormant = list(
    left.gate = FALSE,
    right.value = 1L,
    right.static = 2L
  )
  collection$values = dormant
  expect_identical(collection_inputs[[1L]], list(static = 2L))

  expect_true(collection$test_constraint(dormant, assert_value = FALSE))
  expect_identical(collection_inputs[[2L]], list(static = 2L))

  active = list(
    left.gate = TRUE,
    right.value = 1L,
    right.static = 2L
  )
  expect_true(collection$test_constraint(active, assert_value = FALSE))
  expect_identical(
    collection_inputs[[3L]],
    list(value = 1L, static = 2L)
  )

  detached = collection$flatten()
  expect_true(detached$test_constraint(dormant, assert_value = FALSE))
  expect_identical(collection_inputs[[4L]], list(static = 2L))

  shadow_inputs = list()
  origin = ps(
    hidden_gate = p_lgl(),
    hidden_value = p_int(depends = hidden_gate == TRUE),
    gate = p_lgl(),
    value = p_int(depends = gate == TRUE)
  )
  origin$values = list(
    hidden_gate = FALSE,
    hidden_value = 9L
  )
  origin$constraint = function(x) {
    shadow_inputs[[length(shadow_inputs) + 1L]] <<- x
    TRUE
  }
  shadow = ParamSetShadow$new(origin, c("hidden_gate", "hidden_value"))
  shadow$values = list(gate = FALSE, value = 1L)

  expect_length(shadow_inputs, 1L)
  expect_identical(
    shadow_inputs[[1L]],
    list(hidden_gate = FALSE, gate = FALSE)
  )
  expect_true(shadow$test_constraint(
    list(gate = FALSE, value = 2L),
    assert_value = FALSE
  ))
  expect_identical(
    shadow_inputs[[2L]],
    list(hidden_gate = FALSE, gate = FALSE)
  )
})

test_that("T14 special-value parent operands retain comparator behavior", {
  param_set = ps(
    parent = p_dbl(special_vals = list("AUTO", NULL)),
    child = p_int(depends = parent == "AUTO")
  )

  param_set$values = list(parent = NULL, child = 1L)
  inactive = param_set$get_values()
  expect_named(inactive, "parent")
  expect_null(inactive$parent)
  expect_match(
    param_set$check(param_set$values),
    "child: can only be set"
  )

  param_set$values = list(parent = "AUTO", child = 1L)
  expect_identical(
    param_set$get_values(),
    list(parent = "AUTO", child = 1L)
  )
  expect_identical(param_set$check(param_set$values), TRUE)
})

test_that("T15 ParamUty dependency operands retain closed comparator behavior", {
  param_set = ps(parent = p_uty(), child = p_int())
  param_set$add_dep("child", "parent", CondEqual(1L))

  param_set$values = list(parent = 1L, child = 2L)
  expect_identical(
    param_set$get_values(),
    list(parent = 1L, child = 2L)
  )
  expect_identical(param_set$check(param_set$values), TRUE)

  opaque = list(payload = 1L)
  param_set$values = list(parent = opaque, child = 2L)
  expect_identical(param_set$get_values(), list(parent = opaque))
  expect_match(
    param_set$check(param_set$values),
    "requires a plain scalar logical, integer, double, or character"
  )
  expect_match(
    param_set$check_dependencies(param_set$values),
    "requires a plain scalar logical, integer, double, or character"
  )
})

test_that("T16 dangling parents remain absent after bulk deps and subset", {
  bulk = ps(child = p_int())
  bulk$deps = data.table::data.table(
    id = "child",
    on = "ghost",
    cond = list(CondEqual(TRUE))
  )
  bulk$values = list(child = 1L)
  expect_identical(bulk$get_values(), setNames(list(), character()))
  expect_match(
    bulk$check(bulk$values),
    "child: can only be set.*ghost.*not set at all"
  )

  original = ps(
    parent = p_lgl(default = TRUE),
    child = p_int(depends = parent == TRUE)
  )
  dangling = original$subset(
    "child",
    allow_dangling_dependencies = TRUE
  )
  dangling$values = list(child = 1L)
  expect_identical(dangling$get_values(), setNames(list(), character()))
  expect_match(
    dangling$check(dangling$values),
    "child: can only be set.*parent.*not set at all"
  )
})

test_that("T17 presence modes demand children activated by defaults", {
  active = ps(
    parent = p_lgl(default = TRUE),
    child = p_int(tags = "required", depends = parent == TRUE)
  )
  expect_match(
    active$check(list(), presence = "required"),
    "satisfied dependencies: child|Missing parameters: child"
  )
  expect_match(
    active$check(list(parent = TRUE), presence = "all"),
    "satisfied dependencies: child|Missing parameters: child"
  )

  inactive = ps(
    parent = p_lgl(default = FALSE),
    child = p_int(tags = "required", depends = parent == TRUE)
  )
  expect_identical(inactive$check(list(), presence = "required"), TRUE)
  expect_identical(
    inactive$check(list(parent = FALSE), presence = "all"),
    TRUE
  )
})

test_that("T18 required get_values checks use the active stored view", {
  dormant = ps(
    parent = p_lgl(),
    child = p_int(tags = "required", depends = parent == TRUE)
  )
  dormant$values = list(parent = FALSE)
  expect_identical(
    dormant$get_values(check_required = TRUE),
    list(parent = FALSE)
  )

  default_active = ps(
    parent = p_lgl(default = TRUE),
    child = p_int(tags = "required", depends = parent == TRUE)
  )
  expect_error(
    default_active$get_values(check_required = TRUE),
    "Missing required parameters: child"
  )

  explicitly_active = ps(
    parent = p_lgl(),
    child = p_int(tags = "required", depends = parent == TRUE)
  )
  explicitly_active$values = list(parent = TRUE)
  expect_error(
    explicitly_active$get_values(check_required = TRUE),
    "Missing required parameters: child"
  )
})

test_that("T19 dormant state survives public lifecycle operations", {
  make_set = function(value) {
    result = ps(
      parent = p_lgl(),
      child = p_int(
        depends = parent == TRUE,
        trafo = function(x) x + 1L
      )
    )
    result$values = list(parent = FALSE, child = value)
    result
  }
  param_set = make_set(1L)

  restored = unserialize(serialize(param_set, NULL, version = 3L))
  cloned = param_set$clone(deep = TRUE)
  for (copy in list(restored, cloned)) {
    expect_identical(copy$values, list(parent = FALSE, child = 1L))
    expect_identical(copy$get_values(), list(parent = FALSE))
    expect_true(isTRUE(all.equal(param_set, copy)))
  }

  different = make_set(2L)
  expect_false(isTRUE(all.equal(param_set, different)))
  expect_identical(upgrade_paradox_object(param_set), param_set)
  carrier = new.env(parent = emptyenv())
  carrier$param_set = param_set
  expect_identical(upgrade_paradox_object_graph(carrier), carrier)
  expect_identical(carrier$param_set, param_set)

  collection = ParamSetCollection$new(list(owner = param_set))
  expect_identical(
    collection$values,
    list(owner.parent = FALSE, owner.child = 1L)
  )
  expect_identical(collection$get_values(), list(owner.parent = FALSE))

  for (keep_trafo in c(FALSE, TRUE)) {
    subset = param_set$subset(
      c("parent", "child"),
      keep_trafo = keep_trafo
    )
    expect_identical(subset$values, list(parent = FALSE, child = 1L))
    expect_identical(subset$get_values(), list(parent = FALSE))
  }
})

test_that("T20 outside search-space parents retain the bounded current rule", {
  make_space = function(default) {
    parent = if (inherits(default, "NoDefault")) {
      p_lgl()
    } else {
      p_lgl(default = default)
    }
    ps(
      parent = parent,
      child = p_int(0L, 10L, depends = parent == TRUE)
    )
  }

  cases = list(
    default_satisfies = list(default = TRUE, parent = NULL),
    default_does_not_satisfy = list(default = FALSE, parent = NULL),
    no_default = list(default = NO_DEF, parent = NULL),
    fixed_satisfies = list(default = FALSE, parent = TRUE),
    fixed_does_not_satisfy = list(default = TRUE, parent = FALSE)
  )
  for (case in cases) {
    param_set = make_space(case$default)
    values = list(child = to_tune())
    if (!is.null(case$parent)) {
      values = c(list(parent = case$parent), values)
    }
    param_set$values = values

    search_space = param_set$search_space()
    expect_identical(search_space$ids(), "child")
    expect_identical(nrow(search_space$deps), 0L)
  }
})

test_that("T21 a legal dormant store is not necessarily a strict point", {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(depends = parent == TRUE)
  )
  param_set$values = list(parent = FALSE, child = 1L)

  expect_match(param_set$check(param_set$values), "child: can only be set")
  expect_identical(param_set$check(param_set$get_values()), TRUE)
  expect_identical(
    param_set$check(param_set$values, check_strict = FALSE),
    TRUE
  )
})

test_that("T22 complete design rows do not consult Domain defaults", {
  without_default = ps(
    mode = p_fct(c("off", "on")),
    value = p_int(0L, 2L, depends = mode == "on")
  )
  with_default = ps(
    mode = p_fct(c("off", "on"), default = "on"),
    value = p_int(0L, 2L, depends = mode == "on")
  )

  expect_identical(
    generate_design_grid(without_default, resolution = 3L)$data,
    generate_design_grid(with_default, resolution = 3L)$data
  )

  set.seed(20260726L)
  first = generate_design_random(without_default, 64L)$data
  set.seed(20260726L)
  second = generate_design_random(with_default, 64L)$data
  expect_identical(first, second)

  set.seed(20260727L)
  first = SamplerUnif$new(without_default)$sample(64L)$data
  set.seed(20260727L)
  second = SamplerUnif$new(with_default)$sample(64L)$data
  expect_identical(first, second)
})

test_that("T24 check_dt remains point-strict and treats NA as absent", {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(depends = parent == TRUE)
  )

  expect_match(
    param_set$check_dt(data.table::data.table(
      parent = FALSE,
      child = 1L
    )),
    "child: can only be set"
  )
  expect_identical(
    param_set$check_dt(data.table::data.table(
      parent = FALSE,
      child = NA_integer_
    )),
    TRUE
  )
  expect_identical(
    param_set$check_dt(
      data.table::data.table(parent = FALSE, child = 1L),
      check_strict = FALSE
    ),
    TRUE
  )
})
