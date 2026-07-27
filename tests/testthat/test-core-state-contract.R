core_private = function(x) x$.__enclos_env__$private

core_state = function(x) paradox:::param_set_core_state(core_private(x))

test_that("current nodes expose only one versioned state capsule", {
  base = ps(x = p_dbl(0, 1), flag = p_lgl())
  collection = ParamSetCollection$new(list(child = base))
  shadow = ParamSetShadow$new(base, "flag")

  expect_type(core_private(base)$.core, "externalptr")
  expect_type(core_private(collection)$.core, "externalptr")
  expect_type(core_private(shadow)$.core, "externalptr")
  expect_identical(.Call(paradox:::C_param_set_core_kind, core_private(base)), 1L)
  expect_identical(.Call(paradox:::C_param_set_core_kind, core_private(collection)), 2L)
  expect_identical(.Call(paradox:::C_param_set_core_kind, core_private(shadow)), 3L)

  retired = c(
    ".params", ".values", ".tags", ".deps", ".trafos",
    ".extra_trafo", ".constraint", ".sets", ".translation", ".postfix"
  )
  for (object in list(base, collection, shadow)) {
    private = core_private(object)
    expect_true(exists(".core", private, inherits = FALSE))
    expect_false(any(vapply(
      retired,
      exists,
      logical(1L),
      envir = private,
      inherits = FALSE
    )))
  }
})

test_that("capsule admission rejects noncanonical physical schemas", {
  set = ps(x = p_int())
  state = core_state(set)

  extra_payload_attribute = state
  attr(extra_payload_attribute, "rogue") = TRUE
  expect_error(
    .Call(
      paradox:::C_param_set_core_new,
      1L,
      extra_payload_attribute
    ),
    "exact canonical ten-field"
  )

  attributed_names = state
  attr(attributed_names, "names") = structure(
    names(attributed_names),
    rogue = TRUE
  )
  expect_error(
    .Call(paradox:::C_param_set_core_new, 1L, attributed_names),
    "exact canonical ten-field"
  )
})

test_that("installed capsule carrier attributes are rejected, not healed", {
  objects = list(
    base = ps(x = p_int()),
    collection = ParamSetCollection$new(list(unit = ps(x = p_int())))
  )

  for (object in objects) {
    private = core_private(object)
    attr(private$.core, "rogue") = TRUE

    expect_error(object$get_values(), "Corrupt ParamSet")
    expect_error(object$clone(deep = TRUE), "Corrupt ParamSet")
    restored = unserialize(serialize(object, NULL, version = 3L))
    expect_error(restored$get_values(), "Corrupt ParamSet")
    expect_error(
      upgrade_paradox_object(object),
      "corrupt current state capsule"
    )
  }
})

test_that("capsule admission rejects S4-marked carriers and schema metadata", {
  set = ps(x = p_int())
  state = core_state(set)

  s4_state = asS4(state)
  expect_error(
    .Call(paradox:::C_param_set_core_new, 1L, s4_state),
    "exact canonical ten-field"
  )

  s4_names = state
  attr(s4_names, "names") = asS4(names(s4_names))
  expect_error(
    .Call(paradox:::C_param_set_core_new, 1L, s4_names),
    "exact canonical ten-field"
  )

  carrier_set = ps(x = p_int())
  asS4(core_private(carrier_set)$.core)
  expect_error(carrier_set$get_values(), "Corrupt ParamSet")

  private_set = ps(x = p_int())
  private = core_private(private_set)
  asS4(private)
  expect_error(
    paradox:::param_set_core_state(private),
    "Corrupt ParamSet|missing versioned core capsule"
  )

  expect_error(
    paradox:::param_set_core_state(new.env(parent = emptyenv())),
    "missing versioned core capsule|no binding"
  )
})

test_that("capsule tables are plain immutable column stores", {
  set = ps(
    x = p_dbl(0, 1, tags = "numeric"),
    flag = p_lgl(depends = x == 1)
  )
  state = core_state(set)

  for (field in c(".params", ".tags", ".deps", ".trafos")) {
    table = state[[field]]
    expect_identical(class(table), "data.frame")
    expect_false(inherits(table, "data.table"))
    expect_null(attr(table, ".internal.selfref", exact = TRUE))
    expect_null(attr(table, "index", exact = TRUE))
    expect_null(attr(table, "sorted", exact = TRUE))
  }
})

test_that("public mutation swaps capsules and old operation snapshots stay fixed", {
  set = ps(x = p_int(), flag = p_lgl())
  old_core = core_private(set)$.core
  old_state = paradox:::param_set_core_state(old_core)

  set$values = list(flag = TRUE, x = 2L)
  new_core = core_private(set)$.core
  expect_false(identical(
    data.table::address(old_core),
    data.table::address(new_core)
  ))
  expect_identical(old_state$.values, named_list())
  expect_identical(core_state(set)$.values, list(x = 2L, flag = TRUE))

  set$tags = list(x = "changed", flag = character())
  expect_identical(old_state$.tags$id, character())
  expect_identical(set$tags, list(x = "changed", flag = character()))
})

test_that("public table facades are detached data.tables", {
  set = ps(x = p_int(), flag = p_lgl())
  set$add_dep("x", "flag", CondEqual(TRUE))

  params = set$params
  deps = set$deps
  expect_s3_class(params, "data.table")
  expect_s3_class(deps, "data.table")

  data.table::set(params, 1L, "id", "mutated")
  data.table::set(deps, 1L, "id", "mutated")
  deps$cond[[1L]]$rhs = FALSE

  expect_identical(set$ids(), c("x", "flag"))
  expect_identical(set$deps$id, "x")
  expect_identical(set$deps$cond[[1L]]$rhs, TRUE)
})

test_that("condition assignment snapshots the closed condition value", {
  set = ps(x = p_int(), flag = p_lgl())
  condition = CondEqual(TRUE)
  set$add_dep("x", "flag", condition)
  condition$rhs = FALSE

  expect_identical(set$deps$cond[[1L]]$rhs, TRUE)
  expect_identical(set$get_values(remove_dependencies = FALSE), named_list())
})

test_that("capsules serialize and deep-clone without unmanaged state", {
  origin = ps(hidden = p_int(), x = p_dbl(0, 1))
  origin$values = list(hidden = 1L, x = 0.5)
  graph = ParamSetCollection$new(list(
    base = origin,
    view = ParamSetShadow$new(origin, "hidden")
  ))

  restored = unserialize(serialize(graph, NULL))
  expect_identical(restored$values, graph$values)
  expect_identical(
    .Call(paradox:::C_param_set_core_kind, core_private(restored)),
    2L
  )

  clone = graph$clone(deep = TRUE)
  clone$sets$view$values = list(x = 0.75)
  expect_identical(clone$sets$base$values$x, 0.75)
  expect_identical(origin$values$x, 0.5)
})

test_that("additive ParamSet subclasses share the sealed engine", {
  AdditiveSet = R6::R6Class(
    "AdditiveSet",
    inherit = ParamSet,
    public = list(
      label = NULL,
      initialize = function(params, label) {
        super$initialize(params)
        self$label = label
      }
    )
  )
  set = AdditiveSet$new(list(x = p_int()), "kept")
  set$values = list(x = 1L)

  expect_identical(set$label, "kept")
  expect_identical(set$get_values(), list(x = 1L))
  expect_identical(.Call(paradox:::C_param_set_core_kind, core_private(set)), 1L)
})
