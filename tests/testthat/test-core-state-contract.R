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
    ".extra_trafo", ".constraint", ".sets", ".translation", ".postfix",
    ".edges"
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
    "exact canonical eleven-field"
  )

  attributed_names = state
  attr(attributed_names, "names") = structure(
    names(attributed_names),
    rogue = TRUE
  )
  expect_error(
    .Call(paradox:::C_param_set_core_new, 1L, attributed_names),
    "exact canonical eleven-field"
  )
})

test_that("every capsule field name is checked on each admission", {
  state = core_state(ps(x = p_int()))
  expected_names = names(state)
  for (field in seq_along(expected_names)) {
    changed = state
    names(changed)[field] = paste0(expected_names[field], "_changed")
    expect_error(.Call(paradox:::C_param_set_core_new, 1L, changed),
      "exact canonical eleven-field")
    names(changed)[field] = NA_character_
    expect_error(.Call(paradox:::C_param_set_core_new, 1L, changed),
      "exact canonical eleven-field")
  }
  restored = unserialize(serialize(state, NULL, version = 2L))
  capsule = .Call(paradox:::C_param_set_core_new, 1L, restored)
  expect_identical(names(paradox:::param_set_core_state(capsule)), expected_names)
})

test_that("private readers ignore unused capsule attributes but explicit admission checks them", {
  objects = list(
    base = ps(x = p_int()),
    collection = ParamSetCollection$new(list(unit = ps(x = p_int())))
  )

  for (object in objects) {
    private = core_private(object)
    attr(private$.core, "rogue") = TRUE

    expect_identical(object$get_values(), named_list())
    expect_error(object$clone(deep = TRUE), "Invalid ParamSet deep-clone capsule")
    restored = unserialize(serialize(object, NULL, version = 3L))
    expect_identical(restored$get_values(), named_list())
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
    "exact canonical eleven-field"
  )

  s4_names = state
  attr(s4_names, "names") = asS4(names(s4_names))
  expect_error(
    .Call(paradox:::C_param_set_core_new, 1L, s4_names),
    "exact canonical eleven-field"
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

test_that("deep capsule graph validation roots every active generation", {
  graph = ps(x = p_int())
  for (index in seq_len(20L)) {
    graph = ParamSetCollection$new(list(node = graph))
  }

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  shadow = ParamSetShadow$new(graph, character())
  gctorture(previous)

  expect_s3_class(shadow, "ParamSetShadow")
  expect_identical(shadow$origin, graph)
  expect_identical(
    shadow$ids(),
    paste0(paste(rep("node", 20L), collapse = "."), ".x")
  )
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

test_that("capsule tables must be rectangular before any consumer indexes them", {
  # Private translation columns matter only to operations that consume them.
  # Those consumers must still reject shorter columns before indexing.
  make_collection = function() {
    collection = ParamSetCollection$new(list(
      a = ps(x = p_dbl(0, 1), y = p_dbl(0, 1)),
      b = ps(z = p_dbl(0, 1))
    ))
    collection$values = list(a.x = 0.25, a.y = 0.5, b.z = 0.75)
    collection
  }

  truncate_column = function(table, column) {
    ragged = unclass(table)
    ragged[[column]] = ragged[[column]][1L]
    attributes(ragged) = list(
      names = names(table),
      class = "data.frame",
      row.names = seq_along(ragged[[1L]])
    )
    ragged
  }

  for (column in c("original_id", "owner_ps_index", "owner_name")) {
    collection = make_collection()
    private = core_private(collection)
    state = core_state(collection)
    state$.translation = truncate_column(state$.translation, column)
    assign(
      ".core",
      .Call(paradox:::C_param_set_core_new, 2L, state),
      envir = private
    )

    expect_identical(collection$values, list(a.x = 0.25, a.y = 0.5, b.z = 0.75))
    expect_identical(collection$get_values(), collection$values)
    expect_identical(nrow(collection$deps), 0L)
    expect_error(collection$subset("a.x"), "Corrupt ParamSetCollection")
  }
})

test_that("capsule tables reject S4 structural shells and metadata when used", {
  install_base_state = function(state, check = function(set) set$get_values(),
    reject = TRUE) {
    set = ps(x = p_int())
    assign(
      ".core",
      .Call(paradox:::C_param_set_core_new, 1L, state),
      envir = core_private(set)
    )
    if (reject) expect_error(check(set), "Corrupt") else {
      expect_identical(check(set), named_list())
    }
  }

  fresh_base_state = function() {
    unserialize(serialize(core_state(ps(x = p_int())), NULL))
  }

  for (field in c(".params", ".tags", ".deps")) {
    state = fresh_base_state()
    state[[field]] = asS4(state[[field]])
    install_base_state(state)
  }

  state = fresh_base_state()
  state$.trafos = asS4(state$.trafos)
  install_base_state(state, function(set) set$params)

  for (metadata in c("names", "class", "row.names")) {
    state = fresh_base_state()
    attr(state$.params, metadata) = asS4(
      attr(state$.params, metadata, exact = TRUE)
    )
    install_base_state(state, reject = FALSE)
  }

  state = fresh_base_state()
  state$.params[[1L]] = asS4(state$.params[[1L]])
  install_base_state(state)

  state = unserialize(serialize(
    core_state(ps(x = p_fct(c("a", "b")))),
    NULL
  ))
  state$.params$levels[[1L]] = asS4(state$.params$levels[[1L]])
  install_base_state(state, reject = FALSE)

  state = fresh_base_state()
  state$.params$special_vals[[1L]] = asS4(
    state$.params$special_vals[[1L]]
  )
  install_base_state(state, reject = FALSE)

  state = fresh_base_state()
  special_values = state$.params$special_vals[[1L]]
  attr(special_values, "class") = asS4("rogue_special_values")
  state$.params$special_vals[[1L]] = special_values
  install_base_state(state, reject = FALSE)

  state = fresh_base_state()
  special_values = state$.params$special_vals[[1L]]
  class(special_values) = "rogue_special_values"
  state$.params$special_vals[[1L]] = special_values
  install_base_state(state, reject = FALSE)

  # Neither table attributes nor special values are used by this reader.
  state = fresh_base_state()
  for (index in seq_len(65L)) {
    attr(
      state$.params,
      sprintf("paradox.table.attribute.%03d", index)
    ) = index
  }
  install_base_state(state, reject = FALSE)

  state = fresh_base_state()
  special_values = state$.params$special_vals[[1L]]
  for (index in seq_len(65L)) {
    attr(
      special_values,
      sprintf("paradox.special.attribute.%03d", index)
    ) = index
  }
  state$.params$special_vals[[1L]] = special_values
  install_base_state(state, reject = FALSE)

  for (mutate in list(
      function(state) {
        state$.values = asS4(state$.values)
        state
      },
      function(state) {
        attr(state$.values, "names") = asS4(names(state$.values))
        state
      }
    )) {
    install_base_state(mutate(fresh_base_state()), reject = FALSE)
  }

  # Construction does not pre-admit unrelated private child fields.
  for (field in c(".postfix", ".values", ".deps")) {
    state = fresh_base_state()
    state[[field]] = asS4(state[[field]])
    install_base_state(
      state,
      function(set) {
        collection = ParamSetCollection$new(list(child = set))
        expect_identical(collection$ids(), "child.x")
        named_list()
      }, reject = FALSE
    )
  }

  install_collection_state = function(mutate) {
    collection = ParamSetCollection$new(list(child = ps(x = p_int())))
    state = unserialize(serialize(core_state(collection), NULL))
    state = mutate(state)
    assign(
      ".core",
      .Call(paradox:::C_param_set_core_new, 2L, state),
      envir = core_private(collection)
    )
    expect_identical(collection$get_values(), named_list())
  }
  for (mutate in list(
      function(state) {
        state$.translation = asS4(state$.translation)
        state
      },
      function(state) {
        state$.sets = asS4(state$.sets)
        state
      },
      function(state) {
        attr(state$.sets, "names") = asS4(names(state$.sets))
        state
      },
      function(state) {
        state$.postfix = asS4(state$.postfix)
        state
      }
    )) {
    install_collection_state(mutate)
  }
})

test_that("$params ignores unused private row-name carriers", {
  set = ps(x = p_int())
  private = core_private(set)
  state = unserialize(serialize(core_state(set), NULL))
  callbacks = 0L
  # R <= 4.5's public row.names<- path obtains DATAPTR from integer row-name
  # values while checking their compact spelling. Character row names avoid
  # that base-R construction detail, so the same deliberately no-DATAPTR
  # fixture proves non-observation on every supported runtime.
  wrong_length = native_stateful_altrep(
    c("first", "second"),
    c("first", "second"),
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("row-name ALTREP was observed", call. = FALSE)
    },
    callback_after = 0L
  )
  # Base attr<- installs the fixture without touching its data pointer;
  # data.table::setattr() materializes a referenced value and would replace
  # the fixture with an ordinary copy before the capsule ever sees it.
  attr(state$.params, "row.names") = wrong_length
  private$.core = .Call(paradox:::C_param_set_core_new, 1L, state)

  expect_identical(set$params$id, "x")
  expect_identical(callbacks, 0L)

  state = unserialize(serialize(core_state(ps(x = p_int())), NULL))
  data.table::setattr(state$.params, "row.names", 2L)
  private$.core = .Call(paradox:::C_param_set_core_new, 1L, state)
  expect_identical(set$params$id, "x")
})

test_that("capsule tables admit each exact ordinary row-name spelling", {
  for (row_names in list(
      c(1L, 2L),
      c(NA_integer_, -2L),
      c(NA_integer_, 2L)
    )) {
    set = ps(x = p_int(), y = p_int())
    state = unserialize(serialize(core_state(set), NULL))
    attr(state$.params, "row.names") = row_names
    assign(
      ".core",
      .Call(paradox:::C_param_set_core_new, 1L, state),
      envir = core_private(set)
    )
    expect_identical(set$get_values(), named_list())
  }
})

test_that("capsule readers guard IDs but do not observe private row names", {
  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )

  calls = 0L
  set = ps(x = p_int(), y = p_int())
  state = unserialize(serialize(core_state(set), NULL))
  hostile = native_stateful_altrep(
    state$.params[[1L]],
    rev(state$.params[[1L]]),
    callback = function() calls <<- calls + 1L
  )
  state$.params[[1L]] = hostile
  native_stateful_altrep_rearm(hostile, c(NA_integer_, 0L))
  calls = 0L
  assign(
    ".core",
    .Call(paradox:::C_param_set_core_new, 1L, state),
    envir = core_private(set)
  )

  expect_error(set$get_values(), "Corrupt ParamSet")
  expect_identical(calls, 0L)

  calls = 0L
  set = ps(x = p_int(), y = p_int())
  state = unserialize(serialize(core_state(set), NULL))
  row_names = c("one", "two")
  hostile = native_stateful_altrep(
    row_names,
    rev(row_names),
    callback = function() calls <<- calls + 1L
  )
  attr(state$.params, "row.names") = hostile
  native_stateful_altrep_rearm(hostile, c(NA_integer_, 0L))
  calls = 0L
  assign(
    ".core",
    .Call(paradox:::C_param_set_core_new, 1L, state),
    envir = core_private(set)
  )

  expect_identical(set$get_values(), named_list())
  expect_identical(calls, 0L)
})

test_that("capsule table columns must use ordinary representations", {
  set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  private = core_private(set)
  state = core_state(set)

  params = unclass(state$.params)
  params$lower = seq_along(params$lower) / 10
  attributes(params) = list(
    names = names(state$.params),
    class = "data.frame",
    # Materialized on purpose: `seq_along()` yields a compact ALTREP sequence,
    # and the capsule validator rejects ALTREP row names by design (see the
    # sibling tests above).
    row.names = c(1L, 2L)
  )
  state$.params = params
  assign(
    ".core",
    .Call(paradox:::C_param_set_core_new, 1L, state),
    envir = private
  )
  expect_identical(unname(set$lower), c(0.1, 0.2))
})

test_that("deep cloning a childless collection keeps a usable capsule", {
  origin = ParamSetCollection$new(list())
  clone = origin$clone(deep = TRUE)

  expect_identical(clone$ids(), character(0))
  expect_identical(clone$values, named_list())
  expect_identical(nrow(clone$deps), 0L)
  expect_identical(nrow(clone$params), 0L)
  expect_identical(clone$get_values(), named_list())
  expect_identical(clone$flatten()$ids(), character(0))
  expect_identical(clone$subset(character(0))$ids(), character(0))
  expect_true(all.equal(clone, origin))

  clone$add(ps(x = p_dbl(0, 1)), n = "later")
  expect_identical(clone$ids(), "later.x")
  expect_identical(origin$ids(), character(0))
})
