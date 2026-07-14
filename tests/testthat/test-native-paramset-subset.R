native_subset_available = function() {
  namespace = asNamespace("paradox")
  all(vapply(
    c("C_param_set_subset_state", "C_param_set_adopt_subset_state"),
    exists,
    logical(1L),
    envir = namespace,
    inherits = FALSE
  ))
}

native_subset_symbol = function() {
  get(
    "C_param_set_subset_state",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_subset_adopt_symbol = function() {
  get(
    "C_param_set_adopt_subset_state",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_subset_call = function(param_set, ids, check_dependencies = FALSE) {
  .Call(
    native_subset_symbol(),
    param_set$.__enclos_env__$private,
    param_set,
    ids,
    check_dependencies
  )
}

# Frozen copy of the pre-native R implementation. This intentionally retains
# data.table's joins and the public active-binding assignments, so randomized
# tests compare the new path with an independent implementation.
native_subset_reference = function(param_set, ids,
    allow_dangling_dependencies = FALSE, keep_constraint = TRUE) {
  private = param_set$.__enclos_env__$private
  checkmate::assert_subset(ids, private$.params$id)
  deps = param_set$deps
  if (!allow_dangling_dependencies && nrow(deps)) {
    parents = unique(deps[ids, on, on = "id", nomatch = NULL])
    missing = setdiff(parents, ids)
    if (length(missing)) stop("reference dependency failure")
  }

  result = ParamSet$new()
  result_private = result$.__enclos_env__$private
  result_private$.params = data.table::setindexv(
    private$.params[ids, on = "id"],
    c("id", "cls", "grouping")
  )
  result_private$.trafos = data.table::setkeyv(
    private$.trafos[ids, on = "id", nomatch = NULL],
    "id"
  )
  result_private$.tags = data.table::setkeyv(
    private$.tags[ids, on = "id", nomatch = NULL],
    "id"
  )
  result$assert_values = FALSE
  result$deps = deps[ids, on = "id", nomatch = NULL]
  if (keep_constraint) result$constraint = param_set$constraint
  result$extra_trafo = param_set$extra_trafo
  values = param_set$values
  result$values = values[match(ids, names(values), nomatch = 0L)]
  result$assert_values = TRUE
  result
}

native_subset_expect_table = function(observed, expected) {
  expect_identical(names(observed), names(expected))
  expect_identical(class(observed), class(expected))
  expect_identical(nrow(observed), nrow(expected))
  expect_identical(lapply(observed, identity), lapply(expected, identity))
  expect_identical(data.table::key(observed), data.table::key(expected))
  expect_identical(data.table::indices(observed), data.table::indices(expected))
  expect_identical(data.table:::selfrefok(observed, FALSE), 1L)
}

native_subset_expect_equivalent = function(observed, expected) {
  expect_identical(class(observed), c("ParamSet", "R6"))
  expect_identical(class(observed), class(expected))
  observed_private = observed$.__enclos_env__$private
  expected_private = expected$.__enclos_env__$private
  for (field in c(".params", ".tags", ".trafos", ".deps")) {
    native_subset_expect_table(
      observed_private[[field]],
      expected_private[[field]]
    )
  }
  expect_identical(observed_private$.values, expected_private$.values)
  expect_identical(observed$assert_values, expected$assert_values)
  expect_identical(observed$constraint, expected$constraint)
  expect_identical(observed$extra_trafo, expected$extra_trafo)
}

native_subset_rich_set = function() {
  marker = new.env(parent = emptyenv())
  transform = function(x) exp(x)
  result = ps(
    zeta = p_dbl(
      -2,
      2,
      tags = c("numeric", "shared"),
      trafo = transform
    ),
    alpha = p_int(
      -3,
      3,
      tags = c("control", "shared"),
      special_vals = list(-99L)
    ),
    mode = p_fct(c("small", "large"), tags = "choice"),
    flag = p_lgl(tags = character()),
    payload = p_uty(custom_check = function(x) TRUE)
  )
  result$values = list(
    zeta = 0.5,
    alpha = 2L,
    mode = "large",
    flag = TRUE,
    payload = marker
  )
  result$add_dep("mode", "alpha", CondAnyOf(c(-1L, 2L)))
  result$add_dep("mode", "flag", CondEqual(TRUE))
  result$add_dep("zeta", "alpha", CondEqual(2L))
  result$constraint = function(x) TRUE
  result$extra_trafo = function(x, param_set) x
  result
}

native_subset_collection_fixture = function(postfix = FALSE,
    nested = FALSE) {
  left = ps(
    enabled = p_lgl(tags = "switch", init = TRUE),
    amount = p_int(0, 10, tags = c("numeric", "shared"), init = 2L)
  )
  left$add_dep("amount", "enabled", CondEqual(TRUE))
  right = ps(
    score = p_dbl(0, 1, tags = c("numeric", "shared"), trafo = exp),
    mode = p_fct(c("small", "large"), tags = "choice", init = "large")
  )
  right$values$score = 0.5
  collection = ParamSetCollection$new(
    list(left = left, right = right),
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = postfix
  )
  left_enabled = if (postfix) "enabled.left" else "left.enabled"
  right_mode = if (postfix) "mode.right" else "right.mode"
  collection$add_dep(right_mode, left_enabled, CondEqual(TRUE))
  if (!nested) return(collection)
  ParamSetCollection$new(list(
    layer = collection,
    spare = ps(flag = p_lgl(tags = "spare", init = FALSE))
  ))
}

native_subset_expect_collection_equivalent = function(observed, expected) {
  expect_identical(class(observed), c("ParamSet", "R6"))
  observed_private = observed$.__enclos_env__$private
  expected_private = expected$.__enclos_env__$private
  for (field in c(".params", ".tags", ".trafos")) {
    native_subset_expect_table(
      observed_private[[field]],
      expected_private[[field]]
    )
  }
  expect_identical(names(observed_private$.deps), names(expected_private$.deps))
  expect_identical(class(observed_private$.deps), class(expected_private$.deps))
  expect_identical(observed_private$.deps$id, expected_private$.deps$id)
  expect_identical(observed_private$.deps$on, expected_private$.deps$on)
  expect_identical(
    lapply(observed_private$.deps$cond, class),
    lapply(expected_private$.deps$cond, class)
  )
  expect_identical(
    lapply(observed_private$.deps$cond, function(condition) condition$rhs),
    lapply(expected_private$.deps$cond, function(condition) condition$rhs)
  )
  expect_identical(
    data.table:::selfrefok(observed_private$.deps, FALSE),
    1L
  )
  expect_identical(observed_private$.values, expected_private$.values)
  expect_identical(observed$ids(), expected$ids())
  expect_identical(observed$class, expected$class)
  expect_identical(observed$storage_type, expected$storage_type)
}

native_subset_replace = function(table, column, value) {
  result = unclass(table)
  result[[column]] = value
  attributes(result) = attributes(table)
  result
}

native_subset_with_private = function(param_set, name, value, ids = "x") {
  private = param_set$.__enclos_env__$private
  original = private[[name]]
  on.exit(private[[name]] <- original)
  private[[name]] = value
  native_subset_call(param_set, ids)
}

test_that("native subset routines are registered with forced symbols", {
  skip_if_not(native_subset_available())
  subset_symbol = native_subset_symbol()
  adopt_symbol = native_subset_adopt_symbol()
  expect_s3_class(subset_symbol, "NativeSymbolInfo")
  expect_s3_class(adopt_symbol, "NativeSymbolInfo")
  expect_identical(subset_symbol$numParameters, 4L)
  expect_identical(adopt_symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_subset_state", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
  expect_identical(
    names(formals(ParamSet$public_methods$initialize)),
    c("params", "allow_dangling_dependencies")
  )
})

test_that("native subset admission rejects ALTREP arguments before Length", {
  skip_if_not(native_subset_available())
  param_set = ps(x = p_int())
  callbacks = 0L
  stateful = function(value) {
    native_stateful_altrep(
      value,
      value,
      callback = function() {
        callbacks <<- callbacks + 1L
        stop("subset admission invoked ALTREP Length", call. = FALSE)
      },
      callback_after = c(NA_integer_, 0L)
    )
  }

  expect_null(native_subset_call(param_set, stateful("x"), FALSE))
  expect_null(native_subset_call(param_set, "x", stateful(FALSE)))
  expect_identical(callbacks, 0L)
})

test_that("native subset matches the frozen rich R implementation", {
  skip_if_not(native_subset_available())
  param_set = native_subset_rich_set()
  selections = list(
    c("payload", "alpha", "zeta"),
    c("zeta", "zeta", "mode", "alpha", "mode", "flag"),
    character(),
    param_set$ids()
  )

  for (ids in selections) {
    observed = param_set$subset(ids, allow_dangling_dependencies = TRUE)
    expected = native_subset_reference(
      param_set,
      ids,
      allow_dangling_dependencies = TRUE
    )
    native_subset_expect_equivalent(observed, expected)
    expect_identical(observed$ids(), unname(ids))
    expect_identical(
      data.table::indices(observed$.__enclos_env__$private$.params),
      "id__cls__grouping"
    )
  }

  without_constraint = param_set$subset(
    c("alpha", "flag"),
    allow_dangling_dependencies = TRUE,
    keep_constraint = FALSE
  )
  expect_null(without_constraint$constraint)
  expect_identical(without_constraint$extra_trafo, param_set$extra_trafo)

  empty = param_set$subset(character())
  empty_private = empty$.__enclos_env__$private
  for (table in mget(
      c(".params", ".tags", ".trafos", ".deps"),
      envir = empty_private,
      inherits = FALSE
    )) {
    expect_identical(attr(table, "row.names"), integer())
    expect_identical(data.table:::selfrefok(table, FALSE), 1L)
  }
})

test_that("native subset transfers exact flat, postfix, and nested collections", {
  skip_if_not(native_subset_available())
  fixtures = list(
    prefix = list(
      collection = native_subset_collection_fixture(),
      selections = list(
        c("right.mode", "left.enabled", "right.score"),
        c("right.mode", "right.mode", "left.enabled"),
        character()
      )
    ),
    postfix = list(
      collection = native_subset_collection_fixture(postfix = TRUE),
      selections = list(
        c("mode.right", "enabled.left", "score.right"),
        c("score.right", "score.right"),
        character()
      )
    ),
    nested = list(
      collection = native_subset_collection_fixture(nested = TRUE),
      selections = list(
        c("layer.right.mode", "layer.left.enabled", "spare.flag"),
        c("spare.flag", "layer.right.score", "layer.right.score"),
        character()
      )
    )
  )

  for (fixture in fixtures) {
    collection = fixture$collection
    for (ids in fixture$selections) {
      plan = native_subset_call(collection, ids)
      expect_type(plan$state, "externalptr")
      expect_identical(plan$missing_parents, character())

      observed = collection$subset(
        ids,
        allow_dangling_dependencies = TRUE
      )
      expected = native_subset_reference(
        collection,
        ids,
        allow_dangling_dependencies = TRUE
      )
      native_subset_expect_collection_equivalent(observed, expected)
      expect_identical(observed$ids(), unname(ids))
    }
  }
})

test_that("native collection subset preserves dependency refusal and detached callbacks", {
  skip_if_not(native_subset_available())
  child = ps(
    x = p_dbl(0, 10, init = 1),
    enabled = p_lgl(init = TRUE)
  )
  child$add_dep("x", "enabled", CondEqual(TRUE))
  child$extra_trafo = function(x) {
    x$x = x$x + 1
    x
  }
  child$constraint = function(x) is.null(x$x) || x$x <= 5
  collection = psc(component = child)

  refused = native_subset_call(collection, "component.x", TRUE)
  expect_identical(refused$missing_parents, "component.enabled")
  expect_null(refused$state)
  expect_error(
    collection$subset(
      "component.x",
      allow_dangling_dependencies = FALSE,
      keep_constraint = stop("keep_constraint was forced")
    ),
    "dependencies on params exist"
  )

  plan = native_subset_call(
    collection,
    c("component.x", "component.enabled")
  )
  expect_type(plan$state, "externalptr")
  observed = collection$subset(
    c("component.x", "component.enabled"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    observed$trafo(list(component.x = 1))$component.x,
    2
  )
  expect_true(observed$test_constraint(list(component.x = 4)))
  expect_false(observed$test_constraint(list(component.x = 8)))

  child$extra_trafo = function(x) {
    x$x = x$x + 100
    x
  }
  child$constraint = function(x) FALSE
  expect_identical(
    observed$trafo(list(component.x = 1))$component.x,
    2
  )
  expect_true(observed$test_constraint(list(component.x = 4)))

  without_constraint = collection$subset(
    c("component.x", "component.enabled"),
    allow_dangling_dependencies = TRUE,
    keep_constraint = FALSE
  )
  expect_null(without_constraint$constraint)
  expect_false(is.null(without_constraint$extra_trafo))
})

test_that("collection extensions fail closed before native subset callbacks", {
  skip_if_not(native_subset_available())
  events = new.env(parent = emptyenv())
  events$values = 0L
  events$constraints = 0L
  events$trafos = 0L
  CountingChild = R6::R6Class(
    "NativeSubsetCollectionCountingChild",
    inherit = ParamSet,
    active = list(
      values = function(value) {
        if (!missing(value)) {
          super$values <- value
          return(value)
        }
        events$values = events$values + 1L
        super$values
      },
      constraint = function(value) {
        if (!missing(value)) {
          super$constraint <- value
          return(value)
        }
        events$constraints = events$constraints + 1L
        super$constraint
      },
      extra_trafo = function(value) {
        if (!missing(value)) {
          super$extra_trafo <- value
          return(value)
        }
        events$trafos = events$trafos + 1L
        super$extra_trafo
      }
    )
  )
  child = CountingChild$new(list(x = p_int(0, 2, init = 1L)))
  child$constraint = function(x) TRUE
  child$extra_trafo = function(x) x
  collection = psc(component = child)
  events$constraints = 0L
  events$trafos = 0L

  expect_null(native_subset_call(collection, "component.x"))
  expect_identical(events$values, 0L)
  expect_identical(events$constraints, 0L)
  expect_identical(events$trafos, 0L)
  observed = collection$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )
  expect_identical(observed$values, list(component.x = 1L))
  expect_identical(events$values, 1L)
  expect_identical(events$constraints, 2L)
  expect_identical(events$trafos, 2L)

  CountingCollection = R6::R6Class(
    "NativeSubsetCollectionSubclass",
    inherit = ParamSetCollection
  )
  subclass = CountingCollection$new(list(component = ps(x = p_int())))
  expect_null(native_subset_call(subclass, "component.x"))
  expect_identical(
    subclass$subset(
      "component.x",
      allow_dangling_dependencies = TRUE
    )$ids(),
    "component.x"
  )
})

test_that("collection feature discovery authenticates generated bindings", {
  skip_if_not(native_subset_available())
  child = ps(x = p_int(0, 2, init = 1L))
  child$constraint = function(x) is.null(x$x) || x$x <= 1L
  child$extra_trafo = function(x) {
    x$x = x$x + 1L
    x
  }
  collection = psc(component = child)
  feature_indices = getFromNamespace(
    "param_set_collection_exact_feature_indices",
    "paradox"
  )
  expect_identical(feature_indices(list(child), ".constraint"), 1L)
  expect_identical(feature_indices(list(child), ".extra_trafo"), 1L)

  for (copy in list(
      collection$clone(deep = TRUE),
      unserialize(serialize(collection, NULL, version = 3L))
    )) {
    copy_sets = copy$.__enclos_env__$private$.sets
    expect_identical(feature_indices(copy_sets, ".constraint"), 1L)
    expect_identical(feature_indices(copy_sets, ".extra_trafo"), 1L)
    observed = copy$subset("component.x", allow_dangling_dependencies = TRUE)
    expect_true(observed$test_constraint(list(component.x = 1L)))
    expect_false(observed$test_constraint(list(component.x = 2L)))
    expect_identical(
      observed$trafo(list(component.x = 1L))$component.x,
      2L
    )
  }

  untouched = new.env(parent = emptyenv())
  untouched$callbacks = 0L
  delayed_enclosure_child = ps(x = p_int())
  original_enclosure = get(
    ".__enclos_env__",
    envir = delayed_enclosure_child,
    inherits = FALSE
  )
  delayedAssign(
    ".__enclos_env__",
    {
      untouched$callbacks = untouched$callbacks + 1L
      original_enclosure
    },
    assign.env = delayed_enclosure_child
  )
  expect_null(feature_indices(
    list(delayed_enclosure_child),
    ".constraint"
  ))
  expect_identical(untouched$callbacks, 0L)

  active_private_child = ps(x = p_int())
  active_private_enclosure = get(
    ".__enclos_env__",
    envir = active_private_child,
    inherits = FALSE
  )
  original_private = get(
    "private",
    envir = active_private_enclosure,
    inherits = FALSE
  )
  rm(list = "private", envir = active_private_enclosure)
  makeActiveBinding("private", function(value) {
    untouched$callbacks = untouched$callbacks + 1L
    original_private
  }, active_private_enclosure)
  expect_null(feature_indices(list(active_private_child), ".constraint"))
  expect_identical(untouched$callbacks, 0L)

  shadowed_method_child = ps(x = p_int())
  shadowed_enclosure = get(
    ".__enclos_env__",
    envir = shadowed_method_child,
    inherits = FALSE
  )
  assign(
    ".__ParamSet__constraint",
    function(...) {
      untouched$callbacks = untouched$callbacks + 1L
      NULL
    },
    envir = shadowed_enclosure
  )
  expect_null(feature_indices(list(shadowed_method_child), ".constraint"))
  expect_identical(untouched$callbacks, 0L)

  delayed_constraint_child = ps(x = p_int())
  delayed_constraint_private = delayed_constraint_child$.__enclos_env__$private
  delayedAssign(
    ".constraint",
    {
      untouched$callbacks = untouched$callbacks + 1L
      function(x) TRUE
    },
    assign.env = delayed_constraint_private
  )
  expect_null(feature_indices(
    list(delayed_constraint_child),
    ".constraint"
  ))
  expect_identical(untouched$callbacks, 0L)

  delayed_sets_child = psc(inner = ps(x = p_int()))
  delayed_sets_private = delayed_sets_child$.__enclos_env__$private
  original_sets = delayed_sets_private$.sets
  delayedAssign(
    ".sets",
    {
      untouched$callbacks = untouched$callbacks + 1L
      original_sets
    },
    assign.env = delayed_sets_private
  )
  expect_null(feature_indices(list(delayed_sets_child), ".constraint"))
  expect_identical(untouched$callbacks, 0L)

  events = new.env(parent = emptyenv())
  events$constraints = 0L
  events$trafos = 0L
  child_private = child$.__enclos_env__$private
  original_constraint = activeBindingFunction("constraint", child)
  original_extra_trafo = activeBindingFunction("extra_trafo", child)
  on.exit({
    makeActiveBinding("constraint", original_constraint, child)
    makeActiveBinding("extra_trafo", original_extra_trafo, child)
  })
  makeActiveBinding("constraint", function(value) {
    if (!missing(value)) stop("replacement constraint is read-only")
    events$constraints = events$constraints + 1L
    child_private$.constraint
  }, child)
  makeActiveBinding("extra_trafo", function(value) {
    if (!missing(value)) stop("replacement extra_trafo is read-only")
    events$trafos = events$trafos + 1L
    child_private$.extra_trafo
  }, child)

  expect_null(feature_indices(list(child), ".constraint"))
  expect_null(feature_indices(list(child), ".extra_trafo"))
  expect_false(is.null(native_subset_call(collection, "component.x")))
  expect_identical(events$constraints, 0L)
  expect_identical(events$trafos, 0L)

  observed = collection$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )
  expect_identical(events$constraints, 2L)
  expect_identical(events$trafos, 2L)
  expect_true(observed$test_constraint(list(component.x = 1L)))
  expect_false(observed$test_constraint(list(component.x = 2L)))
  expect_identical(observed$trafo(list(component.x = 1L))$component.x, 2L)
})

test_that("base subset authenticates deps before values without replay", {
  skip_if_not(native_subset_available())
  param_set = ps(parent = p_lgl(init = TRUE), x = p_int(0, 2, init = 1L))
  param_set$add_dep("x", "parent", CondEqual(TRUE))
  events = new.env(parent = emptyenv())
  events$seen = character()
  original_deps = activeBindingFunction("deps", param_set)
  original_values = activeBindingFunction("values", param_set)
  makeActiveBinding("deps", function(value) {
    if (missing(value)) {
      events$seen = c(events$seen, "deps")
      return(original_deps())
    }
    original_deps(value)
  }, param_set)
  makeActiveBinding("values", function(xs) {
    if (missing(xs)) {
      events$seen = c(events$seen, "values")
      return(original_values())
    }
    original_values(xs)
  }, param_set)

  expect_null(native_subset_call(param_set, c("parent", "x")))
  expect_identical(events$seen, character())
  observed = param_set$subset(
    c("parent", "x"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(observed$ids(), c("parent", "x"))
  expect_identical(events$seen, c("deps", "values"))

  values_only = ps(x = p_int(0, 2, init = 1L))
  events$values = 0L
  original_values = activeBindingFunction("values", values_only)
  makeActiveBinding("values", function(xs) {
    if (missing(xs)) {
      events$values = events$values + 1L
      return(original_values())
    }
    original_values(xs)
  }, values_only)
  expect_null(native_subset_call(values_only, "x"))
  expect_identical(events$values, 0L)
  expect_identical(
    values_only$subset("x", allow_dangling_dependencies = TRUE)$ids(),
    "x"
  )
  expect_identical(events$values, 1L)
})

test_that("attributed and differently encoded IDs fail closed to R joins", {
  skip_if_not(native_subset_available())
  param_set = ps(a = p_int(0, 2, tags = c("one", "two")), b = p_lgl())
  attributed = list(
    structure(c("b", "a"), names = c("right", "left")),
    structure(c("b", "a"), class = "subset_ids"),
    structure(c("b", "a"), marker = 42L),
    structure(
      c("b", "a"),
      names = c("right", "left"),
      class = "subset_ids",
      marker = new.env(parent = emptyenv())
    )
  )

  for (ids in attributed) {
    expect_null(native_subset_call(param_set, ids))
    observed = param_set$subset(ids, allow_dangling_dependencies = TRUE)
    expected = native_subset_reference(
      param_set,
      ids,
      allow_dangling_dependencies = TRUE
    )
    native_subset_expect_equivalent(observed, expected)
    expect_identical(
      attributes(observed$.__enclos_env__$private$.params$id),
      attributes(expected$.__enclos_env__$private$.params$id)
    )
  }

  events = character()
  observed = param_set$subset(
    {
      events = c(events, "ids")
      structure("a", marker = 1L)
    },
    {
      events = c(events, "allow")
      TRUE
    },
    {
      events = c(events, "keep")
      FALSE
    }
  )
  expect_identical(as.vector(observed$ids()), "a")
  expect_identical(attr(observed$ids(), "marker"), 1L)
  expect_identical(events, c("ids", "allow", "keep"))

  events = character()
  expect_error(
    param_set$subset(
      {
        events = c(events, "ids")
        structure("unknown", marker = 1L)
      },
      {
        events = c(events, "allow")
        TRUE
      },
      {
        events = c(events, "keep")
        FALSE
      }
    ),
    "Must be a subset of"
  )
  expect_identical(events, "ids")
})

test_that("native subset removes data.table's duplicate-tag cartesian limit", {
  skip_if_not(native_subset_available())
  previous = getOption("datatable.allow.cartesian")
  on.exit(options(datatable.allow.cartesian = previous), add = TRUE)
  options(datatable.allow.cartesian = FALSE)

  param_set = ps(
    repeated = p_dbl(
      0,
      1,
      tags = c("first", "second", "third"),
      trafo = sqrt
    )
  )
  param_set$values = list(repeated = 0.25)
  ids = rep("repeated", 5L)

  expect_error(
    native_subset_reference(
      param_set,
      ids,
      allow_dangling_dependencies = TRUE
    ),
    "Join results in 15 rows"
  )
  observed = param_set$subset(ids, allow_dangling_dependencies = TRUE)
  private = observed$.__enclos_env__$private
  expect_identical(observed$ids(), ids)
  expect_identical(names(observed$values), ids)
  expect_identical(nrow(private$.tags), 15L)
  expect_identical(
    private$.tags$tag,
    rep(c("first", "second", "third"), 5L)
  )
  expect_identical(private$.trafos$id, ids)
  expect_true(all(vapply(
    mget(
      c(".params", ".tags", ".trafos", ".deps"),
      envir = private,
      inherits = FALSE
    ),
    function(table) data.table:::selfrefok(table, FALSE) == 1L,
    logical(1L)
  )))
})

test_that("native subset preserves dependency order and R diagnostics", {
  skip_if_not(native_subset_available())
  param_set = ps(
    a = p_int(-1, 1),
    b = p_int(-1, 1),
    c = p_int(-1, 1),
    d = p_int(-1, 1)
  )
  first = CondEqual(0L)
  second = CondAnyOf(c(-1L, 1L))
  third = CondEqual(1L)
  param_set$add_dep("c", "b", first)
  param_set$add_dep("c", "a", second)
  param_set$add_dep("d", "b", third)

  plan = native_subset_call(param_set, c("c", "d"), TRUE)
  expect_identical(plan$missing_parents, c("b", "a"))
  expect_null(plan$state)

  condition = tryCatch(
    param_set$subset(c("c", "d")),
    error = identity
  )
  expect_s3_class(condition, "Mlr3Error")
  expect_identical(
    conditionMessage(condition),
    paste0(
      "Subsetting so that dependencies on params exist which would be gone: b, a.",
      "\nIf you still want to subset, set allow_dangling_dependencies to TRUE."
    )
  )

  dangling = param_set$subset(
    c("d", "c", "c"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(dangling$deps$id, c("d", "c", "c", "c", "c"))
  expect_identical(dangling$deps$on, c("b", "b", "a", "b", "a"))
  expect_identical(
    dangling$deps$cond,
    list(third, first, second, first, second)
  )
  expect_identical(
    param_set$subset("a", allow_dangling_dependencies = TRUE)$deps$id,
    character()
  )
})

test_that("subset state tokens are authenticated, fresh-only, and single-use", {
  skip_if_not(native_subset_available())
  param_set = native_subset_rich_set()
  plan = native_subset_call(param_set, c("mode", "alpha", "mode"))
  expect_type(plan$state, "externalptr")

  initialized = ParamSet$new()
  expect_false(.Call(
    native_subset_adopt_symbol(),
    initialized$.__enclos_env__$private,
    plan$state
  ))

  adopted = ParamSet$new(plan$state)
  expect_identical(adopted$ids(), c("mode", "alpha", "mode"))
  expect_identical(adopted$values, list(
    mode = "large",
    alpha = 2L,
    mode = "large"
  ))

  another = ParamSet$new()
  expect_false(.Call(
    native_subset_adopt_symbol(),
    another$.__enclos_env__$private,
    plan$state
  ))
  expect_error(
    ParamSet$new(plan$state),
    "Must be of type 'list', not 'externalptr'",
    fixed = TRUE
  )

  foreign = methods::new("externalptr")
  expect_false(.Call(
    native_subset_adopt_symbol(),
    another$.__enclos_env__$private,
    foreign
  ))
  expect_error(
    ParamSet$new(foreign),
    "Must be of type 'list', not 'externalptr'",
    fixed = TRUE
  )
})

test_that("native subset owns outer state and shares nested payloads", {
  skip_if_not(native_subset_available())
  param_set = native_subset_rich_set()
  source = param_set$.__enclos_env__$private
  result = param_set$subset(
    c("zeta", "alpha", "mode", "payload"),
    allow_dangling_dependencies = TRUE
  )
  target = result$.__enclos_env__$private

  expect_false(identical(target$.params, source$.params))
  expect_false(identical(target$.tags, source$.tags))
  expect_false(identical(target$.trafos, source$.trafos))
  expect_false(identical(target$.deps, source$.deps))
  expect_false(identical(target$.values, source$.values))
  expect_identical(target$.trafos$trafo[[1L]], source$.trafos$trafo[[1L]])
  expect_identical(target$.deps$cond[[1L]], source$.deps$cond[[3L]])
  expect_identical(target$.values$payload, source$.values$payload)

  data.table::set(target$.params, i = 1L, j = "lower", value = -100)
  expect_identical(source$.params$lower[[1L]], -2)
  data.table::setnames(target$.params, "id", "renamed")
  expect_identical(names(source$.params)[[1L]], "id")
  data.table::set(target$.tags, i = 1L, j = "tag", value = "changed")
  expect_false("changed" %in% source$.tags$tag)
  target$.values$payload = new.env(parent = emptyenv())
  expect_false(identical(target$.values$payload, source$.values$payload))

  clean = param_set$subset(
    c("zeta", "alpha", "mode", "payload"),
    allow_dangling_dependencies = TRUE
  )
  restored = unserialize(serialize(clean, NULL, version = 3L))
  cloned = clean$clone(deep = TRUE)
  for (copy in list(restored, cloned)) {
    expect_identical(copy$ids(), clean$ids())
    expect_true(copy$test(list(
      zeta = 0,
      alpha = 1L,
      mode = "small",
      payload = NULL
    ), check_strict = FALSE))
    expect_identical(
      copy$subset(c("alpha", "mode"),
        allow_dangling_dependencies = TRUE)$ids(),
      c("alpha", "mode")
    )
  }
})

test_that("extension storage and subclasses retain the R fallback", {
  skip_if_not(native_subset_available())
  make_custom_domain = function() {
    paradox:::Domain(
      cls = "ParamSubsetExtension",
      grouping = "ParamSubsetExtension",
      storage_type = "list"
    )
  }
  custom = make_custom_domain()
  extension_set = ParamSet$new(list(
    builtin = p_int(0, 2),
    custom = custom
  ))
  expect_null(native_subset_call(extension_set, "custom"))
  extension_subset = extension_set$subset(
    "custom",
    allow_dangling_dependencies = TRUE
  )
  expect_identical(extension_subset$ids(), "custom")
  expect_identical(extension_subset$class, c(custom = "ParamSubsetExtension"))

  events = new.env(parent = emptyenv())
  events$seen = character()
  CountingSubset = R6::R6Class(
    "ParamSetCountingSubset",
    inherit = ParamSet,
    active = list(
      values = function(xs) {
        if (!missing(xs)) {
          super$values <- xs
          return(xs)
        }
        events$seen = c(events$seen, "values")
        super$values
      },
      deps = function(value) {
        if (!missing(value)) {
          super$deps <- value
          return(value)
        }
        events$seen = c(events$seen, "deps")
        super$deps
      },
      constraint = function(value) {
        if (!missing(value)) {
          super$constraint <- value
          return(value)
        }
        events$seen = c(events$seen, "constraint")
        super$constraint
      },
      extra_trafo = function(value) {
        if (!missing(value)) {
          super$extra_trafo <- value
          return(value)
        }
        events$seen = c(events$seen, "extra_trafo")
        super$extra_trafo
      }
    )
  )
  subclass = CountingSubset$new(list(x = p_dbl(0, 1), y = p_lgl()))
  expect_null(native_subset_call(subclass, "x"))
  events$seen = character()
  observed = subclass$subset("x", allow_dangling_dependencies = TRUE)
  expect_identical(class(observed), c("ParamSet", "R6"))
  expect_identical(
    events$seen,
    c("deps", "constraint", "extra_trafo", "values")
  )
})

test_that("native subset rejects malformed private stores atomically", {
  skip_if_not(native_subset_available())
  param_set = ps(
    x = p_dbl(0, 1, tags = "tag", trafo = exp),
    y = p_int(0, 2, init = 1L)
  )
  private = param_set$.__enclos_env__$private
  params = data.table::copy(private$.params)
  tags = data.table::copy(private$.tags)
  trafos = data.table::copy(private$.trafos)

  expect_null(native_subset_with_private(param_set, ".params", 1L))
  expect_null(native_subset_with_private(
    param_set,
    ".params",
    native_subset_replace(params, "cls", c("Unknown", "ParamInt"))
  ))

  unkeyed_tags = data.table::copy(tags)
  data.table::setattr(unkeyed_tags, "sorted", NULL)
  expect_null(native_subset_with_private(param_set, ".tags", unkeyed_tags))
  foreign_tags = data.table::rbindlist(list(
    tags,
    data.table::data.table(id = "ghost", tag = "foreign")
  ))
  data.table::setkeyv(foreign_tags, "id")
  expect_null(native_subset_with_private(param_set, ".tags", foreign_tags))

  duplicate_trafos = data.table::rbindlist(list(trafos, trafos))
  data.table::setkeyv(duplicate_trafos, "id")
  expect_null(native_subset_with_private(
    param_set,
    ".trafos",
    duplicate_trafos
  ))
  foreign_deps = data.table::data.table(
    id = "ghost",
    on = "x",
    cond = list(CondEqual(0))
  )
  expect_null(native_subset_with_private(param_set, ".deps", foreign_deps))
  duplicate_values = structure(list(1L, 2L), names = c("y", "y"))
  expect_null(native_subset_with_private(
    param_set,
    ".values",
    duplicate_values
  ))
  foreign_values = structure(list(1L), names = "ghost")
  expect_null(native_subset_with_private(
    param_set,
    ".values",
    foreign_values
  ))
  expect_null(native_subset_call(param_set, NULL))
  expect_null(.Call(
    native_subset_symbol(),
    private,
    new.env(parent = emptyenv()),
    "x",
    FALSE
  ))
  expect_error(param_set$subset(NULL))
})

test_that("native subset preserves encoded matches", {
  skip_if_not(native_subset_available())
  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  Encoding(utf8) = "UTF-8"
  Encoding(latin1) = "latin1"
  skip_if(identical(charToRaw(utf8), charToRaw(latin1)))

  param_set = ps(x = p_dbl(0, 1, tags = "encoded", trafo = exp), y = p_lgl())
  private = param_set$.__enclos_env__$private
  data.table::set(private$.params, i = 1L, j = "id", value = utf8)
  private$.tags = data.table::data.table(
    id = latin1,
    tag = "encoded",
    key = "id"
  )
  private$.trafos = data.table::data.table(
    id = latin1,
    trafo = list(exp),
    key = "id"
  )
  private$.values = setNames(list(0.5), latin1)

  expect_null(native_subset_call(param_set, latin1))
  observed = param_set$subset(latin1, allow_dangling_dependencies = TRUE)
  expected = native_subset_reference(
    param_set,
    latin1,
    allow_dangling_dependencies = TRUE
  )
  native_subset_expect_equivalent(observed, expected)
  expect_identical(Encoding(observed$ids()), "latin1")
  expect_identical(enc2utf8(observed$ids()), enc2utf8(utf8))
  expect_identical(enc2utf8(names(observed$values)), enc2utf8(latin1))
  expect_identical(enc2utf8(observed$.__enclos_env__$private$.tags$id), enc2utf8(latin1))
  expect_identical(observed$.__enclos_env__$private$.trafos$trafo[[1L]], exp)
})

test_that("native subset matches randomized canonical R joins", {
  skip_if_not(native_subset_available())
  set.seed(20260714)
  for (iteration in seq_len(40L)) {
    size = sample.int(16L, 1L)
    ids = sprintf("p%02d", sample.int(99L, size))
    domains = lapply(seq_len(size), function(index) {
      tags = if (runif(1L) < 0.6) {
        sample(c("red", "blue", "shared"), 1L)
      } else {
        character()
      }
      p_int(
        -2,
        2,
        tags = tags,
        trafo = if (runif(1L) < 0.3) identity else NULL
      )
    })
    names(domains) = ids
    param_set = ParamSet$new(domains)
    values = as.list(sample(-2:2, size, replace = TRUE))
    names(values) = ids
    param_set$values = values

    if (size > 1L) {
      for (child in 2:size) {
        if (runif(1L) < 0.45) {
          parent = sample.int(child - 1L, 1L)
          param_set$add_dep(ids[[child]], ids[[parent]], CondEqual(0L))
        }
      }
    }

    requested = if (runif(1L) < 0.15) {
      character()
    } else {
      sample(ids, sample.int(2L * size, 1L), replace = TRUE)
    }
    observed = param_set$subset(
      requested,
      allow_dangling_dependencies = TRUE
    )
    expected = native_subset_reference(
      param_set,
      requested,
      allow_dangling_dependencies = TRUE
    )
    native_subset_expect_equivalent(observed, expected)
  }
})

test_that("subset preserves lazy flag forcing and dependency short-circuiting", {
  skip_if_not(native_subset_available())
  param_set = ps(x = p_int(0, 1), y = p_int(0, 1))
  events = character()
  observed = param_set$subset(
    {
      events = c(events, "ids")
      "x"
    },
    {
      events = c(events, "allow")
      TRUE
    },
    {
      events = c(events, "keep")
      FALSE
    }
  )
  expect_identical(observed$ids(), "x")
  expect_identical(events, c("ids", "allow", "keep"))

  param_set$add_dep("y", "x", CondEqual(1L))
  expect_error(
    param_set$subset(
      "y",
      FALSE,
      stop("keep_constraint was forced")
    ),
    "dependencies on params exist"
  )
})

test_that("native subset uses one cumulative interrupt budget", {
  skip_if_not(native_subset_available())
  size = 4000L
  ids = sprintf("parameter_%05d", seq_len(size))
  params = data.table::data.table(
    id = ids,
    cls = rep("ParamInt", size),
    grouping = rep("ParamInt", size),
    cargo = rep(list(NULL), size),
    lower = rep(0, size),
    upper = rep(10, size),
    tolerance = rep(0, size),
    levels = rep(list(NULL), size),
    special_vals = rep(list(list()), size),
    default = rep(list(paradox:::NO_DEF), size),
    storage_type = rep("integer", size)
  )
  tags = data.table::data.table(id = ids, tag = rep("bulk", size))
  data.table::setkeyv(tags, "id")
  trafos = data.table::data.table(id = character(), trafo = list())
  data.table::setkeyv(trafos, "id")
  deps = data.table::data.table(
    id = ids,
    on = rep(ids[[1L]], size),
    cond = rep(list(CondEqual(0L)), size)
  )

  param_set = ParamSet$new()
  private = param_set$.__enclos_env__$private
  private$.params = params
  private$.tags = tags
  private$.trafos = trafos
  private$.deps = deps
  private$.values = structure(list(), names = character())

  requested = c(rev(ids), ids[seq_len(100L)])
  observed = param_set$subset(
    requested,
    allow_dangling_dependencies = TRUE
  )
  expect_identical(observed$ids(), requested)
  expect_identical(observed$deps$id, requested)
  expect_identical(nrow(observed$.__enclos_env__$private$.tags), length(requested))
  expect_true(all(vapply(
    mget(
      c(".params", ".tags", ".trafos", ".deps"),
      envir = observed$.__enclos_env__$private,
      inherits = FALSE
    ),
    function(table) data.table:::selfrefok(table, FALSE) == 1L,
    logical(1L)
  )))
})
