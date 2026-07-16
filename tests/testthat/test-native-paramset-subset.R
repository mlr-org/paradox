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

native_subset_adopt_without_initializer = function(plan) {
  target = ParamSet$new()
  private = target$.__enclos_env__$private
  private$.params = NULL
  expect_true(.Call(native_subset_adopt_symbol(), private, plan$state))
  private
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

# Independent copy of the historical one-dimensional construction path.  It
# intentionally keeps the data.table joins and public assignments so the
# native-token lane is compared with behavior it does not implement itself.
native_subspaces_reference = function(param_set,
    ids = param_set$.__enclos_env__$private$.params$id) {
  private = param_set$.__enclos_env__$private
  values = param_set$values
  sapply(ids, simplify = FALSE, function(get_id) {
    result = ParamSet$new()
    result$extra_trafo = param_set$extra_trafo
    result_private = result$.__enclos_env__$private
    result_private$.params = data.table::setindexv(
      private$.params[get_id, on = "id"],
      c("id", "cls", "grouping")
    )
    result_private$.trafos = data.table::setkeyv(
      private$.trafos[get_id, on = "id", nomatch = NULL],
      "id"
    )
    result_private$.tags = data.table::setkeyv(
      private$.tags[get_id, on = "id", nomatch = NULL],
      "id"
    )
    result$assert_values = FALSE
    result$values = values[match(get_id, names(values), nomatch = 0L)]
    result$assert_values = TRUE
    result
  })
}

native_subspaces_expect_fallback_equivalent = function(observed, expected) {
  expect_identical(class(observed), class(expected))
  observed_private = observed$.__enclos_env__$private
  expected_private = expected$.__enclos_env__$private
  for (field in c(".params", ".tags", ".trafos", ".deps")) {
    expect_identical(names(observed_private[[field]]), names(expected_private[[field]]))
    expect_identical(class(observed_private[[field]]), class(expected_private[[field]]))
    expect_identical(
      lapply(observed_private[[field]], identity),
      lapply(expected_private[[field]], identity)
    )
    expect_identical(
      data.table::key(observed_private[[field]]),
      data.table::key(expected_private[[field]])
    )
    expect_identical(
      data.table::indices(observed_private[[field]]),
      data.table::indices(expected_private[[field]])
    )
  }
  expect_identical(observed_private$.values, expected_private$.values)
  expect_identical(observed$constraint, expected$constraint)
  expect_identical(observed$extra_trafo, expected$extra_trafo)
}

test_that("subspace mode adopts exact dependency-free one-row states", {
  skip_if_not(native_subset_available())
  param_set = ps(
    double = p_dbl(-2, 3, tags = c("numeric", "shared"), trafo = exp),
    integer = p_int(-3, 4, tags = "numeric"),
    factor = p_fct(c("slow", "fast"), tags = "categorical"),
    logical = p_lgl(tags = c("categorical", "shared"))
  )
  param_set$values = list(
    double = 0.5,
    integer = 2L,
    factor = "fast",
    logical = TRUE
  )
  param_set$constraint = function(x) FALSE
  param_set$extra_trafo = function(x) x

  plan = native_subset_call(param_set, "double", NA)
  expect_type(plan, "list")
  expect_identical(plan$missing_parents, character())
  expect_type(plan$state, "externalptr")
  direct = ParamSet$new(plan$state)
  expect_identical(direct$ids(), "double")
  expect_identical(direct$values, list(double = 0.5))
  expect_identical(nrow(direct$deps), 0L)

  requested = c("factor", "double", "double", "logical")
  observed = param_set$subspaces(requested)
  expected = native_subspaces_reference(param_set, requested)
  expect_identical(names(observed), requested)
  expect_length(observed, length(requested))
  for (index in seq_along(requested)) {
    native_subset_expect_equivalent(observed[[index]], expected[[index]])
    expect_identical(observed[[index]]$ids(), requested[[index]])
    expect_null(observed[[index]]$constraint)
    expect_identical(observed[[index]]$extra_trafo, param_set$extra_trafo)
    expect_identical(nrow(observed[[index]]$deps), 0L)
  }

  first_double = observed[[2L]]$.__enclos_env__$private
  second_double = observed[[3L]]$.__enclos_env__$private
  source = param_set$.__enclos_env__$private
  data.table::set(first_double$.params, 1L, "lower", -100)
  expect_identical(second_double$.params$lower, -2)
  expect_identical(source$.params[id == "double", lower], -2)
  observed[[2L]]$values = list(double = 1)
  expect_identical(observed[[3L]]$values, list(double = 0.5))
  expect_identical(param_set$values$double, 0.5)

  named_ids = c(second = "integer", first = "double")
  named_observed = param_set$subspaces(named_ids)
  named_expected = native_subspaces_reference(param_set, named_ids)
  expect_identical(names(named_observed), names(named_ids))
  for (index in seq_along(named_ids)) {
    native_subspaces_expect_fallback_equivalent(
      named_observed[[index]],
      named_expected[[index]]
    )
  }
  expect_identical(param_set$subspaces(character()), named_list())
})

test_that("subspace tokens carry canonical owned singleton indices", {
  skip_if_not(native_subset_available())
  param_set = ps(
    x = p_dbl(-2, 3, tags = c("numeric", "shared"), trafo = exp),
    y = p_int(-3, 4, tags = "numeric")
  )

  raw = native_subset_adopt_without_initializer(
    native_subset_call(param_set, "x", NA)
  )
  raw_index = attr(raw$.params, "index", exact = TRUE)
  raw_marker = attr(raw_index, "__id__cls__grouping", exact = TRUE)
  # Native synthesis is deliberately disabled for an unreviewed data.table
  # layout. When enabled, compare every private byte with data.table itself;
  # otherwise the public adopter below must install the index via setindexv().
  if (!is.null(raw_marker)) {
    reference = data.table::data.table(
      id = "x",
      cls = "ParamDbl",
      grouping = "ParamDbl"
    )
    data.table::setindexv(reference, c("id", "cls", "grouping"))
    expect_identical(
      raw_index,
      attr(reference, "index", exact = TRUE)
    )
  }

  observed = param_set$subspaces(c("x", "x", "y"))
  expected = native_subspaces_reference(param_set, c("x", "x", "y"))
  for (index in seq_along(observed)) {
    observed_params = observed[[index]]$.__enclos_env__$private$.params
    expected_params = expected[[index]]$.__enclos_env__$private$.params
    expect_identical(
      attr(observed_params, "index", exact = TRUE),
      attr(expected_params, "index", exact = TRUE)
    )
  }

  marker = function(table) {
    attr(
      attr(table, "index", exact = TRUE),
      "__id__cls__grouping",
      exact = TRUE
    )
  }
  first = observed[[1L]]$.__enclos_env__$private$.params
  second = observed[[2L]]$.__enclos_env__$private$.params
  source = param_set$.__enclos_env__$private$.params
  first_cache = marker(first)
  data.table::setattr(first_cache, "ownership_probe", 1L)
  expect_identical(attr(first_cache, "ownership_probe", exact = TRUE), 1L)
  expect_null(attr(marker(second), "ownership_probe", exact = TRUE))
  expect_null(attr(marker(source), "ownership_probe", exact = TRUE))
  data.table::setattr(first_cache, "ownership_probe", NULL)

  data.table::setindexv(first, NULL)
  expect_null(marker(first))
  expect_false(is.null(marker(second)))
  expect_false(is.null(marker(source)))
})

test_that("unsupported singleton index shapes retain the R setter", {
  skip_if_not(native_subset_available())
  param_set = ps(x = p_int(0, 2))
  private = param_set$.__enclos_env__$private
  encoded_id = enc2utf8("caf\u00e9")
  Encoding(encoded_id) = "UTF-8"
  data.table::set(private$.params, i = 1L, j = "id", value = encoded_id)

  plan = native_subset_call(param_set, private$.params$id, NA)
  expect_type(plan$state, "externalptr")
  raw = native_subset_adopt_without_initializer(plan)
  expect_null(attr(
    attr(raw$.params, "index", exact = TRUE),
    "__id__cls__grouping",
    exact = TRUE
  ))

  plan = native_subset_call(param_set, private$.params$id, NA)
  public = ParamSet$new(plan$state)
  expect_identical(
    data.table::indices(public$.__enclos_env__$private$.params),
    "id__cls__grouping"
  )
  expect_identical(public$ids(), encoded_id)
})

test_that("dependency-bearing subspaces emit empty child dependencies", {
  skip_if_not(native_subset_available())
  param_set = ps(
    parent = p_lgl(init = TRUE),
    child = p_int(0, 4, init = 2L)
  )
  param_set$add_dep("child", "parent", CondEqual(TRUE))
  param_set$constraint = function(x) TRUE
  param_set$extra_trafo = function(x, param_set) x

  plan = native_subset_call(param_set, "child", NA)
  expect_type(plan$state, "externalptr")
  direct = ParamSet$new(plan$state)
  expect_identical(direct$ids(), "child")
  expect_identical(nrow(direct$deps), 0L)
  observed = param_set$subspaces(c("child", "parent"))
  expected = native_subspaces_reference(
    param_set,
    c("child", "parent")
  )
  for (index in seq_along(observed)) {
    native_subspaces_expect_fallback_equivalent(
      observed[[index]],
      expected[[index]]
    )
    expect_identical(nrow(observed[[index]]$deps), 0L)
    expect_null(observed[[index]]$constraint)
  }
})

test_that("subspaces preserve values-first forcing and altered fallback", {
  skip_if_not(native_subset_available())
  param_set = ps(x = p_int(0, 2, init = 1L))
  stored = param_set$values
  original = activeBindingFunction("values", param_set)
  state = new.env(parent = emptyenv())
  state$events = character()
  makeActiveBinding("values", function(value) {
    if (!missing(value)) stop("test values binding is read-only")
    state$events = c(state$events, "values")
    stored
  }, param_set)
  on.exit(makeActiveBinding("values", original, param_set), add = TRUE)

  observed = param_set$subspaces({
    state$events = c(state$events, "ids")
    "x"
  })
  expect_identical(state$events, c("values", "ids"))
  expect_identical(observed$x$values, list(x = 1L))

  makeActiveBinding("values", original, param_set)
  private = param_set$.__enclos_env__$private
  original_extra = private$.extra_trafo
  delayed_state = new.env(parent = emptyenv())
  delayed_state$forced = 0L
  unlockBinding(".extra_trafo", private)
  delayedAssign(
    ".extra_trafo",
    {
      delayed_state$forced = delayed_state$forced + 1L
      original_extra
    },
    assign.env = private
  )
  lockBinding(".extra_trafo", private)
  on.exit({
    unlockBinding(".extra_trafo", private)
    assign(".extra_trafo", original_extra, envir = private)
    lockBinding(".extra_trafo", private)
  }, add = TRUE)
  expect_null(native_subset_call(param_set, "x", NA))
  expect_identical(delayed_state$forced, 0L)

  callbacks = 0L
  hostile_ids = native_stateful_altrep(
    c("x", "x"),
    c("x", "x"),
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("subspace planning invoked ALTREP", call. = FALSE)
    },
    callback_after = c(NA_integer_, 0L)
  )
  planner = get(
    "param_set_subspace_plans",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  expect_null(planner(param_set, private, hostile_ids))
  expect_identical(callbacks, 0L)
})

test_that("subspace planning safely resolves a pending values finalizer", {
  skip_if_not(native_subset_available())
  namespace = asNamespace("paradox")
  planner = get("param_set_subspace_plans", namespace)
  param_set = ps(
    x = p_dbl(0, 1, init = 0.25),
    y = p_int(0, 3, init = 2L)
  )
  private = param_set$.__enclos_env__$private
  captured = param_set$values
  replacement = list(x = 0.75, y = 3L)
  state = new.env(parent = emptyenv())
  state$ran = FALSE
  trigger = new.env(parent = emptyenv())
  reg.finalizer(trigger, function(ignored) {
    state$ran = TRUE
    private$.values = replacement
  }, onexit = FALSE)
  rm(trigger)

  previous = gctorture2(1L, wait = 0L)
  on.exit(gctorture2(previous), add = TRUE)
  plans = planner(param_set, private, param_set$ids(), captured)
  gctorture2(previous)
  on.exit({
    private$.values = captured
  }, add = TRUE)
  if (is.null(plans)) {
    # A mutation observed before the final native audit declines atomically.
    expect_true(state$ran)
  } else {
    # The validate-once planner has no intermediate R return boundary. Pending
    # finalizers may therefore run only after the complete batch is returned;
    # its detached payload must retain the captured public values snapshot.
    expect_type(plans, "list")
    expect_length(plans, 2L)
    invisible(gc(FALSE))
    expect_true(state$ran)
    children = lapply(plans, function(plan) ParamSet$new(plan$state))
    expect_identical(children$x$values, list(x = 0.25))
    expect_identical(children$y$values, list(y = 2L))
  }
})

test_that("public subspaces and SamplerUnif reuse a declined plan snapshot", {
  skip_if_not(native_subset_available())
  namespace = asNamespace("paradox")
  binding = "param_set_subspace_plans"
  original = get(binding, namespace, inherits = FALSE)
  param_set = ps(
    x = p_dbl(0, 1, init = 0.25),
    y = p_int(0, 3, init = 2L)
  )
  private = param_set$.__enclos_env__$private
  captured = param_set$values
  replacement = list(x = 0.75, y = 3L)
  unlockBinding(binding, namespace)
  assign(binding, function(param_set, private, ids, values) {
    private$.values = replacement
    NULL
  }, namespace)
  lockBinding(binding, namespace)
  on.exit({
    unlockBinding(binding, namespace)
    assign(binding, original, namespace)
    lockBinding(binding, namespace)
    private$.values = captured
  }, add = TRUE)

  subspaces = param_set$subspaces()
  expect_identical(subspaces$x$values, list(x = 0.25))
  expect_identical(subspaces$y$values, list(y = 2L))

  private$.values = captured
  sampler = SamplerUnif$new(param_set)
  expect_identical(sampler$samplers$x$param$values, list(x = 0.25))
  expect_identical(sampler$samplers$y$param$values, list(y = 2L))
})

test_that("fresh subspaces retain downstream one-dimensional samplers", {
  skip_if_not(native_subset_available())
  param_set = ps(
    left = p_dbl(0, 1, init = 0.25),
    right = p_dbl(0, 1, init = 0.75)
  )
  subspaces = param_set$subspaces(c("right", "left"))
  samplers = lapply(subspaces, function(subspace) {
    Sampler1DRfun$new(
      subspace,
      rfun = function(n) rep(0.5, n),
      trunc = FALSE
    )
  })
  expect_identical(
    vapply(samplers, function(sampler) sampler$param$ids(), character(1L)),
    c(right = "right", left = "left")
  )
  expect_identical(
    lapply(samplers, function(sampler) as.list(sampler$sample(3L)$data)),
    list(
      right = list(right = rep(0.75, 3L)),
      left = list(left = rep(0.25, 3L))
    )
  )

  subspaces$right$values = list(right = 0.1)
  expect_identical(samplers$right$param$values, list(right = 0.75))
})

test_that("owned subspace tokens transfer once without a child clone", {
  skip_if_not(native_subset_available())
  param_set = ps(
    x = p_dbl(-2, 3, tags = "numeric", trafo = exp)
  )
  param_set$values = list(x = 0.5)
  param_set$extra_trafo = function(x) x
  plan = native_subset_call(param_set, "x", NA)
  expect_true(.Call(native_subset_adopt_symbol(), NULL, plan$state))
  expect_true(.Call(native_subset_adopt_symbol(), NULL, plan$state))

  sampler = Sampler1DUnif$new(plan$state)
  expect_false(.Call(native_subset_adopt_symbol(), NULL, plan$state))
  expect_identical(sampler$param$ids(), "x")
  expect_identical(sampler$param$values, list(x = 0.5))
  expect_identical(sampler$param$tags$x, "numeric")
  expect_identical(sampler$param$extra_trafo, param_set$extra_trafo)
  expect_identical(nrow(sampler$param$deps), 0L)

  param_set$values = list(x = 1)
  data.table::set(
    param_set$.__enclos_env__$private$.params,
    1L,
    "lower",
    -100
  )
  expect_identical(sampler$param$values, list(x = 0.5))
  expect_identical(unname(sampler$param$lower), -2)

  Box = R6::R6Class(
    "NativeOwnedSubspaceValueBox",
    public = list(
      value = NULL,
      initialize = function(value) self$value = value
    )
  )
  box = Box$new(42L)
  special = ps(x = p_dbl(0, 1, special_vals = list(box)))
  special$values = list(x = box)
  special_sampler = SamplerUnif$new(special)
  cloned_box = special_sampler$samplers$x$param$values$x
  expect_s3_class(cloned_box, "NativeOwnedSubspaceValueBox")
  expect_false(identical(cloned_box, box))
  expect_identical(cloned_box$value, 42L)

  foreign = new("externalptr")
  expect_false(.Call(native_subset_adopt_symbol(), NULL, foreign))
  expect_error(
    Sampler1DUnif$new(foreign),
    "R6"
  )
})

test_that("SamplerUnif owned construction matches the complete fallback", {
  skip_if_not(native_subset_available())
  param_set = ps(
    double = p_dbl(-2, 3, tags = "numeric", trafo = exp),
    integer = p_int(-3, 4, init = 2L, tags = "numeric"),
    factor = p_fct(c("slow", "fast"), init = "fast"),
    logical = p_lgl(init = TRUE)
  )
  param_set$values$double = 0.5
  param_set$extra_trafo = function(x, param_set) x

  fast = SamplerUnif$new(param_set)
  namespace = asNamespace("paradox")
  binding = "param_set_subspace_plans"
  original = get(binding, envir = namespace, inherits = FALSE)
  unlockBinding(binding, namespace)
  on.exit({
    assign(binding, original, envir = namespace)
    lockBinding(binding, namespace)
  }, add = TRUE)
  assign(binding, function(param_set, private, ids) NULL, envir = namespace)
  fallback = SamplerUnif$new(param_set)

  expect_identical(class(fast), class(fallback))
  expect_identical(names(fast$samplers), names(fallback$samplers))
  expect_identical(fast$param_set$ids(), fallback$param_set$ids())
  expect_identical(fast$param_set$values, fallback$param_set$values)
  for (index in seq_along(fast$samplers)) {
    observed = fast$samplers[[index]]$param
    expected = fallback$samplers[[index]]$param
    native_subset_expect_equivalent(observed, expected)
    expect_identical(observed$extra_trafo, param_set$extra_trafo)
  }

  param_set$values = list(double = 1)
  expect_identical(fast$param_set$values$double, 0.5)
  expect_identical(fast$samplers$double$param$values$double, 0.5)
})

native_bulk_subspace_symbols = function() {
  namespace = asNamespace("paradox")
  list(
    factory = get("C_param_set_bulk_shells", namespace),
    generator_auth = get("C_param_set_bulk_generator_auth", namespace),
    token_probe = get("C_param_set_adopt_subset_state", namespace)
  )
}

native_bulk_subspace_available = function() {
  if (!native_subset_available()) return(FALSE)
  namespace = asNamespace("paradox")
  isTRUE(.Call(
    native_bulk_subspace_symbols()$generator_auth,
    get("ParamSet", namespace)
  ))
}

test_that("reviewed R and R6 combination admits bulk ParamSet shells", {
  if (getRversion() >= "4.6.0" &&
      packageVersion("R6") == package_version("2.6.1")) {
    expect_true(native_bulk_subspace_available())
  }
})

native_bulk_subspace_call = function(param_set, ids = param_set$ids()) {
  namespace = asNamespace("paradox")
  plans = get("param_set_subspace_plans", namespace)(
    param_set,
    param_set$.__enclos_env__$private,
    ids
  )
  list(
    plans = plans,
    result = .Call(
      native_bulk_subspace_symbols()$factory,
      ParamSet,
      plans
    )
  )
}

expect_param_set_shell_graph = function(observed, expected) {
  expect_identical(attributes(observed), attributes(expected))
  expect_identical(parent.env(observed), parent.env(expected))
  expect_identical(environmentIsLocked(observed), environmentIsLocked(expected))

  observed_enclosure = observed$.__enclos_env__
  expected_enclosure = expected$.__enclos_env__
  observed_private = observed_enclosure$private
  expected_private = expected_enclosure$private
  environments = list(
    list(observed, expected),
    list(observed_enclosure, expected_enclosure),
    list(observed_private, expected_private)
  )
  for (pair in environments) {
    observed_environment = pair[[1L]]
    expected_environment = pair[[2L]]
    observed_names = ls(observed_environment, all.names = TRUE)
    expected_names = ls(expected_environment, all.names = TRUE)
    expect_identical(observed_names, expected_names)
    expect_identical(
      ls(observed_environment, all.names = TRUE, sorted = FALSE),
      ls(expected_environment, all.names = TRUE, sorted = FALSE)
    )
    expect_identical(
      environmentIsLocked(observed_environment),
      environmentIsLocked(expected_environment)
    )
    expect_identical(
      env.profile(observed_environment),
      env.profile(expected_environment)
    )
    for (name in observed_names) {
      expect_identical(
        bindingIsActive(name, observed_environment),
        bindingIsActive(name, expected_environment)
      )
      expect_identical(
        bindingIsLocked(name, observed_environment),
        bindingIsLocked(name, expected_environment)
      )
      if (name %in% c("self", "private", ".__enclos_env__", ".__active__")) {
        next
      }
      observed_value = if (bindingIsActive(name, observed_environment)) {
        activeBindingFunction(name, observed_environment)
      } else {
        get(name, observed_environment, inherits = FALSE)
      }
      expected_value = if (bindingIsActive(name, expected_environment)) {
        activeBindingFunction(name, expected_environment)
      } else {
        get(name, expected_environment, inherits = FALSE)
      }
      generated_closure = bindingIsActive(name, expected_environment) ||
        bindingIsLocked(name, expected_environment)
      if (is.function(expected_value) && generated_closure) {
        expect_identical(formals(observed_value), formals(expected_value))
        expect_identical(body(observed_value), body(expected_value))
        expect_identical(attributes(observed_value), attributes(expected_value))
        expect_identical(environment(observed_value), observed_enclosure)
        expect_identical(environment(expected_value), expected_enclosure)
      } else {
        expect_identical(observed_value, expected_value)
      }
    }
  }
  expect_identical(parent.env(observed_enclosure), parent.env(expected_enclosure))
  expect_identical(parent.env(observed_private), parent.env(expected_private))
  expect_identical(observed_enclosure$self, observed)
  expect_identical(observed_enclosure$private, observed_private)

  observed_active = observed_enclosure$.__active__
  expected_active = expected_enclosure$.__active__
  expect_identical(names(observed_active), names(expected_active))
  for (name in names(observed_active)) {
    expect_identical(
      data.table::address(observed_active[[name]]),
      data.table::address(activeBindingFunction(name, observed))
    )
    expect_identical(
      data.table::address(expected_active[[name]]),
      data.table::address(activeBindingFunction(name, expected))
    )
  }
}

test_that("bulk singleton shells reproduce the complete ParamSet graph", {
  skip_if_not(native_bulk_subspace_available())
  param_set = ps(
    x = p_dbl(-2, 3, tags = c("numeric", "shared"), trafo = exp),
    y = p_int(-3, 4, init = 2L, tags = "numeric"),
    z = p_lgl(init = TRUE)
  )
  param_set$values$x = 0.5
  param_set$extra_trafo = function(x, param_set) x

  observed_batch = native_bulk_subspace_call(param_set, c("z", "x", "x"))
  expect_type(observed_batch$result, "list")
  expect_named(observed_batch$result, c("z", "x", "x"))

  requested = c("z", "x", "x")
  captured_values = param_set$values
  scalar_subspace = get(
    "C_param_set_subspace_state",
    asNamespace("paradox")
  )
  expected_plans = lapply(requested, function(id) {
    .Call(
      scalar_subspace,
      param_set$.__enclos_env__$private,
      param_set,
      id,
      NA,
      captured_values
    )
  })
  expected = lapply(expected_plans, function(plan) ParamSet$new(plan$state))
  for (index in seq_along(expected)) {
    expect_param_set_shell_graph(observed_batch$result[[index]], expected[[index]])
  }
})

test_that("bulk shells clone, serialize, and mutate independently", {
  skip_if_not(native_bulk_subspace_available())
  param_set = ps(
    x = p_dbl(-2, 3, init = 0.5),
    y = p_int(-3, 4, init = 2L)
  )
  batch = native_bulk_subspace_call(param_set, c("x", "x", "y"))$result
  expect_length(batch, 3L)

  shallow = batch[[1L]]$clone()
  deep = batch[[1L]]$clone(deep = TRUE)
  restored = unserialize(serialize(batch[[1L]], NULL))
  for (object in list(shallow, deep, restored)) {
    expect_s3_class(object, "ParamSet")
    expect_identical(object$ids(), "x")
    expect_identical(object$values, list(x = 0.5))
  }

  first_private = batch[[1L]]$.__enclos_env__$private
  second_private = batch[[2L]]$.__enclos_env__$private
  data.table::set(first_private$.params, 1L, "lower", -100)
  batch[[1L]]$values = list(x = 1)
  batch[[1L]]$assert_values = FALSE
  expect_identical(second_private$.params$lower, -2)
  expect_identical(batch[[2L]]$values, list(x = 0.5))
  expect_true(batch[[2L]]$assert_values)
  expect_identical(param_set$lower[["x"]], -2)
})

test_that("bulk admission fails atomically for altered surfaces and tokens", {
  skip_if_not(native_bulk_subspace_available())
  namespace = asNamespace("paradox")
  symbols = native_bulk_subspace_symbols()
  param_set = ps(x = p_dbl(0, 1), y = p_int(0, 2))
  planner = get("param_set_subspace_plans", namespace)
  plans = planner(
    param_set,
    param_set$.__enclos_env__$private,
    param_set$ids()
  )
  duplicate = structure(
    list(plans[[1L]], plans[[1L]]),
    names = c("x", "x")
  )
  expect_null(.Call(symbols$factory, ParamSet, duplicate))
  expect_true(.Call(symbols$token_probe, NULL, plans[[1L]]$state))

  invalid = plans
  invalid[[2L]]$state = new("externalptr")
  expect_null(.Call(symbols$factory, ParamSet, invalid))
  expect_true(.Call(symbols$token_probe, NULL, plans[[1L]]$state))
  expect_true(.Call(symbols$token_probe, NULL, plans[[2L]]$state))

  empty = structure(list(), names = character())
  expect_identical(.Call(symbols$factory, ParamSet, empty), empty)
})

test_that("complete generator tampering never invokes live R6 machinery", {
  skip_if_not(native_bulk_subspace_available())
  namespace = asNamespace("paradox")
  planner = get("param_set_subspace_plans", namespace)
  helper = get("param_set_bulk_subspace_shells", namespace)
  param_set = ps(x = p_dbl(0, 1))
  plans = planner(
    param_set,
    param_set$.__enclos_env__$private,
    "x"
  )
  calls = 0L
  replacements = list(
    new = function(...) {
      calls <<- calls + 1L
      stop("altered new ran", call. = FALSE)
    },
    class = FALSE,
    debug_names = "initialize",
    get_inherit = function() {
      calls <<- calls + 1L
      stop("altered get_inherit ran", call. = FALSE)
    },
    has_private = function() {
      calls <<- calls + 1L
      stop("altered has_private ran", call. = FALSE)
    }
  )
  for (name in names(replacements)) {
    original = get(name, ParamSet, inherits = FALSE)
    assign(name, replacements[[name]], ParamSet)
    expect_null(helper(plans), info = name)
    expect_identical(calls, 0L, info = name)
    expect_true(.Call(
      native_bulk_subspace_symbols()$token_probe,
      NULL,
      plans[[1L]]$state
    ), info = name)
    assign(name, original, ParamSet)
    expect_true(native_bulk_subspace_available(), info = name)
  }
})

test_that("bulk singleton shell assembly survives allocation torture", {
  skip_if_not(native_bulk_subspace_available())
  param_set = ps(
    x = p_dbl(0, 1, init = 0.25),
    y = p_int(0, 3, init = 2L)
  )
  previous = gctorture2(25L, wait = 0L)
  on.exit(gctorture2(previous), add = TRUE)
  observed = param_set$subspaces(c("y", "x"))
  gctorture2(previous)
  expect_identical(
    unname(vapply(observed, function(child) child$ids(), character(1L))),
    c("y", "x")
  )
  expect_identical(observed[[2L]]$values, list(x = 0.25))
})
