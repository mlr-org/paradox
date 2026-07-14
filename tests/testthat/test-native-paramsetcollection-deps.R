native_collection_deps_available = function() {
  exists(
    "C_param_set_collection_deps",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_deps_symbol = function() {
  get(
    "C_param_set_collection_deps",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_deps_call = function(collection, private = NULL) {
  if (is.null(private)) private = collection$.__enclos_env__$private
  .Call(native_collection_deps_symbol(), private, collection)
}

native_collection_deps_reference = function(collection) {
  private = collection$.__enclos_env__$private
  children = Map(function(child, owner) {
    result = if (identical(
        class(child),
        c("ParamSetCollection", "ParamSet", "R6")
      )) {
      native_collection_deps_reference(child)
    } else {
      child$deps
    }
    if (owner != "" && nrow(result)) {
      old_ids = child$ids()
      new_ids = private$.add_name_prefix(owner, old_ids)
      result$id = mlr3misc::map_values(result$id, old_ids, new_ids)
      result$on = mlr3misc::map_values(result$on, old_ids, new_ids)
    }
    result
  }, private$.sets, names(private$.sets))
  data.table::rbindlist(c(children, list(private$.deps)), use.names = TRUE)
}

native_collection_deps_child = function(ids, dependencies = list()) {
  result = ParamSet$new(setNames(
    lapply(ids, function(id) p_int(0L, 9L)),
    ids
  ))
  for (dependency in dependencies) {
    result$add_dep(
      dependency[[1L]],
      dependency[[2L]],
      CondEqual(dependency[[3L]])
    )
  }
  result
}

test_that("native collection deps is registered with forced arity two", {
  skip_if_not(native_collection_deps_available())
  symbol = native_collection_deps_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_collection_deps", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native collection deps preserves postorder and every affix layer", {
  skip_if_not(native_collection_deps_available())
  left = native_collection_deps_child(c("a", "b", "c"), list(
    list("b", "a", 1L),
    list("c", "b", 2L)
  ))
  right = native_collection_deps_child(
    c("x", "y"),
    list(list("y", "x", 3L))
  )
  inner = ParamSetCollection$new(
    list(inner = left),
    postfix_names = TRUE
  )
  inner$add_dep("c.inner", "a.inner", CondEqual(4L))
  outer = ParamSetCollection$new(list(outer = inner, right = right))
  outer$add_dep("right.y", "outer.a.inner", CondEqual(5L))

  direct = native_collection_deps_call(outer)
  expected = native_collection_deps_reference(outer)
  expect_false(is.null(direct))
  expect_identical(direct, expected)
  expect_identical(direct$id, c(
    "outer.b.inner", "outer.c.inner", "outer.c.inner",
    "right.y", "right.y"
  ))
  expect_identical(outer$deps, expected)
  expect_identical(names(direct), c("id", "on", "cond"))
  expect_identical(class(direct), c("data.table", "data.frame"))
  expect_identical(data.table::key(direct), NULL)
  expect_identical(data.table::indices(direct), NULL)
  expect_identical(data.table:::selfrefok(direct, FALSE), 1L)

  empty = native_collection_deps_call(ParamSetCollection$new(list()))
  expect_identical(dim(empty), c(0L, 3L))
  expect_identical(attr(empty, "row.names"), integer())
})

test_that("native collection deps retains dangling rows, duplicates, and DAGs", {
  skip_if_not(native_collection_deps_available())
  leaf = native_collection_deps_child(c("x", "y"))
  leaf_private = leaf$.__enclos_env__$private
  leaf_private$.deps = data.table::data.table(
    id = c("inner.x", "inner.x", "foreign"),
    on = c("inner.y", "inner.y", "foreign-on"),
    cond = list(CondEqual(1L), CondEqual(1L), CondEqual(2L))
  )
  inner = ParamSetCollection$new(list(inner = leaf))
  outer = ParamSetCollection$new(list(outer = inner))
  observed = native_collection_deps_call(outer)
  expect_identical(
    observed$id,
    c("outer.inner.x", "outer.inner.x", "foreign")
  )
  expect_identical(
    observed$on,
    c("outer.inner.y", "outer.inner.y", "foreign-on")
  )

  shared = ParamSetCollection$new(list(first = leaf, second = leaf))
  expect_identical(
    native_collection_deps_call(shared),
    native_collection_deps_reference(shared)
  )
  unnamed_left = native_collection_deps_child(
    c("a", "b"),
    list(list("b", "a", 1L))
  )
  unnamed_right = native_collection_deps_child(
    c("c", "d"),
    list(list("d", "c", 2L))
  )
  unnamed = ParamSetCollection$new(setNames(
    list(unnamed_left, unnamed_right),
    c("", "")
  ))
  expect_identical(
    native_collection_deps_call(unnamed),
    native_collection_deps_reference(unnamed)
  )
})

test_that("native collection deps owns ordinary shells and shares opaque leaves", {
  skip_if_not(native_collection_deps_available())
  marker = new.env(parent = emptyenv())
  shared = list(value = 1L)
  condition = structure(list(
    first = shared,
    second = shared,
    marker = marker
  ), class = "Condition")
  child = native_collection_deps_child(c("a", "b"))
  private = child$.__enclos_env__$private
  private$.deps = data.table::data.table(
    id = "b",
    on = "a",
    cond = list(condition)
  )
  collection = ParamSetCollection$new(list(child = child))

  first = native_collection_deps_call(collection)
  second = native_collection_deps_call(collection)
  expect_identical(first, second)
  for (column in names(first)) {
    expect_false(identical(
      data.table::address(first[[column]]),
      data.table::address(second[[column]])
    ), info = column)
  }
  expect_false(identical(
    data.table::address(first$cond[[1L]]),
    data.table::address(private$.deps$cond[[1L]])
  ))
  expect_false(identical(
    data.table::address(first$cond[[1L]]$first),
    data.table::address(first$cond[[1L]]$second)
  ))
  expect_identical(first$cond[[1L]]$marker, marker)

  first$id[[1L]] = "changed"
  first$cond[[1L]]$first$value = 9L
  expect_identical(private$.deps$id, "b")
  expect_identical(private$.deps$cond[[1L]]$first$value, 1L)
  expect_identical(first$cond[[1L]]$second$value, 1L)

  # rbindlist duplicates the same source Condition independently for every
  # row. Preserve that ownership boundary as well as aliases within one row.
  private$.deps = data.table::data.table(
    id = c("b", "b"),
    on = c("a", "a"),
    cond = list(condition, condition)
  )
  expected = native_collection_deps_reference(collection)
  repeated = native_collection_deps_call(collection)
  expect_identical(repeated, expected)
  expect_false(identical(
    data.table::address(expected$cond[[1L]]),
    data.table::address(expected$cond[[2L]])
  ))
  expect_false(identical(
    data.table::address(repeated$cond[[1L]]),
    data.table::address(repeated$cond[[2L]])
  ))
  expect_false(identical(
    data.table::address(repeated$cond[[1L]]),
    data.table::address(private$.deps$cond[[1L]])
  ))
})

test_that("unsupported descendants decline before public callbacks", {
  skip_if_not(native_collection_deps_available())
  events = new.env(parent = emptyenv())
  events$seen = character()
  Probe = R6::R6Class(
    "NativeCollectionDepsProbe",
    inherit = ParamSet,
    public = list(
      initialize = function(label, params) {
        private$.label = label
        super$initialize(params)
      },
      ids = function(...) {
        events$seen = c(events$seen, sprintf("%s:ids", private$.label))
        super$ids(...)
      }
    ),
    active = list(
      deps = function(value) {
        if (!missing(value)) stop("deps is read-only")
        events$seen = c(events$seen, sprintf("%s:deps", private$.label))
        super$deps
      }
    ),
    private = list(.label = NULL)
  )
  named = Probe$new("named", list(a = p_int(), b = p_int()))
  named$add_dep("b", "a", CondEqual(1L))
  empty = Probe$new("empty", list(x = p_int()))
  unnamed = Probe$new("unnamed", list(a = p_int(), b = p_int()))
  unnamed$add_dep("b", "a", CondEqual(2L))
  collection = ParamSetCollection$new(setNames(
    list(named, empty, unnamed),
    c("named", "empty", "")
  ))

  events$seen = character()
  expect_null(native_collection_deps_call(collection))
  expect_identical(events$seen, character())
  expect_identical(collection$deps$id, c("named.b", "b"))
  expect_identical(
    events$seen,
    c("named:deps", "named:ids", "empty:deps", "unnamed:deps")
  )

  CustomCollection = R6::R6Class(
    "NativeCollectionDepsSubclass",
    inherit = ParamSetCollection
  )
  expect_null(native_collection_deps_call(
    CustomCollection$new(list(child = native_collection_deps_child("x")))
  ))
  make_custom = function() paradox:::Domain(
      cls = "NativeCollectionDepsCustom",
      grouping = "NativeCollectionDepsCustom",
      storage_type = "numeric"
    )
  custom = make_custom()
  expect_null(native_collection_deps_call(ParamSetCollection$new(list(
    child = ParamSet$new(list(custom = custom))
  ))))
})

test_that("replaced exact-class deps and ids bindings decline without execution", {
  skip_if_not(native_collection_deps_available())
  child = native_collection_deps_child(
    c("a", "b"),
    list(list("b", "a", 1L))
  )
  collection = ParamSetCollection$new(list(child = child))

  original_deps = activeBindingFunction("deps", child)
  deps_reads = 0L
  makeActiveBinding("deps", function(value) {
    deps_reads <<- deps_reads + 1L
    original_deps(value)
  }, child)
  on.exit(makeActiveBinding("deps", original_deps, child), add = TRUE)
  expect_null(native_collection_deps_call(collection))
  expect_identical(deps_reads, 0L)
  collection$deps
  expect_identical(deps_reads, 1L)

  makeActiveBinding("deps", original_deps, child)
  original_ids = child$ids
  ids_reads = 0L
  unlockBinding("ids", child)
  child$ids = function(...) {
    ids_reads <<- ids_reads + 1L
    original_ids(...)
  }
  lockBinding("ids", child)
  on.exit({
    unlockBinding("ids", child)
    child$ids = original_ids
    lockBinding("ids", child)
  }, add = TRUE)
  expect_null(native_collection_deps_call(collection))
  expect_identical(ids_reads, 0L)
  collection$deps
  expect_identical(ids_reads, 1L)
})

test_that("collection deps rejects fancy ids-wrapper shadows without forcing", {
  skip_if_not(native_collection_deps_available())
  namespace = asNamespace("paradox")

  for (kind in c("active", "delayed")) {
    for (shadow in c(".__ParamSet__ids", "super")) {
      child = native_collection_deps_child(
        c("a", "b"),
        list(list("b", "a", 1L))
      )
      collection = ParamSetCollection$new(list(child = child))
      wrapper_environment = environment(child$ids)
      reads = new.env(parent = emptyenv())
      reads$count = 0L
      value = if (shadow == "super") {
        NULL
      } else {
        get(shadow, envir = namespace, inherits = FALSE)
      }
      if (kind == "active") {
        makeActiveBinding(shadow, function(replacement) {
          reads$count = reads$count + 1L
          value
        }, wrapper_environment)
      } else {
        evaluation_environment = list2env(
          list(reads = reads, value = value),
          parent = baseenv()
        )
        delayedAssign(
          shadow,
          {
            reads$count = reads$count + 1L
            value
          },
          eval.env = evaluation_environment,
          assign.env = wrapper_environment
        )
      }

      expect_null(
        native_collection_deps_call(collection),
        info = paste(kind, shadow)
      )
      expect_identical(reads$count, 0L, info = paste(kind, shadow))
      expect_identical(nrow(collection$deps), 1L)
      if (shadow == "super") {
        expect_identical(reads$count, 0L, info = paste(kind, shadow))
      } else {
        expect_true(reads$count > 0L, info = paste(kind, shadow))
      }
    }
  }
})

test_that("collection deps rejects reparented ids wrappers before execution", {
  skip_if_not(native_collection_deps_available())
  child = native_collection_deps_child(
    c("a", "b"),
    list(list("b", "a", 1L))
  )
  collection = ParamSetCollection$new(list(child = child))
  namespace = asNamespace("paradox")
  target = get(".__ParamSet__ids", envir = namespace, inherits = FALSE)
  reads = new.env(parent = emptyenv())
  reads$count = 0L
  forged_parent = new.env(parent = namespace)
  forged_parent$.__ParamSet__ids = function(
      self, private, super, class, tags, any_tags) {
    reads$count = reads$count + 1L
    target(
      self = self,
      private = private,
      super = super,
      class = class,
      tags = tags,
      any_tags = any_tags
    )
  }
  wrapper_environment = environment(child$ids)
  parent.env(wrapper_environment) = forged_parent

  expect_null(native_collection_deps_call(collection))
  expect_identical(reads$count, 0L)
  expect_identical(nrow(collection$deps), 1L)
  expect_identical(reads$count, 1L)
})

test_that("ids authentication follows the historical conditional call", {
  skip_if_not(native_collection_deps_available())
  replace_ids = function(child, reads) {
    original = child$ids
    unlockBinding("ids", child)
    child$ids = function(...) {
      reads$count = reads$count + 1L
      original(...)
    }
    lockBinding("ids", child)
    original
  }
  restore_ids = function(child, original) {
    unlockBinding("ids", child)
    child$ids = original
    lockBinding("ids", child)
  }

  empty = native_collection_deps_child("x")
  empty_reads = new.env(parent = emptyenv())
  empty_reads$count = 0L
  empty_original = replace_ids(empty, empty_reads)
  on.exit(restore_ids(empty, empty_original), add = TRUE)
  named_empty = ParamSetCollection$new(list(named = empty))
  expect_false(is.null(native_collection_deps_call(named_empty)))
  expect_identical(empty_reads$count, 0L)

  unnamed = native_collection_deps_child(
    c("a", "b"),
    list(list("b", "a", 1L))
  )
  unnamed_reads = new.env(parent = emptyenv())
  unnamed_reads$count = 0L
  unnamed_original = replace_ids(unnamed, unnamed_reads)
  on.exit(restore_ids(unnamed, unnamed_original), add = TRUE)
  unnamed_collection = ParamSetCollection$new(setNames(list(unnamed), ""))
  expect_false(is.null(native_collection_deps_call(unnamed_collection)))
  expect_identical(unnamed_reads$count, 0L)
})

test_that("empty setter-created dependency index has one exact safe shape", {
  skip_if_not(native_collection_deps_available())
  with_dependencies = function(child, dependencies, code) {
    private = child$.__enclos_env__$private
    original = private$.deps
    on.exit(private$.deps <- original)
    private$.deps = dependencies
    force(code)
  }
  indexed_empty = function(marker = "__on__id", cache = integer(),
      extra = FALSE) {
    index = integer()
    attr(index, marker) = cache
    if (extra) attr(index, "extra") = integer()
    index
  }

  child = ps(a = p_int(init = 1L), token = p_int(0L, 10L))
  child$values$token = to_tune()
  dependencies = child$.__enclos_env__$private$.deps
  expect_identical(
    attr(dependencies, "index"),
    indexed_empty()
  )
  collection = ParamSetCollection$new(list(child = child))
  expect_false(is.null(native_collection_deps_call(collection)))

  for (near_miss in list(
      indexed_empty("__id"),
      indexed_empty(extra = TRUE),
      indexed_empty(cache = 1L),
      structure(logical(), `__on__id` = integer())
    )) {
    bad = data.table::copy(dependencies)
    attr(bad, "index") = near_miss
    expect_null(with_dependencies(
      child,
      bad,
      native_collection_deps_call(collection)
    ))
  }

  nonempty = native_collection_deps_child(
    c("a", "b"),
    list(list("b", "a", 1L))
  )
  nonempty_collection = ParamSetCollection$new(list(child = nonempty))
  expect_false(is.null(native_collection_deps_call(nonempty_collection)))
  bad = data.table::copy(nonempty$.__enclos_env__$private$.deps)
  attr(bad, "index") = indexed_empty()
  expect_null(with_dependencies(
    nonempty,
    bad,
    native_collection_deps_call(nonempty_collection)
  ))
})

test_that("malformed and fancy private state declines atomically", {
  skip_if_not(native_collection_deps_available())
  make_collection = function() {
    child = native_collection_deps_child(
      c("a", "b"),
      list(list("b", "a", 1L))
    )
    ParamSetCollection$new(list(child = child))
  }
  with_private_value = function(object, name, value, code) {
    private = object$.__enclos_env__$private
    original = private[[name]]
    on.exit(private[[name]] <- original)
    private[[name]] = value
    force(code)
  }

  collection = make_collection()
  private = collection$.__enclos_env__$private
  other = make_collection()
  expect_null(native_collection_deps_call(
    collection,
    other$.__enclos_env__$private
  ))
  expect_null(.Call(
    native_collection_deps_symbol(),
    private,
    new.env(parent = emptyenv())
  ))

  bad_sets = private$.sets
  attr(bad_sets, "note") = TRUE
  expect_null(with_private_value(
    collection, ".sets", bad_sets, native_collection_deps_call(collection)
  ))
  expect_null(with_private_value(
    collection, ".postfix", structure(FALSE, note = TRUE),
    native_collection_deps_call(collection)
  ))

  child = private$.sets[[1L]]
  child_private = child$.__enclos_env__$private
  bad_params = data.table::copy(child_private$.params)
  attr(bad_params, "note") = TRUE
  expect_null(with_private_value(
    child, ".params", bad_params, native_collection_deps_call(collection)
  ))
  bad_deps = data.table::copy(child_private$.deps)
  bad_deps$cond[[1L]] = structure(list(), class = "not-a-condition")
  expect_null(with_private_value(
    child, ".deps", bad_deps, native_collection_deps_call(collection)
  ))
  bad_deps = data.table::copy(child_private$.deps)
  data.table::setindexv(bad_deps, "id")
  expect_null(with_private_value(
    child, ".deps", bad_deps, native_collection_deps_call(collection)
  ))

  saved_private = child_private
  active_private = new.env(parent = parent.env(saved_private), hash = TRUE)
  for (binding in setdiff(ls(saved_private, all.names = TRUE), ".deps")) {
    assign(
      binding,
      get(binding, envir = saved_private, inherits = FALSE),
      envir = active_private
    )
  }
  reads = 0L
  makeActiveBinding(".deps", function(value) {
    reads <<- reads + 1L
    saved_private$.deps
  }, active_private)
  wrapper_environment = environment(activeBindingFunction("deps", child))
  child$.__enclos_env__$private = active_private
  wrapper_environment$private = active_private
  on.exit({
    child$.__enclos_env__$private = saved_private
    wrapper_environment$private = saved_private
  }, add = TRUE)
  expect_null(native_collection_deps_call(collection))
  expect_identical(reads, 0L)

  child$.__enclos_env__$private = saved_private
  wrapper_environment$private = saved_private
  delayed_private = new.env(parent = parent.env(saved_private), hash = TRUE)
  for (binding in setdiff(ls(saved_private, all.names = TRUE), ".deps")) {
    assign(
      binding,
      get(binding, envir = saved_private, inherits = FALSE),
      envir = delayed_private
    )
  }
  delayed_reads = 0L
  delayedAssign(
    ".deps",
    {
      delayed_reads <<- delayed_reads + 1L
      saved_private$.deps
    },
    assign.env = delayed_private
  )
  child$.__enclos_env__$private = delayed_private
  wrapper_environment$private = delayed_private
  expect_null(native_collection_deps_call(collection))
  expect_identical(delayed_reads, 0L)
})

test_that("native collection deps rejects ALTREP before Length dispatch", {
  skip_if_not(native_collection_deps_available())
  child = native_collection_deps_child(
    c("a", "b"),
    list(list("b", "a", 1L))
  )
  collection = ParamSetCollection$new(list(owner = child))
  private = collection$.__enclos_env__$private
  original_sets = private$.sets
  original_postfix = private$.postfix
  on.exit({
    private$.sets = original_sets
    private$.postfix = original_postfix
  })

  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("collection deps invoked ALTREP Length", call. = FALSE)
  }
  stateful_unarmed = function(first, later = first) {
    native_stateful_altrep(
      first,
      later,
      callback = callback,
      callback_after = c(NA_integer_, NA_integer_)
    )
  }

  sets_fixture = stateful_unarmed(original_sets)
  private$.sets = sets_fixture
  native_stateful_altrep_rearm(sets_fixture, c(NA_integer_, 0L))
  expect_null(native_collection_deps_call(collection))
  private$.sets = original_sets

  fancy_sets = original_sets
  name_fixture = stateful_unarmed("owner", "changed")
  attr(fancy_sets, "names") = name_fixture
  private$.sets = fancy_sets
  native_stateful_altrep_rearm(name_fixture, c(NA_integer_, 0L))
  expect_null(native_collection_deps_call(collection))
  private$.sets = original_sets

  postfix_fixture = stateful_unarmed(original_postfix)
  private$.postfix = postfix_fixture
  native_stateful_altrep_rearm(postfix_fixture, c(NA_integer_, 0L))
  expect_null(native_collection_deps_call(collection))
  expect_identical(callbacks, 0L)
})

test_that("unsupported encodings use the unchanged public fallback", {
  skip_if_not(native_collection_deps_available())
  latin_id = iconv("caf\u00e9", from = "UTF-8", to = "latin1")
  latin_owner = iconv("gr\u00f6\u00dfe", from = "UTF-8", to = "latin1")
  skip_if(anyNA(c(latin_id, latin_owner)))
  Encoding(latin_id) = "latin1"
  Encoding(latin_owner) = "latin1"

  child = native_collection_deps_child(
    c("source", "target"),
    list(list("target", "source", 1L))
  )
  collection = ParamSetCollection$new(list(owner = child))
  child_private = child$.__enclos_env__$private
  data.table::set(child_private$.params, 1L, "id", latin_id)
  data.table::setindexv(child_private$.params, c("id", "cls", "grouping"))
  data.table::set(child_private$.deps, 1L, "on", latin_id)
  names(collection$.__enclos_env__$private$.sets) = latin_owner

  expect_null(native_collection_deps_call(collection))
  expect_identical(
    collection$deps,
    native_collection_deps_reference(collection)
  )
})

test_that("cycles error while deep graphs, clones, and serialization are safe", {
  skip_if_not(native_collection_deps_available())
  cycle = ParamSetCollection$new(list())
  cycle$.__enclos_env__$private$.sets = list(loop = cycle)
  expect_error(
    native_collection_deps_call(cycle),
    "Cyclic ParamSetCollection graph is unsupported",
    fixed = TRUE
  )
  expect_error(
    cycle$deps,
    "Cyclic ParamSetCollection graph is unsupported",
    fixed = TRUE
  )

  leaf = native_collection_deps_child(
    c("a", "b"),
    list(list("b", "a", 1L))
  )
  deep = leaf
  for (index in seq_len(64L)) {
    deep = ParamSetCollection$new(setNames(list(deep), sprintf("n%02d", index)))
  }
  expect_false(is.null(native_collection_deps_call(deep)))
  expect_identical(
    native_collection_deps_call(deep),
    native_collection_deps_reference(deep)
  )

  collection = ParamSetCollection$new(list(child = leaf))
  for (copy in list(
      collection$clone(deep = TRUE),
      unserialize(serialize(collection, NULL))
    )) {
    expect_false(is.null(native_collection_deps_call(copy)))
    expect_identical(
      native_collection_deps_call(copy),
      native_collection_deps_reference(copy)
    )
  }
})

test_that("native collection deps survives focused gctorture", {
  skip_on_cran()

  skip_if_not(native_collection_deps_available())
  child = native_collection_deps_child(c("a", "b", "c"), list(
    list("b", "a", 1L),
    list("c", "b", 2L)
  ))
  collection = ParamSetCollection$new(list(
    outer = ParamSetCollection$new(list(inner = child)),
    shared = child
  ))
  expected = native_collection_deps_reference(collection)
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  for (iteration in seq_len(4L)) {
    expect_identical(native_collection_deps_call(collection), expected)
  }
  gctorture(previous)
})
