native_collection_params_available = function() {
  exists(
    "C_param_set_collection_params",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_params_symbol = function() {
  get(
    "C_param_set_collection_params",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_params_call = function(collection, private = NULL) {
  if (is.null(private)) private = collection$.__enclos_env__$private
  .Call(native_collection_params_symbol(), private, collection)
}

native_collection_params_reference = function(collection) {
  private = collection$.__enclos_env__$private
  result = data.table::copy(private$.params)
  result[, .tags := list(collection$tags)]
  result[private$.trafos, .trafo := list(trafo), on = "id"]
  result[
    collection$deps,
    .requirements := mlr3misc::transpose_list(list(on, cond)),
    on = "id"
  ]
  values = collection$values
  result[, c(".init_given", ".init") := list(
    id %in% names(values),
    unname(values[id])
  )]
  result[]
}

native_collection_clone_private = function(private, omit = character()) {
  result = new.env(parent = parent.env(private), hash = TRUE)
  bindings = setdiff(ls(private, all.names = TRUE), omit)
  for (binding in bindings) {
    assign(
      binding,
      get(binding, envir = private, inherits = FALSE),
      envir = result
    )
  }
  result
}

native_collection_rich = function(postfix = FALSE) {
  marker = new.env(parent = emptyenv())
  marker$value = 1L
  left = ps(
    zeta = p_fct(c("slow", "fast"), tags = c("choice", "shared"), init = "fast"),
    alpha = p_dbl(-2, 2, tags = c("numeric", "shared"), trafo = exp),
    mu = p_int(-3L, 3L, init = 2L)
  )
  left$add_dep("zeta", "mu", CondEqual(2L))
  left$add_dep("zeta", "alpha", CondAnyOf(c(-1, 1)))
  right = ps(
    payload = p_uty(tags = "opaque", init = marker),
    flag = p_lgl(tags = "logical", init = TRUE)
  )
  result = ParamSetCollection$new(
    list(owner = left, other = right),
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = postfix
  )
  ids = result$ids()
  result$add_dep(ids[[length(ids)]], ids[[1L]], CondEqual("fast"))
  result
}

test_that("native collection params is registered with forced arity two", {
  skip_if_not(native_collection_params_available())
  symbol = native_collection_params_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_collection_params", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native collection params admits exact empty, rich, and nested graphs", {
  skip_if_not(native_collection_params_available())
  prefix = native_collection_rich(FALSE)
  postfix = native_collection_rich(TRUE)
  nested = ParamSetCollection$new(list(
    outer = prefix,
    tail = ps(q = p_dbl(-1, 1, init = 0.25))
  ), tag_sets = TRUE, tag_params = TRUE)
  empty_children = ParamSetCollection$new(setNames(
    replicate(12L, ParamSet$new(), simplify = FALSE),
    c("", "", sprintf("empty%02d", seq_len(10L)))
  ))

  for (collection in list(
      ParamSetCollection$new(list()),
      empty_children,
      prefix,
      postfix,
      nested
    )) {
    expected = native_collection_params_reference(collection)
    direct = native_collection_params_call(collection)
    expect_false(is.null(direct))
    expect_identical(direct, expected)
    expect_identical(collection$params, expected)
    expect_identical(data.table::key(direct), NULL)
    expect_identical(
      data.table::indices(direct),
      data.table::indices(collection$.__enclos_env__$private$.params)
    )
    expect_identical(data.table:::selfrefok(direct, FALSE), 1L)
  }
})

test_that("keyed translation permutations are inverted in construction order", {
  skip_if_not(native_collection_params_available())
  child = ps(
    zeta = p_int(init = 1L),
    alpha = p_lgl(init = TRUE),
    mu = p_dbl(init = 0.5)
  )
  collection = ParamSetCollection$new(list(owner = child))
  private = collection$.__enclos_env__$private

  # `.params` retains zeta, alpha, mu while the keyed translation is
  # alpha, mu, zeta: a three-cycle, not a self-inverse permutation.
  expect_identical(private$.params$id, c(
    "owner.zeta", "owner.alpha", "owner.mu"
  ))
  expect_identical(private$.translation$id, c(
    "owner.alpha", "owner.mu", "owner.zeta"
  ))
  direct = native_collection_params_call(collection)
  expect_false(is.null(direct))
  expect_identical(direct, native_collection_params_reference(collection))
  expect_identical(direct$id, private$.params$id)
})

test_that("native collection params owns shells and preserves opaque leaves", {
  skip_if_not(native_collection_params_available())
  collection = native_collection_rich()
  marker = collection$sets$other$values$payload
  first = native_collection_params_call(collection)
  second = native_collection_params_call(collection)

  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  for (column in names(first)) {
    expect_false(identical(
      data.table::address(first[[column]]),
      data.table::address(second[[column]])
    ), info = column)
  }
  expect_identical(first$.init[[4L]], marker)
  data.table::set(first, i = 1L, j = "lower", value = -100)
  first$.tags[[1L]][[1L]] = "changed"
  first$.requirements[[1L]][[1L]] = "changed"
  expect_identical(native_collection_params_call(collection), second)

  first$.init[[4L]]$value = 42L
  expect_identical(marker$value, 42L)
})

test_that("collection params temporary data.table updates remain detached", {
  skip_if_not(native_collection_params_available())
  collection = ParamSetCollection$new(list(
    child = ps(x = p_int(init = 1L), y = p_dbl(-1, 1))
  ))
  before = collection$params

  added = collection$params[, marker := seq_len(.N)]
  expect_identical(added$marker, c(1L, 2L))
  expect_identical(collection$params, before)

  replaced = collection$params[, lower := c(-10, -20)]
  expect_identical(replaced$lower, c(-10, -20))
  expect_identical(collection$params, before)

  set_result = data.table::set(
    collection$params,
    j = "marker",
    value = c("a", "b")
  )
  expect_identical(set_result$marker, c("a", "b"))
  expect_identical(collection$params, before)

  expect_error(
    collection$params <- added,
    "params is read-only.",
    fixed = TRUE
  )
  expect_error(
    assign("params", added, envir = collection),
    "params is read-only.",
    fixed = TRUE
  )
  expect_identical(collection$params, before)
})

test_that("collection subclasses and custom descendants decline before callbacks", {
  skip_if_not(native_collection_params_available())
  events = new.env(parent = emptyenv())
  events$seen = character()
  CountingChild = R6::R6Class(
    "NativeCollectionParamsCountingChild",
    inherit = ParamSet,
    active = list(
      deps = function(value) {
        if (!missing(value)) stop("deps is read-only")
        events$seen = c(events$seen, "deps")
        super$deps
      },
      values = function(value) {
        if (!missing(value)) {
          super$values = value
          return(value)
        }
        events$seen = c(events$seen, "values")
        super$values
      }
    )
  )
  child = CountingChild$new(list(x = p_int(init = 1L)))
  collection = ParamSetCollection$new(list(child = child))

  events$seen = character()
  expect_null(native_collection_params_call(collection))
  expect_identical(events$seen, character())
  observed = collection$params
  expect_identical(events$seen, c("deps", "values"))
  expect_identical(observed$id, "child.x")

  SubCollection = R6::R6Class(
    "NativeCollectionParamsSubclass",
    inherit = ParamSetCollection
  )
  subclass = SubCollection$new(list(child = ps(x = p_int())))
  expect_null(native_collection_params_call(subclass))
  nested_subclass = ParamSetCollection$new(list(outer = subclass))
  expect_null(native_collection_params_call(nested_subclass))

  make_custom = function() paradox:::Domain(
      cls = "NativeCollectionParamsExtension",
      grouping = "NativeCollectionParamsExtension",
      storage_type = "list"
    )
  custom = make_custom()
  custom_collection = ParamSetCollection$new(list(
    extension = ParamSet$new(list(value = custom))
  ))
  expect_null(native_collection_params_call(custom_collection))
  expect_identical(
    custom_collection$params,
    native_collection_params_reference(custom_collection)
  )
})

test_that("replaced exact-class public bindings decline before execution", {
  skip_if_not(native_collection_params_available())
  collection = native_collection_rich()
  original_deps = activeBindingFunction("deps", collection)
  reads = 0L
  makeActiveBinding("deps", function(value) {
    reads <<- reads + 1L
    original_deps(value)
  }, collection)
  on.exit(makeActiveBinding("deps", original_deps, collection), add = TRUE)

  expect_null(native_collection_params_call(collection))
  expect_identical(reads, 0L)
  observed = collection$params
  expect_identical(reads, 1L)
  expect_identical(observed$id, collection$ids())

  makeActiveBinding("deps", original_deps, collection)
  original_values = activeBindingFunction("values", collection)
  value_reads = 0L
  makeActiveBinding("values", function(value) {
    value_reads <<- value_reads + 1L
    original_values(value)
  }, collection)
  on.exit(makeActiveBinding("values", original_values, collection), add = TRUE)

  expect_null(native_collection_params_call(collection))
  expect_identical(value_reads, 0L)
  collection$params
  expect_identical(value_reads, 1L)

  makeActiveBinding("values", original_values, collection)
  original_tags = activeBindingFunction("tags", collection)
  tag_reads = 0L
  makeActiveBinding("tags", function(value) {
    tag_reads <<- tag_reads + 1L
    original_tags(value)
  }, collection)
  on.exit(makeActiveBinding("tags", original_tags, collection), add = TRUE)

  expect_null(native_collection_params_call(collection))
  expect_identical(tag_reads, 0L)
  collection$params
  expect_identical(tag_reads, 1L)
})

test_that("mutated superclass proxies decline the native collection path", {
  skip_if_not(native_collection_params_available())
  collection = ParamSetCollection$new(list(child = ps(x = p_int())))
  wrapper_environment = environment(activeBindingFunction("params", collection))
  original_super = wrapper_environment$super
  fake_super = new.env(parent = emptyenv())
  reads = 0L
  makeActiveBinding("params", function(value) {
    reads <<- reads + 1L
    data.table::data.table(id = "fake")
  }, fake_super)
  wrapper_environment$super = fake_super
  on.exit(wrapper_environment$super <- original_super, add = TRUE)

  expect_null(native_collection_params_call(collection))
  expect_identical(reads, 0L)
  expect_identical(collection$params$id, "fake")
  expect_identical(reads, 1L)
})

test_that("post-callback dependency drift is consumed without replay", {
  skip_if_not(native_collection_params_available())
  child = ps(parent = p_int(init = 1L), target = p_lgl(init = TRUE))
  child$add_dep("target", "parent", CondEqual(1L))
  collection = ParamSetCollection$new(list(child = child))
  original_ids = child$ids
  reads = 0L
  unlockBinding("ids", child)
  child$ids = function(...) {
    reads <<- reads + 1L
    c("wrong_parent", "wrong_target")
  }
  lockBinding("ids", child)
  on.exit({
    unlockBinding("ids", child)
    child$ids = original_ids
    lockBinding("ids", child)
  }, add = TRUE)

  observed = collection$params
  expect_identical(reads, 1L)
  expect_identical(observed$id, c("child.parent", "child.target"))
  expect_identical(observed$.requirements, list(NULL, NULL))
})

test_that("invalid post-callback values fail once without R fallback replay", {
  skip_if_not(native_collection_params_available())
  child = ps(x = p_int(init = 1L))
  collection = ParamSetCollection$new(list(child = child))
  private = child$.__enclos_env__$private
  original_get_values = private$.get_values
  reads = 0L
  unlockBinding(".get_values", private)
  private$.get_values = function() {
    reads <<- reads + 1L
    c(x = 1L)
  }
  lockBinding(".get_values", private)
  on.exit({
    unlockBinding(".get_values", private)
    private$.get_values = original_get_values
    lockBinding(".get_values", private)
  }, add = TRUE)

  expect_error(
    collection$params,
    "state changed while evaluating the params binding",
    fixed = TRUE
  )
  expect_identical(reads, 1L)
})

test_that("pre-callback snapshots survive private params rebinding during deps", {
  skip_on_cran()

  skip_if_not(native_collection_params_available())
  child = ps(parent = p_int(init = 1L), target = p_lgl(init = TRUE))
  child$add_dep("target", "parent", CondEqual(1L))
  collection = ParamSetCollection$new(list(child = child))
  private = collection$.__enclos_env__$private
  saved_params = data.table::copy(private$.params)
  expected = native_collection_params_reference(collection)

  # The canonical collection deps getter calls each nonempty named child's
  # `$ids()` method. Test-only replacement of that normally locked method lets
  # us rebind and collect the admitted outer source after static assembly,
  # without replacing any public active binding admitted by the native gate.
  original_ids = child$ids
  unlockBinding("ids", child)
  child$ids = function(...) {
    private$.params = data.table::copy(saved_params[0])
    gc()
    original_ids(...)
  }
  lockBinding("ids", child)
  on.exit({
    unlockBinding("ids", child)
    child$ids = original_ids
    lockBinding("ids", child)
    private$.params = saved_params
  }, add = TRUE)

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = native_collection_params_call(collection)
  gctorture(previous)
  private$.params = saved_params

  expect_false(is.null(observed))
  expect_identical(observed, expected)
})

test_that("malformed collection ownership and translation decline atomically", {
  skip_if_not(native_collection_params_available())
  collection = ParamSetCollection$new(list(
    left = ps(z = p_int(), a = p_lgl()),
    right = ps(m = p_dbl())
  ))
  private = collection$.__enclos_env__$private

  with_private_value = function(name, value, code) {
    original = private[[name]]
    on.exit(private[[name]] <- original)
    private[[name]] = value
    force(code)
  }

  bad_postfix = structure(FALSE, note = TRUE)
  expect_null(with_private_value(
    ".postfix", bad_postfix, native_collection_params_call(collection)
  ))
  expect_null(with_private_value(
    ".postfix", c(FALSE, TRUE), native_collection_params_call(collection)
  ))

  bad_sets = private$.sets
  attr(bad_sets, "note") = TRUE
  expect_null(with_private_value(
    ".sets", bad_sets, native_collection_params_call(collection)
  ))

  translation = data.table::copy(private$.translation)
  translation$owner_ps_index[[1L]] = 0L
  expect_null(with_private_value(
    ".translation", translation, native_collection_params_call(collection)
  ))
  translation = data.table::copy(private$.translation)
  translation$owner_name[[1L]] = "wrong"
  expect_null(with_private_value(
    ".translation", translation, native_collection_params_call(collection)
  ))
  translation = data.table::copy(private$.translation)
  data.table::setindexv(translation, NULL)
  expect_null(with_private_value(
    ".translation", translation, native_collection_params_call(collection)
  ))
  translation = data.table::copy(private$.translation)
  translation$original_id[[1L]] = "wrong"
  expect_null(with_private_value(
    ".translation", translation, native_collection_params_call(collection)
  ))

  params = data.table::copy(private$.params)
  params$id[[1L]] = "wrong.affix"
  expect_null(with_private_value(
    ".params", params, native_collection_params_call(collection)
  ))
  params = data.table::copy(private$.params)
  data.table::setattr(
    params,
    "row.names",
    sprintf("row-%d", seq_len(nrow(params)))
  )
  expect_null(with_private_value(
    ".params", params, native_collection_params_call(collection)
  ))

  other = ParamSetCollection$new(list(child = ps(x = p_int())))
  expect_null(.Call(
    native_collection_params_symbol(),
    other$.__enclos_env__$private,
    collection
  ))
  expect_null(.Call(
    native_collection_params_symbol(),
    private,
    new.env(parent = emptyenv())
  ))
})

test_that("active and delayed collection state is never forced by admission", {
  skip_if_not(native_collection_params_available())
  collection = native_collection_rich()
  enclosure = collection$.__enclos_env__
  original_private = enclosure$private
  original_translation = original_private$.translation
  active_private = native_collection_clone_private(
    original_private,
    ".translation"
  )
  active_reads = 0L
  makeActiveBinding(".translation", function(value) {
    if (!missing(value)) stop("unexpected write")
    active_reads <<- active_reads + 1L
    original_translation
  }, active_private)
  enclosure$private = active_private
  on.exit(enclosure$private <- original_private, add = TRUE)

  expect_null(native_collection_params_call(collection))
  expect_identical(active_reads, 0L)
  enclosure$private = original_private

  delayed_private = native_collection_clone_private(
    original_private,
    ".translation"
  )
  delayed_reads = new.env(parent = emptyenv())
  delayed_reads$count = 0L
  promise_environment = list2env(
    list(reads = delayed_reads, value = original_translation),
    parent = baseenv()
  )
  delayedAssign(".translation", {
    reads$count = reads$count + 1L
    value
  }, eval.env = promise_environment, assign.env = delayed_private)
  enclosure$private = delayed_private

  expect_null(native_collection_params_call(collection))
  expect_identical(delayed_reads$count, 0L)
})

test_that("cycles error while repeated sibling references remain admissible", {
  skip_if_not(native_collection_params_available())
  shared = ps(x = p_int(init = 1L))
  repeated = ParamSetCollection$new(setNames(
    list(shared, shared),
    c("left", "right")
  ))
  direct = native_collection_params_call(repeated)
  expect_false(is.null(direct))
  expect_identical(direct, native_collection_params_reference(repeated))

  cyclic = ParamSetCollection$new(list())
  private = cyclic$.__enclos_env__$private
  private$.sets = list(self = cyclic)
  expect_error(
    native_collection_params_call(cyclic),
    "Cyclic ParamSetCollection graph is unsupported",
    fixed = TRUE
  )
})

test_that("serialized and cloned exact collections remain admissible", {
  skip_if_not(native_collection_params_available())
  original = native_collection_rich()
  cases = list(
    unserialize(serialize(original, NULL)),
    original$clone(deep = FALSE),
    original$clone(deep = TRUE)
  )
  for (collection in cases) {
    direct = native_collection_params_call(collection)
    expect_false(is.null(direct))
    expect_identical(direct, native_collection_params_reference(collection))
  }
})

test_that("native collection params survives forced collection", {
  skip_on_cran()

  skip_if_not(native_collection_params_available())
  collection = native_collection_rich()
  expected = native_collection_params_reference(collection)
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = native_collection_params_call(collection)
  gctorture(previous)
  expect_identical(observed, expected)
})
