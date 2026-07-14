native_value_mutation_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"), inherits = FALSE)
}

native_value_merge = function(dots, values = list(), current = NULL,
    insert = FALSE) {
  .Call(
    native_value_mutation_symbol("param_set_values_merge"),
    dots,
    values,
    current,
    insert
  )
}

native_value_store = function(param_set, values) {
  .Call(
    native_value_mutation_symbol("param_set_store_values"),
    param_set$.__enclos_env__$private,
    param_set,
    values
  )
}

native_value_assign_checked = function(param_set, values) {
  .Call(
    native_value_mutation_symbol("param_set_assign_values_checked"),
    param_set$.__enclos_env__$private,
    param_set,
    values
  )
}

native_collection_store_plan = function(collection, values,
    sets = collection$sets) {
  .Call(
    native_value_mutation_symbol("param_set_collection_store_plan"),
    collection$.__enclos_env__$private,
    collection,
    sets,
    values
  )
}

test_that("value mutation routines are forced and have fixed arities", {
  expected = c(
    param_set_values_merge = 4L,
    param_set_store_values = 3L,
    param_set_assign_values_checked = 3L,
    param_set_collection_store_plan = 4L
  )
  for (name in names(expected)) {
    symbol = native_value_mutation_symbol(name)
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, expected[[name]])
  }
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
  expect_error(
    .Call("param_set_values_merge", PACKAGE = "paradox"),
    "not available",
    fixed = TRUE
  )
})

test_that("native merge retains insertion, replacement, and NULL rules", {
  reference = new.env(parent = emptyenv())
  current = list(a = 1L, b = 2L, nullable = NULL, reference = reference)

  inserted = native_value_merge(
    list(b = 3L, nullable = NULL),
    list(c = 4L),
    current,
    TRUE
  )
  expect_identical(
    inserted,
    list(a = 1L, b = 3L, reference = reference, c = 4L)
  )
  expect_false(identical(
    data.table::address(inserted),
    data.table::address(current)
  ))

  replacement = native_value_merge(
    list(nullable = NULL),
    list(b = 5L),
    NULL,
    FALSE
  )
  expect_identical(replacement, list(nullable = NULL, b = 5L))
  expect_identical(native_value_merge(list(), list()), list())

  expect_null(native_value_merge(
    structure(list(1L, 2L), names = c("a", "a")),
    list()
  ))
  expect_null(native_value_merge(
    structure(list(a = 1L), class = "extension"),
    list()
  ))
  expect_null(native_value_merge(list(a = 1L), list(a = 2L)))
})

test_that("checked assignment commits only canonical successful values", {
  param_set = ps(
    count = p_int(0L, 10L, tolerance = 0.2),
    ratio = p_dbl(0, 1, tolerance = 0.1),
    payload = p_uty()
  )
  param_set$values = list(count = 1L, ratio = 0.5)

  returned = native_value_assign_checked(
    param_set,
    list(ratio = 1.05, count = 2.1)
  )
  expect_identical(returned, list(ratio = 1, count = 2L))
  expect_identical(param_set$values, list(count = 2L, ratio = 1))

  before = param_set$values
  expect_null(native_value_assign_checked(
    param_set,
    list(count = 50L)
  ))
  expect_identical(param_set$values, before)
  expect_null(native_value_assign_checked(
    param_set,
    list(payload = "opaque")
  ))
  expect_identical(param_set$values, before)

  events = new.env(parent = emptyenv())
  events$count = 0L
  constrained = ps(
    value = p_int(),
    .constraint = function(x) {
      events$count = events$count + 1L
      TRUE
    }
  )
  constrained$assert_values = FALSE
  constrained$values = list(value = 1L)
  constrained$assert_values = TRUE
  expect_null(native_value_assign_checked(
    constrained,
    list(value = 2L)
  ))
  expect_identical(events$count, 0L)
  expect_identical(constrained$values, list(value = 1L))

  dependent = ps(parent = p_int(), child = p_int())
  dependent$add_dep("child", "parent", CondEqual(1L))
  expect_null(native_value_assign_checked(
    dependent,
    list(parent = 1L, child = 2L)
  ))
  expect_identical(dependent$values, structure(list(), names = character()))
})

test_that("checked assignment declines forged generated wrappers", {
  replace_locked = function(environment, name, replacement) {
    unlockBinding(name, environment)
    assign(name, replacement, envir = environment)
    lockBinding(name, environment)
  }

  param_set = ps(a = p_int())
  param_set$values = list(a = 1L)
  private = param_set$.__enclos_env__$private
  calls = new.env(parent = emptyenv())
  calls$count = 0L
  original = param_set$assert
  replace_locked(param_set, "assert", function(xs, ...) {
    calls$count = calls$count + 1L
    original(xs, ...)
  })
  expect_null(native_value_assign_checked(param_set, list(a = 2L)))
  expect_identical(calls$count, 0L)
  expect_identical(private$.values, list(a = 1L))
  param_set$values = list(a = 2L)
  expect_identical(calls$count, 1L)
  expect_identical(private$.values, list(a = 2L))

  param_set = ps(a = p_int())
  param_set$values = list(a = 1L)
  private = param_set$.__enclos_env__$private
  calls$count = 0L
  original = param_set$check
  replace_locked(param_set, "check", function(xs, ...) {
    calls$count = calls$count + 1L
    original(xs, ...)
  })
  expect_null(native_value_assign_checked(param_set, list(a = 2L)))
  expect_identical(calls$count, 0L)
  expect_identical(private$.values, list(a = 1L))
  param_set$values = list(a = 2L)
  expect_identical(calls$count, 1L)
  expect_identical(private$.values, list(a = 2L))

  param_set = ps(a = p_int())
  param_set$values = list(a = 1L)
  private = param_set$.__enclos_env__$private
  calls$count = 0L
  original = private$.store_values
  replace_locked(private, ".store_values", function(xs) {
    calls$count = calls$count + 1L
    original(xs)
  })
  expect_null(native_value_assign_checked(param_set, list(a = 2L)))
  expect_identical(calls$count, 0L)
  expect_identical(private$.values, list(a = 1L))
  param_set$values = list(a = 2L)
  expect_identical(calls$count, 1L)
  expect_identical(private$.values, list(a = 2L))

  param_set = ps(a = p_int())
  param_set$values = list(a = 1L)
  private = param_set$.__enclos_env__$private
  calls$count = 0L
  original = activeBindingFunction("values", param_set)
  makeActiveBinding("values", function(xs) {
    calls$count = calls$count + 1L
    if (missing(xs)) original() else original(xs)
  }, param_set)
  expect_null(native_value_assign_checked(param_set, list(a = 2L)))
  expect_identical(calls$count, 0L)
  expect_identical(private$.values, list(a = 1L))
  param_set$values = list(a = 2L)
  expect_identical(calls$count, 1L)
  expect_identical(private$.values, list(a = 2L))

  param_set = ps(a = p_int())
  param_set$values = list(a = 1L)
  private = param_set$.__enclos_env__$private
  calls$count = 0L
  namespace = asNamespace("paradox")
  implementation = get(
    ".__ParamSet__values",
    envir = namespace,
    inherits = FALSE
  )
  forged_parent = new.env(parent = namespace)
  forged_parent$.__ParamSet__values = function(self, private, super, xs) {
    calls$count = calls$count + 1L
    implementation(self, private, super, xs)
  }
  wrapper_environment = environment(activeBindingFunction(
    "values",
    param_set
  ))
  parent.env(wrapper_environment) = forged_parent

  expect_null(native_value_assign_checked(param_set, list(a = 2L)))
  expect_identical(calls$count, 0L)
  expect_identical(private$.values, list(a = 1L))
  param_set$values = list(a = 2L)
  expect_identical(calls$count, 1L)
  expect_identical(private$.values, list(a = 2L))

  param_set = ps(a = p_int())
  param_set$values = list(a = 1L)
  private = param_set$.__enclos_env__$private
  calls$count = 0L
  namespace = asNamespace("paradox")
  implementation = get(
    ".__ParamSet__values",
    envir = namespace,
    inherits = FALSE
  )
  wrapper_environment = environment(activeBindingFunction(
    "values",
    param_set
  ))
  delayedAssign(
    ".__ParamSet__values",
    {
      calls$count = calls$count + 1L
      function(self, private, super, xs) {
        implementation(self, private, super, xs)
      }
    },
    eval.env = environment(),
    assign.env = wrapper_environment
  )

  expect_null(native_value_assign_checked(param_set, list(a = 2L)))
  expect_identical(calls$count, 0L)
  expect_identical(private$.values, list(a = 1L))
  param_set$values = list(a = 2L)
  expect_identical(calls$count, 1L)
  expect_identical(private$.values, list(a = 2L))
})

test_that("checked assignment does not execute forged metadata bindings", {
  param_set = ps(payload = p_uty())
  private = param_set$.__enclos_env__$private
  calls = new.env(parent = emptyenv())
  calls$deps = 0L
  calls$constraint = 0L
  original_deps = activeBindingFunction("deps", param_set)
  original_constraint = activeBindingFunction("constraint", param_set)
  makeActiveBinding("deps", function(value) {
    if (missing(value)) {
      calls$deps = calls$deps + 1L
      original_deps()
    } else {
      original_deps(value)
    }
  }, param_set)
  makeActiveBinding("constraint", function(value) {
    if (missing(value)) {
      calls$constraint = calls$constraint + 1L
      original_constraint()
    } else {
      original_constraint(value)
    }
  }, param_set)

  expect_null(native_value_assign_checked(
    param_set,
    list(payload = "native-decline")
  ))
  expect_identical(calls$deps, 0L)
  expect_identical(calls$constraint, 0L)
  expect_identical(private$.values, structure(list(), names = character()))

  param_set$values = list(payload = "r-fallback")
  expect_identical(calls$deps, 1L)
  # Ordinary ParamSets have always read the private constraint store through
  # test_constraint(); only collections synthesize it from the public getter.
  expect_identical(calls$constraint, 0L)
  expect_identical(private$.values, list(payload = "r-fallback"))
})

test_that("native storage orders, filters, and owns the value shell", {
  param_set = ps(a = p_int(), b = p_uty())
  param_set$assert_values = FALSE
  reference = new.env(parent = emptyenv())
  incoming = structure(
    list(reference, 2L, 9L, "ignored"),
    names = c("b", "a", "a", "unknown")
  )

  stored = native_value_store(param_set, incoming)
  expect_identical(stored, list(a = 2L, b = reference))
  expect_identical(param_set$values, stored)
  expect_false(identical(
    data.table::address(stored),
    data.table::address(incoming)
  ))
  incoming[[2L]] = 8L
  expect_identical(param_set$values$a, 2L)
  expect_identical(param_set$values$b, reference)

  before = param_set$values
  expect_null(native_value_store(param_set, list()))
  expect_identical(param_set$values, before)
  expect_null(native_value_store(param_set, list(1L, 2L)))
  expect_identical(param_set$values, before)
  expect_identical(
    param_set$.__enclos_env__$private$.store_values(list(1L, 2L)),
    list()
  )
  expect_identical(param_set$values, list())

  Subclass = R6::R6Class(
    "NativeValueStoreSubclass",
    inherit = ParamSet
  )
  subclass = Subclass$new(list(a = p_int()))
  expect_null(native_value_store(subclass, list(a = 1L)))
})

test_that("private native value storage retains invisible assignment semantics", {
  param_set = ps(x = p_int())
  private = param_set$.__enclos_env__$private

  observed = withVisible(private$.store_values(list(x = 1L)))
  expect_false(observed$visible)
  expect_identical(observed$value, list(x = 1L))
  expect_identical(param_set$values, list(x = 1L))
})

test_that("native storage rejects delayed and malformed state before forcing", {
  param_set = ps(a = p_int())
  private = param_set$.__enclos_env__$private
  events = new.env(parent = emptyenv())
  events$count = 0L
  delayedAssign(
    ".values",
    {
      events$count = events$count + 1L
      list(a = 1L)
    },
    assign.env = private,
    eval.env = environment()
  )
  expect_null(native_value_store(param_set, list(a = 2L)))
  expect_identical(events$count, 0L)
  expect_identical(param_set$values, list(a = 1L))
  expect_identical(events$count, 1L)

  malformed = ps(a = p_int())
  malformed_private = malformed$.__enclos_env__$private
  malformed_private$.params$id[[1L]] = NA_character_
  before = malformed_private$.values
  expect_null(native_value_store(malformed, list(a = 2L)))
  expect_identical(malformed_private$.values, before)
})

test_that("collection plan groups touched children before clearing", {
  one = ps(a = p_int(), b = p_int())
  two = ps(c = p_int())
  three = ps(d = p_int())
  collection = ParamSetCollection$new(list(
    one = one,
    two = two,
    three = three
  ))

  plan = native_collection_store_plan(
    collection,
    list(three.d = 4L, one.b = 2L, one.a = 1L, unknown = 9L)
  )
  expect_identical(plan[[1L]], c(1L, 3L, 2L))
  expect_identical(
    plan[[2L]],
    list(
      list(b = 2L, a = 1L),
      list(d = 4L),
      structure(list(), names = character())
    )
  )

  collection$assert_values = FALSE
  collection$values = list(
    three.d = 4L,
    one.b = 2L,
    one.a = 1L,
    unknown = 9L
  )
  expect_identical(one$values, list(a = 1L, b = 2L))
  expect_identical(two$values, structure(list(), names = character()))
  expect_identical(three$values, list(d = 4L))
})

test_that("collection planning supports postfix nesting and shared children", {
  shared = ps(a = p_int(), b = p_int())
  inner = ParamSetCollection$new(
    list(left = shared, right = shared),
    postfix_names = TRUE
  )
  tail = ps(q = p_dbl())
  outer = ParamSetCollection$new(
    list(inner = inner, tail = tail),
    postfix_names = TRUE
  )

  plan = native_collection_store_plan(
    outer,
    list(b.right.inner = 4L, a.left.inner = 1L, q.tail = 0.5)
  )
  expect_identical(plan[[1L]], c(1L, 2L))
  expect_identical(
    plan[[2L]],
    list(
      list(b.right = 4L, a.left = 1L),
      list(q = 0.5)
    )
  )

  outer$values = list(
    b.right.inner = 4L,
    a.left.inner = 1L,
    q.tail = 0.5
  )
  expect_identical(shared$values, list(b = 4L))
  expect_identical(tail$values, list(q = 0.5))
})

test_that("collection plan falls back for extensions and malformed storage", {
  child = ps(a = p_int())
  collection = ParamSetCollection$new(list(owner = child))
  private = collection$.__enclos_env__$private

  malformed = data.table::copy(private$.translation)
  malformed$id[[1L]] = "wrong.a"
  private$.translation = malformed
  expect_null(native_collection_store_plan(
    collection,
    list(owner.a = 1L)
  ))

  encoded = ParamSetCollection$new(list(owner = ps(a = p_int())))
  encoded_private = encoded$.__enclos_env__$private
  names(encoded_private$.sets) = "ownér"
  encoded_translation = data.table::copy(encoded_private$.translation)
  encoded_translation$id[[1L]] = "ownér.a"
  encoded_translation$owner_name[[1L]] = "ownér"
  encoded_private$.translation = encoded_translation
  expect_null(native_collection_store_plan(
    encoded,
    setNames(list(1L), "ownér.a")
  ))

  ExtendedCollection = R6::R6Class(
    "NativeValueStoreCollectionSubclass",
    inherit = ParamSetCollection
  )
  extended = ExtendedCollection$new(list(owner = ps(a = p_int())))
  expect_null(native_collection_store_plan(
    extended,
    list(owner.a = 1L)
  ))
})

test_that("miesmuschel ParamSetShadow remains a public setter extension", {
  skip_if_not_installed("miesmuschel")
  origin = ps(x = p_int(), y = p_lgl())
  origin$values = list(x = 1L, y = TRUE)
  ParamSetShadow = getExportedValue("miesmuschel", "ParamSetShadow")
  shadow = ParamSetShadow$new(origin, "x")
  collection = ParamSetCollection$new(list(shadow = shadow))

  collection$values = list(shadow.y = FALSE)
  expect_identical(origin$values, list(x = 1L, y = FALSE))
  expect_identical(shadow$values, list(y = FALSE))
  collection$values = structure(list(), names = character())
  expect_identical(origin$values, list(x = 1L))
})

test_that("reentrant custom checks may replace parameter storage and collect", {
  skip_on_cran()

  events = new.env(parent = emptyenv())
  events$count = 0L
  holder = new.env(parent = emptyenv())
  param_set = ps(payload = p_uty(custom_check = function(value) {
    events$count = events$count + 1L
    private = holder$param_set$.__enclos_env__$private
    private$.params = data.table::copy(private$.params)
    gc(FALSE)
    TRUE
  }))
  holder$param_set = param_set
  events$count = 0L

  # The native success-only validator does not invoke extension callbacks.
  expect_null(native_value_assign_checked(
    param_set,
    list(payload = "first")
  ))
  expect_identical(events$count, 0L)

  previous = gctorture2(1L, wait = 0L)
  on.exit(gctorture2(previous), add = TRUE)
  param_set$values = list(payload = "second")
  observed = param_set$values
  gctorture2(previous)
  expect_identical(events$count, 1L)
  expect_identical(observed, list(payload = "second"))
})

test_that("native mutation entries and public setters survive gctorture", {
  skip_on_cran()

  param_set = ps(a = p_int(), b = p_dbl())
  param_set$values = list(a = 1L, b = 0.25)
  other = ps(flag = p_lgl(init = TRUE))
  collection = ParamSetCollection$new(list(left = param_set, right = other))

  previous = gctorture2(1L, wait = 0L)
  on.exit(gctorture2(previous), add = TRUE)
  merged = native_value_merge(
    list(a = 2L),
    list(),
    param_set$values,
    TRUE
  )
  checked = native_value_assign_checked(param_set, merged)
  plan = native_collection_store_plan(
    collection,
    list(left.a = 3L, right.flag = FALSE)
  )
  collection$set_values(left.a = 3L, right.flag = FALSE, .insert = FALSE)
  observed = collection$values
  gctorture2(previous)

  expect_identical(merged, list(a = 2L, b = 0.25))
  expect_identical(checked, merged)
  expect_identical(plan[[1L]], c(1L, 2L))
  expect_identical(observed, list(left.a = 3L, right.flag = FALSE))
})
