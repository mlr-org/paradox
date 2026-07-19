collection_deps_symbol = function() {
  get(
    "C_param_set_collection_deps",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

collection_deps_native = function(x) {
  .Call(collection_deps_symbol(), x$.__enclos_env__$private, x)
}

collection_deps_child = function(ids, dependencies = list()) {
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

test_that("collection deps native entry has one fixed capsule interface", {
  symbol = collection_deps_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("collection deps preserve postorder and every affix layer", {
  left = collection_deps_child(c("a", "b", "c"), list(
    list("b", "a", 1L),
    list("c", "b", 2L)
  ))
  right = collection_deps_child(
    c("x", "y"),
    list(list("y", "x", 3L))
  )
  inner = ParamSetCollection$new(list(inner = left), postfix_names = TRUE)
  inner$add_dep("c.inner", "a.inner", CondEqual(4L))
  outer = ParamSetCollection$new(list(outer = inner, right = right))
  outer$add_dep("right.y", "outer.a.inner", CondEqual(5L))

  observed = collection_deps_native(outer)
  expect_identical(observed$id, c(
    "outer.b.inner", "outer.c.inner", "outer.c.inner",
    "right.y", "right.y"
  ))
  expect_identical(observed$on, c(
    "outer.a.inner", "outer.b.inner", "outer.a.inner",
    "right.x", "outer.a.inner"
  ))
  expect_identical(outer$deps, observed)
  expect_identical(class(observed), c("data.table", "data.frame"))
  expect_null(data.table::key(observed))
  expect_null(data.table::indices(observed))
  expect_identical(data.table:::selfrefok(observed, FALSE), 1L)
})

test_that("collection deps keep dangling endpoints and repeat DAG occurrences", {
  child = collection_deps_child(c("x", "y"))
  child$add_dep("y", "foreign", CondEqual(1L),
    allow_dangling_dependencies = TRUE)
  shared = ParamSetCollection$new(list(first = child, second = child))

  observed = shared$deps
  expect_identical(observed$id, c("first.y", "second.y"))
  expect_identical(observed$on, c("foreign", "foreign"))
})

test_that("collection deps reflect live dependencies through shadows", {
  origin = collection_deps_child(c("a", "b"))
  shadow = ParamSetShadow$new(origin, character())
  collection = ParamSetCollection$new(list(left = shadow, right = shadow))
  expect_identical(nrow(collection$deps), 0L)

  origin$add_dep("b", "a", CondEqual(2L))
  expect_identical(collection$deps$id, c("left.b", "right.b"))
  expect_identical(collection$deps$on, c("left.a", "right.a"))
})

test_that("collection dependency facades own mutable shells", {
  child = collection_deps_child(
    c("a", "b"),
    list(list("b", "a", 1L))
  )
  collection = ParamSetCollection$new(list(child = child))
  first = collection$deps
  second = collection$deps

  expect_identical(first, second)
  for (column in names(first)) {
    expect_false(identical(
      data.table::address(first[[column]]),
      data.table::address(second[[column]])
    ), info = column)
  }
  data.table::set(first, i = 1L, j = "id", value = "changed")
  first$cond[[1L]]$rhs = 99L
  expect_identical(collection$deps, second)
})

test_that("collection deps return canonical named empty and reject cycles", {
  empty = ParamSetCollection$new(list())$deps
  expect_identical(dim(empty), c(0L, 3L))
  expect_identical(names(empty), c("id", "on", "cond"))

  collection = ParamSetCollection$new(list())
  paradox:::param_set_core_replace(
    collection$.__enclos_env__$private,
    sets = list(self = collection)
  )
  expect_error(collection$deps, "cycle", ignore.case = TRUE)
})
