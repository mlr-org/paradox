context("contract: ParamSetCollection dependencies")

psc_deps_child = function(ids, dependencies = list()) {
  result = ParamSet$new(setNames(
    lapply(ids, function(id) p_int(0L, 9L)),
    ids
  ))
  for (dependency in dependencies) {
    result$add_dep(
      dependency[[1L]],
      dependency[[2L]],
      dependency[[3L]]
    )
  }
  result
}

expect_deps_facade = function(deps, rows) {
  expect_identical(dim(deps), c(as.integer(rows), 3L))
  expect_identical(names(deps), c("id", "on", "cond"))
  expect_identical(
    vapply(deps, typeof, character(1L)),
    c(id = "character", on = "character", cond = "list")
  )
  expect_identical(class(deps), c("data.table", "data.frame"))
  expect_null(data.table::key(deps))
  expect_null(data.table::indices(deps))
  expect_identical(data.table:::selfrefok(deps, FALSE), 1L)
}

test_that("collection deps are one registered native operation", {
  symbol = get(
    "C_param_set_collection_deps",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_collection_deps", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("collection deps retain prefix, postfix, nesting, and row order", {
  left = psc_deps_child(c("a", "b", "c"), list(
    list("b", "a", CondEqual(1L)),
    list("c", "b", CondAnyOf(c(2L, 3L)))
  ))
  right = psc_deps_child(
    c("x", "y"),
    list(list("y", "x", CondEqual(3L)))
  )

  prefix = ParamSetCollection$new(list(left = left, right = right))
  prefix$add_dep("right.y", "left.a", CondEqual(4L))
  observed = prefix$deps
  expect_identical(
    observed$id,
    c("left.b", "left.c", "right.y", "right.y")
  )
  expect_identical(
    observed$on,
    c("left.a", "left.b", "right.x", "left.a")
  )
  expect_identical(
    lapply(observed$cond, class),
    list(
      c("CondEqual", "Condition"),
      c("CondAnyOf", "Condition"),
      c("CondEqual", "Condition"),
      c("CondEqual", "Condition")
    )
  )

  postfix = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  postfix$add_dep("y.right", "a.left", CondEqual(4L))
  expect_identical(
    postfix$deps$id,
    c("b.left", "c.left", "y.right", "y.right")
  )
  expect_identical(
    postfix$deps$on,
    c("a.left", "b.left", "x.right", "a.left")
  )

  inner = ParamSetCollection$new(list(inner = left))
  inner$add_dep("inner.c", "inner.a", CondEqual(5L))
  nested = ParamSetCollection$new(list(outer = inner, sibling = right))
  nested$add_dep("sibling.y", "outer.inner.a", CondEqual(6L))
  expect_identical(nested$deps$id, c(
    "outer.inner.b",
    "outer.inner.c",
    "outer.inner.c",
    "sibling.y",
    "sibling.y"
  ))
  expect_identical(nested$deps$on, c(
    "outer.inner.a",
    "outer.inner.b",
    "outer.inner.a",
    "sibling.x",
    "outer.inner.a"
  ))

  postfix_inner = ParamSetCollection$new(
    list(inner = left),
    postfix_names = TRUE
  )
  mixed = ParamSetCollection$new(list(outer = postfix_inner))
  expect_identical(mixed$deps$id, c("outer.b.inner", "outer.c.inner"))
})

test_that("collection dependencies are live child semantics", {
  child = psc_deps_child(c("a", "b", "c"))
  collection = ParamSetCollection$new(list(child = child))
  expect_deps_facade(collection$deps, 0L)

  condition = CondEqual(2L)
  child$add_dep("b", "a", condition)
  first = collection$deps
  expect_identical(first$id, "child.b")
  expect_identical(first$on, "child.a")
  expect_identical(first$cond[[1L]], condition)

  child$add_dep("c", "b", CondAnyOf(c(3L, 4L)))
  second = collection$deps
  expect_identical(second$id, c("child.b", "child.c"))
  expect_identical(second$on, c("child.a", "child.b"))
  expect_identical(second$cond[[2L]], CondAnyOf(c(3L, 4L)))

  # add_dep() snapshots the condition; later source mutation cannot change the
  # child or collection capsule.
  condition$rhs = 9L
  expect_identical(child$deps$cond[[1L]]$rhs, 2L)
  expect_identical(collection$deps$cond[[1L]]$rhs, 2L)
})

test_that("shared-child DAGs emit one translated occurrence per edge", {
  child = psc_deps_child(
    c("a", "b", "c"),
    list(list("c", "a", CondEqual(1L)))
  )
  shared = ParamSetCollection$new(list(first = child, second = child))
  expect_identical(shared$deps$id, c("first.c", "second.c"))
  expect_identical(shared$deps$on, c("first.a", "second.a"))

  outer = ParamSetCollection$new(list(shared = shared, direct = child))
  expect_identical(
    outer$deps$id,
    c("shared.first.c", "shared.second.c", "direct.c")
  )
  expect_identical(
    outer$deps$on,
    c("shared.first.a", "shared.second.a", "direct.a")
  )
})

test_that("deps facades do not alias capsule shells or semantic operands", {
  child = psc_deps_child(c("a", "b"))
  child$add_dep("b", "a", CondEqual(2L))
  collection = ParamSetCollection$new(list(child = child))

  first = collection$deps
  second = collection$deps
  expect_deps_facade(first, 1L)
  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  for (column in names(first)) {
    expect_false(
      identical(data.table::address(first[[column]]), data.table::address(second[[column]])),
      info = column
    )
  }
  first$id[[1L]] = "changed"
  first$cond[[1L]]$rhs[[1L]] = 8L
  fresh = collection$deps
  expect_identical(fresh$id, "child.b")
  expect_identical(fresh$cond[[1L]]$rhs[[1L]], 2L)
})

test_that("empty dependency results are complete detached facades", {
  empty = ParamSetCollection$new(list())
  first = empty$deps
  second = empty$deps
  expect_deps_facade(first, 0L)
  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))

  children = ParamSetCollection$new(setNames(
    replicate(16L, ParamSet$new(), simplify = FALSE),
    sprintf("empty%02d", seq_len(16L))
  ))
  expect_deps_facade(children$deps, 0L)

  expect_error(
    {
      empty$deps = data.table::data.table()
    },
    "deps is read-only in ParamSetCollection.",
    fixed = TRUE
  )
})

test_that("closed Condition admission rejects extensions deterministically", {
  child = psc_deps_child(c("a", "b"))
  unknown = structure(
    list(rhs = 1L, condition_format_string = "%s custom %s"),
    class = c("CustomCondition", "Condition")
  )
  expect_error(
    child$add_dep("b", "a", unknown),
    "Unsupported Condition class",
    fixed = TRUE
  )
  expect_deps_facade(child$deps, 0L)
})

test_that("randomized public collection graphs preserve exact edge order", {
  set.seed(20260714L)
  for (seed in seq_len(30L)) {
    child_count = seed %% 5L
    expected_id = character()
    expected_on = character()
    children = lapply(seq_len(child_count), function(child_index) {
      size = 1L + (seed + child_index) %% 5L
      ids = sprintf("p%02d", seq_len(size))
      result = psc_deps_child(ids)
      if (size >= 2L) {
        row_count = (seed + child_index) %% 4L
        for (row in seq_len(row_count)) {
          pair = sample(ids, 2L)
          result$add_dep(
            pair[[1L]],
            pair[[2L]],
            CondEqual(sample.int(9L, 1L) - 1L)
          )
          owner = sprintf("set%02d", child_index)
          expected_id <<- c(expected_id, sprintf("%s.%s", owner, pair[[1L]]))
          expected_on <<- c(expected_on, sprintf("%s.%s", owner, pair[[2L]]))
        }
      }
      result
    })
    names(children) = if (child_count) {
      sprintf("set%02d", seq_len(child_count))
    } else {
      character()
    }

    collection = ParamSetCollection$new(children)
    expect_identical(collection$deps$id, expected_id, info = sprintf("seed %d", seed))
    expect_identical(collection$deps$on, expected_on, info = sprintf("seed %d", seed))
  }
})
