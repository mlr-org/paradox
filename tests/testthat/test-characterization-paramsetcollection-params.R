context("contract: ParamSetCollection params")

rich_collection_child = function(prefix) {
  result = ParamSet$new(setNames(list(
    p_int(0L, 5L, tags = "integer", init = 1L),
    p_dbl(-1, 1, tags = "double", trafo = exp),
    p_lgl(tags = "logical")
  ), paste0(prefix, c("_count", "_scale", "_flag"))))
  result$add_dep(
    paste0(prefix, "_flag"),
    paste0(prefix, "_count"),
    CondEqual(1L)
  )
  result
}

test_that("prefix, postfix, and nested public row order is stable", {
  left = rich_collection_child("left")
  right = rich_collection_child("right")

  prefix = ParamSetCollection$new(list(a = left, b = right))
  expect_identical(prefix$params$id, c(
    "a.left_count", "a.left_scale", "a.left_flag",
    "b.right_count", "b.right_scale", "b.right_flag"
  ))

  postfix = ParamSetCollection$new(
    list(a = left, b = right),
    postfix_names = TRUE
  )
  expect_identical(postfix$params$id, c(
    "left_count.a", "left_scale.a", "left_flag.a",
    "right_count.b", "right_scale.b", "right_flag.b"
  ))

  inner = ParamSetCollection$new(list(inner = left))
  nested = ParamSetCollection$new(list(outer = inner, direct = right))
  expect_identical(nested$params$id, c(
    "outer.inner.left_count",
    "outer.inner.left_scale",
    "outer.inner.left_flag",
    "direct.right_count",
    "direct.right_scale",
    "direct.right_flag"
  ))
  expect_identical(nested$params$.requirements[[3L]][[1L]],
    "outer.inner.left_count")
  expect_identical(nested$params$.requirements[[6L]][[1L]],
    "direct.right_count")
})

test_that("empty collection params retain the complete facade", {
  params = ParamSetCollection$new(list())$params
  expect_identical(dim(params), c(0L, 16L))
  expect_identical(names(params), paradox:::domain_names)
  expect_identical(class(params), c("data.table", "data.frame"))
  expect_identical(data.table::key(params), NULL)
  expect_identical(data.table::indices(params), NULL)
  expect_identical(data.table:::selfrefok(params, FALSE), 1L)
  expect_identical(vapply(params, typeof, character(1L)), c(
    id = "character", cls = "character", grouping = "character",
    cargo = "list", lower = "double", upper = "double",
    tolerance = "double", levels = "list", special_vals = "list",
    default = "list", storage_type = "character", .tags = "list",
    .trafo = "list", .requirements = "list", .init_given = "logical",
    .init = "list"
  ))
})

test_that("params combine fixed schema with live child semantics", {
  child = ps(
    choice = p_fct(c("slow", "fast"), tags = "initial", init = "fast"),
    amount = p_dbl(-2, 2, trafo = exp),
    flag = p_lgl()
  )
  collection = ParamSetCollection$new(list(child = child))
  fixed = collection$params

  child$tags = list(choice = "changed", amount = "changed", flag = "changed")
  child$values = list(choice = "slow", amount = 1)
  child$add_dep("flag", "choice", CondEqual("slow"))

  current = collection$params
  expect_identical(current$id, fixed$id)
  # A contained set's tags reach the flatten: `$tags<-` is a schema change and
  # every ancestor re-derives from it.
  expect_identical(current$.tags, list("changed", "changed", "changed"))
  expect_identical(current$.trafo[[2L]], exp)
  expect_identical(current$.init_given, c(TRUE, TRUE, FALSE))
  expect_identical(current$.init, list("slow", 1, NULL))
  expect_identical(current$.requirements[[3L]][[1L]], "child.choice")
  expect_identical(current$.requirements[[3L]][[2L]], CondEqual("slow"))
})

test_that("shared child params are emitted per graph edge", {
  child = ps(x = p_int(init = 2L), y = p_lgl(init = TRUE))
  collection = ParamSetCollection$new(list(first = child, second = child))
  expect_identical(
    collection$params$id,
    c("first.x", "first.y", "second.x", "second.y")
  )
  expect_identical(collection$params$.init, list(2L, TRUE, 2L, TRUE))

  child$values = list(x = 4L)
  expect_identical(collection$params$.init, list(4L, NULL, 4L, NULL))
})

test_that("returned params are detached from collection and child state", {
  marker = new.env(parent = emptyenv())
  child = ps(
    x = p_dbl(0, 1, trafo = sqrt),
    payload = p_uty(init = marker)
  )
  collection = ParamSetCollection$new(list(owner = child))
  first = collection$params
  second = collection$params

  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  expect_identical(first$.trafo[[1L]], sqrt)
  expect_identical(first$.init[[2L]], marker)

  data.table::set(first, 1L, "lower", -100)
  first$.init[[2L]] = NULL
  expect_identical(collection$params, second)
  expect_identical(child$params$lower[[1L]], 0)
})

test_that("randomized collection row order follows child insertion order", {
  set.seed(20260718L)
  for (seed in seq_len(30L)) {
    child_count = seed %% 6L
    children = lapply(seq_len(child_count), function(owner) {
      size = 1L + (seed + owner) %% 5L
      ids = sample(sprintf("p%02d", seq_len(8L)), size)
      ParamSet$new(setNames(replicate(size, p_int(), simplify = FALSE), ids))
    })
    names(children) = sprintf("set%02d", seq_along(children))
    collection = ParamSetCollection$new(children)
    expected = if (length(children)) {
      unlist(Map(
        function(child, owner) paste0(owner, ".", child$ids()),
        children,
        names(children)
      ), use.names = FALSE)
    } else {
      character()
    }
    expect_identical(collection$params$id, expected, info = sprintf("seed %d", seed))
  }
})
