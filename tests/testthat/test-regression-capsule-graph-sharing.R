# The capsule-graph walk validates every node once. Before, a graph that shares
# one child between two parents was re-walked once per distinct path, which is
# exponential in the sharing depth: a 56-node graph took 0.2s and larger shapes
# were unusable.

test_that("a shared capsule subgraph is validated once, not once per path", {
  build = function(depth) {
    graph = ps()
    for (level in seq_len(depth)) {
      shared = ParamSetShadow$new(
        ParamSetCollection$new(list(n = graph)),
        character(0)
      )
      graph = ParamSetCollection$new(list(a = shared, b = shared))
    }
    graph
  }

  # 4 * 20 = 80 objects, and no parameters at all: this measures only the walk.
  # The old walk visited 2^20 paths through them, both while building each
  # level and again for the shadow below.
  elapsed = system.time({
    graph = build(20L)
    shadow = ParamSetShadow$new(graph, character(0))
  })[["elapsed"]]
  expect_lt(elapsed, 5)
  expect_equal(shadow$ids(), character(0))
})

test_that("sharing does not change what the capsule graph walk accepts", {
  shared = ps(x = p_dbl(0, 1))
  left = ParamSetCollection$new(list(s = shared))
  right = ParamSetCollection$new(list(s = shared))
  root = ParamSetCollection$new(list(a = left, b = right))

  expect_equal(root$ids(), c("a.s.x", "b.s.x"))
  expect_equal(ParamSetShadow$new(root, "a.s.x")$ids(), "b.s.x")

  # The same child twice under one parent is still a shared subgraph.
  twice = ParamSetCollection$new(list(p = shared, q = shared))
  expect_equal(ParamSetShadow$new(twice, character(0))$ids(), c("p.x", "q.x"))
})

test_that("cycles are still rejected once nodes are remembered", {
  collection = ParamSetCollection$new(list())
  expect_error(collection$add(collection, "s"), "cycle")

  inner = ParamSetCollection$new(list())
  outer = ParamSetCollection$new(list(x = inner))
  expect_error(inner$add(outer, "y"), "cycle")
})

test_that("deep chains and wide fan-outs stay linear", {
  chain = ps(x = p_dbl(0, 1))
  for (level in seq_len(2000L)) chain = ParamSetCollection$new(list(n = chain))
  expect_equal(length(ParamSetShadow$new(chain, character(0))$ids()), 1L)

  children = setNames(
    lapply(seq_len(2000L), function(index) ps(x = p_dbl(0, 1))),
    paste0("c", seq_len(2000L))
  )
  wide = ParamSetCollection$new(children)
  expect_equal(length(ParamSetShadow$new(wide, character(0))$ids()), 2000L)
})

test_that("the visit table survives a collecting traversal", {
  shared = ps(x = p_dbl(0, 1))
  root = ParamSetCollection$new(list(
    a = ParamSetCollection$new(list(s = shared)),
    b = ParamSetCollection$new(list(s = shared))
  ))
  gctorture(TRUE)
  on.exit(gctorture(FALSE), add = TRUE)
  expect_equal(length(ParamSetShadow$new(root, character(0))$ids()), 2L)
})
