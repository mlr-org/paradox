# Both graph builders expanded every *occurrence* of a shared node. That is by
# design when the subtree exposes parameters -- each occurrence contributes its
# own affixed IDs, so the work is bounded by the result -- but a shared subtree
# that exposes nothing at all was still re-expanded once per path. An
# alternating graph of 41 objects with no parameters therefore needed 2^20 node
# snapshots to produce an empty answer: $check(list()) took 19s and $values 1.2s.

barren_chain = function(depth) {
  graph = ps()
  for (level in seq_len(depth)) {
    graph = ParamSetCollection$new(list(l = graph, r = graph))
  }
  graph
}

test_that("a shared parameter-free subtree is expanded once, not once per path", {
  graph = barren_chain(20L)
  expect_identical(graph$ids(), character(0))

  elapsed = system.time({
    expect_true(graph$check(list()))
    expect_identical(graph$values, named_list())
    expect_identical(graph$ids(), character(0))
    expect_equal(nrow(as.data.table(graph)), 0L)
  })[["elapsed"]]
  expect_lt(elapsed, 5)
})

test_that("shared alternating shadow/collection graphs stay flat", {
  # Construction is part of the pinned bound: nothing at or above a Shadow
  # can hold a verification stamp, and the post-order capsule-graph heal once
  # re-healed the shared origin subtree per shadow occurrence, which made
  # this exact loop cost Theta(4^depth).
  construction = system.time({
    graph = ps()
    for (level in seq_len(18L)) {
      shared = ParamSetShadow$new(
        ParamSetCollection$new(list(n = graph)),
        character(0)
      )
      graph = ParamSetCollection$new(list(a = shared, b = shared))
    }
  })[["elapsed"]]
  expect_lt(construction, 5)
  elapsed = system.time({
    expect_true(graph$check(list()))
    expect_identical(graph$values, named_list())
  })[["elapsed"]]
  expect_lt(elapsed, 5)
})

test_that("a sharing subtree that contributes is still expanded per occurrence", {
  # Every occurrence exposes its own affixed IDs, so the node count is the
  # result size. Pruning must not touch this.
  graph = ps(v = p_dbl(0, 1))
  for (level in seq_len(6L)) {
    graph = ParamSetCollection$new(list(l = graph, r = graph))
  }
  expect_length(graph$ids(), 2^6)
  expect_true(all(grepl("^[lr](\\.[lr])*\\.v$", graph$ids())))
  expect_true(graph$check(list()))

  # One shared leaf underlies every ID, so the last plan wins -- announced
  # exactly once for that one target, not once per redundant path to it.
  expect_warning(
    graph$values <- setNames(as.list(seq_along(graph$ids()) / 100), graph$ids()),
    "more than one path"
  )
  expect_length(unique(unlist(graph$values)), 1L)
})

test_that("a barren shared subtree that carries a callback is not pruned", {
  # A constraint or extra_trafo is a contribution even with no parameters: it
  # runs once per occurrence, and pruning would silently drop those calls.
  leaf = ps()
  calls = 0L
  leaf$extra_trafo = function(x, param_set) {
    calls <<- calls + 1L
    x
  }
  collection = ParamSetCollection$new(list(a = leaf, b = leaf))
  expect_identical(collection$ids(), character(0))
  collection$trafo(list())
  expect_identical(calls, 2L)
})

test_that("pruning does not hide corrupt or cyclic graphs", {
  leaf = ps()
  collection = ParamSetCollection$new(list(a = leaf, b = leaf))
  outer = ParamSetCollection$new(list(x = collection, y = collection))
  expect_true(outer$check(list()))

  # A self-referential edge is still rejected after the shared child below it
  # was pruned on its repeated occurrences.
  expect_error(outer$add(outer, "self"), "cycle", ignore.case = TRUE)

  # Adding the already-shared child a third time is legal sharing, not a cycle.
  expect_silent(outer$add(collection, "again"))

  # And the pruned graph is still fully usable afterwards.
  expect_true(outer$check(list()))
  expect_identical(outer$ids(), character(0))
})

test_that("pruned graphs survive gctorture", {
  skip_on_cran()
  graph = barren_chain(12L)
  gctorture(TRUE)
  on.exit(gctorture(FALSE), add = TRUE)
  for (iteration in seq_len(3L)) {
    expect_true(graph$check(list()))
    expect_identical(graph$values, named_list())
  }
})
