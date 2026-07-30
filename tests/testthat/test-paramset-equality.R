test_that("all.equal compares detached ParamSet family state", {
  left = ps(x = p_dbl(0, 1), flag = p_lgl())
  right = ps(x = p_dbl(0, 1), flag = p_lgl())
  expect_true(isTRUE(all.equal(left, right)))

  right$values = list(x = 0.5)
  expect_match(all.equal(left, right), "init|params|values|Component")

  tagged = ps(x = p_dbl(0, 1), flag = p_lgl())
  tagged$tags = list(x = "numeric", flag = character())
  expect_match(all.equal(left, tagged), "tags|params|Component")

  dependent = ps(x = p_dbl(0, 1), flag = p_lgl())
  dependent$add_dep("x", "flag", CondEqual(TRUE))
  expect_match(all.equal(left, dependent), "deps|params|Component")

  left_collection = ParamSetCollection$new(list(
    component = ps(x = p_dbl(0, 1))
  ))
  right_collection = ParamSetCollection$new(list(
    component = ps(x = p_dbl(0, 1))
  ))
  expect_true(isTRUE(all.equal(left_collection, right_collection)))
  expect_true(isTRUE(all.equal(
    left_collection,
    right_collection,
    check.environment = FALSE
  )))

  right_collection$sets$component$values = list(x = 0.25)
  expect_match(
    all.equal(left_collection, right_collection),
    "init|params|values|sets|Component"
  )
  expect_match(all.equal(left_collection, left), "class|Length|Component")
  expect_match(all.equal(left_collection, list()), "not a ParamSet")
})

test_that("all.equal projects Shadow origin semantics without adapter frames", {
  left_origin = ps(hidden = p_int(), shown = p_int())
  right_origin = ps(hidden = p_int(), shown = p_int())
  left = ParamSetShadow$new(left_origin, "hidden")
  right = ParamSetShadow$new(right_origin, "hidden")

  expect_true(isTRUE(all.equal(left, right)))

  right_origin$values = list(hidden = 2L)
  expect_match(all.equal(left, right), "origin|values|params|Component")

  right_origin$values = list()
  right_origin$constraint = function(x) TRUE
  expect_match(all.equal(left, right), "origin|constraint|Component")
})

test_that("all.equal rejects cyclic ParamSet graphs deterministically", {
  collection = ParamSetCollection$new(list())
  shadow = ParamSetShadow$new(collection, character())
  paradox:::param_set_core_replace(
    collection$.__enclos_env__$private,
    sets = list(cycle = shadow)
  )

  expect_error(
    all.equal(collection, collection),
    "capsule graph contains a cycle"
  )
})

test_that("all.equal includes public graph sharing semantics", {
  shared = ps(x = p_int())
  shared_graph = ParamSetCollection$new(list(a = shared, b = shared))
  duplicated_graph = ParamSetCollection$new(list(
    a = ps(x = p_int()),
    b = ps(x = p_int())
  ))
  another_leaf = ps(x = p_int())
  another_shared_graph = ParamSetCollection$new(list(
    a = another_leaf,
    b = another_leaf
  ))

  expect_true(isTRUE(all.equal(shared_graph, another_shared_graph)))
  expect_match(
    all.equal(shared_graph, duplicated_graph),
    "reference|node|Length|Component"
  )
})

test_that("all.equal snapshots one coherent graph before presentation reads", {
  source = ps(value = p_uty())
  mutator = new.env(parent = emptyenv())
  class(mutator) = c("paradox_equality_clone_mutator", "R6")
  mutator$clone = function(deep = FALSE) {
    source$values = list(value = "later")
    new.env(parent = emptyenv())
  }
  source$values = list(value = mutator)

  # A self-comparison takes one admitted snapshot. The opaque value's
  # established R6 clone callback may mutate the source while that snapshot is
  # being detached, but it cannot make one equality view combine the old
  # schema/value with a later live field.
  expect_true(isTRUE(all.equal(source, source)))
  expect_identical(source$values, list(value = "later"))
})
