# A dependency whose `on` names nothing in its own set is resolved outward
# through the enclosing collection namespaces and then against the reading
# root's flat table. Every consumer -- the `$deps` getter, `$check()`,
# `$get_values()`, designs, and the constraint adapter -- has to use that one
# scope, or the same object answers two different questions about the same
# dependency.

dangling_child = function(on) {
  set = ps(x = p_int(0, 10))
  set$add_dep("x", on, CondEqual(2L), allow_dangling_dependencies = TRUE)
  set
}

test_that("one dangling dependency gets one answer from every consumer", {
  collection = ParamSetCollection$new(list(
    s = dangling_child("t.y"),
    t = ps(y = p_int(0, 10))
  ))

  # The getter translates the parent outward; it is the spelling every other
  # consumer has to resolve.
  expect_identical(collection$deps$id, "s.x")
  expect_identical(collection$deps$on, "t.y")

  expect_true(collection$check(list(s.x = 1L, t.y = 2L)))
  expect_true(collection$test(list(s.x = 1L, t.y = 2L)))
  expect_match(
    collection$check(list(s.x = 1L, t.y = 3L)),
    "condition is met 't.y == 2'"
  )
  expect_match(
    collection$check(list(s.x = 1L)),
    "value for 't.y' is not set at all"
  )

  # `$get_values()` filters by the same activity, and a detached subset of the
  # very same object may not disagree with the live one.
  collection$values = list(s.x = 1L, t.y = 2L)
  expect_identical(collection$get_values(), list(s.x = 1L, t.y = 2L))
  expect_true(collection$check(collection$get_values()))
  expect_true(collection$subset(c("s.x", "t.y"))$check(collection$get_values()))

  collection$values = list(s.x = 1L, t.y = 3L)
  expect_identical(collection$values, list(s.x = 1L, t.y = 3L))
  expect_identical(collection$get_values(), list(t.y = 3L))

  # Designs consume the same resolved edge: `s.x` is only drawn where the
  # condition holds.
  design = generate_design_grid(
    ParamSetCollection$new(list(
      s = dangling_child("t.y"),
      t = ps(y = p_int(1, 2))
    )),
    resolution = 2L
  )$data
  expect_identical(design$t.y, c(1L, 2L, 2L))
  expect_identical(is.na(design$s.x), c(TRUE, FALSE, FALSE))
})

test_that("a dangling parent starts being enforced when a sibling supplies it", {
  collection = ParamSetCollection$new(list(s = dangling_child("t.y")))
  expect_identical(collection$deps$on, "t.y")
  # Nothing supplies `t.y` yet, so the edge is never satisfiable.
  expect_match(
    collection$check(list(s.x = 1L)),
    "value for 't.y' is not set at all"
  )

  collection$add(ps(y = p_int(0, 10)), "t")

  # No explicit refresh: the next read heals the flatten and resolves the edge.
  expect_identical(collection$ids(), c("s.x", "t.y"))
  expect_identical(collection$deps$on, "t.y")
  expect_true(collection$check(list(s.x = 1L, t.y = 2L)))
  expect_match(
    collection$check(list(s.x = 1L, t.y = 3L)),
    "current parameter value is: t.y == 3"
  )
})

test_that("outward resolution walks through nested collection namespaces", {
  inner = ParamSetCollection$new(list(
    a = dangling_child("b.y"),
    b = ps(y = p_int(0, 10))
  ))
  outer = ParamSetCollection$new(list(o = inner))

  expect_identical(outer$deps$id, "o.a.x")
  expect_identical(outer$deps$on, "o.b.y")
  expect_true(outer$check(list(o.a.x = 1L, o.b.y = 2L)))
  expect_match(
    outer$check(list(o.a.x = 1L, o.b.y = 3L)),
    "current parameter value is: o.b.y == 3"
  )

  # The inner collection is its own reading root and answers in its own
  # namespace.
  expect_identical(inner$deps$on, "b.y")
  expect_true(inner$check(list(a.x = 1L, b.y = 2L)))
})

test_that("a parent spelled in no reachable namespace stays never-satisfiable", {
  collection = ParamSetCollection$new(list(
    s = dangling_child("y"),
    t = ps(y = p_int(0, 10))
  ))

  # `y` is the child-local spelling of `t.y`, which no namespace on the path
  # exposes; it must not resolve in any consumer.
  expect_identical(collection$deps$on, "y")
  expect_match(
    collection$check(list(s.x = 1L, t.y = 2L)),
    "value for 'y' is not set at all"
  )
  collection$values = list(s.x = 1L, t.y = 2L)
  expect_identical(collection$get_values(), list(t.y = 2L))
})

test_that("a child constraint sees the same active values as check", {
  constrained = dangling_child("t.y")
  seen = NULL
  constrained$constraint = function(x) {
    seen <<- names(x)
    TRUE
  }
  collection = ParamSetCollection$new(list(
    s = constrained,
    t = ps(y = p_int(0, 10))
  ))

  expect_true(collection$test_constraint(list(s.x = 1L, t.y = 2L)))
  expect_identical(seen, "x")

  seen = NULL
  expect_true(collection$test_constraint(list(s.x = 1L, t.y = 3L)))
  expect_identical(seen, character())
})
