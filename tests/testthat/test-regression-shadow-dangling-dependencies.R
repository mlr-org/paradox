# A dangling dependency -- one whose `on` names no existing parameter -- is an
# ordinary state of a ParamSet, so a view over such a set is ordinary too: the
# row is shown verbatim, enforced as never-satisfiable, and starts being
# enforced for real once the origin supplies the parent. Only a dependency that
# spans the visible/hidden boundary is refused.

dangling_origin = function() {
  origin = ps(hidden = p_lgl(), x = p_int(0, 10), flag = p_lgl())
  origin$add_dep("x", "future", CondEqual(2L), allow_dangling_dependencies = TRUE)
  origin
}

test_that("a view over a dangling-dependency origin is constructible and usable", {
  origin = dangling_origin()
  view = ParamSetShadow$new(origin, "hidden")

  expect_identical(view$ids(), c("x", "flag"))
  expect_identical(view$deps$id, "x")
  expect_identical(view$deps$on, "future")
  expect_true(view$has_deps)

  # Exactly the origin's own answer for the visible slice.
  expect_identical(
    view$check(list(x = 1L)),
    origin$check(list(x = 1L))
  )
  expect_true(view$check(list(flag = TRUE)))
  expect_false(view$test(list(x = 1L)))
})

test_that("an existing view survives an origin that gains a dangling dependency", {
  origin = ps(hidden = p_lgl(), x = p_int(0, 10), flag = p_lgl())
  view = ParamSetShadow$new(origin, "hidden")
  expect_identical(nrow(view$deps), 0L)

  origin$add_dep("x", "future", CondEqual(2L), allow_dangling_dependencies = TRUE)

  expect_identical(view$deps$on, "future")
  expect_identical(view$ids(), c("x", "flag"))
  expect_match(view$check(list(x = 1L)), "value for 'future' is not set at all")
})

test_that("an enclosing collection does not resolve a view's dangling parent", {
  origin = dangling_origin()
  view = ParamSetShadow$new(origin, "hidden")
  expect_match(view$check(list(x = 1L)), "value for 'future' is not set at all")

  grown = ParamSetCollection$new(list(o = origin))
  grown$add(ps(future = p_int(0, 10)), "")
  # `origin` did not change, but the same names now live in one collection.
  expect_identical(grown$ids(), c("o.hidden", "o.x", "o.flag", "future"))

  # The collection is a reading root that does supply the name, so it enforces
  # the edge ...
  expect_true(grown$check(list(o.x = 1L, future = 2L)))
  # ... while the view, whose own origin is still the BASE set, keeps the row
  # dangling. The reading root decides the scope, not some enclosing object.
  expect_identical(view$deps$on, "future")
  expect_match(view$check(list(x = 1L)), "value for 'future' is not set at all")
})

test_that("a view over a collection resolves a dangling parent when a set arrives", {
  child = ps(x = p_int(0, 10))
  child$add_dep("x", "t.y", CondEqual(2L), allow_dangling_dependencies = TRUE)
  origin = ParamSetCollection$new(list(s = child, h = ps(hide = p_lgl())))
  view = ParamSetShadow$new(origin, "h.hide")

  expect_identical(view$ids(), "s.x")
  expect_identical(view$deps$on, "t.y")
  expect_match(view$check(list(s.x = 1L)), "value for 't.y' is not set at all")

  origin$add(ps(y = p_int(0, 10)), "t")

  # No explicit refresh anywhere: the next read heals the flatten, re-derives
  # the projection, and resolves the edge.
  expect_identical(view$ids(), c("s.x", "t.y"))
  expect_identical(view$deps$on, "t.y")
  expect_true(view$check(list(s.x = 1L, t.y = 2L)))
  expect_match(
    view$check(list(s.x = 1L, t.y = 3L)),
    "current parameter value is: t.y == 3"
  )
})

test_that("live dangling resolution survives forced collection", {
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)

  child = ps(x = p_int(0, 10))
  child$add_dep("x", "t.y", CondEqual(2L), allow_dangling_dependencies = TRUE)
  origin = ParamSetCollection$new(list(s = child, h = ps(hide = p_lgl())))
  view = ParamSetShadow$new(origin, "h.hide")
  before = view$deps$on
  origin$add(ps(y = p_int(0, 10)), "t")
  after = view$check(list(s.x = 1L, t.y = 2L))
  ids = view$ids()
  gctorture(previous)

  expect_identical(before, "t.y")
  expect_true(after)
  expect_identical(ids, c("s.x", "t.y"))
})

test_that("a dependency between hidden parameters stays invisible", {
  origin = ps(hidden = p_lgl(), other = p_int(0, 10), x = p_int(0, 10))
  origin$add_dep("other", "hidden", CondEqual(TRUE))
  origin$add_dep("hidden", "gone", CondEqual(2L), allow_dangling_dependencies = TRUE)
  view = ParamSetShadow$new(origin, c("hidden", "other"))

  expect_identical(view$ids(), "x")
  expect_identical(nrow(view$deps), 0L)
  expect_false(view$has_deps)
  expect_true(view$check(list(x = 1L)))
})

test_that("a crossing dependency is refused in both directions, early and late", {
  condition = CondEqual(TRUE)

  crossing = ps(hidden = p_lgl(), x = p_int(0, 10))
  crossing$add_dep("x", "hidden", condition)
  expect_error(
    ParamSetShadow$new(crossing, "hidden"),
    "Dependency of visible 'x' on hidden 'hidden' crosses the ParamSetShadow boundary",
    fixed = TRUE
  )

  reverse = ps(hidden = p_lgl(), flag = p_lgl())
  reverse$add_dep("hidden", "flag", condition)
  expect_error(
    ParamSetShadow$new(reverse, "hidden"),
    "Dependency of hidden 'hidden' on visible 'flag' crosses the ParamSetShadow boundary",
    fixed = TRUE
  )

  # The same rows are refused when the origin gains them later, on every read
  # surface, and a dangling row that is already present changes nothing.
  late = dangling_origin()
  view = ParamSetShadow$new(late, "hidden")
  expect_identical(view$deps$on, "future")
  late$add_dep("flag", "hidden", condition)
  expect_error(
    view$ids(),
    "Dependency of visible 'flag' on hidden 'hidden' crosses the ParamSetShadow boundary",
    fixed = TRUE
  )
  expect_error(
    view$check(list(x = 1L)),
    "Dependency of visible 'flag' on hidden 'hidden' crosses the ParamSetShadow boundary",
    fixed = TRUE
  )
})

test_that("add_dep through a view distinguishes hidden from absent parents", {
  fresh = function() {
    origin = ps(hidden = p_lgl(), x = p_int(0, 10), flag = p_lgl())
    list(origin = origin, view = ParamSetShadow$new(origin, "hidden"))
  }

  # {visible, hidden, absent} x {FALSE, TRUE}, on both endpoints.
  for (allow in c(FALSE, TRUE)) {
    case = fresh()
    case$view$add_dep("x", "flag", CondEqual(TRUE),
      allow_dangling_dependencies = allow)
    expect_identical(case$origin$deps$id, "x")
    expect_identical(case$origin$deps$on, "flag")

    case = fresh()
    expect_error(
      case$view$add_dep("x", "hidden", CondEqual(TRUE),
        allow_dangling_dependencies = allow),
      "Dependency of visible 'x' on hidden 'hidden' crosses the ParamSetShadow boundary",
      fixed = TRUE
    )
    expect_identical(nrow(case$origin$deps), 0L)

    case = fresh()
    expect_error(
      case$view$add_dep("hidden", "x", CondEqual(1L),
        allow_dangling_dependencies = allow),
      "'hidden' is hidden by this ParamSetShadow",
      fixed = TRUE
    )
    expect_identical(nrow(case$origin$deps), 0L)

    case = fresh()
    expect_error(
      case$view$add_dep("nope", "x", CondEqual(1L),
        allow_dangling_dependencies = allow),
      "`id` is not a parameter in this ParamSet",
      fixed = TRUE
    )
    expect_identical(nrow(case$origin$deps), 0L)
  }

  case = fresh()
  expect_error(
    case$view$add_dep("x", "future", CondEqual(2L)),
    "`on` is not a parameter in this ParamSet",
    fixed = TRUE
  )
  expect_identical(nrow(case$origin$deps), 0L)

  # The declared row lands in the origin, where every other observer sees it.
  case$view$add_dep("x", "future", CondEqual(2L),
    allow_dangling_dependencies = TRUE)
  expect_identical(case$origin$deps$on, "future")
  expect_identical(case$view$deps$on, "future")
  expect_identical(
    ParamSetShadow$new(case$origin, "flag")$deps$on,
    "future"
  )
  expect_identical(
    case$view$check(list(x = 1L)),
    case$origin$check(list(x = 1L))
  )
})

test_that("a dangling parent can never resolve to a hidden parameter", {
  # The hidden set is fixed at construction from then-existing IDs, and IDs are
  # unique, so an ID the origin gains later is always visible to the view: a
  # dangling parent can only ever resolve inside the visible slice. A BASE
  # origin cannot gain a parameter at all ...
  base_origin = ps(hidden = p_lgl(), x = p_int(0, 10))
  base_view = ParamSetShadow$new(base_origin, "hidden")
  expect_error(
    base_view$add_dep("x", "hidden", CondEqual(TRUE),
      allow_dangling_dependencies = TRUE),
    "crosses the ParamSetShadow boundary",
    fixed = TRUE
  )

  # ... and a collection origin refuses the ID that would resurrect a hidden
  # one, so the dangling row stays dangling.
  origin = ParamSetCollection$new(list(
    h = ps(hidden = p_lgl()),
    s = ps(x = p_int(0, 10))
  ))
  view = ParamSetShadow$new(origin, "h.hidden")
  expect_error(
    view$add_dep("s.x", "h.hidden", CondEqual(TRUE),
      allow_dangling_dependencies = TRUE),
    "crosses the ParamSetShadow boundary",
    fixed = TRUE
  )
  view$add_dep("s.x", "h.later", CondEqual(TRUE),
    allow_dangling_dependencies = TRUE)

  expect_error(
    origin$add(ps(h.hidden = p_lgl()), ""),
    "nameclashes: h.hidden",
    fixed = TRUE
  )
  expect_identical(view$ids(), "s.x")
  expect_identical(view$deps$on, "h.later")

  # The parent that does arrive is visible, and the row starts being enforced.
  origin$add(ps(h.later = p_lgl()), "")
  expect_identical(view$ids(), c("s.x", "h.later"))
  expect_true(view$check(list(s.x = 1L, h.later = TRUE)))
})

test_that("every read surface of a dangling view equals the origin modulo hiding", {
  for (origin in list(
    dangling_origin(),
    ParamSetCollection$new(list(o = dangling_origin()))
  )) {
    prefix = if (inherits(origin, "ParamSetCollection")) "o." else ""
    hide = paste0(prefix, "hidden")
    id = paste0(prefix, "x")
    flag = paste0(prefix, "flag")
    view = ParamSetShadow$new(origin, hide)

    expect_identical(view$ids(), c(id, flag))
    expect_identical(view$deps$id, id)
    expect_identical(view$deps$on, "future")
    expect_identical(nrow(as.data.table(view)), 2L)
    expect_identical(view$class[[id]], "ParamInt")

    origin$values = structure(list(TRUE), names = hide)
    view$values = structure(list(1L), names = id)
    expect_identical(names(origin$values), c(hide, id))
    expect_identical(names(view$values), id)
    # The dependency-inactive value is stored dormant and filtered on read,
    # exactly as in the origin.
    expect_identical(view$get_values(), named_list())
    expect_identical(names(origin$get_values()), hide)

    design = generate_design_grid(view, resolution = 2L)$data
    expect_true(all(is.na(design[[id]])))
    expect_identical(nrow(design), 2L)

    unif = generate_design_random(view, 3L)$data
    expect_true(all(is.na(unif[[id]])))

    qunif = view$qunif(structure(
      as.data.frame(matrix(0.5, nrow = 1L, ncol = 2L)),
      names = c(flag, id)
    ))
    expect_identical(ncol(qunif), 2L)

    expect_error(
      view$subset(id),
      "Set `allow_dangling_dependencies = TRUE` to retain it",
      fixed = TRUE
    )
    expect_identical(
      view$subset(id, allow_dangling_dependencies = TRUE)$deps$on,
      "future"
    )
    expect_identical(view$flatten()$deps$on, "future")

    # Detached copies keep the row, and a re-derivation afterwards still holds.
    expect_identical(view$clone(deep = TRUE)$deps$on, "future")
    restored = unserialize(serialize(view, NULL))
    expect_identical(restored$deps$on, "future")
    expect_identical(restored$ids(), c(id, flag))
    expect_identical(view$deps$on, "future")

    expect_output(print(view), "future")
  }
})

test_that("a dangling parent is dropped from a search space, as in a plain set", {
  origin = dangling_origin()
  view = ParamSetShadow$new(origin, "hidden")
  view$values = list(x = to_tune())

  # `x`'s parent is not among the tuned IDs, so the tuning space carries no
  # dependency at all -- the same silent drop a plain ParamSet performs.
  space = view$search_space()
  expect_identical(space$ids(), "x")
  expect_identical(nrow(space$deps), 0L)
})
