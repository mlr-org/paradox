heal_private = function(param_set) {
  param_set$.__enclos_env__$private
}

heal_state = function(param_set) {
  paradox:::param_set_core_state(heal_private(param_set))
}

heal_core = function(param_set) {
  heal_private(param_set)$.core
}

test_that("an ancestor collection follows a contained set that grows", {
  inner = ParamSetCollection$new(list(a = ps(x = p_dbl(0, 1))))
  outer = ParamSetCollection$new(list(o = inner))
  expect_identical(outer$ids(), "o.a.x")

  inner$add(ps(y = p_dbl(0, 1)), "b")

  # Every read surface, not just the ones that traverse the graph.
  expect_identical(outer$ids(), c("o.a.x", "o.b.y"))
  expect_identical(outer$length, 2L)
  expect_identical(outer$lower, c(o.a.x = 0, o.b.y = 0))
  expect_identical(outer$class, c(o.a.x = "ParamDbl", o.b.y = "ParamDbl"))
  expect_identical(nrow(as.data.table(outer)), 2L)
  expect_identical(outer$params$id, c("o.a.x", "o.b.y"))
  expect_identical(outer$tags, list(o.a.x = character(), o.b.y = character()))
  expect_identical(outer$values, named_list())
  expect_identical(nrow(outer$deps), 0L)
  expect_true(outer$check(list(o.a.x = 0.5, o.b.y = 0.5)))
  expect_identical(
    as.data.frame(outer$qunif(data.frame(o.a.x = 0.5, o.b.y = 0.25))),
    data.frame(o.a.x = 0.5, o.b.y = 0.25)
  )
  expect_identical(outer$subset("o.b.y")$ids(), "o.b.y")

  # ... including writing a value that only exists because of the growth.
  outer$values = list(o.b.y = 0.25)
  expect_identical(outer$values, list(o.b.y = 0.25))
  expect_identical(inner$values, list(b.y = 0.25))
})

test_that("assigning tags to a contained set reaches every ancestor", {
  child = ps(z = p_dbl(0, 1))
  parent = ParamSetCollection$new(list(s = child))
  grandparent = ParamSetCollection$new(list(g = parent))
  expect_identical(grandparent$ids(tags = "fresh"), character())

  child$tags = list(z = "fresh")

  expect_identical(parent$tags, list(s.z = "fresh"))
  expect_identical(parent$ids(tags = "fresh"), "s.z")
  expect_identical(grandparent$tags, list(g.s.z = "fresh"))
  expect_identical(grandparent$ids(tags = "fresh"), "g.s.z")
})

test_that("tags assigned on a derived set survive re-derivation", {
  child = ps(z = p_dbl(0, 1), w = p_lgl())
  collection = ParamSetCollection$new(list(s = child))
  collection$tags = list(s.z = "direct", s.w = character())

  # The assignment is the collection's own answer for the IDs it named: the
  # contained set is untouched and keeps answering for itself.
  expect_identical(collection$tags, list(s.z = "direct", s.w = character()))
  expect_identical(child$tags, list(z = character(), w = character()))
  expect_identical(collection$ids(tags = "direct"), "s.z")

  child$tags = list(z = "later", w = "later")
  expect_identical(child$tags, list(z = "later", w = "later"))
  expect_identical(collection$tags, list(s.z = "direct", s.w = character()))

  # An ID the assignment never named is still derived from its set.
  collection$add(ps(fresh = p_int(tags = "inherited")), "extra")
  expect_identical(collection$tags$extra.fresh, "inherited")
  expect_identical(collection$tags$s.z, "direct")

  grown = ParamSetCollection$new(list(g = child))
  outer = ParamSetCollection$new(list(o = grown))
  outer$tags = list(o.g.z = "outer", o.g.w = character())
  child$add_dep("w", "z", CondEqual(0.5))
  child$tags = list(z = "newer", w = "newer")
  expect_identical(outer$tags, list(o.g.z = "outer", o.g.w = character()))
  expect_identical(grown$tags, list(g.z = "newer", g.w = "newer"))

  expect_identical(
    unserialize(serialize(collection, NULL, version = 3L))$tags$s.z,
    "direct"
  )
  expect_identical(collection$clone(deep = TRUE)$tags$s.z, "direct")
})

test_that("a Shadow owns the tags it is assigned without touching its origin", {
  origin = ps(hidden = p_int(), visible = p_int(), other = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  shadow$tags = list(visible = "mine", other = character())

  expect_identical(shadow$tags, list(visible = "mine", other = character()))
  expect_identical(
    origin$tags,
    list(hidden = character(), visible = character(), other = character())
  )

  origin$tags = list(hidden = "h", visible = "theirs", other = "theirs")
  expect_identical(shadow$tags, list(visible = "mine", other = character()))
  expect_identical(shadow$clone(deep = TRUE)$tags$visible, "mine")

  # Two views over one set may disagree about the same parameter.
  second = ParamSetShadow$new(origin, "hidden")
  expect_identical(second$tags$visible, "theirs")
})

test_that("growth propagates through deeply nested and shared containment", {
  leaf = ps(x = p_int())
  level1 = ParamSetCollection$new(list(l1 = leaf))
  level2 = ParamSetCollection$new(list(l2 = level1))
  level3 = ParamSetCollection$new(list(l3 = level2))
  expect_identical(level3$ids(), "l3.l2.l1.x")

  leaf_extra = ps(y = p_int())
  level1$add(leaf_extra, "extra")
  expect_identical(level3$ids(), c("l3.l2.l1.x", "l3.l2.extra.y"))
  expect_identical(level2$ids(), c("l2.l1.x", "l2.extra.y"))

  # One shared child reached through two independent ancestors.
  shared = ParamSetCollection$new(list(s = ps(a = p_lgl())))
  left = ParamSetCollection$new(list(left = shared))
  right = ParamSetCollection$new(list(right = shared, other = ps(b = p_lgl())))
  shared$add(ps(c = p_lgl()), "grown")
  expect_identical(left$ids(), c("left.s.a", "left.grown.c"))
  expect_identical(right$ids(), c("right.s.a", "right.grown.c", "other.b"))

  # A node reachable twice on one path is still healed once and stays valid.
  diamond_leaf = ps(d = p_int())
  diamond_child = ParamSetCollection$new(list(dc = diamond_leaf))
  diamond = ParamSetCollection$new(list(l = diamond_child, r = diamond_child))
  diamond_child$add(ps(e = p_int()), "more")
  expect_identical(
    diamond$ids(),
    c("l.dc.d", "l.more.e", "r.dc.d", "r.more.e")
  )
})

test_that("per-edge tag flags survive a refresh", {
  flagged = ps(p = p_int())
  plain = ps(q = p_int())
  collection = ParamSetCollection$new(list(one = flagged))
  collection$add(plain, "two", tag_sets = TRUE, tag_params = TRUE)
  before = collection$tags

  # An edge that was empty when it was added carries no tag row at all until
  # its child grows, so its flags cannot be recovered from the flat tables.
  grown = ParamSetCollection$new(list())
  collection$add(grown, "four", tag_sets = TRUE, tag_params = TRUE)
  expect_identical(collection$tags[names(before)], before)
  grown$add(ps(r = p_int()), "base")
  grown$add(ps(s = p_int()), "later")

  expect_identical(collection$tags$one.p, character())
  expect_identical(collection$tags$two.q, c("set_two", "param_q"))
  expect_setequal(
    collection$tags$four.base.r,
    c("set_four", "param_base.r")
  )
  expect_setequal(
    collection$tags$four.later.s,
    c("set_four", "param_later.s")
  )
})

test_that("a name collision created below is reported with its context", {
  child = ps(x = p_int())
  collection = ParamSetCollection$new(list(a = child, b = ps(a.x = p_int())))
  expect_identical(collection$ids(), c("a.x", "b.a.x"))

  clash = ParamSetCollection$new(list(one = ps(v = p_int())))
  outer = ParamSetCollection$new(setNames(
    list(clash, ps(n.two.w = p_int())),
    c("n", "")
  ))
  expect_identical(outer$ids(), c("n.one.v", "n.two.w"))
  clash$add(ps(w = p_int()), "two")
  expect_error(outer$ids(), "after contained set 'n' changed")
  expect_error(outer$ids(), "'n.two.w' is not unique")
})

test_that("a refreshed collection matches one constructed from the same sets", {
  build = function() {
    list(one = ps(a = p_dbl(0, 1, tags = "keep")), two = ps(b = p_int()))
  }
  grown_parts = build()
  grown = ParamSetCollection$new(list(u = grown_parts$one))
  grown$add(grown_parts$two, "v", tag_sets = TRUE)
  inner_grown = ParamSetCollection$new(list(w = grown))
  grown_parts$one$tags = list(a = c("keep", "later"))

  fresh_parts = build()
  fresh_parts$one$tags = list(a = c("keep", "later"))
  fresh = ParamSetCollection$new(list(u = fresh_parts$one))
  fresh$add(fresh_parts$two, "v", tag_sets = TRUE)
  inner_fresh = ParamSetCollection$new(list(w = fresh))

  expect_identical(inner_grown$ids(), inner_fresh$ids())
  expect_identical(inner_grown$tags, inner_fresh$tags)
  expect_identical(
    as.data.table(inner_grown)$id,
    as.data.table(inner_fresh)$id
  )
})

test_that("a stale graph heals identically after serialization and cloning", {
  inner = ParamSetCollection$new(list(a = ps(x = p_dbl(0, 1))))
  outer = ParamSetCollection$new(list(o = inner))
  inner$add(ps(y = p_dbl(0, 1)), "b")

  restored = unserialize(serialize(outer, NULL, version = 3L))
  expect_identical(restored$ids(), c("o.a.x", "o.b.y"))

  cloned = outer$clone(deep = TRUE)
  expect_identical(cloned$ids(), c("o.a.x", "o.b.y"))

  # A clone taken before the change stays isolated from it.
  independent_inner = ParamSetCollection$new(list(a = ps(x = p_dbl(0, 1))))
  independent = ParamSetCollection$new(list(o = independent_inner))
  snapshot = independent$clone(deep = TRUE)
  independent_inner$add(ps(y = p_dbl(0, 1)), "b")
  expect_identical(snapshot$ids(), "o.a.x")
  expect_identical(independent$ids(), c("o.a.x", "o.b.y"))
})

test_that("a round trip that is already current revalidates without rebuilding", {
  inner = ParamSetCollection$new(list(a = ps(x = p_dbl(0, 1))))
  outer = ParamSetCollection$new(list(o = inner))
  expect_identical(outer$ids(), "o.a.x")

  restored = unserialize(serialize(outer, NULL, version = 3L))
  before = heal_state(restored)$.params
  expect_identical(restored$ids(), "o.a.x")
  # A restored capsule is unverified, but its recorded edge generations still
  # match, so the first read revalidates instead of re-flattening.
  expect_identical(heal_state(restored)$.params, before)
})

test_that("value writes leave every containing flatten untouched", {
  child = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  collection = ParamSetCollection$new(list(c = child))
  outer = ParamSetCollection$new(list(o = collection))
  expect_identical(outer$ids(), c("o.c.x", "o.c.y"))
  before_outer = heal_core(outer)
  before_inner = heal_core(collection)

  for (index in 1:5) {
    outer$values = list(o.c.x = index / 10, o.c.y = 0.5)
  }

  expect_identical(heal_core(outer), before_outer)
  expect_identical(heal_core(collection), before_inner)
  expect_identical(outer$values, list(o.c.x = 0.5, o.c.y = 0.5))
})

test_that("a Shadow exposes its origin minus the hidden set, live", {
  origin = ParamSetCollection$new(list(a = ps(x = p_dbl(0, 1))))
  shadow = ParamSetShadow$new(origin, character(0))
  expect_identical(shadow$ids(), "a.x")

  origin$add(ps(y = p_dbl(0, 1)), "b")
  expect_identical(shadow$ids(), c("a.x", "b.y"))
  expect_true(shadow$check(list(b.y = 0.5)))
  shadow$values = list(b.y = 0.25)
  expect_identical(origin$values, list(b.y = 0.25))

  # The hidden set is what a Shadow fixes, and it stays hidden.
  base = ps(hidden = p_int(), visible = p_int())
  hiding = ParamSetShadow$new(base, "hidden")
  expect_identical(hiding$ids(), "visible")
  base$tags = list(hidden = "h", visible = "v")
  expect_identical(hiding$tags, list(visible = "v"))
  expect_identical(hiding$ids(), "visible")
})

test_that("a Shadow inside a collection does not churn its ancestors", {
  base = ps(hidden = p_int(), visible = p_int())
  shadow = ParamSetShadow$new(base, "hidden")
  collection = ParamSetCollection$new(list(s = shadow))
  expect_identical(collection$ids(), "s.visible")
  before = heal_core(collection)

  base$values = list(hidden = 1L, visible = 2L)
  expect_identical(collection$values, list(s.visible = 2L))
  expect_identical(heal_core(collection), before)

  # A schema change to the origin does reach the collection.
  grown = ParamSetCollection$new(list(p = ps(a = p_int())))
  grown_shadow = ParamSetShadow$new(grown, character(0))
  grown_collection = ParamSetCollection$new(list(t = grown_shadow))
  expect_identical(grown_collection$ids(), "t.p.a")
  grown$add(ps(b = p_int()), "q")
  expect_identical(grown_collection$ids(), c("t.p.a", "t.q.b"))
})

test_that("healing survives forced collection at every step", {
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)

  leaf = ps(x = p_int())
  inner = ParamSetCollection$new(list(i = leaf))
  outer = ParamSetCollection$new(list(o = inner))
  inner$add(ps(y = p_int()), "grown")
  observed = outer$ids()
  leaf$tags = list(x = "tagged")
  tags = outer$tags
  gctorture(previous)

  expect_identical(observed, c("o.i.x", "o.grown.y"))
  expect_identical(tags$o.i.x, "tagged")
})
