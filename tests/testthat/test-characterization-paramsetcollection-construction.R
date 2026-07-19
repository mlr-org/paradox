context("contract: ParamSetCollection construction")

test_that("construction preserves child order, translated IDs, and tags", {
  left = ps(
    zeta = p_int(tags = c("set_left", "param_zeta")),
    alpha = p_dbl(tags = "numeric", trafo = sqrt)
  )
  right = ps(flag = p_lgl())
  collection = ParamSetCollection$new(
    list(left = left, right = right),
    tag_sets = TRUE,
    tag_params = TRUE
  )

  expect_identical(
    collection$ids(),
    c("left.zeta", "left.alpha", "right.flag")
  )
  # Existing tags precede generated tags and existing/generated duplicates
  # remain observable compatibility behavior.
  expect_identical(collection$tags, list(
    left.zeta = c("set_left", "param_zeta", "set_left", "param_zeta"),
    left.alpha = c("numeric", "set_left", "param_alpha"),
    right.flag = c("set_right", "param_flag")
  ))
  expect_identical(collection$params$.trafo[[2L]], sqrt)
  expect_identical(names(collection$sets), c("left", "right"))
  expect_identical(collection$sets[[1L]], left)
  expect_identical(collection$sets[[2L]], right)
})

test_that("postfix and unnamed owners preserve public spelling", {
  left = ps(zeta = p_int(), alpha = p_dbl())
  right = ps(flag = p_lgl())
  postfix = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  expect_identical(postfix$ids(), c("zeta.left", "alpha.left", "flag.right"))

  unnamed = ParamSetCollection$new(list(ps(x = p_int()), ps(y = p_lgl())))
  expect_identical(names(unnamed$sets), c("", ""))
  expect_identical(unnamed$ids(), c("x", "y"))

  mixed = ParamSetCollection$new(setNames(list(left, right), c("named", "")))
  expect_identical(mixed$ids(), c("named.zeta", "named.alpha", "flag"))
})

test_that("constructor fixes schema metadata and keeps live child semantics", {
  child = ps(
    enabled = p_lgl(tags = "initial"),
    amount = p_int(trafo = as.double)
  )
  child$values = list(enabled = TRUE, amount = 1L)
  collection = ParamSetCollection$new(list(component = child))
  schema_before = collection$params

  child$tags = list(enabled = "changed", amount = "changed")
  child$extra_trafo = function(x) {
    x$amount = x$amount + 1L
    x
  }
  child$constraint = function(x) x$amount <= 2L
  child$add_dep("amount", "enabled", CondEqual(TRUE))
  child$assert_values = FALSE
  child$values = list(enabled = FALSE, amount = 2L)

  expect_identical(collection$params$.tags, schema_before$.tags)
  expect_identical(collection$tags, list(
    component.enabled = "initial",
    component.amount = character()
  ))
  expect_identical(collection$values, list(
    component.enabled = FALSE,
    component.amount = 2L
  ))
  expect_identical(collection$deps$id, "component.amount")
  expect_identical(collection$deps$on, "component.enabled")
  expect_identical(
    collection$trafo(list(component.amount = 1L))$component.amount,
    2
  )
  expect_true(is.function(collection$constraint))
})

test_that("constructor validates public inputs deterministically", {
  child = ps(x = p_int())
  expect_error(
    ParamSetCollection$new(list(owner = child, owner = child)),
    "unique names",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(setNames(list(child), NA_character_)),
    "is NA at position 1",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(list("not a ParamSet")),
    "ParamSet",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(list(child), tag_sets = NA),
    "May not be NA",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(list(child), tag_params = 1),
    "logical flag",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(list(child), postfix_names = NULL),
    "logical flag",
    fixed = TRUE
  )

  first = ps(x = p_int())
  second = ps(x = p_lgl())
  expect_error(
    ParamSetCollection$new(setNames(list(first, second), c("", ""))),
    "Cannot construct ParamSetCollection",
    fixed = TRUE
  )
})

test_that("public metadata facades are detached and callbacks retain identity", {
  marker = new.env(parent = emptyenv())
  trafo = local({
    held = marker
    function(x) {
      invisible(held)
      x
    }
  })
  checker = local({
    held = marker
    function(x) {
      invisible(held)
      TRUE
    }
  })
  child = ps(
    x = p_dbl(0, 1, tags = c("first", "second"), trafo = trafo),
    payload = p_uty(custom_check = checker)
  )
  collection = ParamSetCollection$new(list(owner = child))

  first = collection$params
  second = collection$params
  expect_identical(first, second)
  expect_false(identical(data.table::address(first), data.table::address(second)))
  expect_identical(first$.trafo[[1L]], trafo)
  expect_identical(first$cargo[[2L]]$custom_check, checker)

  data.table::set(first, 1L, "lower", -100)
  data.table::set(first, 1L, ".tags", list("changed"))
  data.table::set(first, 1L, ".trafo", list(log))
  fresh = collection$params
  expect_identical(fresh$lower[[1L]], 0)
  expect_identical(fresh$.tags[[1L]], c("first", "second"))
  expect_identical(fresh$.trafo[[1L]], trafo)
  expect_identical(child$params$lower[[1L]], 0)
})

test_that("empty, nested, and shared child graphs retain references", {
  empty = ParamSetCollection$new(list())
  expect_identical(empty$ids(), character())
  expect_identical(names(empty$sets), character())
  expect_identical(dim(empty$params), c(0L, 16L))

  leaf = ps(x = p_int(), y = p_lgl())
  inner = ParamSetCollection$new(list(inner = leaf))
  outer = ParamSetCollection$new(list(left = inner, right = leaf))
  expect_identical(
    outer$ids(),
    c("left.inner.x", "left.inner.y", "right.x", "right.y")
  )
  expect_identical(outer$sets[[1L]], inner)
  expect_identical(outer$sets[[2L]], leaf)

  shared = ParamSetCollection$new(list(first = leaf, second = leaf))
  expect_identical(shared$sets[[1L]], shared$sets[[2L]])
  expect_identical(
    shared$ids(),
    c("first.x", "first.y", "second.x", "second.y")
  )
})

test_that("clone and serialization preserve collection graph topology", {
  child = ps(
    enabled = p_lgl(init = TRUE),
    amount = p_int(0L, 3L, tags = "quantity", trafo = as.double)
  )
  child$add_dep("amount", "enabled", CondEqual(TRUE))
  collection = ParamSetCollection$new(list(first = child, second = child))

  shallow = collection$clone()
  deep = collection$clone(deep = TRUE)
  restored = unserialize(serialize(collection, NULL, version = 3L))
  expect_identical(shallow$sets[[1L]], child)
  expect_false(identical(deep$sets[[1L]], child))
  expect_identical(deep$sets[[1L]], deep$sets[[2L]])
  expect_identical(restored$sets[[1L]], restored$sets[[2L]])
  expect_false(identical(restored$sets[[1L]], child))
  expect_identical(restored$ids(), collection$ids())
  expect_identical(restored$tags, collection$tags)
  expect_identical(restored$values, collection$values)
  expect_identical(restored$deps, collection$deps)
})

test_that("strict ASCII IDs translate and non-strict encodings are rejected", {
  child = ParamSet$new(list(cafe = p_int(tags = "encoded", trafo = identity)))
  collection = ParamSetCollection$new(list(owner = child))
  expect_identical(collection$ids(), "owner.cafe")
  expect_identical(names(collection$tags), "owner.cafe")

  encoded = enc2utf8("caf\u00e9")
  expect_error(
    ParamSet$new(setNames(list(p_int()), encoded)),
    "strict ASCII IDs",
    fixed = TRUE
  )

  bytes = rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))
  Encoding(bytes) = "bytes"
  expect_error(
    ParamSet$new(setNames(list(p_int()), bytes)),
    "strict ASCII IDs",
    fixed = TRUE
  )
})

test_that("small rich construction survives forced collection", {
  skip_on_cran()

  left = ps(
    x = p_dbl(0, 1, tags = "numeric", trafo = sqrt),
    y = p_int(0L, 3L)
  )
  right = ps(flag = p_lgl())
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = ParamSetCollection$new(
    list(left = left, right = right),
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = TRUE
  )
  gctorture(previous)
  expect_identical(observed$ids(), c("x.left", "y.left", "flag.right"))
  expect_identical(observed$sets[[1L]], left)
  expect_identical(observed$sets[[2L]], right)
})
