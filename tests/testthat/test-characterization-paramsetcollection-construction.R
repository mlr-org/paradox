context("characterization: ParamSetCollection construction")

collection_constructor_private = function(param_set) {
  param_set$.__enclos_env__$private
}

collection_constructor_probe_binding = function(param_set, member, label,
    events) {
  private = collection_constructor_private(param_set)
  value = private[[member]]
  rm(list = member, envir = private)
  makeActiveBinding(
    member,
    local({
      snapshot = value
      event = sprintf("%s:%s", label, member)
      function(rhs) {
        if (!missing(rhs)) stop("probe storage is read-only")
        events$seen = c(events$seen, event)
        snapshot
      }
    }),
    private
  )
  invisible(param_set)
}

test_that("construction preserves child and row order but keys lookup tables", {
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
  private = collection_constructor_private(collection)

  expect_identical(
    collection$ids(),
    c("left.zeta", "left.alpha", "right.flag")
  )
  expect_identical(
    private$.params$id,
    c("left.zeta", "left.alpha", "right.flag")
  )
  expect_identical(
    private$.translation$id,
    c("left.alpha", "left.zeta", "right.flag")
  )
  expect_identical(
    private$.translation$original_id,
    c("alpha", "zeta", "flag")
  )
  expect_identical(private$.translation$owner_ps_index, c(1L, 1L, 2L))
  expect_identical(private$.translation$owner_name, c("left", "left", "right"))
  expect_identical(data.table::key(private$.translation), "id")
  expect_identical(data.table::indices(private$.translation), "original_id")

  # Existing tags precede generated tags for an ID, and generated duplicates
  # are deliberately retained.
  expect_identical(
    collection$tags,
    list(
      left.zeta = c("set_left", "param_zeta", "set_left", "param_zeta"),
      left.alpha = c("numeric", "set_left", "param_alpha"),
      right.flag = c("set_right", "param_flag")
    )
  )
  expect_identical(data.table::key(private$.tags), "id")
  expect_identical(private$.trafos$id, "left.alpha")
  expect_identical(private$.trafos$trafo[[1L]], sqrt)
  expect_identical(data.table::key(private$.trafos), "id")
  expect_identical(names(collection$sets), c("left", "right"))
  expect_identical(collection$sets[[1L]], left)
  expect_identical(collection$sets[[2L]], right)
})

test_that("postfix and unnamed owners retain the established translation", {
  left = ps(zeta = p_int(), alpha = p_dbl())
  right = ps(flag = p_lgl())
  postfix = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  private = collection_constructor_private(postfix)

  expect_identical(postfix$ids(), c("zeta.left", "alpha.left", "flag.right"))
  expect_identical(
    private$.translation$id,
    c("alpha.left", "flag.right", "zeta.left")
  )
  expect_identical(
    private$.translation$original_id,
    c("alpha", "flag", "zeta")
  )
  expect_identical(private$.translation$owner_ps_index, c(1L, 2L, 1L))
  expect_true(private$.postfix)

  unnamed = ParamSetCollection$new(list(ps(x = p_int()), ps(y = p_lgl())))
  expect_identical(names(unnamed$sets), c("", ""))
  expect_identical(unnamed$ids(), c("x", "y"))
  expect_identical(
    collection_constructor_private(unnamed)$.translation$owner_name,
    c("", "")
  )
})

test_that("initializer invisibly returns its normalized sets argument", {
  collection = ParamSetCollection$new(list())
  cases = list(
    empty = list(
      input = list(),
      expected = setNames(list(), character())
    ),
    named = list(
      input = list(component = ps(x = p_dbl())),
      expected = NULL
    ),
    unnamed = list(
      input = list(ps(x = p_int())),
      expected = NULL
    )
  )
  cases$named$expected = cases$named$input
  cases$unnamed$expected = setNames(cases$unnamed$input, "")

  for (case_name in names(cases)) {
    case = cases[[case_name]]
    result = withVisible(collection$initialize(case$input))
    expect_false(result$visible, info = case_name)
    expect_identical(result$value, case$expected, info = case_name)
    expect_identical(collection$sets, case$expected, info = case_name)
  }
})

test_that("construction snapshots metadata while retaining live child state", {
  child = ps(enabled = p_lgl(), amount = p_int())
  child$values = list(enabled = TRUE, amount = 1L)
  collection = ParamSetCollection$new(list(component = child))

  child$tags = list(enabled = "changed", amount = "changed")
  child$extra_trafo = function(x) {
    x$amount = x$amount + 1L
    x
  }
  child$constraint = function(x) x$amount <= 2L
  child$add_dep("amount", "enabled", CondEqual(TRUE))
  child$assert_values = FALSE
  child$values = list(enabled = FALSE, amount = 2L)

  expect_identical(collection$tags, list(
    component.enabled = character(),
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
    2L
  )
  expect_true(is.function(collection$constraint))
})

test_that("extension storage reads keep their historical construction order", {
  events = new.env(parent = emptyenv())
  events$seen = character()
  Probe = R6::R6Class(
    "CharacterizationCollectionConstructorProbe",
    inherit = ParamSet,
    lock_objects = FALSE
  )
  left = Probe$new(list(a = p_int(tags = "left", trafo = identity)))
  right = Probe$new(list(b = p_lgl(tags = "right")))
  for (member in c(".params", ".tags", ".trafos")) {
    collection_constructor_probe_binding(left, member, "left", events)
    collection_constructor_probe_binding(right, member, "right", events)
  }

  events$seen = character()
  collection = ParamSetCollection$new(
    list(one = left, two = right),
    tag_sets = TRUE,
    tag_params = TRUE
  )
  expect_s3_class(collection, "ParamSetCollection")
  expect_identical(events$seen, c(
    "left:.params", "right:.params",
    "left:.params", "left:.tags",
    "right:.params", "right:.tags",
    "left:.trafos", "right:.trafos"
  ))
})

test_that("constructor validation retains public diagnostics and order", {
  child = ps(x = p_int())
  expect_error(
    ParamSetCollection$new(list(owner = child, owner = child)),
    "Must have unique names",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(setNames(list(child), NA_character_)),
    "Must have names, but is NA at position 1",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(list("not a ParamSet")),
    "May only contain the following types: {ParamSet}",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(list(child), tag_sets = NA),
    "May not be NA",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(list(child), tag_params = 1),
    "Must be of type 'logical flag', not 'double'",
    fixed = TRUE
  )
  expect_error(
    ParamSetCollection$new(list(child), postfix_names = NULL),
    "Must be of type 'logical flag', not 'NULL'",
    fixed = TRUE
  )

  first = ps(x = p_int())
  second = ps(x = p_lgl())
  expect_error(
    ParamSetCollection$new(setNames(list(first, second), c("", ""))),
    "duplicated parameter names: x",
    fixed = TRUE
  )
})

test_that("construction owns metadata shells and preserves opaque leaves", {
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
  child_private = collection_constructor_private(child)
  collection = ParamSetCollection$new(list(owner = child))
  private = collection_constructor_private(collection)

  expect_false(identical(
    data.table::address(private$.params),
    data.table::address(child_private$.params)
  ))
  for (column in names(private$.params)) {
    expect_false(identical(
      data.table::address(private$.params[[column]]),
      data.table::address(child_private$.params[[column]])
    ), info = column)
  }
  expect_false(identical(
    data.table::address(private$.tags),
    data.table::address(child_private$.tags)
  ))
  expect_false(identical(
    data.table::address(private$.trafos),
    data.table::address(child_private$.trafos)
  ))
  expect_false(identical(
    data.table::address(private$.translation$id),
    data.table::address(private$.params$id)
  ))
  expect_identical(private$.trafos$trafo[[1L]], trafo)
  expect_identical(private$.params$cargo[[2L]]$custom_check, checker)

  data.table::set(private$.params, 1L, "lower", -100)
  data.table::set(private$.tags, 1L, "tag", "changed")
  data.table::set(private$.trafos, 1L, "trafo", list(log))
  expect_identical(child_private$.params$lower[[1L]], 0)
  expect_identical(child_private$.tags$tag, c("first", "second"))
  expect_identical(child_private$.trafos$trafo[[1L]], trafo)
})

test_that("empty, nested, and shared children retain their references", {
  empty = ParamSetCollection$new(list())
  empty_private = collection_constructor_private(empty)
  expect_identical(empty$ids(), character())
  expect_identical(names(empty$sets), character())
  expect_identical(nrow(empty_private$.params), 0L)
  expect_identical(nrow(empty_private$.translation), 0L)

  leaf = ps(x = p_int(), y = p_lgl())
  inner = ParamSetCollection$new(list(inner = leaf))
  outer = ParamSetCollection$new(list(left = inner, right = leaf))
  outer_private = collection_constructor_private(outer)
  expect_identical(
    outer$ids(),
    c("left.inner.x", "left.inner.y", "right.x", "right.y")
  )
  expect_identical(outer$sets[[1L]], inner)
  expect_identical(outer$sets[[2L]], leaf)
  expect_identical(
    outer_private$.translation$owner_ps_index,
    c(1L, 1L, 2L, 2L)
  )

  shared = ParamSetCollection$new(list(first = leaf, second = leaf))
  expect_identical(shared$sets[[1L]], shared$sets[[2L]])
  expect_identical(
    shared$ids(),
    c("first.x", "first.y", "second.x", "second.y")
  )
})

test_that("clone and serialization preserve constructor graph behavior", {
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
  expect_identical(restored$deps$id, collection$deps$id)
  expect_identical(restored$deps$on, collection$deps$on)
})

test_that("encoded child IDs keep the established fallback behavior", {
  encoded = enc2utf8("caf\u00e9")
  child = ps(x = p_int(tags = "encoded", trafo = identity))
  private = collection_constructor_private(child)
  data.table::set(private$.params, 1L, "id", encoded)
  data.table::set(private$.tags, 1L, "id", encoded)
  data.table::set(private$.trafos, 1L, "id", encoded)
  collection = ParamSetCollection$new(list(owner = child))
  expect_identical(enc2utf8(collection$ids()), "owner.caf\u00e9")
  expect_identical(enc2utf8(names(collection$tags)), "owner.caf\u00e9")
  expect_identical(enc2utf8(private$.params$id), "caf\u00e9")

  bytes = rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))
  Encoding(bytes) = "bytes"
  data.table::set(private$.params, 1L, "id", bytes)
  data.table::set(private$.tags, 1L, "id", bytes)
  data.table::set(private$.trafos, 1L, "id", bytes)
  expect_error(
    ParamSetCollection$new(list(owner = child)),
    "translating strings with \"bytes\" encoding is not allowed",
    fixed = TRUE
  )
})

test_that("delayed extension storage is forced only by the R constructor", {
  events = new.env(parent = emptyenv())
  events$seen = character()
  Probe = R6::R6Class(
    "CharacterizationCollectionConstructorDelayedProbe",
    inherit = ParamSet,
    lock_objects = FALSE
  )
  child = Probe$new(list(x = p_int(tags = "tag", trafo = identity)))
  private = collection_constructor_private(child)
  for (member in c(".params", ".tags", ".trafos")) {
    value = private[[member]]
    rm(list = member, envir = private)
    evaluation = list2env(list(
      events = events,
      event = member,
      snapshot = value
    ), parent = baseenv())
    delayedAssign(
      member,
      {
        events$seen = c(events$seen, event)
        snapshot
      },
      assign.env = private,
      eval.env = evaluation
    )
  }

  events$seen = character()
  observed = ParamSetCollection$new(list(owner = child))
  expect_s3_class(observed, "ParamSetCollection")
  expect_identical(events$seen, c(".params", ".tags", ".trafos"))
})

test_that("small rich construction survives forced collection", {
  left = ps(
    x = p_dbl(0, 1, tags = "numeric", trafo = sqrt),
    y = p_int(0L, 3L)
  )
  right = ps(flag = p_lgl())
  gctorture(TRUE)
  on.exit(gctorture(FALSE), add = TRUE)
  observed = ParamSetCollection$new(
    list(left = left, right = right),
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = TRUE
  )
  gctorture(FALSE)
  expect_identical(observed$ids(), c("x.left", "y.left", "flag.right"))
  expect_identical(observed$sets[[1L]], left)
  expect_identical(observed$sets[[2L]], right)
})
