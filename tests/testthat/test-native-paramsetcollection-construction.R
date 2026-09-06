collection2_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"), inherits = FALSE)
}

collection2_construct = function(sets, tag_sets = FALSE,
    tag_params = FALSE, postfix_names = FALSE) {
  .Call(
    collection2_symbol("param_set_collection_construct"),
    sets,
    tag_sets,
    tag_params,
    postfix_names
  )
}

collection2_private = function(param_set) {
  param_set$.__enclos_env__$private
}

collection2_state = function(param_set) {
  paradox:::param_set_core_state(collection2_private(param_set))
}

# Install malformed state through the capsule replacement primitive. This is
# solely an adversarial validator probe, not a supported mutation mechanism.
collection2_forge_child = function(field, mutate) {
  child = ps(
    number = p_dbl(0, 1, tags = "numeric", trafo = exp),
    flag = p_lgl()
  )
  private = collection2_private(child)
  field_name = paste0(".", field)
  value = collection2_state(child)[[field_name]]
  value = unserialize(serialize(value, NULL, version = 3L))
  .Call(
    paradox:::C_param_set_core_replace,
    private,
    setNames(list(mutate(value)), field_name)
  )
  child
}

test_that("collection constructor routines have fixed registered interfaces", {
  constructor = collection2_symbol("param_set_collection_construct")
  add = collection2_symbol("param_set_collection_add")
  affix_probe = collection2_symbol("test_checked_affixed_size")

  expect_s3_class(constructor, "NativeSymbolInfo")
  expect_identical(constructor$numParameters, 4L)
  expect_s3_class(add, "NativeSymbolInfo")
  expect_identical(add$numParameters, 6L)
  expect_identical(affix_probe$numParameters, 2L)
  add_reentry = collection2_symbol("test_param_set_collection_add_reentry")
  expect_identical(add_reentry$numParameters, 8L)
  construct_reentry = collection2_symbol(
    "test_param_set_collection_construct_reentry"
  )
  expect_identical(construct_reentry$numParameters, 5L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
  expect_error(
    .Call("param_set_collection_construct", PACKAGE = "paradox"),
    "not available",
    fixed = TRUE
  )
})

test_that("affixed ID arithmetic is checked at native C boundaries", {
  probe = function(owner, id) {
    .Call(
      collection2_symbol("test_checked_affixed_size"),
      as.integer(owner),
      as.integer(id)
    )
  }

  # Selectors denote 0, 1, INT_MAX - 1, INT_MAX, SIZE_MAX - 1, SIZE_MAX.
  expect_identical(probe(0L, 0L), 1L)
  expect_identical(probe(1L, 0L), 2L)
  expect_identical(probe(2L, 0L), .Machine$integer.max)
  expect_identical(probe(0L, 2L), .Machine$integer.max)
  for (pair in list(c(2L, 1L), c(3L, 0L), c(4L, 1L), c(5L, 0L))) {
    expect_identical(probe(pair[[1L]], pair[[2L]]), NA_integer_)
  }
  expect_error(probe(-1L, 0L), "Invalid affix boundary selector", fixed = TRUE)
  expect_error(
    .Call(collection2_symbol("test_checked_affixed_size"), 0, 0L),
    "ordinary integer scalars",
    fixed = TRUE
  )
})

test_that("collection construction rejects S4 structural arguments", {
  sets = list(child = ps(x = p_int()))
  expect_error(
    collection2_construct(asS4(sets)),
    "ordinary named list"
  )

  s4_names = sets
  attr(s4_names, "names") = asS4(names(s4_names))
  expect_error(
    collection2_construct(s4_names),
    "ordinary character names"
  )

  constructor = collection2_symbol("param_set_collection_construct")
  for (position in 2:4) {
    arguments = list(sets, FALSE, FALSE, FALSE)
    arguments[[position]] = asS4(arguments[[position]])
    expect_error(
      do.call(.Call, c(list(constructor), arguments)),
      "unclassed logical flag"
    )
  }
})

test_that("native construction creates exact canonical collection state", {
  left = ps(
    zeta = p_int(tags = c("set_left", "param_zeta")),
    alpha = p_dbl(tags = "numeric", trafo = sqrt)
  )
  right = ps(flag = p_lgl())
  sets = list(left = left, right = right)
  native = collection2_construct(sets, tag_sets = TRUE, tag_params = TRUE)

  expect_named(
    native,
    c("params", "tags", "trafos", "translation", "edges", "sets")
  )
  expect_identical(native$sets, sets)
  for (table in native[c("params", "tags", "trafos", "translation")]) {
    expect_identical(class(table), "data.frame")
    expect_null(attr(table, ".internal.selfref", exact = TRUE))
    expect_null(attr(table, "index", exact = TRUE))
    expect_null(attr(table, "sorted", exact = TRUE))
  }
  expect_identical(
    native$params$id,
    c("left.zeta", "left.alpha", "right.flag")
  )
  expect_identical(
    native$translation$id,
    c("left.alpha", "left.zeta", "right.flag")
  )
  expect_identical(native$translation$original_id, c("alpha", "zeta", "flag"))
  expect_identical(native$translation$owner_ps_index, c(1L, 1L, 2L))
  expect_identical(native$translation$owner_name, c("left", "left", "right"))
  expect_identical(
    split(native$tags$tag, native$tags$id),
    list(
      left.alpha = c("numeric", "set_left", "param_alpha"),
      left.zeta = c("set_left", "param_zeta", "set_left", "param_zeta"),
      right.flag = c("set_right", "param_flag")
    )
  )
  expect_identical(native$trafos$id, "left.alpha")
  expect_identical(native$trafos$trafo[[1L]], sqrt)

  collection = ParamSetCollection$new(
    sets,
    tag_sets = TRUE,
    tag_params = TRUE
  )
  state = collection2_state(collection)
  expect_identical(
    names(state),
    c(
      ".params", ".values", ".tags", ".deps", ".trafos",
      ".extra_trafo", ".constraint", ".sets", ".translation", ".postfix",
      ".edges"
    )
  )
  expect_identical(
    .Call(paradox:::C_param_set_core_kind, collection2_private(collection)),
    2L
  )
  expect_identical(state$.params, native$params)
  expect_identical(state$.tags, native$tags)
  expect_identical(state$.trafos, native$trafos)
  expect_identical(state$.translation, native$translation)
  expect_identical(state$.sets[[1L]], left)
  expect_identical(state$.sets[[2L]], right)
  expect_identical(state$.postfix, FALSE)
  expect_identical(
    names(state$.edges),
    c("cores", "tag_sets", "tag_params", "tag_override")
  )
  expect_null(state$.edges$tag_override)
  expect_identical(state$.edges$tag_sets, c(TRUE, TRUE))
  expect_identical(state$.edges$tag_params, c(TRUE, TRUE))
  expect_identical(
    state$.edges$cores,
    list(collection2_private(left)$.core, collection2_private(right)$.core)
  )
})

test_that("collection construction keeps each child paired with one name generation", {
  # A finalizer dropped under gctorture() cannot pin this window: the per-
  # allocation collections are young-generation only, so a promoted trigger
  # never finalizes inside the constructor. The registered reentry seam runs
  # the same mutation deterministically at the exact boundary between the
  # stable name/child capture and child admission.
  state = new.env(parent = emptyenv())
  state$sets = list(old = ps(x = p_int()))
  state$fired = FALSE

  native = .Call(
    collection2_symbol("test_param_set_collection_construct_reentry"),
    state$sets,
    FALSE,
    FALSE,
    FALSE,
    function() {
      state$fired = TRUE
      data.table::setattr(state$sets, "names", "new")
    }
  )
  expect_true(state$fired)
  expect_identical(names(state$sets), "new")

  # The stable capture precedes the mutation window, so every derived name
  # belongs to the captured generation; nothing pairs an old child with the
  # new name or the other way around.
  expect_identical(names(native$sets), "old")
  expect_identical(
    native$params$id,
    paste0(names(native$sets), ".x")
  )
  expect_identical(
    native$translation$owner_name,
    names(native$sets)
  )

  collection = ParamSetCollection$new(native$sets)
  expect_identical(collection$ids(), native$params$id)
})

test_that("native collection add installs one complete replacement generation", {
  left = ps(
    enabled = p_lgl(),
    value = p_int(tags = "existing")
  )
  collection = ParamSetCollection$new(list(left = left))
  collection$add_dep("left.value", "left.enabled", CondEqual(TRUE))
  private = collection2_private(collection)
  before_state = collection2_state(collection)
  before_generation = data.table::address(before_state)

  child = ps(
    zeta = p_dbl(tags = "numeric", trafo = sqrt),
    alpha = p_int()
  )
  result = collection$add(
    child,
    n = "right",
    tag_sets = TRUE,
    tag_params = TRUE
  )
  after = collection2_state(collection)

  expect_identical(result, collection)
  expect_false(identical(data.table::address(after), before_generation))
  expect_identical(names(before_state$.sets), "left")
  expect_identical(after$.deps, before_state$.deps)
  expect_identical(after$.values, before_state$.values)
  expect_identical(after$.extra_trafo, before_state$.extra_trafo)
  expect_identical(after$.constraint, before_state$.constraint)
  expect_identical(after$.postfix, before_state$.postfix)
  expect_identical(after$.params$id, c(
    "left.enabled", "left.value", "right.zeta", "right.alpha"
  ))
  expect_identical(names(after$.sets), c("left", "right"))
  expect_identical(after$.sets[[1L]], left)
  expect_identical(after$.sets[[2L]], child)
  expect_identical(
    after$.translation$owner_ps_index[after$.translation$id %in%
      c("right.zeta", "right.alpha")],
    c(2L, 2L)
  )
  expect_identical(
    after$.translation$owner_name[after$.translation$id %in%
      c("right.zeta", "right.alpha")],
    c("right", "right")
  )
  expect_identical(after$.trafos$id, "right.zeta")
  expect_identical(after$.trafos$trafo[[1L]], sqrt)
  expect_setequal(
    after$.tags$tag[after$.tags$id == "right.zeta"],
    c("numeric", "set_right", "param_zeta")
  )
  expect_setequal(
    after$.tags$tag[after$.tags$id == "right.alpha"],
    c("set_right", "param_alpha")
  )
})

test_that("native collection add preserves ordering, shared nodes, and affixes", {
  first = ps(x = p_int())
  second = ps(y = p_lgl())
  prefix = ParamSetCollection$new(list())
  prefix$add(first)
  prefix$add(second)
  prefix$add(first, "shared")
  prefix$add(ps(), "empty")

  expect_identical(names(prefix$sets), c("", "", "shared", "empty"))
  expect_identical(prefix$sets[[1L]], prefix$sets[[3L]])
  expect_identical(prefix$ids(), c("x", "y", "shared.x"))

  postfix = ParamSetCollection$new(list(), postfix_names = TRUE)
  postfix$add(first, "owner", tag_sets = TRUE, tag_params = TRUE)
  expect_identical(postfix$ids(), "x.owner")
  state = collection2_state(postfix)
  expect_identical(state$.translation$original_id, "x")
  expect_identical(state$.translation$owner_name, "owner")
  expect_setequal(state$.tags$tag, c("set_owner", "param_x"))
})

test_that("native collection add rejects cycles and corruption atomically", {
  collection = ParamSetCollection$new(list())
  private = collection2_private(collection)
  shadow = ParamSetShadow$new(collection, character())
  before = collection2_state(collection)
  before_generation = data.table::address(before)
  expect_error(collection$add(shadow, "cycle"), "cycle")
  expect_identical(
    data.table::address(collection2_state(collection)),
    before_generation
  )
  expect_identical(collection$sets, setNames(list(), character()))

  expect_error(collection$add(collection, "self"), "cycle")
  expect_identical(
    data.table::address(collection2_state(collection)),
    before_generation
  )

  valid = ParamSetCollection$new(list(base = ps(x = p_int())))
  valid_private = collection2_private(valid)
  valid_before = collection2_state(valid)
  valid_before_generation = data.table::address(valid_before)
  expect_error(valid$add(ps(y = p_int()), "base"), "already present")
  expect_identical(
    data.table::address(collection2_state(valid)),
    valid_before_generation
  )
  expect_error(valid$add(ps(base.x = p_int())), "nameclashes.*base\\.x")
  expect_identical(
    data.table::address(collection2_state(valid)),
    valid_before_generation
  )
  expect_error(valid$add(ps(y = p_int()), n = NA_character_), "non-missing")
  expect_error(valid$add(ps(y = p_int()), tag_sets = NA), "May not be NA")
  expect_error(valid$add(1L), "ParamSet child state")
  expect_identical(
    data.table::address(collection2_state(valid)),
    valid_before_generation
  )

  corrupt_child = collection2_forge_child("tags", function(table) {
    table$id[[1L]] = "ghost"
    table
  })
  expect_error(valid$add(corrupt_child, "bad"), "corrupt ParamSet child state")
  expect_identical(
    data.table::address(collection2_state(valid)),
    valid_before_generation
  )

  corrupt_values = ps(x = p_int())
  paradox:::param_set_core_replace(
    collection2_private(corrupt_values),
    values = list(ghost = 1L)
  )
  expect_error(
    valid$add(corrupt_values, "bad_values"),
    "Corrupt ParamSet"
  )
  expect_identical(
    data.table::address(collection2_state(valid)),
    valid_before_generation
  )

  corrupt = ParamSetCollection$new(list(base = ps(x = p_int())))
  corrupt_private = collection2_private(corrupt)
  bad_translation = collection2_state(corrupt)$.translation
  bad_translation$owner_ps_index[[1L]] = 99L
  paradox:::param_set_core_replace(
    corrupt_private,
    translation = bad_translation
  )
  corrupt_before = collection2_state(corrupt)
  corrupt_before_generation = data.table::address(corrupt_before)
  expect_error(corrupt$add(ps(y = p_int()), "next"), "Corrupt")
  expect_identical(
    data.table::address(collection2_state(corrupt)),
    corrupt_before_generation
  )
})

test_that("collection add rejects shell reparenting during a child refresh", {
  collection = ParamSetCollection$new(list(existing = ps(x = p_int())))
  original_enclosure = collection$.__enclos_env__
  original_private = original_enclosure$private
  original_core = original_private$.core
  donor = ParamSetCollection$new(list(donor = ps(z = p_int())))

  set_collection_enclosure = function(value) {
    was_locked = bindingIsLocked(".__enclos_env__", collection)
    if (was_locked) {
      unlockBinding(".__enclos_env__", collection)
    }
    assign(".__enclos_env__", value, envir = collection)
    if (was_locked) {
      lockBinding(".__enclos_env__", collection)
    }
  }
  on.exit(set_collection_enclosure(original_enclosure), add = TRUE)

  origin = ps(hidden = p_int(), shown = p_int())
  origin$constraint = function(x) TRUE
  shadow = ParamSetShadow$new(origin, "hidden")
  # Make the Shadow stale so collection add must rebuild its constraint through
  # the fixed package factory after the receiving graph was already selected.
  origin$constraint = function(x) is.null(x$shown) || x$shown >= 0L

  namespace = asNamespace("paradox")
  factory_name = "param_set_shadow_constraint_factory"
  original_factory = get(factory_name, envir = namespace, inherits = FALSE)
  factory_was_locked = bindingIsLocked(factory_name, namespace)
  set_factory = function(value) {
    if (bindingIsLocked(factory_name, namespace)) {
      unlockBinding(factory_name, namespace)
    }
    assign(factory_name, value, envir = namespace)
    if (factory_was_locked) {
      lockBinding(factory_name, namespace)
    }
  }
  on.exit(set_factory(original_factory), add = TRUE)

  calls = 0L
  set_factory(function(callback, hidden_values) {
    calls <<- calls + 1L
    set_collection_enclosure(donor$.__enclos_env__)
    original_factory(callback, hidden_values)
  })

  expect_error(
    collection$add(shadow, "child"),
    "capsule graph changed during collection add",
    fixed = TRUE
  )
  expect_identical(calls, 1L)

  # The hostile callback changed an unsupported shell surface, but the native
  # transaction must not have committed through the private environment it
  # selected before that reparenting.
  set_collection_enclosure(original_enclosure)
  expect_identical(original_private$.core, original_core)
  expect_named(collection$sets, "existing")
})

test_that("empty, prefix, postfix, nested, and shared public graphs are stable", {
  empty_native = collection2_construct(setNames(list(), character()))
  expect_named(
    empty_native,
    c("params", "tags", "trafos", "translation", "edges", "sets")
  )
  expect_identical(empty_native$sets, setNames(list(), character()))
  expect_identical(
    unname(vapply(
      empty_native[c("params", "tags", "trafos", "translation")],
      nrow,
      integer(1L)
    )),
    rep(0L, 4L)
  )
  expect_identical(empty_native$edges$cores, list())
  empty = ParamSetCollection$new(list())
  expect_identical(empty$ids(), character())
  expect_identical(empty$sets, setNames(list(), character()))

  leaf = ps(x = p_int(), y = p_lgl())
  inner = ParamSetCollection$new(list(inner = leaf))
  prefix = ParamSetCollection$new(list(left = inner, right = leaf, leaf))
  postfix = ParamSetCollection$new(
    list(left = inner, right = leaf, leaf),
    postfix_names = TRUE
  )
  expect_identical(prefix$ids(), c(
    "left.inner.x", "left.inner.y", "right.x", "right.y", "x", "y"
  ))
  expect_identical(postfix$ids(), c(
    "inner.x.left", "inner.y.left", "x.right", "y.right", "x", "y"
  ))
  expect_identical(postfix$sets[[2L]], postfix$sets[[3L]])
  expect_identical(postfix$sets[[2L]], leaf)
  expect_identical(inner$sets[[1L]], leaf)

  leaf$values = list(x = 2L)
  expect_identical(
    postfix$values,
    list(inner.x.left = 2L, x.right = 2L, x = 2L)
  )

  origin = ps(hidden = p_int(), visible = p_dbl())
  shadow = ParamSetShadow$new(origin, "hidden")
  graph = ParamSetCollection$new(list(view = shadow, direct = origin))
  expect_identical(graph$sets[[1L]], shadow)
  expect_identical(graph$sets[[2L]], origin)
  expect_identical(
    graph$ids(),
    c("view.visible", "direct.hidden", "direct.visible")
  )
})

test_that("native boundaries reject invalid sets, flags, and duplicate IDs", {
  child = ps(x = p_int())
  expect_error(collection2_construct(1L), "ordinary named list")
  expect_error(collection2_construct(unname(list(child))), "ordinary named list")
  expect_error(
    collection2_construct(structure(list(owner = child), class = "list")),
    "ordinary named list"
  )
  expect_error(collection2_construct(list(owner = 1L)), "ParamSet child state")
  expect_error(collection2_construct(
    setNames(list(child, child), c("owner", "owner"))
  ), "unique names")
  expect_error(collection2_construct(
    setNames(list(child, ps(x = p_lgl())), c("", ""))
  ), "translated parameter IDs must be unique")

  valid = list(owner = child)
  expect_error(collection2_construct(valid, tag_sets = NA), "May not be NA")
  expect_error(collection2_construct(valid, tag_sets = logical()), "logical flag")
  expect_error(collection2_construct(valid, tag_sets = 0L), "logical flag")
  expect_error(collection2_construct(
    valid,
    tag_sets = structure(FALSE, note = TRUE)
  ), "logical flag")
  expect_error(
    collection2_construct(valid, tag_params = c(FALSE, TRUE)),
    "logical flag"
  )
  expect_error(collection2_construct(valid, postfix_names = NA), "May not be NA")

  expect_identical(ParamSetCollection$new(list(child))$ids(), "x")
  expect_error(
    ParamSetCollection$new(setNames(
      list(child, ps(x = p_lgl())),
      c("", "")
    )),
    "Cannot construct ParamSetCollection",
    fixed = TRUE
  )
})

test_that("constructor validates consumed capsule fields, not private labels", {
  malformed = list(
    table_class = collection2_forge_child("params", function(table) {
      class(table) = c("data.table", "data.frame")
      table
    }),
    column_schema = collection2_forge_child("params", function(table) {
      names(table)[[1L]] = "not_id"
      table
    }),
    domain_kind = collection2_forge_child("params", function(table) {
      table$cls[[1L]] = "ParamUnknown"
      table
    }),
    tag_owner = collection2_forge_child("tags", function(table) {
      table$id[[1L]] = "ghost"
      table
    }),
    duplicate_trafo = collection2_forge_child("trafos", function(table) {
      table[c(1L, 1L), , drop = FALSE]
    })
  )

  # Native readers use positional columns and construct new outward headers.
  # Neither those private labels nor the copied class string need admission.
  for (name in c("table_class", "column_schema", "domain_kind")) {
    sets = list(owner = malformed[[name]])
    native = collection2_construct(sets)
    collection = ParamSetCollection$new(sets)
    expect_identical(native$params$id, c("owner.number", "owner.flag"))
    expect_identical(collection$ids(), native$params$id)
    if (name == "domain_kind") {
      expect_identical(native$params$cls[[1L]], "ParamUnknown")
      expect_identical(collection$class[[1L]], "ParamUnknown")
    }
  }
  malformed[c("table_class", "column_schema", "domain_kind")] = NULL

  for (name in names(malformed)) {
    sets = list(owner = malformed[[name]])
    expect_error(
      collection2_construct(sets),
      "Cannot construct ParamSetCollection",
      fixed = TRUE,
      info = name
    )
    expect_error(
      ParamSetCollection$new(sets),
      "Cannot construct ParamSetCollection",
      fixed = TRUE,
      info = name
    )
  }
})

test_that("collection construction remains rooted under forced collection", {
  skip_on_cran()

  marker = new.env(parent = emptyenv())
  leaf = ps(
    number = p_dbl(0, 1, tags = "numeric", trafo = sqrt),
    payload = p_uty(custom_check = function(x) TRUE, init = marker)
  )
  nested = ParamSetCollection$new(list(inner = leaf))
  sets = list(left = leaf, nested = nested, shared = leaf)

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  native = collection2_construct(
    sets,
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = TRUE
  )
  collection = ParamSetCollection$new(
    sets,
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = TRUE
  )
  gctorture(previous)

  expected = c(
    "number.left", "payload.left",
    "inner.number.nested", "inner.payload.nested",
    "number.shared", "payload.shared"
  )
  expect_identical(native$params$id, expected)
  expect_identical(collection$ids(), expected)
  expect_identical(collection$sets[[1L]], collection$sets[[3L]])
  expect_identical(collection$params$.init[[2L]], marker)
})

# The exact package-private Shadow refresh signature. `$add()` retains it for
# every node of both topology walks, so an in-place rewrite of the carrier or
# of one of its entries is a graph change the terminal barrier must see.
collection2_shadow_signature = function(shadow) {
  attr(
    collection2_private(shadow)$.core,
    ".paradox.shadow.snapshot.v1",
    exact = TRUE
  )
}

# A fresh carrier holding the identical entries: this isolates the carrier
# identity comparison from the per-entry comparison.
collection2_reforge_signature = function(shadow) {
  core = collection2_private(shadow)$.core
  signature = attr(core, ".paradox.shadow.snapshot.v1", exact = TRUE)
  forged = vector("list", length(signature))
  for (index in seq_along(signature)) forged[[index]] = signature[[index]]
  data.table::setattr(core, ".paradox.shadow.snapshot.v1", forged)
  invisible(NULL)
}

collection2_add_reentry = function(collection, child, n = "",
    tag_sets = FALSE, tag_params = FALSE, graph_hook = NULL,
    topology_hook = NULL) {
  .Call(
    collection2_symbol("test_param_set_collection_add_reentry"),
    collection2_private(collection),
    collection,
    child,
    n,
    tag_sets,
    tag_params,
    graph_hook,
    topology_hook
  )
}

test_that("collection add retains every Shadow refresh signature", {
  # A Shadow's `.core` external pointer stays pointer-identical while its
  # package-private signature attribute moves, so the shell/private/core
  # topology receipt alone cannot see the change.
  origin = ps(x = p_int(), hidden = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  collection = ParamSetCollection$new(list(existing = ps(y = p_int())))
  collection$ids()
  shadow$ids()
  before = data.table::address(collection2_state(collection))
  before_core = collection2_private(shadow)$.core

  fired = 0L
  expect_error(
    collection2_add_reentry(collection, shadow, "child",
      topology_hook = function() {
        fired <<- fired + 1L
        collection2_reforge_signature(shadow)
      }),
    "ParamSet capsule graph changed during collection add",
    fixed = TRUE
  )
  expect_identical(fired, 1L)
  expect_identical(data.table::address(collection2_state(collection)), before)
  expect_named(collection$sets, "existing")
  expect_identical(collection2_private(shadow)$.core, before_core)

  # A single entry replaced in place is the same defect at finer grain.
  other = ParamSetShadow$new(ps(z = p_int(), hidden = p_lgl()), "hidden")
  other$ids()
  signature = collection2_shadow_signature(shadow)
  original_entry = signature[[2L]]
  on.exit(.Call(
    collection2_symbol("test_gc_column_mutator"),
    signature, 1L, original_entry
  ), add = TRUE)
  expect_error(
    collection2_add_reentry(collection, shadow, "child",
      topology_hook = function() {
        pointer = .Call(
          collection2_symbol("test_gc_column_mutator"),
          signature,
          1L,
          collection2_private(other)$.core
        )
        rm(pointer)
        gc(full = TRUE)
      }),
    "ParamSet capsule graph changed during collection add",
    fixed = TRUE
  )
  expect_identical(data.table::address(collection2_state(collection)), before)
  expect_named(collection$sets, "existing")
})

test_that("collection add receipts cover every reachable Shadow", {
  # A Shadow that is already an edge of the receiving collection is retained
  # by the flattened graph as well as by the topology walk, so both scans must
  # reject the mutation.
  origin = ps(x = p_int(), hidden = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  receiver = ParamSetCollection$new(list(layer = shadow))
  receiver$ids()
  before = data.table::address(collection2_state(receiver))
  for (phase in c("graph", "topology")) {
    hook = function() collection2_reforge_signature(shadow)
    expect_error(
      collection2_add_reentry(receiver, ps(z = p_int()), "extra",
        graph_hook = if (phase == "graph") hook,
        topology_hook = if (phase == "topology") hook),
      "ParamSet capsule graph changed during collection add",
      fixed = TRUE,
      info = phase
    )
    expect_identical(
      data.table::address(collection2_state(receiver)),
      before,
      info = phase
    )
  }

  # A Shadow below the added child Collection is only in the child graph and
  # the child topology walk.
  nested = ParamSetShadow$new(ps(a = p_int(), hidden = p_lgl()), "hidden")
  child = ParamSetCollection$new(list(layer = nested))
  child$ids()
  child_ids = child$ids()
  child_core = collection2_private(child)$.core
  plain = ParamSetCollection$new(list(existing = ps(y = p_int())))
  plain$ids()
  plain_before = data.table::address(collection2_state(plain))
  for (phase in c("graph", "topology")) {
    hook = function() collection2_reforge_signature(nested)
    expect_error(
      collection2_add_reentry(plain, child, "kid",
        graph_hook = if (phase == "graph") hook,
        topology_hook = if (phase == "topology") hook),
      "ParamSet capsule graph changed during collection add",
      fixed = TRUE,
      info = phase
    )
    expect_identical(
      data.table::address(collection2_state(plain)),
      plain_before,
      info = phase
    )
    expect_identical(collection2_private(child)$.core, child_core, info = phase)
    expect_identical(child$ids(), child_ids, info = phase)
  }

  # A Shadow reachable only by descending another Shadow's origin edge is
  # invisible to both flattened graphs; the topology walk is its only receipt.
  base = ps(b = p_int(), hidden2 = p_lgl())
  inner_shadow = ParamSetShadow$new(base, "hidden2")
  inner = ParamSetCollection$new(list(inner = inner_shadow))
  outer_shadow = ParamSetShadow$new(inner, character())
  deep = ParamSetCollection$new(list(layer = outer_shadow))
  deep$ids()
  deep_before = data.table::address(collection2_state(deep))
  expect_error(
    collection2_add_reentry(deep, ps(z = p_int()), "extra",
      topology_hook = function() collection2_reforge_signature(inner_shadow)),
    "ParamSet capsule graph changed during collection add",
    fixed = TRUE
  )
  expect_identical(data.table::address(collection2_state(deep)), deep_before)
  expect_named(deep$sets, "layer")

  # A shared Shadow occurrence is deduplicated by the topology walk and still
  # rejected.
  shared = ParamSetShadow$new(ps(s = p_int(), hidden = p_lgl()), "hidden")
  wrapper = ParamSetCollection$new(list(one = shared))
  dag = ParamSetCollection$new(list(nested = wrapper, direct = shared))
  dag$ids()
  dag_before = data.table::address(collection2_state(dag))
  expect_error(
    collection2_add_reentry(dag, ps(z = p_int()), "extra",
      topology_hook = function() collection2_reforge_signature(shared)),
    "ParamSet capsule graph changed during collection add",
    fixed = TRUE
  )
  expect_identical(data.table::address(collection2_state(dag)), dag_before)
})

test_that("the collection add reentry seam is inert without a hook", {
  shadow = ParamSetShadow$new(ps(x = p_int(), hidden = p_lgl()), "hidden")
  collection = ParamSetCollection$new(list(layer = shadow))
  expect_identical(
    collection2_add_reentry(collection, ps(z = p_dbl()), "extra",
      tag_sets = TRUE),
    collection
  )
  expect_named(collection$sets, c("layer", "extra"))
  expect_identical(collection$ids(), c("layer.x", "extra.z"))
  expect_identical(collection$tags[["extra.z"]], "set_extra")

  # A hook that observes without mutating must not produce a false positive
  # across the ordinary allocation storm of the append itself.
  quiet = ParamSetCollection$new(list(
    layer = ParamSetShadow$new(ps(x = p_int(), hidden = p_lgl()), "hidden")
  ))
  observed = 0L
  expect_silent(collection2_add_reentry(quiet, ps(z = p_dbl()), "extra",
    graph_hook = function() observed <<- observed + 1L,
    topology_hook = function() observed <<- observed + 1L))
  expect_identical(observed, 2L)
  expect_identical(quiet$ids(), c("layer.x", "extra.z"))

  # And the public entry point remains equivalent to the seam.
  public = ParamSetCollection$new(list(
    layer = ParamSetShadow$new(ps(x = p_int(), hidden = p_lgl()), "hidden")
  ))
  public$add(ps(z = p_dbl()), "extra")
  expect_identical(public$ids(), quiet$ids())
  expect_named(public$sets, names(quiet$sets))

  expect_error(
    collection2_add_reentry(public, ps(w = p_int()), "bad", graph_hook = 1L),
    "reentry test hook must be a function"
  )
})
