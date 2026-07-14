native_collection_constructor_available = function() {
  exists(
    "C_param_set_collection_construct",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_constructor_symbol = function() {
  get(
    "C_param_set_collection_construct",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

native_collection_constructor_call = function(sets, tag_sets = FALSE,
    tag_params = FALSE, postfix_names = FALSE) {
  .Call(
    native_collection_constructor_symbol(),
    sets,
    tag_sets,
    tag_params,
    postfix_names
  )
}

test_that("native collection constructor is forced and arity four", {
  skip_if_not(native_collection_constructor_available())
  symbol = native_collection_constructor_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 4L)
  expect_error(
    .Call("param_set_collection_construct", PACKAGE = "paradox"),
    "not available",
    fixed = TRUE
  )
})

test_that("native affix arithmetic checks C boundary values", {
  symbol = get(
    "C_test_checked_affixed_size",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)

  # Selectors map to 0, 1, INT_MAX - 1, INT_MAX, SIZE_MAX - 1,
  # and SIZE_MAX. A valid result includes the separating dot; NA means the
  # addition or R's signed string-length limit rejected the combination.
  probe = function(owner, id) {
    .Call(symbol, as.integer(owner), as.integer(id))
  }
  expect_identical(probe(0L, 0L), 1L)
  expect_identical(probe(1L, 0L), 2L)
  expect_identical(probe(2L, 0L), .Machine$integer.max)
  expect_identical(probe(0L, 2L), .Machine$integer.max)
  expect_identical(probe(2L, 1L), NA_integer_)
  expect_identical(probe(3L, 0L), NA_integer_)
  expect_identical(probe(5L, 1L), NA_integer_)
  expect_identical(probe(1L, 5L), NA_integer_)
  expect_identical(probe(5L, 0L), NA_integer_)
  expect_identical(probe(4L, 1L), NA_integer_)
  expect_error(probe(-1L, 0L), "Invalid affix boundary selector", fixed = TRUE)
})

test_that("native collection constructor assembles rich exact state", {
  skip_if_not(native_collection_constructor_available())
  left = ps(
    zeta = p_int(tags = c("set_left", "param_zeta")),
    alpha = p_dbl(tags = "numeric", trafo = sqrt)
  )
  right = ps(flag = p_lgl())
  sets = list(left = left, right = right)
  state = native_collection_constructor_call(
    sets,
    tag_sets = TRUE,
    tag_params = TRUE
  )

  expect_named(state, c("params", "tags", "trafos", "translation"))
  expect_identical(
    state$params$id,
    c("left.zeta", "left.alpha", "right.flag")
  )
  expect_identical(
    state$translation$id,
    c("left.alpha", "left.zeta", "right.flag")
  )
  expect_identical(state$translation$original_id, c("alpha", "zeta", "flag"))
  expect_identical(state$translation$owner_ps_index, c(1L, 1L, 2L))
  expect_identical(state$translation$owner_name, c("left", "left", "right"))
  expect_identical(data.table::key(state$translation), "id")
  expect_identical(
    split(state$tags$tag, state$tags$id),
    list(
      left.alpha = c("numeric", "set_left", "param_alpha"),
      left.zeta = c("set_left", "param_zeta", "set_left", "param_zeta"),
      right.flag = c("set_right", "param_flag")
    )
  )
  expect_identical(data.table::key(state$tags), "id")
  expect_identical(state$trafos$id, "left.alpha")
  expect_identical(state$trafos$trafo[[1L]], sqrt)
  expect_identical(data.table::key(state$trafos), "id")
  expect_true(data.table:::selfrefok(state$params, FALSE) == 1L)
  expect_true(data.table:::selfrefok(state$tags, FALSE) == 1L)
  expect_true(data.table:::selfrefok(state$trafos, FALSE) == 1L)

  collection = ParamSetCollection$new(
    sets,
    tag_sets = TRUE,
    tag_params = TRUE
  )
  private = collection$.__enclos_env__$private
  expect_identical(collection$sets[[1L]], left)
  expect_identical(collection$sets[[2L]], right)
  expect_identical(data.table::indices(private$.params), "id__cls__grouping")
  expect_identical(data.table::indices(private$.translation), "original_id")
})

test_that("native collection constructor owns every mutable shell", {
  skip_if_not(native_collection_constructor_available())
  child = ps(
    x = p_dbl(0, 1, tags = c("first", "second"), trafo = exp),
    payload = p_uty(custom_check = function(x) TRUE)
  )
  child_private = child$.__enclos_env__$private
  first = native_collection_constructor_call(list(owner = child))
  second = native_collection_constructor_call(list(owner = child))
  expect_identical(first, second)
  for (member in names(first)) {
    expect_false(identical(
      data.table::address(first[[member]]),
      data.table::address(second[[member]])
    ), info = member)
    for (column in names(first[[member]])) {
      expect_false(identical(
        data.table::address(first[[member]][[column]]),
        data.table::address(second[[member]][[column]])
      ), info = sprintf("%s/%s", member, column))
    }
  }
  for (column in names(first$params)) {
    expect_false(identical(
      data.table::address(first$params[[column]]),
      data.table::address(child_private$.params[[column]])
    ), info = column)
  }
  expect_identical(first$trafos$trafo[[1L]], exp)
  expect_identical(
    first$params$cargo[[2L]]$custom_check,
    child_private$.params$cargo[[2L]]$custom_check
  )

  data.table::set(first$params, 1L, "lower", -100)
  data.table::set(first$tags, 1L, "tag", "changed")
  data.table::set(first$trafos, 1L, "trafo", list(log))
  expect_identical(child_private$.params$lower[[1L]], 0)
  expect_identical(child_private$.tags$tag, c("first", "second"))
  expect_identical(child_private$.trafos$trafo[[1L]], exp)
  expect_identical(second$params$lower[[1L]], 0)
})

test_that("native collection constructor handles empty, nested, postfix, and DAG state", {
  skip_if_not(native_collection_constructor_available())
  empty = native_collection_constructor_call(setNames(list(), character()))
  expect_named(empty, c("params", "tags", "trafos", "translation"))
  expect_identical(nrow(empty$params), 0L)
  expect_identical(nrow(empty$tags), 0L)
  expect_identical(nrow(empty$trafos), 0L)
  expect_identical(nrow(empty$translation), 0L)

  leaf = ps(x = p_int(), y = p_lgl())
  nested = ParamSetCollection$new(list(inner = leaf))
  state = native_collection_constructor_call(
    list(left = nested, right = leaf, leaf),
    postfix_names = TRUE
  )
  expect_identical(state$params$id, c(
    "inner.x.left", "inner.y.left",
    "x.right", "y.right", "x", "y"
  ))
  expect_identical(
    state$translation$owner_ps_index,
    c(1L, 1L, 3L, 2L, 3L, 2L)
  )

  shared = ParamSetCollection$new(list(first = leaf, second = leaf))
  expect_identical(shared$sets[[1L]], shared$sets[[2L]])
  expect_identical(
    shared$ids(),
    c("first.x", "first.y", "second.x", "second.y")
  )
})

test_that("native collection constructor declines extensions and duplicate output", {
  skip_if_not(native_collection_constructor_available())
  Probe = R6::R6Class(
    "NativeCollectionConstructorProbe",
    inherit = ParamSet
  )
  probe = Probe$new(list(x = p_int()))
  expect_null(native_collection_constructor_call(list(owner = probe)))

  custom_set = ps(x = p_int())
  data.table::set(
    custom_set$.__enclos_env__$private$.params,
    1L,
    "cls",
    "ParamCollectionConstructorExtension"
  )
  expect_null(native_collection_constructor_call(list(owner = custom_set)))
  expect_s3_class(
    ParamSetCollection$new(list(owner = custom_set)),
    "ParamSetCollection"
  )

  first = ps(x = p_int())
  second = ps(x = p_lgl())
  duplicate_sets = setNames(list(first, second), c("", ""))
  expect_null(native_collection_constructor_call(duplicate_sets))
  expect_error(
    ParamSetCollection$new(duplicate_sets),
    "duplicated parameter names: x",
    fixed = TRUE
  )
})

test_that("collection subclasses retain private name-affixing overrides", {
  AffixCollection = R6::R6Class(
    "NativeCollectionAffixSubclass",
    inherit = ParamSetCollection,
    private = list(
      .add_name_prefix = function(owner, id) paste0(owner, "__", id)
    )
  )
  child = ps(x = p_int(tags = "existing", trafo = identity))
  collection = AffixCollection$new(
    list(a = child),
    tag_sets = TRUE,
    tag_params = TRUE
  )
  private = collection$.__enclos_env__$private

  expect_identical(collection$ids(), "a__x")
  expect_identical(private$.params$id, "a__x")
  expect_identical(private$.translation$id, "a__x")
  expect_identical(private$.trafos$id, "a__x")
  expect_true(all(private$.tags$id == "a__x"))
  expect_identical(
    private$.tags$tag,
    c("existing", "set_a", "param_x")
  )
})

test_that("native collection constructor rejects fancy storage without forcing it", {
  skip_if_not(native_collection_constructor_available())
  active_events = new.env(parent = emptyenv())
  active_events$calls = 0L
  Probe = R6::R6Class(
    "NativeCollectionConstructorStorageProbe",
    inherit = ParamSet,
    lock_objects = FALSE
  )
  active = Probe$new(list(x = p_int(tags = "tag")))
  class(active) = c("ParamSet", "R6")
  active_private = active$.__enclos_env__$private
  tags = active_private$.tags
  rm(".tags", envir = active_private)
  makeActiveBinding(".tags", function(value) {
    if (!missing(value)) stop("read-only")
    active_events$calls = active_events$calls + 1L
    tags
  }, active_private)
  expect_null(native_collection_constructor_call(list(owner = active)))
  expect_identical(active_events$calls, 0L)

  delayed_events = new.env(parent = emptyenv())
  delayed_events$calls = 0L
  delayed = Probe$new(list(x = p_int(trafo = identity)))
  class(delayed) = c("ParamSet", "R6")
  delayed_private = delayed$.__enclos_env__$private
  trafos = delayed_private$.trafos
  rm(".trafos", envir = delayed_private)
  evaluation = list2env(list(
    events = delayed_events,
    snapshot = trafos
  ), parent = baseenv())
  delayedAssign(
    ".trafos",
    {
      events$calls = events$calls + 1L
      snapshot
    },
    assign.env = delayed_private,
    eval.env = evaluation
  )
  expect_null(native_collection_constructor_call(list(owner = delayed)))
  expect_identical(delayed_events$calls, 0L)

  malformed = ps(x = p_int())
  malformed$.__enclos_env__$private$.params = list(id = "x")
  expect_null(native_collection_constructor_call(list(owner = malformed)))
  expect_null(native_collection_constructor_call(list(owner = ps(x = p_int())), tag_sets = NA))
  expect_null(native_collection_constructor_call(unname(list(ps(x = p_int())))))
})

test_that("constructor leaves unrelated child state live and unread", {
  skip_if_not(native_collection_constructor_available())
  Probe = R6::R6Class(
    "NativeCollectionConstructorLiveStateProbe",
    inherit = ParamSet,
    lock_objects = FALSE
  )
  child = Probe$new(list(enabled = p_lgl(), amount = p_int()))
  class(child) = c("ParamSet", "R6")
  child$add_dep("amount", "enabled", CondEqual(TRUE))
  child$values = list(enabled = TRUE, amount = 1L)
  private = child$.__enclos_env__$private
  events = new.env(parent = emptyenv())
  events$deps = 0L
  dependencies = private$.deps
  rm(".deps", envir = private)
  makeActiveBinding(".deps", function(value) {
    if (!missing(value)) stop("read-only")
    events$deps = events$deps + 1L
    dependencies
  }, private)

  state = native_collection_constructor_call(list(component = child))
  expect_named(state, c("params", "tags", "trafos", "translation"))
  expect_identical(events$deps, 0L)
  collection = ParamSetCollection$new(list(component = child))
  expect_identical(events$deps, 0L)
  expect_identical(collection$values, list(
    component.enabled = TRUE,
    component.amount = 1L
  ))
  expect_identical(collection$deps$id, "component.amount")
  expect_identical(events$deps, 1L)
})

test_that("native collection constructor declines encoded IDs for the R path", {
  skip_if_not(native_collection_constructor_available())
  child = ps(x = p_int(tags = "encoded", trafo = identity))
  private = child$.__enclos_env__$private
  encoded = enc2utf8("caf\u00e9")
  data.table::set(private$.params, 1L, "id", encoded)
  data.table::set(private$.tags, 1L, "id", encoded)
  data.table::set(private$.trafos, 1L, "id", encoded)
  expect_null(native_collection_constructor_call(list(owner = child)))
  expect_identical(
    enc2utf8(ParamSetCollection$new(list(owner = child))$ids()),
    "owner.caf\u00e9"
  )
})

test_that("native collection constructor rejects ALTREP before Length dispatch", {
  skip_if_not(native_collection_constructor_available())
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("collection constructor invoked ALTREP Length", call. = FALSE)
  }
  stateful_unarmed = function(first, later = first) {
    native_stateful_altrep(
      first,
      later,
      callback = callback,
      callback_after = c(NA_integer_, NA_integer_)
    )
  }
  child = ps(x = p_int())
  sets = list(owner = child)

  sets_fixture = stateful_unarmed(sets)
  native_stateful_altrep_rearm(
    sets_fixture,
    c(NA_integer_, 0L)
  )
  expect_null(native_collection_constructor_call(sets_fixture))
  fancy_names = sets
  name_fixture = stateful_unarmed("owner", "changed")
  attr(fancy_names, "names") = name_fixture
  native_stateful_altrep_rearm(
    name_fixture,
    c(NA_integer_, 0L)
  )
  expect_null(native_collection_constructor_call(fancy_names))
  flag_fixture = stateful_unarmed(FALSE)
  native_stateful_altrep_rearm(
    flag_fixture,
    c(NA_integer_, 0L)
  )
  expect_null(native_collection_constructor_call(
    sets,
    tag_sets = flag_fixture
  ))
  expect_identical(callbacks, 0L)
})

test_that("native collection constructor detaches names from finalizer mutation", {
  skip_if_not(native_collection_constructor_available())
  symbol = native_collection_constructor_symbol()
  sets = list(owner = ps(x = p_int(tags = "tag", trafo = identity)))
  events = new.env(parent = emptyenv())
  events$calls = 0L
  arm = function() {
    trigger = new.env(parent = emptyenv())
    reg.finalizer(trigger, function(ignored) {
      events$calls = events$calls + 1L
      data.table::setattr(sets, "names", "changed")
      invisible(gc())
    })
    invisible(NULL)
  }

  arm()
  observed = .Call(symbol, sets, TRUE, TRUE, FALSE)
  invisible(gc())

  expect_identical(events$calls, 1L)
  expect_identical(names(sets), "changed")
  expect_identical(observed$params$id, "owner.x")
  expect_identical(observed$translation$owner_name, "owner")
  expect_identical(observed$tags$tag, c("tag", "set_owner", "param_x"))
  expect_identical(observed$trafos$id, "owner.x")
})

test_that("native collection constructor survives forced collection", {
  skip_if_not(native_collection_constructor_available())
  left = ps(
    x = p_dbl(0, 1, tags = "numeric", trafo = sqrt),
    y = p_int(0L, 3L)
  )
  right = ParamSetCollection$new(list(inner = ps(flag = p_lgl())))
  sets = list(left = left, right = right, shared = left)
  gctorture(TRUE)
  on.exit(gctorture(FALSE), add = TRUE)
  state = native_collection_constructor_call(
    sets,
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = TRUE
  )
  gctorture(FALSE)
  expect_identical(state$params$id, c(
    "x.left", "y.left", "inner.flag.right", "x.shared", "y.shared"
  ))
  gctorture(TRUE)
  observed = ParamSetCollection$new(
    sets,
    tag_sets = TRUE,
    tag_params = TRUE,
    postfix_names = TRUE
  )
  gctorture(FALSE)
  expect_identical(observed$ids(), state$params$id)
  expect_identical(observed$sets[[1L]], observed$sets[[3L]])
})
