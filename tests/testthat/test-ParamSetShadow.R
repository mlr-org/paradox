test_that("ParamSetShadow captures its schema and keeps a live origin", {
  origin = ps(
    hidden = p_int(tags = "hidden"),
    x = p_dbl(0, 1, tags = "visible"),
    flag = p_lgl()
  )
  origin$values = list(hidden = 4L, x = 0.25, flag = TRUE)
  shadow = ParamSetShadow$new(origin, "hidden")

  expect_s3_class(shadow, "ParamSetShadow")
  expect_true(inherits(shadow, "ParamSet"))
  expect_identical(shadow$origin, origin)
  expect_identical(shadow$ids(), c("x", "flag"))
  expect_identical(shadow$values, list(x = 0.25, flag = TRUE))
  expect_identical(
    shadow$tags,
    list(x = "visible", flag = character())
  )

  origin$tags = list(
    hidden = "changed",
    x = "changed",
    flag = "changed"
  )
  # The visible schema is "origin minus hidden", computed live: the hidden set
  # is what the Shadow fixes, not the parameters or tags themselves.
  expect_identical(
    shadow$tags,
    list(x = "changed", flag = "changed")
  )

  expect_error({shadow$origin = ps()}, "origin is read-only")
  expect_error({shadow$deps = data.table::data.table()}, "deps is read-only")

  # Tags are the one derived field a view owns outright: the assignment is the
  # shadow's own answer and leaves the origin alone.
  shadow$tags = list(x = "mine", flag = character())
  expect_identical(shadow$tags, list(x = "mine", flag = character()))
  expect_identical(
    origin$tags,
    list(hidden = "changed", x = "changed", flag = "changed")
  )
})

test_that("ParamSetShadow keeps no parallel R visible or hidden schema", {
  view = ParamSetShadow$new(ps(hidden = p_int(), visible = p_int()), "hidden")
  private_names = ls(get_private(view), all.names = TRUE)

  expect_false(".shadowed" %in% private_names)
  expect_false(".visible" %in% private_names)
  expect_identical(view$ids(), "visible")
})

test_that("ParamSetShadow construction is one native admission boundary", {
  origin = ps(hidden = p_int(), visible = p_int())

  expect_error(ParamSetShadow$new(1L, character()), "Corrupt ParamSet node")
  expect_error(ParamSetShadow$new(origin, 1L), "must be a character vector")
  expect_error(
    ParamSetShadow$new(origin, c("hidden", "hidden")),
    "must be a unique character vector"
  )
  expect_error(
    ParamSetShadow$new(origin, "unknown"),
    "unknown or unsupported parameter ID"
  )

  unlockBinding("ids", origin)
  origin$ids = function(...) stop("R ids override executed", call. = FALSE)
  lockBinding("ids", origin)
  view = ParamSetShadow$new(origin, "hidden")
  expect_identical(view$params$id, "visible")
})

test_that("ParamSetShadow values write through and preserve hidden values", {
  origin = ps(hidden = p_int(), x = p_dbl(), flag = p_lgl())
  origin$values = list(hidden = 3L, x = 0.5, flag = TRUE)
  shadow = ParamSetShadow$new(origin, "hidden")

  shadow$values = list(x = 0.75)
  expect_identical(origin$values, list(hidden = 3L, x = 0.75))
  expect_identical(shadow$values, list(x = 0.75))

  shadow$set_values(flag = FALSE)
  expect_identical(
    origin$values,
    list(hidden = 3L, x = 0.75, flag = FALSE)
  )

  shadow$values = mlr3misc::named_list()
  expect_identical(origin$values, list(hidden = 3L))
  expect_identical(shadow$values, mlr3misc::named_list())
  expect_error({shadow$values = list(hidden = 8L)}, "'hidden' not available")

  constrained = ps(hidden = p_lgl(), x = p_int(), flag = p_lgl())
  constrained$add_dep("x", "flag", CondEqual$new(TRUE))
  unchecked = ParamSetShadow$new(constrained, "hidden")
  unchecked$assert_values = FALSE
  unchecked$values = list(x = 1L)
  expect_identical(constrained$values, list(x = 1L))
  expect_true(constrained$assert_values)
})

test_that("ParamSetShadow dependencies are live and cannot cross bounds", {
  origin = ps(hidden = p_lgl(), x = p_int(), flag = p_lgl())
  condition = CondEqual$new(TRUE)
  origin$add_dep("x", "flag", condition)
  shadow = ParamSetShadow$new(origin, "hidden")

  expect_identical(shadow$deps$id, "x")
  expect_identical(shadow$deps$on, "flag")

  shadow$add_dep("flag", "x", CondAnyOf$new(c(1L, 2L)))
  expect_identical(origin$deps$id, c("x", "flag"))
  expect_error(
    shadow$add_dep("x", "hidden", condition),
    "Dependency of visible 'x' on hidden 'hidden' crosses the ParamSetShadow boundary",
    fixed = TRUE
  )
  expect_error(
    shadow$add_dep("hidden", "x", condition),
    "'hidden' is hidden by this ParamSetShadow",
    fixed = TRUE
  )

  crossing = ps(hidden = p_lgl(), x = p_int(), flag = p_lgl())
  crossing$add_dep("x", "hidden", condition)
  expect_error(
    ParamSetShadow$new(crossing, "hidden"),
    "Dependency of visible 'x' on hidden 'hidden' crosses the ParamSetShadow boundary",
    fixed = TRUE
  )

  origin$add_dep("hidden", "flag", condition)
  expect_error(
    shadow$deps,
    "Dependency of hidden 'hidden' on visible 'flag' crosses the ParamSetShadow boundary",
    fixed = TRUE
  )
  expect_error(
    shadow$check(list(x = 1L, flag = TRUE)),
    "Dependency of hidden 'hidden' on visible 'flag' crosses the ParamSetShadow boundary",
    fixed = TRUE
  )
})

test_that("ParamSetShadow dependency reads have one native refresh boundary", {
  origin = ps(hidden = p_lgl(), x = p_int(), flag = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  origin$add_dep("x", "flag", CondEqual(TRUE))

  deps = testthat::with_mocked_bindings(
    shadow$deps,
    C_param_set_core_refresh = NULL,
    .package = "paradox"
  )
  expect_identical(deps$id, "x")
  expect_identical(deps$on, "flag")
})

test_that("ParamSetShadow observes live constraints and transformations", {
  origin = ps(
    hidden = p_int(),
    x = p_dbl(trafo = function(x) x + 1)
  )
  origin$values = list(hidden = 2L)
  origin$constraint = function(x) is.null(x$x) || x$x < x$hidden
  shadow = ParamSetShadow$new(origin, "hidden")

  expect_true(shadow$has_constraint)
  expect_true(shadow$test_constraint(list(x = 1)))
  expect_false(shadow$test_constraint(list(x = 3)))
  expect_error(
    {shadow$constraint = function(x) TRUE},
    "does not allow setting constraint"
  )

  origin$constraint = function(x) is.null(x$x) || x$x > x$hidden
  expect_false(shadow$test_constraint(list(x = 1)))
  expect_true(shadow$test_constraint(list(x = 3)))

  origin$extra_trafo = function(x) list(answer = x$x * 2)
  expect_identical(shadow$trafo(list(x = 2)), list(answer = 6))
  shadow$extra_trafo = function(x) list(answer = x$x * 3)
  expect_identical(origin$trafo(list(x = 2)), list(answer = 9))
})

test_that("the Shadow constraint adapter is a registered native boundary", {
  symbol = get(
    "C_param_set_shadow_constraint",
    envir = asNamespace("paradox")
  )
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)

  callback = function(x) TRUE
  plan = list(
    callback = callback,
    hidden_values = setNames(list(), character())
  )
  visible = setNames(list(), character())
  expect_error(
    .Call(symbol, asS4(plan), visible),
    "Corrupt ParamSetShadow constraint plan"
  )
  attr(plan, "names") = asS4(names(plan))
  expect_error(
    .Call(symbol, plan, visible),
    "Corrupt ParamSetShadow constraint plan"
  )
})

test_that("Shadow construction rejects attributed hidden-ID carriers", {
  origin = ps(hidden = p_int(), visible = p_lgl())
  expect_error(
    ParamSetShadow$new(
      origin,
      structure("hidden", probe = TRUE)
    ),
    "`shadowed` must be a character vector",
    fixed = TRUE
  )
})

test_that("Shadow constraints merge hidden values natively without S3 dispatch", {
  hidden = new.env(parent = emptyenv())
  origin = ps(hidden = p_uty(), x = p_int())
  origin$values = list(hidden = hidden)
  observed = NULL
  calls = 0L
  origin$constraint = function(x) {
    calls <<- calls + 1L
    observed <<- x
    identical(x$hidden, hidden) && identical(x$x, 1L)
  }
  shadow = ParamSetShadow$new(origin, "hidden")
  adapter = shadow$constraint

  c_calls = 0L
  c.paradox_shadow_probe = function(...) {
    c_calls <<- c_calls + 1L
    stop("S3 c method executed", call. = FALSE)
  }
  visible = structure(
    list(x = 1L),
    names = "x",
    class = "paradox_shadow_probe"
  )
  expect_true(adapter(visible))
  expect_identical(calls, 1L)
  expect_identical(names(observed), c("hidden", "x"))
  expect_null(attr(observed, "class", exact = TRUE))
  expect_identical(observed$hidden, hidden)
  expect_identical(c_calls, 0L)

  expect_error(
    adapter(list(hidden = 2L, x = 1L)),
    "names must be unique"
  )
  expect_error(
    adapter(structure(list(x = 1L), names = NA_character_)),
    "non-missing and non-empty"
  )
  expect_error(adapter(1L), "must be a named list")

  s4_class = structure(list(x = 1L), names = "x")
  attr(s4_class, "class") = asS4("paradox_shadow_probe")
  expect_error(adapter(s4_class), "must be a named list")
  expect_identical(c_calls, 0L)

  corrupt_plan = environment(adapter)$plan
  corrupt_plan$hidden_values = structure(
    corrupt_plan$hidden_values,
    class = "paradox_shadow_probe"
  )
  environment(adapter)$plan = corrupt_plan
  expect_error(
    adapter(list(x = 1L)),
    "Corrupt ParamSetShadow constraint plan"
  )
  expect_identical(c_calls, 0L)
})

test_that("Shadow constraint plans and callback answers fail closed", {
  origin = ps(hidden = p_int(), x = p_int())
  origin$values = list(hidden = 1L)
  answer_calls = 0L
  origin$constraint = function(x) {
    answer_calls <<- answer_calls + 1L
    1L
  }
  shadow = ParamSetShadow$new(origin, "hidden")
  adapter = shadow$constraint

  expect_identical(ls(environment(adapter), all.names = TRUE), "plan")
  expect_error(
    adapter(list(x = 2L)),
    "must return one non-missing logical"
  )
  expect_identical(answer_calls, 1L)

  environment(adapter)$plan = list(
    callback = function(x) TRUE,
    hidden_values = structure(list(1L), names = "hidden"),
    extra = NULL
  )
  expect_error(adapter(list(x = 2L)), "Corrupt ParamSetShadow constraint plan")

  origin$constraint = function(x) NA
  adapter = shadow$constraint
  expect_error(
    adapter(list(x = 2L)),
    "must return one non-missing logical"
  )
})

test_that("Shadow constraint adapters admit one logical observation", {
  namespace = asNamespace("paradox")
  skip_if_not(
    exists("C_test_stateful_altrep", namespace, inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )

  answer = native_stateful_altrep(
    structure(TRUE, class = "paradox_shadow_probe"),
    structure(FALSE, class = "paradox_shadow_probe"),
    elt_switch_after = 1L
  )
  origin = ps(hidden = p_int(), x = p_int())
  origin$values = list(hidden = 1L)
  origin$constraint = function(x) answer
  adapter = ParamSetShadow$new(origin, "hidden")$constraint

  # The native boundary observes the callback once and returns an ordinary
  # scalar. It must not leak an attributed/stateful callback representation.
  expect_identical(adapter(list(x = 2L)), TRUE)
})

test_that("a running Shadow constraint finishes from its exact plan snapshot", {
  origin = ps(hidden = p_int(), x = p_int())
  origin$values = list(hidden = 1L)
  adapter = NULL
  calls = 0L
  origin$constraint = function(x) {
    calls <<- calls + 1L
    environment(adapter)$plan = list(corrupt = TRUE)
    TRUE
  }
  shadow = ParamSetShadow$new(origin, "hidden")
  adapter = shadow$constraint

  expect_true(adapter(list(x = 2L)))
  expect_identical(calls, 1L)
  expect_error(
    adapter(list(x = 2L)),
    "Corrupt ParamSetShadow constraint plan"
  )
  expect_identical(calls, 1L)
})

test_that("ParamSetShadow composes with collections", {
  origin = ps(hidden = p_int(), x = p_int(), flag = p_lgl())
  origin$values = list(hidden = 9L, x = 1L, flag = TRUE)
  shadow = ParamSetShadow$new(origin, "hidden")
  collection = ParamSetCollection$new(list(view = shadow))

  expect_identical(
    collection$values,
    list(view.x = 1L, view.flag = TRUE)
  )
  collection$values = list(view.flag = FALSE)
  expect_identical(origin$values, list(hidden = 9L, flag = FALSE))
})

test_that("old-R Shadow generation receipts never enter the evaluator", {
  skip_if_no_old_r_binding_existence_path()

  left = ps(ax = p_dbl(tags = "left"))
  right = ps(bx = p_dbl(tags = "right"))
  origin = ParamSetCollection$new(list(a = left, b = right))
  shadow = ParamSetShadow$new(origin, "a.ax")
  enclosing = ParamSetCollection$new(list(s = shadow))

  # Make the Shadow unverified without changing its origin signature.  Before
  # the allocation-free receipt, its old-R terminal scan called base::exists()
  # for every graph node.  A finalizer at the second complete scan could then
  # mutate the already observed root, and the Shadow was stamped at the new
  # epoch with the old projection.  An enclosing collection trusted that stamp
  # and made the stale projection persistent.
  epoch_source = ps(epoch = p_dbl())
  epoch_source$values = list(epoch = 1)

  audit = new.env(parent = emptyenv())
  audit$busy = FALSE
  audit$phase = 0L
  audit$complete_passes = 0L
  audit$hit = FALSE
  path = list(
    origin,
    origin$.__enclos_env__,
    origin$.__enclos_env__$private,
    left,
    left$.__enclos_env__,
    left$.__enclos_env__$private,
    right,
    right$.__enclos_env__,
    right$.__enclos_env__$private
  )
  tracer = function() {
    if (audit$busy) {
      return(invisible())
    }
    environment = get("envir", envir = parent.frame(), inherits = FALSE)
    next_phase = audit$phase + 1L
    if (next_phase <= length(path) &&
        identical(environment, path[[next_phase]])) {
      audit$phase = next_phase
    } else {
      audit$phase = if (identical(environment, path[[1L]])) 1L else 0L
    }
    if (!audit$hit && audit$complete_passes == 1L &&
        audit$phase == 4L) {
      audit$busy = TRUE
      origin$tags = list(a.ax = "changed", b.bx = "changed")
      audit$hit = TRUE
      audit$busy = FALSE
    }
    if (audit$phase == length(path)) {
      audit$complete_passes = audit$complete_passes + 1L
      audit$phase = 0L
    }
    invisible()
  }

  tracer_name = ".__paradox_shadow_receipt_tracer__"
  had_tracer = exists(
    tracer_name,
    envir = .GlobalEnv,
    inherits = FALSE
  )
  previous_tracer = if (had_tracer) {
    get(tracer_name, envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  assign(tracer_name, tracer, envir = .GlobalEnv)
  traced = FALSE
  on.exit({
    if (traced) {
      invisible(untrace("exists", where = baseenv()))
    }
    if (had_tracer) {
      assign(tracer_name, previous_tracer, envir = .GlobalEnv)
    } else if (exists(tracer_name, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = tracer_name, envir = .GlobalEnv)
    }
  }, add = TRUE)
  invisible(trace(
    "exists",
    tracer = quote(
      get(
        ".__paradox_shadow_receipt_tracer__",
        envir = .GlobalEnv,
        inherits = FALSE
      )()
    ),
    where = baseenv(),
    print = FALSE
  ))
  traced = TRUE
  first = shadow$tags
  invisible(untrace("exists", where = baseenv()))
  traced = FALSE
  if (had_tracer) {
    assign(tracer_name, previous_tracer, envir = .GlobalEnv)
  } else {
    rm(list = tracer_name, envir = .GlobalEnv)
  }

  # A required-linkage receipt performs no second evaluator-visible pass, so
  # the adversarial mutation is never reached.  Keep the semantic assertions
  # as well: they capture the persistent enclosing-collection failure rather
  # than testing only an implementation detail.
  expect_false(audit$hit)
  expected_shadow = list(b.bx = origin$tags[["b.bx"]])
  expected_enclosing = list(s.b.bx = origin$tags[["b.bx"]])
  expect_identical(first, expected_shadow)
  expect_identical(shadow$tags, expected_shadow)
  expect_identical(enclosing$tags, expected_enclosing)
  expect_identical(enclosing$tags, expected_enclosing)
})

test_that("a COLLECTION-origin Shadow follows nested Shadow children", {
  base = ps(hidden = p_int(), x = p_int(), flag = p_lgl())
  base$values = list(hidden = 1L, x = 2L, flag = TRUE)
  inner = ParamSetShadow$new(base, "hidden")
  collection = ParamSetCollection$new(list(view = inner))
  outer = ParamSetShadow$new(collection, character())

  expect_identical(outer$values, list(view.x = 2L, view.flag = TRUE))

  base$values = list(hidden = 3L, x = 4L, flag = FALSE)
  expect_identical(outer$values, list(view.x = 4L, view.flag = FALSE))

  outer$values = list(view.x = 9L)
  expect_identical(base$values, list(hidden = 3L, x = 9L))
})

test_that("ParamSetShadow clone and serialization preserve view graphs", {
  origin = ps(hidden = p_int(), x = p_int())
  origin$values = list(hidden = 1L, x = 2L)
  shadow = ParamSetShadow$new(origin, "hidden")

  shallow = shadow$clone()
  expect_identical(shallow$origin, origin)

  deep = shadow$clone(deep = TRUE)
  expect_false(identical(deep$origin, origin))
  deep$values = list(x = 8L)
  expect_identical(deep$origin$values, list(hidden = 1L, x = 8L))
  expect_identical(origin$values, list(hidden = 1L, x = 2L))

  restored = unserialize(serialize(shadow, NULL))
  expect_identical(restored$values, list(x = 2L))
  restored$origin$values = list(hidden = 1L, x = 7L)
  expect_identical(restored$origin$values, list(hidden = 1L, x = 7L))
  expect_identical(restored$values, list(x = 7L))
  expect_identical(origin$values, list(hidden = 1L, x = 2L))
})

test_that("deep cloning preserves shared ParamSet graph identity", {
  shared = ps(hidden = p_int(), x = p_int())
  shared$values = list(hidden = 1L, x = 2L)
  view = ParamSetShadow$new(shared, "hidden")
  inner = ParamSetCollection$new(list(
    left = shared,
    right = shared,
    view = view
  ))
  graph = ParamSetCollection$new(list(first = inner, second = inner))

  cloned = graph$clone(deep = TRUE)
  expect_false(identical(cloned$sets[[1L]], inner))
  expect_identical(cloned$sets[[1L]], cloned$sets[[2L]])

  cloned_inner = cloned$sets[[1L]]
  expect_identical(cloned_inner$sets$left, cloned_inner$sets$right)
  expect_identical(cloned_inner$sets$left, cloned_inner$sets$view$origin)
  expect_false(identical(cloned_inner$sets$left, shared))

  cloned_inner$sets$view$values = list(x = 9L)
  expect_identical(
    cloned_inner$sets$left$values,
    list(hidden = 1L, x = 9L)
  )
  expect_identical(shared$values, list(hidden = 1L, x = 2L))

  restored = unserialize(serialize(graph, NULL, version = 3L))
  expect_identical(restored$sets[[1L]], restored$sets[[2L]])
  restored_inner = restored$sets[[1L]]
  expect_identical(restored_inner$sets$left, restored_inner$sets$right)
  expect_identical(restored_inner$sets$left, restored_inner$sets$view$origin)
})

test_that("deep-cloned Shadow constraint adapters use the cloned origin", {
  origin = ps(hidden = p_int(), x = p_int())
  origin$values = list(hidden = 10L, x = 1L)
  origin$constraint = function(x) x$x < x$hidden
  shadow = ParamSetShadow$new(origin, "hidden")

  cloned = shadow$clone(deep = TRUE)
  cloned$origin$values = list(hidden = 2L, x = 1L)

  expect_true(shadow$test_constraint(list(x = 5L)))
  expect_false(cloned$test_constraint(list(x = 5L)))
  expect_true(shadow$check(list(x = 5L)))
  expect_identical(cloned$check(list(x = 5L)), "Constraint not fulfilled.")

  cloned$origin$constraint = function(x) x$x > x$hidden
  expect_true(cloned$check(list(x = 5L)))
  expect_true(shadow$check(list(x = 5L)))
})

test_that("cloned Shadow dynamic adapters retain only the cloned origin", {
  child = ps(x = p_int())
  child$extra_trafo = function(x) list(answer = x$x + 1L)
  origin = ParamSetCollection$new(list(unit = child))
  shadow = ParamSetShadow$new(origin, character())

  cloned = shadow$clone(deep = TRUE)
  expect_identical(
    cloned$trafo(list(unit.x = 2L)),
    list(unit.answer = 3L)
  )

  origin$sets$unit$extra_trafo = function(x) list(answer = x$x + 100L)
  expect_identical(
    shadow$trafo(list(unit.x = 2L)),
    list(unit.answer = 102L)
  )
  expect_identical(
    cloned$trafo(list(unit.x = 2L)),
    list(unit.answer = 3L)
  )

  cloned$origin$sets$unit$extra_trafo = function(x) {
    list(answer = x$x + 5L)
  }
  expect_identical(
    cloned$trafo(list(unit.x = 2L)),
    list(unit.answer = 7L)
  )

  restored = unserialize(serialize(cloned, NULL, version = 3L))
  expect_identical(
    restored$trafo(list(unit.x = 2L)),
    list(unit.answer = 7L)
  )

  cloned$origin$sets$unit$extra_trafo = function(x) {
    list(answer = x$x + 9L)
  }
  expect_identical(
    restored$trafo(list(unit.x = 2L)),
    list(unit.answer = 7L)
  )
})

test_that("ParamSetShadow rejects a direct Shadow origin", {
  origin = ps(hidden = p_int(), x = p_int())
  first = ParamSetShadow$new(origin, "hidden")

  expect_error(
    ParamSetShadow$new(first, character()),
    "cannot directly wrap another ParamSetShadow"
  )
})

test_that("native Shadow snapshots refresh only when their origin generation changes", {
  core_address = function(x) {
    data.table::address(x$.__enclos_env__$private$.core)
  }

  base = ps(hidden = p_int(), x = p_int())
  base$values = list(hidden = 1L, x = 2L)
  base_shadow = ParamSetShadow$new(base, "hidden")
  initial = core_address(base_shadow)
  signature = attr(
    base_shadow$.__enclos_env__$private$.core,
    ".paradox.shadow.snapshot.v1",
    exact = TRUE
  )
  expect_type(signature, "list")
  expect_null(attributes(signature))
  expect_identical(signature[[1L]], base)
  expect_identical(signature[[2L]], base$.__enclos_env__$private$.core)
  expect_identical(
    names(attributes(base_shadow$.__enclos_env__$private$.core)),
    ".paradox.shadow.snapshot.v1"
  )

  expect_identical(base_shadow$values, list(x = 2L))
  expect_identical(core_address(base_shadow), initial)

  base$values = list(hidden = 3L, x = 4L)
  expect_identical(core_address(base_shadow), initial)
  expect_identical(base_shadow$values, list(x = 4L))
  refreshed = core_address(base_shadow)
  expect_false(identical(refreshed, initial))
  expect_identical(base_shadow$values, list(x = 4L))
  expect_identical(core_address(base_shadow), refreshed)

  child = ps(hidden = p_int(), x = p_int())
  child$values = list(hidden = 5L, x = 6L)
  collection = ParamSetCollection$new(list(unit = child))
  collection_shadow = ParamSetShadow$new(collection, "unit.hidden")
  collection_initial = core_address(collection_shadow)

  expect_identical(collection_shadow$values, list(unit.x = 6L))
  expect_identical(core_address(collection_shadow), collection_initial)

  child$values = list(hidden = 7L, x = 8L)
  expect_identical(collection_shadow$values, list(unit.x = 8L))
  collection_refreshed = core_address(collection_shadow)
  expect_false(identical(collection_refreshed, collection_initial))
  expect_identical(collection_shadow$values, list(unit.x = 8L))
  expect_identical(core_address(collection_shadow), collection_refreshed)
})

test_that("cold Shadow metadata errors without semantic replay", {
  origin = ps(hidden = p_int(), x = p_int())
  origin$values = list(hidden = 1L, x = 2L)
  shadow = ParamSetShadow$new(origin, "hidden")
  private = shadow$.__enclos_env__$private

  attr(private$.core, ".paradox.shadow.snapshot.v1") = list()
  epoch_source = ps(epoch = p_int())
  epoch_source$values = list(epoch = 1L)
  before_values = origin$values
  before_extra_trafo = origin$extra_trafo
  before_deps = origin$deps
  expect_error(shadow$origin, "Corrupt ParamSetShadow native snapshot metadata")
  expect_error(
    { shadow$origin$values = list(hidden = 3L, x = 4L) },
    "Corrupt ParamSetShadow native snapshot metadata"
  )
  expect_error(
    { shadow$extra_trafo = function(x) x },
    "Corrupt ParamSetShadow native snapshot metadata"
  )
  expect_error(
    shadow$add_dep("x", "x", CondEqual(1L)),
    "Corrupt ParamSet"
  )
  expect_identical(origin$values, before_values)
  expect_identical(origin$extra_trafo, before_extra_trafo)
  expect_identical(origin$deps, before_deps)
  expect_error(
    shadow$values,
    "Corrupt ParamSetShadow native snapshot metadata"
  )
  expect_error(shadow$tags, "Corrupt ParamSet")
  expect_error(
    shadow$clone(deep = TRUE),
    "Corrupt ParamSetShadow native snapshot metadata"
  )
  expect_error(
    upgrade_paradox_object(shadow),
    "corrupt current state capsule"
  )
})

test_that("Shadow capsule structure rejects S4 carriers and flags", {
  forge = function(field, value) {
    origin = ps(hidden = p_int(), x = p_int())
    shadow = ParamSetShadow$new(origin, "hidden")
    private = shadow$.__enclos_env__$private
    state = paradox:::param_set_core_state(private)
    state[[field]] = value(state[[field]])
    forged = .Call(paradox:::C_param_set_core_new, 3L, state)
    attr(forged, ".paradox.shadow.snapshot.v1") = attr(
      private$.core,
      ".paradox.shadow.snapshot.v1",
      exact = TRUE
    )
    private$.core = forged
    shadow
  }

  s4_sets = forge(".sets", asS4)
  expect_error(s4_sets$values, "Corrupt ParamSetShadow")

  s4_postfix = forge(".postfix", asS4)
  # A value read does not consume the private postfix flag.
  expect_identical(s4_postfix$values, setNames(list(), character()))
})

test_that("warm Shadow stamps prove public freshness, not private cache semantics", {
  origin = ps(x = p_int(init = 1L))
  shadow = ParamSetShadow$new(origin, character())
  inner = ParamSetCollection$new(list(left = shadow, right = shadow))
  outer = ParamSetCollection$new(list(nested = inner))
  expect_identical(outer$values, list(nested.left.x = 1L, nested.right.x = 1L))
  private = shadow$.__enclos_env__$private
  selected = private$.core

  # Private signature edits do not require admission on an independent read.
  data.table::setattr(selected, ".paradox.shadow.snapshot.v1", list())
  expect_identical(shadow$values, list(x = 1L))
  expect_identical(outer$values, list(nested.left.x = 1L, nested.right.x = 1L))
  expect_identical(private$.core, selected)

  # A public generation change requires refresh, which consumes the signature.
  origin$values = list(x = 2L)
  expect_error(shadow$values, "Corrupt ParamSetShadow")
  expect_error(outer$values, "Corrupt ParamSetShadow")
})

test_that("warm Shadow value reads still check selected private column shapes", {
  origin = ps(x = p_int(init = 1L))
  shadow = ParamSetShadow$new(origin, character())
  expect_identical(shadow$values, list(x = 1L))
  params = paradox:::param_set_core_state(shadow$.__enclos_env__$private)$.params
  data.table::set(params, j = "cls", value = list(new.env(parent = emptyenv())))
  expect_error(shadow$values, "Corrupt ParamSet")
})

test_that("Shadow refresh rejects related IDs outside its fixed schema", {
  origin = ps(hidden = p_int(), x = p_int(tags = "visible"))
  origin$values = list(hidden = 1L, x = 2L)
  shadow = ParamSetShadow$new(origin, "hidden")
  private = shadow$.__enclos_env__$private

  state = paradox:::param_set_core_state(private)
  state$.values = setNames(state$.values, "forged")
  forged = .Call(paradox:::C_param_set_core_new, 3L, state)
  attr(forged, ".paradox.shadow.snapshot.v1") = attr(
    private$.core,
    ".paradox.shadow.snapshot.v1",
    exact = TRUE
  )
  private$.core = forged

  expect_error(shadow$values, "Corrupt ParamSet")
})

test_that("deep cloning rejects cycles without rejecting shared siblings", {
  shared = ps(x = p_int())
  expect_no_error(
    ParamSetCollection$new(list(a = shared, b = shared))$clone(deep = TRUE)
  )

  collection = ParamSetCollection$new(list())
  shadow = ParamSetShadow$new(collection, character())
  paradox:::param_set_core_replace(
    collection$.__enclos_env__$private,
    sets = list(cycle = shadow)
  )
  expect_error(
    collection$clone(deep = TRUE),
    "capsule graph contains a cycle"
  )
  expect_error(shadow$clone(deep = TRUE), "capsule graph contains a cycle")
})

test_that("the shadow constraint adapter admits only ordinary names", {
  skip_on_cran()
  namespace = asNamespace("paradox")
  skip_if_not(
    exists("C_test_stateful_altrep", namespace, inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )

  # The merge loop indexes this names vector element by element, so a names
  # object that can answer differently per observation would pair a value with
  # another parameter's name and silently change the constraint's verdict.
  origin = ps(x = p_dbl(0, 1), flag = p_lgl(), g = p_dbl(0, 1))
  origin$constraint = function(x) isTRUE(x$flag)
  origin$values = list(x = 0.5, flag = TRUE)
  view = ParamSetShadow$new(origin, "x")

  hostile = native_stateful_altrep(
    c("flag", "g"), c("g", "flag"),
    elt_switch_after = 2L
  )
  values = list(TRUE, 0.5)
  names(values) = hostile
  expect_error(view$constraint(values), "must be a named list")

  expect_true(view$constraint(list(flag = TRUE, g = 0.5)))
  expect_false(view$constraint(list(flag = FALSE, g = 0.5)))
})
