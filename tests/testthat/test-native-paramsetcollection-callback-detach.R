native_collection_detach_plan = function(collection, ids = NULL) {
  .Call(
    paradox:::C_param_set_collection_detach_plan,
    collection$.__enclos_env__$private,
    collection,
    ids
  )
}

test_that("collection callback entries are registered and forced", {
  symbols = c(
    param_set_collection_detach_plan = 3L,
    param_set_collection_has_callback = 3L,
    param_set_collection_extra_trafo = 3L,
    param_set_collection_constraint = 3L,
    param_set_collection_detached_extra_trafo = 2L,
    param_set_collection_detached_constraint = 2L,
    param_set_collection_owner_subset_state = 3L
  )
  namespace = asNamespace("paradox")
  for (name in names(symbols)) {
    symbol = get(paste0("C_", name), envir = namespace)
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, unname(symbols[[name]]))
  }
  expect_error(
    .Call("param_set_collection_detach_plan", PACKAGE = "paradox"),
    "not available"
  )
})

test_that("callback-free collection subsets bypass callback planning", {
  plain = psc(component = ps(x = p_int(), y = p_lgl()))
  observed = plain$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )
  expect_identical(observed$ids(), "component.x")
  expect_null(observed$constraint)
  expect_null(observed$extra_trafo)
})

test_that("native plans flatten nested capsule routes in schema order", {
  first = ps(x = p_int(), plain = p_lgl())
  second = ps(y = p_int())
  third = ps(z = p_int())
  first$constraint = function(x) TRUE
  second$extra_trafo = function(x) {
    x$y = x$y + 10L
    x
  }
  third$extra_trafo = function(x) {
    x$z = x$z + 100L
    x
  }
  nested = psc(
    layer = psc(first = first, second = second),
    third = third
  )

  plan = native_collection_detach_plan(
    nested,
    c("third.z", "layer.first.x", "layer.second.y", "layer.second.y")
  )
  expect_identical(
    plan$translation$id,
    c("layer.first.x", "layer.second.y", "third.z")
  )
  expect_identical(
    names(plan$translation),
    c(
      "id", "original_id", "owner_ps_index", "owner_name", ".prefix",
      ".suffix"
    )
  )
  expect_identical(plan$translation$original_id, c("x", "y", "z"))
  expect_identical(plan$constraint_indices, 1L)
  expect_identical(plan$trafo_indices, c(2L, 3L))

  snapshot = nested$subset(
    c("third.z", "layer.first.x", "layer.second.y"),
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    snapshot$trafo(list(
      third.z = 1L,
      ignored = 9L,
      layer.second.y = 2L,
      layer.first.x = 3L
    )),
    list(
      ignored = 9L,
      layer.first.x = 3L,
      layer.second.y = 12L,
      third.z = 101L
    )
  )
})

test_that("prefix and postfix routes compose without retaining collections", {
  child = ps(x = p_int())
  child$extra_trafo = function(x) {
    x$x = x$x + 1L
    x$new = 5L
    x
  }
  inner = ParamSetCollection$new(
    list(component = child),
    postfix_names = TRUE
  )
  outer = psc(layer = inner)
  snapshot = outer$subset(
    "layer.x.component",
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    snapshot$trafo(list(layer.x.component = 1L)),
    list(layer.x.component = 2L, layer.new.component = 5L)
  )
  shadow = ParamSetShadow$new(outer, character())
  expect_identical(
    shadow$trafo(list(layer.x.component = 1L)),
    list(layer.x.component = 2L, layer.new.component = 5L)
  )

  bindings = ls(environment(snapshot$extra_trafo), all.names = TRUE)
  expect_identical(bindings, "plan")
  expect_null(get("plan", environment(snapshot$extra_trafo))$sets[[1L]]$param_set)
  expect_false(any(vapply(
    mget(bindings, environment(snapshot$extra_trafo)),
    inherits,
    logical(1L),
    what = "ParamSetCollection"
  )))
})

test_that("callback replacements after subset do not change snapshots", {
  closure_state = new.env(parent = emptyenv())
  closure_state$increment = 1L
  closure_state$limit = 5L
  child = ps(x = p_int())
  child$extra_trafo = function(x) {
    x$x = x$x + closure_state$increment
    x
  }
  child$constraint = function(x) x$x <= closure_state$limit
  snapshot = psc(component = child)$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )

  child$extra_trafo = function(x) {
    x$x = x$x + 100L
    x
  }
  child$constraint = function(x) FALSE
  expect_identical(
    snapshot$trafo(list(component.x = 1L))$component.x,
    2L
  )
  expect_true(snapshot$constraint(list(component.x = 4L)))

  closure_state$increment = 3L
  closure_state$limit = 2L
  expect_identical(
    snapshot$trafo(list(component.x = 1L))$component.x,
    4L
  )
  expect_false(snapshot$constraint(list(component.x = 4L)))
})

test_that("documented two-argument trafos receive detached leaf ParamSets", {
  observed = new.env(parent = emptyenv())
  child = ps(x = p_int(), y = p_lgl())
  child$extra_trafo = function(x, param_set) {
    observed$ids = param_set$ids()
    observed$class = class(param_set)
    x
  }
  snapshot = psc(component = child)$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )
  child$values = list(y = TRUE)
  snapshot$trafo(list(component.x = 1L))

  expect_identical(observed$ids, "x")
  expect_identical(observed$class, c("ParamSet", "R6"))
  carriers = get("plan", environment(snapshot$extra_trafo))$sets
  expect_false(".source" %in% names(carriers[[1L]]))
  expect_false(".core" %in% names(carriers[[1L]]))
  expect_s3_class(carriers[[1L]]$param_set, "ParamSet")
  expect_false(identical(carriers[[1L]]$param_set, child))
})

test_that("live collection callbacks use capsule state, not overridable methods", {
  observed = new.env(parent = emptyenv())
  HostileParamSet = R6::R6Class(
    "HostileParamSet",
    inherit = ParamSet,
    public = list(
      initialize = function() {
        super$initialize(list(x = p_int(), y = p_int()))
        paradox:::param_set_core_replace(
          private,
          extra_trafo = function(x, param_set) {
            observed$ids = param_set$ids()
            list(x = x$x + length(param_set$ids()))
          },
          constraint = function(x) is.null(x$x) || x$x <= 2L
        )
      },
      subset = function(...) stop("overridden subset dispatched"),
      trafo = function(...) stop("overridden trafo dispatched")
    ),
    active = list(
      extra_trafo = function(value) {
        if (!missing(value)) stop("overridden extra_trafo setter dispatched")
        stop("overridden extra_trafo getter dispatched")
      },
      constraint = function(value) {
        if (!missing(value)) stop("overridden constraint setter dispatched")
        stop("overridden constraint getter dispatched")
      },
      has_extra_trafo = function() FALSE,
      has_constraint = function() FALSE
    )
  )
  hostile = HostileParamSet$new()
  collection = psc(component = hostile)

  expect_identical(
    collection$extra_trafo(list(component.x = 1L, component.y = 2L)),
    list(component.x = 3L)
  )
  expect_identical(
    collection$trafo(list(component.x = 1L, component.y = 2L)),
    list(component.x = 3L)
  )
  expect_true(collection$constraint(list(component.x = 2L)))
  expect_false(collection$constraint(list(component.x = 3L)))
  expect_true(collection$check(list(component.x = 2L)))
  expect_identical(
    collection$check(list(component.x = 3L)),
    "Constraint not fulfilled."
  )

  detached = collection$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    detached$trafo(list(component.x = 1L)),
    list(component.x = 2L)
  )
  expect_identical(observed$ids, "x")
})

test_that("native aggregate replacement handles omission and collisions", {
  child = ps(x = p_int(), y = p_int())
  child$extra_trafo = function(x) list(x = x$x + 1L)
  collection = psc(component = child)
  detached = collection$flatten()
  shadow = ParamSetShadow$new(collection, character())
  input = list(
    unknown = 9L,
    component.x = 1L,
    component.y = 2L
  )
  expected = list(unknown = 9L, component.x = 2L)

  expect_identical(collection$extra_trafo(input), expected)
  expect_identical(collection$trafo(input), expected)
  expect_identical(detached$trafo(input), expected)
  expect_identical(shadow$trafo(input), expected)

  child$extra_trafo = function(x) list()
  expect_identical(
    collection$extra_trafo(input),
    list(unknown = 9L)
  )

  child$extra_trafo = function(x) unname(list(x$x))
  expect_error(
    collection$extra_trafo(input),
    "must have one name for every element"
  )
  child$extra_trafo = function(x) structure(
    list(x$x, x$y),
    names = c("same", "same")
  )
  expect_error(collection$extra_trafo(input), "unique names")

  child$extra_trafo = function(x) list(collision = x$x)
  expect_error(
    collection$extra_trafo(c(
      input,
      list(component.collision = 10L)
    )),
    "collides with a retained value"
  )
})

test_that("native collection constraints require an exact logical scalar", {
  child = ps(x = p_int())
  collection = psc(component = child)
  expect_null(collection$constraint)
  expect_null(collection$extra_trafo)

  for (answer in list(NA, logical(), c(TRUE, FALSE), 1L)) {
    child$constraint = local({
      value = answer
      function(x) value
    })
    expect_error(
      collection$constraint(list(component.x = 1L)),
      "one non-missing logical value"
    )
    detached = collection$subset(
      "component.x",
      allow_dangling_dependencies = TRUE
    )
    expect_error(
      detached$constraint(list(component.x = 1L)),
      "one non-missing logical value"
    )
  }
})

test_that("detached callback plans fail closed when their mapping is forged", {
  child = ps(x = p_int())
  child$extra_trafo = function(x) x
  detached = psc(component = child)$subset(
    "component.x",
    allow_dangling_dependencies = TRUE
  )
  plan = get("plan", environment(detached$extra_trafo))

  expect_error(
    .Call(
      paradox:::C_param_set_collection_detached_extra_trafo,
      asS4(plan),
      list(component.x = 1L)
    ),
    "callback plan"
  )

  invalid_names = plan
  attr(invalid_names, "names") = asS4(names(invalid_names))
  expect_error(
    .Call(
      paradox:::C_param_set_collection_detached_extra_trafo,
      invalid_names,
      list(component.x = 1L)
    ),
    "callback plan"
  )

  invalid_unit = plan
  invalid_unit$indices = 0L
  expect_error(
    .Call(
      paradox:::C_param_set_collection_detached_extra_trafo,
      invalid_unit,
      list(component.x = 1L)
    ),
    "callback order"
  )

  invalid_mapping = plan
  invalid_mapping$translation = as.data.frame(plan$translation)
  invalid_mapping$translation$id = "different.x"
  expect_error(
    .Call(
      paradox:::C_param_set_collection_detached_extra_trafo,
      invalid_mapping,
      list(component.x = 1L)
    ),
    "callback translation"
  )
})

test_that("shared DAG paths remain distinct callback units", {
  events = character()
  shared = ps(x = p_int())
  shared$constraint = function(x) {
    events <<- c(events, names(x))
    TRUE
  }
  snapshot = ParamSetCollection$new(list(left = shared, right = shared))$subset(
    c("left.x", "right.x"),
    allow_dangling_dependencies = TRUE
  )
  expect_true(snapshot$constraint(list(left.x = 1L, right.x = 2L)))
  expect_identical(events, c("x", "x"))
})

test_that("corrupt callback graphs error instead of returning a sentinel", {
  child = ps(x = p_int())
  child$constraint = function(x) TRUE
  collection = psc(component = child)
  private = collection$.__enclos_env__$private
  paradox:::param_set_core_replace(
    private,
    sets = structure(list(collection), names = "component")
  )
  expect_error(
    native_collection_detach_plan(collection, "component.x"),
    "cycle"
  )
})

test_that("detachment affixes stay rooted while the plan is built", {
  skip_on_cran()

  # `make_affix()` mints a fresh CHARSXP that the plan parks in operation-local
  # R_alloc storage, which the collector does not scan. R sweeps its string
  # cache on every collection, so each affix must be rooted before the next
  # allocation or the plan is built from freed strings.
  nodes = 20L
  for (round in seq_len(10L)) {
    tag = sprintf("t%05d", round)
    prefixes = sprintf("pzzz%s%04d", tag, seq_len(nodes))
    suffixes = sprintf("qzzz%s%04d", tag, seq_len(nodes))
    children = lapply(seq_len(nodes), function(index) {
      ParamSetCollection$new(
        set_names(list(ps(x = p_dbl(0, 1))), suffixes[[index]]),
        postfix_names = TRUE
      )
    })
    collection = ParamSetCollection$new(set_names(children, prefixes))

    invisible(gc())
    previous = gctorture(TRUE)
    plan = .Call(
      paradox:::C_param_set_collection_detach_plan,
      collection$.__enclos_env__$private,
      collection,
      NULL
    )
    gctorture(previous)

    # Compare only after the call: interning the expected text beforehand would
    # root it in the string cache and hide the defect.
    expect_identical(plan$translation$.prefix, paste0(prefixes, "."))
    expect_identical(plan$translation$.suffix, paste0(".", suffixes))
  }
})
