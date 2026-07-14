native_get_domain_symbol = function() {
  get("C_param_set_get_domain", envir = asNamespace("paradox"))
}

native_get_domain_call = function(param_set, id) {
  .Call(
    native_get_domain_symbol(),
    param_set$.__enclos_env__$private,
    param_set,
    id
  )
}

native_get_domain_with_private = function(param_set, name, value, id = "x") {
  private = param_set$.__enclos_env__$private
  original = private[[name]]
  on.exit(private[[name]] <- original)
  private[[name]] = value
  native_get_domain_call(param_set, id)
}

native_get_domain_reference = function(param_set, id) {
  checkmate::assert_string(id)
  private = param_set$.__enclos_env__$private
  paramrow = private$.params[id, on = "id", nomatch = NULL]

  if (!nrow(paramrow)) stop(sprintf("No param with id '%s'", id))

  vals = param_set$values
  depstbl = param_set$deps[id, .(on, cond), on = "id", nomatch = 0]
  paramrow[, `:=`(
    .tags = list(private$.tags[id, tag, nomatch = 0]),
    .trafo = private$.trafos[id, trafo],
    .requirements = list(if (nrow(depstbl)) mlr3misc::transpose_list(depstbl)),
    .init_given = id %in% names(vals),
    .init = unname(vals[id])
  )]

  data.table::setattr(
    paramrow,
    "class",
    c(paramrow$cls, "Domain", class(paramrow))
  )
}

native_get_domain_expect_equivalent = function(observed, expected, selfref = 1L) {
  expect_identical(names(observed), paradox:::domain_names)
  expect_identical(names(observed), names(expected))
  expect_identical(lapply(observed, identity), lapply(expected, identity))
  expect_identical(class(observed), class(expected))
  expect_identical(attr(observed, "row.names"), 1L)
  expect_identical(
    names(attributes(observed)),
    c("class", "row.names", ".internal.selfref", "names")
  )
  expect_null(attr(observed, "repr", exact = TRUE))
  expect_identical(data.table:::selfrefok(observed, verbose = FALSE), selfref)
}

native_get_domain_replace = function(table, column, value) {
  result = unclass(table)
  result[[column]] = value
  attributes(result) = attributes(table)
  result
}

test_that("native get_domain is registered with a forced symbol", {
  symbol = native_get_domain_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 3L)
  expect_error(
    .Call("param_set_get_domain", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native get_domain reproduces rich built-in ParamSet rows", {
  marker = new.env(parent = emptyenv())
  transform = function(x) exp(x)
  param_set = ps(
    number = p_dbl(-2, 2, tags = c("numeric", "shared"), trafo = transform),
    integer = p_int(-3L, 3L, tolerance = 0L, tags = "shared", init = 2L),
    factor = p_fct(c("small", "large"), default = "small"),
    flag = p_lgl(tags = character()),
    payload = p_uty(custom_check = function(x) TRUE, init = marker)
  )
  param_set$add_dep("factor", "integer", CondAnyOf(c(-1L, 2L)))
  param_set$add_dep("factor", "flag", CondEqual(TRUE))

  private = param_set$.__enclos_env__$private
  expected = setNames(
    lapply(param_set$ids(), function(id) native_get_domain_reference(param_set, id)),
    param_set$ids()
  )
  params_before = lapply(private$.params, identity)
  tags_before = lapply(private$.tags, identity)
  trafos_before = lapply(private$.trafos, identity)
  deps_before = lapply(private$.deps, identity)
  values_before = lapply(private$.values, identity)

  for (id in param_set$ids()) {
    direct = native_get_domain_call(param_set, id)
    observed = param_set$get_domain(id)

    native_get_domain_expect_equivalent(direct, expected[[id]])
    native_get_domain_expect_equivalent(observed, expected[[id]])
  }

  expect_identical(lapply(private$.params, identity), params_before)
  expect_identical(lapply(private$.tags, identity), tags_before)
  expect_identical(lapply(private$.trafos, identity), trafos_before)
  expect_identical(lapply(private$.deps, identity), deps_before)
  expect_identical(lapply(private$.values, identity), values_before)
  expect_identical(param_set$get_domain("payload")$.init[[1L]], marker)
  expect_identical(param_set$get_domain("number")$.trafo[[1L]], transform)
  expect_identical(
    vapply(
      param_set$get_domain("factor")$.requirements[[1L]],
      `[[`,
      character(1L),
      "on"
    ),
    c("integer", "flag")
  )
})

test_that("native Domains are detached tables with valid ownership", {
  param_set = ps(
    x = p_dbl(0, 1, tags = c("first", "second")),
    parent = p_int(0, 2)
  )
  param_set$add_dep("x", "parent", CondEqual(1L))
  private = param_set$.__enclos_env__$private
  params_before = lapply(private$.params, identity)
  tags_before = lapply(private$.tags, identity)
  deps_before = lapply(private$.deps, identity)
  domain = param_set$get_domain("x")

  expect_warning(
    data.table::set(domain, i = 1L, j = "lower", value = -10),
    NA
  )
  expect_warning(data.table::set(domain, j = "added", value = 1L), NA)
  domain$.tags[[1L]][[1L]] = "changed"
  domain$.requirements[[1L]][[1L]]$on = "changed"

  expect_identical(domain$lower, -10)
  expect_identical(domain$added, 1L)
  expect_identical(data.table:::selfrefok(domain, verbose = FALSE), 1L)
  expect_identical(lapply(private$.params, identity), params_before)
  expect_identical(lapply(private$.tags, identity), tags_before)
  expect_identical(lapply(private$.deps, identity), deps_before)
})

test_that("native get_domain retains admitted columns across GC finalizers", {
  collection = NULL
  finalizer = new.env(parent = emptyenv())
  finalizer$pointer = NULL
  FinalizingChild = R6::R6Class(
    "ParamSetGetDomainFinalizingChild",
    inherit = ParamSet,
    private = list(
      .get_values = function() {
        # Make the mutator unreachable only after native admission and the
        # permanent parameter-row snapshot. Finalizer timing before this
        # callback would test a different, legitimately declined state.
        finalizer$pointer = NULL
        gc(FALSE)
        list(x = 0.5)
      }
    )
  )
  child = FinalizingChild$new(list(x = p_dbl(0, 1)))
  collection = ParamSetCollection$new(list(child))
  private = collection$.__enclos_env__$private

  finalizer$pointer = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    private$.params,
    0L,
    "mutated-after-admission"
  )
  expect_identical(private$.params$id, "x")
  previous = gctorture2(1L, wait = 0L)
  on.exit(gctorture2(previous), add = TRUE)
  observed = native_get_domain_call(collection, "x")
  gctorture2(previous)

  expect_identical(private$.params$id, "mutated-after-admission")
  expect_identical(observed$id, "x")
  expect_identical(observed$.init[[1L]], 0.5)
})

test_that("ParamSetCollection prefixes, postfixes, and live state are preserved", {
  left = ps(
    parent = p_int(0, 3, tags = "left", init = 2L),
    child = p_fct(c("a", "b"), trafo = toupper)
  )
  left$add_dep("child", "parent", CondEqual(2L))
  right = ps(flag = p_lgl(tags = "right", init = TRUE))

  prefix = ParamSetCollection$new(list(left = left, right = right))
  prefix$add_dep("right.flag", "left.parent", CondEqual(2L))
  for (id in prefix$ids()) {
    native_get_domain_expect_equivalent(
      prefix$get_domain(id),
      native_get_domain_reference(prefix, id)
    )
  }
  expect_identical(prefix$get_domain("left.parent")$.init[[1L]], 2L)
  expect_identical(
    prefix$get_domain("left.child")$.requirements[[1L]][[1L]]$on,
    "left.parent"
  )
  expect_identical(
    prefix$get_domain("right.flag")$.requirements[[1L]][[1L]]$on,
    "left.parent"
  )

  left$values = list(parent = 3L)
  expect_identical(prefix$get_domain("left.parent")$.init[[1L]], 3L)

  postfix = ParamSetCollection$new(
    list(left = left, right = right),
    postfix_names = TRUE
  )
  for (id in postfix$ids()) {
    native_get_domain_expect_equivalent(
      postfix$get_domain(id),
      native_get_domain_reference(postfix, id)
    )
  }
  expect_identical(
    postfix$get_domain("child.left")$.requirements[[1L]][[1L]]$on,
    "parent.left"
  )
})

test_that("collection state is read in historical order only after a hit", {
  events = new.env(parent = emptyenv())
  events$seen = character()
  CountingParamSet = R6::R6Class(
    "ParamSetGetDomainCountingChild",
    inherit = ParamSet,
    active = list(
      values = function(xs) {
        if (!missing(xs)) {
          super$values <- xs
          return(xs)
        }
        events$seen = c(events$seen, "values")
        super$values
      },
      deps = function(value) {
        if (!missing(value)) {
          super$deps <- value
          return(value)
        }
        events$seen = c(events$seen, "deps")
        super$deps
      }
    )
  )
  child = CountingParamSet$new(list(x = p_dbl(0, 1)))
  collection = ParamSetCollection$new(list(child = child))

  events$seen = character()
  expect_s3_class(collection$get_domain("child.x"), "Domain")
  expect_identical(events$seen, c("values", "deps"))

  events$seen = character()
  expect_error(
    collection$get_domain("absent"),
    "No param with id 'absent'",
    fixed = TRUE
  )
  expect_identical(events$seen, character())
})

test_that("base get_domain authenticates values and deps without replay", {
  param_set = ps(x = p_int(0, 2, init = 1L), parent = p_lgl(init = TRUE))
  param_set$add_dep("x", "parent", CondEqual(TRUE))
  events = new.env(parent = emptyenv())
  events$seen = character()
  original_values = activeBindingFunction("values", param_set)
  original_deps = activeBindingFunction("deps", param_set)
  makeActiveBinding("values", function(xs) {
    if (missing(xs)) {
      events$seen = c(events$seen, "values")
      return(original_values())
    }
    original_values(xs)
  }, param_set)
  makeActiveBinding("deps", function(value) {
    if (missing(value)) {
      events$seen = c(events$seen, "deps")
      return(original_deps())
    }
    original_deps(value)
  }, param_set)

  expect_null(native_get_domain_call(param_set, "x"))
  expect_identical(events$seen, character())
  expect_s3_class(param_set$get_domain("x"), "Domain")
  expect_identical(events$seen, c("values", "deps"))

  events$seen = character()
  expect_error(
    param_set$get_domain("absent"),
    "No param with id 'absent'",
    fixed = TRUE
  )
  expect_identical(events$seen, character())
})

test_that("collection extension callbacks cannot invalidate native snapshots", {
  skip_on_cran()

  collection = NULL
  RebindingParamSet = R6::R6Class(
    "ParamSetGetDomainRebindingChild",
    inherit = ParamSet,
    private = list(
      .get_values = function() {
        collection_private = collection$.__enclos_env__$private
        collection_private$.params = data.table::copy(collection_private$.params)
        data.table::set(
          collection_private$.params,
          i = 1L,
          j = "lower",
          value = -999
        )
        data.table::set(collection_private$.tags, i = 2L, j = "id", value = "x")
        data.table::setkeyv(collection_private$.tags, "id")
        replacement = function(value) value + 1
        data.table::set(
          collection_private$.trafos,
          i = 1L,
          j = "trafo",
          value = list(replacement)
        )
        list(x = 0.5)
      }
    )
  )
  child = RebindingParamSet$new(list(
    x = p_dbl(0, 1, tags = "first", trafo = identity),
    y = p_dbl(0, 1, tags = "second")
  ))
  collection = ParamSetCollection$new(list(child))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = collection$get_domain("x")
  gctorture(previous)

  expect_s3_class(observed, "Domain")
  expect_identical(observed$id, "x")
  expect_identical(observed$lower, 0)
  expect_identical(observed$.tags[[1L]], c("first", "second"))
  expect_identical(observed$.trafo[[1L]](1), 2)
  expect_identical(observed$.init_given, TRUE)
  expect_identical(observed$.init[[1L]], 0.5)
})

test_that("collection callbacks expose live tag and trafo table rebindings", {
  skip_on_cran()

  collection = NULL
  RebindingTablesParamSet = R6::R6Class(
    "ParamSetGetDomainRebindingTablesChild",
    inherit = ParamSet,
    private = list(
      .get_values = function() {
        collection_private = collection$.__enclos_env__$private
        collection_private$.tags = data.table::data.table(
          id = "x",
          tag = "rebound",
          key = "id"
        )
        collection_private$.trafos = data.table::data.table(
          id = "x",
          trafo = list(function(value) value + 10),
          key = "id"
        )
        list(x = 0.5)
      }
    )
  )
  child = RebindingTablesParamSet$new(list(
    x = p_dbl(0, 1, tags = "original", trafo = identity)
  ))
  collection = ParamSetCollection$new(list(child))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = collection$get_domain("x")
  gctorture(previous)

  expect_identical(observed$.tags[[1L]], "rebound")
  expect_identical(observed$.trafo[[1L]](1), 11)
  expect_identical(observed$.init[[1L]], 0.5)
})

test_that("collection get_domain never replays admitted callbacks", {
  collection = NULL
  state = new.env(parent = emptyenv())
  state$calls = 0L
  CorruptingParamSet = R6::R6Class(
    "ParamSetGetDomainCorruptingChild",
    inherit = ParamSet,
    private = list(
      .get_values = function() {
        state$calls = state$calls + 1L
        collection$.__enclos_env__$private$.tags = list(corrupt = TRUE)
        list(x = 0.5)
      }
    )
  )
  child = CorruptingParamSet$new(list(x = p_dbl(0, 1)))
  collection = ParamSetCollection$new(list(child))

  expect_error(
    collection$get_domain("x"),
    "ParamSetCollection get_domain callbacks produced unsupported state",
    fixed = TRUE
  )
  expect_identical(state$calls, 1L)
})

test_that("collection domain batches retain rebound trafo callbacks across rows", {
  skip_on_cran()

  collection = NULL
  state = new.env(parent = emptyenv())
  state$row = 0L
  state$events = character()

  RebindingDepsParamSet = R6::R6Class(
    "ParamSetGetDomainRebindingDepsChild",
    inherit = ParamSet,
    active = list(
      deps = function(value) {
        if (!missing(value)) {
          super$deps = value
          return(value)
        }

        state$row = state$row + 1L
        id = c("x", "y")[[state$row]]
        state$events = c(state$events, sprintf("%s:deps-rebind", id))
        collection_private = collection$.__enclos_env__$private
        replacement = function(value) value + 100
        trafos = data.table::copy(collection_private$.trafos)
        data.table::set(
          trafos,
          which(trafos$id == id),
          "trafo",
          list(replacement)
        )
        collection_private$.trafos = trafos
        super$deps
      }
    )
  )

  child = RebindingDepsParamSet$new(list(
    x = p_dbl(0, 1, trafo = identity),
    y = p_dbl(0, 1, trafo = identity)
  ))
  collection = ParamSetCollection$new(list(child))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = collection$domains
  gctorture(previous)

  expect_identical(state$events, c("x:deps-rebind", "y:deps-rebind"))
  expect_identical(observed$x$.trafo[[1L]](1), 101)
  expect_identical(observed$y$.trafo[[1L]](1), 101)
})

test_that("extensions and invalid requests retain the R fallback", {
  make_custom_domain = function() {
    paradox:::Domain(
      cls = "ParamGetDomainExtension",
      grouping = "ParamGetDomainExtension",
      storage_type = "list"
    )
  }
  custom = make_custom_domain()
  custom_set = ParamSet$new(list(custom = custom))
  expect_null(native_get_domain_call(custom_set, "custom"))
  observed = custom_set$get_domain("custom")
  expect_s3_class(observed, "ParamGetDomainExtension")
  expect_identical(observed$cls, "ParamGetDomainExtension")

  Subclass = R6::R6Class(
    "ParamSetGetDomainSubclass",
    inherit = ParamSet
  )
  subclass = Subclass$new(list(x = p_int(0, 1)))
  expect_null(native_get_domain_call(subclass, "x"))
  native_get_domain_expect_equivalent(
    subclass$get_domain("x"),
    native_get_domain_reference(subclass, "x"),
    selfref = 0L
  )

  expect_error(custom_set$get_domain("absent"), "No param with id 'absent'", fixed = TRUE)
  expect_error(custom_set$get_domain(1L), "Must be of type 'string'")
  expect_null(native_get_domain_call(custom_set, NA_character_))
  expect_null(native_get_domain_call(custom_set, character()))
})

test_that("native get_domain rejects malformed storage without reading it", {
  param_set = ps(x = p_dbl(0, 1, tags = "tag", trafo = exp))
  private = param_set$.__enclos_env__$private
  params = data.table::copy(private$.params)
  tags = data.table::copy(private$.tags)
  trafos = data.table::copy(private$.trafos)

  expect_null(native_get_domain_with_private(param_set, ".params", 1L))
  expect_null(native_get_domain_with_private(param_set, ".params", unname(params)))
  expect_null(native_get_domain_with_private(
    param_set,
    ".params",
    native_get_domain_replace(params, "id", NA_character_)
  ))
  expect_null(native_get_domain_with_private(
    param_set,
    ".params",
    native_get_domain_replace(params, "cls", "Unknown")
  ))
  expect_null(native_get_domain_with_private(
    param_set,
    ".params",
    native_get_domain_replace(params, "lower", list(0))
  ))
  expect_null(native_get_domain_with_private(
    param_set,
    ".params",
    native_get_domain_replace(params, "levels", list("bad"))
  ))
  expect_null(native_get_domain_with_private(
    param_set,
    ".params",
    native_get_domain_replace(params, "lower", structure(0, note = TRUE))
  ))
  expect_null(native_get_domain_with_private(
    param_set,
    ".params",
    native_get_domain_replace(params, "lower", matrix(0, 1L, 1L))
  ))

  expect_null(native_get_domain_with_private(
    param_set,
    ".tags",
    list(id = "x", tag = "tag")
  ))
  expect_null(native_get_domain_with_private(
    param_set,
    ".tags",
    native_get_domain_replace(tags, "tag", NA_character_)
  ))
  unkeyed_tags = data.table::copy(tags)
  data.table::setattr(unkeyed_tags, "sorted", NULL)
  expect_null(native_get_domain_with_private(param_set, ".tags", unkeyed_tags))

  expect_null(native_get_domain_with_private(
    param_set,
    ".trafos",
    native_get_domain_replace(trafos, "trafo", list(1L))
  ))
  unkeyed_trafos = data.table::copy(trafos)
  data.table::setattr(unkeyed_trafos, "sorted", NULL)
  expect_null(native_get_domain_with_private(param_set, ".trafos", unkeyed_trafos))

  expect_null(native_get_domain_with_private(
    param_set,
    ".deps",
    list(id = character(), on = character(), cond = list())
  ))
  expect_null(native_get_domain_with_private(
    param_set,
    ".values",
    unname(private$.values)
  ))
  expect_null(native_get_domain_call(
    param_set,
    structure("x", class = "custom_id")
  ))

  duplicate = data.table::rbindlist(list(params, params))
  expect_null(native_get_domain_with_private(param_set, ".params", duplicate))
  expect_null(.Call(native_get_domain_symbol(), private, new.env(), "x"))
})

test_that("native joins compare character encodings like R", {
  utf8 = enc2utf8("caf\u00e9")
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  Encoding(utf8) = "UTF-8"
  Encoding(latin1) = "latin1"
  skip_if(identical(charToRaw(utf8), charToRaw(latin1)))

  param_set = ps(x = p_dbl(0, 1, tags = "encoded", trafo = exp))
  private = param_set$.__enclos_env__$private
  data.table::set(private$.params, i = 1L, j = "id", value = utf8)
  data.table::set(private$.tags, i = 1L, j = "id", value = latin1)
  data.table::set(private$.trafos, i = 1L, j = "id", value = latin1)
  data.table::setkeyv(private$.tags, "id")
  data.table::setkeyv(private$.trafos, "id")
  private$.values = setNames(list(NULL), latin1)

  observed = native_get_domain_call(param_set, latin1)
  expect_identical(enc2utf8(observed$id), enc2utf8(utf8))
  expect_identical(observed$.tags[[1L]], "encoded")
  expect_identical(observed$.trafo[[1L]], exp)
  expect_true(observed$.init_given)
  expect_null(observed$.init[[1L]])
})

test_that("native get_domain scans share one cumulative interrupt budget", {
  size = 22000L
  ids = sprintf("parameter_%05d", seq_len(size))
  target = ids[[size]]
  params = data.table::data.table(
    id = ids,
    cls = rep("ParamDbl", size),
    grouping = rep("ParamDbl", size),
    cargo = rep(list(NULL), size),
    lower = rep(0, size),
    upper = rep(1, size),
    tolerance = rep(0, size),
    levels = rep(list(NULL), size),
    special_vals = rep(list(list()), size),
    default = rep(list(paradox:::NO_DEF), size),
    storage_type = rep("numeric", size)
  )
  tags = data.table::data.table(id = ids, tag = rep("bulk", size))
  condition = CondEqual(0)
  deps = data.table::data.table(
    id = ids,
    on = rep(ids[[1L]], size),
    cond = rep(list(condition), size)
  )
  trafos = data.table::data.table(id = character(), trafo = list())
  data.table::setkeyv(tags, "id")
  data.table::setkeyv(trafos, "id")

  param_set = ParamSet$new()
  private = param_set$.__enclos_env__$private
  private$.params = params
  private$.tags = tags
  private$.trafos = trafos
  private$.deps = deps
  private$.values = structure(list(), names = character())

  observed = native_get_domain_call(param_set, target)

  expect_identical(observed$id, target)
  expect_identical(observed$.tags[[1L]], "bulk")
  expect_identical(observed$.requirements[[1L]][[1L]]$on, ids[[1L]])
  expect_identical(data.table:::selfrefok(observed, verbose = FALSE), 1L)
})
