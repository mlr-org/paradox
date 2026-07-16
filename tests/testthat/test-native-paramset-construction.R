native_paramset_construct_symbol = function() {
  get("C_param_set_construct", envir = asNamespace("paradox"))
}

native_paramset_index_layout_replay = function(first_valid) {
  dso = getLoadedDLLs()[["paradox"]][["path"]]
  callr::r(function(dso, first_valid) {
    dll = dyn.load(dso)
    symbol = getDLLRegisteredRoutines(dll)[[".Call"]][[
      "param_set_index_layout"
    ]]

    params = data.table::data.table(
      id = c("z", "a", "m"),
      cls = rep("ParamDbl", 3L),
      grouping = rep("ParamDbl", 3L)
    )
    data.table::setindexv(params, c("id", "cls", "grouping"))
    tags = data.table::data.table(
      tag = c("a", "B", "", "_", "b", "A", "a")
    )
    data.table::setindexv(tags, "tag")
    identity = data.table::data.table(
      id = c("a", "m", "z"),
      cls = rep("ParamDbl", 3L),
      grouping = rep("ParamDbl", 3L)
    )
    data.table::setindexv(identity, c("id", "cls", "grouping"))
    empty = data.table::data.table(tag = character())
    data.table::setindexv(empty, "tag")

    arguments = list(
      base::getNamespaceVersion("data.table")[[1L]],
      attr(params, "index", exact = TRUE),
      attr(tags, "index", exact = TRUE),
      attr(identity, "index", exact = TRUE),
      attr(empty, "index", exact = TRUE)
    )
    invoke = function(valid) {
      .Call(
        symbol,
        if (valid) arguments[[1L]] else "unsupported",
        arguments[[2L]],
        arguments[[3L]],
        arguments[[4L]],
        arguments[[5L]]
      )
    }
    c(first = invoke(first_valid), replay = invoke(!first_valid))
  }, args = list(dso = dso, first_valid = first_valid))
}

native_paramset_force_constructor_fallback = function(domains) {
  lapply(domains, function(domain) {
    domain = data.table::copy(domain)
    class(domain) = c("ParamSetConstructionFallback", class(domain))
    domain
  }) |>
    setNames(names(domains))
}

native_paramset_private_state = function(param_set) {
  private = param_set$.__enclos_env__$private
  list(
    params = as.list(private$.params),
    tags = as.list(private$.tags),
    trafos = as.list(private$.trafos),
    deps = as.list(private$.deps),
    values = private$.values
  )
}

native_paramset_expect_equivalent = function(actual, expected) {
  expect_identical(actual$ids(), expected$ids())
  expect_identical(actual$class, expected$class)
  expect_identical(actual$lower, expected$lower)
  expect_identical(actual$upper, expected$upper)
  expect_identical(actual$levels, expected$levels)
  expect_identical(actual$storage_type, expected$storage_type)
  expect_identical(actual$special_vals, expected$special_vals)
  expect_identical(actual$default, expected$default)
  expect_identical(actual$tags, expected$tags)
  expect_identical(actual$values, expected$values)
  expect_identical(as.list(actual$deps), as.list(expected$deps))
  expect_identical(
    native_paramset_private_state(actual),
    native_paramset_private_state(expected)
  )
}

test_that("native ParamSet construction is registered with a forced symbol", {
  symbol = native_paramset_construct_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 1L)
  layout_symbol = get(
    "C_param_set_index_layout",
    envir = asNamespace("paradox")
  )
  expect_s3_class(layout_symbol, "NativeSymbolInfo")
  expect_identical(layout_symbol$numParameters, 5L)
  expect_error(
    .Call("param_set_construct", list(), PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("data.table index-layout configuration is one-shot and fail-closed", {
  enabled_first = native_paramset_index_layout_replay(TRUE)
  expect_identical(enabled_first[["replay"]], enabled_first[["first"]])
  if (base::getNamespaceVersion("data.table")[[1L]] %in%
      c("1.17.8", "1.18.4")) {
    expect_true(enabled_first[["first"]])
  }

  unsupported_first = native_paramset_index_layout_replay(FALSE)
  expect_false(unsupported_first[["first"]])
  expect_false(unsupported_first[["replay"]])
})

test_that("native construction reuses the probed data.table index layout", {
  domains = list(
    z = p_int(tags = c("a", "B")),
    a = p_dbl(tags = c("", "_")),
    m = p_lgl(tags = c("b", "A"))
  )
  bundle = .Call(native_paramset_construct_symbol(), domains)
  params_index = attr(bundle$params, "index", exact = TRUE)
  tags_index = attr(bundle$tags, "index", exact = TRUE)
  if (is.null(attr(
      params_index,
      "__id__cls__grouping",
      exact = TRUE
    ))) {
    skip("loaded data.table uses a different secondary-index layout")
  }

  expected_params = data.table::copy(bundle$params)
  attr(expected_params, "index") = NULL
  data.table::setindexv(expected_params, c("id", "cls", "grouping"))
  expected_tags = data.table::copy(bundle$tags)
  attr(expected_tags, "index") = NULL
  data.table::setindexv(expected_tags, "tag")
  expect_identical(
    params_index,
    attr(expected_params, "index", exact = TRUE)
  )
  expect_identical(
    tags_index,
    attr(expected_tags, "index", exact = TRUE)
  )

  non_ascii = list(
    x = p_fct(c("gr\u00f6\u00dfe", "small"), tags = "gr\u00f6\u00dfe")
  )
  direct = .Call(native_paramset_construct_symbol(), non_ascii)
  expect_null(attr(
    attr(direct$params, "index", exact = TRUE),
    "__id__cls__grouping",
    exact = TRUE
  ))
  expect_null(attr(
    attr(direct$tags, "index", exact = TRUE),
    "__tag",
    exact = TRUE
  ))
  public = ParamSet$new(non_ascii)
  expect_identical(
    data.table::indices(public$.__enclos_env__$private$.params),
    "id__cls__grouping"
  )
  expect_identical(
    data.table::indices(public$.__enclos_env__$private$.tags),
    "tag"
  )
})

test_that("native construction creates canonical empty and mixed bundles", {
  symbol = native_paramset_construct_symbol()

  empty = .Call(symbol, setNames(list(), character()))
  expect_named(
    empty,
    c("params", "tags", "trafos", "requirements", "init_values")
  )
  expect_s3_class(empty$params, "data.table")
  expect_identical(
    vapply(empty$params, typeof, character(1L)),
    c(
      id = "character", cls = "character", grouping = "character",
      cargo = "list", lower = "double", upper = "double",
      tolerance = "double", levels = "list", special_vals = "list",
      default = "list", storage_type = "character"
    )
  )
  expect_identical(dim(empty$params), c(0L, 11L))
  expect_identical(empty$init_values, setNames(list(), character()))
  expect_identical(attr(empty$tags, "sorted"), "id")
  expect_identical(attr(empty$trafos, "sorted"), "id")

  marker = new.env(parent = emptyenv())
  custom_check = function(x) TRUE
  scale_trafo = function(x) x * 2
  domains = list(
    zeta = p_dbl(-2, 2, tags = c("numeric", "common"), trafo = scale_trafo),
    alpha = p_int(0, 5, tags = c("control", "common"), init = 2L),
    payload = p_uty(custom_check = custom_check, default = marker, tags = "object"),
    mode = p_fct(
      c("small", "large"),
      tags = "categorical",
      depends = alpha == 2L && zeta %in% c(-1, 1)
    ),
    flag = p_lgl(default = FALSE)
  )
  bundle = .Call(symbol, domains)

  expect_s3_class(bundle$params, "data.table")
  expect_identical(bundle$params$id, names(domains))
  expect_identical(
    bundle$params$cls,
    unname(vapply(domains, `[[`, "", "cls"))
  )
  expect_identical(
    bundle$tags$id,
    c("alpha", "alpha", "mode", "payload", "zeta", "zeta")
  )
  expect_identical(
    bundle$tags$tag,
    c("control", "common", "categorical", "object", "numeric", "common")
  )
  expect_identical(bundle$trafos$id, "zeta")
  expect_identical(bundle$trafos$trafo[[1L]], scale_trafo)
  expect_length(bundle$requirements, length(domains))
  expect_length(bundle$requirements[[4L]], 2L)
  expect_identical(bundle$init_values, list(alpha = 2L))
  expect_identical(bundle$params$default[[3L]], marker)
  expect_identical(bundle$params$cargo[[3L]]$custom_check, custom_check)

  restored_domains = unserialize(serialize(domains, NULL, version = 3L))
  expect_named(.Call(symbol, restored_domains), names(bundle))

  integer_bounds = list(x = p_int(0L, 5L, tolerance = 0L))
  integer_bundle = .Call(symbol, integer_bounds)
  expect_false(is.null(integer_bundle))
  expect_type(integer_bundle$params$lower, "integer")
  expect_type(integer_bundle$params$upper, "integer")
  expect_type(integer_bundle$params$tolerance, "integer")

  promoted_bounds = .Call(symbol, c(integer_bounds, y = list(p_dbl(0, 1))))
  expect_type(promoted_bounds$params$lower, "double")
  expect_type(promoted_bounds$params$upper, "double")
  expect_type(promoted_bounds$params$tolerance, "double")
})

test_that("native and retained constructor paths agree on rich Domain input", {
  marker = new.env(parent = emptyenv())
  marker$value = 42L
  scale_trafo = local({
    multiplier = 3
    function(x) x * multiplier
  })
  domains = list(
    zeta = p_dbl(-2, 2, tags = c("numeric", "common"), trafo = scale_trafo),
    alpha = p_int(
      0,
      5,
      special_vals = list(99L),
      default = 1L,
      tags = c("control", "common"),
      init = 2L
    ),
    payload = p_uty(
      custom_check = function(x) TRUE,
      default = marker,
      tags = "object"
    ),
    mode = p_fct(
      c("small", "large"),
      tags = "categorical",
      depends = alpha == 2L && zeta %in% c(-1, 1)
    ),
    flag = p_lgl(default = FALSE)
  )

  native = ParamSet$new(domains)
  fallback = ParamSet$new(native_paramset_force_constructor_fallback(domains))
  native_paramset_expect_equivalent(native, fallback)

  native_private = native$.__enclos_env__$private
  expect_identical(native_private$.params$id, names(domains))
  expect_identical(native_private$.tags$id, sort(native_private$.tags$id))
  expect_identical(attr(native_private$.tags, "sorted"), "id")
  expect_identical(attr(native_private$.trafos, "sorted"), "id")
  expect_identical(data.table:::selfrefok(native_private$.params, FALSE), 1L)
  expect_identical(data.table:::selfrefok(native_private$.tags, FALSE), 1L)
  expect_identical(data.table:::selfrefok(native_private$.trafos, FALSE), 1L)
  expect_identical(native_private$.params$default[[3L]], marker)
  expect_identical(native_private$.trafos$trafo[[1L]], scale_trafo)

  expect_true(native$check(list(
    zeta = 1,
    alpha = 2L,
    payload = marker,
    mode = "large",
    flag = TRUE
  )))
  expect_false(native$test(list(mode = "large", alpha = 2L, flag = TRUE)))
  expect_identical(native$trafo(list(zeta = 2, alpha = 2L)), list(zeta = 6, alpha = 2L))
  expect_identical(native$get_domain("mode")$.requirements, domains$mode$.requirements)

  subset = native$subset(c("zeta", "alpha", "mode"))
  expect_identical(subset$ids(), c("zeta", "alpha", "mode"))
  expect_identical(subset$values, list(alpha = 2L))
  expect_true(subset$check(list(zeta = 1, alpha = 2L, mode = "small")))

  for (copy in list(
    native$clone(deep = TRUE),
    unserialize(serialize(native, NULL, version = 3L))
  )) {
    expect_identical(copy$ids(), native$ids())
    expect_identical(copy$tags, native$tags)
    expect_identical(copy$values$alpha, 2L)
    expect_true(copy$check(list(
      zeta = 1,
      alpha = 2L,
      payload = marker,
      mode = "small",
      flag = FALSE
    )))
  }
})

test_that("native constructor storage is immediately safe by reference", {
  param_set = ParamSet$new(list(x = p_dbl(0, 1)))
  private = param_set$.__enclos_env__$private

  expect_warning(
    data.table::set(private$.params, i = 1L, j = "lower", value = -1),
    NA
  )
  expect_identical(private$.params$lower, -1)
  expect_identical(data.table:::selfrefok(private$.params, FALSE), 1L)
})

test_that("construction falls back for extensions, encodings, and corrupt rows", {
  symbol = native_paramset_construct_symbol()
  canonical = list(x = p_int(0, 3))

  custom = native_paramset_force_constructor_fallback(canonical)
  expect_null(.Call(symbol, custom))
  expect_identical(ParamSet$new(custom)$ids(), "x")

  unicode = setNames(canonical, "gr\u00f6\u00dfe")
  expect_null(.Call(symbol, unicode))

  invalid_ascii = setNames(canonical, "not a strict id")
  expect_null(.Call(symbol, invalid_ascii))

  expect_null(.Call(symbol, unname(canonical)))
  expect_s3_class(.Call(symbol, list())$params, "data.table")

  corruptions = list(
    function(x) { names(x)[[1L]] = "unexpected"; x },
    function(x) { x$lower = TRUE; x },
    function(x) { x$.tags = list(NA_character_); x },
    function(x) { x$.trafo = list(1); x },
    function(x) { x$.requirements = list(quote(x == 1)); x },
    function(x) { x$.init_given = NA; x },
    function(x) { class(x) = c("ParamInt", "Domain", "data.frame"); x }
  )
  for (corrupt in corruptions) {
    domain = corrupt(data.table::copy(canonical$x))
    expect_null(.Call(symbol, list(x = domain)))
  }
})

test_that("native constructor sorting crosses its interrupt interval", {
  size = 32769L
  ids = sprintf("parameter_%05d", rev(seq_len(size)))
  domains = setNames(rep(list(p_lgl()), size), ids)

  bundle = .Call(native_paramset_construct_symbol(), domains)
  expect_false(is.null(bundle))
  expect_identical(bundle$params$id, ids)
  expect_length(bundle$requirements, size)
  expect_identical(bundle$tags$id, character())
  expect_identical(bundle$trafos$id, character())
})

test_that("the native gate retains exact invalid constructor diagnostics", {
  capture_error = function(expr) {
    tryCatch(
      {
        force(expr)
        NA_character_
      },
      error = conditionMessage
    )
  }

  expect_identical(
    capture_error(ParamSet$new(1)),
    "Assertion on 'params' failed: Must be of type 'list', not 'double'."
  )
  expect_identical(
    capture_error(ParamSet$new(list(x = 1))),
    paste0(
      "Assertion on 'params' failed: May only contain the following types: ",
      "{Domain}, but element 1 has type 'numeric'."
    )
  )
  expect_identical(
    capture_error(ParamSet$new(list(p_int()))),
    "Assertion on 'names(params)' failed: Must have names."
  )
  expect_identical(
    capture_error(ParamSet$new(setNames(list(p_int()), "a b"))),
    paste0(
      "Assertion on 'names(params)' failed: Must have names according to R's ",
      "variable naming conventions, but element 1 does not comply."
    )
  )
  expect_identical(
    capture_error(ParamSet$new(setNames(
      list(p_int(), p_dbl()),
      c("a", "a")
    ))),
    paste0(
      "Assertion on 'names(params)' failed: Must have unique names, but ",
      "element 2 is duplicated."
    )
  )
})

test_that("initialize retains its invisible init-value return contract", {
  domains = list(
    count = p_int(0, 5, init = 2L),
    mode = p_fct(c("a", "b"))
  )
  native = ParamSet$new()
  fallback = ParamSet$new()

  native_return = withVisible(native$initialize(domains))
  fallback_return = withVisible(fallback$initialize(
    native_paramset_force_constructor_fallback(domains)
  ))
  expect_identical(native_return, fallback_return)
  expect_identical(native_return, list(value = list(count = 2L), visible = FALSE))
  native_paramset_expect_equivalent(native, fallback)
})

test_that("native construction agrees with fallback across randomized schemas", {
  set.seed(20260713)
  tags = c("train", "control", "required", "internal", "common")

  for (iteration in seq_len(30L)) {
    size = sample.int(40L, 1L)
    ids = sprintf("parameter_%03d", sample.int(2000L, size))
    domains = lapply(seq_len(size), function(index) {
      selected_tags = sample(tags, sample.int(3L, 1L) - 1L)
      switch(
        as.character((index - 1L) %% 5L),
        "0" = p_dbl(
          -index,
          index,
          tolerance = index / 10000,
          tags = selected_tags,
          trafo = function(x) x + 1
        ),
        "1" = p_int(-index, index, tags = selected_tags),
        "2" = p_fct(letters[seq_len(1L + index %% 12L)], tags = selected_tags),
        "3" = p_lgl(tags = selected_tags, init = index %% 2L == 0L),
        "4" = p_uty(custom_check = function(x) TRUE, tags = selected_tags)
      )
    })
    names(domains) = ids

    native = ParamSet$new(domains)
    fallback = ParamSet$new(native_paramset_force_constructor_fallback(domains))
    native_paramset_expect_equivalent(native, fallback)
    expect_identical(native$ids(), ids)
  }
})
