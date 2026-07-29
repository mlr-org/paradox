native_value_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"), inherits = FALSE)
}

native_value_private = function(param_set) {
  param_set$.__enclos_env__$private
}

native_value_state = function(param_set) {
  paradox:::param_set_core_state(native_value_private(param_set))
}

native_value_core_address = function(param_set) {
  data.table::address(native_value_private(param_set)$.core)
}

native_value_merge = function(dots, values = list(), current = NULL,
    insert = FALSE) {
  .Call(
    native_value_symbol("param_set_values_merge"),
    dots,
    values,
    current,
    insert
  )
}

native_value_store = function(param_set, values,
    private = native_value_private(param_set)) {
  .Call(
    native_value_symbol("param_set_store_values"),
    private,
    param_set,
    values
  )
}

native_value_check = function(param_set, values,
    private = native_value_private(param_set)) {
  .Call(
    native_value_symbol("param_set_assign_values_checked"),
    private,
    param_set,
    values
  )
}

test_that("value mutation routines have one forced native signature", {
  expected = c(
    param_set_values_merge = 4L,
    param_set_store_values = 3L,
    param_set_assign_values_checked = 3L
  )
  for (name in names(expected)) {
    symbol = native_value_symbol(name)
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, expected[[name]])
  }
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("native merge owns replacement and insertion semantics", {
  reference = new.env(parent = emptyenv())
  current = list(a = 1L, b = 2L, nullable = NULL, reference = reference)

  inserted = native_value_merge(
    list(b = 3L, nullable = NULL),
    list(c = 4L),
    current,
    TRUE
  )
  expect_identical(
    inserted,
    list(a = 1L, b = 3L, reference = reference, c = 4L)
  )
  expect_false(identical(
    data.table::address(inserted),
    data.table::address(current)
  ))

  expect_identical(
    native_value_merge(list(nullable = NULL), list(b = 5L)),
    list(nullable = NULL, b = 5L)
  )
  expect_identical(native_value_merge(list(), list()), list())
})

test_that("native merge rejects malformed input without a sentinel", {
  expect_error(
    native_value_merge(
      structure(list(1L, 2L), names = c("a", "a")),
      list()
    ),
    "unique, disjoint names"
  )
  expect_error(
    native_value_merge(list(a = 1L), list(a = 2L)),
    "unique, disjoint names"
  )
  expect_error(
    native_value_merge(list(1L), list()),
    "plain named list"
  )
  expect_identical(
    native_value_merge(
      structure(list(a = 1L), class = "configuration_extension"),
      list()
    ),
    list(a = 1L)
  )
  expect_error(
    native_value_merge(list(a = 1L), list(), list(), NA),
    "`.insert` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    native_value_merge(list(a = 1L), list(), list(1L), TRUE),
    "Current ParamSet values are corrupt",
    fixed = TRUE
  )
})

test_that("value transactions ignore harmless outer S3 container classes", {
  param_set = ps(
    n_searches = p_int(1L, 20L),
    mut_sd = p_dbl(0, 1)
  )
  configuration = structure(
    list(n_searches = 10L, mut_sd = 0.1),
    class = "local_search_control"
  )

  param_set$values = configuration

  expect_identical(
    param_set$values,
    list(n_searches = 10L, mut_sd = 0.1)
  )
  expect_null(attr(param_set$values, "class", exact = TRUE))
})

test_that("public BASE setters commit one fresh canonical capsule", {
  param_set = ps(
    count = p_int(0L, 10L, tolerance = 0.2),
    ratio = p_dbl(0, 1, tolerance = 0.1),
    payload = p_uty()
  )
  old_state = native_value_state(param_set)
  old_address = native_value_core_address(param_set)

  param_set$values = list(ratio = 1.05, count = 2.1)
  expect_identical(param_set$values, list(count = 2L, ratio = 1))
  expect_identical(old_state$.values, setNames(list(), character()))
  expect_false(identical(old_address, native_value_core_address(param_set)))

  accepted_address = native_value_core_address(param_set)
  expect_error({
    param_set$values = list(count = 50L)
  }, "count:")
  expect_identical(param_set$values, list(count = 2L, ratio = 1))
  expect_identical(native_value_core_address(param_set), accepted_address)

  param_set$set_values(count = 3L)
  expect_identical(param_set$values, list(count = 3L, ratio = 1))
  param_set$set_values(.values = list(ratio = 0.25), .insert = FALSE)
  expect_identical(param_set$values, list(ratio = 0.25))
  param_set$set_values(ratio = NULL)
  expect_identical(param_set$values, setNames(list(), character()))

  expect_error(
    param_set$set_values(count = 1L, .values = list(count = 2L)),
    "unique, disjoint names"
  )
  expect_error(
    param_set$set_values(count = 1L, .insert = NA),
    "`.insert` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_identical(param_set$values, setNames(list(), character()))
})

test_that("direct checked and unchecked calls each commit one capsule", {
  param_set = ps(a = p_int(0L, 4L, tolerance = 0.2), b = p_uty())
  param_set$values = list(a = 1L)
  old_state = native_value_state(param_set)
  before_address = native_value_core_address(param_set)

  checked = native_value_check(param_set, list(a = 2.1))
  expect_identical(checked, list(a = 2L))
  expect_identical(param_set$values, list(a = 2L))
  expect_identical(old_state$.values, list(a = 1L))
  expect_false(identical(native_value_core_address(param_set), before_address))
  checked_address = native_value_core_address(param_set)
  expect_error(native_value_check(param_set, list(a = 10L)), "a:")
  expect_identical(param_set$values, list(a = 2L))
  expect_identical(native_value_core_address(param_set), checked_address)

  reference = new.env(parent = emptyenv())
  incoming = list(b = reference, a = 3L, unknown = "ignored")
  checked_state = native_value_state(param_set)
  stored = native_value_store(param_set, incoming)
  expect_identical(stored, list(a = 3L, b = reference))
  expect_identical(param_set$values, stored)
  expect_identical(checked_state$.values, list(a = 2L))
  expect_false(identical(checked_address, native_value_core_address(param_set)))
  expect_false(identical(
    data.table::address(stored),
    data.table::address(incoming)
  ))
  incoming$a = 4L
  expect_identical(param_set$values$a, 3L)
})

test_that("value transactions reject structural ALTREP list shells", {
  skip_if_no_list_altrep()

  callbacks = 0L
  values = native_stateful_altrep(
    structure(list(x = 1L), names = "x"),
    structure(list(x = 2L), names = "x"),
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = 0L
  )
  param_set = ps(x = p_int(0L, 2L))

  expect_error(
    { param_set$values = values },
    "ParamSet values must be supplied as a plain named list",
    fixed = TRUE
  )
  expect_error(
    native_value_store(param_set, values),
    "ParamSet values must be supplied as a plain named list",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
  expect_identical(param_set$values, setNames(list(), character()))

  hostile_names = native_stateful_altrep(
    "x",
    "y",
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = 0L
  )
  named_values = structure(list(1L), names = hostile_names)
  native_stateful_altrep_rearm(hostile_names)
  callbacks = 0L
  expect_error(
    { param_set$values = named_values },
    "ParamSet values must be supplied as a plain named list",
    fixed = TRUE
  )
  expect_error(
    native_value_store(param_set, named_values),
    "ParamSet values must be supplied as a plain named list",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)

  param_set$values = list()
  expect_identical(param_set$values, setNames(list(), character()))
  param_set$assert_values = FALSE
  param_set$values = list()
  expect_identical(param_set$values, setNames(list(), character()))
})

test_that("checked value diagnostics preserve non-native string encodings", {
  diagnostic_utf8 = enc2utf8("gr\u00fcndlich rejected")
  diagnostic_latin1 = iconv(
    diagnostic_utf8,
    from = "UTF-8",
    to = "latin1"
  )
  skip_if(
    is.na(diagnostic_latin1),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(diagnostic_latin1) = "latin1"

  armed = FALSE
  param_set = ps(payload = p_uty(custom_check = function(value) {
    if (armed) diagnostic_latin1 else TRUE
  }))
  armed = TRUE
  error = tryCatch(
    native_value_check(param_set, list(payload = 1L)),
    error = identity
  )

  expect_s3_class(error, "error")
  expect_identical(
    enc2utf8(conditionMessage(error)),
    "Assertion on 'xs' failed: payload: gr\u00fcndlich rejected."
  )
  expect_identical(param_set$values, setNames(list(), character()))
})

test_that("checked diagnostics transcode unknown IDs, token calls, and dependencies", {
  utf8 = enc2utf8(c(
    "m\u00fcssing", "appel\u00e9()", "accept\u00e9", "refus\u00e9"
  ))
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(
    anyNA(latin1),
    "this platform cannot represent the latin1 fixtures"
  )
  Encoding(latin1) = "latin1"

  checked = ps(payload = p_int())
  error = tryCatch(
    native_value_check(
      checked,
      setNames(list(1L), latin1[[1L]])
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_identical(
    enc2utf8(conditionMessage(error)),
    "Assertion on 'xs' failed: Parameter 'm\u00fcssing' not available."
  )
  bytes_id = utf8[[1L]]
  Encoding(bytes_id) = "bytes"
  expect_identical(
    checked$check(setNames(list(1L), bytes_id)),
    "Parameter 'm\\xc3\\xbcssing' not available"
  )

  token_target = ps(payload = p_dbl())
  token = to_tune()
  token$call = latin1[[2L]]
  token_diagnostic = token_target$check(
    list(payload = token)
  )
  expect_identical(
    enc2utf8(token_diagnostic),
    "appel\u00e9() must give a range for unbounded parameter payload."
  )

  dependent = ps(child = p_int(), parent = p_fct(latin1[3:4]))
  dependent$add_dep("child", "parent", CondEqual(latin1[[3L]]))
  dependency_diagnostic = dependent$check(list(
    child = 1L,
    parent = latin1[[4L]]
  ))
  expect_identical(
    enc2utf8(dependency_diagnostic),
    paste0(
      "child: can only be set if the following condition is met '",
      "parent == \"accept\u00e9\"'. Instead the current parameter value is: ",
      "parent == \"refus\u00e9\""
    )
  )
})

test_that("Shadow unknown-value diagnostics admit text and reject bytes", {
  unknown_utf8 = enc2utf8("m\u00fcssing")
  unknown_latin1 = iconv(unknown_utf8, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(unknown_latin1),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(unknown_latin1) = "latin1"
  unknown_bytes = unknown_utf8
  Encoding(unknown_bytes) = "bytes"

  origin = ps(hidden = p_int(), visible = p_int())
  shadow = ParamSetShadow$new(origin, "hidden")
  error = tryCatch(
    native_value_store(
      shadow,
      setNames(list(1L), unknown_latin1)
    ),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_identical(
    enc2utf8(conditionMessage(error)),
    "Parameter 'm\u00fcssing' not available in ParamSetShadow"
  )

  expect_error(
    native_value_store(shadow, setNames(list(1L), unknown_bytes)),
    "Unknown bytes-encoded parameter ID",
    fixed = TRUE
  )
  expect_identical(origin$values, setNames(list(), character()))
})

test_that("direct mutation calls fail closed on malformed ownership and input", {
  param_set = ps(a = p_int())
  other = ps(a = p_int())

  expect_error(
    native_value_store(param_set, list(1L)),
    "named list"
  )
  expect_error(
    native_value_store(
      param_set,
      structure(list(1L, 2L), names = c("a", "a"))
    ),
    "unique and non-missing"
  )
  expect_error(
    native_value_store(param_set, list(a = 1L), native_value_private(other)),
    "ownership|unsupported"
  )
  expect_error(
    native_value_check(param_set, list(a = 1L), native_value_private(other)),
    "ownership"
  )

  forged = new.env(parent = emptyenv())
  forged$.core = new("externalptr")
  expect_error(
    native_value_store(param_set, list(a = 1L), forged),
    "Corrupt|unsupported"
  )
  expect_error(
    native_value_check(param_set, list(a = 1L), forged),
    "Corrupt"
  )
  expect_identical(param_set$values, setNames(list(), character()))
})

test_that("core-method overrides are not executed by the sealed native path", {
  param_set = ps(a = p_int())
  calls = 0L
  unlockBinding("check", param_set)
  param_set$check = function(...) {
    calls <<- calls + 1L
    stop("unsupported override executed", call. = FALSE)
  }
  lockBinding("check", param_set)

  expect_identical(native_value_check(param_set, list(a = 1L)), list(a = 1L))
  expect_identical(calls, 0L)
  param_set$values = list(a = 2L)
  expect_identical(param_set$values, list(a = 2L))
  expect_identical(calls, 0L)
})

test_that("an intervening reentrant commit aborts the enclosing assignment", {
  holder = new.env(parent = emptyenv())
  armed = FALSE
  nested = FALSE
  param_set = ps(payload = p_uty(custom_check = function(value) {
    if (armed && !nested) {
      nested <<- TRUE
      holder$param_set$assert_values = FALSE
      holder$param_set$values = list(payload = "nested")
      holder$param_set$assert_values = TRUE
    }
    TRUE
  }))
  holder$param_set = param_set
  armed = TRUE

  expect_error({
    param_set$values = list(payload = "outer")
  }, "changed")
  expect_identical(param_set$values, list(payload = "nested"))
})

test_that("post-callback generation checks do not force delayed core bindings", {
  forced = 0L
  armed = FALSE
  private = NULL
  original_core = NULL
  param_set = ps(payload = p_uty(custom_check = function(value) {
    if (armed) {
      delayedAssign(
        ".core",
        {
          forced <<- forced + 1L
          original_core
        },
        assign.env = private
      )
    }
    TRUE
  }))
  private = native_value_private(param_set)
  original_core = private$.core
  armed = TRUE

  expect_error(
    param_set$values <- list(payload = "outer"),
    "changed during validation",
    fixed = TRUE
  )
  expect_identical(forced, 0L)
  expect_type(substitute(.core, private), "language")
})

test_that("generation checks reject a literal-core delayed binding", {
  armed = FALSE
  private = NULL
  original_core = NULL
  param_set = ps(payload = p_uty(custom_check = function(value) {
    if (armed) {
      eval(as.call(list(
        quote(delayedAssign),
        ".core",
        original_core,
        private,
        private
      )), baseenv())
    }
    TRUE
  }))
  private = native_value_private(param_set)
  original_core = private$.core
  armed = TRUE

  expect_error(
    param_set$values <- list(payload = "outer"),
    "changed during validation",
    fixed = TRUE
  )
  expect_identical(substitute(.core, private), original_core)
})

test_that("collection assignment routes all children through one transaction", {
  one = ps(a = p_int(), b = p_int())
  two = ps(c = p_int())
  three = ps(d = p_int())
  collection = ParamSetCollection$new(list(
    one = one,
    two = two,
    three = three
  ))

  collection$assert_values = FALSE
  collection$values = list(
    three.d = 4L,
    one.b = 2L,
    one.a = 1L,
    unknown = 9L
  )
  expect_identical(one$values, list(a = 1L, b = 2L))
  expect_identical(two$values, setNames(list(), character()))
  expect_identical(three$values, list(d = 4L))
})

test_that("collection validation preserves a nested commit in another child", {
  holder = new.env(parent = emptyenv())
  armed = FALSE
  nested = FALSE
  left = ps(payload = p_uty(custom_check = function(value) {
    if (armed && !nested) {
      nested <<- TRUE
      holder$right$assert_values = FALSE
      holder$right$values = list(x = 9L)
      holder$right$assert_values = TRUE
    }
    TRUE
  }))
  right = ps(x = p_int())
  holder$right = right
  collection = ParamSetCollection$new(list(left = left, right = right))
  armed = TRUE

  expect_error({
    collection$values = list(left.payload = "outer", right.x = 1L)
  }, "changed")
  expect_identical(left$values, setNames(list(), character()))
  expect_identical(right$values, list(x = 9L))
})

test_that("a malformed later child cannot partially commit an earlier child", {
  first = ps(a = p_int())
  second = ps(b = p_int())
  first$values = list(a = 7L)
  first_address = native_value_core_address(first)
  collection = ParamSetCollection$new(list(first = first, second = second))

  malformed = native_value_state(second)$.params
  malformed$id[[1L]] = NA_character_
  paradox:::param_set_core_replace(
    native_value_private(second),
    params = malformed
  )
  collection$assert_values = FALSE

  expect_error(
    collection$values <- list(first.a = 1L, second.b = 2L),
    "Corrupt"
  )
  expect_identical(first$values, list(a = 7L))
  expect_identical(native_value_core_address(first), first_address)
})

test_that("shared collection targets use deterministic last-owner semantics", {
  shared = ps(a = p_int(), b = p_int())
  collection = ParamSetCollection$new(list(left = shared, right = shared))
  collection$assert_values = FALSE

  expect_warning(
    collection$values <- list(left.a = 1L, right.b = 2L),
    "more than one path"
  )
  expect_identical(shared$values, list(b = 2L))
})

test_that("shadow writes resolve to the origin and preserve hidden values", {
  origin = ps(hidden = p_int(), visible = p_int())
  origin$values = list(hidden = 7L, visible = 1L)
  shadow = ParamSetShadow$new(origin, "hidden")

  shadow$values = list(visible = 3L)
  expect_identical(origin$values, list(hidden = 7L, visible = 3L))
  expect_identical(shadow$values, list(visible = 3L))

  collection = ParamSetCollection$new(list(view = shadow))
  collection$values = list(view.visible = 4L)
  expect_identical(origin$values, list(hidden = 7L, visible = 4L))

  shadow$values = list()
  expect_identical(origin$values, list(hidden = 7L))
})

test_that("shadow validation preserves a nested origin commit", {
  holder = new.env(parent = emptyenv())
  armed = FALSE
  nested = FALSE
  origin = ps(
    hidden = p_int(),
    visible = p_uty(custom_check = function(value) {
      if (armed && !nested) {
        nested <<- TRUE
        holder$origin$assert_values = FALSE
        holder$origin$values = list(hidden = 9L)
        holder$origin$assert_values = TRUE
      }
      TRUE
    })
  )
  holder$origin = origin
  origin$values = list(hidden = 1L)
  shadow = ParamSetShadow$new(origin, "hidden")
  armed = TRUE

  expect_error({
    shadow$values = list(visible = "outer")
  }, "changed")
  expect_identical(origin$values, list(hidden = 9L))
  expect_identical(shadow$values, setNames(list(), character()))
})

test_that("ObjectTuneToken receipts stay rooted across later validation work", {
  skip_on_cran()

  # The receipt set is retained for the whole checked assignment and handed to
  # the caller afterwards, so it must be the object left on the protection
  # stack. Torturing the collector while a constraint and many ordinary values
  # allocate exposes a receipt set that is only reachable through a C local.
  make_set = function() {
    domains = c(
      list(candidate = p_dbl(0, 1)),
      set_names(
        lapply(seq_len(30L), function(index) p_dbl(0, 1)),
        paste0("v", seq_len(30L))
      )
    )
    set = do.call(ps, domains)
    set$constraint = function(x) {
      invisible(vapply(seq_len(40L), function(i) sum(runif(20L)), numeric(1)))
      TRUE
    }
    set
  }

  for (round in seq_len(8L)) {
    set = make_set()
    values = c(
      list(candidate = to_tune(ps(z = p_dbl(0, 1)))),
      set_names(as.list(runif(30L)), paste0("v", seq_len(30L)))
    )

    previous = gctorture2(11L)
    on.exit(gctorture2(previous), add = TRUE)
    set$values = values
    gctorture2(previous)

    expect_class(set$values$candidate, "ObjectTuneToken")
    expect_identical(set$search_space()$ids(), "z")
  }
})
