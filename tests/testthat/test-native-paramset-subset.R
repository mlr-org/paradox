native_subset_symbols = function() {
  namespace = asNamespace("paradox")
  mget(
    paste0("C_", c(
      "param_set_subset_state",
      "param_set_subspace_states",
      "param_set_adopt_subset_state"
    )),
    envir = namespace,
    inherits = FALSE
  )
}

native_subset_bundle = function(param_set, ids,
    allow_dangling_dependencies = FALSE, keep_constraint = TRUE,
    keep_trafo = TRUE) {
  symbols = native_subset_symbols()
  .Call(
    symbols$C_param_set_subset_state,
    param_set$.__enclos_env__$private,
    param_set,
    ids,
    allow_dangling_dependencies,
    keep_constraint,
    keep_trafo
  )
}

native_subset_token = function(param_set, ids,
    allow_dangling_dependencies = FALSE, keep_constraint = TRUE,
    keep_trafo = TRUE) {
  native_subset_bundle(
    param_set,
    ids,
    allow_dangling_dependencies,
    keep_constraint,
    keep_trafo
  )$token
}

native_subset_rich_set = function() {
  result = ps(
    z = p_dbl(-2, 2, tags = c("numeric", "shared"), trafo = exp),
    a = p_int(-3, 3, tags = c("control", "shared")),
    mode = p_fct(c("small", "large"), tags = "choice"),
    flag = p_lgl()
  )
  result$values = list(z = 0.5, a = 2L, mode = "large", flag = TRUE)
  result$add_dep("mode", "a", CondAnyOf(c(-1L, 2L)))
  result$add_dep("z", "a", CondEqual(2L))
  result$constraint = function(x) TRUE
  result$extra_trafo = function(x, param_set) x
  result
}

test_that("subset capsule routines are the complete registered surface", {
  symbols = native_subset_symbols()
  expect_identical(symbols$C_param_set_subset_state$numParameters, 6L)
  expect_identical(symbols$C_param_set_subspace_states$numParameters, 3L)
  expect_identical(symbols$C_param_set_adopt_subset_state$numParameters, 2L)
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])

  namespace = asNamespace("paradox")
  retired = c(
    "C_param_set_subspace_state",
    "C_param_set_bulk_shell_register",
    "C_param_set_bulk_generator_auth",
    "C_param_set_bulk_shells",
    "C_sampler_1d_unif_bulk_register",
    "C_sampler_1d_unif_bulk_auth",
    "C_sampler_1d_unif_bulk_shells"
  )
  expect_false(any(vapply(
    retired,
    exists,
    logical(1L),
    envir = namespace,
    inherits = FALSE
  )))
})

test_that("keep_trafo is an additive final argument with a compatible default", {
  expect_identical(formals(ParamSet$public_methods$subset)$keep_trafo, TRUE)
  expect_identical(
    formals(ParamSetCollection$public_methods$subset)$keep_trafo,
    TRUE
  )
  expect_identical(
    formals(ParamSetShadow$public_methods$subset)$keep_trafo,
    TRUE
  )
  for (generator in list(ParamSet, ParamSetCollection, ParamSetShadow)) {
    expect_identical(
      tail(names(formals(generator$public_methods$subset)), 1L),
      "keep_trafo"
    )
  }
})

test_that("subset slices one canonical snapshot in requested order", {
  param_set = native_subset_rich_set()
  observed = param_set$subset(c("mode", "a", "z"))

  expect_s3_class(observed, "ParamSet")
  expect_identical(class(observed), c("ParamSet", "R6"))
  expect_identical(observed$ids(), c("mode", "a", "z"))
  expect_identical(
    observed$values,
    list(mode = "large", a = 2L, z = 0.5)
  )
  expect_identical(observed$tags, list(
    mode = "choice",
    a = c("control", "shared"),
    z = c("numeric", "shared")
  ))
  expect_identical(observed$deps$id, c("mode", "z"))
  expect_identical(observed$deps$on, c("a", "a"))
  expect_identical(
    lapply(observed$deps$cond, `[[`, "rhs"),
    list(c(-1L, 2L), 2L)
  )
  expect_true(observed$has_trafo_param[["z"]])
  expect_identical(observed$constraint, param_set$constraint)
  expect_identical(observed$extra_trafo, param_set$extra_trafo)

  state = .Call(
    get("C_param_set_core_state", envir = asNamespace("paradox")),
    observed$.__enclos_env__$private,
    observed
  )
  for (field in c(".params", ".tags", ".trafos", ".deps")) {
    expect_identical(class(state[[field]]), "data.frame")
    expect_false(inherits(state[[field]], "data.table"))
  }
  expect_s3_class(observed$params, "data.table")
  expect_s3_class(observed$deps, "data.table")

  param_set$values = list(a = 1L, flag = FALSE)
  param_set$constraint = function(x) FALSE
  param_set$extra_trafo = NULL
  expect_identical(
    observed$values,
    list(mode = "large", a = 2L, z = 0.5)
  )
  expect_true(observed$constraint(list()))
  expect_true(is.function(observed$extra_trafo))
})

test_that("subset dependency and option contracts are explicit", {
  param_set = native_subset_rich_set()
  expect_error(
    param_set$subset(c("mode", "z")),
    "Subsetting so that dependencies"
  )

  dangling = param_set$subset(
    c("mode", "z"),
    allow_dangling_dependencies = TRUE,
    keep_constraint = FALSE
  )
  expect_identical(dangling$deps$id, c("mode", "z"))
  expect_identical(dangling$deps$on, c("a", "a"))
  expect_null(dangling$constraint)
  expect_identical(dangling$extra_trafo, param_set$extra_trafo)

  expect_error(param_set$subset(c("a", "a")), "must not contain duplicates")
  expect_error(param_set$subset("absent"), "unknown parameter 'absent'")
  expect_error(param_set$subset(NA_character_), "must not contain missing")
  expect_error(param_set$subset("a", keep_constraint = NA), "keep_constraint")
  expect_error(param_set$subset("a", keep_trafo = NA), "keep_trafo")
  expect_error(param_set$subset("a", keep_trafo = 1), "keep_trafo")
  expect_error(
    param_set$subset("a", allow_dangling_dependencies = NA),
    "allow_dangling_dependencies"
  )

  empty = param_set$subset(character(), keep_constraint = FALSE)
  expect_true(empty$is_empty)
  expect_identical(empty$values, named_list())
  expect_equal(nrow(empty$deps), 0L)
})

test_that("subset flags cannot dispatch or carry semantic attributes", {
  dispatched = 0L
  `!.hostile_subset_flag` = function(x) {
    dispatched <<- dispatched + 1L
    stop("hostile subset dispatch")
  }
  hostile = structure(TRUE, class = "hostile_subset_flag")
  named = structure(TRUE, names = "flag")
  base = ps(x = p_dbl(trafo = exp), y = p_lgl())
  child = ps(x = p_dbl(trafo = exp), y = p_lgl())
  child$extra_trafo = function(x, param_set) x
  collection = ParamSetCollection$new(list(owner = child))

  for (set in list(base, collection)) {
    expect_error(set$subset(set$ids(), keep_trafo = hostile), "keep_trafo")
    expect_error(
      set$subset(set$ids(), keep_constraint = hostile),
      "keep_constraint"
    )
    expect_error(
      set$subset(set$ids(), allow_dangling_dependencies = hostile),
      "allow_dangling_dependencies"
    )
    expect_error(set$subset(set$ids(), keep_trafo = named), "keep_trafo")
  }
  expect_identical(dispatched, 0L)
})

test_that("subset retains its exact BASE generation across reentry", {
  param_set = ps(x = p_dbl(0, 1), y = p_int())
  param_set$values = list(x = 0.25)
  symbol = get(
    "C_test_param_set_subset_reentry",
    envir = asNamespace("paradox")
  )
  expect_identical(symbol$numParameters, 7L)

  bundle = .Call(
    symbol,
    param_set$.__enclos_env__$private,
    param_set,
    "x",
    FALSE,
    TRUE,
    TRUE,
    function() param_set$values = list(x = 0.75)
  )
  snapshot = ParamSet$new(bundle$token)
  expect_identical(snapshot$values, list(x = 0.25))
  expect_identical(param_set$values, list(x = 0.75))
})

test_that("nested Collection-Shadow subsets retain generations across finalizers", {
  origin = ps(x = p_dbl(0, 1), hidden = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  collection = psc(layer = shadow)
  symbol = get(
    "C_test_param_set_subset_reentry",
    envir = asNamespace("paradox")
  )
  fired = new.env(parent = emptyenv())
  fired$value = FALSE

  hook = function() {
    victim = new.env(parent = emptyenv())
    reg.finalizer(victim, function(ignored) {
      fired$value = TRUE
      origin$values = list(x = 0.5)
    })
    rm(victim)
    invisible(gc(full = TRUE))
  }
  bundle = .Call(
    symbol,
    collection$.__enclos_env__$private,
    collection,
    "layer.x",
    FALSE,
    TRUE,
    TRUE,
    hook
  )
  snapshot = ParamSet$new(bundle$token)
  expect_true(fired$value)
  expect_identical(origin$values, list(x = 0.5))
  expect_identical(snapshot$values, named_list())
})

test_that("nested Shadow receipts detect in-place signature entry changes", {
  origin = ps(x = p_int(), hidden = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  collection = psc(layer = shadow)
  signature = attr(
    shadow$.__enclos_env__$private$.core,
    ".paradox.shadow.snapshot.v1",
    exact = TRUE
  )
  replacement = ps(other = p_int())$.__enclos_env__$private$.core
  subset_symbol = get(
    "C_test_param_set_subset_reentry",
    envir = asNamespace("paradox")
  )
  mutator_symbol = get(
    "C_test_gc_column_mutator",
    envir = asNamespace("paradox")
  )

  expect_error(
    .Call(
      subset_symbol,
      collection$.__enclos_env__$private,
      collection,
      "layer.x",
      FALSE,
      TRUE,
      TRUE,
      function() {
        pending = .Call(mutator_symbol, signature, 1L, replacement)
        rm(pending)
        invisible(gc(full = TRUE))
      }
    ),
    "changed while constructing a subset",
    fixed = TRUE
  )
  expect_identical(signature[[2L]], replacement)
})

test_that("subset can discard all transformation authority independently", {
  param_set = native_subset_rich_set()
  default = param_set$subset(c("z", "a"))
  explicit = param_set$subset(c("z", "a"), keep_trafo = TRUE)
  stripped = param_set$subset(c("z", "a"), keep_trafo = FALSE)

  expect_equal(default, explicit)
  expect_true(default$has_trafo_param[["z"]])
  expect_identical(default$extra_trafo, param_set$extra_trafo)
  expect_identical(stripped$has_trafo_param, c(z = FALSE, a = FALSE))
  expect_null(stripped$get_domain("z")$trafo)
  expect_null(stripped$extra_trafo)
  expect_identical(stripped$constraint, param_set$constraint)

  state = .Call(
    get("C_param_set_core_state", envir = asNamespace("paradox")),
    stripped$.__enclos_env__$private,
    stripped
  )
  expect_identical(state$.trafos$id, character())
  expect_null(state$.extra_trafo)

  unconstrained = param_set$subset(
    c("z", "a"), keep_constraint = FALSE, keep_trafo = FALSE
  )
  expect_null(unconstrained$constraint)
  expect_null(unconstrained$extra_trafo)
  expect_false(unconstrained$has_trafo_param[["z"]])
})

test_that("unknown subset IDs preserve text and reject bytes", {
  unknown_utf8 = enc2utf8("m\u00fcssing")
  unknown_latin1 = iconv(unknown_utf8, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(unknown_latin1),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(unknown_latin1) = "latin1"
  unknown_bytes = unknown_utf8
  Encoding(unknown_bytes) = "bytes"

  param_set = ps(a = p_int())
  error = tryCatch(param_set$subset(unknown_latin1), error = identity)
  expect_s3_class(error, "error")
  expect_identical(
    enc2utf8(conditionMessage(error)),
    "`ids` contains unknown parameter 'm\u00fcssing'"
  )
  expect_error(
    param_set$subset(unknown_bytes),
    "Unknown bytes-encoded parameter ID",
    fixed = TRUE
  )
})

test_that("corrupt subset ownership diagnostics retain offending IDs", {
  unknown_utf8 = enc2utf8("m\u00fcssing")
  unknown_latin1 = iconv(unknown_utf8, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(unknown_latin1),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(unknown_latin1) = "latin1"
  unknown_bytes = unknown_utf8
  Encoding(unknown_bytes) = "bytes"

  corrupt_latin1 = ps(a = p_int())
  paradox:::param_set_core_replace(
    corrupt_latin1$.__enclos_env__$private,
    values = setNames(list(1L), unknown_latin1)
  )
  error = tryCatch(corrupt_latin1$subset("a"), error = identity)
  expect_s3_class(error, "error")
  expect_identical(
    enc2utf8(conditionMessage(error)),
    "Corrupt ParamSet state: `values` refers to unknown parameter 'm\u00fcssing'"
  )

  corrupt_bytes = ps(a = p_int())
  paradox:::param_set_core_replace(
    corrupt_bytes$.__enclos_env__$private,
    values = setNames(list(1L), unknown_bytes)
  )
  expect_error(
    corrupt_bytes$subset("a"),
    paste0(
      "Corrupt ParamSet state: `values` refers to an ",
      "unknown bytes-encoded parameter ID"
    ),
    fixed = TRUE
  )
})

test_that("subspaces accept repeated IDs but create canonical singleton cores", {
  param_set = native_subset_rich_set()
  all_spaces = param_set$subspaces()
  expect_identical(names(all_spaces), param_set$ids())
  spaces = param_set$subspaces(c("z", "a", "z", "mode"))
  expect_identical(names(spaces), c("z", "a", "z", "mode"))
  expect_true(all(vapply(spaces, inherits, logical(1L), "ParamSet")))
  expect_true(all(vapply(spaces, function(x) x$length == 1L, logical(1L))))
  expect_true(all(vapply(spaces, function(x) nrow(x$deps) == 0L, logical(1L))))
  expect_identical(spaces[[1L]]$values, list(z = 0.5))
  expect_identical(spaces[[2L]]$values, list(a = 2L))
  expect_identical(spaces[[3L]]$values, list(z = 0.5))
  expect_identical(spaces[[4L]]$values, list(mode = "large"))
  expect_identical(spaces[[1L]]$extra_trafo, param_set$extra_trafo)
  expect_null(spaces[[1L]]$constraint)
  expect_identical(param_set$subspaces(character()), named_list())
  expect_error(param_set$subspaces("absent"), "unknown parameter 'absent'")
})

test_that("subset capabilities are single-use, unforgeable, and destination-safe", {
  symbols = native_subset_symbols()
  param_set = native_subset_rich_set()
  token = native_subset_token(param_set, c("a", "z"))

  expect_type(token, "externalptr")
  expect_true(.Call(symbols$C_param_set_adopt_subset_state, NULL, token))
  missing = new.env(parent = emptyenv())
  expect_false(.Call(
    symbols$C_param_set_adopt_subset_state,
    missing,
    token
  ))
  expect_true(.Call(symbols$C_param_set_adopt_subset_state, NULL, token))
  occupied = ParamSet$new()
  expect_false(.Call(
    symbols$C_param_set_adopt_subset_state,
    occupied$.__enclos_env__$private,
    token
  ))
  expect_identical(occupied$ids(), character())
  expect_true(.Call(symbols$C_param_set_adopt_subset_state, NULL, token))

  adopted = ParamSet$new(token)
  expect_identical(adopted$ids(), c("a", "z"))
  expect_false(.Call(symbols$C_param_set_adopt_subset_state, NULL, token))
  expect_error(ParamSet$new(token), "already consumed")

  forged = new("externalptr")
  expect_false(.Call(symbols$C_param_set_adopt_subset_state, NULL, forged))
  expect_error(ParamSet$new(forged), "Invalid")

  serial_token = native_subset_token(param_set, "a")
  restored = unserialize(serialize(serial_token, NULL))
  expect_false(.Call(symbols$C_param_set_adopt_subset_state, NULL, restored))
  expect_error(ParamSet$new(restored), "Invalid")
  expect_identical(ParamSet$new(serial_token)$ids(), "a")
})

test_that("collections and shadows subset through the same BASE engine", {
  left = ps(x = p_int(0, 10), flag = p_lgl())
  left$values = list(x = 4L, flag = TRUE)
  left$add_dep("x", "flag", CondEqual(TRUE))
  right = ps(y = p_dbl(0, 1, tags = "numeric"))
  collection = ParamSetCollection$new(list(left = left, right = right))

  flat = collection$subset(c("right.y", "left.flag", "left.x"))
  expect_identical(class(flat), c("ParamSet", "R6"))
  expect_identical(flat$ids(), c("right.y", "left.flag", "left.x"))
  expect_identical(flat$values, list(left.flag = TRUE, left.x = 4L))
  expect_identical(flat$deps$id, "left.x")
  expect_identical(flat$deps$on, "left.flag")

  collection_spaces = collection$subspaces(
    c("right.y", "left.x", "right.y")
  )
  expect_identical(
    names(collection_spaces),
    c("right.y", "left.x", "right.y")
  )

  origin = ps(hidden = p_int(), x = p_int(0, 10), flag = p_lgl())
  origin$values = list(hidden = 9L, x = 2L, flag = TRUE)
  origin$add_dep("x", "flag", CondEqual(TRUE))
  shadow = ParamSetShadow$new(origin, "hidden")
  shadow_subset = shadow$subset(c("flag", "x"))
  expect_identical(class(shadow_subset), c("ParamSet", "R6"))
  expect_identical(shadow_subset$values, list(flag = TRUE, x = 2L))
  expect_identical(shadow_subset$deps$id, "x")
  expect_identical(names(shadow$subspaces(c("x", "x"))), c("x", "x"))
})

test_that("collection and shadow subsets honor keep_trafo without losing constraints", {
  child = ps(
    x = p_dbl(0, 1, trafo = exp),
    flag = p_lgl()
  )
  child$extra_trafo = function(x, param_set) x
  child$constraint = function(x) TRUE
  collection = ParamSetCollection$new(list(owner = child))

  collection_subset = collection$subset(
    collection$ids(),
    keep_trafo = FALSE
  )
  expect_true(is.function(collection_subset$constraint))
  expect_null(collection_subset$extra_trafo)
  expect_true(all(!collection_subset$has_trafo_param))
  expect_null(collection_subset$get_domain("owner.x")$trafo)

  collection_callback_free = collection$subset(
    collection$ids(),
    keep_constraint = FALSE,
    keep_trafo = FALSE
  )
  expect_null(collection_callback_free$constraint)
  expect_null(collection_callback_free$extra_trafo)
  expect_true(all(!collection_callback_free$has_trafo_param))

  origin = ps(
    hidden = p_int(),
    x = p_dbl(0, 1, trafo = exp)
  )
  origin$extra_trafo = function(x, param_set) x
  origin$constraint = function(x) TRUE
  shadow = ParamSetShadow$new(origin, "hidden")
  shadow_subset = shadow$subset("x", keep_trafo = FALSE)

  expect_true(is.function(shadow_subset$constraint))
  expect_null(shadow_subset$extra_trafo)
  expect_false(shadow_subset$has_trafo_param[["x"]])
  expect_null(shadow_subset$get_domain("x")$trafo)
})
