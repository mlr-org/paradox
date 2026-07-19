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

native_subset_token = function(param_set, ids,
    allow_dangling_dependencies = FALSE, keep_constraint = TRUE) {
  symbols = native_subset_symbols()
  .Call(
    symbols$C_param_set_subset_state,
    param_set$.__enclos_env__$private,
    param_set,
    ids,
    allow_dangling_dependencies,
    keep_constraint,
    param_set$constraint,
    param_set$extra_trafo
  )
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
  expect_identical(symbols$C_param_set_subset_state$numParameters, 7L)
  expect_identical(symbols$C_param_set_subspace_states$numParameters, 4L)
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
    observed$.__enclos_env__$private
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
  expect_error(
    param_set$subset("a", allow_dangling_dependencies = NA),
    "allow_dangling_dependencies"
  )

  empty = param_set$subset(character(), keep_constraint = FALSE)
  expect_true(empty$is_empty)
  expect_identical(empty$values, named_list())
  expect_equal(nrow(empty$deps), 0L)
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
