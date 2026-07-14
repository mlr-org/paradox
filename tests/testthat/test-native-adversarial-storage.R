native_adversarial_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"))
}

native_adversarial_private_params = function(param_set) {
  param_set$.__enclos_env__$private$.params
}

native_adversarial_plain_copy = function(value) {
  object_attributes = attributes(value)
  result = unclass(value)
  attributes(result) = object_attributes
  result
}

native_adversarial_replace = function(value, name, replacement) {
  object_attributes = attributes(value)
  result = unclass(value)
  result[[name]] = replacement
  attributes(result) = object_attributes
  result
}

test_that("native table access rejects corrupt ParamSet storage safely", {
  param_set = ps(x = p_dbl(0, 1), choice = p_fct(c("a", "b")))
  params = native_adversarial_private_params(param_set)
  property = native_adversarial_symbol("param_set_property")
  ids = native_adversarial_symbol("param_set_ids")

  expect_error(
    .Call(property, 1L, 0L),
    "`.params` must be a list",
    fixed = TRUE
  )

  unnamed = native_adversarial_plain_copy(params)
  names(unnamed) = NULL
  expect_error(
    .Call(property, unnamed, 0L),
    "`.params` must be a named list",
    fixed = TRUE
  )

  wrong_id = native_adversarial_replace(params, "id", seq_along(params$id))
  expect_error(.Call(property, wrong_id, 0L), "`id` must have type `character`")

  duplicate_id = c(params, list(id = params$id))
  expect_error(
    .Call(property, duplicate_id, 0L),
    "has more than one `id` column",
    fixed = TRUE
  )
  duplicate_levels = c(params, list(levels = params$levels))
  expect_error(
    .Call(property, duplicate_levels, 0L),
    "has more than one `levels` column",
    fixed = TRUE
  )

  short_levels = native_adversarial_replace(
    params,
    "levels",
    params$levels[-1L]
  )
  expect_error(.Call(property, short_levels, 0L), "`levels` must have type `list`")

  wrong_bounds = native_adversarial_replace(
    params,
    "lower",
    as.list(params$lower)
  )
  expect_error(.Call(property, wrong_bounds, 3L), "`lower` must be numeric")

  expect_error(.Call(property, params, -1L), "invalid ParamSet property selector")
  expect_error(.Call(property, params, 4L), "invalid ParamSet property selector")
  expect_error(.Call(property, params, NA_integer_), "invalid ParamSet property selector")

  expect_error(
    .Call(ids, params, param_set$.__enclos_env__$private$.tags, NA_character_, NULL, NULL),
    "Contains missing values"
  )
  expect_error(
    .Call(ids, params, param_set$.__enclos_env__$private$.tags, 1L, NULL, NULL),
    "Must be of type 'character'"
  )

  malformed_tags = list(id = "x")
  expect_error(
    .Call(ids, params, malformed_tags, NULL, "tag", NULL),
    "has no `tag` column"
  )
  unknown_owner = list(id = "not-a-parameter", tag = "tag")
  expect_error(
    .Call(ids, params, unknown_owner, NULL, "tag", NULL),
    "unknown parameter ID"
  )
})

test_that("native Domain kernels validate recognized storage before reading it", {
  check = native_adversarial_symbol("domain_check_builtin")
  qunif = native_adversarial_symbol("domain_qunif_builtin")
  sanitize = native_adversarial_symbol("domain_sanitize_builtin")

  domain = p_dbl(0, 1)
  unnamed = native_adversarial_plain_copy(domain)
  names(unnamed) = NULL
  expect_error(
    .Call(check, unnamed, list(0.5)),
    "`Domain` must be a named list",
    fixed = TRUE
  )

  wrong_grouping = native_adversarial_replace(domain, "grouping", 1L)
  expect_error(.Call(check, wrong_grouping, list(0.5)), "`grouping` must have type `character`")

  wrong_lower = native_adversarial_replace(domain, "lower", list(0))
  expect_error(.Call(check, wrong_lower, list(0.5)), "`lower` must be numeric")
  expect_error(.Call(qunif, wrong_lower, 0.5), "`lower` must be numeric")
  expect_error(.Call(sanitize, wrong_lower, list(0.5)), "`lower` must be numeric")

  invalid_tolerance = native_adversarial_replace(domain, "tolerance", -1)
  expect_error(
    .Call(check, invalid_tolerance, list(0.5)),
    "invalid numeric bounds or tolerance"
  )
  inverted_bounds = native_adversarial_replace(domain, "lower", 2)
  expect_error(
    .Call(qunif, inverted_bounds, 0.5),
    "invalid numeric bounds",
    fixed = TRUE
  )
  expect_error(
    .Call(sanitize, inverted_bounds, list(0.5)),
    "invalid numeric bounds",
    fixed = TRUE
  )

  factor = native_adversarial_replace(
    p_fct(c("a", "b")),
    "levels",
    list(new.env(parent = emptyenv()))
  )
  expect_error(
    .Call(check, factor, list("a")),
    "each `levels` element must be character"
  )
  expect_error(
    .Call(qunif, factor, 0.5),
    "each `levels` element must be character"
  )

  missing_factor = native_adversarial_replace(
    p_fct(c("a", "b")),
    "levels",
    list(c("a", NA_character_))
  )
  expect_error(
    .Call(check, missing_factor, list("a")),
    "`levels` may not contain missing values",
    fixed = TRUE
  )
  expect_error(
    .Call(qunif, missing_factor, 0.5),
    "`levels` may not contain missing values",
    fixed = TRUE
  )

  # An unrecognized class is an extension/fallback request, not corrupt
  # built-in storage. It must return the documented sentinel without touching
  # arbitrary fields.
  unknown = structure(list(bad = new.env()), class = "ParamThirdParty")
  expect_identical(.Call(check, unknown, list(1)), FALSE)
  expect_null(.Call(qunif, unknown, 0.5))
  expect_null(.Call(sanitize, unknown, list(1)))
})

test_that("native construction gates fail closed on malformed shapes", {
  construct_param_set = native_adversarial_symbol("param_set_construct")
  construct_domain = native_adversarial_symbol("domain_construct")

  expect_null(.Call(construct_param_set, 1L))
  expect_null(.Call(construct_param_set, list(p_dbl(0, 1))))
  expect_null(.Call(construct_param_set, setNames(list(list()), "x")))

  bad_domain = native_adversarial_replace(p_dbl(0, 1), "lower", numeric())
  expect_null(.Call(construct_param_set, list(x = bad_domain)))

  valid = p_dbl(0, 1)
  call_domain = function(
      cls = valid$cls,
      grouping = valid$grouping,
      cargo = valid$cargo[[1L]],
      lower = valid$lower,
      upper = valid$upper,
      tolerance = valid$tolerance,
      levels = valid$levels[[1L]],
      special_vals = valid$special_vals[[1L]],
      default = valid$default[[1L]],
      tags = valid$.tags[[1L]],
      trafo = valid$.trafo[[1L]],
      storage_type = valid$storage_type,
      init_given = valid$.init_given,
      init = valid$.init[[1L]]) {
    .Call(
      construct_domain, cls, grouping, cargo, lower, upper, tolerance,
      levels, special_vals, default, tags, trafo, storage_type, init_given,
      init
    )
  }

  expect_type(call_domain(), "list")
  expect_null(call_domain(cls = "ParamUnknown"))
  expect_null(call_domain(grouping = character()))
  expect_null(call_domain(lower = numeric()))
  expect_null(call_domain(tags = c("x", "x")))
  expect_null(call_domain(trafo = 1L))
  expect_null(call_domain(init_given = NA))
})

test_that("native ParamSet checking gates return fallback sentinels safely", {
  scalar = native_adversarial_symbol("param_set_check_builtin")
  table = native_adversarial_symbol("param_set_check_dt_builtin")
  param_set = ps(x = p_dbl(0, 1))
  params = native_adversarial_private_params(param_set)

  expect_null(.Call(scalar, 1L, list(x = 0.5), FALSE))
  expect_null(.Call(scalar, params, list(x = 0.5), NA))
  expect_null(.Call(scalar, params, structure(list(0.5), names = NULL), FALSE))
  expect_null(.Call(scalar, params, list(unknown = 0.5), FALSE))

  malformed = native_adversarial_replace(
    params,
    "storage_type",
    logical(length(params$storage_type))
  )
  expect_null(.Call(scalar, malformed, list(x = 0.5), FALSE))

  expect_null(.Call(table, params, list(x = 0.5)))
  expect_null(.Call(table, params, data.frame(unknown = 0.5)))
  expect_null(.Call(table, malformed, data.frame(x = 0.5)))
  duplicate_id = c(params, list(id = params$id))
  expect_null(.Call(scalar, duplicate_id, list(x = 0.5), FALSE))
  expect_null(.Call(table, duplicate_id, data.frame(x = 0.5)))
})
