native_adversarial_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"))
}

native_adversarial_private_params = function(param_set) {
  paradox:::param_set_core_state(
    param_set$.__enclos_env__$private
  )$.params
}

native_adversarial_state = function(param_set) {
  paradox:::param_set_core_state(param_set$.__enclos_env__$private)
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
    .Call(ids, params, native_adversarial_state(param_set)$.tags, NA_character_, NULL, NULL),
    "Contains missing values"
  )
  expect_error(
    .Call(ids, params, native_adversarial_state(param_set)$.tags, 1L, NULL, NULL),
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
    .Call(check, unnamed, list(0.5), FALSE),
    "`Domain` must be a named list",
    fixed = TRUE
  )

  wrong_grouping = native_adversarial_replace(domain, "grouping", 1L)
  expect_error(.Call(check, wrong_grouping, list(0.5), FALSE), "`grouping` must have type `character`")

  wrong_lower = native_adversarial_replace(domain, "lower", list(0))
  expect_error(.Call(check, wrong_lower, list(0.5), FALSE), "`lower` must be numeric")
  expect_error(.Call(qunif, wrong_lower, 0.5), "`lower` must be numeric")
  expect_error(.Call(sanitize, wrong_lower, list(0.5)), "`lower` must be numeric")

  invalid_tolerance = native_adversarial_replace(domain, "tolerance", -1)
  expect_error(
    .Call(check, invalid_tolerance, list(0.5), FALSE),
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
    .Call(check, factor, list("a"), FALSE),
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
    .Call(check, missing_factor, list("a"), FALSE),
    "`levels` may not contain missing values",
    fixed = TRUE
  )
  expect_error(
    .Call(qunif, missing_factor, 0.5),
    "`levels` may not contain missing values",
    fixed = TRUE
  )

  # Unknown classes are rejected without touching arbitrary fields. There is
  # no extension sentinel or R replay path.
  unknown = structure(list(bad = new.env()), class = "ParamThirdParty")
  expect_error(.Call(check, unknown, list(1), FALSE), "Unsupported Domain class")
  expect_error(.Call(qunif, unknown, 0.5), "Unsupported Domain class")
  expect_error(.Call(sanitize, unknown, list(1)), "Unsupported Domain class")
})

test_that("native construction gates fail closed on malformed shapes", {
  construct_param_set = native_adversarial_symbol("param_set_construct")
  construct_domain = native_adversarial_symbol("domain_construct")

  expect_error(.Call(construct_param_set, 1L), "ordinary named list")
  expect_error(
    .Call(construct_param_set, list(p_dbl(0, 1))),
    "ordinary character names"
  )
  expect_error(
    .Call(construct_param_set, setNames(list(list()), "x")),
    "canonical built-in Domain"
  )

  bad_domain = native_adversarial_replace(p_dbl(0, 1), "lower", numeric())
  expect_error(
    .Call(construct_param_set, list(x = bad_domain)),
    "noncanonical field.*lower/upper/tolerance"
  )

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
      init = valid$.init[[1L]],
      numeric_source_kind = 1L,
      numeric_logscale = FALSE,
      id = valid$id,
      requirements = valid$.requirements[[1L]]) {
    .Call(
      construct_domain, cls, grouping, cargo, lower, upper, tolerance,
      levels, special_vals, default, tags, trafo, storage_type, init_given,
      init, numeric_source_kind, numeric_logscale, id, requirements
    )
  }

  expect_type(call_domain(), "list")
  expect_error(
    call_domain(cls = "ParamUnknown"),
    "`cls` and `storage_type` must describe one canonical Domain",
    fixed = TRUE
  )
  expect_error(
    call_domain(grouping = character()),
    "`grouping` must be one non-missing string",
    fixed = TRUE
  )
  expect_error(
    call_domain(lower = numeric()),
    "`lower` must be one number",
    fixed = TRUE
  )
  expect_error(
    call_domain(tags = c("x", "x")),
    "`tags` must be an attribute-free character vector",
    fixed = TRUE
  )
  expect_error(
    call_domain(trafo = 1L),
    "`trafo` must be a function or NULL",
    fixed = TRUE
  )
  expect_error(
    call_domain(init_given = NA),
    "Internal error: invalid `init` admission flag",
    fixed = TRUE
  )
})

test_that("unified ParamSet checking rejects malformed calls without replay", {
  scalar = native_adversarial_symbol("param_set_check_builtin")
  table = native_adversarial_symbol("param_set_check_dt_builtin")
  param_set = ps(x = p_dbl(0, 1))
  private = param_set$.__enclos_env__$private

  expect_error(.Call(
    scalar, 1L, param_set, list(x = 0.5), TRUE, FALSE, "none", TRUE
  ))
  expect_error(.Call(
    scalar, private, param_set, list(x = 0.5), NA, FALSE, "none", TRUE
  ))
  unnamed_result = .Call(
    scalar, private, param_set, structure(list(0.5), names = NULL),
    TRUE, FALSE, "none", TRUE
  )
  non_table_result = .Call(
    table, private, param_set, list(x = 0.5), TRUE, "none", TRUE
  )
  expect_identical(unnamed_result, "Must be a named list")
  expect_identical(non_table_result, "Must be a data.frame or data.table")

  corrupt_private = new.env(parent = emptyenv())
  corrupt_private$.core = new("externalptr")
  expect_error(.Call(
    scalar, corrupt_private, param_set, list(x = 0.5),
    TRUE, FALSE, "none", TRUE
  ), "Corrupt ParamSet")
  expect_error(.Call(
    table, corrupt_private, param_set, data.frame(x = 0.5),
    TRUE, "none", TRUE
  ), "Corrupt ParamSet")

  scalar_result = .Call(
    scalar, private, param_set, list(unknown = 0.5),
    TRUE, FALSE, "none", TRUE
  )
  table_result = .Call(
    table, private, param_set, data.frame(unknown = 0.5),
    TRUE, "none", TRUE
  )
  expect_type(scalar_result, "character")
  expect_type(table_result, "character")
  expect_false(is.null(scalar_result))
  expect_false(is.null(table_result))
})
