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

  # Selectors 0-3 are vector properties, 4-10 detached schema projections,
  # 11-13 scalar reductions, and 14-15 dimension queries.
  expect_error(.Call(property, params, -1L), "invalid ParamSet property selector")
  expect_error(.Call(property, params, 100L), "invalid ParamSet property selector")
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

  expect_error(
    .Call(construct_param_set, 1L, FALSE),
    "ordinary named list"
  )
  expect_error(
    .Call(construct_param_set, list(p_dbl(0, 1)), FALSE),
    "ordinary character names"
  )
  expect_error(
    .Call(
      construct_param_set,
      setNames(list(list()), "x"),
      FALSE
    ),
    "canonical built-in Domain"
  )

  bad_domain = native_adversarial_replace(p_dbl(0, 1), "lower", numeric())
  expect_error(
    .Call(construct_param_set, list(x = bad_domain), FALSE),
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

# One helper for hand-built Domain column-name corruption: unclass, rewrite the
# names cell, and reinstall the original attribute spine so the result still
# presents to native code as the public Domain it claims to be.
native_adversarial_rename = function(domain, from, to) {
  spine = attributes(domain)
  result = unclass(domain)
  spine$names[match(from, spine$names)] = to
  attributes(result) = spine
  result
}

test_that("public Domain operations reject duplicate and missing columns", {
  operations = list(
    check = function(d) domain_check(d, list(0.5)),
    qunif = function(d) domain_qunif(d, 0.5),
    sanitize = function(d) domain_sanitize(d, list(0.5)),
    nlevels = function(d) domain_nlevels(d)
  )
  # Spelling `lower` as `upper` leaves `lower` absent and `upper` duplicated;
  # the selector reports in ascending canonical column order.
  duplicated_name = native_adversarial_rename(p_dbl(0, 1), "lower", "upper")
  missing_name = native_adversarial_rename(p_dbl(0, 1), "upper", "not_a_column")
  for (name in names(operations)) {
    expect_error(
      operations[[name]](duplicated_name),
      "Corrupt Domain storage: `Domain` has no `lower` column",
      fixed = TRUE,
      info = name
    )
    expect_error(
      operations[[name]](missing_name),
      "Corrupt Domain storage: `Domain` has no `upper` column",
      fixed = TRUE,
      info = name
    )
  }

  # A seventeenth column duplicating a canonical name leaves every name
  # present and is still rejected.
  seventeen = unclass(p_dbl(0, 1))
  spine = attributes(p_dbl(0, 1))
  seventeen = c(seventeen, list(id = seventeen$id))
  spine$names = c(spine$names, "id")
  attributes(seventeen) = spine
  expect_error(
    domain_check(seventeen, list(0.5)),
    "Corrupt Domain storage: `Domain` has more than one `id` column",
    fixed = TRUE
  )
})

test_that("canonical Domain column names are decided by their bytes", {
  # R interns one CHARSXP per exact string, and re-marking the encoding of a
  # pure-ASCII canonical name yields that same interned object: the selector's
  # byte fallback therefore accepts every spelling whose bytes match a
  # canonical name, and encoding marking never changes column identity.
  for (encoding in c("UTF-8", "latin1", "bytes")) {
    for (column in c("id", "cls", "lower", "levels")) {
      domain = p_dbl(0, 1)
      names_cell = names(domain)
      spelling = names_cell[match(column, names_cell)]
      Encoding(spelling) = encoding
      names_cell[match(column, names_cell)] = spelling
      data.table::setattr(domain, "names", names_cell)
      expect_true(
        domain_check(domain, list(0.5)),
        info = paste(encoding, column)
      )
      expect_identical(
        domain_qunif(domain, 0.5),
        0.5,
        info = paste(encoding, column)
      )
    }
  }

  # A name whose bytes differ is a different column, whatever it translates
  # from: the missing canonical column is reported, never silently matched.
  domain = p_dbl(0, 1)
  names_cell = names(domain)
  foreign = enc2utf8("löwer")
  Encoding(foreign) = "UTF-8"
  names_cell[match("lower", names_cell)] = foreign
  data.table::setattr(domain, "names", names_cell)
  expect_error(
    domain_check(domain, list(0.5)),
    "Corrupt Domain storage: `Domain` has no `lower` column",
    fixed = TRUE
  )
})

test_that("in-place column mutation from a phase hook fails admission", {
  reentry = native_adversarial_symbol("test_domain_admission_reentry")
  param_fct_kind = 3L
  interpret_all = 63L
  build = function() {
    paradox:::recover_domain(data.table::rbindlist(
      list(p_fct(c("a", "b")), p_fct(c("a", "b"))),
      use.names = TRUE,
      fill = TRUE
    ))
  }
  # `data.table::set()` writes through the already selected column, so the
  # outward table and column spine are unchanged and only the terminal
  # byte/pointer receipt can observe the write.
  for (phase in c("capture", "ownership")) {
    domain = build()
    mutate = function() {
      data.table::set(domain, j = "cls", value = c("ParamFct", "ParamFct"))
    }
    hooks = if (identical(phase, "capture")) mutate else list(NULL, mutate)
    expect_error(
      .Call(reentry, domain, param_fct_kind, interpret_all, hooks),
      "Domain changed during admission",
      fixed = TRUE,
      info = phase
    )
  }
})

test_that("data.table cache attributes name their cause and remedy", {
  # An ordinary `i` filter installs data.table's auto-index on the Domain by
  # reference and a key installs `sorted`. Both leave a table no operation may
  # admit, so the rejection names the attribute and how to clear it.
  indexed = p_dbl(0, 1)
  invisible(indexed[cls == "ParamDbl"])
  if (!is.null(attr(indexed, "index", exact = TRUE))) {
    expect_error(
      domain_check(indexed, list(0.5)),
      paste0(
        "Corrupt Domain storage: `Domain` carries the data.table `index` ",
        "cache attribute; remove it with ",
        "`data.table::setattr(x, \"index\", NULL)` or rebuild the Domain"
      ),
      fixed = TRUE
    )
    data.table::setattr(indexed, "index", NULL)
    expect_true(domain_check(indexed, list(0.5)))
  }

  keyed = p_dbl(0, 1)
  data.table::setattr(keyed, "sorted", "id")
  for (operation in list(
    function(d) domain_check(d, list(0.5)),
    function(d) domain_qunif(d, 0.5),
    function(d) domain_nlevels(d)
  )) {
    expect_error(
      operation(keyed),
      paste0(
        "Corrupt Domain storage: `Domain` carries the data.table `sorted` ",
        "cache attribute; remove it with ",
        "`data.table::setattr(x, \"sorted\", NULL)` or rebuild the Domain"
      ),
      fixed = TRUE
    )
  }
  data.table::setattr(keyed, "sorted", NULL)
  expect_true(domain_check(keyed, list(0.5)))

  # An unsupported attribute the package does not know keeps the general
  # fail-closed rejection.
  foreign = p_dbl(0, 1)
  data.table::setattr(foreign, "unexpected", TRUE)
  expect_error(
    domain_check(foreign, list(0.5)),
    "Corrupt Domain storage: outer metadata must be ordinary and bounded",
    fixed = TRUE
  )
})
