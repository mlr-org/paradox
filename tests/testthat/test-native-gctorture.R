test_that("every allocating native entry point survives forced collection", {
  namespace = asNamespace("paradox")
  symbols = mget(
    paste0("C_", c(
      "design_transpose",
      "domain_construct",
      "domain_construct_frame",
      "domain_check_builtin",
      "domain_qunif_builtin",
      "domain_sanitize_builtin",
      "param_set_construct",
      "param_set_ids",
      "param_set_property",
      "param_set_check_builtin",
      "param_set_check_dt_builtin",
      "param_set_qunif_builtin",
      "param_set_get_domain",
      "param_set_domains",
      "param_set_params",
      "param_set_collection_params",
      "param_set_collection_deps",
      "param_set_subset_state",
      "param_set_adopt_subset_state"
    )),
    envir = namespace,
    inherits = FALSE
  )

  domains = list(
    double = p_dbl(-1, 1, tolerance = 1e-6),
    integer = p_int(-2, 2),
    factor = p_fct(c("slow", "fast")),
    logical = p_lgl()
  )
  parameter_set = ParamSet$new(domains)
  parameter_set$add_dep("logical", "integer", CondEqual(1L))
  private = parameter_set$.__enclos_env__$private
  collection = ParamSetCollection$new(list(inner = parameter_set))
  collection_private = collection$.__enclos_env__$private
  params = private$.params
  tags = private$.tags
  scalar_values = list(
    double = -1 - 5e-7,
    integer = 1,
    factor = "fast",
    logical = TRUE
  )
  tabular_values = data.frame(
    double = c(-0.5, 0.5),
    integer = c(-1L, 1L),
    factor = c("slow", "fast"),
    logical = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  transpose_values = data.table::as.data.table(tabular_values)
  units = matrix(
    c(0, 1, 0.25, 0.75, 0.5, 0.5, 1, 0),
    nrow = 2L,
    dimnames = list(NULL, names(domains))
  )
  subset_private = new.env(parent = emptyenv())
  subset_private$.params = NULL
  subset_private$.tags = NULL
  subset_private$.trafos = NULL
  subset_private$.deps = NULL
  subset_private$.values = NULL
  collection_subset_private = new.env(parent = emptyenv())
  collection_subset_private$.params = NULL
  collection_subset_private$.tags = NULL
  collection_subset_private$.trafos = NULL
  collection_subset_private$.deps = NULL
  collection_subset_private$.values = NULL

  construct_domain_frame = function(
      cls = "ParamLgl",
      grouping = "ParamLgl",
      cargo = list(),
      lower = NA_real_,
      upper = NA_real_,
      tolerance = NA_real_,
      levels = c(TRUE, FALSE),
      special_vals = list(),
      default = stop("default was forced", call. = FALSE),
      tags = character(),
      trafo = NULL,
      depends_expr = stop("depends_expr was forced", call. = FALSE),
      storage_type = stop("storage_type was forced", call. = FALSE),
      init = stop("init was forced", call. = FALSE)) {
    .Call(symbols$C_domain_construct_frame, environment())
  }

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)

  constructed_domain = .Call(
    symbols$C_domain_construct,
    "ParamDbl",
    "ParamDbl",
    NULL,
    0,
    1,
    0,
    NULL,
    list(),
    paradox:::NO_DEF,
    character(),
    NULL,
    "numeric",
    FALSE,
    NULL
  )
  constructed_domain_plan = construct_domain_frame()
  checked_domain = .Call(
    symbols$C_domain_check_builtin,
    domains$double,
    list(0.25)
  )
  mapped_domain = .Call(
    symbols$C_domain_qunif_builtin,
    domains$double,
    c(0, 0.5, 1)
  )
  sanitized_domain = .Call(
    symbols$C_domain_sanitize_builtin,
    domains$double,
    list(-1 - 5e-7, 1 + 5e-7)
  )
  constructed_set = .Call(symbols$C_param_set_construct, domains)
  selected_ids = .Call(
    symbols$C_param_set_ids,
    params,
    tags,
    "ParamDbl",
    NULL,
    NULL
  )
  properties = .Call(symbols$C_param_set_property, params, 0L)
  checked_values = .Call(
    symbols$C_param_set_check_builtin,
    params,
    scalar_values,
    TRUE
  )
  checked_table = .Call(
    symbols$C_param_set_check_dt_builtin,
    params,
    tabular_values
  )
  mapped_set = .Call(symbols$C_param_set_qunif_builtin, params, units)
  recovered_domain = .Call(
    symbols$C_param_set_get_domain,
    private,
    parameter_set,
    "double"
  )
  recovered_domains = .Call(
    symbols$C_param_set_domains,
    private,
    parameter_set
  )
  recovered_params = .Call(
    symbols$C_param_set_params,
    private,
    parameter_set
  )
  recovered_collection_params = .Call(
    symbols$C_param_set_collection_params,
    collection_private,
    collection
  )
  recovered_collection_deps = .Call(
    symbols$C_param_set_collection_deps,
    collection_private,
    collection
  )
  subset_plan = .Call(
    symbols$C_param_set_subset_state,
    private,
    parameter_set,
    c("factor", "double"),
    FALSE
  )
  adopted_subset_token = .Call(
    symbols$C_param_set_adopt_subset_state,
    subset_private,
    subset_plan$state
  )
  consumed_subset_token = .Call(
    symbols$C_param_set_adopt_subset_state,
    subset_private,
    subset_plan$state
  )
  collection_subset_plan = .Call(
    symbols$C_param_set_subset_state,
    collection_private,
    collection,
    c("inner.factor", "inner.double"),
    FALSE
  )
  adopted_collection_subset_token = .Call(
    symbols$C_param_set_adopt_subset_state,
    collection_subset_private,
    collection_subset_plan$state
  )
  transposed = .Call(symbols$C_design_transpose, transpose_values, TRUE)

  gctorture(previous)

  expect_identical(names(constructed_domain), paradox:::domain_names)
  expect_identical(constructed_domain_plan[[2L]], "logical")
  expect_identical(
    names(constructed_domain_plan[[1L]]),
    paradox:::domain_names
  )
  expect_true(checked_domain)
  expect_identical(mapped_domain, c(-1, 0, 1))
  expect_identical(sanitized_domain, list(-1, 1))
  expect_named(
    constructed_set,
    c("params", "tags", "trafos", "requirements", "init_values")
  )
  expect_identical(selected_ids, "double")
  expect_length(properties, 2L)
  expect_true(checked_values)
  expect_named(attr(checked_values, "sanitized"), names(scalar_values))
  expect_true(checked_table)
  expect_s3_class(mapped_set, "data.table")
  expect_identical(names(mapped_set), names(domains))
  expect_s3_class(recovered_domain, "Domain")
  expect_identical(recovered_domain$id, "double")
  expect_identical(data.table:::selfrefok(recovered_domain, FALSE), 1L)
  expect_named(recovered_domains, names(domains))
  expect_true(all(vapply(
    recovered_domains,
    function(domain) data.table:::selfrefok(domain, FALSE) == 1L,
    logical(1L)
  )))
  expect_s3_class(recovered_params, "data.table")
  expect_identical(recovered_params$id, names(domains))
  expect_identical(data.table:::selfrefok(recovered_params, FALSE), 1L)
  expect_s3_class(recovered_collection_params, "data.table")
  expect_identical(
    recovered_collection_params$id,
    sprintf("inner.%s", names(domains))
  )
  expect_identical(
    data.table:::selfrefok(recovered_collection_params, FALSE),
    1L
  )
  expect_s3_class(recovered_collection_deps, "data.table")
  expect_identical(recovered_collection_deps$id, "inner.logical")
  expect_identical(recovered_collection_deps$on, "inner.integer")
  expect_identical(
    data.table:::selfrefok(recovered_collection_deps, FALSE),
    1L
  )
  expect_true(adopted_subset_token)
  expect_identical(subset_private$.params$id, c("factor", "double"))
  expect_false(consumed_subset_token)
  expect_true(adopted_collection_subset_token)
  expect_identical(
    collection_subset_private$.params$id,
    c("inner.factor", "inner.double")
  )
  expect_identical(transposed[[1L]]$double, -0.5)
})
