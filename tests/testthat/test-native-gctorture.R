test_that("every allocating native entry point survives forced collection", {
  skip_on_cran()

  namespace = asNamespace("paradox")
  symbols = mget(
    paste0("C_", c(
      "design_transpose",
      "domain_construct",
      "domain_check_builtin",
      "domain_property_builtin",
      "domain_qunif_builtin",
      "domain_sanitize_builtin",
      "param_set_construct",
      "param_set_ids",
      "param_set_property",
      "param_set_check_builtin",
      "param_set_check_dt_builtin",
      "param_set_qunif_builtin",
      "sampler_unif_sample_builtin",
      "generate_design_grid_builtin",
      "param_set_get_domain",
      "param_set_domains",
      "param_set_params",
      "param_set_collection_params",
      "param_set_collection_deps",
      "param_set_has_dependencies",
      "param_set_core_state",
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
  shadow = ParamSetShadow$new(parameter_set, "double")
  shadow_private = shadow$.__enclos_env__$private
  state = private$.state()
  params = state$.params
  tags = state$.tags
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
    # `logical` is inactive in row 1 because `integer != 1`.
    logical = c(NA, FALSE),
    stringsAsFactors = FALSE
  )
  transpose_values = data.table::as.data.table(tabular_values)
  units = matrix(
    c(0, 1, 0.25, 0.75, 0.5, 0.5, 1, 0),
    nrow = 2L,
    dimnames = list(NULL, names(domains))
  )
  subset_private = new.env(parent = emptyenv())
  subset_private$.core = NULL
  collection_subset_private = new.env(parent = emptyenv())
  collection_subset_private$.core = NULL
  trafo_parameter_set = ps(x = p_dbl(trafo = exp))
  trafo_parameter_set$extra_trafo = function(x, param_set) x
  trafo_private = trafo_parameter_set$.__enclos_env__$private
  stripped_subset_private = new.env(parent = emptyenv())
  stripped_subset_private$.core = NULL
  sampler = SamplerUnif$new(ParamSet$new(domains))
  sampler$sample(0L)

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
    NULL,
    1L,
    FALSE,
    "x",
    NULL
  )
  checked_domain = .Call(
    symbols$C_domain_check_builtin,
    domains$double,
    list(0.25),
    FALSE
  )
  domain_levels = .Call(
    symbols$C_domain_property_builtin,
    domains$double,
    0L
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
    private,
    parameter_set,
    scalar_values,
    TRUE,
    TRUE,
    "none",
    TRUE
  )
  checked_table = .Call(
    symbols$C_param_set_check_dt_builtin,
    private,
    parameter_set,
    tabular_values,
    TRUE,
    "all",
    TRUE
  )
  mapped_set = .Call(
    symbols$C_param_set_qunif_builtin,
    private,
    parameter_set,
    units
  )
  set.seed(1729L)
  sampler_seed_before_zero = .Random.seed
  sampled_zero = .Call(
    symbols$C_sampler_unif_sample_builtin,
    sampler$param_set,
    0L
  )
  sampler_seed_after_zero = .Random.seed
  set.seed(1730L)
  sampler_seed_before_one = serialize(.Random.seed, NULL)
  sampled_one = .Call(
    symbols$C_sampler_unif_sample_builtin,
    sampler$param_set,
    1L
  )
  sampler_seed_after_one = .Random.seed
  set.seed(1731L)
  sampler_seed_before_rows = serialize(.Random.seed, NULL)
  sampled_rows = .Call(
    symbols$C_sampler_unif_sample_builtin,
    sampler$param_set,
    7L
  )
  sampler_seed_after_rows = .Random.seed
  generated_grid = .Call(
    symbols$C_generate_design_grid_builtin,
    private,
    parameter_set,
    c(double = 3, integer = 5, factor = 2, logical = 2),
    NULL
  )
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
  base_has_dependencies = .Call(
    symbols$C_param_set_has_dependencies,
    private,
    parameter_set
  )
  collection_has_dependencies = .Call(
    symbols$C_param_set_has_dependencies,
    collection_private,
    collection
  )
  shadow_has_dependencies = .Call(
    symbols$C_param_set_has_dependencies,
    shadow_private,
    shadow
  )
  subset_token = .Call(
    symbols$C_param_set_subset_state,
    private,
    parameter_set,
    c("factor", "double"),
    FALSE,
    TRUE,
    parameter_set$constraint,
    parameter_set$extra_trafo,
    TRUE
  )
  adopted_subset_token = .Call(
    symbols$C_param_set_adopt_subset_state,
    subset_private,
    subset_token
  )
  consumed_subset_token = .Call(
    symbols$C_param_set_adopt_subset_state,
    subset_private,
    subset_token
  )
  collection_subset_token = .Call(
    symbols$C_param_set_subset_state,
    collection_private,
    collection,
    c("inner.factor", "inner.double"),
    FALSE,
    TRUE,
    collection$constraint,
    collection$extra_trafo,
    TRUE
  )
  adopted_collection_subset_token = .Call(
    symbols$C_param_set_adopt_subset_state,
    collection_subset_private,
    collection_subset_token
  )
  stripped_subset_token = .Call(
    symbols$C_param_set_subset_state,
    trafo_private,
    trafo_parameter_set,
    "x",
    FALSE,
    TRUE,
    trafo_parameter_set$constraint,
    trafo_parameter_set$extra_trafo,
    FALSE
  )
  adopted_stripped_subset_token = .Call(
    symbols$C_param_set_adopt_subset_state,
    stripped_subset_private,
    stripped_subset_token
  )
  transposed = .Call(symbols$C_design_transpose, transpose_values, TRUE)

  gctorture(previous)

  expect_identical(names(constructed_domain), paradox:::domain_names)
  expect_true(checked_domain)
  expect_identical(domain_levels, Inf)
  expect_identical(mapped_domain, c(-1, 0, 1))
  expect_identical(sanitized_domain, list(-1, 1))
  expect_named(
    constructed_set,
    c("params", "tags", "trafos", "requirements", "init_values")
  )
  expect_identical(selected_ids, "double")
  expect_identical(
    properties,
    c(double = Inf, integer = 5, factor = 2, logical = 2)
  )
  expect_true(checked_values)
  expect_named(attr(checked_values, "sanitized"), names(scalar_values))
  expect_true(checked_table)
  expect_s3_class(mapped_set, "data.table")
  expect_identical(names(mapped_set), names(domains))
  expect_s3_class(sampled_zero, "data.table")
  expect_identical(dim(sampled_zero), c(0L, 4L))
  expect_identical(names(sampled_zero), names(domains))
  expect_identical(sampler_seed_after_zero, sampler_seed_before_zero)
  expect_s3_class(sampled_one, "data.table")
  expect_identical(dim(sampled_one), c(1L, 4L))
  expect_identical(names(sampled_one), names(domains))
  expect_false(identical(
    serialize(sampler_seed_after_one, NULL),
    sampler_seed_before_one
  ))
  expect_s3_class(sampled_rows, "data.table")
  expect_identical(dim(sampled_rows), c(7L, 4L))
  expect_identical(names(sampled_rows), names(domains))
  expect_false(identical(
    serialize(sampler_seed_after_rows, NULL),
    sampler_seed_before_rows
  ))
  expect_identical(
    vapply(sampled_rows, typeof, character(1L)),
    c(
      double = "double",
      integer = "integer",
      factor = "character",
      logical = "logical"
    )
  )
  expect_s3_class(generated_grid, "data.table")
  expect_identical(names(generated_grid), names(domains))
  expect_identical(dim(generated_grid), c(36L, 4L))
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
  expect_identical(base_has_dependencies, TRUE)
  expect_identical(collection_has_dependencies, TRUE)
  expect_identical(shadow_has_dependencies, TRUE)
  expect_true(adopted_subset_token)
  expect_identical(
    .Call(symbols$C_param_set_core_state, subset_private, NULL)$.params$id,
    c("factor", "double")
  )
  expect_false(consumed_subset_token)
  expect_true(adopted_collection_subset_token)
  expect_identical(
    .Call(
      symbols$C_param_set_core_state,
      collection_subset_private,
      NULL
    )$.params$id,
    c("inner.factor", "inner.double")
  )
  expect_true(adopted_stripped_subset_token)
  stripped_state = .Call(
    symbols$C_param_set_core_state,
    stripped_subset_private,
    NULL
  )
  expect_identical(nrow(stripped_state$.trafos), 0L)
  expect_null(stripped_state$.extra_trafo)
  expect_identical(transposed[[1L]]$double, -0.5)
})
