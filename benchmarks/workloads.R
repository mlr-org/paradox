# Deterministic workloads used by the paired benchmark worker. Keep this file
# free of package-loading side effects: run.R sources it to implement
# --list-workloads before either benchmark package is loaded.

benchmark_workload_names <- function() {
  c(
    "domain_p_dbl",
    "domain_p_int",
    "domain_p_fct",
    "domain_p_lgl",
    "domain_p_uty",
    "construct_ps_small",
    "construct_paramset_bulk",
    "construct_full_mixed",
    "shadow_construct_budget",
    "ids_all",
    "ids_class",
    "ids_tags",
    "ids_any_tags",
    "static_properties",
    "check_scalar",
    "check_dependencies",
    "has_deps_flags",
    "condition_equal_vector",
    "test_constraint_dt",
    "sanitize_scalar",
    "check_dt",
    "qunif",
    "paramset_subspaces",
    "paramset_subspaces_deps",
    "sampler_unif_construct",
    "sampler_unif_construct_deps",
    "sampler_unif_sample",
    "generate_design_random",
    "generate_design_grid_mixed4",
    "generate_design_grid_mixed8",
    "design_transpose_plain",
    "design_transpose_filtered",
    "design_transpose_trafo",
    "subset",
    "get_domain",
    "get_domain_middle",
    "get_domain_last",
    "domains_all",
    "params",
    "get_values",
    "get_values_deep_dependencies",
    "get_values_no_dependencies",
    "get_values_tags",
    "set_values_insert",
    "set_values_deep_dependencies",
    "shadow_values_live",
    "shadow_constraint_live",
    "shadow_domains_live",
    "shadow_assign_values",
    "collection_construct_plain",
    "collection_construct_rich",
    "collection_assign_values",
    "collection_values_plain",
    "collection_values_rich",
    "collection_values_nested",
    "collection_get_values_rich",
    "collection_get_values_nested",
    "collection_deps_rich",
    "collection_deps_nested",
    "collection_domains_plain",
    "collection_domains_rich",
    "collection_domains_nested",
    "collection_subset_rich",
    "collection_subset_callbacks",
    "collection_flatten_rich",
    "collection_flatten_callbacks",
    "collection_params_plain",
    "collection_params_rich",
    "collection_params_nested",
    "trafo"
  )
}

benchmark_make_inputs <- function(n_params, n_rows) {
  stopifnot(
    length(n_params) == 1L,
    is.integer(n_params),
    !is.na(n_params),
    n_params >= 4L,
    length(n_rows) == 1L,
    is.integer(n_rows),
    !is.na(n_rows),
    n_rows >= 1L
  )

  parameter_ids <- sprintf("parameter_%05d", seq_len(n_params))

  make_domain <- function(index) {
    switch(
      as.character((index - 1L) %% 4L),
      "0" = p_dbl(
        lower = -10,
        upper = 10,
        tolerance = 1e-6,
        tags = c("train", "bounded"),
        trafo = function(x) exp(x / 10)
      ),
      "1" = p_int(
        lower = -20L,
        upper = 20L,
        tags = c("train", "bounded")
      ),
      "2" = p_fct(
        levels = letters[1:8],
        tags = c("train", "categorical")
      ),
      "3" = p_lgl(tags = c("flag", "categorical", "required"))
    )
  }

  make_domain_list <- function() {
    domains <- lapply(seq_len(n_params), make_domain)
    names(domains) <- parameter_ids
    domains
  }

  # Reused domains isolate ParamSet$new() from p_*() construction. The
  # construct_full_mixed workload deliberately creates both from scratch.
  prebuilt_domains <- make_domain_list()
  space <- ParamSet$new(prebuilt_domains)
  sampler_unif <- SamplerUnif$new(space)

  make_full_mixed <- function() {
    ParamSet$new(make_domain_list())
  }

  make_small_ps <- function() {
    ps(
      learning_rate = p_dbl(1e-4, 1, tags = c("train", "bounded")),
      max_depth = p_int(1L, 32L, tags = c("train", "bounded")),
      booster = p_fct(c("linear", "tree", "dart"), tags = "categorical"),
      early_stopping = p_lgl(tags = c("flag", "required"))
    )
  }

  make_grid_space <- function(cycles) {
    domains <- lapply(seq_len(4L * cycles), function(index) {
      switch(
        as.character((index - 1L) %% 4L),
        "0" = p_dbl(-10, 10),
        "1" = p_int(-20L, 20L),
        "2" = p_fct(letters[1:3]),
        "3" = p_lgl()
      )
    })
    names(domains) <- sprintf("grid_parameter_%02d", seq_along(domains))
    ParamSet$new(domains)
  }
  grid_mixed4 <- make_grid_space(1L)
  grid_mixed8 <- make_grid_space(2L)

  scalar_values <- vector("list", n_params)
  for (i in seq_len(n_params)) {
    scalar_values[[i]] <- switch(
      as.character((i - 1L) %% 4L),
      "0" = 0.25,
      "1" = 10L,
      "2" = "c",
      "3" = TRUE
    )
  }
  names(scalar_values) <- parameter_ids

  sanitize_values <- scalar_values
  first_double <- which((seq_len(n_params) - 1L) %% 4L == 0L)[[1L]]
  sanitize_values[[first_double]] <- -10 - 5e-6
  sanitized_expected <- scalar_values
  sanitized_expected[[first_double]] <- -10

  # `$params` materializes every side table, so retain a separate rich space
  # with values and dependencies instead of making unrelated design/check
  # workloads dependency-aware.
  params_space <- ParamSet$new(prebuilt_domains)
  params_space$values <- scalar_values
  for (start in seq.int(1L, n_params, by = 4L)) {
    if (start + 3L > n_params) next
    params_space$add_dep(
      parameter_ids[[start + 2L]],
      parameter_ids[[start + 3L]],
      CondEqual(TRUE)
    )
  }

  mutation_space <- ParamSet$new(prebuilt_domains)
  mutation_space$values <- scalar_values
  mutation_update <- scalar_values[seq_len(min(4L, n_params))]
  mutation_update[[1L]] <- 0.5
  mutation_update[[2L]] <- 11L
  mutation_update[[3L]] <- "d"
  mutation_update[[4L]] <- FALSE
  mutation_expected <- scalar_values
  mutation_expected[names(mutation_update)] <- mutation_update

  # Keep these dependency workloads fixed at 64 parameters so their graph
  # depth does not silently change with the general --n-params benchmark
  # control. The checked assignment fixture has every edge satisfied and is
  # therefore semantically identical under Paradox 1 and 2.
  dependency_ids <- sprintf("dependency_%02d", seq_len(64L))
  dependency_domains <- lapply(seq_along(dependency_ids), function(index) {
    p_lgl(
      default = TRUE,
      tags = if (index %% 2L == 0L) "payload" else "control"
    )
  })
  names(dependency_domains) <- dependency_ids
  make_dependency_space <- function() {
    dependency_space <- ParamSet$new(dependency_domains)
    for (index in seq.int(2L, length(dependency_ids))) {
      dependency_space$add_dep(
        dependency_ids[[index]],
        dependency_ids[[index - 1L]],
        CondEqual(TRUE)
      )
    }
    dependency_space
  }

  dependency_assignment_space <- make_dependency_space()
  dependency_assignment_values <- setNames(
    as.list(rep(TRUE, length(dependency_ids))),
    dependency_ids
  )
  dependency_assignment_space$set_values(
    .values = dependency_assignment_values,
    .insert = FALSE
  )

  # The read fixture deliberately represents the same logical configuration
  # differently on the two sides of the paired benchmark. Paradox 2 stores
  # only even-numbered payload values: missing odd parents use their TRUE
  # defaults until dependency_32 = FALSE makes the remaining payload values
  # dormant. Paradox 1 has no default-aware dependency read, so it receives the
  # equivalent explicit odd parent values. Those control values are excluded
  # by the timed tag filter, keeping the validated outward result identical.
  dependency_read_space <- make_dependency_space()
  dependency_read_values <- dependency_assignment_values
  dependency_read_values[[32L]] <- FALSE
  if (utils::packageVersion("paradox") >= "2.0.0") {
    dependency_read_values <- dependency_read_values[
      seq.int(2L, length(dependency_ids), by = 2L)
    ]
    dependency_read_space$values <- dependency_read_values
  } else {
    dependency_read_space$assert_values <- FALSE
    dependency_read_space$values <- dependency_read_values
    dependency_read_space$assert_values <- TRUE
  }
  dependency_read_expected_ids <- dependency_ids[seq.int(2L, 32L, by = 2L)]
  dependency_read_expected <- setNames(
    as.list(c(rep(TRUE, 15L), FALSE)),
    dependency_read_expected_ids
  )

  collection_groups <- split(
    seq_len(n_params),
    ceiling(seq_len(n_params) / 8L)
  )
  make_collection_children <- function(rich) {
    children <- lapply(collection_groups, function(indices) {
      child <- ParamSet$new(prebuilt_domains[indices])
      if (rich) {
        child$values <- scalar_values[indices]
        if (length(indices) >= 2L) {
          child$add_dep(
            parameter_ids[[indices[[2L]]]],
            parameter_ids[[indices[[1L]]]],
            CondEqual(scalar_values[[indices[[1L]]]])
          )
        }
      }
      child
    })
    names(children) <- sprintf("set%03d", seq_along(children))
    children
  }
  plain_collection_children <- make_collection_children(FALSE)
  rich_collection_children <- make_collection_children(TRUE)
  plain_collection <- ParamSetCollection$new(plain_collection_children)
  mutation_collection <- ParamSetCollection$new(
    make_collection_children(FALSE)
  )
  mutation_collection_ids <- mutation_collection$ids()
  mutation_collection_values <- scalar_values[rev(seq_len(n_params))]
  names(mutation_collection_values) <- rev(mutation_collection_ids)
  mutation_collection_expected <- mutation_collection_values[match(
    mutation_collection_ids,
    names(mutation_collection_values)
  )]
  rich_collection <- ParamSetCollection$new(
    rich_collection_children,
    tag_sets = TRUE,
    tag_params = TRUE
  )
  if (rich_collection$length >= 2L) {
    rich_ids <- rich_collection$ids()
    rich_collection$add_dep(
      rich_ids[[length(rich_ids)]],
      rich_ids[[1L]],
      CondEqual(scalar_values[[1L]])
    )
  }
  callback_collection_children <- make_collection_children(TRUE)
  for (index in seq_along(callback_collection_children)) {
    if (index %% 2L == 1L) {
      callback_collection_children[[index]]$constraint = function(x) TRUE
      callback_collection_children[[index]]$extra_trafo = function(x) x
    }
  }
  callback_collection <- ParamSetCollection$new(
    callback_collection_children,
    tag_sets = TRUE,
    tag_params = TRUE
  )
  nested_children <- make_collection_children(TRUE)
  nested_groups <- split(
    seq_along(nested_children),
    ceiling(seq_along(nested_children) / 4L)
  )
  nested_inners <- lapply(nested_groups, function(indices) {
    ParamSetCollection$new(
      nested_children[indices],
      tag_sets = TRUE,
      tag_params = TRUE
    )
  })
  names(nested_inners) <- sprintf("outer%03d", seq_along(nested_inners))
  nested_collection <- ParamSetCollection$new(
    nested_inners,
    tag_sets = TRUE,
    tag_params = TRUE
  )

  # ParamSetShadow was introduced in Paradox 2 after being maintained by
  # miesmuschel. Use the package-owned generator when it exists and the exact
  # downstream compatibility implementation for the upstream baseline. This
  # keeps the paired result tied to the implementation users are migrating
  # from instead of timing a benchmark-only imitation.
  shadow_generator <- if ("ParamSetShadow" %in% getNamespaceExports("paradox")) {
    getExportedValue("paradox", "ParamSetShadow")
  } else {
    if (!requireNamespace("miesmuschel", quietly = TRUE)) {
      stop(
        "the upstream Shadow benchmark requires miesmuschel",
        call. = FALSE
      )
    }
    getExportedValue("miesmuschel", "ParamSetShadow")
  }
  shadow_hidden_id <- parameter_ids[[1L]]
  shadow_visible_ids <- parameter_ids[-1L]

  shadow_constructor_origin <- ParamSet$new(prebuilt_domains)
  make_shadow <- function() {
    shadow_generator$new(shadow_constructor_origin, shadow_hidden_id)
  }

  # Change a visible value after constructing the read fixture. The first
  # admission therefore proves that reads follow the origin's current
  # generation; timed repetitions then measure the normal unchanged-generation
  # hot path used by primed miesmuschel operators.
  shadow_read_origin <- ParamSet$new(prebuilt_domains)
  shadow_read_origin$values <- scalar_values
  shadow_read <- shadow_generator$new(shadow_read_origin, shadow_hidden_id)
  shadow_live_id <- parameter_ids[[2L]]
  shadow_read_expected <- scalar_values[shadow_visible_ids]
  shadow_read_expected[[shadow_live_id]] <- 11L
  shadow_read_origin$set_values(
    .values = setNames(list(11L), shadow_live_id)
  )

  # Keep mutation state independent from the read fixture: assignment is
  # intentionally idempotent across validation, warmup, and timed samples.
  shadow_write_origin <- ParamSet$new(prebuilt_domains)
  shadow_write_origin$values <- scalar_values
  shadow_write <- shadow_generator$new(shadow_write_origin, shadow_hidden_id)
  shadow_assignment_ids <- shadow_visible_ids[
    seq_len(min(4L, length(shadow_visible_ids)))
  ]
  shadow_assignment <- scalar_values[shadow_assignment_ids]
  shadow_assignment[[1L]] <- 11L
  if (length(shadow_assignment) >= 2L) shadow_assignment[[2L]] <- "d"
  if (length(shadow_assignment) >= 3L) shadow_assignment[[3L]] <- FALSE
  if (length(shadow_assignment) >= 4L) shadow_assignment[[4L]] <- 0.5
  shadow_write_expected <- c(
    scalar_values[shadow_hidden_id],
    shadow_assignment
  )

  # Exercise the live hidden-value merge used by miesmuschel's former
  # ParamSetShadow and the package-owned implementation. Keep the callback
  # deliberately tiny so the paired result exposes adapter overhead instead
  # of timing unrelated user work.
  shadow_constraint_origin <- ParamSet$new(prebuilt_domains)
  shadow_constraint_origin$values <- scalar_values[shadow_hidden_id]
  shadow_constraint_visible_id <- parameter_ids[[4L]]
  shadow_constraint_origin$constraint <- local({
    hidden_id <- shadow_hidden_id
    visible_id <- shadow_constraint_visible_id
    hidden_expected <- scalar_values[[hidden_id]]
    visible_expected <- scalar_values[[visible_id]]
    function(x) {
      identical(x[[hidden_id]], hidden_expected) &&
        identical(x[[visible_id]], visible_expected)
    }
  })
  shadow_constraint <- shadow_generator$new(
    shadow_constraint_origin,
    shadow_hidden_id
  )
  shadow_constraint_values <- scalar_values[shadow_visible_ids]

  batch <- as.data.frame(
    lapply(scalar_values, rep, times = n_rows),
    optional = TRUE,
    stringsAsFactors = FALSE
  )
  names(batch) <- parameter_ids
  constraint_batch <- data.table::as.data.table(batch)
  constraint_batch_space <- ParamSet$new(prebuilt_domains)
  constraint_batch_space$constraint <- local({
    observed_id <- parameter_ids[[1L]]
    function(x) !is.null(x[[observed_id]])
  })

  condition_equal <- CondEqual(2L)
  condition_values <- rep_len(1:4, n_rows)
  condition_expected <- condition_values == 2L

  unit_values <- ((seq_len(n_rows * n_params) - 1L) %% 997L + 0.5) / 997
  unit_matrix <- matrix(unit_values, nrow = n_rows, ncol = n_params)
  colnames(unit_matrix) <- parameter_ids

  design_data <- space$qunif(unit_matrix)
  design <- Design$new(
    space,
    data.table::copy(design_data),
    remove_dupl = FALSE
  )
  filtered_data <- data.table::copy(design_data)
  for (column_index in seq_along(filtered_data)) {
    if (column_index > n_rows) next
    rows <- seq.int(column_index, n_rows, by = 8L)
    missing <- switch(
      typeof(filtered_data[[column_index]]),
      double = NA_real_,
      integer = NA_integer_,
      character = NA_character_,
      logical = NA,
      stop("unexpected design column type", call. = FALSE)
    )
    data.table::set(filtered_data, i = rows, j = column_index, value = missing)
  }
  filtered_design <- Design$new(
    space,
    filtered_data,
    remove_dupl = FALSE
  )

  subset_ids <- parameter_ids[seq.int(1L, n_params, by = 2L)]
  first_id <- parameter_ids[[1L]]
  middle_id <- parameter_ids[[(n_params + 1L) %/% 2L]]
  last_id <- parameter_ids[[n_params]]

  list(
    n_params = n_params,
    n_rows = n_rows,
    parameter_ids = parameter_ids,
    prebuilt_domains = prebuilt_domains,
    space = space,
    sampler_unif = sampler_unif,
    params_space = params_space,
    constraint_batch_space = constraint_batch_space,
    constraint_batch = constraint_batch,
    condition_equal = condition_equal,
    condition_values = condition_values,
    condition_expected = condition_expected,
    mutation_space = mutation_space,
    mutation_update = mutation_update,
    mutation_expected = mutation_expected,
    dependency_ids = dependency_ids,
    dependency_assignment_space = dependency_assignment_space,
    dependency_assignment_values = dependency_assignment_values,
    dependency_read_space = dependency_read_space,
    dependency_read_values = dependency_read_values,
    dependency_read_expected = dependency_read_expected,
    shadow_hidden_id = shadow_hidden_id,
    shadow_visible_ids = shadow_visible_ids,
    shadow_constructor_origin = shadow_constructor_origin,
    make_shadow = make_shadow,
    shadow_read_origin = shadow_read_origin,
    shadow_read = shadow_read,
    shadow_live_id = shadow_live_id,
    shadow_read_expected = shadow_read_expected,
    shadow_write_origin = shadow_write_origin,
    shadow_write = shadow_write,
    shadow_assignment = shadow_assignment,
    shadow_write_expected = shadow_write_expected,
    shadow_constraint_origin = shadow_constraint_origin,
    shadow_constraint = shadow_constraint,
    shadow_constraint_values = shadow_constraint_values,
    plain_collection = plain_collection,
    mutation_collection = mutation_collection,
    mutation_collection_values = mutation_collection_values,
    mutation_collection_expected = mutation_collection_expected,
    rich_collection = rich_collection,
    callback_collection = callback_collection,
    nested_collection = nested_collection,
    plain_collection_children = plain_collection_children,
    rich_collection_children = rich_collection_children,
    plain_collection_ids = plain_collection$ids(),
    rich_collection_ids = rich_collection$ids(),
    callback_collection_ids = callback_collection$ids(),
    nested_collection_ids = nested_collection$ids(),
    rich_collection_dep_rows = nrow(rich_collection$deps),
    nested_collection_dep_rows = nrow(nested_collection$deps),
    rich_collection_subset_ids = rich_collection$ids()[
      seq.int(1L, rich_collection$length, by = 2L)
    ],
    callback_collection_subset_ids = callback_collection$ids()[
      seq.int(1L, callback_collection$length, by = 2L)
    ],
    make_full_mixed = make_full_mixed,
    make_small_ps = make_small_ps,
    grid_mixed4 = grid_mixed4,
    grid_mixed8 = grid_mixed8,
    grid_resolution = 3L,
    scalar_values = scalar_values,
    sanitize_values = sanitize_values,
    sanitized_expected = sanitized_expected,
    batch = batch,
    unit_matrix = unit_matrix,
    design = design,
    filtered_design = filtered_design,
    subset_ids = subset_ids,
    first_id = first_id,
    middle_id = middle_id,
    last_id = last_id
  )
}

benchmark_make_workloads <- function(inputs) {
  stopifnot(is.list(inputs), inherits(inputs$space, "ParamSet"))

  exact_ids <- function(result, expected) {
    stopifnot(is.character(result), identical(unname(result), unname(expected)))
    paste(result, collapse = "\037")
  }

  exact_param_set <- function(result, expected_ids) {
    stopifnot(inherits(result, "ParamSet"))
    exact_ids(result$ids(), expected_ids)
  }

  exact_grid <- function(result, space) {
    data <- result$data
    resolutions <- space$nlevels
    resolutions[space$is_number] <- inputs$grid_resolution
    expected_rows <- as.integer(prod(resolutions))
    stopifnot(
      inherits(result, "Design"),
      inherits(data, "data.table"),
      nrow(data) == expected_rows,
      ncol(data) == space$length,
      identical(names(data), space$ids()),
      identical(result$param_set$ids(), space$ids()),
      identical(
        unname(vapply(data, typeof, character(1L))),
        rep(c("double", "integer", "character", "logical"),
          length.out = space$length)
      )
    )
    paste(expected_rows, space$length, data[[1L]][[1L]],
      data[[1L]][[expected_rows]], sep = "|")
  }

  workloads <- list(
    domain_p_dbl = list(
      expression = quote(p_dbl(-10, 10, tolerance = 1e-6, tags = c("train", "bounded"))),
      validate = function(result) {
        stopifnot(
          inherits(result, "Domain"),
          inherits(result, "ParamDbl"),
          nrow(result) == 1L,
          identical(result$lower, -10),
          identical(result$upper, 10)
        )
        "ParamDbl[-10,10]"
      }
    ),
    domain_p_int = list(
      expression = quote(p_int(-20L, 20L, tolerance = 0, tags = c("train", "bounded"))),
      validate = function(result) {
        stopifnot(
          inherits(result, "Domain"),
          inherits(result, "ParamInt"),
          nrow(result) == 1L,
          identical(result$lower, -20L),
          identical(result$upper, 20L)
        )
        "ParamInt[-20,20]"
      }
    ),
    domain_p_fct = list(
      expression = quote(p_fct(letters[1:8], tags = c("train", "categorical"))),
      validate = function(result) {
        stopifnot(
          inherits(result, "Domain"),
          inherits(result, "ParamFct"),
          nrow(result) == 1L,
          identical(result$levels[[1L]], letters[1:8])
        )
        "ParamFct[8]"
      }
    ),
    domain_p_lgl = list(
      expression = quote(p_lgl(tags = c("flag", "categorical"))),
      validate = function(result) {
        stopifnot(
          inherits(result, "Domain"),
          inherits(result, "ParamLgl"),
          nrow(result) == 1L,
          identical(result$levels[[1L]], c(TRUE, FALSE))
        )
        "ParamLgl"
      }
    ),
    domain_p_uty = list(
      expression = quote(p_uty(custom_check = is.numeric, tags = "payload")),
      validate = function(result) {
        stopifnot(
          inherits(result, "Domain"),
          inherits(result, "ParamUty"),
          nrow(result) == 1L,
          identical(result$cargo[[1L]]$custom_check, is.numeric)
        )
        "ParamUty"
      }
    ),
    construct_ps_small = list(
      expression = quote(inputs$make_small_ps()),
      validate = function(result) {
        exact_param_set(
          result,
          c("learning_rate", "max_depth", "booster", "early_stopping")
        )
      }
    ),
    construct_paramset_bulk = list(
      expression = quote(ParamSet$new(inputs$prebuilt_domains)),
      validate = function(result) exact_param_set(result, inputs$parameter_ids)
    ),
    construct_full_mixed = list(
      expression = quote(inputs$make_full_mixed()),
      validate = function(result) exact_param_set(result, inputs$parameter_ids)
    ),
    shadow_construct_budget = list(
      expression = quote(inputs$make_shadow()),
      validate = function(result) {
        stopifnot(
          inherits(result, "ParamSetShadow"),
          inherits(result, "ParamSet"),
          identical(result$origin, inputs$shadow_constructor_origin),
          identical(result$ids(), inputs$shadow_visible_ids)
        )
        paste(
          result$length,
          inputs$shadow_hidden_id,
          result$ids()[[1L]],
          result$ids()[[result$length]],
          sep = "|"
        )
      }
    ),
    ids_all = list(
      expression = quote(inputs$space$ids()),
      validate = function(result) exact_ids(result, inputs$parameter_ids)
    ),
    ids_class = list(
      expression = quote(inputs$space$ids(class = c("ParamDbl", "ParamFct"))),
      validate = function(result) {
        keep <- (seq_along(inputs$parameter_ids) - 1L) %% 4L %in% c(0L, 2L)
        exact_ids(result, inputs$parameter_ids[keep])
      }
    ),
    ids_tags = list(
      expression = quote(inputs$space$ids(tags = c("train", "bounded"))),
      validate = function(result) {
        keep <- (seq_along(inputs$parameter_ids) - 1L) %% 4L %in% c(0L, 1L)
        exact_ids(result, inputs$parameter_ids[keep])
      }
    ),
    ids_any_tags = list(
      # A single any_tags value avoids the duplicate-ID bug in the pinned
      # upstream release; semantic bug cases belong in differential tests.
      expression = quote(inputs$space$ids(any_tags = "categorical")),
      validate = function(result) {
        keep <- (seq_along(inputs$parameter_ids) - 1L) %% 4L %in% c(2L, 3L)
        exact_ids(result, inputs$parameter_ids[keep])
      }
    ),
    static_properties = list(
      expression = quote(list(
        nlevels = inputs$space$nlevels,
        is_number = inputs$space$is_number,
        is_categ = inputs$space$is_categ,
        is_bounded = inputs$space$is_bounded
      )),
      validate = function(result) {
        index <- seq_along(inputs$parameter_ids) - 1L
        stopifnot(
          is.list(result),
          identical(names(result), c("nlevels", "is_number", "is_categ", "is_bounded")),
          identical(names(result$nlevels), inputs$parameter_ids),
          identical(unname(result$is_number), index %% 4L %in% c(0L, 1L)),
          identical(unname(result$is_categ), index %% 4L %in% c(2L, 3L)),
          identical(unname(result$is_bounded), rep(TRUE, inputs$n_params))
        )
        paste(
          paste(result$nlevels, collapse = ","),
          paste(as.integer(result$is_number), collapse = ""),
          paste(as.integer(result$is_categ), collapse = ""),
          paste(as.integer(result$is_bounded), collapse = ""),
          sep = "|"
        )
      }
    ),
    check_scalar = list(
      expression = quote(inputs$space$check(inputs$scalar_values, presence = "all")),
      validate = function(result) {
        stopifnot(isTRUE(result), is.null(attributes(result)))
        "TRUE"
      }
    ),
    check_dependencies = list(
      expression = quote(inputs$params_space$check_dependencies(
        inputs$scalar_values
      )),
      validate = function(result) {
        stopifnot(isTRUE(result), is.null(attributes(result)))
        "TRUE"
      }
    ),
    has_deps_flags = list(
      expression = quote(c(
        base = inputs$params_space$has_deps,
        collection = inputs$rich_collection$has_deps,
        shadow = inputs$shadow_read$has_deps
      )),
      validate = function(result) {
        stopifnot(identical(result, c(
          base = TRUE,
          collection = TRUE,
          shadow = FALSE
        )))
        paste(as.integer(result), collapse = "")
      }
    ),
    condition_equal_vector = list(
      expression = quote(condition_test(
        inputs$condition_equal,
        inputs$condition_values
      )),
      validate = function(result) {
        stopifnot(
          is.logical(result),
          is.null(attributes(result)),
          identical(result, inputs$condition_expected)
        )
        paste(length(result), sum(result), sep = "x")
      }
    ),
    test_constraint_dt = list(
      expression = quote(inputs$constraint_batch_space$test_constraint_dt(
        inputs$constraint_batch,
        assert_value = FALSE
      )),
      validate = function(result) {
        stopifnot(
          is.logical(result),
          is.null(attributes(result)),
          identical(result, rep(TRUE, inputs$n_rows))
        )
        paste(length(result), "TRUE", sep = "x")
      }
    ),
    sanitize_scalar = list(
      expression = quote(inputs$space$check(
        inputs$sanitize_values,
        sanitize = TRUE,
        presence = "all"
      )),
      validate = function(result) {
        stopifnot(
          isTRUE(result),
          identical(attr(result, "sanitized", exact = TRUE), inputs$sanitized_expected)
        )
        "TRUE+sanitized"
      }
    ),
    check_dt = list(
      expression = quote(inputs$space$check_dt(inputs$batch, presence = "all")),
      validate = function(result) {
        stopifnot(isTRUE(result))
        "TRUE"
      }
    ),
    qunif = list(
      expression = quote(inputs$space$qunif(inputs$unit_matrix)),
      validate = function(result) {
        stopifnot(
          is.data.frame(result),
          nrow(result) == inputs$n_rows,
          ncol(result) == inputs$n_params,
          identical(names(result), inputs$parameter_ids)
        )
        first <- result[[1L]]
        stopifnot(is.numeric(first), all(is.finite(first)), all(first >= -10), all(first <= 10))
        paste(
          nrow(result),
          ncol(result),
          format(first[[1L]], digits = 17),
          format(first[[length(first)]], digits = 17),
          sep = "|"
        )
      }
    ),
    paramset_subspaces = list(
      expression = quote(inputs$space$subspaces()),
      validate = function(result) {
        child_ids = vapply(
          result,
          function(subspace) subspace$ids(),
          character(1L)
        )
        stopifnot(
          length(result) == inputs$n_params,
          identical(names(result), inputs$parameter_ids),
          identical(unname(child_ids), inputs$parameter_ids),
          all(vapply(result, inherits, logical(1L), "ParamSet")),
          all(vapply(result, function(subspace) subspace$length == 1L,
            logical(1L)))
        )
        paste(length(result), paste(child_ids, collapse = ","), sep = "|")
      }
    ),
    paramset_subspaces_deps = list(
      expression = quote(inputs$params_space$subspaces()),
      validate = function(result) {
        child_ids = vapply(
          result,
          function(subspace) subspace$ids(),
          character(1L)
        )
        stopifnot(
          length(result) == inputs$n_params,
          identical(names(result), inputs$parameter_ids),
          identical(unname(child_ids), inputs$parameter_ids),
          nrow(inputs$params_space$deps) > 0L,
          all(vapply(result, function(subspace) nrow(subspace$deps) == 0L,
            logical(1L)))
        )
        paste(length(result), nrow(inputs$params_space$deps), sep = "|")
      }
    ),
    sampler_unif_construct = list(
      expression = quote(SamplerUnif$new(inputs$space)),
      validate = function(result) {
        child_ids = vapply(
          result$samplers,
          function(sampler) sampler$param$ids(),
          character(1L)
        )
        stopifnot(
          inherits(result, "SamplerUnif"),
          identical(result$param_set$ids(), inputs$parameter_ids),
          identical(names(result$samplers), inputs$parameter_ids),
          identical(unname(child_ids), inputs$parameter_ids)
        )
        paste(length(result$samplers), paste(child_ids, collapse = ","),
          sep = "|")
      }
    ),
    sampler_unif_construct_deps = list(
      expression = quote(SamplerUnif$new(inputs$params_space)),
      validate = function(result) {
        child_ids = vapply(
          result$samplers,
          function(sampler) sampler$param$ids(),
          character(1L)
        )
        stopifnot(
          inherits(result, "SamplerUnif"),
          identical(result$param_set$ids(), inputs$parameter_ids),
          identical(names(result$samplers), inputs$parameter_ids),
          identical(unname(child_ids), inputs$parameter_ids),
          nrow(result$param_set$deps) == nrow(inputs$params_space$deps),
          all(vapply(result$samplers,
            function(sampler) nrow(sampler$param$deps) == 0L,
            logical(1L)))
        )
        paste(length(result$samplers), nrow(result$param_set$deps), sep = "|")
      }
    ),
    sampler_unif_sample = list(
      # Consumers retain the sampler and invoke it inside optimizer loops;
      # construction is intentionally outside the timed expression.
      expression = quote(inputs$sampler_unif$sample(inputs$n_rows)),
      validate = function(result) {
        stopifnot(
          inherits(result, "Design"),
          nrow(result$data) == inputs$n_rows,
          ncol(result$data) == inputs$n_params,
          identical(names(result$data), inputs$parameter_ids),
          identical(result$param_set, inputs$sampler_unif$param_set)
        )
        paste(
          nrow(result$data),
          ncol(result$data),
          paste(vapply(result$data, typeof, character(1L)), collapse = ","),
          sep = "|"
        )
      }
    ),
    generate_design_random = list(
      expression = quote(generate_design_random(inputs$space, inputs$n_rows)),
      validate = function(result) {
        stopifnot(
          inherits(result, "Design"),
          nrow(result$data) == inputs$n_rows,
          ncol(result$data) == inputs$n_params,
          identical(names(result$data), inputs$parameter_ids),
          identical(result$param_set$ids(), inputs$parameter_ids)
        )
        paste(
          nrow(result$data),
          ncol(result$data),
          paste(vapply(result$data, typeof, character(1L)), collapse = ","),
          sep = "|"
        )
      }
    ),
    generate_design_grid_mixed4 = list(
      expression = quote(generate_design_grid(
        inputs$grid_mixed4,
        inputs$grid_resolution
      )),
      validate = function(result) exact_grid(result, inputs$grid_mixed4)
    ),
    generate_design_grid_mixed8 = list(
      expression = quote(generate_design_grid(
        inputs$grid_mixed8,
        inputs$grid_resolution
      )),
      validate = function(result) exact_grid(result, inputs$grid_mixed8)
    ),
    design_transpose_plain = list(
      expression = quote(inputs$design$transpose(filter_na = FALSE, trafo = FALSE)),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_rows,
          all(lengths(result) == inputs$n_params),
          identical(names(result[[1L]]), inputs$parameter_ids)
        )
        paste(length(result), sum(lengths(result)), result[[1L]][[2L]], sep = "|")
      }
    ),
    design_transpose_filtered = list(
      expression = quote(inputs$filtered_design$transpose(
        filter_na = TRUE,
        trafo = FALSE
      )),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_rows,
          all(lengths(result) <= inputs$n_params),
          any(lengths(result) < inputs$n_params)
        )
        paste(length(result), sum(lengths(result)), min(lengths(result)), sep = "|")
      }
    ),
    design_transpose_trafo = list(
      expression = quote(inputs$design$transpose(filter_na = TRUE, trafo = TRUE)),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_rows,
          all(lengths(result) == inputs$n_params),
          identical(names(result[[1L]]), inputs$parameter_ids),
          is.double(result[[1L]][[1L]])
        )
        paste(
          length(result),
          sum(lengths(result)),
          format(result[[1L]][[1L]], digits = 17),
          sep = "|"
        )
      }
    ),
    subset = list(
      expression = quote(inputs$space$subset(inputs$subset_ids)),
      validate = function(result) exact_param_set(result, inputs$subset_ids)
    ),
    get_domain = list(
      expression = quote(inputs$space$get_domain(inputs$first_id)),
      validate = function(result) {
        stopifnot(
          inherits(result, "Domain"),
          inherits(result, "ParamDbl"),
          identical(result$lower, -10),
          identical(result$upper, 10)
        )
        "ParamDbl[-10,10]"
      }
    ),
    get_domain_middle = list(
      expression = quote(inputs$space$get_domain(inputs$middle_id)),
      validate = function(result) {
        stopifnot(
          inherits(result, "Domain"),
          identical(result$id, inputs$middle_id)
        )
        paste(result$id, result$cls, sep = "|")
      }
    ),
    get_domain_last = list(
      expression = quote(inputs$space$get_domain(inputs$last_id)),
      validate = function(result) {
        stopifnot(
          inherits(result, "Domain"),
          identical(result$id, inputs$last_id)
        )
        paste(result$id, result$cls, sep = "|")
      }
    ),
    domains_all = list(
      expression = quote(inputs$space$domains),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_params,
          identical(names(result), inputs$parameter_ids),
          all(vapply(result, inherits, logical(1L), "Domain")),
          identical(
            unname(vapply(result, function(domain) domain$id, character(1L))),
            inputs$parameter_ids
          )
        )
        paste(length(result), result[[1L]]$cls, result[[length(result)]]$cls, sep = "|")
      }
    ),
    params = list(
      expression = quote(inputs$params_space$params),
      validate = function(result) {
        stopifnot(
          inherits(result, "data.table"),
          nrow(result) == inputs$n_params,
          ncol(result) == length(paradox:::domain_names),
          identical(names(result), paradox:::domain_names),
          identical(result$id, inputs$parameter_ids),
          all(lengths(result$.tags) >= 2L),
          sum(!vapply(result$.trafo, is.null, logical(1L))) ==
            ceiling(inputs$n_params / 4),
          sum(result$.init_given) == inputs$n_params,
          sum(!vapply(result$.requirements, is.null, logical(1L))) ==
            inputs$n_params %/% 4L
        )
        paste(
          nrow(result),
          sum(lengths(result$.tags)),
          sum(result$.init_given),
          sum(!vapply(result$.requirements, is.null, logical(1L))),
          sep = "|"
        )
      }
    ),
    get_values = list(
      expression = quote(inputs$params_space$get_values()),
      validate = function(result) {
        stopifnot(
          is.list(result),
          identical(names(result), inputs$parameter_ids),
          identical(result, inputs$scalar_values)
        )
        paste(length(result), result[[2L]], result[[3L]], sep = "|")
      }
    ),
    get_values_deep_dependencies = list(
      expression = quote(inputs$dependency_read_space$get_values(
        tags = "payload"
      )),
      validate = function(result) {
        stopifnot(
          is.list(result),
          identical(result, inputs$dependency_read_expected),
          identical(
            inputs$dependency_read_space$values,
            inputs$dependency_read_values
          )
        )
        paste(
          length(inputs$dependency_ids),
          length(result),
          result[[length(result)]],
          sep = "|"
        )
      }
    ),
    get_values_no_dependencies = list(
      expression = quote(inputs$params_space$get_values(
        check_required = FALSE,
        remove_dependencies = FALSE
      )),
      validate = function(result) {
        stopifnot(
          is.list(result),
          identical(names(result), inputs$parameter_ids),
          identical(result, inputs$scalar_values)
        )
        paste(length(result), result[[2L]], result[[3L]], sep = "|")
      }
    ),
    get_values_tags = list(
      expression = quote(inputs$params_space$get_values(tags = "train")),
      validate = function(result) {
        keep = (seq_along(inputs$parameter_ids) - 1L) %% 4L %in% 0:2
        stopifnot(
          is.list(result),
          identical(names(result), inputs$parameter_ids[keep]),
          identical(result, inputs$scalar_values[keep])
        )
        paste(length(result), result[[2L]], result[[3L]], sep = "|")
      }
    ),
    set_values_insert = list(
      expression = quote(inputs$mutation_space$set_values(
        .values = inputs$mutation_update
      )),
      validate = function(result) {
        observed = inputs$mutation_space$values
        stopifnot(
          identical(result, inputs$mutation_space),
          identical(names(observed), inputs$parameter_ids),
          identical(observed, inputs$mutation_expected)
        )
        paste(
          length(observed),
          observed[[1L]],
          observed[[2L]],
          observed[[3L]],
          observed[[4L]],
          sep = "|"
        )
      }
    ),
    set_values_deep_dependencies = list(
      expression = quote(inputs$dependency_assignment_space$set_values(
        .values = inputs$dependency_assignment_values,
        .insert = FALSE
      )),
      validate = function(result) {
        observed = inputs$dependency_assignment_space$values
        stopifnot(
          identical(result, inputs$dependency_assignment_space),
          identical(observed, inputs$dependency_assignment_values),
          identical(names(observed), inputs$dependency_ids)
        )
        paste(
          length(observed),
          observed[[1L]],
          observed[[length(observed)]],
          sep = "|"
        )
      }
    ),
    shadow_values_live = list(
      expression = quote(inputs$shadow_read$values),
      validate = function(result) {
        stopifnot(
          is.list(result),
          identical(names(result), inputs$shadow_visible_ids),
          identical(result, inputs$shadow_read_expected),
          identical(
            inputs$shadow_read_origin$values[[inputs$shadow_hidden_id]],
            inputs$scalar_values[[inputs$shadow_hidden_id]]
          )
        )
        paste(
          length(result),
          inputs$shadow_live_id,
          result[[inputs$shadow_live_id]],
          sep = "|"
        )
      }
    ),
    shadow_constraint_live = list(
      expression = quote(inputs$shadow_constraint$constraint(
        inputs$shadow_constraint_values
      )),
      validate = function(result) {
        stopifnot(
          isTRUE(result),
          identical(
            inputs$shadow_constraint_origin$values,
            inputs$scalar_values[inputs$shadow_hidden_id]
          )
        )
        "TRUE"
      }
    ),
    shadow_domains_live = list(
      expression = quote(inputs$shadow_read$domains),
      validate = function(result) {
        stopifnot(
          is.list(result),
          identical(names(result), inputs$shadow_visible_ids),
          all(vapply(result, inherits, logical(1L), "Domain")),
          identical(
            result[[inputs$shadow_live_id]]$.init[[1L]],
            inputs$shadow_read_expected[[inputs$shadow_live_id]]
          )
        )
        paste(
          length(result),
          inputs$shadow_live_id,
          result[[inputs$shadow_live_id]]$.init[[1L]],
          sep = "|"
        )
      }
    ),
    shadow_assign_values = list(
      expression = quote(
        inputs$shadow_write$values <- inputs$shadow_assignment
      ),
      validate = function(result) {
        stopifnot(
          identical(result, inputs$shadow_assignment),
          identical(inputs$shadow_write$values, inputs$shadow_assignment),
          identical(
            inputs$shadow_write_origin$values,
            inputs$shadow_write_expected
          ),
          identical(
            inputs$shadow_write_origin$values[[inputs$shadow_hidden_id]],
            inputs$scalar_values[[inputs$shadow_hidden_id]]
          )
        )
        paste(
          length(result),
          inputs$shadow_hidden_id,
          inputs$shadow_write_origin$values[[inputs$shadow_hidden_id]],
          sep = "|"
        )
      }
    ),
    collection_construct_plain = list(
      expression = quote(ParamSetCollection$new(
        inputs$plain_collection_children
      )),
      validate = function(result) {
        stopifnot(
          inherits(result, "ParamSetCollection"),
          identical(result$ids(), inputs$plain_collection_ids),
          result$length == inputs$n_params
        )
        paste(result$length, length(result$sets), sep = "|")
      }
    ),
    collection_construct_rich = list(
      expression = quote(ParamSetCollection$new(
        inputs$rich_collection_children,
        tag_sets = TRUE,
        tag_params = TRUE
      )),
      validate = function(result) {
        stopifnot(
          inherits(result, "ParamSetCollection"),
          identical(result$ids(), inputs$rich_collection_ids),
          result$length == inputs$n_params,
          all(lengths(result$tags) >= 4L)
        )
        paste(result$length, sum(lengths(result$tags)), sep = "|")
      }
    ),
    collection_assign_values = list(
      expression = quote(
        inputs$mutation_collection$values <-
          inputs$mutation_collection_values
      ),
      validate = function(result) {
        observed = inputs$mutation_collection$values
        stopifnot(
          identical(result, inputs$mutation_collection_values),
          identical(names(observed), inputs$mutation_collection$ids()),
          identical(observed, inputs$mutation_collection_expected)
        )
        paste(
          length(observed),
          observed[[1L]],
          observed[[2L]],
          observed[[3L]],
          observed[[4L]],
          sep = "|"
        )
      }
    ),
    collection_values_plain = list(
      expression = quote(inputs$plain_collection$values),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == 0L,
          !is.null(names(result))
        )
        paste(length(result), length(names(result)), sep = "|")
      }
    ),
    collection_values_rich = list(
      expression = quote(inputs$rich_collection$values),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_params,
          identical(names(result), inputs$rich_collection_ids)
        )
        paste(length(result), result[[2L]], result[[3L]], sep = "|")
      }
    ),
    collection_values_nested = list(
      expression = quote(inputs$nested_collection$values),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_params,
          identical(names(result), inputs$nested_collection_ids)
        )
        paste(length(result), result[[2L]], result[[3L]], sep = "|")
      }
    ),
    collection_get_values_rich = list(
      expression = quote(inputs$rich_collection$get_values()),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_params,
          identical(names(result), inputs$rich_collection_ids)
        )
        paste(length(result), result[[2L]], result[[3L]], sep = "|")
      }
    ),
    collection_get_values_nested = list(
      expression = quote(inputs$nested_collection$get_values()),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_params,
          identical(names(result), inputs$nested_collection_ids)
        )
        paste(length(result), result[[2L]], result[[3L]], sep = "|")
      }
    ),
    collection_deps_rich = list(
      expression = quote(inputs$rich_collection$deps),
      validate = function(result) {
        stopifnot(
          inherits(result, "data.table"),
          identical(names(result), c("id", "on", "cond")),
          nrow(result) == inputs$rich_collection_dep_rows
        )
        paste(nrow(result), result$id[[1L]], result$on[[1L]], sep = "|")
      }
    ),
    collection_deps_nested = list(
      expression = quote(inputs$nested_collection$deps),
      validate = function(result) {
        stopifnot(
          inherits(result, "data.table"),
          identical(names(result), c("id", "on", "cond")),
          nrow(result) == inputs$nested_collection_dep_rows
        )
        paste(nrow(result), result$id[[1L]], result$on[[1L]], sep = "|")
      }
    ),
    collection_domains_plain = list(
      expression = quote(inputs$plain_collection$domains),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_params,
          identical(names(result), inputs$plain_collection_ids),
          all(vapply(result, inherits, logical(1L), "Domain"))
        )
        paste(length(result), result[[1L]]$cls, sep = "|")
      }
    ),
    collection_domains_rich = list(
      expression = quote(inputs$rich_collection$domains),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_params,
          identical(names(result), inputs$rich_collection_ids),
          all(vapply(result, inherits, logical(1L), "Domain")),
          all(vapply(result, function(domain) domain$.init_given, logical(1L)))
        )
        requirements = lapply(result, function(domain) {
          domain$.requirements[[1L]]
        })
        paste(length(result), sum(lengths(requirements)), sep = "|")
      }
    ),
    collection_domains_nested = list(
      expression = quote(inputs$nested_collection$domains),
      validate = function(result) {
        stopifnot(
          is.list(result),
          length(result) == inputs$n_params,
          identical(names(result), inputs$nested_collection_ids),
          all(vapply(result, inherits, logical(1L), "Domain")),
          all(vapply(result, function(domain) domain$.init_given, logical(1L)))
        )
        requirements = lapply(result, function(domain) {
          domain$.requirements[[1L]]
        })
        paste(length(result), sum(lengths(requirements)), sep = "|")
      }
    ),
    collection_subset_rich = list(
      expression = quote(inputs$rich_collection$subset(
        inputs$rich_collection_subset_ids,
        allow_dangling_dependencies = TRUE
      )),
      validate = function(result) {
        stopifnot(
          inherits(result, "ParamSet"),
          !inherits(result, "ParamSetCollection"),
          identical(result$ids(), inputs$rich_collection_subset_ids)
        )
        paste(result$length, result$ids()[[1L]], sep = "|")
      }
    ),
    collection_subset_callbacks = list(
      expression = quote(inputs$callback_collection$subset(
        inputs$callback_collection_subset_ids,
        allow_dangling_dependencies = TRUE
      )),
      validate = function(result) {
        stopifnot(
          inherits(result, "ParamSet"),
          !inherits(result, "ParamSetCollection"),
          identical(result$ids(), inputs$callback_collection_subset_ids),
          is.function(result$constraint),
          is.function(result$extra_trafo)
        )
        paste(result$length, result$ids()[[1L]], sep = "|")
      }
    ),
    collection_flatten_rich = list(
      expression = quote(inputs$rich_collection$flatten()),
      validate = function(result) {
        stopifnot(
          inherits(result, "ParamSet"),
          !inherits(result, "ParamSetCollection"),
          identical(result$ids(), inputs$rich_collection_ids)
        )
        paste(result$length, result$ids()[[1L]], sep = "|")
      }
    ),
    collection_flatten_callbacks = list(
      expression = quote(inputs$callback_collection$flatten()),
      validate = function(result) {
        stopifnot(
          inherits(result, "ParamSet"),
          !inherits(result, "ParamSetCollection"),
          identical(result$ids(), inputs$callback_collection_ids),
          is.function(result$constraint),
          is.function(result$extra_trafo)
        )
        paste(result$length, result$ids()[[1L]], sep = "|")
      }
    ),
    collection_params_plain = list(
      expression = quote(inputs$plain_collection$params),
      validate = function(result) {
        stopifnot(
          inherits(result, "data.table"),
          identical(result$id, inputs$plain_collection_ids),
          nrow(result) == inputs$n_params,
          identical(names(result), paradox:::domain_names)
        )
        paste(nrow(result), sum(result$.init_given), sep = "|")
      }
    ),
    collection_params_rich = list(
      expression = quote(inputs$rich_collection$params),
      validate = function(result) {
        stopifnot(
          inherits(result, "data.table"),
          identical(result$id, inputs$rich_collection_ids),
          nrow(result) == inputs$n_params,
          all(result$.init_given),
          any(lengths(result$.tags) >= 4L),
          any(!vapply(result$.requirements, is.null, logical(1L)))
        )
        paste(
          nrow(result),
          sum(lengths(result$.tags)),
          sum(!vapply(result$.requirements, is.null, logical(1L))),
          sep = "|"
        )
      }
    ),
    collection_params_nested = list(
      expression = quote(inputs$nested_collection$params),
      validate = function(result) {
        stopifnot(
          inherits(result, "data.table"),
          identical(result$id, inputs$nested_collection_ids),
          nrow(result) == inputs$n_params,
          all(result$.init_given),
          any(!vapply(result$.requirements, is.null, logical(1L)))
        )
        paste(
          nrow(result),
          sum(lengths(result$.tags)),
          sum(!vapply(result$.requirements, is.null, logical(1L))),
          sep = "|"
        )
      }
    ),
    trafo = list(
      expression = quote(inputs$space$trafo(inputs$scalar_values)),
      validate = function(result) {
        stopifnot(
          is.list(result),
          identical(names(result), inputs$parameter_ids),
          isTRUE(all.equal(result[[1L]], exp(0.25 / 10), tolerance = 1e-14)),
          identical(result[[2L]], 10L),
          identical(result[[3L]], "c"),
          identical(result[[4L]], TRUE)
        )
        paste(
          length(result),
          format(result[[1L]], digits = 17),
          result[[2L]],
          result[[3L]],
          result[[4L]],
          sep = "|"
        )
      }
    )
  )

  stopifnot(identical(names(workloads), benchmark_workload_names()))
  workloads
}
