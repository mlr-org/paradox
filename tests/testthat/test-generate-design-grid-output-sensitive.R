grid_output_legacy_reference = function(param_set, resolutions) {
  unit_columns = lapply(resolutions, function(resolution) {
    seq(0, 1, length.out = resolution)
  })
  mapped = mlr3misc::imap(unit_columns, function(value, id) {
    column = data.table::data.table(value)
    data.table::setnames(column, id)
    param_set$qunif(column)[[1L]]
  })
  Design$new(
    param_set,
    mlr3misc::cross_join(mapped, sorted = FALSE),
    remove_dupl = TRUE
  )$data
}

test_that("output-sensitive grids preserve legacy order after axis collapse", {
  param_set = ps(
    first = p_int(0L, 1L),
    second = p_int(-1L, 1L),
    flag = p_lgl()
  )
  resolutions = c(second = 11L, first = 9L, flag = 2L)

  expected = grid_output_legacy_reference(param_set, resolutions)
  observed = generate_design_grid(
    param_set,
    param_resolutions = resolutions[c("second", "first")]
  )$data

  expect_identical(observed, expected)
  expect_identical(nrow(observed), 12L)
})

test_that("nominally enormous integer grids materialize only realized rows", {
  domains = setNames(
    rep(list(p_int(0L, 1L)), 6L),
    paste0("x", seq_len(6L))
  )
  param_set = ParamSet$new(domains)

  # The nominal product is 100^6, well beyond R's data-frame row limit, while
  # integer rounding leaves only two values on every realized axis.
  observed = generate_design_grid(
    param_set,
    resolution = 100L,
    upper_limit = 64L
  )$data
  expected = mlr3misc::cross_join(
    setNames(rep(list(0:1), 6L), names(domains)),
    sorted = FALSE
  )

  expect_identical(observed, expected)
  expect_identical(nrow(observed), 64L)
  expect_error(
    generate_design_grid(param_set, resolution = 100L, upper_limit = 63L),
    "upper_limit"
  )
})

test_that("fixed and dormant values collapse before dependency expansion", {
  fixed = ps(
    a = p_dbl(0, 1),
    b = p_int(0L, 9L),
    c = p_fct(c("left", "right")),
    d = p_lgl()
  )
  fixed$values = list(a = 0.25, b = 4L, c = "right", d = TRUE)
  fixed_grid = generate_design_grid(
    fixed,
    resolution = 1000L,
    upper_limit = 1L
  )$data
  expect_identical(
    fixed_grid,
    data.table::data.table(
      a = 0.25,
      b = 4L,
      c = "right",
      d = TRUE
    )
  )

  dormant = ps(
    gate = p_lgl(),
    value = p_int(1L, 3L, depends = gate == TRUE)
  )
  dormant$values = list(value = 2L)
  dormant_grid = generate_design_grid(
    dormant,
    resolution = 100L,
    upper_limit = 2L
  )$data
  expect_identical(
    dormant_grid,
    data.table::data.table(
      gate = c(TRUE, FALSE),
      value = c(2L, NA_integer_)
    )
  )
})

test_that("fixed special values retain exact identity in list columns", {
  s4_special = asS4(0.5)
  param_set = ps(
    cross_type = p_int(0L, 2L, special_vals = list("special")),
    null = p_int(0L, 2L, special_vals = list(NULL)),
    object = p_dbl(0, 1, special_vals = list(s4_special))
  )
  param_set$values = list(
    cross_type = "special",
    null = NULL,
    object = s4_special
  )

  design = generate_design_grid(
    param_set,
    resolution = 100L,
    upper_limit = 1L
  )
  expect_identical(dim(design$data), c(1L, 3L))
  expect_identical(
    vapply(design$data, typeof, character(1L)),
    c(cross_type = "list", null = "list", object = "list")
  )
  expect_identical(design$data$cross_type[[1L]], "special")
  expect_null(design$data$null[[1L]])
  expect_identical(design$data$object[[1L]], s4_special)

  point = design$transpose()[[1L]]
  expect_identical(names(point), c("cross_type", "null", "object"))
  expect_identical(point$cross_type, "special")
  expect_null(point$null)
  expect_identical(point$object, s4_special)

  dormant = ps(
    gate = p_lgl(),
    value = p_int(
      0L,
      2L,
      special_vals = list("special"),
      depends = gate == TRUE
    )
  )
  dormant$values = list(value = "special")
  dormant_design = generate_design_grid(dormant, resolution = 100L)
  expect_identical(dormant_design$data$gate, c(TRUE, FALSE))
  expect_identical(dormant_design$data$value, list("special", NA))
  expect_identical(
    dormant_design$transpose(),
    list(
      list(gate = TRUE, value = "special"),
      list(gate = FALSE)
    )
  )

  special_parent = ps(
    parent = p_int(0L, 2L, special_vals = list("special")),
    child = p_int(0L, 2L, depends = parent == "special")
  )
  special_parent$values = list(parent = "special")
  special_parent_design = generate_design_grid(
    special_parent,
    resolution = 10L,
    upper_limit = 3L
  )
  expect_identical(
    special_parent_design$data$parent,
    rep(list("special"), 3L)
  )
  expect_identical(special_parent_design$data$child, 0:2)

  # The complete-row Design vector kernel consumes the same scalar list leaf
  # with the same built-in Condition semantics.
  replayed = Design$new(
    special_parent,
    data.table::copy(special_parent_design$data),
    remove_dupl = FALSE
  )
  expect_identical(
    as.list(replayed$data),
    as.list(special_parent_design$data)
  )
})

test_that("a fixed TuneToken is not materialized as a concrete grid value", {
  param_set = ps(value = p_int(0L, 2L))
  param_set$values = list(value = to_tune(0L, 2L))

  expect_error(
    generate_design_grid(param_set, resolution = 3L),
    "grid.*TuneToken|TuneToken.*grid",
    ignore.case = TRUE
  )
})

test_that("Design dependency masking skips TuneToken parent and child edges", {
  param_set = ps(
    parent = p_lgl(),
    child = p_int(0L, 1L, depends = parent == TRUE)
  )
  token = to_tune(0L, 1L)

  token_parent = Design$new(
    param_set,
    data.table::data.table(parent = list(token), child = 1L),
    remove_dupl = FALSE
  )$data
  expect_identical(token_parent$parent[[1L]], token)
  expect_identical(token_parent$child, 1L)

  token_child = Design$new(
    param_set,
    data.table::data.table(parent = FALSE, child = list(token)),
    remove_dupl = FALSE
  )$data
  expect_identical(token_child$parent, FALSE)
  expect_identical(token_child$child[[1L]], token)
})

test_that("dependency pruning preserves first raw occurrence order", {
  param_set = ps(
    a = p_int(1L, 2L),
    b = p_int(1L, 2L),
    c = p_int(1L, 2L)
  )
  param_set$add_dep("a", "c", CondEqual(1L))

  observed = generate_design_grid(param_set, resolution = 2L)$data
  expect_identical(
    observed,
    data.table::data.table(
      a = c(1L, NA_integer_, 1L, NA_integer_, 2L, 2L),
      b = c(1L, 1L, 2L, 2L, 1L, 2L),
      c = c(1L, 2L, 1L, 2L, 1L, 1L)
    )
  )

  chain_domains = setNames(
    rep(list(p_int(1L, 10L)), 8L),
    paste0("x", seq_len(8L))
  )
  chain = ParamSet$new(chain_domains)
  for (index in 2:8) {
    chain$add_dep(
      paste0("x", index),
      paste0("x", index - 1L),
      CondEqual(1L)
    )
  }
  chain_grid = generate_design_grid(
    chain,
    resolution = 10L,
    upper_limit = 73L
  )
  expect_identical(nrow(chain_grid$data), 73L)
  expect_true(all(mlr3misc::map_lgl(chain_grid$transpose(), chain$test)))
  expect_error(
    generate_design_grid(chain, resolution = 10L, upper_limit = 72L),
    "upper_limit"
  )

  cyclic = ps(left = p_lgl(), right = p_lgl())
  cyclic$add_dep("left", "right", CondEqual(TRUE))
  cyclic$add_dep("right", "left", CondEqual(TRUE))
  expect_error(
    generate_design_grid(cyclic, resolution = 2L),
    "cycle"
  )
  expect_error(
    generate_design_grid(cyclic, resolution = 0L, upper_limit = 0L),
    "cycle"
  )
})

test_that("grid and Design treat admitted infeasible dependencies as inactive", {
  incompatible = ps(
    parent = p_int(0L, 1L),
    child = p_int(0L, 1L)
  )
  incompatible$deps = data.table::data.table(
    id = "child",
    on = "parent",
    cond = list(CondEqual("not-an-integer"))
  )
  incompatible_input = data.table::data.table(
    parent = c(0L, 0L, 1L, 1L),
    child = c(0L, 1L, 0L, 1L)
  )
  expected_incompatible = data.table::data.table(
    parent = 0:1,
    child = c(NA_integer_, NA_integer_)
  )
  expect_identical(
    Design$new(
      incompatible,
      incompatible_input,
      remove_dupl = TRUE
    )$data,
    expected_incompatible
  )
  expect_identical(
    generate_design_grid(incompatible, resolution = 2L)$data,
    expected_incompatible
  )

  dangling = ps(child = p_int(0L, 1L))
  dangling$deps = data.table::data.table(
    id = "child",
    on = "ghost",
    cond = list(CondEqual(TRUE))
  )
  expected_dangling = data.table::data.table(child = NA_integer_)
  expect_identical(
    Design$new(
      dangling,
      data.table::data.table(child = 0:1),
      remove_dupl = TRUE
    )$data,
    expected_dangling
  )
  expect_identical(
    generate_design_grid(dangling, resolution = 2L)$data,
    expected_dangling
  )

  original = ps(
    parent = p_lgl(),
    child = p_int(0L, 1L, depends = parent == TRUE)
  )
  subset = original$subset(
    "child",
    allow_dangling_dependencies = TRUE
  )
  expect_identical(
    generate_design_grid(subset, resolution = 2L)$data,
    expected_dangling
  )
})

test_that("output-sensitive traversal matches the nominal specification", {
  set.seed(20260726L)
  for (iteration in seq_len(24L)) {
    param_set = ps(
      a = p_int(0L, 2L),
      b = p_int(0L, 1L),
      c = p_dbl(0, 1),
      d = p_lgl(),
      e = p_fct(c("x", "y"))
    )
    if (sample(c(FALSE, TRUE), 1L)) {
      param_set$add_dep("b", "a", CondEqual(sample(0:2, 1L)))
    }
    if (sample(c(FALSE, TRUE), 1L)) {
      param_set$add_dep("c", "b", CondEqual(sample(0:1, 1L)))
    }
    if (sample(c(FALSE, TRUE), 1L)) {
      param_set$add_dep("d", "a", CondAnyOf(sample(0:2, 2L)))
    }
    if (sample(c(FALSE, TRUE), 1L)) {
      param_set$add_dep("d", "b", CondEqual(sample(0:1, 1L)))
    }
    if (sample(c(FALSE, TRUE), 1L)) {
      param_set$add_dep("e", "d", CondEqual(sample(c(FALSE, TRUE), 1L)))
    }

    fixed_candidates = list(
      a = sample(0:2, 1L),
      b = sample(0:1, 1L),
      c = sample(c(0, 0.5, 1), 1L),
      d = sample(c(FALSE, TRUE), 1L),
      e = sample(c("x", "y"), 1L)
    )
    fixed_ids = sample(
      c(names(fixed_candidates), NA_character_),
      sample(0:3, 1L)
    )
    fixed_ids = fixed_ids[!is.na(fixed_ids)]
    param_set$values = fixed_candidates[fixed_ids]

    numeric_resolutions = c(
      a = sample(3:6, 1L),
      b = sample(3:6, 1L),
      c = sample(2:5, 1L)
    )
    numeric_resolutions = numeric_resolutions[
      sample(names(numeric_resolutions))
    ]
    resolutions = c(numeric_resolutions, d = 2L, e = 2L)
    expected = grid_output_legacy_reference(param_set, resolutions)
    observed = generate_design_grid(
      param_set,
      param_resolutions = numeric_resolutions
    )$data

    expect_identical(
      observed,
      expected,
      info = sprintf("randomized nominal comparison %d", iteration)
    )
  }
})

test_that("output-sensitive grids consume Collection and Shadow graphs", {
  left = ps(
    gate = p_lgl(),
    value = p_int(1L, 2L, depends = gate == TRUE)
  )
  left$values = list(value = 2L)
  right = ps(choice = p_fct(c("a", "b")))
  collection = ParamSetCollection$new(list(left = left, right = right))
  collection$add_dep(
    "right.choice",
    "left.gate",
    CondEqual(TRUE)
  )
  collection_resolutions = c(
    left.gate = 2L,
    left.value = 2L,
    right.choice = 2L
  )
  expect_identical(
    generate_design_grid(collection, resolution = 2L)$data,
    grid_output_legacy_reference(collection, collection_resolutions)
  )

  shared = ps(
    gate = p_lgl(),
    value = p_int(1L, 2L, depends = gate == TRUE)
  )
  shared$values = list(value = 2L)
  shared_collection = ParamSetCollection$new(list(
    first = shared,
    second = shared
  ))
  shared_resolutions = c(
    first.gate = 2L,
    first.value = 2L,
    second.gate = 2L,
    second.value = 2L
  )
  expect_identical(
    generate_design_grid(shared_collection, resolution = 2L)$data,
    grid_output_legacy_reference(
      shared_collection,
      shared_resolutions
    )
  )

  origin = ps(
    hidden = p_int(0L, 9L),
    gate = p_lgl(),
    value = p_int(1L, 2L, depends = gate == TRUE)
  )
  origin$values = list(hidden = 7L, value = 2L)
  shadow = ParamSetShadow$new(origin, "hidden")
  origin$values = list(hidden = 8L, value = 1L)
  shadow_resolutions = c(gate = 2L, value = 2L)
  expect_identical(
    generate_design_grid(shadow, resolution = 2L)$data,
    grid_output_legacy_reference(shadow, shadow_resolutions)
  )

  collection_origin = ParamSetCollection$new(list(unit = origin))
  collection_shadow = ParamSetShadow$new(
    collection_origin,
    "unit.hidden"
  )
  collection_shadow_resolutions = c(
    unit.gate = 2L,
    unit.value = 2L
  )
  expect_identical(
    generate_design_grid(collection_shadow, resolution = 2L)$data,
    grid_output_legacy_reference(
      collection_shadow,
      collection_shadow_resolutions
    )
  )
})

test_that("zero axes short-circuit large later axes and respect a zero limit", {
  param_set = ps(
    empty = p_fct(character()),
    first = p_dbl(0, 1),
    second = p_dbl(0, 1)
  )
  observed = generate_design_grid(
    param_set,
    resolution = 50000L,
    upper_limit = 0L
  )$data

  expect_identical(dim(observed), c(0L, 3L))
  expect_identical(names(observed), c("empty", "first", "second"))
  expect_identical(vapply(observed, typeof, character(1L)), c(
    empty = "character",
    first = "double",
    second = "double"
  ))
})

test_that("legal scratch-like IDs and realized upper limits are supported", {
  param_set = ParamSet$new(list(
    .count = p_int(1L, 3L),
    gate = p_lgl(),
    value = p_int(1L, 3L, depends = gate == TRUE)
  ))
  resolutions = c(.count = 10L, gate = 2L, value = 10L)

  expected = grid_output_legacy_reference(param_set, resolutions)
  observed = generate_design_grid(
    param_set,
    resolution = 10L,
    upper_limit = nrow(expected)
  )$data
  expect_identical(observed, expected)
  expect_true(".count" %in% names(observed))

  realized = ps(
    x = p_int(1L, 3L),
    y = p_fct(c("a", "b"))
  )
  expect_identical(
    nrow(generate_design_grid(
      realized,
      resolution = 10L,
      upper_limit = 6L
    )$data),
    6L
  )
  expect_error(
    generate_design_grid(realized, resolution = 10L, upper_limit = 5L),
    "upper_limit"
  )
})

test_that("output-sensitive mapping emits each axis warning only once", {
  param_set = ps(value = p_int(0L, Inf))
  observed_warnings = character()
  observed = withCallingHandlers(
    generate_design_grid(
      param_set,
      resolution = 100L,
      upper_limit = 2L
    )$data,
    warning = function(condition) {
      observed_warnings <<- c(observed_warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(observed$value, c(0L, NA_integer_))
  expect_identical(length(observed_warnings), 1L)
  expect_match(
    observed_warnings,
    "NAs introduced by coercion to integer range"
  )
})
