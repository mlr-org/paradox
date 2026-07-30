native_grid_symbol = function() {
  get("C_generate_design_grid_builtin", envir = asNamespace("paradox"))
}

native_grid_private = function(param_set) {
  param_set$.__enclos_env__$private
}

native_grid_call = function(param_set, resolutions, upper_limit = NULL) {
  numeric_ids = param_set$ids()[param_set$is_number]
  param_resolutions = resolutions[
    match(numeric_ids, names(resolutions), nomatch = 0L)
  ]
  .Call(
    native_grid_symbol(),
    native_grid_private(param_set),
    param_set,
    list(
      resolution = NULL,
      param_resolutions = param_resolutions
    ),
    upper_limit
  )[[1L]]
}

native_grid_control_call = function(param_set, resolution = NULL,
    param_resolutions = NULL, upper_limit = NULL) {
  .Call(
    native_grid_symbol(),
    native_grid_private(param_set),
    param_set,
    list(
      resolution = resolution,
      param_resolutions = param_resolutions
    ),
    upper_limit
  )[[1L]]
}

native_grid_reference = function(param_set, resolutions) {
  resolutions = resolutions[param_set$ids()]
  unit_columns = lapply(resolutions, function(resolution) {
    seq(0, 1, length.out = resolution)
  })
  mapped = mlr3misc::imap(unit_columns, function(value, id) {
    column = data.table::data.table(value)
    data.table::setnames(column, id)
    param_set$qunif(column)[[1L]]
  })
  mlr3misc::cross_join(mapped, sorted = FALSE)
}

native_grid_final_reference = function(param_set, resolutions) {
  Design$new(
    param_set,
    native_grid_reference(param_set, resolutions),
    remove_dupl = TRUE
  )$data
}

native_grid_visible_attributes = function(table) {
  result = attributes(table)
  result$.internal.selfref = NULL
  result[sort(names(result), method = "radix")]
}

native_grid_numeric_bytes = function(table) {
  lapply(Filter(is.double, table), function(column) {
    writeBin(column, raw(), size = 8L, endian = .Platform$endian)
  })
}

test_that("native grid generation is registered with a forced symbol", {
  symbol = native_grid_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 4L)
  expect_error(
    .Call(
      "generate_design_grid_builtin",
      new.env(parent = emptyenv()),
      NULL,
      c(x = 2L),
      NULL,
      PACKAGE = "paradox"
    ),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("zero-dimensional grids are constructed by the native engine", {
  param_set = ParamSet$new()
  counts = setNames(integer(), character())
  observed = native_grid_call(param_set, counts)
  expect_s3_class(observed, "data.table")
  expect_identical(dim(observed), c(0L, 0L))
  expect_identical(names(observed), character())
  expect_identical(data.table:::selfrefok(observed, verbose = FALSE), 1L)

  design = generate_design_grid(param_set)
  expect_s3_class(design$data, "data.table")
  expect_identical(dim(design$data), c(0L, 0L))
  expect_identical(names(design$data), character())

  # A zero-dimensional public ParamSet cannot acquire dependency rows. Ensure a
  # deliberately forged current capsule cannot use the empty-result path to
  # conceal that impossible graph.
  private = native_grid_private(param_set)
  state = paradox:::param_set_core_state(private)
  dependency_source = ps(
    parent = p_lgl(),
    child = p_int(depends = parent == TRUE)
  )
  state$.deps = paradox:::param_set_core_state(
    native_grid_private(dependency_source)
  )$.deps
  private$.core = .Call(paradox:::C_param_set_core_new, 1L, state)
  expect_error(
    native_grid_call(param_set, counts),
    "unknown child parameter"
  )
})

test_that("grid facades use one canonical native attribute order", {
  param_set = ps(x = p_int(0L, 1L))
  observed = native_grid_call(param_set, c(x = 2L))
  expect_identical(
    names(attributes(observed)),
    c("row.names", "class", "names", ".internal.selfref")
  )
  expect_identical(data.table:::selfrefok(observed, verbose = FALSE), 1L)
})

test_that("one-shot grids preserve randomized resolution order exactly", {
  param_set = ps(
    double = p_dbl(-10, 13),
    integer = p_int(-4, 7),
    factor = p_fct(c("slow", "fast", "turbo")),
    logical = p_lgl()
  )

  set.seed(20260716)
  for (iteration in seq_len(40L)) {
    iteration_info = sprintf("iteration %d", iteration)
    counts = c(
      double = sample.int(6L, 1L),
      integer = sample.int(7L, 1L),
      factor = 3,
      logical = 2
    )
    counts = counts[sample(names(counts))]
    observed = native_grid_call(param_set, counts)
    expected = native_grid_reference(param_set, counts)

    expect_identical(as.list(observed), as.list(expected), info = iteration_info)
    expect_identical(names(observed), param_set$ids(), info = iteration_info)
    expect_identical(
      vapply(observed, typeof, character(1L)),
      vapply(expected, typeof, character(1L)),
      info = iteration_info
    )
    expect_identical(
      native_grid_numeric_bytes(observed),
      native_grid_numeric_bytes(expected),
      info = iteration_info
    )
    expect_identical(
      native_grid_visible_attributes(observed),
      native_grid_visible_attributes(expected),
      info = iteration_info
    )
    expect_identical(data.table:::selfrefok(observed, verbose = FALSE), 1L)
  }
})

test_that("grid length one and endpoints match seq length.out", {
  param_set = ps(
    unbounded = p_dbl(),
    fixed_double = p_dbl(5, 5),
    integer = p_int(-2, 2),
    fixed_integer = p_int(4, 4),
    factor = p_fct("only"),
    logical = p_lgl()
  )
  zero_counts = c(
    unbounded = 0,
    fixed_double = 2,
    integer = 2,
    fixed_integer = 1,
    factor = 1,
    logical = 2
  )
  zero = native_grid_call(param_set, zero_counts)
  expect_identical(dim(zero), c(0L, length(zero_counts)))
  expect_identical(names(zero), names(zero_counts))

  one_counts = c(
    fixed_integer = 1L,
    unbounded = 1L,
    factor = 1L,
    logical = 2L,
    integer = 1L,
    fixed_double = 1L
  )
  one = native_grid_call(param_set, one_counts)
  one_reference = native_grid_final_reference(param_set, one_counts)
  expect_identical(as.list(one), as.list(one_reference))
  expect_identical(one$unbounded, c(-Inf, -Inf))
  expect_identical(one$integer, c(-2L, -2L))

  edge_counts = c(
    integer = 7,
    logical = 2,
    fixed_double = 3,
    factor = 1,
    unbounded = 3,
    fixed_integer = 5
  )
  edges = native_grid_call(param_set, edge_counts)
  edge_reference = native_grid_final_reference(param_set, edge_counts)
  expect_identical(as.list(edges), as.list(edge_reference))
  expect_identical(
    native_grid_numeric_bytes(edges),
    native_grid_numeric_bytes(edge_reference)
  )
  expect_true(any(is.nan(edges$unbounded)))
  expect_identical(unique(edges$fixed_double), 5)
  expect_identical(unique(edges$fixed_integer), 4L)
  expect_identical(range(edges$integer), c(-2L, 2L))
})

test_that("zero-axis grids return an owned typed empty design", {
  param_set = ps(
    empty = p_dbl(0, 1),
    infinite_integer = p_int(0, Inf)
  )
  expect_no_warning(
    {
      observed = generate_design_grid(
        param_set,
        resolution = 0L,
        param_resolutions = c(infinite_integer = 2L)
      )
    }
  )
  expect_identical(dim(observed$data), c(0L, 2L))
  expect_identical(names(observed$data), c("empty", "infinite_integer"))
  expect_identical(
    vapply(observed$data, typeof, character(1L)),
    c(empty = "double", infinite_integer = "integer")
  )

  per_axis = generate_design_grid(
    param_set,
    resolution = 2L,
    param_resolutions = c(empty = 0L, infinite_integer = 2L)
  )
  expect_identical(dim(per_axis$data), c(0L, 2L))
  expect_identical(
    vapply(per_axis$data, typeof, character(1L)),
    c(empty = "double", infinite_integer = "integer")
  )

  # A fixed stored value does not turn a nominally empty axis into a singleton.
  param_set$values = list(empty = 0.25)
  expect_no_warning(
    {
      fixed_zero = generate_design_grid(
        param_set,
        resolution = 0L,
        param_resolutions = c(infinite_integer = 50000L),
        upper_limit = 0L
      )
    }
  )
  expect_identical(dim(fixed_zero$data), c(0L, 2L))
  expect_identical(vapply(fixed_zero$data, typeof, character(1L)), c(
    empty = "double",
    infinite_integer = "integer"
  ))
})

test_that("zero-level factors produce typed empty categorical and mixed grids", {
  categorical = ps(choice = p_fct(character()))
  categorical_design = generate_design_grid(categorical)
  expect_identical(dim(categorical_design$data), c(0L, 1L))
  expect_identical(names(categorical_design$data), "choice")
  expect_identical(categorical_design$data$choice, character())

  mixed = ps(
    choice = p_fct(character()),
    number = p_dbl(0, 1),
    flag = p_lgl()
  )
  counts = c(choice = 0L, number = 3L, flag = 2L)
  direct = native_grid_call(mixed, counts)
  expect_identical(dim(direct), c(0L, 3L))
  expect_identical(names(direct), names(counts))
  expect_identical(vapply(direct, typeof, character(1L)), c(
    choice = "character",
    number = "double",
    flag = "logical"
  ))

  mixed_design = generate_design_grid(mixed, resolution = 3L)
  expect_identical(dim(mixed_design$data), c(0L, 3L))
  expect_identical(names(mixed_design$data), mixed$ids())
  expect_identical(vapply(mixed_design$data, typeof, character(1L)), c(
    choice = "character",
    number = "double",
    flag = "logical"
  ))
})

test_that("public mixed grids retain values, dependencies, and table facade", {
  param_set = ps(
    x = p_dbl(0, 1),
    enabled = p_lgl(),
    child = p_fct(c("a", "b"))
  )
  param_set$values = list(x = 0.25)
  param_set$add_dep("child", "enabled", CondEqual(TRUE))

  counts = c(x = 4, enabled = 2, child = 2)
  expected = native_grid_call(param_set, counts)
  observed = generate_design_grid(
    param_set,
    param_resolutions = c(x = 4L)
  )

  expect_identical(as.list(observed$data), as.list(expected))
  expect_identical(
    native_grid_visible_attributes(observed$data),
    native_grid_visible_attributes(expected)
  )
  expect_identical(vapply(observed$data, typeof, character(1L)), c(
    x = "double",
    enabled = "logical",
    child = "character"
  ))
  expect_true(all(observed$data$x == 0.25))
  expect_true(all(is.na(observed$data$child[!observed$data$enabled])))
})

test_that("direct grid admission fails closed without allocating huge grids", {
  param_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  valid = c(y = 3, x = 2)
  before = valid

  invalid = list(
    list(unname(valid), "one name per value"),
    list(structure(valid, class = "grid_resolution"), "named numeric vector"),
    list(c(y = NA_real_, x = 2), "non-negative whole numbers"),
    list(c(y = NaN, x = 2), "non-negative whole numbers"),
    list(c(y = Inf, x = 2), "non-negative whole numbers"),
    list(c(y = -1, x = 2), "non-negative whole numbers"),
    list(c(y = 1.5, x = 2), "non-negative whole numbers"),
    list(c(y = 3), "Resolution setting missing"),
    list(c(y = 3, unknown = 2), "must name numerical ParamSet parameters"),
    list(c(y = 3, y = 2), "unique"),
    list(c(y = 50000, x = 50000), "Grid product exceeds")
  )
  for (case in invalid) {
    expect_error(
      native_grid_control_call(
        param_set,
        param_resolutions = case[[1L]]
      ),
      case[[2L]],
      fixed = TRUE,
      info = deparse(case[[1L]])
    )
  }

  mixed = ps(x = p_dbl(0, 1), factor = p_fct(c("a", "b")))
  expect_error(
    native_grid_control_call(
      mixed,
      param_resolutions = c(x = 2, factor = 1)
    ),
    "must name numerical ParamSet parameters",
    fixed = TRUE
  )

  utility = ps(x = p_uty())
  expect_error(
    native_grid_control_call(utility, param_resolutions = c(x = 1)),
    "undefined for ParamUty",
    fixed = TRUE
  )

  expect_error(
    .Call(
      native_grid_symbol(),
      new.env(parent = emptyenv()),
      param_set,
      list(resolution = NULL, param_resolutions = valid),
      NULL
    ),
    "Corrupt ParamSet"
  )
  for (upper_limit in list(-1L, NA_integer_, 1.5, c(1L, 2L), "2")) {
    expect_error(
      native_grid_call(param_set, valid, upper_limit),
      "upper_limit",
      info = deparse(upper_limit)
    )
  }
  expect_identical(valid, before)
})

test_that("infinite integer grids warn once in the native engine", {
  param_set = ps(x = p_int(0, Inf))
  expect_warning(
    {
      direct = native_grid_call(param_set, c(x = 2))
    },
    "NAs introduced by coercion to integer range"
  )
  expect_identical(direct$x, c(0L, NA_integer_))
  expect_warning(
    {
      observed = generate_design_grid(param_set, resolution = 2L)
    },
    "NAs introduced by coercion to integer range"
  )
  expect_identical(observed$data$x, c(0L, NA_integer_))
})
