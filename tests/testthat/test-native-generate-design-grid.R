native_grid_symbol = function() {
  get("C_generate_design_grid_builtin", envir = asNamespace("paradox"))
}

native_grid_params = function(param_set) {
  param_set$.__enclos_env__$private$.params
}

native_grid_reference = function(param_set, resolutions) {
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

native_grid_visible_attributes = function(table) {
  result = attributes(table)
  result$.internal.selfref = NULL
  result
}

native_grid_numeric_bytes = function(table) {
  lapply(Filter(is.double, table), function(column) {
    writeBin(column, raw(), size = 8L, endian = .Platform$endian)
  })
}

test_that("native grid generation is registered with a forced symbol", {
  symbol = native_grid_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call(
      "generate_design_grid_builtin",
      list(),
      c(x = 2L),
      PACKAGE = "paradox"
    ),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("one-shot grids preserve randomized resolution order exactly", {
  param_set = ps(
    double = p_dbl(-10, 13),
    integer = p_int(-4, 7),
    factor = p_fct(c("slow", "fast", "turbo")),
    logical = p_lgl()
  )
  params = native_grid_params(param_set)
  symbol = native_grid_symbol()

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
    observed = .Call(symbol, params, counts)
    expected = native_grid_reference(param_set, counts)

    expect_identical(as.list(observed), as.list(expected), info = iteration_info)
    expect_identical(names(observed), names(counts), info = iteration_info)
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
  params = native_grid_params(param_set)
  symbol = native_grid_symbol()

  zero_counts = c(
    unbounded = 0,
    fixed_double = 2,
    integer = 2,
    fixed_integer = 1,
    factor = 1,
    logical = 2
  )
  expect_null(.Call(symbol, params, zero_counts))

  one_counts = c(
    fixed_integer = 1L,
    unbounded = 1L,
    factor = 1L,
    logical = 2L,
    integer = 1L,
    fixed_double = 1L
  )
  one = .Call(symbol, params, one_counts)
  one_reference = native_grid_reference(param_set, one_counts)
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
  edges = .Call(symbol, params, edge_counts)
  edge_reference = native_grid_reference(param_set, edge_counts)
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

test_that("zero-axis grids retain mapping warnings before the empty join", {
  param_set = ps(
    empty = p_dbl(0, 1),
    infinite_integer = p_int(0, Inf)
  )
  expect_warning(
    {
      observed = generate_design_grid(
        param_set,
        resolution = 0L,
        param_resolutions = c(infinite_integer = 2L)
      )
    },
    "NAs introduced by coercion to integer range"
  )
  expect_identical(dim(observed$data), c(0L, 2L))
  expect_identical(names(observed$data), c("empty", "infinite_integer"))
  expect_identical(
    vapply(observed$data, typeof, character(1L)),
    c(empty = "double", infinite_integer = "integer")
  )
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
  expected = Design$new(
    param_set,
    data.table::copy(native_grid_reference(param_set, counts)),
    remove_dupl = TRUE
  )
  observed = generate_design_grid(
    param_set,
    param_resolutions = c(x = 4L)
  )

  expect_identical(as.list(observed$data), as.list(expected$data))
  expect_identical(attributes(observed$data), attributes(expected$data))
  expect_identical(vapply(observed$data, typeof, character(1L)), c(
    x = "double",
    enabled = "logical",
    child = "character"
  ))
  expect_true(all(observed$data$x == 0.25))
  expect_true(all(is.na(observed$data$child[!observed$data$enabled])))
})

test_that("subclassed and replaced qunif methods retain the R grid path", {
  subclass_calls = 0L
  GridParamSetSubclass = R6::R6Class(
    "GridParamSetSubclass",
    inherit = ParamSet,
    public = list(
      qunif = function(x) {
        subclass_calls <<- subclass_calls + 1L
        super$qunif(x)
      }
    )
  )
  subclass = GridParamSetSubclass$new(list(
    x = p_dbl(0, 1),
    flag = p_lgl()
  ))
  subclass_design = generate_design_grid(subclass, resolution = 3L)
  expect_identical(subclass_calls, 2L)
  expect_s3_class(subclass_design$param_set, "GridParamSetSubclass")
  expect_identical(nrow(subclass_design$data), 6L)

  replaced_calls = 0L
  replaced = ps(x = p_dbl(0, 1), flag = p_lgl())
  inherited_qunif = replaced$qunif
  unlockBinding("qunif", replaced)
  assign(
    "qunif",
    function(x) {
      replaced_calls <<- replaced_calls + 1L
      inherited_qunif(x)
    },
    envir = replaced
  )
  lockBinding("qunif", replaced)

  replaced_design = generate_design_grid(replaced, resolution = 3L)
  expect_identical(replaced_calls, 2L)
  expect_identical(as.list(replaced_design$data), as.list(subclass_design$data))
})

test_that("direct grid admission fails closed without allocating huge grids", {
  symbol = native_grid_symbol()
  param_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  params = native_grid_params(param_set)
  valid = c(y = 3, x = 2)
  before = valid

  invalid = list(
    unname(valid),
    structure(valid, class = "grid_resolution"),
    c(y = NA_real_, x = 2),
    c(y = NaN, x = 2),
    c(y = Inf, x = 2),
    c(y = -1, x = 2),
    c(y = 1.5, x = 2),
    c(y = 3),
    c(y = 3, unknown = 2),
    c(y = 3, y = 2),
    c(y = 50000, x = 50000)
  )
  for (counts in invalid) {
    expect_null(.Call(symbol, params, counts), info = deparse(counts))
  }

  mixed = ps(x = p_dbl(0, 1), factor = p_fct(c("a", "b")))
  expect_null(.Call(
    symbol,
    native_grid_params(mixed),
    c(x = 2, factor = 1)
  ))

  corrupt = data.table::copy(params)
  data.table::set(corrupt, i = 1L, j = "cls", value = "ParamCustom")
  expect_null(.Call(symbol, corrupt, valid))
  expect_identical(valid, before)
})

test_that("unsupported infinite integer grids retain their warning fallback", {
  param_set = ps(x = p_int(0, Inf))
  expect_null(.Call(
    native_grid_symbol(),
    native_grid_params(param_set),
    c(x = 2)
  ))
  expect_warning(
    {
      observed = generate_design_grid(param_set, resolution = 2L)
    },
    "NAs introduced by coercion to integer range"
  )
  expect_identical(observed$data$x, c(0L, NA_integer_))
})
