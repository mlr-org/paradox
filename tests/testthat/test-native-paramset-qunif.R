native_paramset_qunif_symbol = function() {
  get("C_param_set_qunif_builtin", envir = asNamespace("paradox"))
}

native_paramset_qunif_params = function(param_set) {
  param_set$.__enclos_env__$private$.params
}

native_paramset_qunif_space = function() {
  ps(
    double = p_dbl(-10, 10),
    integer = p_int(-2, 2),
    factor = p_fct(c("slow", "fast", "turbo")),
    logical = p_lgl()
  )
}

test_that("native ParamSet qunif is registered with a forced symbol", {
  symbol = native_paramset_qunif_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call(
      "param_set_qunif_builtin",
      list(),
      matrix(0, nrow = 1L, dimnames = list(NULL, "x")),
      PACKAGE = "paradox"
    ),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("bulk qunif preserves requested order, storage, and endpoints", {
  param_set = native_paramset_qunif_space()
  units = cbind(
    logical = c(0, 0.499, 0.5, 1),
    factor = c(0, 0.499, 0.5, 1),
    double = c(0, 0.499, 0.5, 1),
    integer = c(0, 0.499, 0.5, 1)
  )

  native = .Call(
    native_paramset_qunif_symbol(),
    native_paramset_qunif_params(param_set),
    units
  )
  expected = list(
    logical = c(TRUE, TRUE, FALSE, FALSE),
    factor = c("slow", "fast", "fast", "turbo"),
    double = pmax(
      pmin(units[, "double"] * 10 - (units[, "double"] - 1) * -10, 10),
      -10
    ),
    integer = c(-2L, 0L, 0L, 2L)
  )

  expect_identical(as.list(native), expected)
  expect_identical(names(native), colnames(units))
  expect_identical(class(native), c("data.table", "data.frame"))
  expect_identical(
    names(attributes(native)),
    c("row.names", "class", "names", ".internal.selfref")
  )
  expect_identical(.row_names_info(native, type = 0L), c(NA_integer_, -4L))
  expect_identical(data.table:::selfrefok(native, verbose = FALSE), 1L)
  expect_identical(param_set$qunif(units), native)
})

test_that("bulk qunif results are immediately safe for by-reference use", {
  param_set = native_paramset_qunif_space()
  units = matrix(
    c(0, 0.5, 1, 0.25),
    ncol = 2L,
    dimnames = list(NULL, c("double", "integer"))
  )
  input_names = colnames(units)
  result = param_set$qunif(units)

  expect_identical(data.table:::selfrefok(result, verbose = FALSE), 1L)
  expect_warning(
    data.table::set(result, j = "added", value = seq_len(nrow(result))),
    NA
  )
  expect_identical(result$added, seq_len(nrow(result)))
  expect_identical(colnames(units), input_names)

  restored = unserialize(serialize(param_set$qunif(units), NULL))
  expect_identical(data.table:::selfrefok(restored, verbose = FALSE), -1L)
  expect_warning(
    data.table::set(restored, i = 1L, j = "double", value = 0),
    NA
  )
  expect_identical(data.table:::selfrefok(restored, verbose = FALSE), -1L)
  expect_warning(
    data.table::set(restored, j = "after_restore", value = seq_len(nrow(restored))),
    NA
  )
  expect_identical(restored$after_restore, seq_len(nrow(restored)))
  expect_identical(data.table:::selfrefok(restored, verbose = FALSE), 1L)
})

test_that("bulk qunif agrees with each built-in Domain across rows", {
  param_set = native_paramset_qunif_space()
  set.seed(20260713)
  units = matrix(
    runif(257L * 4L),
    ncol = 4L,
    dimnames = list(NULL, c("factor", "integer", "logical", "double"))
  )

  result = param_set$qunif(units)
  expect_identical(names(result), colnames(units))
  for (id in colnames(units)) {
    expect_identical(
      result[[id]],
      domain_qunif(param_set$get_domain(id), units[, id]),
      info = id
    )
  }
})

test_that("integer matrices, data frames, subsets, and zero rows are supported", {
  param_set = native_paramset_qunif_space()
  integer_units = matrix(
    c(0L, 1L, 1L, 0L),
    nrow = 2L,
    dimnames = list(c("first", "second"), c("integer", "logical"))
  )
  integer_result = param_set$qunif(integer_units)
  expect_identical(integer_result$integer, c(-2L, 2L))
  expect_identical(integer_result$logical, c(FALSE, TRUE))
  expect_identical(row.names(integer_result), c("1", "2"))

  frame = data.frame(
    factor = c(0, 1),
    double = c(0.25, 0.75),
    row.names = c("discarded-a", "discarded-b")
  )
  frame_result = param_set$qunif(frame)
  expect_identical(frame_result$factor, c("slow", "turbo"))
  expect_identical(frame_result$double, c(-5, 5))
  expect_identical(row.names(frame_result), c("1", "2"))
  expect_null(.Call(
    native_paramset_qunif_symbol(),
    native_paramset_qunif_params(param_set),
    frame
  ))

  empty = matrix(
    numeric(),
    nrow = 0L,
    ncol = 2L,
    dimnames = list(NULL, c("logical", "integer"))
  )
  empty_result = param_set$qunif(empty)
  expect_identical(empty_result$logical, logical())
  expect_identical(empty_result$integer, integer())
  expect_identical(.row_names_info(empty_result, type = 0L), integer())
})

test_that("result names never alias matrix dimnames", {
  param_set = native_paramset_qunif_space()
  source_names = c("double", "integer")
  original_names = source_names
  units = matrix(
    c(0.25, 0.75, 0.5, 1),
    ncol = 2L,
    dimnames = list(NULL, source_names)
  )

  result = param_set$qunif(units)
  data.table::setnames(result, c("mapped_double", "mapped_integer"))

  expect_identical(names(result), c("mapped_double", "mapped_integer"))
  expect_identical(colnames(units), original_names)
  expect_identical(source_names, original_names)
})

test_that("ParamSetCollection uses the same public bulk contract", {
  collection = ParamSetCollection$new(list(
    left = ps(x = p_dbl(2, 6)),
    right = ps(i = p_int(10, 12), flag = p_lgl())
  ))
  units = matrix(
    c(0, 1, 0.5, 0.25, 0.75, 1),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(NULL, c("right.flag", "left.x", "right.i"))
  )

  result = collection$qunif(units)
  expect_identical(names(result), colnames(units))
  expect_identical(result$right.flag, c(TRUE, TRUE))
  expect_identical(result$left.x, c(6, 5))
  expect_identical(result$right.i, c(11L, 12L))
})

test_that("custom and utility Domains retain established dispatch", {
  class_name = "ParamNativeParamSetQunifCustom"
  calls = 0L
  registerS3method(
    "domain_qunif",
    class_name,
    function(param, x) {
      calls <<- calls + 1L
      x + 10
    },
    envir = asNamespace("paradox")
  )
  make_custom_domain = function() {
    paradox:::Domain(
      cls = class_name,
      grouping = class_name,
      storage_type = "numeric"
    )
  }
  custom = make_custom_domain()
  param_set = ParamSet$new(list(
    custom = custom,
    double = p_dbl(0, 2)
  ))
  params = native_paramset_qunif_params(param_set)
  units = cbind(custom = c(0, 1), double = c(0.25, 0.75))

  expect_null(.Call(native_paramset_qunif_symbol(), params, units))
  result = param_set$qunif(units)
  expect_identical(result$custom, c(10, 11))
  expect_identical(result$double, c(0.5, 1.5))
  expect_identical(calls, 1L)

  # An extension elsewhere in a ParamSet does not disable a requested built-in
  # slice: only selected rows need native mappings.
  built_in = units[, "double", drop = FALSE]
  expect_s3_class(
    .Call(native_paramset_qunif_symbol(), params, built_in),
    "data.table"
  )
  expect_identical(param_set$qunif(built_in)$double, c(0.5, 1.5))
  expect_identical(calls, 1L)

  utility = ps(value = p_uty())
  utility_units = matrix(
    0.5,
    nrow = 1L,
    dimnames = list(NULL, "value")
  )
  expect_null(.Call(
    native_paramset_qunif_symbol(),
    native_paramset_qunif_params(utility),
    utility_units
  ))
  expect_error(utility$qunif(utility_units), "undefined")
})

test_that("public validation diagnostics still precede native dispatch", {
  param_set = native_paramset_qunif_space()
  unnamed = matrix(0.5, nrow = 1L)
  expect_error(param_set$qunif(unnamed), "Must have names", fixed = TRUE)

  unknown = matrix(
    0.5,
    nrow = 1L,
    dimnames = list(NULL, "unknown")
  )
  expect_error(param_set$qunif(unknown), "subset of")

  duplicated = matrix(
    c(0, 1),
    nrow = 1L,
    dimnames = list(NULL, c("double", "double"))
  )
  expect_error(param_set$qunif(duplicated), "unique")

  missing = matrix(
    NA_real_,
    nrow = 1L,
    dimnames = list(NULL, "double")
  )
  expect_error(param_set$qunif(missing), "missing")

  outside = matrix(
    1.01,
    nrow = 1L,
    dimnames = list(NULL, "double")
  )
  expect_error(param_set$qunif(outside), "Assertion on 'x' failed", fixed = TRUE)
})

test_that("direct native qunif rejects unsupported input without mutation", {
  symbol = native_paramset_qunif_symbol()
  param_set = native_paramset_qunif_space()
  params = native_paramset_qunif_params(param_set)
  valid = matrix(
    c(0.25, 0.75),
    ncol = 1L,
    dimnames = list(NULL, "double")
  )
  before = valid

  invalid_inputs = list(
    unname(valid),
    structure(valid, class = "native_qunif_matrix"),
    matrix(numeric(), nrow = 1L, ncol = 0L),
    matrix(NA_real_, nrow = 1L, dimnames = list(NULL, "double")),
    matrix(NaN, nrow = 1L, dimnames = list(NULL, "double")),
    matrix(-0.01, nrow = 1L, dimnames = list(NULL, "double")),
    matrix(1.01, nrow = 1L, dimnames = list(NULL, "double")),
    matrix(0.5, nrow = 1L, dimnames = list(NULL, "unknown")),
    matrix(
      c(0.25, 0.75),
      nrow = 1L,
      dimnames = list(NULL, c("double", "double"))
    )
  )
  for (input in invalid_inputs) {
    expect_null(.Call(symbol, params, input), info = deparse(input))
  }
  expect_null(.Call(symbol, params, as.data.frame(valid)))
  expect_null(.Call(symbol, unclass(params), valid))
  expect_identical(valid, before)
})

test_that("direct native qunif fails closed on corrupt ParamSet storage", {
  symbol = native_paramset_qunif_symbol()
  param_set = native_paramset_qunif_space()
  params = native_paramset_qunif_params(param_set)
  units = matrix(
    0.5,
    nrow = 1L,
    ncol = length(params$id),
    dimnames = list(NULL, params$id)
  )

  corruptions = list()
  column_order = c(2L, 1L, seq.int(3L, length(params)))
  corruptions[[1L]] = params[, column_order, with = FALSE]

  corruptions[[2L]] = data.table::copy(params)
  data.table::set(
    corruptions[[2L]],
    i = 2L,
    j = "id",
    value = corruptions[[2L]]$id[[1L]]
  )

  corruptions[[3L]] = data.table::copy(params)
  data.table::set(corruptions[[3L]], i = 1L, j = "grouping", value = NA_character_)

  corruptions[[4L]] = data.table::copy(params)
  double_row = match("double", corruptions[[4L]]$id)
  data.table::set(corruptions[[4L]], i = double_row, j = "lower", value = NA_real_)

  corruptions[[5L]] = data.table::copy(params)
  factor_row = match("factor", corruptions[[5L]]$id)
  data.table::set(
    corruptions[[5L]],
    i = factor_row,
    j = "levels",
    value = list(c("slow", NA_character_))
  )

  for (corrupt in corruptions) {
    expect_null(.Call(symbol, corrupt, units))
  }
})

test_that("bulk qunif remains rooted under adversarial collection", {
  skip_on_cran()

  symbol = native_paramset_qunif_symbol()
  param_set = native_paramset_qunif_space()
  units = matrix(
    c(0, 0.25, 0.5, 0.75, 1, 0.125, 0.875, 0.5),
    ncol = 4L,
    dimnames = list(NULL, c("factor", "logical", "double", "integer"))
  )

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  result = .Call(
    symbol,
    native_paramset_qunif_params(param_set),
    units
  )
  gctorture(previous)

  expect_identical(names(result), colnames(units))
  expect_identical(unname(lengths(result)), rep(2L, 4L))
  expect_identical(result$factor, c("slow", "slow"))
  expect_identical(result$logical, c(FALSE, FALSE))
  expect_identical(result$double, c(10, -7.5))
  expect_identical(result$integer, c(2L, 0L))
})

test_that("bulk qunif accumulates work across many short columns", {
  size = 32769L
  ids = sprintf("logical_%05d", seq_len(size))
  template = native_paramset_qunif_params(ps(x = p_lgl()))
  params = template[rep.int(1L, size)]
  data.table::set(params, j = "id", value = ids)
  units = matrix(
    0.5,
    nrow = 1L,
    ncol = size,
    dimnames = list(NULL, ids)
  )

  result = .Call(native_paramset_qunif_symbol(), params, units)
  expect_s3_class(result, "data.table")
  expect_identical(names(result), ids)
  expect_identical(unname(lengths(result)), rep.int(1L, size))
  expect_true(all(vapply(result, identical, logical(1L), FALSE)))
})
