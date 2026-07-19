native_paramset_qunif_symbol = function() {
  get("C_param_set_qunif_builtin", envir = asNamespace("paradox"))
}

native_paramset_qunif_private = function(param_set) {
  param_set$.__enclos_env__$private
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
  expect_identical(symbol$numParameters, 3L)
  expect_error(
    .Call(
      "param_set_qunif_builtin",
      new.env(parent = emptyenv()),
      new.env(parent = emptyenv()),
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
    native_paramset_qunif_private(param_set),
    param_set,
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
  expect_identical(.Call(
    native_paramset_qunif_symbol(),
    native_paramset_qunif_private(param_set),
    param_set,
    frame
  ), frame_result)

  table = data.table::as.data.table(frame)
  table_result = param_set$qunif(table)
  expect_identical(table_result, frame_result)
  expect_identical(.Call(
    native_paramset_qunif_symbol(),
    native_paramset_qunif_private(param_set),
    param_set,
    table
  ), frame_result)

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

test_that("zero-level factor parameters retain typed zero-row quantiles", {
  param_set = ps(choice = p_fct(character()))
  units = matrix(
    numeric(),
    nrow = 0L,
    ncol = 1L,
    dimnames = list(NULL, "choice")
  )

  result = param_set$qunif(units)
  expect_s3_class(result, "data.table")
  expect_identical(dim(result), c(0L, 1L))
  expect_identical(result$choice, character())
  expect_identical(data.table:::selfrefok(result, verbose = FALSE), 1L)

  expect_error(
    param_set$qunif(matrix(
      0.5,
      nrow = 1L,
      dimnames = list(NULL, "choice")
    )),
    "Cannot map quantiles for a factor parameter with no levels",
    fixed = TRUE
  )
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

test_that("utility parameters deterministically reject quantile mapping", {
  utility = ps(value = p_uty())
  utility_units = matrix(
    0.5,
    nrow = 1L,
    dimnames = list(NULL, "value")
  )
  expect_error(.Call(
    native_paramset_qunif_symbol(),
    native_paramset_qunif_private(utility),
    utility,
    utility_units
  ), "undefined for ParamUty", fixed = TRUE)
  expect_error(utility$qunif(utility_units), "undefined for ParamUty", fixed = TRUE)
})

test_that("integer range failures warn once and yield missing values", {
  param_set = ps(x = p_int(0, Inf), y = p_int(-Inf, 2))
  units = matrix(
    c(0, 1, 0, 1),
    nrow = 2L,
    dimnames = list(NULL, c("x", "y"))
  )

  expect_warning(
    {
      result = param_set$qunif(units)
    },
    "NAs introduced by coercion to integer range"
  )
  expect_identical(result$x, c(0L, NA_integer_))
  expect_identical(result$y, c(NA_integer_, 2L))
})

test_that("native qunif owns public type, range, and name diagnostics", {
  param_set = native_paramset_qunif_space()
  unnamed = matrix(0.5, nrow = 1L)
  expect_error(param_set$qunif(unnamed), "one column name", fixed = TRUE)

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
  expect_error(param_set$qunif(outside), "between zero and one", fixed = TRUE)

  expect_error(param_set$qunif(TRUE), "numeric matrix or data.frame", fixed = TRUE)
  expect_error(
    param_set$qunif(data.frame(double = factor("a"))),
    "unclassed numeric vector",
    fixed = TRUE
  )
})

test_that("direct native qunif errors on malformed input without mutation", {
  symbol = native_paramset_qunif_symbol()
  param_set = native_paramset_qunif_space()
  private = native_paramset_qunif_private(param_set)
  valid = matrix(
    c(0.25, 0.75),
    ncol = 1L,
    dimnames = list(NULL, "double")
  )
  before = valid

  invalid_inputs = list(
    list(unname(valid), "one column name"),
    list(structure(valid, class = "native_qunif_matrix"), "numeric matrix or data.frame"),
    list(matrix(numeric(), nrow = 1L, ncol = 0L), "at least one column"),
    list(matrix(NA_real_, nrow = 1L, dimnames = list(NULL, "double")), "missing"),
    list(matrix(NaN, nrow = 1L, dimnames = list(NULL, "double")), "NaN"),
    list(matrix(Inf, nrow = 1L, dimnames = list(NULL, "double")), "finite"),
    list(matrix(-0.01, nrow = 1L, dimnames = list(NULL, "double")), "between zero and one"),
    list(matrix(1.01, nrow = 1L, dimnames = list(NULL, "double")), "between zero and one"),
    list(matrix(0.5, nrow = 1L, dimnames = list(NULL, "unknown")), "subset of"),
    list(matrix(
      c(0.25, 0.75),
      nrow = 1L,
      dimnames = list(NULL, c("double", "double"))
    ), "unique")
  )
  for (case in invalid_inputs) {
    expect_error(
      .Call(symbol, private, param_set, case[[1L]]),
      case[[2L]],
      fixed = TRUE,
      info = deparse(case[[1L]])
    )
  }
  expect_identical(
    .Call(symbol, private, param_set, as.data.frame(valid)),
    param_set$qunif(as.data.frame(valid))
  )
  expect_error(
    .Call(symbol, new.env(parent = emptyenv()), param_set, valid),
    "shell ownership",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, private, new.env(parent = emptyenv()), valid),
    "shell ownership",
    fixed = TRUE
  )
  expect_identical(valid, before)
})

test_that("direct native qunif fails closed on corrupt ParamSet storage", {
  symbol = native_paramset_qunif_symbol()
  template_set = native_paramset_qunif_space()
  params = paradox:::param_set_core_state(
    native_paramset_qunif_private(template_set)
  )$.params
  units = matrix(
    0.5,
    nrow = 1L,
    ncol = length(params$id),
    dimnames = list(NULL, params$id)
  )

  corruptions = list()
  column_order = c(2L, 1L, seq.int(3L, length(params)))
  corruptions[[1L]] = params[, column_order, drop = FALSE]

  corruptions[[2L]] = params
  corruptions[[2L]]$id[[2L]] = corruptions[[2L]]$id[[1L]]

  corruptions[[3L]] = params
  corruptions[[3L]]$grouping[[1L]] = NA_character_

  corruptions[[4L]] = params
  double_row = match("double", corruptions[[4L]]$id)
  corruptions[[4L]]$lower[[double_row]] = NA_real_

  corruptions[[5L]] = params
  factor_row = match("factor", corruptions[[5L]]$id)
  corruptions[[5L]]$levels[[factor_row]] = c("slow", NA_character_)

  for (corrupt in corruptions) {
    param_set = native_paramset_qunif_space()
    private = native_paramset_qunif_private(param_set)
    paradox:::param_set_core_replace(private, params = corrupt)
    expect_error(
      .Call(symbol, private, param_set, units),
      "Corrupt ParamSet quantile state",
      fixed = TRUE
    )
  }
})

test_that("qunif refreshes a live ParamSetShadow before selecting state", {
  origin = ps(x = p_dbl(0, 1), hidden = p_lgl())
  origin$values = list(x = 0.25, hidden = TRUE)
  shadow = ParamSetShadow$new(origin, "hidden")
  private = native_paramset_qunif_private(shadow)
  expect_identical(paradox:::param_set_core_state(private)$.values$x, 0.25)

  origin$values = list(x = 0.75, hidden = FALSE)
  expect_identical(paradox:::param_set_core_state(private)$.values$x, 0.25)
  result = shadow$qunif(matrix(
    c(0, 0.5, 1),
    ncol = 1L,
    dimnames = list(NULL, "x")
  ))

  expect_identical(result$x, c(0, 0.5, 1))
  expect_identical(paradox:::param_set_core_state(private)$.values$x, 0.75)
})

test_that("qunif snapshots ALTREP input before selecting capsule state", {
  param_set = ps(x = p_dbl(0, 1))
  private = native_paramset_qunif_private(param_set)
  first = matrix(c(0, 0.5, 1), ncol = 1L,
    dimnames = list(NULL, "x"))
  later = matrix(rep(0.25, 3L), ncol = 1L,
    dimnames = dimnames(first))
  callbacks = 0L
  units = native_stateful_altrep(
    first,
    later,
    elt_switch_after = length(first),
    callback = function() {
      callbacks <<- callbacks + 1L
      params = unserialize(serialize(
        paradox:::param_set_core_state(private)$.params,
        NULL
      ))
      params$upper[] = 2
      paradox:::param_set_core_replace(private, params = params)
    },
    callback_after = 0L
  )

  expect_identical(param_set$qunif(units)$x, c(0, 1, 2))
  expect_identical(callbacks, 1L)
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
    native_paramset_qunif_private(param_set),
    param_set,
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
  param_set = ps(x = p_lgl())
  private = native_paramset_qunif_private(param_set)
  template = paradox:::param_set_core_state(private)$.params
  params = template[rep.int(1L, size), , drop = FALSE]
  params$id = ids
  params = paradox:::param_set_internal_table(params)
  paradox:::param_set_core_replace(private, params = params)
  units = matrix(
    0.5,
    nrow = 1L,
    ncol = size,
    dimnames = list(NULL, ids)
  )

  result = .Call(
    native_paramset_qunif_symbol(), private, param_set, units
  )
  expect_s3_class(result, "data.table")
  expect_identical(names(result), ids)
  expect_identical(unname(lengths(result)), rep.int(1L, size))
  expect_true(all(vapply(result, identical, logical(1L), FALSE)))
})
