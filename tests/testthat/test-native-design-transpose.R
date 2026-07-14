test_that("native Design transpose preserves ordinary row configurations", {
  parameter_set = ps(
    double = p_dbl(-Inf, Inf),
    integer = p_int(-10, 10),
    factor = p_fct(c("a", "b")),
    logical = p_lgl(),
    utility = p_uty()
  )
  data = data.table(
    double = c(1, NA_real_, NaN),
    integer = c(1L, NA_integer_, 3L),
    factor = c("a", NA_character_, "b"),
    logical = c(TRUE, NA, FALSE),
    utility = list(list(x = 1), NULL, NA_real_)
  )
  design = Design$new(parameter_set, copy(data), remove_dupl = FALSE)

  expect_identical(
    design$transpose(filter_na = FALSE, trafo = FALSE),
    transpose_list(design$data)
  )
  expected = map(
    transpose_list(design$data),
    function(row) Filter(Negate(is_scalar_na), row)
  )
  expect_identical(
    design$transpose(filter_na = TRUE, trafo = FALSE),
    expected
  )
  expect_named(design$transpose()[[2L]], "utility")
  expect_identical(design$transpose()[[2L]]$utility, NULL)
  expect_named(design$transpose()[[3L]], c("integer", "factor", "logical"))
})

test_that("native transpose covers standard atomic storage exactly", {
  native = get("C_design_transpose", envir = asNamespace("paradox"))
  data = list(
    logical = c(TRUE, NA),
    integer = c(1L, NA_integer_),
    double = c(1, NaN),
    complex = c(1 + 2i, NA_complex_),
    character = c("x", NA_character_),
    raw = as.raw(c(1, 2)),
    utility = list(identity, integer())
  )

  actual = .Call(native, data, FALSE)
  expected = transpose_list(data)
  expect_identical(actual, expected)
  expect_identical(.Call(native, data, TRUE), list(
    list(
      logical = TRUE,
      integer = 1L,
      double = 1,
      complex = 1 + 2i,
      character = "x",
      raw = as.raw(1),
      utility = identity
    ),
    list(raw = as.raw(2), utility = integer())
  ))
})

test_that("native transpose fails closed for dispatching and malformed inputs", {
  native = get("C_design_transpose", envir = asNamespace("paradox"))

  expect_null(.Call(native, list(x = factor(c("a", "b"))), FALSE))
  expect_null(.Call(
    native,
    list(x = as.Date(c("2020-01-01", "2020-01-02"))),
    FALSE
  ))
  expect_null(.Call(
    native,
    list(x = structure(1:2, names = c("a", "b"))),
    FALSE
  ))
  expect_null(.Call(
    native,
    list(x = list(structure(NA, class = "custom"))),
    TRUE
  ))
  expect_null(.Call(
    native,
    list(x = list(structure(c(NA, NA), class = "custom"))),
    TRUE
  ))
  expect_null(.Call(
    native,
    list(x = structure(list(1L, 2L), class = "AsIs")),
    FALSE
  ))
  expect_null(.Call(
    native,
    structure(list(x = 1:2), class = "ParadoxTransposeContainer"),
    FALSE
  ))
  expect_null(.Call(native, setNames(list(1:2), NA_character_), FALSE))
  expect_null(.Call(native, setNames(list(1:2), ""), FALSE))
  expect_null(.Call(native, list(x = 1:2, y = 1), FALSE))
  expect_null(.Call(native, unname(list(1:2)), FALSE))
  expect_null(.Call(native, 1:2, FALSE))
  expect_null(.Call(native, list(x = 1:2), NA))
  expect_null(.Call(native, list(x = 1:2), logical()))

  parameter_set = ps(x = p_uty())
  factor_data = data.table(x = factor(c("a", NA), levels = c("a", "b")))
  factor_design = Design$new(parameter_set, factor_data, remove_dupl = FALSE)
  expect_identical(
    factor_design$transpose(filter_na = FALSE, trafo = FALSE),
    transpose_list(factor_data)
  )
  expect_identical(
    factor_design$transpose(filter_na = TRUE, trafo = FALSE),
    map(
      transpose_list(factor_data),
      function(row) Filter(Negate(is_scalar_na), row)
    )
  )

  list_data = data.table(x = I(list(1L, NA_integer_)))
  list_design = Design$new(parameter_set, list_data, remove_dupl = FALSE)
  expect_identical(
    list_design$transpose(filter_na = TRUE, trafo = FALSE),
    map(
      transpose_list(list_data),
      function(row) Filter(Negate(is_scalar_na), row)
    )
  )

  element_class = "ParadoxTransposeScalarDispatch"
  registerS3method(
    "length",
    element_class,
    function(value) 1L,
    envir = asNamespace("base")
  )
  registerS3method(
    "is.na",
    element_class,
    function(value) TRUE,
    envir = asNamespace("base")
  )
  dispatch_data = data.table(
    x = list(structure(c(1, 2), class = element_class))
  )
  dispatch_design = Design$new(
    parameter_set,
    dispatch_data,
    remove_dupl = FALSE
  )
  expect_identical(
    dispatch_design$transpose(filter_na = TRUE, trafo = FALSE),
    map(
      transpose_list(dispatch_data),
      function(row) Filter(Negate(is_scalar_na), row)
    )
  )

  mutable_design = Design$new(
    parameter_set,
    data.table(x = list(1L, 2L)),
    remove_dupl = FALSE
  )
  names(mutable_design$data) = NA_character_
  expect_identical(
    mutable_design$transpose(filter_na = FALSE, trafo = FALSE),
    transpose_list(mutable_design$data)
  )
  names(mutable_design$data) = ""
  expect_identical(
    mutable_design$transpose(filter_na = FALSE, trafo = FALSE),
    transpose_list(mutable_design$data)
  )

  container_class = "ParadoxTransposeLengthDispatch"
  registerS3method(
    "length",
    container_class,
    function(value) 0L,
    envir = asNamespace("base")
  )
  names(mutable_design$data) = "x"
  class(mutable_design$data) = c(
    container_class,
    "data.table",
    "data.frame"
  )
  expect_identical(
    mutable_design$transpose(filter_na = FALSE, trafo = FALSE),
    transpose_list(mutable_design$data)
  )
})

test_that("native transpose authenticates control scalars before Length", {
  native = get("C_design_transpose", envir = asNamespace("paradox"))
  callbacks = 0L
  filter_na = native_stateful_altrep(
    FALSE,
    TRUE,
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("transpose control invoked ALTREP Length", call. = FALSE)
    },
    callback_after = c(NA_integer_, 0L)
  )

  expect_null(.Call(native, list(x = 1L), filter_na))
  expect_identical(callbacks, 0L)
})

test_that("native transpose declines callback-capable inputs without observation", {
  native = get("C_design_transpose", envir = asNamespace("paradox"))
  callbacks = 0L

  container = native_stateful_altrep(
    list(x = 1:2, y = 3:4),
    list(x = 10:11, y = 30:31, z = 50:51),
    length_switch_after = 0L,
    elt_switch_after = 0L,
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = c(NA_integer_, 1L)
  )
  expect_null(.Call(native, container, FALSE))
  expect_identical(callbacks, 0L)

  column = native_stateful_altrep(
    c(1L, 2L),
    c(10L, 11L),
    length_switch_after = 0L,
    elt_switch_after = 0L,
    callback = function() callbacks <<- callbacks + 1L,
    callback_after = 0L
  )
  expect_null(.Call(native, list(x = column), FALSE))
  expect_identical(callbacks, 0L)

  name_callbacks = 0L
  column_names = native_stateful_altrep(
    "x",
    "changed",
    length_switch_after = 0L,
    elt_switch_after = 0L,
    callback = function() name_callbacks <<- name_callbacks + 1L,
    callback_after = 0L
  )
  named_data = list(1:2)
  attr(named_data, "names") = column_names
  column_names = attr(named_data, "names", exact = TRUE)
  native_stateful_altrep_rearm(column_names, c(0L, 0L))
  name_callbacks = 0L
  expect_null(.Call(native, named_data, FALSE))
  expect_identical(name_callbacks, 0L)
})

test_that("native transpose handles empty dimensions and owns row names", {
  native = get("C_design_transpose", envir = asNamespace("paradox"))

  expect_identical(.Call(native, list(), FALSE), list())
  expect_identical(.Call(native, list(x = numeric()), FALSE), list())
  expect_identical(
    .Call(native, list(x = NA_real_), TRUE),
    list(setNames(list(), character()))
  )

  observed_names = NULL
  parameter_set = ps(
    x = p_dbl(),
    .extra_trafo = function(x) {
      observed_names <<- names(x)
      x
    }
  )
  design = Design$new(
    parameter_set,
    data.table(x = NA_real_),
    remove_dupl = FALSE
  )
  expect_identical(design$transpose(), list(setNames(list(), character())))
  expect_identical(observed_names, character())

  source_names = c("x", "y")
  data = setNames(list(c(1L, 2L), c("a", "b")), source_names)
  result = .Call(native, data, FALSE)
  names(result[[1L]])[[1L]] = "changed"
  expect_identical(names(data), source_names)
  expect_identical(names(result[[2L]]), source_names)
})
