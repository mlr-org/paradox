base_lazy_table_wrapper = function(table) {
  stopifnot(length(table) >= 64L, inherits(table, "data.frame"))
  # R's attribute-only duplicate path wraps sufficiently wide vectors in the
  # base `wrap_list` ALTREP (WRAP_THRESHOLD is 64 in supported R sources).
  # Reinstalling an unchanged class gives us that production representation
  # without a package-private constructor or an unbounded inspect call.
  structure(table, class = class(table))
}

test_that("public table operations materialize base lazy list wrappers", {
  ids = sprintf("parameter_%03d", seq_len(65L))
  domains = lapply(seq_along(ids), function(index) {
    p_dbl(0, 1, trafo = if (index == 1L) function(x) x + 1 else NULL)
  })
  names(domains) = ids
  param_set = ParamSet$new(domains)

  frame = as.data.frame(
    setNames(rep(list(c(0.25, 0.75)), length(ids)), ids),
    optional = TRUE,
    stringsAsFactors = FALSE
  )
  wrapped_frame = base_lazy_table_wrapper(frame)

  expect_true(param_set$check_dt(wrapped_frame, presence = "all"))
  mapped = param_set$qunif(wrapped_frame)
  expect_identical(names(mapped), ids)
  expect_equal(mapped[[1L]], c(0.25, 0.75))

  transformed = param_set$trafo(wrapped_frame)
  expect_identical(names(transformed), ids)
  expect_equal(transformed[[1L]], c(1.25, 1.75))
  expect_equal(transformed[[length(transformed)]], c(0.25, 0.75))

  table = data.table::as.data.table(frame)
  wrapped_table = base_lazy_table_wrapper(table)
  param_set$constraint = function(x) TRUE
  expect_identical(
    param_set$test_constraint_dt(wrapped_table, assert_value = FALSE),
    c(TRUE, TRUE)
  )

  param_set$constraint = NULL
  param_set$add_dep(ids[[2L]], ids[[1L]], CondEqual$new(0.25))
  design = Design$new(
    param_set,
    base_lazy_table_wrapper(data.table::copy(table)),
    remove_dupl = FALSE
  )
  expect_false(is.na(design$data[[2L]][[1L]]))
  expect_true(is.na(design$data[[2L]][[2L]]))

  # Design's dependency mutation may materialize its mutable data.table. Give
  # transpose a fresh base wrapper so both native Design ingress paths are
  # exercised independently.
  design$data = base_lazy_table_wrapper(data.table::copy(design$data))
  rows = design$transpose(filter_na = TRUE, trafo = FALSE)
  expect_length(rows, 2L)
  expect_named(rows[[1L]], ids)
  expect_false(ids[[2L]] %in% names(rows[[2L]]))
})

test_that("lazy table normalization does not admit exotic attributes", {
  ids = sprintf("parameter_%03d", seq_len(65L))
  param_set = ParamSet$new(setNames(
    lapply(ids, function(id) p_dbl(0, 1)),
    ids
  ))
  frame = as.data.frame(
    setNames(rep(list(0.5), length(ids)), ids),
    optional = TRUE,
    stringsAsFactors = FALSE
  )
  attr(frame, "paradox_exotic") = TRUE
  wrapped = base_lazy_table_wrapper(frame)

  expect_identical(
    param_set$check_dt(wrapped),
    "Must be a data.frame or data.table."
  )
  expect_error(
    param_set$qunif(wrapped),
    "numeric matrix or data.frame",
    fixed = TRUE
  )
})

test_that("wide ALTREP table admission observes each top-level element once", {
  skip_if_not(
    exists(
      "C_test_stateful_altrep",
      envir = asNamespace("paradox"),
      inherits = FALSE
    ),
    "the internal stateful ALTREP test class is unavailable"
  )
  ids = sprintf("column_%03d", seq_len(65L))
  first_columns = setNames(
    lapply(seq_along(ids), function(index) c(index, index + 0.5)),
    ids
  )
  later_columns = setNames(
    lapply(seq_along(ids), function(index) rep(-index, 2L)),
    ids
  )
  # as.data.frame() returns an ordinary backing list here. Constructing the
  # wide frame with structure() would itself create base R's lazy wrapper on
  # current R, which the deliberately custom fixture quite properly rejects.
  frame = as.data.frame(
    first_columns,
    optional = TRUE,
    stringsAsFactors = FALSE
  )
  later = as.data.frame(
    later_columns,
    optional = TRUE,
    stringsAsFactors = FALSE
  )
  callbacks = 0L
  outer = NULL
  outer = native_stateful_altrep(
    frame,
    later,
    elt_switch_after = length(ids),
    callback = function() {
      callbacks <<- callbacks + 1L
      # The callback runs during the final Elt observation. Both changes are
      # deliberately in place: metadata and the already-copied first spine
      # slot must have been captured before this reentry.
      data.table::setattr(outer, "names", rev(ids))
      data.table::set(frame, j = ids[[1L]], value = c(999, 999))
    },
    callback_after = length(ids) - 1L
  )

  rows = .Call(
    get("C_design_transpose", envir = asNamespace("paradox")),
    outer,
    FALSE
  )
  expect_length(rows, 2L)
  expect_named(rows[[1L]], ids)
  expect_identical(
    unname(unlist(rows[[1L]], use.names = FALSE)),
    unname(vapply(first_columns, `[[`, numeric(1L), 1L))
  )
  expect_identical(
    unname(unlist(rows[[2L]], use.names = FALSE)),
    unname(vapply(first_columns, `[[`, numeric(1L), 2L))
  )
  expect_false(any(unlist(rows, recursive = TRUE, use.names = FALSE) < 0))
  expect_identical(callbacks, 1L)
  expect_identical(names(outer), rev(ids))
  expect_identical(frame[[ids[[1L]]]], c(999, 999))
})
