base_lazy_table_wrapper = function(table) {
  stopifnot(length(table) >= 64L, inherits(table, "data.frame"))
  # R's attribute-only duplicate path can wrap sufficiently wide vectors in
  # the base `wrap_list` ALTREP. That optimization differs across supported R
  # versions, so tests which require an ALTREP shell use the native fixture;
  # this helper covers the real base representation whenever R selects it.
  structure(table, class = class(table))
}

public_table_native_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"))
}

forge_public_row_names = function(table, row_names) {
  attr(table, "row.names") = row_names
  table
}

public_table_fixture = function(as_data_table = FALSE) {
  table = data.frame(x = c(0.25, 0.75), y = c(0.5, 0.5))
  if (as_data_table) data.table::as.data.table(table) else table
}

expect_public_row_names_rejected = function(row_names, remove = FALSE) {
  param_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  param_set$constraint = function(x) TRUE
  param_set$add_dep("y", "x", CondEqual$new(0.25))
  frame = function(as_data_table = FALSE) {
    result = public_table_fixture(as_data_table)
    forge_public_row_names(result, if (remove) NULL else row_names)
  }

  expect_identical(
    param_set$check_dt(frame(), presence = "all"),
    "Invalid data.frame row names."
  )
  expect_error(
    param_set$test_constraint_dt(frame(TRUE)),
    "Invalid data.frame row names",
    fixed = TRUE
  )
  expect_error(
    param_set$qunif(frame()),
    "invalid data.frame row names",
    fixed = TRUE
  )
  expect_error(
    param_set$trafo(frame()),
    "invalid data.frame row names",
    fixed = TRUE
  )
  expect_error(
    .Call(public_table_native_symbol("design_transpose"), frame(), FALSE),
    "invalid data.frame row names",
    fixed = TRUE
  )
  expect_error(
    .Call(
      public_table_native_symbol("design_dependency_plan"),
      frame(),
      param_set
    ),
    "invalid data.frame row names",
    fixed = TRUE
  )
}

test_that("all public table ingresses reject malformed row-name structure", {
  expect_public_row_names_rejected(asS4(c(1L, 2L)))
  expect_public_row_names_rejected(structure(1:2, paradox_rows = TRUE))
  expect_public_row_names_rejected(structure(1:2, class = "paradox_rows"))
  expect_public_row_names_rejected(NULL, remove = TRUE)

  no_dependencies = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  malformed = forge_public_row_names(
    public_table_fixture(),
    asS4(c(1L, 2L))
  )
  expect_error(
    .Call(
      public_table_native_symbol("design_dependency_plan"),
      malformed,
      no_dependencies
    ),
    "invalid data.frame row names",
    fixed = TRUE
  )
})

test_that("zero-column Design transpose retains the table row count", {
  table = data.frame(row.names = c("a", "b", "c"))
  empty_row = structure(list(), names = character())
  expect_identical(
    .Call(public_table_native_symbol("design_transpose"), table, FALSE),
    rep(list(empty_row), 3L)
  )
  expect_identical(
    .Call(public_table_native_symbol("design_transpose"), list(), FALSE),
    list()
  )
})

test_that("public row-name admission retains ordinary base representations", {
  param_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  param_set$constraint = function(x) TRUE
  dependency_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  dependency_set$add_dep("y", "x", CondEqual$new(0.25))

  frame = data.frame(
    x = c(0.25, 0.75),
    y = c(0.5, 0.5),
    row.names = 1:2
  )
  expect_true(param_set$check_dt(frame, presence = "all"))
  mapped = param_set$qunif(frame)
  expect_equal(mapped$x, frame$x)
  expect_equal(mapped$y, frame$y)
  expect_identical(
    param_set$trafo(frame),
    list(x = frame$x, y = frame$y)
  )
  expect_length(.Call(
    public_table_native_symbol("design_transpose"),
    frame,
    FALSE
  ), 2L)
  expect_length(.Call(
    public_table_native_symbol("design_dependency_plan"),
    frame,
    dependency_set
  )$rows, 1L)
  table = data.table::as.data.table(frame)
  table = forge_public_row_names(
    table,
    attr(frame, "row.names", exact = TRUE)
  )
  expect_identical(
    param_set$test_constraint_dt(table, assert_value = FALSE),
    c(TRUE, TRUE)
  )

  positive_compact = public_table_fixture()
  positive_compact = forge_public_row_names(
    positive_compact,
    c(NA_integer_, 2L)
  )
  expect_true(param_set$check_dt(positive_compact, presence = "all"))

  deferred_character = public_table_fixture()
  deferred_character = forge_public_row_names(
    deferred_character,
    as.character(seq_len(nrow(deferred_character)))
  )
  expect_true(param_set$check_dt(deferred_character, presence = "all"))
})

test_that("row-count mismatches reject only row-consuming table operations", {
  param_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  param_set$constraint = function(x) TRUE
  param_set$add_dep("y", "x", CondEqual$new(0.25))
  mismatched = function(as_data_table = FALSE) {
    forge_public_row_names(public_table_fixture(as_data_table), 1L)
  }

  expect_identical(
    param_set$check_dt(mismatched(), presence = "all"),
    "Invalid data.frame row names."
  )
  expect_error(
    param_set$test_constraint_dt(mismatched(TRUE)),
    "Invalid data.frame row names",
    fixed = TRUE
  )
  expect_error(
    param_set$qunif(mismatched()),
    "invalid data.frame row names",
    fixed = TRUE
  )
  expect_error(
    .Call(public_table_native_symbol("design_transpose"), mismatched(), FALSE),
    "invalid data.frame row names",
    fixed = TRUE
  )
  expect_error(
    .Call(
      public_table_native_symbol("design_dependency_plan"),
      mismatched(),
      param_set
    ),
    "invalid data.frame row names",
    fixed = TRUE
  )
  # ParamSet$trafo() interprets table columns as opaque named parameter values,
  # not as rows, and therefore does not inspect column lengths solely to
  # authenticate a dimension it never consumes.
  expected = public_table_fixture()
  expect_identical(
    param_set$trafo(mismatched()),
    list(x = expected$x, y = expected$y)
  )

  no_dependencies = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  empty_plan = .Call(
    public_table_native_symbol("design_dependency_plan"),
    mismatched(),
    no_dependencies
  )
  expect_identical(
    empty_plan,
    list(rows = list(), columns = character(), values = list())
  )
})

test_that("ignored data.table caches still have ordinary carrier metadata", {
  param_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  table = public_table_fixture(TRUE)
  data.table::setattr(table, "sorted", asS4("x"))
  expect_identical(
    param_set$check_dt(table),
    "Must be a data.frame or data.table."
  )
  expect_error(param_set$qunif(table), "numeric matrix or data.frame")
  expect_error(param_set$trafo(table), "ordinary named list")
  expect_error(
    .Call(public_table_native_symbol("design_transpose"), table, FALSE),
    "list-like data frame"
  )

  malformed_carriers = list(
    selfref_type = function(value) {
      data.table::setattr(value, ".internal.selfref", 1L)
    },
    selfref_attributes = function(value) {
      pointer = attr(value, ".internal.selfref", exact = TRUE)
      attr(pointer, "paradox_test") = TRUE
      data.table::setattr(value, ".internal.selfref", pointer)
    },
    sorted_attributes = function(value) {
      data.table::setattr(
        value,
        "sorted",
        structure("x", paradox_test = TRUE)
      )
    },
    index_type = function(value) {
      data.table::setattr(value, "index", character())
    },
    index_nonempty = function(value) {
      data.table::setattr(value, "index", 1L)
    }
  )
  for (mutate in malformed_carriers) {
    malformed = mutate(public_table_fixture(TRUE))
    expect_identical(
      param_set$check_dt(malformed),
      "Must be a data.frame or data.table."
    )
    expect_error(
      .Call(
        public_table_native_symbol("design_transpose"),
        malformed,
        FALSE
      ),
      "list-like data frame",
      fixed = TRUE
    )
  }
})

test_that("real data.table key and index carriers pass every public ingress", {
  param_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  param_set$constraint = function(x) TRUE
  dependency_set = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
  dependency_set$add_dep("y", "x", CondEqual$new(0.25))
  table = public_table_fixture(TRUE)
  data.table::setkeyv(table, "x")
  data.table::setindexv(table, "y")

  expect_true(param_set$check_dt(table, presence = "all"))
  expect_identical(
    param_set$test_constraint_dt(table, assert_value = FALSE),
    c(TRUE, TRUE)
  )
  mapped = param_set$qunif(table)
  expect_equal(mapped$x, table$x)
  expect_equal(mapped$y, table$y)
  expect_identical(
    param_set$trafo(table),
    list(x = table$x, y = table$y)
  )
  expect_length(
    .Call(public_table_native_symbol("design_transpose"), table, FALSE),
    2L
  )
  expect_length(.Call(
    public_table_native_symbol("design_dependency_plan"),
    table,
    dependency_set
  )$rows, 1L)

  ids = sprintf("parameter_%03d", seq_len(65L))
  wide_set = ParamSet$new(setNames(
    lapply(ids, function(id) p_dbl(0, 1)),
    ids
  ))
  wide = data.table::as.data.table(as.data.frame(
    setNames(rep(list(c(0.25, 0.75)), length(ids)), ids),
    optional = TRUE,
    stringsAsFactors = FALSE
  ))
  data.table::setkeyv(wide, ids[[1L]])
  data.table::setindexv(wide, ids[[2L]])
  wrapped_wide = base_lazy_table_wrapper(wide)
  # Cache disposal is a property of materializing an admitted ALTREP shell.
  # The base wrapper above is intentionally retained for realistic ingress
  # coverage, but older supported R versions may leave it ordinary.
  cache_shell = native_stateful_altrep(wide, wide)
  materialized = .Call(
    public_table_native_symbol("test_materialize_public_table_shell"),
    cache_shell
  )
  expect_identical(names(materialized), ids)
  expect_identical(class(materialized), c("data.table", "data.frame"))
  expect_null(attr(materialized, ".internal.selfref", exact = TRUE))
  expect_null(attr(materialized, "sorted", exact = TRUE))
  expect_null(attr(materialized, "index", exact = TRUE))
  expect_identical(
    data.table::address(materialized[[1L]]),
    data.table::address(wide[[1L]])
  )
  expect_identical(
    data.table::address(materialized[[2L]]),
    data.table::address(wide[[2L]])
  )
  expect_true(wide_set$check_dt(
    base_lazy_table_wrapper(wide),
    presence = "all"
  ))
})

test_that("row-name ALTREP is length-snapshotted once without label access", {
  skip_if_not(
    exists(
      "C_test_stateful_altrep",
      envir = asNamespace("paradox"),
      inherits = FALSE
    ),
    "the internal stateful ALTREP test class is unavailable"
  )
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  row_names = native_stateful_altrep(
    c("row-a", "row-b"),
    c("later-a", "later-b", "later-c"),
    length_switch_after = 1L,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      invisible(gc())
    },
    callback_after = c(0L, 0L)
  )
  expect_identical(.Call(
    public_table_native_symbol("test_public_row_names_count"),
    row_names
  ), 2)
  # The fixture invokes the same callback independently from its Length and
  # Elt methods. One callback therefore proves one Length and zero Elt calls;
  # the later length of three would also expose a second Length observation.
  expect_identical(state$callbacks, 1L)
})

test_that("table metadata is owned before row-name Length reentry", {
  skip_if_not(
    exists(
      "C_test_stateful_altrep_row_names_rearm",
      envir = asNamespace("paradox"),
      inherits = FALSE
    ),
    "the internal raw row-name ALTREP fixture is unavailable"
  )
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$outer = NULL
  row_names = native_stateful_altrep(
    c("row-a", "row-b"),
    c("later-a", "later-b"),
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setnames(state$outer, c("mutated_y", "mutated_x"))
      invisible(gc())
    }
  )
  frame = structure(
    list(x = c(1L, 2L), y = c(3L, 4L)),
    names = c("x", "y"),
    row.names = row_names,
    class = "data.frame"
  )
  state$outer = frame
  invisible(.Call(
    public_table_native_symbol("test_stateful_altrep_row_names_rearm"),
    state$outer,
    c(NA_integer_, 0L)
  ))

  rows = .Call(
    public_table_native_symbol("design_transpose"),
    state$outer,
    FALSE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(names(state$outer), c("mutated_y", "mutated_x"))
  expect_named(rows[[1L]], c("x", "y"))
  expect_identical(rows[[1L]], list(x = 1L, y = 3L))
})

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

test_that("wide ALTREP table admission owns names before Elt reentry", {
  skip_if_not(
    exists(
      "C_test_stateful_altrep",
      envir = asNamespace("paradox"),
      inherits = FALSE
    ),
    "the internal stateful ALTREP test class is unavailable"
  )
  ids = sprintf("column_%03d", seq_len(65L))
  frame = as.data.frame(
    setNames(rep(list(c(0.25, 0.75)), length(ids)), ids),
    optional = TRUE,
    stringsAsFactors = FALSE
  )
  callbacks = 0L
  outer = NULL
  outer = native_stateful_altrep(
    frame,
    frame,
    callback = function() {
      callbacks <<- callbacks + 1L
      data.table::setnames(outer, rev(ids))
    },
    callback_after = 0L
  )

  rows = .Call(
    public_table_native_symbol("design_transpose"),
    outer,
    FALSE
  )
  expect_identical(callbacks, 1L)
  expect_identical(names(outer), rev(ids))
  expect_named(rows[[1L]], ids)
  expect_equal(unname(unlist(rows[[1L]])), rep(0.25, length(ids)))
})
