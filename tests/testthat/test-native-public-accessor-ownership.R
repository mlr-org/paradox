test_that("the general data.table finalizer owns every column spine", {
  leaf = new.env(parent = emptyenv())
  factor_column = factor(c("left", "right"))
  attr(factor_column, "metadata") = list(labels = c("L", "R"))
  source = data.table::data.table(
    atomic = 1:2,
    factor = factor_column,
    listed = I(list(leaf, list(y = 2L)))
  )
  result = .Call(paradox:::C_finalize_data_table, source)

  expect_false(identical(
    data.table::address(result$atomic),
    data.table::address(source$atomic)
  ))
  expect_false(identical(
    data.table::address(result$factor),
    data.table::address(source$factor)
  ))
  expect_false(identical(
    data.table::address(levels(result$factor)),
    data.table::address(levels(source$factor))
  ))
  expect_false(identical(
    data.table::address(attr(result$factor, "metadata")),
    data.table::address(attr(source$factor, "metadata"))
  ))
  expect_false(identical(
    data.table::address(attr(result$factor, "metadata")$labels),
    data.table::address(attr(source$factor, "metadata")$labels)
  ))
  expect_false(identical(
    data.table::address(result$listed),
    data.table::address(source$listed)
  ))
  # Generic list-column leaves are semantic/opaque, not recursively copied.
  expect_true(identical(result$listed[[1L]], source$listed[[1L]]))
  expect_identical(result$listed[[1L]], leaf)

  data.table::set(result, i = 1L, j = "atomic", value = 99L)
  data.table::setattr(result$factor, "levels", c("changed", "right"))
  attr(result$factor, "metadata")$labels[[1L]] = "changed"
  data.table::set(
    result,
    i = 1L,
    j = "listed",
    value = list(list(replaced = TRUE))
  )
  expect_identical(source$atomic, 1:2)
  expect_identical(levels(source$factor), c("left", "right"))
  expect_identical(
    attr(source$factor, "metadata")$labels,
    c("L", "R")
  )
  expect_identical(source$listed[[1L]], leaf)
})

test_that("the general data.table finalizer materializes stable ALTREP columns", {
  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )
  column = native_stateful_altrep(
    structure(1:3, metadata = list(labels = letters[1:3])),
    structure(1:3, metadata = list(labels = letters[1:3]))
  )
  source = structure(
    list(value = column),
    names = "value",
    row.names = 1:3,
    class = c("data.table", "data.frame")
  )

  result = .Call(paradox:::C_finalize_data_table, source)
  expect_identical(as.integer(result$value), 1:3)
  expect_identical(
    attr(result$value, "metadata"),
    list(labels = letters[1:3])
  )
  expect_false(identical(
    data.table::address(result$value),
    data.table::address(source$value)
  ))
  expect_false(identical(
    data.table::address(attr(result$value, "metadata")),
    data.table::address(attr(source$value, "metadata"))
  ))
})

test_that("package facades own ordinary structure and materialize ALTREP metadata", {
  facade = paradox:::param_set_data_table_facade(list(
    left = 1:3,
    right = letters[1:3]
  ))
  expect_s3_class(facade, "data.table")
  expect_identical(facade$left, 1:3)
  expect_identical(facade$right, letters[1:3])
  expect_identical(row.names(facade), as.character(1:3))
  expect_identical(.row_names_info(facade, 0L), c(NA_integer_, -3L))

  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )
  # A stable atomic ALTREP attribute is ordinary base-R output, so the
  # finalizer materializes it instead of rejecting the whole table.
  atomic_altrep = native_stateful_altrep(
    c(1L, 2L),
    c(1L, 2L)
  )
  source = structure(
    list(value = 1:2),
    names = "value",
    row.names = c(NA_integer_, -2L),
    class = c("data.table", "data.frame"),
    metadata = atomic_altrep
  )
  finalized = .Call(paradox:::C_finalize_data_table, source)
  expect_identical(attr(finalized, "metadata", exact = TRUE), c(1L, 2L))

  unstable_altrep = native_stateful_altrep(
    c(1L, 2L),
    c(9L, 9L),
    elt_switch_after = 2L
  )
  unstable_source = structure(
    list(value = 1:2),
    names = "value",
    row.names = c(NA_integer_, -2L),
    class = c("data.table", "data.frame"),
    metadata = unstable_altrep
  )
  expect_error(
    .Call(paradox:::C_finalize_data_table, unstable_source),
    "Built-in metadata ALTREP changed while being snapshotted",
    fixed = TRUE
  )
})

test_that("the general data.table finalizer bounds recursive column metadata", {
  finalize_column = function(column) {
    table = structure(
      list(value = column),
      names = "value",
      row.names = c(NA_integer_, -2L),
      class = c("data.table", "data.frame")
    )
    .Call(paradox:::C_finalize_data_table, table)
  }
  finalize = function(metadata) {
    finalize_column(structure(1:2, metadata = metadata))
  }
  finalize_cycle = function() {
    metadata = list(NULL)
    # Build the complete input while its metadata is acyclic. R 3.6's public
    # structure/attribute setters recursively duplicate an already-cyclic
    # value and overflow before the package can inspect it.
    column = structure(1:2, metadata = metadata)
    table = structure(
      list(value = column),
      names = "value",
      row.names = c(NA_integer_, -2L),
      class = c("data.table", "data.frame")
    )
    pointer = .Call(
      get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
      metadata,
      0L,
      metadata
    )
    rm(pointer)
    for (attempt in seq_len(3L)) {
      invisible(gc(full = TRUE))
    }
    stopifnot(identical(
      data.table::address(metadata[[1L]]),
      data.table::address(metadata)
    ))
    .Call(paradox:::C_finalize_data_table, table)
  }

  deep = TRUE
  for (depth in seq_len(100L)) {
    deep = list(deep)
  }
  expect_error(
    finalize(deep),
    "metadata must be ordinary, acyclic, and bounded"
  )
  expect_error(
    finalize_cycle(),
    "metadata must be ordinary, acyclic, and bounded"
  )
})

test_that("ALTREP column callbacks cannot introduce recursive metadata", {
  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  column = native_stateful_altrep(
    structure(1:2, metadata = list(NULL)),
    structure(1:2, metadata = list(NULL)),
    callback = function() {
      state$callbacks = state$callbacks + 1L
      pointer = state$mutator
      state$mutator = NULL
      rm(pointer)
      invisible(gc(full = TRUE))
    },
    callback_after = 0L
  )
  metadata = attr(column, "metadata", exact = TRUE)
  state$mutator = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    metadata,
    0L,
    metadata
  )
  table = structure(
    list(value = column),
    names = "value",
    row.names = c(NA_integer_, -2L),
    class = c("data.table", "data.frame")
  )

  expect_error(
    .Call(paradox:::C_finalize_data_table, table),
    "metadata must be ordinary, acyclic, and bounded"
  )
  expect_identical(state$callbacks, 1L)
})

test_that("metadata mutation after preflight cannot enter R's duplicator", {
  metadata = list(NULL)
  value = structure(3L, metadata = metadata)
  state = new.env(parent = emptyenv())
  state$hook_calls = 0L
  mutator = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    metadata,
    0L,
    metadata
  )
  hook = function() {
    state$hook_calls = state$hook_calls + 1L
    pointer = mutator
    mutator <<- NULL
    rm(pointer)
    for (attempt in seq_len(3L)) {
      invisible(gc(full = TRUE))
    }
  }

  expect_error(
    .Call(
      get(
        "C_test_builtin_metadata_copy_reentry",
        envir = asNamespace("paradox")
      ),
      value,
      hook
    ),
    "metadata must be ordinary, acyclic, and bounded"
  )
  expect_identical(state$hook_calls, 1L)
  expect_identical(
    data.table::address(metadata[[1L]]),
    data.table::address(metadata)
  )
})

test_that("finalizer-expanded attribute spines remain bounded", {
  value = structure(3L, metadata = "selected")
  labels = sprintf("late_attribute_%03d", seq_len(65L))
  # Intern every label before the finalizers run so this fixture tests the
  # bounded spine walk rather than symbol allocation inside the GC callback.
  invisible(lapply(labels, as.name))
  mutators = lapply(seq_along(labels), function(index) {
    .Call(
      get("C_test_gc_attribute_mutator", envir = asNamespace("paradox")),
      value,
      labels[[index]],
      index
    )
  })
  hook = function() {
    pointers = mutators
    mutators <<- NULL
    rm(pointers)
    for (attempt in seq_len(3L)) {
      invisible(gc(full = TRUE))
    }
  }

  expect_error(
    .Call(
      get(
        "C_test_builtin_metadata_copy_reentry",
        envir = asNamespace("paradox")
      ),
      value,
      hook
    ),
    "metadata must be ordinary, acyclic, and bounded"
  )
  expect_true(all(labels %in% names(attributes(value))))
})

test_that("preexisting overlong attribute spines reject cleanly", {
  value = 3L
  labels = sprintf("attribute_%03d", seq_len(65L))
  attributes(value) = stats::setNames(as.list(seq_along(labels)), labels)

  expect_error(
    .Call(
      get(
        "C_test_builtin_metadata_copy_reentry",
        envir = asNamespace("paradox")
      ),
      value,
      NULL
    ),
    "metadata must be ordinary, acyclic, and bounded"
  )
})

test_that("bounded metadata copying preserves selected attribute order", {
  ordered = structure(3L, class = "foo", extra = 1)
  result = .Call(
    get(
      "C_test_builtin_metadata_copy_reentry",
      envir = asNamespace("paradox")
    ),
    ordered,
    NULL
  )
  expect_identical(names(attributes(result)), c("class", "extra"))

  reversed = structure(3L, extra = 1, class = "foo")
  result = .Call(
    get(
      "C_test_builtin_metadata_copy_reentry",
      envir = asNamespace("paradox")
    ),
    reversed,
    NULL
  )
  expect_identical(names(attributes(result)), c("extra", "class"))

  # Nested attribute carriers keep their own selected order too.
  nested = structure(
    3L,
    payload = structure(1:4, dim = c(2L, 2L), note = "n", class = "m")
  )
  result = .Call(
    get(
      "C_test_builtin_metadata_copy_reentry",
      envir = asNamespace("paradox")
    ),
    nested,
    NULL
  )
  expect_identical(
    names(attributes(attr(result, "payload", exact = TRUE))),
    c("dim", "note", "class")
  )
})

test_that("setter-normalized raw attributes cannot disappear silently", {
  # Public setters remove an empty `class`, so create the same raw serialized
  # pairlist by renaming an equally long ordinary attribute in the bytes.
  value = structure(3L, zzzzz = character())
  bytes = serialize(value, NULL, version = 2L)
  marker = charToRaw("zzzzz")
  offsets = which(vapply(
    seq_len(length(bytes) - length(marker) + 1L),
    function(offset) {
      identical(
        bytes[offset:(offset + length(marker) - 1L)],
        marker
      )
    },
    logical(1L)
  ))
  expect_length(offsets, 1L)
  bytes[offsets[[1L]]:(offsets[[1L]] + length(marker) - 1L)] =
    charToRaw("class")
  value = unserialize(bytes)
  expect_identical(attr(value, "class", exact = TRUE), character())

  # The source itself never moves here, so the receipt must name the spelling
  # rather than blame a concurrent change.
  expect_error(
    .Call(
      get(
        "C_test_builtin_metadata_copy_reentry",
        envir = asNamespace("paradox")
      ),
      value,
      NULL
    ),
    "Built-in value metadata uses a spelling public R setters cannot reproduce",
    fixed = TRUE
  )
})

test_that("genuine metadata mutation keeps the changed-while-snapshotted report", {
  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )
  # The mutation has to land after the first attribute has already been
  # selected and copied, so arm it from an ALTREP attribute that is copied
  # second. A hook would run before any attribute was selected at all.
  metadata = list("original")
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  trigger = native_stateful_altrep(
    c(1, 2),
    c(1, 2),
    callback = function() {
      state$callbacks = state$callbacks + 1L
      pointer = state$mutator
      state$mutator = NULL
      rm(pointer)
      for (attempt in seq_len(3L)) {
        invisible(gc(full = TRUE))
      }
    },
    callback_after = 0L
  )
  state$mutator = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    metadata,
    0L,
    "replaced"
  )
  value = structure(3L, metadata = metadata, trigger = trigger)

  expect_error(
    .Call(
      get(
        "C_test_builtin_metadata_copy_reentry",
        envir = asNamespace("paradox")
      ),
      value,
      NULL
    ),
    "Built-in value changed while being snapshotted",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(metadata[[1L]], "replaced")
})

test_that("metadata root back-edges reject at the bounded graph root", {
  metadata = list(NULL)
  value = structure(3L, metadata = metadata)
  mutator = .Call(
    get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
    metadata,
    0L,
    value
  )
  rm(mutator)
  for (attempt in seq_len(3L)) {
    invisible(gc(full = TRUE))
  }
  expect_identical(
    data.table::address(attr(metadata[[1L]], "metadata")),
    data.table::address(metadata)
  )
  expect_error(
    .Call(
      get(
        "C_test_builtin_metadata_copy_reentry",
        envir = asNamespace("paradox")
      ),
      value,
      NULL
    ),
    "metadata must be ordinary, acyclic, and bounded"
  )
})

test_that("bounded metadata copying preserves public attribute families", {
  named = stats::setNames(c(1, 2), c("left", "right"))
  matrixish = 1:4
  attributes(matrixish) = list(
    dim = c(2L, 2L),
    names = letters[1:4],
    dimnames = list(c("r1", "r2"), c("c1", "c2")),
    matrix_note = list(labels = c("one", "two")),
    class = "matrixish"
  )
  frame = data.frame(
    value = c(3L, 4L),
    row.names = c("row-a", "row-b")
  )
  series = stats::ts(c(5, 6, 7, 8), start = c(2001, 2), frequency = 4)
  comment(series) = "bounded metadata"
  categorical = factor(c("b", "a"), levels = c("a", "b"))
  custom = structure(
    c("x", "y"),
    custom = list(labels = structure(c("X", "Y"), marker = TRUE))
  )
  source = structure(
    11L,
    metadata = list(
      named = named,
      matrixish = matrixish,
      frame = frame,
      series = series,
      categorical = categorical,
      custom = custom
    )
  )

  result = .Call(
    get(
      "C_test_builtin_metadata_copy_reentry",
      envir = asNamespace("paradox")
    ),
    source,
    NULL
  )
  source_metadata = attr(source, "metadata", exact = TRUE)
  result_metadata = attr(result, "metadata", exact = TRUE)
  expect_identical(unclass(result), unclass(source))
  expect_identical(names(result_metadata), names(source_metadata))
  expect_false(identical(
    data.table::address(result_metadata),
    data.table::address(source_metadata)
  ))
  for (name in names(source_metadata)) {
    source_value = source_metadata[[name]]
    result_value = result_metadata[[name]]
    expect_identical(typeof(result_value), typeof(source_value))
    expect_identical(length(result_value), length(source_value))
    source_attributes = attributes(source_value)
    result_attributes = attributes(result_value)
    expect_identical(names(result_attributes), names(source_attributes))
    for (attribute in names(source_attributes)) {
      expect_identical(
        attr(result_value, attribute, exact = TRUE),
        attr(source_value, attribute, exact = TRUE)
      )
    }
    expect_false(identical(
      data.table::address(result_value),
      data.table::address(source_value)
    ))
  }
  expect_false(identical(
    data.table::address(levels(result_metadata$categorical)),
    data.table::address(levels(source_metadata$categorical))
  ))
  expect_false(identical(
    data.table::address(attr(result_metadata$custom, "custom")$labels),
    data.table::address(attr(source_metadata$custom, "custom")$labels)
  ))
})

test_that("ParamSet schema accessors own interpreted and typed leaves", {
  aggregate = function(x) sum(unlist(x))
  convert = function(domain, param_vals) param_vals[[1L]]
  utility = new.env(parent = emptyenv())
  typed_s4 = asS4(7L)
  factor_special = structure(
    NA_character_,
    metadata = list(labels = structure("special", marker = TRUE))
  )
  factor_default = structure(
    "a",
    metadata = list(labels = structure("default", marker = TRUE))
  )
  factor_init = structure(
    "b",
    metadata = list(labels = structure("init", marker = TRUE))
  )
  set = ps(
    factor = p_fct(
      c("a", "b"),
      special_vals = list(factor_special, typed_s4),
      default = factor_default,
      init = factor_init
    ),
    integer = p_int(
      0,
      10,
      tags = "internal_tuning",
      aggr = aggregate,
      in_tune_fn = convert,
      disable_in_tune = list(control = 1L)
    ),
    utility = p_uty(
      special_vals = list(utility),
      default = utility
    )
  )
  set$add_dep("factor", "integer", CondEqual(1L))

  params = set$params
  expect_identical(params$cargo[[2L]]$aggr, aggregate)
  expect_identical(params$cargo[[2L]]$in_tune_fn, convert)
  expect_identical(params$special_vals[[1L]][[2L]], typed_s4)
  expect_identical(params$special_vals[[3L]][[1L]], utility)
  expect_identical(params$default[[3L]], utility)

  data.table::setattr(params$levels[[1L]], "adversarial", TRUE)
  data.table::setattr(
    params$cargo[[2L]]$disable_in_tune,
    "names",
    "changed"
  )
  data.table::setattr(
    params$special_vals[[1L]][[1L]],
    "adversarial",
    TRUE
  )
  data.table::setattr(
    attr(params$special_vals[[1L]][[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(params$default[[1L]], "adversarial", TRUE)
  data.table::setattr(
    attr(params$default[[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(params$.init[[1L]], "adversarial", TRUE)
  data.table::setattr(
    attr(params$.init[[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(
    params$.requirements[[1L]][[2L]],
    "names",
    c("changed", "condition_format_string")
  )

  current = set$params
  expect_null(attr(current$levels[[1L]], "adversarial"))
  expect_identical(
    names(current$cargo[[2L]]$disable_in_tune),
    "control"
  )
  expect_null(attr(current$special_vals[[1L]][[1L]], "adversarial"))
  expect_null(attr(
    attr(current$special_vals[[1L]][[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_null(attr(current$default[[1L]], "adversarial"))
  expect_null(attr(
    attr(current$default[[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_null(attr(current$.init[[1L]], "adversarial"))
  expect_null(attr(
    attr(current$.init[[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_identical(names(set$deps$cond[[1L]]), c(
    "rhs",
    "condition_format_string"
  ))

  domains = set$domains
  factor = domains$factor
  data.table::setattr(factor$levels[[1L]], "adversarial", TRUE)
  data.table::setattr(
    factor$special_vals[[1L]][[1L]],
    "adversarial",
    TRUE
  )
  data.table::setattr(
    attr(factor$special_vals[[1L]][[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(factor$default[[1L]], "adversarial", TRUE)
  data.table::setattr(
    attr(factor$default[[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(factor$.init[[1L]], "adversarial", TRUE)
  data.table::setattr(
    attr(factor$.init[[1L]], "metadata")$labels,
    "adversarial",
    TRUE
  )
  data.table::setattr(
    factor$.requirements[[1L]][[1L]]$cond,
    "names",
    c("changed", "condition_format_string")
  )
  expect_null(attr(set$domains$factor$levels[[1L]], "adversarial"))
  expect_null(attr(
    set$domains$factor$special_vals[[1L]][[1L]],
    "adversarial"
  ))
  expect_null(attr(
    attr(
      set$domains$factor$special_vals[[1L]][[1L]],
      "metadata"
    )$labels,
    "adversarial"
  ))
  expect_null(attr(set$domains$factor$default[[1L]], "adversarial"))
  expect_null(attr(
    attr(set$domains$factor$default[[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_null(attr(set$domains$factor$.init[[1L]], "adversarial"))
  expect_null(attr(
    attr(set$domains$factor$.init[[1L]], "metadata")$labels,
    "adversarial"
  ))
  expect_identical(names(set$deps$cond[[1L]]), c(
    "rhs",
    "condition_format_string"
  ))

  data = set$data
  data.table::set(data, i = 1L, j = "lower", value = 99)
  expect_identical(set$lower[["factor"]], NA_real_)

  detached = list(
    class = set$class,
    lower = set$lower,
    upper = set$upper,
    levels = set$levels,
    storage_type = set$storage_type,
    special_vals = set$special_vals,
    default = set$default
  )
  data.table::setattr(detached$lower, "adversarial", TRUE)
  data.table::setattr(detached$levels[[1L]], "adversarial", TRUE)
  data.table::setattr(
    detached$special_vals[[1L]][[1L]],
    "adversarial",
    TRUE
  )
  data.table::setattr(detached$default[[1L]], "adversarial", TRUE)
  expect_null(attr(set$lower, "adversarial"))
  expect_null(attr(set$levels[[1L]], "adversarial"))
  expect_null(attr(set$special_vals[[1L]][[1L]], "adversarial"))
  expect_null(attr(set$default[[1L]], "adversarial"))
})

test_that("typed special ingress is owned while opaque identity is retained", {
  typed = structure(
    3L,
    source_marker = TRUE,
    metadata = list(labels = structure("typed", marker = TRUE))
  )
  typed_domain = p_int(0, 5, special_vals = list(typed))
  data.table::setattr(typed, "after_construction", TRUE)
  data.table::setattr(
    attr(typed, "metadata")$labels,
    "after_construction",
    TRUE
  )
  expect_null(attr(
    typed_domain$special_vals[[1L]][[1L]],
    "after_construction"
  ))
  expect_null(attr(
    attr(typed_domain$special_vals[[1L]][[1L]], "metadata")$labels,
    "after_construction"
  ))

  set = ps(value = typed_domain)
  data.table::setattr(
    typed_domain$special_vals[[1L]][[1L]],
    "after_paramset",
    TRUE
  )
  data.table::setattr(
    attr(
      typed_domain$special_vals[[1L]][[1L]],
      "metadata"
    )$labels,
    "after_paramset",
    TRUE
  )
  expect_null(attr(
    set$special_vals$value[[1L]],
    "after_paramset"
  ))
  expect_null(attr(
    attr(set$special_vals$value[[1L]], "metadata")$labels,
    "after_paramset"
  ))

  opaque = new.env(parent = emptyenv())
  utility_domain = p_uty(special_vals = list(opaque), default = opaque)
  utility_set = ps(value = utility_domain)
  expect_identical(utility_set$special_vals$value[[1L]], opaque)
  expect_identical(utility_set$default$value, opaque)
  expect_identical(utility_set$domains$value$special_vals[[1L]][[1L]], opaque)
})

test_that("typed special ingress bounds metadata before duplication", {
  metadata = TRUE
  for (depth in seq_len(300L)) {
    metadata = list(metadata)
  }
  typed = structure(3L, metadata = metadata)
  expect_error(
    p_int(0, 5, special_vals = list(typed)),
    "metadata must be ordinary, acyclic, and bounded"
  )

  wide = 3L
  attributes(wide) = stats::setNames(
    as.list(seq_len(65L)),
    paste0("metadata_", seq_len(65L))
  )
  expect_error(
    p_int(0, 5, special_vals = list(wide)),
    "metadata must be ordinary, acyclic, and bounded"
  )

  broad = structure(3L, metadata = rep(list(NULL), 65536L))
  expect_error(
    p_int(0, 5, special_vals = list(broad)),
    "metadata must be ordinary, acyclic, and bounded"
  )
})

test_that("semantic functions retain identity but closure metadata rejects", {
  semantic_function = function(value) value
  utility_domain = p_uty(
    special_vals = list(semantic_function),
    default = semantic_function
  )
  expect_identical(
    data.table::address(utility_domain$special_vals[[1L]][[1L]]),
    data.table::address(semantic_function)
  )
  expect_identical(
    data.table::address(utility_domain$default[[1L]]),
    data.table::address(semantic_function)
  )

  typed = structure(3L, metadata = semantic_function)
  expect_error(
    p_int(0, 5, special_vals = list(typed)),
    "metadata must be ordinary, acyclic, and bounded"
  )

  table = structure(
    list(value = structure(1:2, metadata = semantic_function)),
    names = "value",
    row.names = c(NA_integer_, -2L),
    class = c("data.table", "data.frame")
  )
  expect_error(
    .Call(paradox:::C_finalize_data_table, table),
    "metadata must be ordinary, acyclic, and bounded"
  )
})

test_that("raw and filtered value accessors detach typed values on every graph", {
  opaque = new.env(parent = emptyenv())
  opaque_nodefault = structure(
    list(marker = opaque),
    class = "NoDefault"
  )
  base = ps(
    typed = p_int(),
    absent = p_lgl(),
    opaque = p_uty(),
    opaque_nodefault = p_uty()
  )
  # A partial raw store exercises the value-to-parameter match direction.
  # Unchecked storage deliberately retains arbitrary typed value metadata so
  # the outward accessor, rather than checked sanitization, owns this boundary.
  base$assert_values = FALSE
  base$values = list(
    typed = structure(
      1L,
      metadata = list(labels = structure("typed", marker = TRUE))
    ),
    opaque = opaque,
    opaque_nodefault = opaque_nodefault
  )
  collection = ParamSetCollection$new(list(child = base))
  shadow = ParamSetShadow$new(base, "absent")

  accessors = list(
    raw = function(x) x$values,
    filtered = function(x) x$get_values(
      check_required = FALSE,
      remove_dependencies = FALSE
    )
  )
  for (node in list(base, collection, shadow)) {
    for (access in accessors) {
      values = access(node)
      original_names = names(access(node))
      data.table::setattr(values, "names", paste0(names(values), ".changed"))
      expect_identical(names(access(node)), original_names)

      values = access(node)
      typed_name = names(values)[grepl("typed", names(values), fixed = TRUE)]
      opaque_name = names(values)[grepl("opaque", names(values), fixed = TRUE)]
      nodefault_name = names(values)[grepl(
        "opaque_nodefault",
        names(values),
        fixed = TRUE
      )]
      opaque_name = setdiff(opaque_name, nodefault_name)
      data.table::setattr(values[[typed_name]], "adversarial", TRUE)
      data.table::setattr(
        attr(values[[typed_name]], "metadata")$labels,
        "adversarial",
        TRUE
      )
      expect_null(attr(base$values$typed, "adversarial"))
      expect_null(attr(
        attr(base$values$typed, "metadata")$labels,
        "adversarial"
      ))
      expect_identical(values[[opaque_name]], opaque)
      expect_identical(
        data.table::address(values[[nodefault_name]]),
        data.table::address(opaque_nodefault)
      )
    }
  }
})

test_that("collection sets detach the carrier but retain exact children", {
  child = ps(x = p_int())
  collection = ParamSetCollection$new(list(child = child))

  sets = collection$sets
  expect_identical(sets[[1L]], child)
  data.table::setattr(sets, "names", "changed")
  expect_identical(names(collection$sets), "child")
  expect_identical(collection$sets[[1L]], child)
})

test_that("unfiltered ids own their public vector", {
  base = ps(alpha = p_int(), beta = p_dbl())
  nodes = list(
    base,
    ParamSetCollection$new(list(child = base)),
    ParamSetShadow$new(base, "beta")
  )
  expected = list(
    c("alpha", "beta"),
    c("child.alpha", "child.beta"),
    "alpha"
  )
  for (index in seq_along(nodes)) {
    ids = nodes[[index]]$ids()
    data.table::setattr(ids, "adversarial", TRUE)
    expect_identical(nodes[[index]]$ids(), expected[[index]])
    expect_null(attr(nodes[[index]]$ids(), "adversarial"))
  }
  expect_identical(base$ids(class = "ParamInt"), "alpha")
})

test_that("R-computed property names do not alias the capsule ID column", {
  set = ps(
    plain = p_int(),
    transformed = p_dbl(1, 10, logscale = TRUE)
  )
  for (property in list(set$has_trafo_param, set$is_logscale)) {
    data.table::setattr(names(property), "adversarial", TRUE)
    expect_identical(set$ids(), c("plain", "transformed"))
  }
  expect_identical(
    set$has_trafo_param,
    c(plain = FALSE, transformed = TRUE)
  )
  expect_identical(
    set$is_logscale,
    c(plain = FALSE, transformed = TRUE)
  )
})
