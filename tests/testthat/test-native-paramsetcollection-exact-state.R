collection_exact_params_symbol = function() {
  get(
    "C_param_set_collection_params",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

collection_exact_values_symbol = function() {
  get(
    "C_param_set_collection_values",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

collection_exact_fixture = function() {
  child = ps(
    first = p_int(init = 1L),
    second = p_lgl(init = FALSE)
  )
  child$values = list(first = 2L, second = TRUE)
  ParamSetCollection$new(list(owner = child))
}

collection_exact_private = function(collection) {
  collection$.__enclos_env__$private
}

collection_exact_replace_column = function(table, name, value) {
  table_attributes = attributes(table)
  result = unclass(table)
  result[[name]] = value
  attributes(result) = table_attributes
  result
}

collection_exact_replace_attribute = function(object, name, value) {
  object_attributes = attributes(object)
  object_attributes[[name]] = value
  attributes(object) = object_attributes
  object
}

collection_exact_expect_both_null = function(collection, info = NULL) {
  private = collection_exact_private(collection)
  expect_null(
    .Call(collection_exact_params_symbol(), private, collection),
    info = info
  )
  expect_null(
    .Call(collection_exact_values_symbol(), private, collection),
    info = info
  )
}

test_that("collection exact-state shells decline before ALTREP Length", {
  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("collection exact-state validation invoked ALTREP Length", call. = FALSE)
  }
  trap = function(value) {
    native_stateful_altrep(
      value,
      value,
      callback = callback,
      callback_after = NA_integer_
    )
  }
  arm_length = function(value) {
    native_stateful_altrep_rearm(value, c(NA_integer_, 0L))
    callbacks <<- 0L
    invisible(NULL)
  }

  cases = list(
    sets_shell = function(collection) {
      private = collection_exact_private(collection)
      changed = trap(private$.sets)
      private$.sets = changed
      arm_length(changed)
    },
    set_names = function(collection) {
      private = collection_exact_private(collection)
      sets = private$.sets
      changed = trap(names(sets))
      private$.sets = collection_exact_replace_attribute(
        sets,
        "names",
        changed
      )
      arm_length(changed)
    },
    postfix = function(collection) {
      private = collection_exact_private(collection)
      changed = trap(private$.postfix)
      private$.postfix = changed
      arm_length(changed)
    },
    translation_shell = function(collection) {
      private = collection_exact_private(collection)
      changed = trap(private$.translation)
      private$.translation = changed
      arm_length(changed)
    },
    translation_names = function(collection) {
      private = collection_exact_private(collection)
      table = private$.translation
      changed = trap(names(table))
      private$.translation = collection_exact_replace_attribute(
        table,
        "names",
        changed
      )
      arm_length(changed)
    },
    translation_sorted = function(collection) {
      private = collection_exact_private(collection)
      table = private$.translation
      changed = trap(attr(table, "sorted", exact = TRUE))
      private$.translation = collection_exact_replace_attribute(
        table,
        "sorted",
        changed
      )
      arm_length(changed)
    },
    translation_index = function(collection) {
      private = collection_exact_private(collection)
      table = private$.translation
      changed = trap(attr(table, "index", exact = TRUE))
      private$.translation = collection_exact_replace_attribute(
        table,
        "index",
        changed
      )
      arm_length(changed)
    },
    translation_index_cache = function(collection) {
      private = collection_exact_private(collection)
      table = private$.translation
      index = attr(table, "index", exact = TRUE)
      changed = trap(attr(index, "__original_id", exact = TRUE))
      index = collection_exact_replace_attribute(
        index,
        "__original_id",
        changed
      )
      private$.translation = collection_exact_replace_attribute(
        table,
        "index",
        index
      )
      arm_length(changed)
    },
    params_shell = function(collection) {
      private = collection_exact_private(collection)
      changed = trap(private$.params)
      private$.params = changed
      arm_length(changed)
    },
    params_names = function(collection) {
      private = collection_exact_private(collection)
      table = private$.params
      changed = trap(names(table))
      private$.params = collection_exact_replace_attribute(
        table,
        "names",
        changed
      )
      arm_length(changed)
    },
    params_index = function(collection) {
      private = collection_exact_private(collection)
      table = private$.params
      changed = trap(attr(table, "index", exact = TRUE))
      private$.params = collection_exact_replace_attribute(
        table,
        "index",
        changed
      )
      arm_length(changed)
    },
    params_index_cache = function(collection) {
      private = collection_exact_private(collection)
      table = private$.params
      index = attr(table, "index", exact = TRUE)
      changed = trap(attr(index, "__id__cls__grouping", exact = TRUE))
      index = collection_exact_replace_attribute(
        index,
        "__id__cls__grouping",
        changed
      )
      private$.params = collection_exact_replace_attribute(
        table,
        "index",
        index
      )
      arm_length(changed)
    },
    values_shell = function(collection) {
      child_private = collection$sets[[1L]]$.__enclos_env__$private
      changed = trap(child_private$.values)
      child_private$.values = changed
      arm_length(changed)
    },
    value_names = function(collection) {
      child_private = collection$sets[[1L]]$.__enclos_env__$private
      values = child_private$.values
      changed = trap(names(values))
      child_private$.values = collection_exact_replace_attribute(
        values,
        "names",
        changed
      )
      arm_length(changed)
    }
  )

  for (case_name in names(cases)) {
    collection = collection_exact_fixture()
    cases[[case_name]](collection)
    collection_exact_expect_both_null(collection, case_name)
    expect_identical(callbacks, 0L, info = case_name)
  }

  for (column in names(collection_exact_private(
      collection_exact_fixture()
    )$.translation)) {
    collection = collection_exact_fixture()
    private = collection_exact_private(collection)
    changed = trap(private$.translation[[column]])
    private$.translation = collection_exact_replace_column(
      private$.translation,
      column,
      changed
    )
    arm_length(changed)
    case_name = paste0("translation column `", column, "`")
    collection_exact_expect_both_null(collection, case_name)
    expect_identical(callbacks, 0L, info = case_name)
  }

  for (column in names(collection_exact_private(
      collection_exact_fixture()
    )$.params)) {
    collection = collection_exact_fixture()
    private = collection_exact_private(collection)
    changed = trap(private$.params[[column]])
    private$.params = collection_exact_replace_column(
      private$.params,
      column,
      changed
    )
    arm_length(changed)
    case_name = paste0("params column `", column, "`")
    collection_exact_expect_both_null(collection, case_name)
    expect_identical(callbacks, 0L, info = case_name)
  }
})

test_that("collection values ignores hostile row-name representation", {
  callbacks = 0L
  collection = collection_exact_fixture()
  private = collection_exact_private(collection)
  expected = .Call(collection_exact_values_symbol(), private, collection)
  row_names = native_stateful_altrep(
    c(1L, 2L),
    c(2L, 1L),
    callback = function() {
      callbacks <<- callbacks + 1L
      stop("collection values invoked row-name ALTREP", call. = FALSE)
    },
    callback_after = NA_integer_
  )
  private$.params = collection_exact_replace_attribute(
    private$.params,
    "row.names",
    row_names
  )
  native_stateful_altrep_rearm(row_names, c(NA_integer_, 0L))

  observed = .Call(collection_exact_values_symbol(), private, collection)
  expect_identical(observed, expected)
  expect_identical(callbacks, 0L)
})

test_that("collection values admits only inert invalidated params indices", {
  collection = collection_exact_fixture()
  private = collection_exact_private(collection)
  expected = .Call(collection_exact_values_symbol(), private, collection)

  saved_attributes = attributes(private$.params)
  data.table::set(
    private$.params,
    1L,
    "id",
    private$.params$id[[1L]]
  )
  attributes(private$.params) = saved_attributes
  expect_identical(attr(private$.params, "index", exact = TRUE), integer())
  expect_null(attributes(attr(private$.params, "index", exact = TRUE)))
  expect_identical(
    .Call(collection_exact_values_symbol(), private, collection),
    expected
  )

  malformed = list(
    nonempty = 1L,
    wrong_attribute = structure(integer(), wrong = integer()),
    extra_attribute = structure(
      integer(),
      `__id__cls__grouping` = integer(),
      extra = integer()
    )
  )
  for (case_name in names(malformed)) {
    collection = collection_exact_fixture()
    private = collection_exact_private(collection)
    private$.params = collection_exact_replace_attribute(
      private$.params,
      "index",
      malformed[[case_name]]
    )
    expect_null(
      .Call(collection_exact_values_symbol(), private, collection),
      info = case_name
    )
  }

  callbacks = 0L
  callback = function() {
    callbacks <<- callbacks + 1L
    stop("collection values invoked malformed index ALTREP", call. = FALSE)
  }
  collection = collection_exact_fixture()
  private = collection_exact_private(collection)
  index = native_stateful_altrep(
    integer(),
    1L,
    callback = callback,
    callback_after = NA_integer_
  )
  private$.params = collection_exact_replace_attribute(
    private$.params,
    "index",
    index
  )
  native_stateful_altrep_rearm(index, c(NA_integer_, 0L))
  expect_null(.Call(collection_exact_values_symbol(), private, collection))
  expect_identical(callbacks, 0L)

  collection = collection_exact_fixture()
  private = collection_exact_private(collection)
  cache = native_stateful_altrep(
    integer(),
    1L,
    callback = callback,
    callback_after = NA_integer_
  )
  index = integer()
  index = collection_exact_replace_attribute(
    index,
    "__id__cls__grouping",
    cache
  )
  private$.params = collection_exact_replace_attribute(
    private$.params,
    "index",
    index
  )
  native_stateful_altrep_rearm(cache, c(NA_integer_, 0L))
  expect_null(.Call(collection_exact_values_symbol(), private, collection))
  expect_identical(callbacks, 0L)
})

test_that("deep collection snapshots survive root and graph growth", {
  skip_on_cran()

  marker = new.env(parent = emptyenv())
  marker$value = 42L
  leaves = lapply(seq_len(12L), function(index) {
    result = ps(
      number = p_int(init = index),
      marker = p_uty(init = marker)
    )
    result$values = list(number = index, marker = marker)
    result
  })
  collection = ParamSetCollection$new(setNames(
    leaves,
    sprintf("leaf%02d", seq_along(leaves))
  ))
  for (depth in seq_len(18L)) {
    collection = ParamSetCollection$new(setNames(
      list(collection),
      sprintf("depth%02d", depth)
    ))
  }
  private = collection_exact_private(collection)
  expected_params = .Call(
    collection_exact_params_symbol(),
    private,
    collection
  )
  expected_values = .Call(
    collection_exact_values_symbol(),
    private,
    collection
  )
  expect_false(is.null(expected_params))
  expect_false(is.null(expected_values))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed_params = .Call(
    collection_exact_params_symbol(),
    private,
    collection
  )
  observed_values = .Call(
    collection_exact_values_symbol(),
    private,
    collection
  )
  gctorture(previous)

  expect_identical(observed_params, expected_params)
  expect_identical(observed_values, expected_values)
  expect_true(any(vapply(
    observed_values,
    identical,
    logical(1L),
    marker
  )))
})
