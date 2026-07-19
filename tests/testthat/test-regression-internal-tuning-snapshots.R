internal_tuning_detached_params = function(param_set) {
  state = paradox:::param_set_core_state(
    param_set$.__enclos_env__$private
  )
  paradox:::param_set_table_rows(
    state$.params,
    seq_len(nrow(state$.params))
  )
}

internal_tuning_replace_params = function(param_set, params) {
  paradox:::param_set_core_replace(
    param_set$.__enclos_env__$private,
    params = params
  )
}

test_that("base conversion captures cargo, values, and Domains", {
  param_set = NULL
  search_space = NULL
  replacement = function(domain, param_vals) -100
  first = function(domain, param_vals) {
    params = internal_tuning_detached_params(param_set)
    row = match("second", params$id)
    cargo = params$cargo[[row]]
    cargo$in_tune_fn = replacement
    params$cargo[[row]] = cargo
    internal_tuning_replace_params(param_set, params)
    param_set$values = list(context = 99L)

    search_params = internal_tuning_detached_params(search_space)
    search_params$upper[search_params$id == "second"] = 999
    internal_tuning_replace_params(search_space, search_params)
    domain$upper + param_vals$context
  }
  second = function(domain, param_vals) domain$upper + param_vals$context
  param_set = ps(
    first = p_int(
      tags = "internal_tuning",
      in_tune_fn = first,
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    second = p_int(
      tags = "internal_tuning",
      in_tune_fn = second,
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    context = p_int()
  )
  param_set$values = list(context = 1L)
  search_space = ps(
    first = p_int(0L, 10L),
    second = p_int(0L, 20L)
  )

  expect_equal(
    param_set$convert_internal_search_space(search_space),
    list(first = 11, second = 21)
  )
  expect_identical(param_set$values, list(context = 99L))
  expect_identical(search_space$upper[["second"]], 999)
})

test_that("inherited Shadow conversion captures refreshed origin values", {
  origin = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, param_vals) {
        domain$upper + param_vals$context
      },
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    context = p_int()
  )
  origin$values = list(context = 1L)
  shadow = ParamSetShadow$new(origin, character())
  origin$values = list(context = 2L)
  search_space = ps(tune = p_int(0L, 10L))

  expect_equal(
    shadow$convert_internal_search_space(search_space),
    list(tune = 12)
  )
})

test_that("collection conversion captures root cargo, Domains, and owner values", {
  collection = NULL
  search_space = NULL
  right = NULL
  replacement = function(domain, param_vals) -100
  trigger = function(domain, param_vals) {
    params = internal_tuning_detached_params(collection)
    row = match("right.late", params$id)
    cargo = params$cargo[[row]]
    cargo$in_tune_fn = replacement
    params$cargo[[row]] = cargo
    internal_tuning_replace_params(collection, params)
    right$values = list(context = 99L)

    search_params = internal_tuning_detached_params(search_space)
    search_params$upper[search_params$id != "left.trigger"] = 999
    internal_tuning_replace_params(search_space, search_params)
    domain$upper + param_vals$context
  }
  original = function(domain, param_vals) domain$upper + param_vals$context
  left = ps(
    trigger = p_int(
      tags = "internal_tuning",
      in_tune_fn = trigger,
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    context = p_int()
  )
  right = ps(
    value = p_int(
      tags = "internal_tuning",
      in_tune_fn = original,
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    late = p_int(
      tags = "internal_tuning",
      in_tune_fn = original,
      aggr = function(x) x[[1L]],
      disable_in_tune = list()
    ),
    context = p_int()
  )
  left$values = list(context = 1L)
  right$values = list(context = 2L)
  collection = psc(left = left, right = right)
  search_space = ps(
    left.trigger = p_int(0L, 10L),
    right.value = p_int(0L, 20L),
    right.late = p_int(0L, 30L)
  )

  expect_equal(
    collection$convert_internal_search_space(search_space),
    list(left.trigger = 11, right.value = 22, right.late = 32)
  )
  expect_identical(right$values, list(context = 99L))
  expect_identical(search_space$upper[["right.value"]], 999)
  expect_identical(search_space$upper[["right.late"]], 999)
})

test_that("flatten rewrites an owned cargo column and preserves other leaves", {
  converter = function(domain, param_vals) domain$upper
  untouched_aggr = function(x) length(x)
  child = ps(
    converted = p_int(
      tags = "internal_tuning",
      in_tune_fn = converter,
      aggr = function(x) x[[1L]],
      disable_in_tune = list(flag = FALSE)
    ),
    untouched = p_int(tags = "internal_tuning", aggr = untouched_aggr),
    flag = p_lgl()
  )
  collection = psc(owner = child)
  collection_private = collection$.__enclos_env__$private
  origin = paradox:::param_set_core_state(collection_private)
  origin_params_address = data.table::address(origin$.params)
  origin_cargo_address = data.table::address(origin$.params$cargo)
  origin_untouched = origin$.params$cargo[[2L]]

  flattened = collection$flatten()
  flat_private = flattened$.__enclos_env__$private
  flat = paradox:::param_set_core_state(flat_private)
  expect_false(identical(
    data.table::address(flat$.params$cargo),
    origin_cargo_address
  ))
  expect_false(identical(
    data.table::address(flat$.params$cargo[[1L]]),
    data.table::address(origin$.params$cargo[[1L]])
  ))
  expect_identical(
    data.table::address(flat$.params$cargo[[2L]]),
    data.table::address(origin_untouched)
  )

  flat_params = paradox:::param_set_table_rows(
    flat$.params,
    seq_len(nrow(flat$.params))
  )
  changed = flat_params$cargo[[2L]]
  changed$aggr = function(x) -1L
  flat_params$cargo[[2L]] = changed
  paradox:::param_set_core_replace(flat_private, params = flat_params)

  expect_identical(
    data.table::address(
      paradox:::param_set_core_state(collection_private)$.params
    ),
    origin_params_address
  )
  expect_identical(
    data.table::address(
      paradox:::param_set_core_state(collection_private)$.params$cargo
    ),
    origin_cargo_address
  )
  expect_identical(origin$.params$cargo[[2L]]$aggr, untouched_aggr)
})
