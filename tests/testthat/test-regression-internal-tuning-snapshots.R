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

test_that("root and owner values come from one collection generation", {
  left = ps(value = p_int(), context = p_int())
  right = ps(value = p_int(), context = p_int())
  left$values = list(value = 1L, context = 10L)
  right$values = list(value = 2L, context = 20L)
  collection = psc(left = left, right = right)

  plan = expect_no_warning(
    paradox:::param_set_internal_tuning_plan(
      collection,
      collection$ids(),
      include_root_values = TRUE
    )
  )
  expect_identical(
    plan$root_values,
    list(
      left.value = 1L,
      left.context = 10L,
      right.value = 2L,
      right.context = 20L
    )
  )
  expect_identical(
    plan$owner_values,
    list(
      list(value = 1L, context = 10L),
      list(value = 2L, context = 20L)
    )
  )

  left$values = list(value = 3L, context = 30L)
  expect_identical(plan$root_values$left.value, 1L)
  expect_identical(plan$owner_values[[1L]]$value, 1L)
  expect_error(
    .Call(
      paradox:::C_param_set_internal_tuning_receipt,
      plan$receipt
    ),
    "graph changed"
  )
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

test_that("callback-free flatten skips route planning and preserves cargo leaves", {
  aggregation = function(x) length(x)
  child = ps(
    value = p_int(tags = "internal_tuning", aggr = aggregation),
    plain = p_lgl()
  )
  collection = psc(owner = child)
  source = paradox:::param_set_core_state(
    collection$.__enclos_env__$private,
    collection
  )

  flattened = testthat::with_mocked_bindings(
    collection$flatten(),
    param_set_internal_tuning_plan = function(...) {
      stop("callback-free flatten entered route planning", call. = FALSE)
    },
    .package = "paradox"
  )
  detached = paradox:::param_set_core_state(
    flattened$.__enclos_env__$private,
    flattened
  )

  expect_identical(
    data.table::address(detached$.params$cargo[[1L]]),
    data.table::address(source$.params$cargo[[1L]])
  )
  expect_identical(detached$.params$cargo[[1L]]$aggr, aggregation)

  shadow = ParamSetShadow$new(child, "plain")
  shadow_flattened = testthat::with_mocked_bindings(
    shadow$flatten(),
    param_set_internal_tuning_plan = function(...) {
      stop("callback-free Shadow flatten entered route planning", call. = FALSE)
    },
    .package = "paradox"
  )
  shadow_detached = paradox:::param_set_core_state(
    shadow_flattened$.__enclos_env__$private,
    shadow_flattened
  )
  expect_identical(
    data.table::address(shadow_detached$.params$cargo[[1L]]),
    data.table::address(source$.params$cargo[[1L]])
  )
})

test_that("a callback-free flatten rejects newly introduced metadata", {
  child = ps(tune = p_int(), gate = p_lgl())
  collection = psc(owner = child)
  replacement = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, values) domain$upper,
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  replacement_state = paradox:::param_set_core_state(
    replacement$.__enclos_env__$private,
    replacement
  )
  original_rows = paradox:::param_set_internal_tuning_rows
  calls = 0L

  expect_error(
    testthat::with_mocked_bindings(
      collection$flatten(),
      param_set_internal_tuning_rows = function(params) {
        calls <<- calls + 1L
        if (calls == 1L) {
          paradox:::param_set_core_replace(
            child$.__enclos_env__$private,
            params = replacement_state$.params,
            tags = replacement_state$.tags,
            trafos = replacement_state$.trafos
          )
          return(integer())
        }
        original_rows(params)
      },
      .package = "paradox"
    ),
    "Internal-tuning metadata changed while the ParamSet was flattened",
    fixed = TRUE
  )
  expect_gte(calls, 2L)
})

test_that("callback-free Shadow flatten selects all IDs natively", {
  origin = psc(base = ps(x = p_int(), hidden = p_lgl()))
  shadow = ParamSetShadow$new(origin, "base.hidden")
  original_rows = paradox:::param_set_internal_tuning_rows
  calls = 0L

  flattened = testthat::with_mocked_bindings(
    shadow$flatten(),
    param_set_internal_tuning_rows = function(params) {
      calls <<- calls + 1L
      if (calls == 1L) {
        origin$add(ps(y = p_dbl()), "late")
        return(integer())
      }
      original_rows(params)
    },
    .package = "paradox"
  )

  expect_identical(flattened$ids(), c("base.x", "late.y"))
  expect_gte(calls, 2L)
})

test_that("flatten rejects a source mutation after its all-ID route plan", {
  child = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, values) domain$upper,
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  collection = psc(owner = child)
  original_plan = paradox:::param_set_internal_tuning_plan

  expect_error(
    testthat::with_mocked_bindings(
      collection$flatten(),
      param_set_internal_tuning_plan = function(...) {
        plan = original_plan(...)
        child$values = list(gate = TRUE)
        plan
      },
      .package = "paradox"
    ),
    "graph changed during internal-tuning operation",
    fixed = TRUE
  )
})

test_that("internal-tuning receipts reject a moved graph generation", {
  param_set = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, values) domain$upper,
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  plan = paradox:::param_set_internal_tuning_plan(
    param_set,
    param_set$ids()
  )
  expect_null(.Call(
    paradox:::C_param_set_internal_tuning_receipt,
    plan$receipt
  ))

  param_set$values = list(gate = TRUE)
  expect_error(
    .Call(
      paradox:::C_param_set_internal_tuning_receipt,
      plan$receipt
    ),
    "graph changed"
  )
})

test_that("internal-tuning receipts bind the exact Shadow metadata carrier", {
  first_origin = ps(hidden = p_int(), tune = p_int())
  second_origin = ps(hidden = p_int(), tune = p_int())
  first = ParamSetShadow$new(first_origin, "hidden")
  second = ParamSetShadow$new(second_origin, "hidden")
  plan = paradox:::param_set_internal_tuning_plan(first, first$ids())

  private = first$.__enclos_env__$private
  replacement = attr(
    second$.__enclos_env__$private$.core,
    ".paradox.shadow.snapshot.v1",
    exact = TRUE
  )
  attr(private$.core, ".paradox.shadow.snapshot.v1") = replacement

  expect_error(
    .Call(
      paradox:::C_param_set_internal_tuning_receipt,
      plan$receipt
    ),
    "graph changed"
  )
})

test_that("a Collection can atomically disable Shadow-hidden controls", {
  make_leaf = function(disabled) {
    leaf = ps(
      tune = p_int(
        tags = "internal_tuning",
        in_tune_fn = function(domain, values) domain$upper,
        aggr = function(x) x[[1L]],
        disable_in_tune = list(gate = disabled)
      ),
      gate = p_lgl()
    )
    leaf$values = list(gate = TRUE)
    leaf
  }

  left = make_leaf(FALSE)
  right = make_leaf(FALSE)
  collection = psc(
    left = ParamSetShadow$new(left, "gate"),
    right = ParamSetShadow$new(right, "gate")
  )
  collection$disable_internal_tuning(c("left.tune", "right.tune"))
  expect_identical(left$values$gate, FALSE)
  expect_identical(right$values$gate, FALSE)
})

test_that("duplicate disable controls use selected-ID order with last wins", {
  param_set = ps(
    first = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, values) domain$upper,
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    second = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, values) domain$upper,
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = TRUE)
    ),
    gate = p_lgl()
  )

  param_set$disable_internal_tuning(c("first", "second"))
  expect_identical(param_set$values$gate, TRUE)
  param_set$disable_internal_tuning(c("second", "first"))
  expect_identical(param_set$values$gate, FALSE)
})

test_that("hidden-control owner writes roll back together on validation error", {
  make_leaf = function(disabled) {
    leaf = ps(
      tune = p_int(
        tags = "internal_tuning",
        in_tune_fn = function(domain, values) domain$upper,
        aggr = function(x) x[[1L]],
        disable_in_tune = list(gate = disabled)
      ),
      gate = p_lgl()
    )
    leaf$values = list(gate = TRUE)
    leaf
  }

  left = make_leaf(FALSE)
  right = make_leaf("not logical")
  collection = psc(
    left = ParamSetShadow$new(left, "gate"),
    right = ParamSetShadow$new(right, "gate")
  )
  expect_error(
    collection$disable_internal_tuning(c("left.tune", "right.tune")),
    "right.gate|gate|logical"
  )
  expect_identical(left$values$gate, TRUE)
  expect_identical(right$values$gate, TRUE)
})

test_that("a direct Shadow disables through complete origin constraint semantics", {
  origin = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, values) domain$upper,
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  origin$values = list(gate = TRUE)
  origin$constraint = function(x) identical(x$gate, FALSE)
  shadow = ParamSetShadow$new(origin, "gate")

  shadow$disable_internal_tuning("tune")
  expect_identical(origin$values$gate, FALSE)
})

test_that("a direct Shadow retains its own assignment policy", {
  make_shadow = function(shadow_policy, origin_policy) {
    origin = ps(
      tune = p_int(
        tags = "internal_tuning",
        in_tune_fn = function(domain, values) domain$upper,
        aggr = function(x) x[[1L]],
        disable_in_tune = list(gate = "unchecked")
      ),
      gate = p_lgl()
    )
    origin$assert_values = origin_policy
    shadow = ParamSetShadow$new(origin, "gate")
    shadow$assert_values = shadow_policy
    list(origin = origin, shadow = shadow)
  }

  unchecked = make_shadow(FALSE, TRUE)
  unchecked$shadow$disable_internal_tuning("tune")
  expect_identical(unchecked$origin$values$gate, "unchecked")

  checked = make_shadow(TRUE, FALSE)
  expect_error(
    checked$shadow$disable_internal_tuning("tune"),
    "logical"
  )
  expect_identical(
    checked$origin$values,
    structure(list(), names = character())
  )
})

test_that("nested hidden controls with composed constraints fail closed", {
  origin = ps(
    tune = p_int(
      tags = "internal_tuning",
      in_tune_fn = function(domain, values) domain$upper,
      aggr = function(x) x[[1L]],
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  origin$values = list(gate = TRUE)
  origin$constraint = function(x) TRUE
  collection = psc(view = ParamSetShadow$new(origin, "gate"))

  expect_error(
    collection$disable_internal_tuning("view.tune"),
    "nested ParamSetShadow.*constraint"
  )
  expect_identical(origin$values$gate, TRUE)
})
