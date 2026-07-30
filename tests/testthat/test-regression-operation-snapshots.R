test_that("LHS preserves caller identity after an exact source receipt", {
  ordinary = ps(x = p_dbl(0, 1))
  ordinary_design = generate_design_lhs(
    ordinary,
    1L,
    lhs_fun = function(n, k) matrix(0.25, nrow = n, ncol = k)
  )
  expect_identical(ordinary_design$param_set, ordinary)

  source = ps(x = p_dbl(0, 1))
  expect_error(
    generate_design_lhs(
      source,
      2L,
      lhs_fun = function(n, k) {
        source$values = list(x = 0.75)
        matrix(c(0.25, 0.5), nrow = n, ncol = k)
      }
    ),
    "graph changed during internal-tuning operation",
    fixed = TRUE
  )

  expect_identical(source$values, list(x = 0.75))
})

test_that("grid generation spans the R6 Design ownership handoff", {
  source = ps(x = p_dbl(0, 1))
  original_design = Design
  replacement = list(
    new = function(param_set, data, remove_dupl) {
      source$values = list(x = 0.75)
      original_design$new(param_set, data, remove_dupl)
    }
  )

  expect_error(
    testthat::with_mocked_bindings(
      generate_design_grid(source, resolution = 2L),
      Design = replacement,
      .package = "paradox"
    ),
    "graph changed during deferred operation",
    fixed = TRUE
  )
  # The nested mutation wins; the grid selected before it is not returned with
  # the later live support.
  expect_identical(source$values, list(x = 0.75))
})

test_that("grid handoff receipt covers every Collection child generation", {
  child = ps(x = p_dbl(0, 1))
  source = psc(component = child)
  original_design = Design
  replacement = list(
    new = function(param_set, data, remove_dupl) {
      child$values = list(x = 0.75)
      original_design$new(param_set, data, remove_dupl)
    }
  )

  expect_error(
    testthat::with_mocked_bindings(
      generate_design_grid(source, resolution = 2L),
      Design = replacement,
      .package = "paradox"
    ),
    "graph changed during deferred operation",
    fixed = TRUE
  )
  # The child commit wins. The enclosing Collection is intentionally stale
  # until its next semantic entry; only the outer grid handoff is refused.
  expect_identical(child$values, list(x = 0.75))
})

test_that("deep-clone receipts compare all selected cores at one barrier", {
  left = ps(x = p_int())
  right = ps(y = p_lgl())
  shells = unname(list(left, right))
  privates = unname(list(
    left$.__enclos_env__$private,
    right$.__enclos_env__$private
  ))
  selected = unname(lapply(privates, function(private) private$.core))
  policies = unname(lapply(shells, `[[`, "assert_values"))
  receipts = rep(list(NULL), length(selected))

  expect_null(.Call(
    paradox:::C_param_set_deep_clone_receipt,
    shells,
    privates,
    selected,
    policies,
    receipts
  ))
  left$assert_values = FALSE
  expect_error(
    .Call(
      paradox:::C_param_set_deep_clone_receipt,
      shells,
      privates,
      selected,
      policies,
      receipts
    ),
    "graph changed during deep clone",
    fixed = TRUE
  )
  left$assert_values = TRUE
  right$values = list(y = TRUE)
  expect_error(
    .Call(
      paradox:::C_param_set_deep_clone_receipt,
      shells,
      privates,
      selected,
      policies,
      receipts
    ),
    "graph changed during deep clone",
    fixed = TRUE
  )
})

test_that("deep-clone receipts reject a rewired shell/private topology", {
  selected = ps(x = p_int())
  other = ps(y = p_lgl())
  selected_private = selected$.__enclos_env__$private
  selected_core = selected_private$.core

  # This is the state a pending finalizer could leave after R discovery had
  # selected `selected_private`: the shell now points at another private
  # generation while the earlier private/core pair itself remains unchanged.
  rewired = new.env(parent = emptyenv())
  class(rewired) = c("ParamSet", "R6")
  rewired$assert_values = selected$assert_values
  enclosure = new.env(parent = emptyenv())
  enclosure$self = rewired
  enclosure$private = other$.__enclos_env__$private
  rewired$.__enclos_env__ = enclosure

  expect_error(
    .Call(
      paradox:::C_param_set_deep_clone_receipt,
      list(rewired),
      list(selected_private),
      list(selected_core),
      list(selected$assert_values),
      list(NULL)
    ),
    "graph changed during deep clone",
    fixed = TRUE
  )
})

test_that("deep-clone receipts detect same-core Shadow carrier mutation", {
  origin = ps(x = p_int(), hidden = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  private = shadow$.__enclos_env__$private
  core = private$.core
  receipt = .Call(
    paradox:::C_param_set_deep_clone_shadow_receipt,
    core
  )
  signature = attr(
    core,
    ".paradox.shadow.snapshot.v1",
    exact = TRUE
  )
  replacement = ps(other = p_int())$.__enclos_env__$private$.core
  pending = .Call(
    paradox:::C_test_gc_column_mutator,
    signature,
    1L,
    replacement
  )
  rm(pending)
  invisible(gc(full = TRUE))

  expect_error(
    .Call(
      paradox:::C_param_set_deep_clone_receipt,
      list(shadow),
      list(private),
      list(core),
      list(shadow$assert_values),
      list(receipt)
    ),
    "graph changed during deep clone",
    fixed = TRUE
  )
})

test_that("deep clones retain shared graph identity after terminal admission", {
  shared = ps(value = p_uty())
  collection = ParamSetCollection$new(
    list(first = shared, second = shared)
  )
  clone = collection$clone(deep = TRUE)

  clone_state = paradox:::param_set_core_state(
    clone$.__enclos_env__$private,
    clone
  )
  expect_identical(clone_state$.sets[[1L]], clone_state$.sets[[2L]])
  expect_false(identical(clone_state$.sets[[1L]], shared))
})

make_clone_policy_mutator = function(target, replacement) {
  value = new.env(parent = emptyenv())
  class(value) = c("paradox_clone_policy_mutator", "R6")
  value$clone = function(deep = FALSE) {
    target$assert_values = replacement
    new.env(parent = emptyenv())
  }
  value
}

test_that("deep-clone callbacks cannot mix a root capsule with later policy", {
  source = ps(value = p_uty())
  source$values = list(
    value = make_clone_policy_mutator(source, FALSE)
  )

  clone = source$clone(deep = TRUE)

  expect_false(source$assert_values)
  expect_true(clone$assert_values)
  expect_false(identical(clone$values$value, source$values$value))
})

test_that("deep-clone callbacks cannot mix a child capsule with later policy", {
  target = ps(value = p_int())
  mutator = ps(value = p_uty())
  mutator$values = list(
    value = make_clone_policy_mutator(target, FALSE)
  )
  source = ParamSetCollection$new(list(
    mutator = mutator,
    target = target
  ))

  clone = source$clone(deep = TRUE)

  expect_false(target$assert_values)
  expect_true(clone$sets$target$assert_values)
  expect_identical(clone$sets$target$values, named_list())
})

test_that("native graph plans tolerate admitted semantic ALTREP reentry", {
  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )

  first = ps(x = p_int(0L, 100L))
  second = ps(y = p_int(0L, 100L))
  collection = ParamSetCollection$new(list(first = first, second = second))
  counter = 0L
  value = native_stateful_altrep(
    0L,
    0L,
    callback = function() {
      counter <<- counter + 1L
      first$values = list(x = counter)
    },
    callback_after = 0L
  )

  # Atomic point values are an admitted semantic ALTREP position. Reentry may
  # mutate a child after the operation selected its immutable graph
  # generations; it must neither corrupt that plan nor be mistaken for
  # unsupported structural ALTREP state.
  expect_true(
    collection$check(list(first.x = value, second.y = 0L))
  )
  expect_identical(counter, 1L)
  expect_identical(first$values, list(x = 1L))
})

test_that("omitted subspace IDs are selected inside the native transaction", {
  source = ParamSetCollection$new(list(
    first = ps(x = p_int())
  ))
  original = paradox:::param_set_subspace_shells
  fired = FALSE

  subspaces = testthat::with_mocked_bindings(
    source$subspaces(),
    param_set_subspace_shells = function(
        param_set,
        private,
        ids = NULL,
        select_all = FALSE
    ) {
      expect_true(select_all)
      if (!fired) {
        fired <<- TRUE
        source$add(ps(y = p_lgl()), "late")
      }
      original(
        param_set,
        private,
        ids,
        select_all
      )
    },
    .package = "paradox"
  )

  expect_true(fired)
  expect_identical(names(subspaces), c("first.x", "late.y"))
  expect_identical(
    unname(vapply(subspaces, function(param_set) param_set$ids(), "")),
    c("first.x", "late.y")
  )

  # The omission-preserving branch lives in the lean R6 stub and therefore
  # has to survive the representation used by serialized current objects.
  restored = unserialize(serialize(source, NULL))
  restored_subspaces = restored$subspaces()
  expect_identical(names(restored_subspaces), c("first.x", "late.y"))
})

test_that("default search-space values and Domains use one native snapshot", {
  source = ps(
    tune = p_dbl(0, 1),
    fixed = p_lgl()
  )
  source$values = list(
    tune = to_tune(),
    fixed = TRUE
  )

  default = source$search_space()
  explicit = source$search_space(source$values)
  expect_equal(default, explicit)
  expect_identical(default$ids(), "tune")

  # The public lean stub must preserve the established reflected default while
  # forwarding an omitted value argument as genuinely missing. Otherwise it
  # evaluates `$values` before the native Domain snapshot.
  expect_identical(
    paste(deparse(formals(source$search_space)$values), collapse = ""),
    "self$values"
  )
  expect_match(
    paste(deparse(body(source$search_space)), collapse = "\n"),
    "missing\\(values\\)"
  )

  restored = unserialize(serialize(source, NULL))
  expect_equal(restored$search_space(), default)
  expect_identical(
    paste(deparse(formals(restored$search_space)$values), collapse = ""),
    "self$values"
  )
  expect_match(
    paste(deparse(body(restored$search_space)), collapse = "\n"),
    "missing\\(values\\)"
  )
})
