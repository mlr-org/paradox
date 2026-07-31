test_that("composite ParamSet accessors consume one captured state", {
  param_set = ps(
    value = p_int(tags = c("first", "second"), trafo = identity),
    flag = p_lgl()
  )
  private = param_set$.__enclos_env__$private
  state = paradox:::param_set_core_state(private, param_set)

  reads = 0L
  data = testthat::with_mocked_bindings(
    param_set$data,
    param_set_core_state = function(...) {
      reads <<- reads + 1L
      state
    },
    .package = "paradox"
  )
  # `$data` derives every column, including `.tags`, from the one detached
  # `$params` projection; it must never take the R-level capsule-state route,
  # which could pair that projection with a newer graph generation.
  expect_identical(reads, 0L)
  expect_identical(data$tags[[1L]], c("first", "second"))
  expect_identical(data$tags[[2L]], character())

  reads = 0L
  has_trafo_param = testthat::with_mocked_bindings(
    param_set$has_trafo_param,
    param_set_core_state = function(...) {
      reads <<- reads + 1L
      state
    },
    .package = "paradox"
  )
  expect_identical(reads, 1L)
  expect_identical(has_trafo_param, c(value = TRUE, flag = FALSE))

  reads = 0L
  has_trafo = testthat::with_mocked_bindings(
    param_set$has_trafo,
    param_set_core_state = function(...) {
      reads <<- reads + 1L
      state
    },
    .package = "paradox"
  )
  expect_identical(reads, 1L)
  expect_true(has_trafo)
})

test_that("Collection has_trafo sees child callbacks in one native graph", {
  child = ps(value = p_int())
  collection = psc(child = child)
  expect_false(collection$has_trafo)

  child$extra_trafo = function(x) x
  expect_true(collection$has_trafo)

  child$extra_trafo = NULL
  expect_false(collection$has_trafo)
  expect_error(
    .Call(
      paradox:::C_param_set_collection_has_callback,
      collection$.__enclos_env__$private,
      collection,
      3L
    ),
    "selector"
  )
})

test_that("S4 values remain opaque through checked and unchecked graph stores", {
  class_name = "ParadoxAdversarialOpaqueSpecial"
  if (!methods::isClass(class_name)) {
    methods::setClass(class_name, slots = c(payload = "integer"))
  }
  special = methods::new(class_name, payload = 1L)
  unchecked = methods::new(class_name, payload = 2L)

  leaf = ps(value = p_int(special_vals = list(special)))
  collection = psc(owner = leaf)
  collection$values = list(owner.value = special)
  expect_identical(leaf$values$value, special)
  expect_identical(
    collection$get_values(type = "without_token"),
    list(owner.value = special)
  )
  tokens = collection$get_values(type = "only_token")
  expect_length(tokens, 0L)
  expect_identical(names(tokens), character())

  collection$assert_values = FALSE
  collection$values = list(owner.value = unchecked)
  expect_identical(leaf$values$value, unchecked)
  expect_identical(collection$values$owner.value, unchecked)
  expect_identical(
    collection$get_values(type = "without_token")$owner.value,
    unchecked
  )

  utility = ps(value = p_uty())
  utility$values = list(value = special)
  expect_identical(utility$get_values(type = "without_token")$value, special)

  formal_class = "ParadoxAdversarialFormalStoredValue"
  if (!methods::isClass(formal_class)) {
    methods::setClass(formal_class, slots = c(payload = "integer"))
  }
  formal = methods::new(formal_class, payload = 1L)
  utility$values = list(value = formal)
  expect_identical(utility$search_space()$ids(), character())
  expect_identical(
    utility$search_space(list(value = formal))$ids(),
    character()
  )
  expect_error(
    utility$search_space(list(value = asS4(to_tune()))),
    "TuneToken"
  )

  dependent = ps(
    controller = p_int(special_vals = list(special)),
    child = p_int()
  )
  dependent$add_dep("child", "controller", CondEqual$new(1L))
  dependent$values = list(controller = special, child = 1L)
  expect_identical(dependent$get_values(), list(controller = special))

  design = generate_design_grid(
    dependent,
    resolution = 2L,
    upper_limit = 1L
  )
  expect_identical(nrow(design$data), 1L)
  expect_identical(design$transpose(trafo = FALSE)[[1L]]$controller, special)
  expect_false("child" %in% names(design$transpose(trafo = FALSE)[[1L]]))
})
