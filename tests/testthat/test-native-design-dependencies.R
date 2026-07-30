design_dependency_symbol = function() {
  get("C_design_dependency_plan", envir = asNamespace("paradox"))
}

test_that("Design dependency planner is registered and always returns a plan", {
  routines = getDLLRegisteredRoutines("paradox")$.Call
  expect_identical(routines$design_dependency_plan$numParameters, 2L)
  expect_false("design_dependency_runtime" %in% names(routines))
  expect_false("design_dependency_plan_builtin" %in% names(routines))

  parameter_set = ps(x = p_dbl())
  plan = .Call(
    design_dependency_symbol(),
    data.table(x = 1:2),
    parameter_set
  )
  expect_identical(plan, list(
    fixed_columns = character(),
    fixed_values = list(),
    fixed_plain = logical(),
    rows = list(),
    columns = character(),
    values = list(),
    receipt = plan$receipt
  ))
  expect_null(.Call(
    paradox:::C_param_set_generation_receipt,
    plan$receipt
  ))
})

test_that("Design fixed values and dependencies share one native plan", {
  parameter_set = ps(
    parent = p_lgl(),
    child = p_int(0L, 9L),
    opaque = p_uty()
  )
  parameter_set$add_dep("child", "parent", CondEqual(TRUE))
  marker = new.env(parent = emptyenv())
  parameter_set$values = list(parent = FALSE, opaque = marker)

  plan = .Call(
    design_dependency_symbol(),
    data.table(
      parent = c(TRUE, TRUE),
      child = c(1L, 2L),
      opaque = list(NULL, NULL)
    ),
    parameter_set
  )
  expect_identical(plan$fixed_columns, c("parent", "opaque"))
  expect_identical(plan$fixed_values, list(FALSE, marker))
  expect_identical(plan$fixed_plain, c(TRUE, FALSE))
  expect_identical(plan$rows, list(1:2))
  expect_identical(plan$columns, "child")
  expect_identical(plan$values, list(NA_integer_))

  design = Design$new(
    parameter_set,
    data.table(
      parent = c(TRUE, TRUE),
      child = c(1L, 2L),
      opaque = list(NULL, NULL)
    ),
    remove_dupl = FALSE
  )
  expect_identical(design$data$parent, c(FALSE, FALSE))
  expect_identical(design$data$child, c(NA_integer_, NA_integer_))
  expect_identical(design$data$opaque, list(marker, marker))
})

test_that("Design rejects support mutation during its R patch handoff", {
  parameter_set = ps(
    parent = p_lgl(),
    child = p_int(0L, 9L)
  )
  parameter_set$add_dep("child", "parent", CondEqual(TRUE))
  parameter_set$values = list(parent = TRUE)
  caller = data.table(
    parent = c(FALSE, FALSE),
    child = c(1L, 2L)
  )
  original_set = data.table::set
  fired = FALSE

  expect_error(
    testthat::with_mocked_bindings(
      Design$new(parameter_set, caller, remove_dupl = FALSE),
      set = function(...) {
        if (!fired) {
          fired <<- TRUE
          parameter_set$values = list(parent = FALSE)
        }
        original_set(...)
      },
      .package = "paradox"
    ),
    "graph changed during deferred operation",
    fixed = TRUE
  )
  expect_true(fired)
  # The nested mutation wins; only the outer, now-stale Design construction is
  # refused.
  expect_identical(parameter_set$values, list(parent = FALSE))
})

test_that("Design applies chained closed Conditions in stable topology", {
  parameter_set = ps(
    parent = p_fct(c("on", "off")),
    middle = p_int(0, 10),
    leaf = p_dbl(),
    utility = p_uty()
  )
  parameter_set$add_dep("middle", "parent", CondEqual("on"))
  parameter_set$add_dep("leaf", "middle", CondAnyOf(c(1L, 2L)))
  parameter_set$add_dep("utility", "parent", CondEqual("off"))

  caller = data.table(
    parent = factor(c("on", "off", NA), levels = c("on", "off")),
    middle = c(1L, 1L, 2L),
    leaf = c(10, 20, 30),
    utility = list("a", "b", "c")
  )
  design = Design$new(parameter_set, caller, remove_dupl = FALSE)
  expect_identical(design$data$middle, c(1L, NA_integer_, NA_integer_))
  expect_identical(design$data$leaf, c(10, NA_real_, NA_real_))
  expect_identical(design$data$utility, list(NA, "b", NA))
  # Design intentionally owns the caller's public by-reference table.
  expect_identical(caller, design$data)
})

test_that("Design dependencies consume live collection and Shadow graphs", {
  child = ps(parent = p_lgl(), value = p_int(0, 10))
  child$add_dep("value", "parent", CondEqual(TRUE))
  collection = psc(left = child)
  collection_data = data.table(
    left.parent = c(TRUE, FALSE),
    left.value = c(1L, 2L)
  )
  collection_design = Design$new(
    collection,
    collection_data,
    remove_dupl = FALSE
  )
  expect_identical(collection_design$data$left.value, c(1L, NA_integer_))

  origin = ps(parent = p_lgl(), visible = p_dbl(), hidden = p_int())
  origin$add_dep("visible", "parent", CondEqual(TRUE))
  shadow = ParamSetShadow$new(origin, "hidden")
  shadow_design = Design$new(
    shadow,
    data.table(parent = c(TRUE, FALSE), visible = c(1, 2)),
    remove_dupl = FALSE
  )
  expect_identical(shadow_design$data$visible, c(1, NA_real_))
})

test_that("Design rejects same-core mutation in a nested Shadow snapshot", {
  origin = ps(parent = p_lgl(), visible = p_dbl(), hidden = p_int())
  origin$add_dep("visible", "parent", CondEqual(TRUE))
  shadow = ParamSetShadow$new(origin, "hidden")
  collection = psc(left = shadow)

  signature = attr(
    shadow$.__enclos_env__$private$.core,
    ".paradox.shadow.snapshot.v1",
    exact = TRUE
  )
  replacement = ps(other = p_int())$.__enclos_env__$private$.core
  mutate_signature = function() {
    pending = .Call(
      paradox:::C_test_gc_column_mutator,
      signature,
      1L,
      replacement
    )
    rm(pending)
    invisible(gc(full = TRUE))
  }
  parent = native_stateful_altrep(
    TRUE,
    TRUE,
    callback = mutate_signature,
    callback_after = 0L
  )
  data = structure(
    list(left.parent = parent, left.visible = 1),
    class = "data.frame",
    row.names = c(NA_integer_, -1L)
  )

  expect_error(
    .Call(design_dependency_symbol(), data, collection),
    "ParamSet graph changed during Design construction",
    fixed = TRUE
  )
})

test_that("dependency planner rejects cycles and malformed Design columns", {
  cyclic = ps(a = p_lgl(), b = p_lgl())
  cyclic$add_dep("a", "b", CondEqual(TRUE))
  cyclic$add_dep("b", "a", CondEqual(TRUE))
  expect_error(
    Design$new(cyclic, data.table(a = TRUE, b = TRUE), FALSE),
    "cycle"
  )

  ordinary = ps(a = p_lgl(), b = p_int())
  ordinary$add_dep("b", "a", CondEqual(TRUE))
  expect_error(
    .Call(
      design_dependency_symbol(),
      structure(list(a = TRUE, changed = 1L), names = c("a", "changed")),
      ordinary
    ),
    "do not match"
  )
})
