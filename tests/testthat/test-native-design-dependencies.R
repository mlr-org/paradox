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
    rows = list(),
    columns = character(),
    values = list()
  ))
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
