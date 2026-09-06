test_that("capsule and diagnostic paths survive forced collection", {
  skip_on_cran()

  base = ps(value = p_int(0L, 1L))
  state = paradox:::param_set_core_state(base$.__enclos_env__$private)
  active_private = new.env(parent = emptyenv())
  active_reads = 0L
  makeActiveBinding(".core", function(value) {
    if (!missing(value)) {
      stop("test binding is read-only")
    }
    active_reads <<- active_reads + 1L
    .Call(paradox:::C_param_set_core_new, 1L, state)
  }, active_private)

  checked = ps(
    value = p_uty(custom_check = function(x) {
      "gr\u00fcndlich rejected"
    })
  )
  dependency = ps(
    child = p_lgl(),
    parent = p_uty(custom_check = function(x) "rejected")
  )
  required = ps(
    zeta = p_int(tags = "required"),
    alpha = p_lgl(tags = "required")
  )
  leaf = ps(number = p_int(tags = "numeric"))

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  active_message = tryCatch({
    .Call(paradox:::C_param_set_core_state, active_private, NULL)
    NA_character_
  }, error = conditionMessage)
  value_message = tryCatch({
    checked$values = list(value = 1L)
    NA_character_
  }, error = conditionMessage)
  dependency_message = tryCatch({
    dependency$add_dep("child", "parent", CondEqual(1L))
    NA_character_
  }, error = conditionMessage)
  required_message = tryCatch({
    required$get_values()
    NA_character_
  }, error = conditionMessage)
  collection = ParamSetCollection$new(
    list(owner = leaf),
    tag_sets = TRUE,
    tag_params = TRUE
  )
  gctorture(previous)

  expect_match(active_message, "Corrupt ParamSet|missing versioned core")
  expect_identical(active_reads, 0L)
  expect_match(value_message, "gr\u00fcndlich rejected", fixed = TRUE)
  expect_match(
    dependency_message,
    "Condition has infeasible values for parent",
    fixed = TRUE
  )
  expect_match(
    required_message,
    "Missing required parameters: zeta, alpha",
    fixed = TRUE
  )
  expect_identical(collection$ids(), "owner.number")
  expect_true(all(
    c("set_owner", "param_number") %in% collection$tags[["owner.number"]]
  ))
})
