params_contract_symbol = function() {
  get(
    "C_param_set_params",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
}

params_contract_native = function(param_set) {
  .Call(
    params_contract_symbol(),
    param_set$.__enclos_env__$private,
    param_set
  )
}

params_contract_rich_set = function() {
  marker = new.env(parent = emptyenv())
  marker$value = 1L
  transform = function(x) exp(x)
  custom_check = function(x) TRUE
  param_set = ps(
    factor = p_fct(
      c("slow", "fast"),
      tags = c("choice", "shared"),
      init = "fast"
    ),
    number = p_dbl(
      -2,
      2,
      tolerance = 1e-7,
      tags = c("numeric", "shared"),
      trafo = transform
    ),
    integer = p_int(
      -3L,
      3L,
      tags = "numeric",
      special_vals = list(-99L),
      init = 2L
    ),
    flag = p_lgl(),
    payload = p_uty(custom_check = custom_check, init = marker)
  )
  param_set$add_dep("factor", "flag", CondEqual(TRUE))
  param_set$add_dep("number", "integer", CondAnyOf(c(-1L, 2L)))
  list(
    param_set = param_set,
    marker = marker,
    transform = transform,
    custom_check = custom_check
  )
}

# Install deliberately malformed state without going through a public setter.
# This helper exercises the capsule validator; it is not an extension pattern.
params_contract_forge = function(field, mutate) {
  param_set = ps(
    x = p_dbl(0, 1, tags = "numeric", trafo = exp),
    flag = p_lgl()
  )
  param_set$add_dep("x", "flag", CondEqual(TRUE))
  private = param_set$.__enclos_env__$private
  state = paradox:::param_set_core_state(private)
  field_name = paste0(".", field)
  value = unserialize(serialize(state[[field_name]], NULL, version = 3L))
  replacement = mutate(value)
  .Call(
    paradox:::C_param_set_core_replace,
    private,
    setNames(list(replacement), field_name)
  )
  param_set
}

test_that("params has one forced native capsule interface", {
  symbol = params_contract_symbol()
  expect_s3_class(symbol, "NativeSymbolInfo")
  expect_identical(symbol$numParameters, 2L)
  expect_error(
    .Call("param_set_params", PACKAGE = "paradox"),
    "not available"
  )
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("params rejects malformed direct-call shells and node kinds", {
  symbol = params_contract_symbol()
  set = ps(x = p_dbl())
  private = set$.__enclos_env__$private

  expect_error(
    .Call(symbol, NULL, set),
    "private and self must be environments",
    fixed = TRUE
  )
  expect_error(
    .Call(symbol, private, NULL),
    "private and self must be environments",
    fixed = TRUE
  )

  other = ps(y = p_int())
  expect_error(
    .Call(symbol, private, other),
    "shell ownership",
    fixed = TRUE
  )

  collection = ParamSetCollection$new(list(child = set))
  expect_error(
    .Call(
      symbol,
      collection$.__enclos_env__$private,
      collection
    ),
    "requires a BASE or SHADOW core",
    fixed = TRUE
  )
})

test_that("params exposes the rich public schema from one capsule snapshot", {
  fixture = params_contract_rich_set()
  param_set = fixture$param_set
  observed = params_contract_native(param_set)

  expect_identical(observed, param_set$params)
  expect_s3_class(observed, "data.table")
  expect_identical(class(observed), c("data.table", "data.frame"))
  expect_identical(names(observed), paradox:::domain_names)
  expect_identical(data.table::key(observed), NULL)
  expect_identical(data.table::indices(observed), NULL)
  expect_identical(data.table:::selfrefok(observed, FALSE), 1L)

  expect_identical(
    observed$id,
    c("factor", "number", "integer", "flag", "payload")
  )
  expect_identical(
    observed$cls,
    c("ParamFct", "ParamDbl", "ParamInt", "ParamLgl", "ParamUty")
  )
  expect_identical(
    observed$storage_type,
    c("character", "numeric", "integer", "logical", "list")
  )
  expect_identical(observed$levels[[1L]], c("slow", "fast"))
  expect_identical(observed$levels[[4L]], c(TRUE, FALSE))
  expect_identical(observed$lower[c(2L, 3L)], c(-2, -3))
  expect_identical(observed$upper[c(2L, 3L)], c(2, 3))
  expect_equal(observed$tolerance[[2L]], 1e-7)
  expect_identical(observed$special_vals[[3L]], list(-99L))

  expect_identical(observed$.tags[[1L]], c("choice", "shared"))
  expect_identical(observed$.tags[[2L]], c("numeric", "shared"))
  expect_identical(observed$.tags[[3L]], "numeric")
  expect_identical(observed$.trafo[[2L]], fixture$transform)
  expect_identical(
    observed$cargo[[5L]]$custom_check,
    fixture$custom_check
  )

  factor_requirement = observed$.requirements[[1L]]
  number_requirement = observed$.requirements[[2L]]
  expect_identical(factor_requirement[[1L]], "flag")
  expect_s3_class(factor_requirement[[2L]], "CondEqual")
  expect_identical(factor_requirement[[2L]]$rhs, TRUE)
  expect_identical(number_requirement[[1L]], "integer")
  expect_s3_class(number_requirement[[2L]], "CondAnyOf")
  expect_identical(number_requirement[[2L]]$rhs, c(-1L, 2L))

  expect_identical(observed$.init_given, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_identical(observed$.init[[1L]], "fast")
  expect_identical(observed$.init[[3L]], 2L)
  expect_identical(observed$.init[[5L]], fixture$marker)
})

test_that("params facades detach mutable shells and retain opaque leaves", {
  fixture = params_contract_rich_set()
  param_set = fixture$param_set
  first = param_set$params
  second = param_set$params

  expect_identical(first, second)
  expect_false(identical(
    data.table::address(first),
    data.table::address(second)
  ))
  expect_false(identical(
    data.table::address(names(first)),
    data.table::address(names(second))
  ))
  for (column in names(first)) {
    expect_false(identical(
      data.table::address(first[[column]]),
      data.table::address(second[[column]])
    ), info = column)
  }

  data.table::set(first, i = 2L, j = "lower", value = -100)
  data.table::set(first, j = "added", value = seq_len(nrow(first)))
  first$.tags[[1L]][[1L]] = "changed"
  first$levels[[1L]][[1L]] = "changed"
  first$special_vals[[3L]][[1L]] = -100L
  first$.requirements[[1L]][[1L]] = "changed"
  first$.requirements[[1L]][[2L]]$rhs = FALSE

  fresh = param_set$params
  expect_identical(fresh, second)
  expect_identical(fresh$lower[[2L]], -2)
  expect_false("added" %in% names(fresh))
  expect_identical(fresh$.tags[[1L]], c("choice", "shared"))
  expect_identical(fresh$levels[[1L]], c("slow", "fast"))
  expect_identical(fresh$special_vals[[3L]], list(-99L))
  expect_identical(fresh$.requirements[[1L]][[1L]], "flag")
  expect_identical(fresh$.requirements[[1L]][[2L]]$rhs, TRUE)
  expect_identical(data.table:::selfrefok(fresh, FALSE), 1L)

  # Functions and environments are opaque semantic leaves. Their identity is
  # intentionally retained even though every enclosing mutable shell is new.
  expect_identical(fresh$.trafo[[2L]], fixture$transform)
  expect_identical(fresh$.init[[5L]], fixture$marker)
  fresh$.init[[5L]]$value = 42L
  expect_identical(fixture$marker$value, 42L)
  expect_identical(param_set$params$.init[[5L]]$value, 42L)
})

test_that("empty, subset, clone, and serialized params remain capsule-backed", {
  empty = ParamSet$new()$params
  expect_s3_class(empty, "data.table")
  expect_identical(names(empty), paradox:::domain_names)
  expect_identical(nrow(empty), 0L)
  expect_identical(vapply(empty, typeof, character(1L)), c(
    id = "character",
    cls = "character",
    grouping = "character",
    cargo = "list",
    lower = "double",
    upper = "double",
    tolerance = "double",
    levels = "list",
    special_vals = "list",
    default = "list",
    storage_type = "character",
    .tags = "list",
    .trafo = "list",
    .requirements = "list",
    .init_given = "logical",
    .init = "list"
  ))
  expect_identical(data.table:::selfrefok(empty, FALSE), 1L)

  source = ps(
    first = p_int(init = 1L),
    value = p_dbl(-1, 1, tags = "numeric", trafo = exp),
    enabled = p_lgl(init = TRUE),
    payload = p_uty()
  )
  source$add_dep("value", "enabled", CondEqual(TRUE))
  subset = source$subset(c("value", "enabled"))
  expect_identical(subset$params$id, c("value", "enabled"))
  expect_identical(subset$params$.requirements[[1L]][[1L]], "enabled")
  expect_identical(subset$params$.trafo[[1L]], exp)

  before = source$params
  shallow = source$clone(deep = FALSE)
  deep = source$clone(deep = TRUE)
  restored = unserialize(serialize(source, NULL, version = 3L))
  for (object in list(shallow, deep, restored)) {
    expect_identical(object$params, before)
    expect_identical(params_contract_native(object), object$params)
  }

  source$tags = list(
    first = "changed",
    value = character(),
    enabled = character(),
    payload = character()
  )
  source$values = list(first = 2L, enabled = FALSE)
  expect_identical(source$params$.tags[[1L]], "changed")
  expect_identical(source$params$.init[[1L]], 2L)
  for (object in list(shallow, deep, restored)) {
    expect_identical(object$params, before)
  }
})

test_that("params is read-only while returned facades remain freely mutable", {
  param_set = ps(x = p_int(0, 2, init = 1L), y = p_dbl(-1, 1))
  before = param_set$params

  modified = param_set$params[, marker := seq_len(.N)]
  expect_identical(modified$marker, c(1L, 2L))
  expect_identical(param_set$params, before)

  modified = data.table::set(
    param_set$params,
    i = 1L,
    j = "lower",
    value = -100
  )
  expect_identical(modified$lower[[1L]], -100)
  expect_identical(param_set$params, before)

  expect_error(param_set$params <- modified, "params is read-only", fixed = TRUE)
  expect_error(
    assign("params", modified, envir = param_set),
    "params is read-only",
    fixed = TRUE
  )

  Additive = R6::R6Class(
    "ParamsContractAdditive",
    inherit = ParamSet,
    public = list(label = "kept")
  )
  additive = Additive$new(list(x = p_int(init = 1L)))
  expect_identical(additive$label, "kept")
  expect_identical(params_contract_native(additive), additive$params)
})

test_that("params rejects malformed capsules deterministically without replay", {
  malformed = list(
    params_class = params_contract_forge("params", function(table) {
      table$cls[[1L]] = "ThirdPartyParam"
      table
    }),
    tags_owner = params_contract_forge("tags", function(table) {
      table$id[[1L]] = "ghost"
      table
    }),
    duplicate_trafo = params_contract_forge("trafos", function(table) {
      table[c(1L, 1L), , drop = FALSE]
    }),
    dependency_condition = params_contract_forge("deps", function(table) {
      table$cond[[1L]] = structure(
        list(rhs = TRUE),
        class = c("UnknownCondition", "Condition")
      )
      table
    }),
    duplicate_values = params_contract_forge("values", function(values) {
      structure(list(0.5, 1), names = c("x", "x"))
    })
  )

  unknown_condition = malformed$dependency_condition
  malformed$dependency_condition = NULL

  for (name in names(malformed)) {
    expect_error(
      malformed[[name]]$params,
      "Corrupt ParamSet parameter state capsule",
      fixed = TRUE,
      info = name
    )
    expect_error(
      params_contract_native(malformed[[name]]),
      "Corrupt ParamSet parameter state capsule",
      fixed = TRUE,
      info = paste(name, "direct")
    )
  }

  expect_error(
    unknown_condition$params,
    "Unsupported Condition class",
    fixed = TRUE
  )
  expect_error(
    params_contract_native(unknown_condition),
    "Unsupported Condition class",
    fixed = TRUE
  )

  missing_core = ps(x = p_int())
  missing_core$.__enclos_env__$private$.core = new("externalptr")
  expect_error(
    missing_core$params,
    "Corrupt ParamSet parameter state",
    fixed = TRUE
  )
})
