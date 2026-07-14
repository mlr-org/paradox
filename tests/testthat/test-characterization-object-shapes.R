context("characterization: object shapes")

test_that("Domain objects retain their table structure and scalar column types", {
  domain = p_int(
    lower = 1,
    upper = 9,
    special_vals = list(99L),
    default = 3L,
    tags = c("alpha", "beta"),
    tolerance = 0.25,
    init = 4L
  )

  expected_names = c(
    "id", "cls", "grouping", "cargo", "lower", "upper", "tolerance",
    "levels", "special_vals", "default", "storage_type", ".tags",
    ".trafo", ".requirements", ".init_given", ".init"
  )
  expected_types = c(
    id = "character", cls = "character", grouping = "character", cargo = "list",
    lower = "double", upper = "double", tolerance = "double", levels = "list",
    special_vals = "list", default = "list", storage_type = "character",
    .tags = "list", .trafo = "list", .requirements = "list",
    .init_given = "logical", .init = "list"
  )

  expect_identical(class(domain), c("ParamInt", "Domain", "data.table", "data.frame"))
  expect_identical(dim(domain), c(1L, 16L))
  expect_identical(names(domain), expected_names)
  expect_identical(vapply(domain, typeof, character(1L)), expected_types)
  expect_true(is.call(attr(domain, "repr")))

  expect_identical(domain$cls, "ParamInt")
  expect_identical(domain$grouping, "ParamInt")
  expect_identical(domain$lower, 1)
  expect_identical(domain$upper, 9)
  expect_identical(domain$tolerance, 0.25)
  expect_null(domain$levels[[1L]])
  expect_identical(domain$special_vals[[1L]], list(99L))
  expect_identical(domain$default[[1L]], 3L)
  expect_identical(domain$storage_type, "integer")
  expect_identical(domain$.tags[[1L]], c("alpha", "beta"))
  expect_null(domain$.trafo[[1L]])
  expect_null(domain$.requirements[[1L]])
  expect_identical(domain$.init_given, TRUE)
  expect_identical(domain$.init[[1L]], 4L)

  domains = list(
    p_int(),
    p_dbl(),
    p_lgl(),
    p_fct(c("a", "b")),
    p_uty()
  )
  expected_domain_classes = c("ParamInt", "ParamDbl", "ParamLgl", "ParamFct", "ParamUty")
  expected_storage_types = c("integer", "numeric", "logical", "character", "list")

  for (i in seq_along(domains)) {
    expect_identical(class(domains[[i]]), c(expected_domain_classes[[i]], "Domain", "data.table", "data.frame"))
    expect_identical(names(domains[[i]]), expected_names)
    expect_identical(vapply(domains[[i]], typeof, character(1L)), expected_types)
    expect_identical(domains[[i]]$storage_type, expected_storage_types[[i]])
  }
})

test_that("Condition objects and Constructor$new retain their list contracts", {
  condition = Condition(2L, "%s relates to %s")
  equal = CondEqual(2L)
  any_of = CondAnyOf(c("a", "b"))

  expect_identical(class(condition), "Condition")
  expect_identical(names(condition), c("rhs", "condition_format_string"))
  expect_identical(vapply(condition, typeof, character(1L)), c(rhs = "integer", condition_format_string = "character"))
  expect_identical(unclass(condition), list(rhs = 2L, condition_format_string = "%s relates to %s"))

  expect_identical(class(equal), c("CondEqual", "Condition"))
  expect_identical(unclass(equal), list(rhs = 2L, condition_format_string = "%s == %s"))
  expect_identical(class(any_of), c("CondAnyOf", "Condition"))
  expect_identical(unclass(any_of), list(rhs = c("a", "b"), condition_format_string = "%s %%in%% {%s}"))

  expect_identical(class(CondEqual), c("Constructor", "function"))
  expect_identical(class(CondAnyOf), c("Constructor", "function"))
  expect_identical(CondEqual$new, CondEqual)
  expect_identical(CondAnyOf$new, CondAnyOf)
  expect_identical(CondEqual$new(2L), equal)
  expect_identical(CondAnyOf$new(c("a", "b")), any_of)
  expect_error(CondEqual$rhs, "only 'new' element can be accessed", fixed = TRUE)
})

test_that("ParamSet exposes the established R6 bindings and typed table views", {
  param_set = ps(
    count = p_int(0, 10, default = 2L, tags = c("alpha", "common"), init = 3L),
    scale = p_dbl(-1, 1, tags = c("beta", "common"), trafo = function(x) x * 2),
    mode = p_fct(c("fast", "safe"), depends = count == 3L),
    flag = p_lgl(),
    payload = p_uty()
  )

  methods = c(
    "initialize", "ids", "get_values", "set_values", "trafo",
    "aggr_internal_tuned_values", "disable_internal_tuning",
    "convert_internal_search_space", "test_constraint", "test_constraint_dt",
    "check", "check_dependencies", "test", "assert", "check_dt", "test_dt",
    "assert_dt", "qunif", "get_domain", "subset", "subspaces", "flatten",
    "search_space", "add_dep", "format", "print", "clone"
  )
  fields = c(
    "assert_values", "data", "values", "tags", "params", "domains",
    "extra_trafo", "constraint", "deps", "length", "is_empty", "has_trafo",
    "has_extra_trafo", "has_deps", "has_constraint", "all_numeric",
    "all_categorical", "all_bounded", "class", "lower", "upper", "levels",
    "storage_type", "special_vals", "default", "has_trafo_param", "is_logscale",
    "nlevels", "is_number", "is_categ", "is_bounded"
  )

  expect_identical(class(param_set), c("ParamSet", "R6"))
  expect_identical(typeof(param_set), "environment")
  expect_identical(sort(names(param_set)), sort(c(".__enclos_env__", methods, fields)))
  expect_true(is.environment(param_set$.__enclos_env__))
  expect_true(all(vapply(methods, function(name) is.function(param_set[[name]]), logical(1L))))

  ids = c("count", "scale", "mode", "flag", "payload")
  expect_identical(param_set$ids(), ids)
  expect_identical(param_set$class, setNames(c("ParamInt", "ParamDbl", "ParamFct", "ParamLgl", "ParamUty"), ids))
  expect_identical(param_set$lower, setNames(c(0, -1, NA_real_, NA_real_, NA_real_), ids))
  expect_identical(param_set$upper, setNames(c(10, 1, NA_real_, NA_real_, NA_real_), ids))
  expect_identical(param_set$storage_type, setNames(c("integer", "numeric", "character", "logical", "list"), ids))
  expect_identical(param_set$levels, setNames(list(NULL, NULL, c("fast", "safe"), c(TRUE, FALSE), NULL), ids))
  expect_identical(param_set$nlevels, setNames(c(11, Inf, 2, 2, Inf), ids))
  expect_identical(param_set$is_bounded, setNames(c(TRUE, TRUE, TRUE, TRUE, FALSE), ids))
  expect_identical(param_set$values, list(count = 3L))
  expect_identical(param_set$default, list(count = 2L))
  expect_identical(param_set$tags, setNames(list(c("alpha", "common"), c("beta", "common"), character(), character(), character()), ids))

  params_types = c(
    id = "character", cls = "character", grouping = "character", cargo = "list",
    lower = "double", upper = "double", tolerance = "double", levels = "list",
    special_vals = "list", default = "list", storage_type = "character",
    .tags = "list", .trafo = "list", .requirements = "list",
    .init_given = "logical", .init = "list"
  )
  data_types = c(
    id = "character", class = "character", lower = "double", upper = "double",
    levels = "list", nlevels = "double", is_bounded = "logical",
    special_vals = "list", default = "list", storage_type = "character", tags = "list"
  )
  deps_types = c(id = "character", on = "character", cond = "list")

  expect_identical(class(param_set$params), c("data.table", "data.frame"))
  expect_identical(vapply(param_set$params, typeof, character(1L)), params_types)
  expect_identical(param_set$params$id, ids)
  expect_identical(param_set$params$.init_given, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(param_set$params$.init, list(3L, NULL, NULL, NULL, NULL))
  expect_true(is.function(param_set$params$.trafo[[2L]]))
  expect_identical(param_set$params$.requirements[[3L]][[1L]], "count")
  expect_identical(class(param_set$params$.requirements[[3L]][[2L]]), c("CondEqual", "Condition"))

  expect_identical(class(param_set$data), c("data.table", "data.frame"))
  expect_identical(vapply(param_set$data, typeof, character(1L)), data_types)
  expect_identical(param_set$data$id, ids)
  expect_identical(class(param_set$deps), c("data.table", "data.frame"))
  expect_identical(vapply(param_set$deps, typeof, character(1L)), deps_types)
  expect_identical(param_set$deps$id, "mode")
  expect_identical(param_set$deps$on, "count")
  expect_identical(class(param_set$deps$cond[[1L]]), c("CondEqual", "Condition"))

  expect_identical(typeof(param_set$domains), "list")
  expect_identical(names(param_set$domains), ids)
  expect_true(all(vapply(param_set$domains, inherits, logical(1L), "Domain")))
  expect_identical(typeof(param_set$length), "integer")
  expect_identical(typeof(param_set$is_empty), "logical")
  expect_identical(typeof(param_set$assert_values), "logical")
})

test_that("ParamSet private stores remain readable with stable table shapes", {
  param_set = ps(
    count = p_int(0, 10, tags = c("alpha", "common"), init = 3L),
    scale = p_dbl(-1, 1, tags = c("beta", "common"), trafo = function(x) x * 2),
    mode = p_fct(c("fast", "safe"), depends = count == 3L)
  )
  private = param_set$.__enclos_env__$private

  expect_true(is.environment(private))
  expect_true(all(c(".params", ".values", ".tags", ".deps", ".trafos") %in% names(private)))
  expect_identical(
    vapply(private$.params, typeof, character(1L)),
    c(
      id = "character", cls = "character", grouping = "character", cargo = "list",
      lower = "double", upper = "double", tolerance = "double", levels = "list",
      special_vals = "list", default = "list", storage_type = "character"
    )
  )
  expect_identical(private$.params$id, c("count", "scale", "mode"))

  expect_identical(typeof(private$.values), "list")
  expect_identical(private$.values, list(count = 3L))
  expect_identical(vapply(private$.tags, typeof, character(1L)), c(id = "character", tag = "character"))
  expect_identical(private$.tags$id, c("count", "count", "scale", "scale"))
  expect_identical(private$.tags$tag, c("alpha", "common", "beta", "common"))
  expect_identical(vapply(private$.deps, typeof, character(1L)), c(id = "character", on = "character", cond = "list"))
  expect_identical(private$.deps$id, "mode")
  expect_identical(private$.deps$on, "count")
  expect_identical(vapply(private$.trafos, typeof, character(1L)), c(id = "character", trafo = "list"))
  expect_identical(private$.trafos$id, "scale")
  expect_true(is.function(private$.trafos$trafo[[1L]]))
})

test_that("empty ParamSet stores and views preserve zero-row prototypes", {
  param_set = ParamSet$new()
  private = param_set$.__enclos_env__$private

  expect_identical(param_set$ids(), character())
  expect_identical(param_set$values, setNames(list(), character()))
  expect_identical(param_set$domains, setNames(list(), character()))
  expect_identical(param_set$tags, setNames(list(), character()))
  expect_identical(param_set$nlevels, setNames(integer(), character()))

  expect_identical(
    vapply(param_set$data, typeof, character(1L)),
    c(
      id = "character", class = "character", lower = "double", upper = "double",
      levels = "list", nlevels = "integer", is_bounded = "logical",
      special_vals = "list", default = "list", storage_type = "character", tags = "list"
    )
  )
  expect_identical(
    vapply(private$.params, typeof, character(1L)),
    c(
      id = "character", cls = "character", grouping = "character", cargo = "list",
      lower = "double", upper = "double", tolerance = "double", levels = "list",
      special_vals = "list", default = "list", storage_type = "character"
    )
  )
  expect_identical(vapply(private$.tags, typeof, character(1L)), c(id = "character", tag = "character"))
  expect_identical(vapply(private$.deps, typeof, character(1L)), c(id = "character", on = "character", cond = "list"))
  expect_identical(vapply(private$.trafos, typeof, character(1L)), c(id = "character", trafo = "list"))
})

test_that("ParamSetCollection adds the established collection bindings and stores", {
  left = ps(enabled = p_lgl(tags = "control"), amount = p_int(0, 5))
  left$add_dep("amount", "enabled", CondEqual(TRUE))
  left$values = list(enabled = TRUE, amount = 2L)
  right = ps(scale = p_dbl(0, 1, tags = "numeric", trafo = sqrt))
  collection = ParamSetCollection$new(list(left = left, right = right), tag_sets = TRUE, tag_params = TRUE)
  collection$add_dep("right.scale", "left.enabled", CondEqual(TRUE))

  param_set_methods = c(
    "initialize", "ids", "get_values", "set_values", "trafo",
    "aggr_internal_tuned_values", "disable_internal_tuning",
    "convert_internal_search_space", "test_constraint", "test_constraint_dt",
    "check", "check_dependencies", "test", "assert", "check_dt", "test_dt",
    "assert_dt", "qunif", "get_domain", "subset", "subspaces", "flatten",
    "search_space", "add_dep", "format", "print", "clone"
  )
  param_set_fields = c(
    "assert_values", "data", "values", "tags", "params", "domains",
    "extra_trafo", "constraint", "deps", "length", "is_empty", "has_trafo",
    "has_extra_trafo", "has_deps", "has_constraint", "all_numeric",
    "all_categorical", "all_bounded", "class", "lower", "upper", "levels",
    "storage_type", "special_vals", "default", "has_trafo_param", "is_logscale",
    "nlevels", "is_number", "is_categ", "is_bounded"
  )

  expect_identical(class(collection), c("ParamSetCollection", "ParamSet", "R6"))
  expect_identical(typeof(collection), "environment")
  expect_identical(
    sort(names(collection)),
    sort(c(".__enclos_env__", param_set_methods, "add", param_set_fields, "sets"))
  )
  expect_identical(collection$ids(), c("left.enabled", "left.amount", "right.scale"))
  expect_identical(typeof(collection$sets), "list")
  expect_identical(names(collection$sets), c("left", "right"))
  expect_true(all(vapply(collection$sets, inherits, logical(1L), "ParamSet")))
  expect_identical(collection$values, list(left.enabled = TRUE, left.amount = 2L))
  expect_identical(collection$tags, list(
    left.enabled = c("control", "set_left", "param_enabled"),
    left.amount = c("set_left", "param_amount"),
    right.scale = c("numeric", "set_right", "param_scale")
  ))
  expect_identical(vapply(collection$params, typeof, character(1L)), c(
    id = "character", cls = "character", grouping = "character", cargo = "list",
    lower = "double", upper = "double", tolerance = "double", levels = "list",
    special_vals = "list", default = "list", storage_type = "character",
    .tags = "list", .trafo = "list", .requirements = "list",
    .init_given = "logical", .init = "list"
  ))
  expect_identical(vapply(collection$data, typeof, character(1L)), c(
    id = "character", class = "character", lower = "double", upper = "double",
    levels = "list", nlevels = "double", is_bounded = "logical",
    special_vals = "list", default = "list", storage_type = "character", tags = "list"
  ))
  expect_identical(vapply(collection$deps, typeof, character(1L)), c(id = "character", on = "character", cond = "list"))
  expect_identical(collection$deps$id, c("left.amount", "right.scale"))
  expect_identical(collection$deps$on, c("left.enabled", "left.enabled"))
  expect_identical(typeof(collection$length), "integer")
  expect_identical(typeof(collection$is_empty), "logical")
  expect_identical(typeof(collection$has_trafo), "logical")
  expect_identical(typeof(collection$has_deps), "logical")
  expect_identical(typeof(collection$assert_values), "logical")

  private = collection$.__enclos_env__$private
  expect_true(all(c(".params", ".values", ".tags", ".deps", ".trafos", ".sets") %in% names(private)))
  expect_identical(private$.params$id, collection$ids())
  expect_identical(vapply(private$.params, typeof, character(1L)), c(
    id = "character", cls = "character", grouping = "character", cargo = "list",
    lower = "double", upper = "double", tolerance = "double", levels = "list",
    special_vals = "list", default = "list", storage_type = "character"
  ))
  expect_identical(private$.values, setNames(list(), character()))
  expect_identical(vapply(private$.tags, typeof, character(1L)), c(id = "character", tag = "character"))
  expect_identical(vapply(private$.deps, typeof, character(1L)), c(id = "character", on = "character", cond = "list"))
  expect_identical(private$.deps$id, "right.scale")
  expect_identical(private$.deps$on, "left.enabled")
  expect_identical(vapply(private$.trafos, typeof, character(1L)), c(id = "character", trafo = "list"))
  expect_identical(private$.trafos$id, "right.scale")
  expect_identical(typeof(private$.sets), "list")
  expect_identical(names(private$.sets), c("left", "right"))
  expect_identical(private$.sets[[1L]]$values, list(enabled = TRUE, amount = 2L))
  expect_identical(private$.sets[[2L]]$values, setNames(list(), character()))

  expect_identical(
    vapply(private$.translation, typeof, character(1L)),
    c(id = "character", original_id = "character", owner_ps_index = "integer", owner_name = "character")
  )
  expect_identical(private$.translation$id, c("left.amount", "left.enabled", "right.scale"))
  expect_identical(private$.translation$original_id, c("amount", "enabled", "scale"))
  expect_identical(private$.translation$owner_ps_index, c(1L, 1L, 2L))
  expect_identical(private$.translation$owner_name, c("left", "left", "right"))
})
