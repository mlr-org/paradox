skip_if_not_installed("reticulate")

test_that("paramset_to_configspace defaults check", {
  param_set = ps(
    x1 = p_int(lower = 0, upper = 10, default = 1),
    x2 = p_dbl(lower = 0, upper = 10),
    x3 = p_fct(levels = c("a", "b", "c"))
  )
  expect_error(paramset_to_configspace(param_set), "All parameters must have a default. Missing for: x2, x3")
})

test_that("paramset_to_configspace numeric bounds check", {
  param_set = ps(
    x1 = p_int(lower = 0, default = 1),
    x2 = p_dbl(upper = 3, default = 1)
  )
  expect_error(paramset_to_configspace(param_set), "Numeric parameters must have both lower and upper bounds. Missing upper bounds for: x1")

  param_set = ps(
    x1 = p_int(lower = 0, upper = 10, default = 1),
    x2 = p_dbl(upper = 10, default = 1)
  )
  expect_error(paramset_to_configspace(param_set), "Numeric parameters must have both lower and upper bounds. Missing lower bounds for: x2")
})

test_that("paramset_to_configspace utility parameters check", {
  param_set = ps(
    x1 = p_uty(),
    x2 = p_dbl(lower = 0, upper = 10, default = 1)
  )
  expect_error(paramset_to_configspace(param_set), "ParamSet contains untyped params!")
})

test_that("paramset_to_configspace works", {
  param_set = ps(
    x1 = p_int(lower = 0, upper = 10, default = 1),
    x2 = p_fct(levels = c("a", "b", "c"), default = "a"),
    x3 = p_lgl(default = TRUE),
    x4 = p_dbl(lower = 0, upper = 1, default = 0.5)
  )
  cs = paramset_to_configspace(param_set)
  expect_class(cs, "ConfigSpace.configuration_space.ConfigurationSpace")
  expect_names(cs$get_hyperparameter_names(), permutation.of = c("x1", "x2", "x3", "x4"))

  params = cs$get_hyperparameters()
  params = set_names(params, cs$get_hyperparameter_names())

  expect_equal(params$x1$lower, param_set$lower[["x1"]])
  expect_equal(params$x1$upper, param_set$upper[["x1"]])
  expect_equal(params$x1$default_value, param_set$default[["x1"]])
  expect_equal(unlist(params$x2$choices), param_set$levels[["x2"]])
  expect_equal(params$x2$default_value, param_set$default[["x2"]])
  expect_equal(params$x3$default_value, "TRUE")
  expect_equal(params$x4$lower, param_set$lower[["x4"]])
  expect_equal(params$x4$upper, param_set$upper[["x4"]])
  expect_equal(params$x4$default_value, param_set$default[["x4"]])
})

test_that("paramset_to_configspace dependencies check", {
  param_set = ps(
    x1 = p_int(lower = 0, upper = 10, default = 1, depends = x2 == "a"),
    x2 = p_fct(levels = c("a", "b", "c"), default = "a"),
    x3 = p_int(lower = 0, upper = 10, default = 1, depends = x2 %in% c("a", "b"))
  )

  cs = paramset_to_configspace(param_set)
  expect_class(cs, "ConfigSpace.configuration_space.ConfigurationSpace")
  expect_names(cs$get_hyperparameter_names(), permutation.of = c("x1", "x2", "x3"))
})

test_that("multiple dependencies for one child are combined", {
  param_set = ps(
    a = p_fct(levels = c("x", "y"), default = "x"),
    b = p_fct(levels = c("u", "v", "w"), default = "u"),
    c = p_int(lower = 1, upper = 5, default = 3, depends = quote(a == "y" && b %in% c("u", "v")))
  )

  cs = paramset_to_configspace(param_set, name = "deps-and")
  conds = cs$get_conditions()
  expect_true(length(conds) == 1)
  expect_class(conds[[1]], "ConfigSpace.conditions.AndConjunction")

  child_names  = map_chr(conds[[1]]$components, function(component) component$child$name)
  parent_names = map_chr(conds[[1]]$components, function(component) component$parent$name)
  expect_true(all(child_names == "c"))
  expect_setequal(parent_names, c("a","b"))
})

test_that("multiple dependent children can coexist", {
  param_set = ps(
    parent = p_fct(levels = c("on","off"), default = "on"),
    c1 = p_dbl(lower = 0, upper = 1, default = 0.1, depends = quote(parent == "on")),
    c2 = p_int(lower = 1, upper = 5, default = 2, depends = quote(parent == "on"))
  )
  cs = paramset_to_configspace(param_set, name = "deps-multi-children")
  conds = cs$get_conditions()
  expect_true(length(conds) == 2)
  children = map_chr(conds, function(z) z$child$name)
  parents  = map_chr(conds, function(z) z$parent$name)
  expect_setequal(children, c("c1", "c2"))
  expect_true(all(parents == c("parent", "parent")))
})

