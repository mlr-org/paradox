skip_if_not_installed("callr")
skip_if_not_installed("reticulate")


test_that("paramset_to_configspace works without defaults with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))

    param_set = ps(
      x1 = p_int(lower = 0, upper = 10, default = 1),
      x2 = p_dbl(lower = 0, upper = 10),
      x3 = p_fct(levels = c("a", "b", "c"))
    )
    cs = paramset_to_configspace(param_set)
    checkmate::expect_class(cs, "ConfigSpace.configuration_space.ConfigurationSpace")
    checkmate::expect_names(cs$get_hyperparameter_names(), permutation.of = c("x1", "x2", "x3"))

    TRUE
  }))
})

test_that("paramset_to_configspace numeric bounds check with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))

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

    TRUE
  }))
})

test_that("paramset_to_configspace utility parameters check with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))

    param_set = ps(
      x1 = p_uty(),
      x2 = p_dbl(lower = 0, upper = 10, default = 1)
    )
    expect_error(paramset_to_configspace(param_set), "ParamSet contains untyped params!")

    TRUE
  }))
})

test_that("paramset_to_configspace works with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)
    library(checkmate)
    library(mlr3misc)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))

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

    TRUE
  }))
})

test_that("paramset_to_configspace dependencies check with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)
    library(checkmate)
    library(mlr3misc)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))

    param_set = ps(
      x1 = p_int(lower = 0, upper = 10, default = 1, depends = x2 == "a"),
      x2 = p_fct(levels = c("a", "b", "c"), default = "a"),
      x3 = p_int(lower = 0, upper = 10, default = 1, depends = x2 %in% c("a", "b"))
    )

    cs = paramset_to_configspace(param_set)
    expect_class(cs, "ConfigSpace.configuration_space.ConfigurationSpace")
    expect_names(cs$get_hyperparameter_names(), permutation.of = c("x1", "x2", "x3"))

    TRUE
  }))
})

test_that("multiple dependencies for one child are combined with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)
    library(checkmate)
    library(mlr3misc)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))

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

    TRUE
  }))
})

test_that("multiple dependent children can coexist with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)
    library(checkmate)
    library(mlr3misc)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))

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

    TRUE
  }))
})
