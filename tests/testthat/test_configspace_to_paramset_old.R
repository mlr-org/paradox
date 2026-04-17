skip_if_not_installed("callr")
skip_if_not_installed("reticulate")

test_that("configspace_to_paramset works with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)
    library(checkmate)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))
    cs_mod = reticulate::import("ConfigSpace")

    cs = cs_mod$ConfigurationSpace()
    cs$add_hyperparameters(list(
      cs_mod$hyperparameters$UniformFloatHyperparameter("x1", lower = 0, upper = 1, default_value = 0.25),
      cs_mod$hyperparameters$UniformIntegerHyperparameter("x2", lower = 1L, upper = 10L, default_value = 3L),
      cs_mod$hyperparameters$CategoricalHyperparameter("x3", choices = c("a", "b", "c"), default_value = "b"),
      cs_mod$hyperparameters$CategoricalHyperparameter("x4", choices = c("TRUE", "FALSE"), default_value = "TRUE"),
      cs_mod$hyperparameters$OrdinalHyperparameter("x5", sequence = c("low", "med", "high"), default_value = "med")
    ))

    param_set = configspace_to_paramset(cs)
    expect_class(param_set, "ParamSet")
    expect_set_equal(param_set$ids(), c("x1", "x2", "x3", "x4", "x5"))
    expect_equal(param_set$class[["x1"]], "ParamDbl")
    expect_equal(param_set$class[["x2"]], "ParamInt")
    expect_equal(param_set$class[["x3"]], "ParamFct")
    expect_equal(param_set$class[["x4"]], "ParamLgl")
    expect_equal(param_set$class[["x5"]], "ParamInt")
    expect_equal(param_set$lower[["x5"]], 1)
    expect_equal(param_set$upper[["x5"]], 3)
    expect_equal(param_set$trafo(list(x5 = 2L))$x5, "med")

    TRUE
  }))
})

test_that("configspace_to_paramset translates conditions with old ConfigSpace API", {
  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")

    library(paradox)
    library(testthat)
    library(checkmate)

    reticulate::py_require(c("numpy<2", "ConfigSpace<0.6.0"))
    cs_mod = reticulate::import("ConfigSpace")

    cs = cs_mod$ConfigurationSpace()
    parent = cs_mod$hyperparameters$CategoricalHyperparameter("parent", choices = c("on", "off"), default_value = "on")
    child = cs_mod$hyperparameters$UniformFloatHyperparameter("child", lower = 0, upper = 1, default_value = 0.5)
    cs$add_hyperparameters(list(parent, child))
    cs$add_condition(cs_mod$EqualsCondition(child, parent, "on"))

    param_set = configspace_to_paramset(cs)
    expect_equal(nrow(param_set$deps), 1L)
    expect_class(param_set$deps$cond[[1L]], "CondEqual")

    TRUE
  }))
})
