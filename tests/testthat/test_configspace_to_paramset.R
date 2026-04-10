skip_if_not_installed("reticulate")

reticulate::py_require("ConfigSpace")

test_that("configspace_to_paramset converts basic hyperparameter types", {
  cs_mod = reticulate::import("ConfigSpace")
  cs = cs_mod$ConfigurationSpace()
  cs$add(list(
    cs_mod$Float("x1", bounds = c(0, 1), default = 0.25),
    cs_mod$Integer("x2", bounds = c(1L, 10L), default = 3L),
    cs_mod$Categorical("x3", items = c("a", "b", "c"), default = "b"),
    cs_mod$Categorical("x4", items = c("TRUE", "FALSE"), default = "TRUE"),
    cs_mod$OrdinalHyperparameter("x5", sequence = c("low", "med", "high"), default_value = "med")
  ))

  param_set = configspace_to_paramset(cs)
  expect_class(param_set, "ParamSet")
  expect_set_equal(param_set$ids(), c("x1", "x2", "x3", "x4", "x5"))

  expect_equal(param_set$class[["x1"]], "ParamDbl")
  expect_equal(param_set$lower[["x1"]], 0)
  expect_equal(param_set$upper[["x1"]], 1)
  expect_equal(param_set$default[["x1"]], 0.25)

  expect_equal(param_set$class[["x2"]], "ParamInt")
  expect_equal(param_set$lower[["x2"]], 1)
  expect_equal(param_set$upper[["x2"]], 10)
  expect_equal(param_set$default[["x2"]], 3L)

  expect_equal(param_set$class[["x3"]], "ParamFct")
  expect_equal(param_set$levels[["x3"]], c("a", "b", "c"))
  expect_equal(param_set$default[["x3"]], "b")

  # categoricals with TRUE/FALSE choices map to ParamLgl
  expect_equal(param_set$class[["x4"]], "ParamLgl")
  expect_equal(param_set$default[["x4"]], TRUE)

  # ordinals map to ParamFct, level order preserved
  expect_equal(param_set$class[["x5"]], "ParamFct")
  expect_equal(param_set$levels[["x5"]], c("low", "med", "high"))
  expect_equal(param_set$default[["x5"]], "med")
})

test_that("configspace_to_paramset honors log-scaled numeric hyperparameters", {
  cs_mod = reticulate::import("ConfigSpace")
  cs = cs_mod$ConfigurationSpace()
  cs$add(list(
    cs_mod$Float("lr", bounds = c(1e-5, 1.0), default = 1e-2, log = TRUE),
    cs_mod$Integer("n", bounds = c(1L, 1000L), default = 10L, log = TRUE)
  ))

  param_set = configspace_to_paramset(cs)
  expect_true(param_set$is_logscale[["lr"]])
  expect_true(param_set$is_logscale[["n"]])
  # paradox stores defaults in transformed (log) space when logscale = TRUE
  expect_equal(param_set$default[["lr"]], log(1e-2))
  expect_equal(param_set$default[["n"]], log(10))
})

test_that("configspace_to_paramset translates EqualsCondition", {
  cs_mod = reticulate::import("ConfigSpace")
  cs = cs_mod$ConfigurationSpace()
  parent = cs_mod$Categorical("parent", items = c("on", "off"), default = "on")
  child = cs_mod$Float("child", bounds = c(0, 1), default = 0.5)
  cs$add(list(parent, child))
  cs$add(cs_mod$EqualsCondition(child, parent, "on"))

  param_set = configspace_to_paramset(cs)
  expect_equal(nrow(param_set$deps), 1L)
  expect_equal(param_set$deps$id, "child")
  expect_equal(param_set$deps$on, "parent")
  expect_class(param_set$deps$cond[[1L]], "CondEqual")
  expect_equal(param_set$deps$cond[[1L]]$rhs, "on")
})

test_that("configspace_to_paramset translates InCondition", {
  cs_mod = reticulate::import("ConfigSpace")
  cs = cs_mod$ConfigurationSpace()
  parent = cs_mod$Categorical("parent", items = c("a", "b", "c"), default = "a")
  child = cs_mod$Float("child", bounds = c(0, 1), default = 0.5)
  cs$add(list(parent, child))
  cs$add(cs_mod$InCondition(child, parent, list("a", "b")))

  param_set = configspace_to_paramset(cs)
  expect_equal(nrow(param_set$deps), 1L)
  expect_class(param_set$deps$cond[[1L]], "CondAnyOf")
  expect_setequal(param_set$deps$cond[[1L]]$rhs, c("a", "b"))
})

test_that("configspace_to_paramset expands AndConjunction into multiple deps", {
  cs_mod = reticulate::import("ConfigSpace")
  cs = cs_mod$ConfigurationSpace()
  p1 = cs_mod$Categorical("p1", items = c("u", "v"), default = "u")
  p2 = cs_mod$Categorical("p2", items = c("x", "y"), default = "x")
  child = cs_mod$Float("child", bounds = c(0, 1), default = 0.5)
  cs$add(list(p1, p2, child))
  cs$add(cs_mod$AndConjunction(
    cs_mod$EqualsCondition(child, p1, "v"),
    cs_mod$EqualsCondition(child, p2, "y")
  ))

  param_set = configspace_to_paramset(cs)
  expect_equal(nrow(param_set$deps), 2L)
  expect_setequal(param_set$deps$on, c("p1", "p2"))
  expect_true(all(param_set$deps$id == "child"))
})

test_that("configspace_to_paramset drops OrConjunction with a warning", {
  cs_mod = reticulate::import("ConfigSpace")
  cs = cs_mod$ConfigurationSpace()
  p1 = cs_mod$Categorical("p1", items = c("u", "v"), default = "u")
  p2 = cs_mod$Categorical("p2", items = c("x", "y"), default = "x")
  child = cs_mod$Float("child", bounds = c(0, 1), default = 0.5)
  cs$add(list(p1, p2, child))
  cs$add(cs_mod$OrConjunction(
    cs_mod$EqualsCondition(child, p1, "v"),
    cs_mod$EqualsCondition(child, p2, "y")
  ))

  expect_warning(
    param_set <- configspace_to_paramset(cs),
    "OrConjunction"
  )
  expect_equal(nrow(param_set$deps), 0L)
})

test_that("configspace_to_paramset drops forbidden clauses with a warning", {
  cs_mod = reticulate::import("ConfigSpace")
  cs = cs_mod$ConfigurationSpace()
  parent = cs_mod$Categorical("parent", items = c("a", "b", "c"), default = "b")
  child = cs_mod$Float("child", bounds = c(0, 1), default = 0.5)
  cs$add(list(parent, child))
  cs$add(cs_mod$ForbiddenEqualsClause(parent, "a"))

  expect_warning(
    param_set <- configspace_to_paramset(cs),
    "forbidden clause"
  )
  expect_set_equal(param_set$ids(), c("parent", "child"))
})

test_that("configspace_to_paramset coerces deps for boolean parents", {
  cs_mod = reticulate::import("ConfigSpace")
  cs = cs_mod$ConfigurationSpace()
  flag = cs_mod$Categorical("flag", items = c("TRUE", "FALSE"), default = "TRUE")
  child = cs_mod$Float("child", bounds = c(0, 1), default = 0.5)
  cs$add(list(flag, child))
  cs$add(cs_mod$EqualsCondition(child, flag, "TRUE"))

  param_set = configspace_to_paramset(cs)
  expect_equal(param_set$class[["flag"]], "ParamLgl")
  expect_identical(param_set$deps$cond[[1L]]$rhs, TRUE)
})

test_that("configspace_to_paramset rejects non-ConfigurationSpace input", {
  expect_error(configspace_to_paramset("not a config space"),
    "Python ConfigSpace.ConfigurationSpace")
})

test_that("paramset_to_configspace then configspace_to_paramset round-trips", {
  param_set = ps(
    x1 = p_int(lower = 0, upper = 10, default = 1),
    x2 = p_fct(levels = c("a", "b", "c"), default = "a"),
    x3 = p_lgl(default = TRUE),
    x4 = p_dbl(lower = 0, upper = 1, default = 0.5)
  )
  cs = paramset_to_configspace(param_set)
  out = configspace_to_paramset(cs)

  expect_set_equal(out$ids(), param_set$ids())
  expect_equal(out$class[param_set$ids()], param_set$class[param_set$ids()])
  expect_equal(out$lower[["x1"]], param_set$lower[["x1"]])
  expect_equal(out$upper[["x1"]], param_set$upper[["x1"]])
  expect_equal(out$levels[["x2"]], param_set$levels[["x2"]])
  expect_equal(out$default[["x3"]], param_set$default[["x3"]])
  expect_equal(out$lower[["x4"]], param_set$lower[["x4"]])
})

test_that("paramset_to_configspace then configspace_to_paramset round-trips with deps", {
  param_set = ps(
    parent = p_fct(levels = c("on", "off"), default = "on"),
    c1 = p_dbl(lower = 0, upper = 1, default = 0.1, depends = quote(parent == "on")),
    c2 = p_int(lower = 1L, upper = 5L, default = 2L, depends = quote(parent %in% c("on", "off")))
  )
  cs = paramset_to_configspace(param_set)
  out = configspace_to_paramset(cs)

  expect_equal(nrow(out$deps), 2L)
  c1_dep = out$deps[id == "c1"]
  c2_dep = out$deps[id == "c2"]
  expect_class(c1_dep$cond[[1L]], "CondEqual")
  expect_class(c2_dep$cond[[1L]], "CondAnyOf")
})
