#' @title Convert a paradox ParamSet to a ConfigSpace ConfigurationSpace
#'
#' @description
#' Translates a [ParamSet] into a Python `ConfigSpace.ConfigurationSpace` via \CRANpkg{reticulate}.
#' This function performs strict validation to ensure the [ParamSet] can be represented in ConfigSpace:
#'
#' Supported parameter mappings:
#' * `p_dbl()`: `Float` / `UniformFloatHyperparameter`
#' * `p_int()`: `Integer` / `UniformIntegerHyperparameter`
#' * `p_lgl()`: `Categorical` (TRUE/FALSE)
#' * `p_fct()`: `Categorical`
#'
#' Dependency conditions (`CondEqual`, `CondAnyOf`) are preserved.
#' Multiple conditions on the same child are combined using `ConfigSpace.AndConjunction`.
#'
#' Defaults are optional. If a parameter has no default, ConfigSpace will auto-assign one
#' (e.g. midpoint for numeric parameters, first level for categoricals).
#'
#' The function auto-detects old ConfigSpace API (ConfigSpace < 0.6.0) vs. new ConfigSpace API (ConfigSpace >= 0.6.0).
#'
#' @param param_set [ParamSet]\cr
#'   The parameter set to convert.
#'   Numeric parameters must define both `lower` and `upper` bounds.
#' @param name `character(1)`\cr
#'   Optional name for the resulting ConfigurationSpace.
#'
#' @return A Python `ConfigSpace.ConfigurationSpace` object representing the given parameter set.
#'
#' @examples
#' \dontrun{
#'   param_set = ps(
#'     lr        = p_dbl(lower = 1e-5, upper = 1,   default = 0.01, tags = "train"),
#'     ntree     = p_int(lower = 10,   upper = 500, default = 100,  tags = c("train","tuning")),
#'     bootstrap = p_lgl(default = TRUE, tags = "train"),
#'     criterion = p_fct(levels = c("gini", "entropy", "other"), default = "gini", tags = "train"),
#'     extras    = p_fct(tags = "predict", default = "alpha",
#'                       levels = c("alpha","beta","gamma","delta","kappa","nu")),
#'     depending = p_lgl(tags = "train", default = TRUE,
#'                       depends = quote(criterion == "entropy" && extras %in% c("alpha","beta")))
#'   )
#'   cs = paramset_to_configspace(param_set, name = "demo")
#' }
#' @export
paramset_to_configspace = function(param_set, name = NULL) {
  assert_python_packages("ConfigSpace")
  assert_param_set(param_set, no_untyped = TRUE)

  # assert that numeric params must have lower & upper
  upper = param_set$upper[param_set$is_number]
  if (anyInfinite(upper)) {
    stopf("Numeric parameters must have both lower and upper bounds. Missing upper bounds for: %s", str_collapse(names(upper)[is.infinite(upper)]))
  }

  lower = param_set$lower[param_set$is_number]
  if (anyInfinite(lower)) {
    stopf("Numeric parameters must have both lower and upper bounds. Missing lower bounds for: %s", str_collapse(names(lower)[is.infinite(lower)]))
  }

  ConfigSpace = reticulate::import("ConfigSpace", delay_load = TRUE)
  cs = ConfigSpace$ConfigurationSpace(name = name)

  # detect API version
  old_cs_version =
    !reticulate::py_has_attr(ConfigSpace, "Float") ||
    !reticulate::py_has_attr(ConfigSpace, "Integer") ||
    !reticulate::py_has_attr(ConfigSpace, "Categorical")

  # add parameters
  pwalk(param_set$params, function(id, cls, lower, upper, levels, default, .tags, ...) {
    meta = list(
      tags = .tags
    )

    if (cls == "ParamDbl") {
      add_hp(cs, build_float(ConfigSpace, id, lower, upper, default, meta, old_cs_version))
    } else if (cls == "ParamInt") {
      add_hp(cs, build_int(ConfigSpace, id, lower, upper, default, meta, old_cs_version))
    } else if (cls == "ParamLgl") {
      add_hp(cs, build_bool(ConfigSpace, id, default, meta, old_cs_version))
    } else if (cls == "ParamFct") {
      add_hp(cs, build_cat(ConfigSpace, id, levels, default, meta, old_cs_version))
    } else {
      stopf("Unsupported parameter class '%s' for parameter '%s'.", cls, id)
    }
  })

  # add dependencies
  if (nrow(param_set$deps)) {
    deps_grouped = split(param_set$deps, by = "id")

    walk(deps_grouped, function(deps) {
      conditions = pmap(deps, function(id, on, cond) {
        if (inherits(cond, "CondEqual")) {
          ConfigSpace$EqualsCondition(cs[id], cs[on], cond$rhs)
        } else {
          ConfigSpace$InCondition(cs[id], cs[on], cond$rhs)
        }
      })

      if (length(conditions) > 1) {
        condition = do.call(ConfigSpace$AndConjunction, unname(conditions))
        cs$add_condition(condition)
      } else {
        cs$add_condition(conditions[[1]])
      }
    })
  }

  cs
}

# add entities across API versions to ConfigSpace
add_hp = function(cs, hp) {
  if (reticulate::py_has_attr(cs, "add")) {
    cs$add(hp)
  } else if (reticulate::py_has_attr(cs, "add_hyperparameter")) {
    cs$add_hyperparameter(hp)
  } else if (reticulate::py_has_attr(cs, "add_hyperparameters")) {
    cs$add_hyperparameters(list(hp))
  } else {
    stopf("Could not detect method to add hyperparameters to ConfigSpace.")
  }
  invisible(cs)
}

# normalize NoDefault to NULL (reticulate maps NULL -> Python None)
normalize_default = function(default) {
  if (is_nodefault(default)) NULL else default
}

# builders for each datatype
build_float = function(ConfigSpace, id, lower, upper, default, meta, old_cs_version) {
  assert_string(id)
  lower = assert_number(lower)
  upper = assert_number(upper)
  assert_list(meta)
  assert_flag(old_cs_version)
  default = normalize_default(default)

  if (old_cs_version) {
    hp = ConfigSpace$hyperparameters$UniformFloatHyperparameter

    # default must not be passed at all if no default is given
    return(invoke(hp, .args = discard(list(name = id, lower = lower, upper = upper, default_value = default, meta = meta), is.null)))
  }
  return(invoke(ConfigSpace$Float, .args = discard(list(name = id, bounds = c(lower, upper), default = default, meta = meta), is.null)))
}

build_int = function(ConfigSpace, id, lower, upper, default, meta, old_cs_version) {
  assert_string(id)
  lower = assert_int(lower)
  upper = assert_int(upper)
  assert_list(meta)
  assert_flag(old_cs_version)
  default = normalize_default(default)

  if (old_cs_version) {
    hp = ConfigSpace$hyperparameters$UniformIntegerHyperparameter
    return(invoke(hp, .args = discard(list(name = id, lower = lower, upper = upper, default_value = default, meta = meta), is.null)))
  }
  return(invoke(ConfigSpace$Integer, .args = discard(list(name = id, bounds = c(lower, upper), default = default, meta = meta), is.null)))
}

build_cat = function(ConfigSpace, id, choices, default, meta, old_cs_version) {
  assert_string(id)
  assert_character(choices)
  assert_list(meta)
  assert_flag(old_cs_version)
  default = normalize_default(default)

  if (old_cs_version) {
    hp = ConfigSpace$hyperparameters$CategoricalHyperparameter
    return(invoke(hp, .args = discard(list(name = id, choices = choices, default_value = default, meta = meta), is.null)))
  }
  return(invoke(ConfigSpace$Categorical, .args = discard(list(name = id, items = choices, default = default, meta = meta), is.null)))
}

build_bool = function(ConfigSpace, id, default, meta, old_cs_version) {
  assert_string(id)
  assert_list(meta)
  assert_flag(old_cs_version)
  default = normalize_default(default)
  if (!is.null(default)) default = as.character(default)

  build_cat(ConfigSpace, id, c("TRUE", "FALSE"), default, meta, old_cs_version)
}

#' @title Convert a ConfigSpace ConfigurationSpace to a paradox ParamSet
#'
#' @description
#' Translates a Python `ConfigSpace.ConfigurationSpace` (via \CRANpkg{reticulate}) into a [ParamSet].
#' This is the inverse of [paramset_to_configspace()] and uses the same type mapping.
#'
#' Supported hyperparameter mappings:
#' * `UniformFloatHyperparameter` / `Float`: `p_dbl()` (with `logscale = TRUE` if `log = True`)
#' * `UniformIntegerHyperparameter` / `Integer`: `p_int()` (with `logscale = TRUE` if `log = True`)
#' * `CategoricalHyperparameter` / `Categorical`: `p_fct()`,
#' or `p_lgl()` if the choices are exactly `c("TRUE", "FALSE")`
#' * `OrdinalHyperparameter`: `p_fct()` (paradox has no ordinal type; ordering is preserved via the level order)
#'
#' Dependencies are translated as follows:
#' * `EqualsCondition` -> `depends = parent == value`
#' * `InCondition` -> `depends = parent %in% values`
#' * `AndConjunction` of the above -> multiple `depends` entries on the same child (paradox combines them with AND)
#' * `OrConjunction` (and other unsupported conjunctions) -> dropped with a warning, since paradox cannot express disjunctions
#'
#' Forbidden clauses are not representable in paradox and cause an error.
#'
#' Hyperparameter `meta` dictionaries written by [paramset_to_configspace()] are inspected for a `tags` entry,
#' which is restored as the parameter's `tags`.
#'
#' The function auto-detects old ConfigSpace API (ConfigSpace < 0.6.0) vs. new ConfigSpace API (ConfigSpace >= 0.6.0).
#'
#' @param config_space (`ConfigSpace.ConfigurationSpace`)\cr
#'   The Python ConfigurationSpace object to convert.
#'
#' @return A [ParamSet] representing the given configuration space.
#'
#' @examples
#' \dontrun{
#'   ConfigSpace = reticulate::import("ConfigSpace")
#'   cs = ConfigSpace$ConfigurationSpace(name = "demo")
#'   cs$add(ConfigSpace$Float("lr", bounds = c(1e-5, 1), default = 0.01, log = TRUE))
#'   cs$add(ConfigSpace$Integer("n", bounds = c(10L, 500L), default = 100L))
#'   cs$add(ConfigSpace$Categorical("crit", items = c("gini", "entropy"), default = "gini"))
#'   param_set = configspace_to_paramset(cs)
#' }
#' @export
configspace_to_paramset = function(config_space) {
  assert_python_packages("ConfigSpace")
  if (!inherits(config_space, "ConfigSpace.configuration_space.ConfigurationSpace")) {
    stopf("'config_space' must be a Python ConfigSpace.ConfigurationSpace, not '%s'.", class(config_space)[[1L]])
  }

  # collect hyperparameters
  hps = config_space$get_hyperparameters()
  hp_names = config_space$get_hyperparameter_names()
  hps = set_names(hps, hp_names)

  args = map(hps, domain_from_hyperparameter)

  # forbidden clauses are not expressible in paradox
  forbiddens = reticulate::py_to_r(config_space$get_forbiddens())
  if (length(forbiddens) > 0L) {
    error_config("ConfigurationSpace contains %i forbidden clause(s); paradox cannot represent forbidden clauses.",
      length(forbiddens))
  }

  param_set = invoke(ps, .args = args)

  # add dependencies
  conditions = config_space$get_conditions()
  walk(conditions, add_condition_to_paramset, param_set = param_set)

  param_set
}

py_attr = function(obj, name) {
  reticulate::py_to_r(reticulate::py_get_attr(obj, name))
}

# build a paradox Domain object from a single ConfigSpace hyperparameter
domain_from_hyperparameter = function(hp) {
  # match by trailing class name (e.g. "UniformFloatHyperparameter") so the same code
  # handles both the new ConfigSpace API (class strings include the submodule, e.g.
  # "ConfigSpace.hyperparameters.uniform_float.UniformFloatHyperparameter") and the
  # old API (class strings like "ConfigSpace.hyperparameters.UniformFloatHyperparameter").
  short_cls = sub(".*\\.", "", class(hp))
  meta = py_attr(hp, "meta")
  if (is.null(meta)) meta = list()
  tags = as.character(meta$tags)

  if (any(c("UniformFloatHyperparameter", "NormalFloatHyperparameter", "BetaFloatHyperparameter") %in% short_cls)) {
    log = isTRUE(py_attr(hp, "log"))
    default = py_attr(hp, "default_value")
    # paradox stores `default` in transformed space when logscale = TRUE,
    # while ConfigSpace stores it in linear space.
    if (log && !is.null(default)) default = log(default)
    p_dbl(
      lower = py_attr(hp, "lower"),
      upper = py_attr(hp, "upper"),
      default = default,
      logscale = log,
      tags = tags
    )
  } else if (any(c("UniformIntegerHyperparameter", "NormalIntegerHyperparameter", "BetaIntegerHyperparameter") %in% short_cls)) {
    log = isTRUE(py_attr(hp, "log"))
    default = as.integer(py_attr(hp, "default_value"))
    # see note above; for integers paradox also expects the default in log space.
    if (log && !is.null(default)) default = log(default)
    p_int(
      lower = as.integer(py_attr(hp, "lower")),
      upper = as.integer(py_attr(hp, "upper")),
      default = default,
      logscale = log,
      tags = tags
    )
  } else if ("CategoricalHyperparameter" %in% short_cls) {
    choices = as.character(py_attr(hp, "choices"))
    default = py_attr(hp, "default_value")
    if (setequal(choices, c("TRUE", "FALSE"))) {
      p_lgl(default = as.logical(default), tags = tags)
    } else {
      p_fct(levels = choices, default = as.character(default), tags = tags)
    }
  } else if ("OrdinalHyperparameter" %in% short_cls) {
    sequence = as.character(py_attr(hp, "sequence"))
    default = as.character(py_attr(hp, "default_value"))
    p_fct(levels = sequence, default = default, tags = tags)
  } else if (any(c("Constant", "UnParametrizedHyperparameter") %in% short_cls)) {
    value = py_attr(hp, "value")
    p_fct(levels = as.character(value), default = as.character(value), tags = tags)
  } else {
    error_config("Unsupported ConfigSpace hyperparameter class '%s' for parameter '%s'.",
      class(hp)[[1L]], py_attr(hp, "name"))
  }
}

# add a single (possibly conjunctive) ConfigSpace condition to a paradox ParamSet
add_condition_to_paramset = function(param_set, cond) {
  short_cls = sub(".*\\.", "", class(cond))

  if ("AndConjunction" %in% short_cls) {
    components = py_attr(cond, "components")
    walk(components, add_condition_to_paramset, param_set = param_set))
    return(invisible(param_set))
  }

  if ("OrConjunction" %in% short_cls) {
    error_config("ConfigurationSpace contains an OrConjunction condition; paradox cannot represent disjunctions.")
  }

  if ("EqualsCondition" %in% short_cls) {
    child = py_attr(cond, "child")$name
    parent = py_attr(cond, "parent")$name
    value = py_attr(cond, "value")
    value = coerce_dependency_value(param_set, parent, value)
    param_set$add_dep(child, on = parent, cond = CondEqual$new(value))
    return(invisible(param_set))
  }

  if ("InCondition" %in% short_cls) {
    child = py_attr(cond, "child")$name
    parent = py_attr(cond, "parent")$name
    values = py_attr(cond, "values")
    values = map(values, coerce_dependency_value, param_set = param_set, parent = parent))
    if (length(values) > 0L && !is.list(values[[1L]])) values = unlist(values)
    param_set$add_dep(child, on = parent, cond = CondAnyOf$new(values))
    return(invisible(param_set))
  }

  error_config("Unsupported condition class '%s'; only EqualsCondition, InCondition, and AndConjunction are supported.",
    class(cond)[[1L]])
}

# coerce a categorical-comparison value into the type of the paradox parent parameter
coerce_dependency_value = function(param_set, parent, value) {
  parent_class = param_set$class[[parent]]
  if (identical(parent_class, "ParamLgl")) {
    return(as.logical(value))
  }
  if (identical(parent_class, "ParamInt")) {
    return(as.integer(value))
  }
  if (identical(parent_class, "ParamDbl")) {
    return(as.numeric(value))
  }
  as.character(value)
}

