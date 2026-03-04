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

