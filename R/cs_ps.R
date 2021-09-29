get_type_ps = function(x) {
  clx = c("UniformIntegerHyperparameter",
          "NormalIntegerHyperparameter",
          "UniformFloatHyperparameter",
          "NormalFloatHyperparameter",
          "CategoricalHyperparameter",
          "OrdinalHyperparameter",
          "Constant")
  # we cannot represent ordered so we use the same as for categorical
  tlx = c("uniform_int", "normal_int", "uniform_float", "normal_float", "categorical", "categorical", "constant")
  tlx[match(x$`__class__`$`__name__`, clx)]
}



#' @title Map a ConfigSpace to a ParamSet
#'
#' @description
#' Maps a ConfigSpace (loaded via \CRANpkg{reticulate}) to a [ParamSet].
#' NormalIntegerHyperparameters are treated as UniformIntegerHyperparameter and the same holds for floats.
#' OrdinalHyperparameters are treated as CategoricalHyperparameters.
#' Constants are mapped to [ParamUty]s with tag `"constant"`.
#' Names are subject to changes via [base::make.names()].
#' q, weights and meta fields are ignored.
#'
#' @param cs (ConfigSpace).
#' @return [ParamSet]
#'
#' @export
#' @examples
#' # see ps_to_cs
cs_to_ps = function(cs) {
  # FIXME: we could do some additional safety checks here
  assert_true(cs$`__class__`$`__name__` == "ConfigurationSpace")
  if (length(cs$get_forbiddens())) {
    stop("Forbiddens are not implemented.")
  }

  # params
  # if q or weights is set for any parameter, we cannot respect this
  ps = ParamSet$new(mlr3misc::map(cs$get_hyperparameters(), .f = function(x) {
    switch(get_type_ps(x),
      "uniform_int" =
        if (x$log) {
          ParamDbl$new(make.names(x$name), lower = log(x$lower), upper = log(x$upper), default = log(x$default_value), tags = c("int", "log"))
        } else {
          ParamInt$new(make.names(x$name), lower = x$lower, upper = x$upper, default = x$default_value)
        },
      # we do not differentiate between uniform and normal ints
      "normal_int" = {
        warning("Normal ints are treated as uniform ints.")
        if (x$log) {
          ParamDbl$new(make.names(x$name), lower = -Inf, upper = Inf, default = log(x$default_value), tags = c("int", "log"))
        } else {
          ParamInt$new(make.names(x$name), lower = -Inf, upper = Inf, default = x$default_value)
        }
      },
      "uniform_float" =
        if (x$log) {
          ParamDbl$new(make.names(x$name), lower = log(x$lower), upper = log(x$upper), default = log(x$default_value), tags = "log")
        } else {
          ParamDbl$new(make.names(x$name), lower = x$lower, upper = x$upper, default = x$default_value)
        },
      # we do not differentiate between uniform and normal floats
      "normal_float" = {
        warning("Normal floats are treated as uniform floats.")
        if (x$log) {
          ParamDbl$new(make.names(x$name), lower = -Inf, upper = Inf, default = log(x$default_value), tags = "log")
        } else {
          ParamDbl$new(make.names(x$name), lower = -Inf, upper = Inf, default = x$default_value)
        }
      },
      "categorical" =
        if (every(x$choices, is.logical)) {
          ParamLgl$new(make.names(x$name), default = x$default_value)
        } else {
          ParamFct$new(make.names(x$name), levels = unlist(x$choices), default = x$default_value)
        },
      "constant" =
        ParamUty$new(make.names(x$name), default = x$default_value, tags = "constant")
    )
  }))

  # trafo
  ps$trafo = function(x, param_set) {
    for (i in names(which(mlr3misc::map_lgl(param_set$tags, .f = function(tags) "log" %in% tags)))) {
      x[[i]] = if ("int" %in% ps$params[[i]]$tags) as.integer(round(exp(x[[i]]))) else exp(x[[i]])
    }
    x
  }

  # deps
  cnds = cs$get_conditions()
  for (cnd in cnds) {
    if ("InCondition" %in% cnd$`__class__`$`__name__`) {
      ps$add_dep(make.names(cnd$child$name), on = make.names(cnd$parent$name), CondAnyOf$new(cnd$values))
    } else if ("EqualsCondition" %in% cnd$`__class__`$`__name__`) {
      ps$add_dep(make.names(cnd$child$name), on = make.names(cnd$parent$name), CondEqual$new(cnd$value))
    } else {
      stop("Not implemented.")
    }
  }

  ps
}

