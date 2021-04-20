get_type = function(x) {
  clx = c("ConfigSpace.hyperparameters.UniformIntegerHyperparameter",
          "ConfigSpace.hyperparameters.NormalIntegerHyperparameter",
          "ConfigSpace.hyperparameters.UniformFloatHyperparameter",
          "ConfigSpace.hyperparameters.NormalFloatHyperparameter",
          "ConfigSpace.hyperparameters.CategoricalHyperparameter",
          "ConfigSpace.hyperparameters.OrdinalHyperparameter",
          "ConfigSpace.hyperparameters.Constant")
  # FIXME: we cannot represent ordered so we use the same as for categorical
  tlx = c("uniform_int", "normal_int", "uniform_float", "normal_float", "categorical", "categorical", "constant")
  tlx[match(class(x)[1L], clx)]
}


### cs (ConfigurationSpace initialized via reticulate in python)
cs_to_ps = function(cs) {
  # params
  # FIXME: if q or weights is set for any parameter, we cannot respect this
  ps = ParamSet$new(map(cs$get_hyperparameters(), .f = function(x) {
    switch(get_type(x),
      "uniform_int" =
        if (x$log) {
          ParamDbl$new(make.names(x$name), lower = log(x$lower), upper = log(x$upper), default = log(x$default_value), tags = c("log", "int"))
        } else {
          ParamInt$new(make.names(x$name), lower = x$lower, upper = x$upper, default = x$default_value)
        },
      # FIXME: we do not differentiate between uniform and normal ints
      "normal_int" =
        if (x$log) {
          ParamDbl$new(make.names(x$name), lower = -Inf, upper = Inf, default = log(x$default_value), tags = c("log", "int"))
        } else {
          ParamInt$new(make.names(x$name), lower = -Inf, upper = Inf, default = x$default_value)
        },
      "uniform_float" =
        if (x$log) {
          ParamDbl$new(make.names(x$name), lower = log(x$lower), upper = log(x$upper), default = log(x$default_value), tags = "log")
        } else {
          ParamDbl$new(make.names(x$name), lower = x$lower, upper = x$upper, default = x$default_value)
        },
      # FIXME: we do not differentiate between uniform and normal floats
      "normal_float" =
        if (x$log) {
          ParamDbl$new(make.names(x$name), lower = -Inf, upper = Inf, default = log(x$default_value), tags = "log")
        } else {
          ParamDbl$new(make.names(x$name), lower = -Inf, upper = Inf, default = x$default_value)
        },
      "categorical" =
        if (length(x$choices) == 1L) {
          ParamUty$new(make.names(x$name), default = unlist(x$choices), tags = "constant")
        } else {
          if (every(x$choices, is.logical)) {
            ParamLgl$new(make.names(x$name), default = x$default_value)
          } else {
            ParamFct$new(make.names(x$name), levels = unlist(x$choices), default = x$default_value)
          }
        },
      "constant" =
        ParamUty$new(make.names(x$name), default = x$default_value, tags = "constant")
    )
  }))

  # trafo
  ps$trafo = function(x, param_set) {
    for(i in names(which(map_lgl(param_set$tags, .f = function(tags) "log" %in% tags)))) {
      x[[i]] = if ("int" %in% ps$params[[i]]$tags) as.integer(round(exp(x[[i]]))) else exp(x[[i]])
    }
    x
  }

  # deps
  cnds = cs$get_conditions()
  for(cnd in cnds) {
    if ("ConfigSpace.conditions.InCondition" %in% class(cnd)) {
      ps$add_dep(make.names(cnd$child$name), on = make.names(cnd$parent$name), CondAnyOf$new(cnd$values))
    } else if ("ConfigSpace.conditions.EqualsCondition" %in% class(cdn)) {
      ps$add_dep(make.names(cnd$child$name), on = make.names(cnd$parent$name), CondEqual$new(cnd$value))
    } else {
      stop("Not implemented.")
    }
  }

  if (length(cs$get_forbiddens())) {
    stop("Not implemented.")
  }

  ps

}

