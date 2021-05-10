get_type_cs = function(x) {
  # FIXME: we cannot represent ParamUty
  # FIXME: we may want to map ParamUty or ParamFct with a single possible value to ConfigSpace.hyperparameters.Constant
  clx = c("ParamInt", "ParamDbl", "ParamLgl", "ParamFct")
  tlx = c("uniform_int", "uniform_float", "categorical", "categorical")
  tlx[match(class(x)[1L], clx)]
}

wrap_default = function(default, trafo = identity) {
  checkmate::assert_function(trafo)
  if (checkmate::test_r6(default, classes = "NoDefault")) {
    NULL
  } else {
    trafo(default)
  }
}

#' @title Map a ParamSet to a ConfigSpace
#'
#' @description
#' Maps a ParamSet to a ConfigSpace.
#' [ParamUty]s cannot be represented.
#' Transformation functions except for log transformations are NOT automatically handled (and ConfigSpace in general cannot do this).
#' To automatically handle a log transformation, set a `"log"` tag for the [Param],
#' if the [Param] would be a [ParamInt] after transformation, additionally set an `"int"` tag, see examples below.
#' Only [Condition]s of class [CondEqual] and [CondAnyOf] are supported.
#'
#' Requires \CRANpkg{reticulate} and \CRANpkg{jsonlite} (if saving in json format is desired) to be installed.
#'
#' @param ps [ParamSet].
#' @param json_file (`character(1)`). \cr
#'   Optional filename ending with `".json"`.
#'   If specified, the returned ConfigSpace is additionally saved in the json format.
#'   Useful for using the ConfigSpace in a pure python session.
#'
#' @return ConfigSpace
#'
#' @export
#' @examples
#'\dontrun{
#'ps = ParamSet$new(list(
#'  ParamDbl$new("x1", lower = log(10), upper = log(20), default = log(15), tags = c("int", "log")),
#'  ParamInt$new("x2", lower = 10, upper = 20, default = 15),
#'  ParamDbl$new("x3", lower = log(10), upper = log(20), default = log(15), tags = "log"),
#'  ParamDbl$new("x4", lower = 10, upper = 20, default = 15),
#'  ParamLgl$new("x5", default = TRUE),
#'  ParamFct$new("x6", levels = c("a", "b", "c"), default = "c"))
#')
#'
#'ps$trafo = function(x, param_set) {
#'  for (i in names(which(mlr3misc::map_lgl(param_set$tags, .f = function(tags) "log" %in% tags)))) {
#'    x[[i]] = if ("int" %in% ps$params[[i]]$tags) as.integer(round(exp(x[[i]]))) else exp(x[[i]])
#'  }
#'  x
#'}
#'
#'ps$add_dep("x6", on = "x5", cond = CondEqual$new(TRUE))
#'ps$add_dep("x4", on = "x6", cond = CondAnyOf$new(c("a", "b")))
#'
#'cs = ps_to_cs(ps)
#'
#'dt_ps = data.table::rbindlist(generate_design_random(ps, n = 1000L)$transpose(filter_na = FALSE))
#'dt_cs = data.table::rbindlist(mlr3misc::map(cs$sample_configuration(1000L), function(x) {
#'  x$get_dictionary()
#'}), fill = TRUE)
#'summary(dt_ps)
#'summary(dt_cs)
#'all(is.na(dt_ps[x5 == FALSE][["x6"]]))  # first dependency
#'all(is.na(dt_cs[x5 == FALSE][["x6"]]))  # first dependency
#'all(is.na(dt_ps[x6 == "c"][["x4"]]))    # second dependency
#'all(is.na(dt_cs[x6 == "c"][["x4"]]))    # second dependency
#'
#'ps_ = cs_to_ps(cs)
#'psparams = ps$params
#'ps_params = ps_$params
#'all.equal(psparams, ps_params[names(psparams)])
#'all.equal(ps$deps, ps_$deps)
#'# ps$trafo, ps_$trafo
#'dt_ps_ = data.table::rbindlist(generate_design_random(ps, n = 1000L)$transpose(filter_na = FALSE))
#'summary(dt_ps_)
#'all(is.na(dt_ps_[x5 == FALSE][["x6"]]))  # first dependency
#'all(is.na(dt_ps_[x6 == "c"][["x4"]]))    # second dependency
#'}
ps_to_cs = function(ps, json_file = NULL) {
  # FIXME: could add an argument to ignore budget params (because most python optimizers do not use budget params in the cs
  # FIXME: we could do some additional safety checks here
  assert_param_set(ps)
  if (!is.null(json_file)) {
    assert_path_for_output(json_file)
    assert_true(endsWith(json_file, suffix = ".json"))
  }

  requireNamespace("reticulate")
  requireNamespace("jsonlite")

  CS = reticulate::import("ConfigSpace", as = "CS")
  CSH = reticulate::import("ConfigSpace.hyperparameters", as = "CSH")
  json = reticulate::import("ConfigSpace.read_and_write.json")

  cs = CS$ConfigurationSpace()

  # params
  for (i in seq_along(ps$params)) {
    param = ps$params[[i]]
    tmp = switch(get_type_cs(param),
      "uniform_int" =
        CSH$UniformIntegerHyperparameter(name = param$id, lower = param$lower, upper = param$upper, default_value = wrap_default(param$default)),
      "uniform_float" = if (all(c("int", "log") %in% param$tags)) {
        CSH$UniformIntegerHyperparameter(name = param$id, lower = as.integer(round(exp(param$lower))), upper = as.integer(round(exp(param$upper))), default_value = wrap_default(param$default, trafo = function(x) as.integer(round(exp(x)))), log = TRUE)
      } else if ("log" %in% param$tags) {
        CSH$UniformFloatHyperparameter(name = param$id, lower = exp(param$lower), upper = exp(param$upper), default_value = wrap_default(param$default, trafo = exp), log = TRUE)
      } else {
        CSH$UniformFloatHyperparameter(name = param$id, lower = param$lower, upper = param$upper, default_value = wrap_default(param$default))
      },
      "categorical" =
        CSH$CategoricalHyperparameter(name = param$id, choices = param$levels, default_value = wrap_default(param$default)),
    )
    cs$add_hyperparameter(tmp)
  }

  # trafo
  if (ps$has_trafo) {
    warning("Only log trafos can be respected automatically. Please check your trafos.")
  }

  # deps
  for (i in seq_len(NROW(ps$deps))) {
    child = cs$get_hyperparameter(ps$deps[i, id])
    parent = cs$get_hyperparameter(ps$deps[i, on])
    cond = ps$deps[i, cond][[1L]]
    cnd = if (checkmate::test_r6(cond, classes = "CondAnyOf")) {
      CS$InCondition(child = child, parent = parent, values = cond$rhs)
    } else if (checkmate::test_r6(cond, classes = "CondEqual")) {
      CS$EqualsCondition(child = child, parent = parent, value = cond$rhs)
    } else {
      stop("Not implemented.")
    }
    cs$add_condition(cnd)
  }

  if (!is.null(json_file)) write(json$write(cs), json_file)
  cs
}

