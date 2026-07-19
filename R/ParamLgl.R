#' @rdname Domain
#' @export
p_lgl = function(special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  cargo = list()
  cargo$aggr = aggr
  cargo$in_tune_fn = in_tune_fn
  cargo$disable_in_tune = disable_in_tune
  Domain(cls = "ParamLgl", grouping = "ParamLgl", levels = c(TRUE, FALSE), special_vals = special_vals, default = default,
    tags = tags, trafo = trafo, storage_type = "logical", depends_expr = substitute(depends), init = init, cargo = if (length(cargo)) cargo)
}
