#' @rdname Domain
#' @export
p_dbl = function(lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(), tolerance = sqrt(.Machine$double.eps), depends = NULL, trafo = NULL, logscale = FALSE, init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  trafo = .paradox_strip_srcref(trafo)
  aggr = .paradox_strip_srcref(aggr)
  in_tune_fn = .paradox_strip_srcref(in_tune_fn)
  cargo = list()
  cargo$aggr = aggr
  cargo$in_tune_fn = in_tune_fn
  cargo$disable_in_tune = disable_in_tune
  Domain(cls = "ParamDbl", grouping = "ParamDbl", lower = lower, upper = upper, special_vals = special_vals, default = default, tags = tags, tolerance = tolerance, trafo = trafo, storage_type = "numeric",
    depends_expr = substitute(depends), init = init, cargo = if (length(cargo)) cargo,
    .numeric_source_kind = 1L, .numeric_logscale = logscale)
}
