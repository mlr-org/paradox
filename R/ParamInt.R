
.make_p_int_logscale_trafo = function(lower, upper) {
  force(lower)
  force(upper)
  .paradox_strip_srcref(
    function(x) as.integer(max(min(exp(x), upper), lower))
  )
}

#' @rdname Domain
#' @export
p_int = function(lower = -Inf, upper = Inf, special_vals = list(), default = NO_DEF, tags = character(), tolerance = sqrt(.Machine$double.eps), depends = NULL, trafo = NULL, logscale = FALSE, init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  trafo = .paradox_strip_srcref(trafo)
  aggr = .paradox_strip_srcref(aggr)
  in_tune_fn = .paradox_strip_srcref(in_tune_fn)
  cargo = list()
  cargo$aggr = aggr
  cargo$in_tune_fn = in_tune_fn
  cargo$disable_in_tune = disable_in_tune

  Domain(cls = "ParamInt", grouping = "ParamInt", lower = lower, upper = upper, special_vals = special_vals, default = default, tags = tags, tolerance = tolerance, trafo = trafo,
    storage_type = "integer", depends_expr = substitute(depends), init = init,
    cargo = if (length(cargo)) cargo, .numeric_source_kind = 2L,
    .numeric_logscale = logscale)
}
