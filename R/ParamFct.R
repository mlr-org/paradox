#' @rdname Domain
#' @export
p_fct = function(levels, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  assert_function(aggr, null.ok = TRUE, nargs = 1L)
  constargs = as.list(match.call()[-1])
  levels = eval.parent(constargs$levels)
  if (!is.character(levels)) {
    # if the "levels" argument is not a character vector, then
    # we add a trafo.
    assert(check_atomic_vector(levels), check_list(levels))
    if (is.null(names(levels))) {
      names(levels) = as.character(levels)
    }
    trafo = crate(function(x) {
      x = levels[[x]]
      if (!is.null(trafo)) x = trafo(x)
      x
    }, trafo, levels)
    real_levels = names(levels)
  } else {
    real_levels = levels
  }
  # group p_fct by levels, so the group can be checked in a vectorized fashion.
  # We escape '"' and '\' to '\"' and '\\', respectively.
  cargo = list()
  cargo$disable_in_tune = disable_in_tune
  cargo$aggr = aggr
  cargo$in_tune_fn = in_tune_fn
  grouping = str_collapse(gsub("([\\\\\"])", "\\\\\\1", sort(real_levels)), quote = '"', sep = ",")
  Domain(cls = "ParamFct", grouping = grouping, levels = real_levels, special_vals = special_vals,
    default = default, tags = tags, trafo = trafo, storage_type = "character",
    depends_expr = substitute(depends), init = init, cargo = if (length(cargo)) cargo)
}

#' @export
domain_check.ParamFct = function(param, values, internal = FALSE) {
  if (qtestr(values, "S1")) {
    values_str = as.character(values)
    if (all(values_str %in% param$levels[[1]])) return(TRUE)  # this works because we have the grouping -- all 'levels' are the same here.
  }
  check_domain_vectorize(param$id, values, check_choice, more_args = list(choices = param$levels))
}

#' @export
domain_nlevels.ParamFct = function(param) map_dbl(param$levels, length)
#' @export
domain_is_bounded.ParamFct = function(param) rep(TRUE, nrow(param))
#' @export
domain_qunif.ParamFct = function(param, x) {
  nlevels = domain_nlevels(param)
  z = pmin(floor(x * nlevels) + 1, nlevels) # make sure we dont map to upper+1
  as.character(pmap(list(param$levels, z), `[[`))
}

#' @export
domain_is_number.ParamFct = function(param) FALSE
#' @export
domain_is_categ.ParamFct = function(param) TRUE
