.make_p_fct_trafo = function(levels, trafo) {
  force(levels)
  force(trafo)
  function(x) {
    x = levels[[x]]
    if (!is.null(trafo)) x = trafo(x)
    x
  }
}

#' @rdname Domain
#' @export
p_fct = function(levels, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  if (!is.null(aggr)) {
    if (!is.function(aggr)) stop("`aggr` must be a function", call. = FALSE)
  }
  constargs = as.list(match.call()[-1])
  levels = eval.parent(constargs$levels)
  if (!is.character(levels)) {
    # if the "levels" argument is not a character vector, then
    # we add a trafo.
    if (!is.atomic(levels) && !is.list(levels)) {
      stop("`levels` must be an atomic vector or list", call. = FALSE)
    }
    if (is.null(names(levels))) {
      names(levels) = as.character(levels)
    }
    # A package-owned two-binding closure is enough here. `crate()` compiled a
    # fresh function for every Domain construction, which dominated numeric
    # and list-valued p_fct() calls without adding semantic isolation.
    trafo = .make_p_fct_trafo(levels, trafo)
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
  sorted_levels = sort(real_levels)
  grouping = paste0(
    '"',
    gsub("([\\\\\"])", "\\\\\\1", sorted_levels),
    '"',
    collapse = ","
  )
  Domain(cls = "ParamFct", grouping = grouping, levels = real_levels, special_vals = special_vals,
    default = default, tags = tags, trafo = trafo, storage_type = "character",
    depends_expr = substitute(depends), init = init, cargo = if (length(cargo)) cargo)
}
