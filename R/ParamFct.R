.make_p_fct_trafo = function(levels, trafo) {
  # Assignment after forcing replaces the formal promise cells with their
  # realized values. Besides retaining the exact force order, this prevents a
  # package-owned callback frame from manufacturing R 4.5's intentional
  # fail-closed migration boundary for arbitrary reached promises.
  levels = force(levels)
  trafo = force(trafo)
  .paradox_strip_srcref(function(x) {
    x = levels[[x]]
    if (!is.null(trafo)) x = trafo(x)
    x
  })
}

#' @rdname Domain
#' @export
p_fct = function(levels, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  # `aggr`/`in_tune_fn`/`disable_in_tune` are validated by the sole native
  # Domain admission owner, which reports them by argument name for all five
  # constructors; boundary code must not restate cargo rules.
  trafo = .paradox_strip_srcref(trafo)
  aggr = .paradox_strip_srcref(aggr)
  in_tune_fn = .paradox_strip_srcref(in_tune_fn)
  constargs = as.list(match.call()[-1])
  levels = eval.parent(constargs$levels)
  if (!is.character(levels)) {
    # if the "levels" argument is not a character vector, then
    # we add a trafo.
    if (!is.atomic(levels) && !is.list(levels)) {
      stop("`levels` must be an atomic vector or list", call. = FALSE)
    }
    if (is.null(names(levels))) {
      # `as.character()` may return a deferred-string ALTREP vector. Names are
      # structural Domain/representation metadata, not a semantic ALTREP
      # position, so materialize the package-generated carrier once here.
      names(levels) = c(as.character(levels))
    }
    if (is.list(levels)) {
      # `names<-` on a referenced list of at least 64 elements answers with a
      # base wrapper ALTREP (R >= 4.3), and the caller's own list may already
      # be one. The closure below captures this carrier, and legacy migration
      # walks that closure's environment, where a structural list ALTREP is
      # rejected before it is observed. Materialize the carrier once here;
      # element identities and names are unchanged.
      levels = .paradox_materialize_list_carrier(levels)
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
