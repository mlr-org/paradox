#' @field values (named `list()`)\cr
#' Raw currently set / fixed parameter values, including Domain-valid dormant
#' entries whose dependencies are currently unsatisfied.
#' Settable. By default Domain, TuneToken, custom-check, and constraint
#' feasibility is checked when values are assigned, but dependency satisfaction
#' is not an assignment precondition. Any constraint receives the active subset
#' of the complete resulting configuration. Use `$get_values()` for the default
#' dependency-filtered view.
#' Set `$assert_values = FALSE` to use the documented unchecked assignment
#' policy; structural admission still applies.
#' Direct checked and unchecked assignment accepts only an ordinary non-ALTREP,
#' non-S4 base list or representation-only S3-classed list, rejects an outer
#' ALTREP before observing length/names/elements, and discards the outer class.
#' The Paradox-1 clear-values spellings (`NULL`, an ordinary attribute-free
#' zero-length atomic/expression vector, or an accepted empty list container)
#' are canonicalized to a named native `list()`. Only
#' `$set_values(.values=)` has the documented one-snapshot outer-list ALTREP
#' exception. Admitted semantic atomic leaves may be stable ALTREP.
#' You do not have to set values for all parameters, but only for a subset.
#' When you set values, all previously set values will be unset / removed.
