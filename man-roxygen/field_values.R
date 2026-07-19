#' @field values (named `list()`)\cr
#' Currently set / fixed parameter values.
#' Settable. By default feasibility is checked when values are assigned.
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
