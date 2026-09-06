#' @field deps ([data.table::data.table()])\cr
#' Table has cols `id` (`character(1)`) and `on` (`character(1)`) and `cond` ([Condition]).
#' Lists all (direct) dependency parents of a param, through parameter IDs.
#' Internally created by a call to `add_dep`.
#' The returned table and its [`Condition`] elements are detached. Mutating
#' them does not mutate package state. Settable on a base [`ParamSet`] if you
#' want to remove dependencies, copy a graph after narrowing Domains, or perform
#' other bulk changes. Assignment validates the table and Conditions but does
#' not require their right-hand sides to be feasible; an impossible predicate
#' simply leaves its child inactive. Use `$add_dep()` for feasibility-checked
#' interactive authoring. This field is read-only and derived from children on
#' a [`ParamSetCollection`]. The facade and Condition shells have ordinary
#' non-ALTREP/non-S4 structural metadata.
