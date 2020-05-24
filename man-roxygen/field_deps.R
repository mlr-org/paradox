#' @field deps [data.table::data.table()]\cr
#' Table has cols `id` (`character(1)`) and `on` (`character(1)`) and `cond` ([Condition]).
#' Lists all (direct) dependency parents of a param, through parameter IDs.
#' Internally created by a call to `add_dep`.
#' Settable, if you want to remove dependencies or perform other changes.
