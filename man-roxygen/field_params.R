#' @field params ([data.table::data.table()])\cr
#' `data.table` representing the combined [`Domain`] objects used to construct the [`ParamSet`].
#' This is a supported, detached public accessor. Mutating the returned table
#' does not mutate the `ParamSet`; assign through documented active bindings and
#' methods instead. The facade has an ordinary non-ALTREP/non-S4 structural
#' shell and metadata; canonical semantic columns are detached ordinary
#' snapshots.
