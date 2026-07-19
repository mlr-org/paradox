#' @param special_vals (`list()`)\cr
#'   Arbitrary special values this parameter is allowed to take, to make it
#'   feasible. This allows extending the domain of the parameter. Note that
#'   these values are only used in feasibility checks, neither in generating
#'   designs nor sampling. The outer `special_vals` list shell, names, and list
#'   metadata must always be ordinary non-ALTREP/non-S4. For `p_dbl()`,
#'   `p_int()`, `p_fct()`, and `p_lgl()`, each leaf must not be ALTREP; an S4
#'   leaf is an opaque pointer-identity token and matches only the exact same
#'   object. `p_uty()` leaves are opaque and may include S4 objects. Its
#'   Paradox-1 special membership is preserved with base `identical()` against
#'   each leaf, including S4, without S3/S4 dispatch.
