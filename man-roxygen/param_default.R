#' @param default (`any`)\cr
#'   Default value. Can be from the domain of the parameter or an element of
#'   `special_vals`. Has value [NO_DEF] if no default exists. `NULL` can be a
#'   valid default. For `p_dbl()`, `p_int()`, `p_fct()`, and `p_lgl()`, an S4
#'   default is accepted only when it is pointer-identical to an admitted S4
#'   special value. `p_uty()` defaults are opaque and may be S4; membership in
#'   its special values uses base `identical()` without S3/S4 dispatch.
#'   The value has no effect on `ParamSet$values` or the behavior of
#'   `ParamSet$check()`, `$test()` or `$assert()`.
#'   The `default` is intended to be used for documentation purposes.
