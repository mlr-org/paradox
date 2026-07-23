#' @title Check Value Validity
#'
#' @description
#' \pkg{checkmate}-like check-function. Check whether a list of values is feasible in the domain.
#' A value is feasible if it is of the same `storage_type`, inside of the bounds or element of
#' `special_vals`. `TuneToken`s are generally *not* accepted, so they should be filtered out
#' before the call, if present.
#'
#' `domain_check` will return `TRUE` for accepted values, a `character(1)` error message otherwise.
#'
#' `domain_test` will return `TRUE` for accepted values, `FALSE` otherwise.
#'
#' `domain_assert` will return the `param` argument silently for accepted values, and throw an error message otherwise.
#'
#' Domain operations use a closed native implementation for [`p_dbl()`],
#' [`p_int()`], [`p_fct()`], [`p_lgl()`], and [`p_uty()`]. They do not perform
#' S3 dispatch. Ordinary invalid built-in values receive an informative native
#' diagnostic that distinguishes missingness, type/shape, integerish, bounds,
#' and factor membership using established checkmate-style message fragments.
#' Scalar missing values keep that missingness diagnosis even when their
#' storage mode would otherwise be incompatible with the Domain.
#' This formatting happens only after native validation fails; it does not call
#' checkmate or repeat the check in R. Exact `conditionCall()` and every
#' checkmate edge-case wording are not compatibility contracts. Malformed or
#' unknown Domain objects raise an error.
#' A Domain's own table shell and structural metadata are ordinary
#' non-ALTREP/non-S4.
#' A value-list shell is likewise structural; admitted semantic atomic leaves
#' may be stable ALTREP. Typed S4 specials match only by pointer identity.
#' ParamUty leaves remain opaque, with special membership alone using base
#' `identical()` (including S4) and no S3/S4 dispatch.
#'
#' @param param (`Domain`).
#' @param values (`any`).
#' @param internal (`logical(1)`)\cr
#'   When set, function arguments are not checked for plausibility and `special_values` are not respected.
#'   This is an optimization for internal purposes and should not be used.
#' @return If successful `TRUE`, if not a string with the error message.
#' @keywords internal
#' @export
domain_check = function(param, values, internal = FALSE) {
  .Call(C_domain_check_builtin, param, values, internal)
}

#' @export
#' @rdname domain_check
domain_assert = makeAssertionFunction(domain_check)

#' @export
#' @rdname domain_check
domain_test = function(param, values) isTRUE(domain_check(param, values))


#' @title The Number of Levels of a Given Domain
#'
#' @description
#' This should be the number of discrete possible levels for discrete type [`Domain`]s such as [`p_int()`] or [`p_fct()`], and
#' `Inf` for continuous or untyped parameters.
#'
#' @param param (`Domain`).
#' @return `numeric`.
#' @keywords internal
#' @export
domain_nlevels = function(param) {
  .Call(C_domain_property_builtin, param, 0L)
}

#' @title Whether a Given Domain is Bounded
#'
#' @description
#' This should generally be `TRUE` when `lower` and `upper` are given and finite, or when the `nlevels` is finite, and `FALSE` otherwise.
#'
#' @param param (`Domain`).
#' @return `logical`.
#' @keywords internal
#' @export
domain_is_bounded = function(param) {
  .Call(C_domain_property_builtin, param, 1L)
}

#' @title Whether a Given Domain is Numeric
#'
#' @description
#' This should generally be `TRUE` for discrete or continuous numeric [`Domain`]s, and `FALSE` otherwise.
#'
#' @param param (`Domain`).
#' @return `logical`.
#' @keywords internal
#' @export
domain_is_number = function(param) {
  .Call(C_domain_property_builtin, param, 2L)
}

#' @title Whether a Given Domain is Categorical
#'
#' @description
#' This should generally be `TRUE` for categorical [`Domain`]s, such as [`p_fct()`] or [`p_lgl()`], and `FALSE` otherwise.
#'
#' @param param (`Domain`).
#' @return `logical`.
#' @keywords internal
#' @export
domain_is_categ = function(param) {
  .Call(C_domain_property_builtin, param, 3L)
}

#' @title Transform a Numeric Value to a Sample
#'
#' @description
#' Return a valid sample from the given [`Domain`], given a value from the interval `[0, 1]`.
#'
#' @param param (`Domain`).
#' @param x `numeric` between 0 and 1.
#'   Stable ALTREP semantic vectors are materialized once; structural Domain
#'   and name metadata must remain ordinary non-ALTREP/non-S4.
#' @return `any` -- format depending on the `Domain`.
#' @keywords internal
#' @export
domain_qunif = function(param, x) {
  .Call(C_domain_qunif_builtin, param, x)
}

#' @title Map to Acceptable Value
#'
#' @description
#' Map values that are close enough to the given [`Domain`] to values that are truly acceptable.
#'
#' This is used to map `numeric()` values that are close to but outside the acceptable interval to the interval bounds.
#' It is also used to convert integer-valued `numeric` values to `integer` values for [`p_int()`].
#'
#' @param param (`Domain`).
#' @param values (`any`) -- format depending on the `Domain`. Structural outer
#'   list metadata must be ordinary non-ALTREP/non-S4; admitted semantic atomic
#'   leaves may be stable ALTREP.
#' @return `any` -- format depending on the `Domain`.
#' @keywords internal
#' @export
domain_sanitize = function(param, values) {
  .Call(C_domain_sanitize_builtin, param, values)
}
