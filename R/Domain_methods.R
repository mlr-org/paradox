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
#' @param param (`Domain`).
#' @param values (`any`).
#' @return If successful `TRUE`, if not a string with the error message.
#' @keywords internal
#' @export
domain_check = function(param, values) {
  if (!test_list(values, len = nrow(param))) return("values must be a list")
  if (length(values) == 0) return(TRUE)  # happens when there are no params + values to check
  assert_string(unique(param$grouping))
  special_vals_hit = pmap_lgl(list(param$special_vals, values), has_element)
  if (any(special_vals_hit)) {
    # don't annoy domain_check methods with the burdon of having to filter out
    # values that match special_values
    Recall(param[!special_vals_hit], values[!special_vals_hit])
  } else {
    UseMethod("domain_check")
  }
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
#' @param x (`Domain`).
#' @return `numeric`.
#' @keywords internal
#' @export
domain_nlevels = function(param) {
  if (!nrow(param)) return(integer(0))
  assert_string(unique(param$grouping))
  UseMethod("domain_nlevels")
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
  if (!nrow(param)) return(logical(0))
  assert_string(unique(param$grouping))
  UseMethod("domain_is_bounded")
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
  if (!nrow(param)) return(logical(0))
  assert_string(unique(param$grouping))
  UseMethod("domain_is_number")
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
  if (!nrow(param)) return(logical(0))
  assert_string(unique(param$grouping))
  UseMethod("domain_is_categ")
}

#' @title Transform a Numeric Value to a Sample
#'
#' @description
#' Return a valid sample from the given [`Domain`], given a value from the interval `[0, 1]`.
#'
#' @param param (`Domain`).
#' @param x `numeric` between 0 and 1.
#' @return `any` -- format depending on the `Domain`.
#' @keywords internal
#' @export
domain_qunif = function(param, x) {
  if (!nrow(param)) return(logical(0))
  assert_string(unique(param$grouping))
  assert_numeric(x, lower = 0, upper = 1, any.missing = FALSE)
  assert_true(length(x) %% length(nrow(param)) == 0)
  UseMethod("domain_qunif")
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
#' @param values (`any`) -- format depending on the `Domain`.
#' @return `any` -- format depending on the `Domain`.
#' @keywords internal
#' @export
domain_sanitize = function(param, values) {
    if (!nrow(param)) return(values)
  assert_string(unique(param$grouping))
  UseMethod("domain_sanitize")
}

#' @export
domain_nlevels.Domain = function(param) rep(Inf, nrow(param))

#' @export
domain_is_bounded.Domain = function(param) rep(FALSE, nrow(param))

#' @export
domain_qunif.Domain = function(param, x) stop("undefined")

#' @export
domain_sanitize.Domain = function(param, values) values

#' @export
domain_is_categ.Domain = function(param) rep(FALSE, nrow(param))

#' @export
domain_is_number.Domain = function(param) rep(FALSE, nrow(param))


# param:
check_domain_vectorize = function(ids, values, checker, more_args = list()) {
  if (is.function(checker)) {
    errors = pmap(c(list(ids, values), more_args), function(id, value, ...) {
      ch = checker(value, ...)
      if (isTRUE(ch)) NULL else sprintf("%s: %s", id, ch)
    })
  } else {
    # `checker` is a list of functions with the same length as `values`
    errors = pmap(c(list(ids, values, checker), more_args), function(id, value, chck, ...) {
      ch = chck(value, ...)
      if (isTRUE(ch)) NULL else sprintf("%s: %s", id, ch)
    })
  }
  errors = unlist(errors)
  if (!length(errors)) return(TRUE)
  str_collapse(errors, sep = "\n")
}
