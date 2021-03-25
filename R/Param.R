#' @title Check value validity
#'
#' @description
#' \pkg{checkmate}-like check-function. Check whether a list of values is feasible in the domain.
#' A value is feasible if it is of the same `storage_type`, inside of the bounds or element of
#' `special_vals`. `TuneToken`s are generally *not* accepted, so they should be filtered out
#' before the call, if present.
#'
#' @param x (`any`).
#' @return If successful `TRUE`, if not a string with the error message.
#' @export
domain_check = function(param, values, describe_error = TRUE) {
  ## # either we are directly feasible, or in special vals, if both are untrue return errmsg from 1st check
  ## if (inherits(value, "TuneToken")) {
  ##   return(tryCatch({
  ##     tunetoken_to_ps(x, self, self$id)
  ##     TRUE
  ##   }, error = function(e) paste("tune token invalid:", conditionMessage(e))))
  ## }
  ## has_element(private$.special_vals, x), TRUE, ch)
  if (!test_list(values, len = nrow(param))) return(if (describe_error) "values must be a list" else FALSE)
  if (length(values) == 0) return(TRUE)  # happens when there are no params + values to check
  assert_string(unique(param$grouping))
  special_vals_hit = pmap_lgl(list(param$special_vals, values), has_element)
  if (any(special_vals_hit)) {
    # don't annoy domain_check methods with the burdon of having to filter out
    # values that match special_values
    Recall(param[!special_vals_hit], values[!special_vals_hit], describe_error = describe_error)
  } else {
    UseMethod("domain_check")
  }
}
#' @export
domain_assert = makeAssertionFunction(domain_check)
#' @export
domain_test = function(param, values) domain_check(param, values, describe_error = FALSE)

#' @export
domain_storage_type = function(param) {
  assert_string(unique(param$grouping))
  UseMethod("domain_storage_type")
}

#' @export
domain_nlevels = function(param) {
  assert_string(unique(param$grouping))
  UseMethod("domain_nlevels")
}

#' @export
domain_is_bounded = function(param) {
  assert_string(unique(param$grouping))
  UseMethod("domain_is_bounded")
}

#' @export
domain_is_number = function(param) {
  assert_string(unique(param$grouping))
  UseMethod("domain_is_number")
}

#' @export
domain_is_categ = function(param) {
  assert_string(unique(param$grouping))
  UseMethod("domain_is_categ")
}

#' @export
domain_qunif = function(param, x) {
  assert_string(unique(param$grouping))
  assert(check_number(x, lower = 0, upper = 1), check_numeric(x, lower = 0, upper = 1, any.missing = FALSE, length = nrow(param)))
  UseMethod("domain_qunif")
}

#' @export
domain_sanitize = function(param, values) {
  assert_string(unique(param$grouping))
  UseMethod("domain_sanitize")
}

#' @export
domain_storage_type.default = function(param) rep("list", nrow(param))

#' @export
domain_nlevels.default = function(param) rep(Inf, nrow(param))

#' @export
domain_is_bounded.default = function(param) rep(FALSE, nrow(param))

#' @export
domain_is_number.default = function(param) rep(FALSE, nrow(param))

#' @export
domain_is_categ.default = function(param) rep(FALSE, nrow(param))

#' @export
domain_is_categ.default = function(param) rep(FALSE, nrow(param))

#' @export
domain_qunif.default = function(param) stop("undefined")

#' @export
domain_sanitize.default = function(param, values) values
