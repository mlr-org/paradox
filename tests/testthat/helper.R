`[[.R6` = function(x, i, ...) {
  if (exists(i, envir = x, inherits = FALSE)) {
    return(get(i, envir = x))
  }
  stop("R6 class ", paste0(class(x), collapse = "/"), " does not have slot '", i, "'!")
}

`$.R6` = function(x, name) {
  if (exists(name, envir = x, inherits = FALSE)) {
    return(get(name, envir = x))
  }
  stop("R6 class ", paste0(class(x), collapse = "/"), " does not have slot '", name, "'!")
}

# Internal native fixture for adversarial snapshot/lifetime tests. Its counters
# are zero-based: `switch_after = n` serves the first `n` Elt/Length calls from
# `first`, then switches to `later`; NA disables that switch/callback.
native_stateful_altrep = function(first, later,
  elt_switch_after = NA_integer_, length_switch_after = NA_integer_,
  callback = NULL, callback_after = NA_integer_,
  duplicate_returns_self = FALSE) {
  if (is.list(first) && getRversion() < "4.3.0") {
    stop(
      "List ALTREP fixtures require the leading reviewed capability guard",
      call. = FALSE
    )
  }
  if (!is.logical(duplicate_returns_self) ||
      length(duplicate_returns_self) != 1L ||
      is.na(duplicate_returns_self)) {
    stop("`duplicate_returns_self` must be TRUE or FALSE")
  }
  callback_control = as.integer(callback_after)
  if (duplicate_returns_self) {
    if (length(callback_control) == 1L) {
      callback_control = c(callback_control, NA_integer_)
    }
    callback_control = c(callback_control, 1L)
  }
  .Call(
    get("C_test_stateful_altrep", envir = asNamespace("paradox")),
    first,
    later,
    as.integer(elt_switch_after),
    as.integer(length_switch_after),
    callback,
    callback_control
  )[[1L]]
}

# Rearm relative to the next Elt/Length observations. This is needed after R
# validates special attributes such as names while installing the fixture.
native_stateful_altrep_rearm = function(value, callback_after = 0L) {
  invisible(.Call(
    get("C_test_stateful_altrep_rearm", envir = asNamespace("paradox")),
    value,
    as.integer(callback_after)
  ))
}

skip_if_no_list_altrep = function() {
  testthat::skip_if(
    getRversion() < "4.3.0",
    "R < 4.3 cannot construct list ALTREP test fixtures"
  )
}

skip_if_no_active_binding_inspection = function() {
  testthat::skip_if(
    getRversion() < "4.0.0",
    "R < 4.0 cannot safely inspect active-binding functions"
  )
}

skip_if_no_old_r_binding_existence_path = function() {
  testthat::skip_if(
    getRversion() >= "4.2.0",
    "R >= 4.2 has a public non-evaluating binding-existence operation"
  )
}
