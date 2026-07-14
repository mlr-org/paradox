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
  callback = NULL, callback_after = NA_integer_) {
  .Call(
    get("C_test_stateful_altrep", envir = asNamespace("paradox")),
    first,
    later,
    as.integer(elt_switch_after),
    as.integer(length_switch_after),
    callback,
    as.integer(callback_after)
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
