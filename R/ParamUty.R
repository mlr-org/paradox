
#' @rdname Domain
#' @export
p_uty = function(custom_check = NULL, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, repr = substitute(default), init, aggr = NULL, in_tune_fn = NULL, disable_in_tune = NULL) {
  if (!is.null(custom_check) && !is.function(custom_check)) {
    stop("`custom_check` must be a function", call. = FALSE)
  }
  if (!is.null(custom_check)) {
    custom_check_result = custom_check(1)
    if (!isTRUE(.Call(C_domain_uty_check_result, custom_check_result))) {
      stop(
        "The result of `custom_check(1)` must be TRUE or one non-missing string",
        call. = FALSE
      )
    }
  }
  repr = if (!is_nodefault(default)) {
    deparse(repr)[[1]]
  } else {
    "NoDefault"
  }
  cargo = list(custom_check = custom_check, repr = repr)
  cargo$aggr = aggr
  cargo$in_tune_fn = in_tune_fn
  cargo$disable_in_tune = disable_in_tune

  Domain(cls = "ParamUty", grouping = "ParamUty", cargo = cargo, special_vals = special_vals, default = default, tags = tags, trafo = trafo, storage_type = "list", depends_expr = substitute(depends), init = init)
}
