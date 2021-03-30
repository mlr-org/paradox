
#' @rdname Domain
#' @export
p_s6 = function(support, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL, init) {
  set = S6Cache$canonicalize(support)
  support_description = if (is.character(support)) support else S6Cache$set6_sane_repr(object)

  if ((("power" %in% names(set)) && set$power != 1) || set$class %nin% c("numeric", "integer")) {
    storage_type = "list"
  } else {
    storage_type = set$class
  }

  domain(cls = "ParamSet6", grouping = support_description,
    cargo = set,
    lower = suppressWarnings(as.numeric(set$lower)),
    upper = suppressWarnings(as.numeric(set$upper)),
    levels = as.list(set$elements),
    special_vals = special_vals,
    default = default,
    tags = tags,
    trafo = trafo,
    storage_type = storage_type,
    depends_expr = substitute(depends),
    init = init)
}

#' @export
domain_check.ParamSet6 = function(param, values) {
  # we can rely on 'grouping' always giving us the same class
  set = set6_cache_get(param$grouping[[1]])
  if (set$contains(values, all = TRUE)) {
    return(TRUE)
  }
  nomatches = param$id[!set$contains(values, all = FALSE)]
  sprintf("%s: not contained in %s.", str_collapse(nomatches, sep = ", "), set$strprint())
}

#' @export
domain_nlevels.ParamSet6 = function(param) {
  # we can rely on 'grouping' always giving us the same class
  set = set6_cache_get(param$grouping[[1]])
  card = set$properties$cardinality
  card = if (!is.numeric(card)) Inf
  rep(card, nrow(param))
}
#' @export
domain_is_bounded.ParamSet6 = function(param) {
  # we can rely on 'grouping' always giving us the same class
  set = set6_cache_get(param$grouping[[1]])
  card = set$properties$cardinality
  card = if (!is.numeric(card)) Inf
  boundedness = is.finite(card) || (is.finite(set$lower) && is.finite(set$upper))
  rep(boundedness, nrow(param))
}
#' @export
domain_qunif.ParamSet6 = function(param, x) stop("undefined")


#' @export
domain_is_number.ParamSet6 = function(param) {
  param$storage_type[[1]] != "list"
}
#' @export
domain_is_categ.ParamSet6 = function(param) {
  set = set6_cache_get(param$grouping[[1]])
  categness = ("power" %nin% names(set) || set$power == 1) &&
    set$class %nin% c("numeric", "integer") && is.finite(set$properties$cardinality)

  categness
}
