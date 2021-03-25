
#' @rdname Domain
#' @export
p_s6 = function(support, special_vals = list(), default = NO_DEF, tags = character(), depends = NULL, trafo = NULL) {
  support_object = set6_cache_dedup(support)
  support_description = if (is.character(support)) support else set6_sane_repr(object)

  domain(cls = "ParamSet6", grouping = support_description,
    lower = suppressWarnings(as.numeric(support_object$lower)),
    upper = suppressWarnings(as.numeric(support_object$upper)),
    levels = as.list(support_object$elements),
    special_vals = special_vals,
    default = default,
    tags = tags,
    trafo = trafo,
    depends_expr = depends_expr)
}

#' @export
domain_check.ParamSet6 = function(param, values, describe_error = TRUE) {
  # we can rely on 'grouping' always giving us the same class
  set = set6_cache_get(param$grouping[[1]])
  if (set$contains(values, all = TRUE)) {
    return(TRUE)
  }
  if (!describe_error) return(FALSE)
  nomatches = param$id[!set$contains(values, all = FALSE)]
  sprintf("%s: not contained in %s.", str_collapse(nomatches, sep = ", "), set$strprint())
}

#' @export
domain_storage_type.ParamSet6 = function(param) {
  set = set6_cache_get(param$grouping[[1]])
  if (set$power != 1 || set$class %nin% c("numeric", "integer")) {
    storage_type = "list"
  } else {
    storage_type = set$class
  }
  rep(storage_type, nrow(param))
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
domain_is_number.ParamSet6 = function(param) {
  rep(domain_storage_type.ParamSet6(param[1]) != "list", nrow(param))
}
#' @export
domain_is_categ.ParamSet6 = function(param) {
  set = set6_cache_get(param$grouping[[1]])
  categness = set$power == 1 && set$class %nin% c("numeric", "integer") && is.finite(set$properties$cardinality)

  rep(categness, nrow(param))
}
#' @export
domain_qunif.ParamSet6 = function(param, x) stop("undefined")



set6_cache = new.env(parent = emptyenv())

set6_cache_get = function(description) {
  assert_string(description)
  set = set6_cache[[description]]
  if (!is.null(set)) return(set)
  s6ns = asNamespace("set6")
  settypes = keep(names(s6ns), function(n) {
    obj = s6ns[[n]]
    is.environment(obj) && is.function(obj[["get_inherit"]]) && (identical(obj, set6::Set) || identical(obj$get_inherit(), set6::Set))
  })
  get_settype = function(st, error_on_not_found = TRUE) {
    matches = settypes[match(tolower(st), tolower(settypes), nomatch = 0)]
    if (!length(matches)) {
      if (error_on_not_found) {
        stopf("Unknown settype '%s' in description '%s'", st, description)
      } else {
        return(NULL)
      }
    }
    if (length(matches) > 1) {
      matches = settypes[match(st, settypes, nomatch = 0)]
      # even error if error_on_not_found is FALSE, because this is a special message
      if (length(matches) != 1) stopf("Settype '%s' in description '%s' matched multiple set6 classes, but none of them matches by case.",st, description)
    }
    s6ns[[matches]]
  }
  intervaldef = regmatches(description, regexec("^([[(])(([^,]*),([^,|]*))?(\\| *([a-zA-Z]+))? *([])])$", description))[[1]]
  discretedef = regmatches(description, regexec("^\\{([^{}|]*)(\\| *([a-zA-Z]+))? *\\}$", description))[[1]]
  trimdesc = trimws(description)
  generated = NULL
  if (length(intervaldef)) {  # interval definition found
    type = paste0(intervaldef[[2]], intervaldef[[8]])
    lower = if (intervaldef[[4]] == "") -Inf else suppressWarnings(as.numeric(intervaldef[[4]]))
    upper = if (intervaldef[[5]] == "") Inf else suppressWarnings(as.numeric(intervaldef[[5]]))
    if (is.na(lower) || is.na(upper)) stopf("Description '%s' is ill-formed interval expression.", description)
    universe = if (intervaldef[[7]] == "") set6::ExtendedReals else get_settype(intervaldef[[7]])
    universe = universe$new()
    generated = set6::Interval$new(lower = lower, upper = upper, type = type, class = universe$class, universe = universe)
  } else if (length(discretedef)) {
    if (discretedef[[2]] == "") {
      entries = character(0)
    } else {
      entries = strsplit(discretedef[[2]], " *, *")[[1]]
      if ("" %in% entries) stopf("Empty string element not allowed in description '%s'", description)
    }
    universe = if (intervaldef[[4]] == "") set6::Universal else get_settype(intervaldef[[4]])
    universe = universe$new()
    generated = set6::Set$new(elements = entries, universe = universe)
  } else if (!is.null(get_settype(trimdesc))) {
    generated = get_settype(trimdesc)$new()
  } else if (tolower(substr(trimdesc, 1, 1)) == "n" && !is.null(get_settype(trimdesc))) {
    generated = set6::setpower(get_settype(trimdesc)$new(), "n")
  } else {
    stopf("Description '%s' not in set6_cache and could not be constructed.", description)  # how do we automatically generate a set from a string?
  }
  set6_cache_enter(generated, description)
  generated
}

set6_cache_enter = function(object, description = NULL) {
  set6_cache[[set6_sane_repr(object)]] = object
  if (!is.null(description)) {
    set6_cache[[description]] = object
  }
}

set6_cache_dedup = function(object) {
  if (is.character(object)) return(set6_cache_get(object))
  assert_class(object, "Set")
  repr = set6_sane_repr(object)
  alternative = set6_cache[[description]]
  if (!is.null(alternative)) return(alternative)
  set6_cache_enter(object)
  object
}

set6_sane_repr = function(object) {
  assert_class(object, "Set")
  using_unicode_for_whatever_reason = set6::useUnicode()
  on.exit(set6::useUnicode(using_unicode_for_whatever_reason))
  set6::useUnicode(FALSE)
  object$strprint(n = 1e10)
}
