# Legacy object migration is deliberately separate from the current execution
# engine.  Everything below inspects inert bindings and ordinary objects; it
# never calls a method or active binding on the object being migrated.

.upgrade_paradox_abort = function(path, message, ...) {
  stop(
    sprintf(
      "Cannot upgrade Paradox object at %s: %s",
      path,
      sprintf(message, ...)
    ),
    call. = FALSE
  )
}

.upgrade_paradox_binding = function(owner, name, path, required = TRUE) {
  if (!is.environment(owner)) {
    .upgrade_paradox_abort(path, "expected an environment")
  }
  if (!exists(name, envir = owner, inherits = FALSE)) {
    if (!required) return(NULL)
    .upgrade_paradox_abort(path, "missing binding `%s`", name)
  }
  if (bindingIsActive(name, owner)) {
    .upgrade_paradox_abort(path, "binding `%s` must not be active", name)
  }

  # Unlike get(), substitute() does not force a delayed binding.  Canonical
  # serialized R6 instances contain realized ordinary bindings.  A language
  # object here is consequently a delayed or reconstructed binding, not one of
  # the admitted legacy payload values.
  value = eval(call("substitute", as.name(name), owner), envir = baseenv())
  if (is.language(value) || is.symbol(value)) {
    .upgrade_paradox_abort(path, "binding `%s` is delayed or malformed", name)
  }
  value
}

.upgrade_paradox_shell = function(x, path) {
  if (!is.environment(x) || !inherits(x, "ParamSet")) {
    .upgrade_paradox_abort(path, "expected a ParamSet R6 object")
  }
  if (!exists(".__enclos_env__", envir = x, inherits = FALSE) ||
      bindingIsActive(".__enclos_env__", x)) {
    .upgrade_paradox_abort(path, "missing canonical R6 enclosure")
  }
  enclosing = .upgrade_paradox_binding(x, ".__enclos_env__", path)
  if (!is.environment(enclosing)) {
    .upgrade_paradox_abort(path, "malformed R6 enclosure")
  }
  private = .upgrade_paradox_binding(enclosing, "private", path)
  self = .upgrade_paradox_binding(enclosing, "self", path)
  if (!is.environment(private) || !identical(self, x)) {
    .upgrade_paradox_abort(path, "malformed R6 self/private relationship")
  }
  list(enclosing = enclosing, private = private)
}

# Keep current-schema admission behind one helper. The empty, non-strict native
# check walks BASE, COLLECTION, and SHADOW graphs without running constraints,
# utility checks, or transformations. It validates all ten capsule fields,
# callback shapes, child translations, sharing, and active-path cycles. This is
# deliberately separate from legacy reconstruction: current-state admission
# and legacy conversion have different contracts and neither is a fallback for
# the other.
.upgrade_paradox_validate_current_graph = function(x, path) {
  shell = .upgrade_paradox_shell(x, path)
  empty_values = structure(list(), names = character())
  result = tryCatch(
    .Call(
      C_param_set_check_builtin,
      shell$private,
      x,
      empty_values,
      FALSE,
      FALSE,
      "none",
      TRUE
    ),
    error = function(error) {
      .upgrade_paradox_abort(
        path,
        "corrupt current state capsule (%s)",
        conditionMessage(error)
      )
    }
  )
  if (!identical(result, TRUE)) {
    .upgrade_paradox_abort(path, "corrupt current state capsule")
  }
  invisible(NULL)
}

.upgrade_paradox_current = function(x, shell, path) {
  private = shell$private
  if (!exists(".core", envir = private, inherits = FALSE)) return(NULL)
  core = .upgrade_paradox_binding(private, ".core", path)
  kind = tryCatch(
    .Call(C_param_set_core_kind, core),
    error = function(error) {
      .upgrade_paradox_abort(
        path,
        "unknown or corrupt current state capsule (%s)",
        conditionMessage(error)
      )
    }
  )
  class_ok = switch(
    as.character(kind),
    `1` = inherits(x, "ParamSet") &&
      !inherits(x, "ParamSetCollection") &&
      !inherits(x, "ParamSetShadow"),
    `2` = inherits(x, "ParamSetCollection"),
    `3` = inherits(x, "ParamSetShadow"),
    FALSE
  )
  if (!isTRUE(class_ok)) {
    .upgrade_paradox_abort(path, "state capsule kind disagrees with shell class")
  }
  .upgrade_paradox_validate_current_graph(x, path)
  x
}

.upgrade_paradox_authenticate_legacy_shell = function(x, shell, kind, path) {
  expected_class = if (kind == "base") {
    c("ParamSet", "R6")
  } else {
    c("ParamSetCollection", "ParamSet", "R6")
  }
  observed_class = .upgrade_paradox_materialize_atomic(
    attr(x, "class", exact = TRUE)
  )
  if (!identical(observed_class, expected_class)) {
    .upgrade_paradox_abort(
      path,
      "legacy third-party subclasses are unsupported (class is %s)",
      paste(observed_class, collapse = "/")
    )
  }
  if (!environmentIsLocked(x)) {
    .upgrade_paradox_abort(path, "legacy R6 shell is not locked")
  }

  enclosing = shell$enclosing
  namespace = parent.env(enclosing)
  if (!isNamespace(namespace) || !identical(environmentName(namespace), "paradox")) {
    .upgrade_paradox_abort(path, "legacy methods do not originate in paradox")
  }
  method_enclosures = list(enclosing)
  if (kind == "collection") {
    super = .upgrade_paradox_binding(enclosing, "super", path)
    if (!is.environment(super)) {
      .upgrade_paradox_abort(path, "legacy superclass enclosure is malformed")
    }
    base_enclosing = .upgrade_paradox_binding(
      super,
      ".__enclos_env__",
      path
    )
    if (!is.environment(base_enclosing) ||
        !identical(.upgrade_paradox_binding(base_enclosing, "self", path), x) ||
        !identical(
          .upgrade_paradox_binding(base_enclosing, "private", path),
          shell$private
        ) ||
        !identical(parent.env(base_enclosing), namespace)) {
      .upgrade_paradox_abort(path, "legacy superclass enclosure is malformed")
    }
    method_enclosures[[2L]] = base_enclosing
  }
  owns_method = function(method) {
    (is.function(method) && any(vapply(
        method_enclosures,
        identical,
        logical(1L),
        y = environment(method)
      ))) ||
      .upgrade_paradox_resumable_method(method, x, list(namespace))
  }

  required_methods = c("initialize", "ids", "clone")
  required_active = c(
    "values", "tags", "params", "domains", "extra_trafo", "constraint",
    "deps"
  )
  public_names = ls(x, all.names = TRUE)
  if (!all(c(".__enclos_env__", "assert_values", required_methods,
      required_active) %in% public_names)) {
    .upgrade_paradox_abort(path, "legacy R6 surface is incomplete")
  }

  for (name in setdiff(public_names, ".__enclos_env__")) {
    if (bindingIsActive(name, x)) {
      binding = activeBindingFunction(name, x)
      if (!owns_method(binding)) {
        .upgrade_paradox_abort(path, "core binding `%s` was replaced", name)
      }
      next
    }

    value = .upgrade_paradox_binding(x, name, path)
    if (identical(name, "assert_values")) {
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        .upgrade_paradox_abort(path, "`assert_values` is malformed")
      }
    } else if (!owns_method(value)) {
      .upgrade_paradox_abort(path, "core method `%s` was replaced", name)
    }
  }

  internal_methods = c(
    ".store_values", ".get_values", "deep_clone", "get_tune_ps",
    ".add_name_prefix", ".constraint_explicit", ".extra_trafo_explicit",
    ".get_constraint_detached", ".get_extra_trafo_detached"
  )
  private_names = ls(shell$private, all.names = TRUE)
  for (name in intersect(internal_methods, private_names)) {
    value = .upgrade_paradox_binding(shell$private, name, path)
    if (!owns_method(value)) {
      .upgrade_paradox_abort(path, "core private method `%s` was replaced", name)
    }
  }
  invisible(NULL)
}

.upgrade_paradox_materialize_atomic = function(x) {
  if (is.null(x) || !is.atomic(x)) return(x)
  attributes = attributes(x)
  attributes(x) = NULL
  x = x[seq_along(x)]
  attributes(x) = attributes
  x
}

.upgrade_paradox_reject_owner_finalizer = function(
    x,
    shell,
    path,
    qualifier = "legacy") {
  has_public = exists("finalize", envir = x, inherits = FALSE)
  has_private = exists(
    "finalize",
    envir = shell$private,
    inherits = FALSE
  )
  if (has_public || has_private) {
    .upgrade_paradox_abort(
      path,
      paste0(
        "%s registered owner shell has an R6 finalizer; ",
        "identity-preserving finalizer transplantation is unsupported"
      ),
      qualifier
    )
  }
  invisible(NULL)
}

.upgrade_paradox_inert_binding_value = function(owner, name) {
  if (!is.environment(owner) ||
      !exists(name, envir = owner, inherits = FALSE) ||
      bindingIsActive(name, owner)) {
    return(list(ok = FALSE))
  }
  value = eval(call("substitute", as.name(name), owner), envir = baseenv())
  if (is.language(value) || is.symbol(value)) {
    return(list(ok = FALSE))
  }
  list(ok = TRUE, value = value)
}

# A non-interruptible transplant replaces the public bindings before it swaps
# `.__enclos_env__`, which is the single completion point. If a catastrophic
# allocation error interrupts that short binding wave, a retry still sees the
# old private state. The already-refreshed method closures are admitted only
# when their enclosure has the expected package parent, points back to this
# shell, and carries a canonical current capsule.
.upgrade_paradox_resumable_method = function(method, self, namespaces) {
  if (!is.function(method) || !is.environment(environment(method))) {
    return(FALSE)
  }
  enclosing = environment(method)
  if (!any(vapply(
    namespaces,
    identical,
    logical(1L),
    y = parent.env(enclosing)
  ))) {
    return(FALSE)
  }
  stored_self = .upgrade_paradox_inert_binding_value(enclosing, "self")
  stored_private = .upgrade_paradox_inert_binding_value(enclosing, "private")
  if (!stored_self$ok || !stored_private$ok ||
      !identical(stored_self$value, self) ||
      !is.environment(stored_private$value)) {
    return(FALSE)
  }
  stored_core = .upgrade_paradox_inert_binding_value(
    stored_private$value,
    ".core"
  )
  stored_core$ok && isTRUE(tryCatch(
    .Call(C_param_set_core_kind, stored_core$value) %in% 1:3,
    error = function(...) FALSE
  ))
}

.upgrade_paradox_copy_list = function(x) {
  if (!is.list(x) || is.object(x)) return(x)
  result = lapply(x, function(value) {
    if (is.atomic(value)) .upgrade_paradox_materialize_atomic(value) else value
  })
  names(result) = .upgrade_paradox_materialize_atomic(names(x))
  result
}

.upgrade_paradox_materialize_columns = function(columns) {
  lapply(columns, function(column) {
    if (is.atomic(column)) {
      .upgrade_paradox_materialize_atomic(column)
    } else if (is.list(column)) {
      .upgrade_paradox_copy_list(column)
    } else {
      column
    }
  })
}

.upgrade_paradox_table = function(
    x,
    columns,
    path,
    classes = c("data.table", "data.frame"),
    extra_attributes = character()) {
  observed_names = .upgrade_paradox_materialize_atomic(
    attr(x, "names", exact = TRUE)
  )
  observed_classes = .upgrade_paradox_materialize_atomic(
    attr(x, "class", exact = TRUE)
  )
  if (!is.list(x) ||
      !identical(observed_classes, classes) ||
      !identical(observed_names, columns)) {
    .upgrade_paradox_abort(
      path,
      "expected canonical data.table columns `%s`",
      paste(columns, collapse = "`, `")
    )
  }
  materialized = lapply(seq_along(columns), function(index) {
    .subset2(x, index)
  })
  names(materialized) = columns
  materialized = .upgrade_paradox_materialize_columns(materialized)
  lengths = lengths(materialized)
  if (length(lengths) && any(lengths != lengths[[1L]])) {
    .upgrade_paradox_abort(path, "table columns have inconsistent lengths")
  }
  attributed = names(materialized)[vapply(
    materialized,
    function(column) !is.null(attributes(column)),
    logical(1L)
  )]
  if (length(attributed)) {
    .upgrade_paradox_abort(
      path,
      "table column `%s` has unsupported attributes",
      attributed[[1L]]
    )
  }
  allowed_attributes = c(
    "names", "row.names", "class", ".internal.selfref", "sorted", "index",
    extra_attributes
  )
  unexpected = setdiff(names(attributes(x)), allowed_attributes)
  if (length(unexpected)) {
    .upgrade_paradox_abort(
      path,
      "table has unsupported attributes: %s",
      paste(unexpected, collapse = ", ")
    )
  }
  materialized
}

.upgrade_paradox_internal_table = function(columns) {
  param_set_internal_table(.upgrade_paradox_materialize_columns(columns))
}

.upgrade_paradox_domain_table = function(columns) {
  param_set_data_table_facade(.upgrade_paradox_materialize_columns(columns))
}

.upgrade_paradox_condition = function(cond, path, allow_base = FALSE) {
  classes = .upgrade_paradox_materialize_atomic(
    attr(cond, "class", exact = TRUE)
  )
  if (!is.list(cond) || length(cond) != 2L ||
      !identical(names(cond), c("rhs", "condition_format_string"))) {
    .upgrade_paradox_abort(path, "malformed legacy Condition")
  }
  rhs = .upgrade_paradox_materialize_atomic(.subset2(cond, 1L))
  format = .subset2(cond, 2L)
  plain_rhs = rhs
  if (is.atomic(plain_rhs)) attributes(plain_rhs) = NULL

  if (identical(classes, c("CondEqual", "Condition"))) {
    if (!is.atomic(rhs) || length(rhs) != 1L || anyNA(plain_rhs) ||
        !identical(format, "%s == %s")) {
      .upgrade_paradox_abort(path, "malformed legacy CondEqual")
    }
    return(CondEqual(rhs))
  }
  if (identical(classes, c("CondAnyOf", "Condition"))) {
    if (!is.atomic(rhs) || !length(rhs) || anyNA(plain_rhs) ||
        anyDuplicated(plain_rhs) ||
        !identical(format, "%s %%in%% {%s}")) {
      .upgrade_paradox_abort(path, "malformed legacy CondAnyOf")
    }
    return(CondAnyOf(rhs))
  }
  if (allow_base && identical(classes, "Condition")) {
    if (!is.character(format) || length(format) != 1L || is.na(format)) {
      .upgrade_paradox_abort(path, "malformed legacy Condition format")
    }
    return(Condition(rhs, format))
  }
  .upgrade_paradox_abort(
    path,
    "unsupported Condition class `%s`",
    paste(classes, collapse = "/")
  )
}

.upgrade_paradox_requirements = function(requirements, path) {
  if (!is.list(requirements) || is.object(requirements)) {
    .upgrade_paradox_abort(path, "Domain requirements must be a plain list")
  }
  lapply(seq_along(requirements), function(index) {
    requirement = requirements[[index]]
    requirement_path = sprintf("%s[[%d]]", path, index)
    if (!is.list(requirement) || is.object(requirement) ||
        !identical(names(requirement), c("on", "cond"))) {
      .upgrade_paradox_abort(requirement_path, "malformed Domain requirement")
    }
    on = .upgrade_paradox_materialize_atomic(.subset2(requirement, "on"))
    if (
        !is.character(on) || length(on) != 1L ||
        !is.null(attributes(on)) || is.na(on)) {
      .upgrade_paradox_abort(requirement_path, "malformed Domain requirement")
    }
    list(
      on = on,
      cond = .upgrade_paradox_condition(
        .subset2(requirement, "cond"),
        paste0(requirement_path, "$cond")
      )
    )
  })
}

.upgrade_paradox_validate_domain_columns = function(columns, path) {
  scalar_character = c("id", "cls", "grouping", "storage_type")
  scalar_double = c("lower", "upper", "tolerance")
  list_columns = c("cargo", "levels", "special_vals", "default")
  for (name in scalar_character) {
    value = columns[[name]]
    if (!is.character(value) || anyNA(value)) {
      .upgrade_paradox_abort(path, "Domain column `%s` must be character", name)
    }
  }
  for (name in scalar_double) {
    if (!is.double(columns[[name]])) {
      .upgrade_paradox_abort(path, "Domain column `%s` must be double", name)
    }
  }
  for (name in list_columns) {
    if (!is.list(columns[[name]])) {
      .upgrade_paradox_abort(path, "Domain column `%s` must be a list", name)
    }
  }

  supported = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl", "ParamUty")
  if (any(columns$cls %nin% supported)) {
    bad = columns$cls[columns$cls %nin% supported][[1L]]
    .upgrade_paradox_abort(path, "unsupported Domain class `%s`", bad)
  }
  expected_storage = c(
    ParamDbl = "numeric", ParamInt = "integer", ParamFct = "character",
    ParamLgl = "logical", ParamUty = "list"
  )
  mismatch = columns$storage_type != unname(expected_storage[columns$cls])
  if (any(mismatch)) {
    .upgrade_paradox_abort(
      sprintf("%s[%d]", path, which(mismatch)[[1L]]),
      "Domain class and storage type disagree"
    )
  }

  for (index in seq_along(columns$cargo)) {
    cargo = columns$cargo[[index]]
    if (!is.null(cargo) && (!is.list(cargo) || is.object(cargo))) {
      .upgrade_paradox_abort(
        sprintf("%s$cargo[[%d]]", path, index),
        "unsupported Domain cargo"
      )
    }
  }
  invisible(NULL)
}

.upgrade_paradox_domain = function(domain, path) {
  classes = .upgrade_paradox_materialize_atomic(
    attr(domain, "class", exact = TRUE)
  )
  supported = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl", "ParamUty")
  kind = if (length(classes)) classes[[1L]] else ""
  if (kind %nin% supported ||
      !identical(classes, c(kind, "Domain", "data.table", "data.frame"))) {
    .upgrade_paradox_abort(
      path,
      "unsupported Domain class `%s`",
      paste(classes, collapse = "/")
    )
  }
  columns = .upgrade_paradox_table(
    domain,
    domain_names,
    path,
    classes,
    extra_attributes = "repr"
  )
  if (length(columns$id) != 1L || !identical(columns$cls, kind)) {
    .upgrade_paradox_abort(path, "Domain must contain one matching built-in row")
  }
  .upgrade_paradox_validate_domain_columns(columns, path)
  if (!is.logical(columns$.init_given) || length(columns$.init_given) != 1L ||
      is.na(columns$.init_given) || !is.list(columns$.init) ||
      !is.list(columns$.tags) || !is.list(columns$.trafo) ||
      !is.list(columns$.requirements)) {
    .upgrade_paradox_abort(path, "malformed transient Domain columns")
  }
  columns$.requirements[[1L]] = .upgrade_paradox_requirements(
    columns$.requirements[[1L]],
    paste0(path, "$.requirements[[1]]")
  )
  domain_tags = .upgrade_paradox_materialize_atomic(columns$.tags[[1L]])
  if (!is.character(domain_tags) || !is.null(attributes(domain_tags)) ||
      anyNA(domain_tags) || anyDuplicated(domain_tags)) {
    .upgrade_paradox_abort(path, "malformed Domain tags")
  }
  columns$.tags[[1L]] = domain_tags
  if (!is.null(columns$.trafo[[1L]]) &&
      !is.function(columns$.trafo[[1L]])) {
    .upgrade_paradox_abort(path, "malformed Domain transformation")
  }

  result = .upgrade_paradox_domain_table(columns)
  class(result) = classes
  repr = attr(domain, "repr", exact = TRUE)
  if (!is.null(repr)) attr(result, "repr") = repr
  result
}

.upgrade_paradox_params = function(params, path) {
  columns = .upgrade_paradox_table(params, domain_names_permanent, path)
  .upgrade_paradox_validate_domain_columns(columns, path)
  ids = columns$id
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    .upgrade_paradox_abort(path, "parameter IDs must be nonempty and unique")
  }
  columns
}

.upgrade_paradox_tags = function(tags, ids, path) {
  columns = .upgrade_paradox_table(tags, c("id", "tag"), path)
  id = columns$id
  tag = columns$tag
  if (!is.character(id) || !is.character(tag) || anyNA(id) || anyNA(tag) ||
      any(id %nin% ids)) {
    .upgrade_paradox_abort(path, "malformed legacy tag table")
  }
  .upgrade_paradox_internal_table(list(id = id, tag = tag))
}

.upgrade_paradox_trafos = function(trafos, ids, path) {
  columns = .upgrade_paradox_table(trafos, c("id", "trafo"), path)
  id = columns$id
  trafo = columns$trafo
  if (!is.character(id) || anyNA(id) || any(id %nin% ids) ||
      anyDuplicated(id) || !is.list(trafo) ||
      any(!vapply(trafo, is.function, logical(1L)))) {
    .upgrade_paradox_abort(path, "malformed legacy transformation table")
  }
  .upgrade_paradox_internal_table(list(id = id, trafo = trafo))
}

.upgrade_paradox_deps = function(deps, ids, path) {
  columns = .upgrade_paradox_table(deps, c("id", "on", "cond"), path)
  id = columns$id
  on = columns$on
  cond = columns$cond
  if (!is.character(id) || !is.character(on) || !is.list(cond) ||
      anyNA(id) || anyNA(on) || any(id %nin% ids)) {
    .upgrade_paradox_abort(path, "malformed legacy dependency table")
  }
  cond = lapply(seq_along(cond), function(index) {
    .upgrade_paradox_condition(
      cond[[index]],
      sprintf("%s$cond[[%d]]", path, index)
    )
  })
  .upgrade_paradox_internal_table(list(id = id, on = on, cond = cond))
}

.upgrade_paradox_values = function(values, ids, path) {
  if (!is.list(values) || is.object(values)) {
    .upgrade_paradox_abort(path, "legacy values must be a plain list")
  }
  value_names = .upgrade_paradox_materialize_atomic(names(values))
  if (is.null(value_names)) value_names = character(length(values))
  if (length(value_names) != length(values) || anyNA(value_names) ||
      any(!nzchar(value_names)) || anyDuplicated(value_names) ||
      any(value_names %nin% ids)) {
    .upgrade_paradox_abort(path, "legacy values have invalid parameter names")
  }
  values = .upgrade_paradox_copy_list(values)
  values[match(ids, names(values), nomatch = 0L)]
}

.upgrade_paradox_base_info = function(x, shell, path, authenticate = TRUE) {
  if (authenticate) {
    .upgrade_paradox_authenticate_legacy_shell(x, shell, "base", path)
  }
  private = shell$private
  required = c(
    ".params", ".values", ".tags", ".deps", ".trafos",
    ".extra_trafo", ".constraint"
  )
  if (!all(required %in% ls(private, all.names = TRUE))) {
    .upgrade_paradox_abort(path, "legacy BASE private state is incomplete")
  }
  params = .upgrade_paradox_binding(private, ".params", path)
  param_columns = .upgrade_paradox_params(params, paste0(path, "$private$.params"))
  ids = param_columns$id
  tags = .upgrade_paradox_tags(
    .upgrade_paradox_binding(private, ".tags", path), ids,
    paste0(path, "$private$.tags")
  )
  trafos = .upgrade_paradox_trafos(
    .upgrade_paradox_binding(private, ".trafos", path), ids,
    paste0(path, "$private$.trafos")
  )
  deps = .upgrade_paradox_deps(
    .upgrade_paradox_binding(private, ".deps", path), ids,
    paste0(path, "$private$.deps")
  )
  values = .upgrade_paradox_values(
    .upgrade_paradox_binding(private, ".values", path), ids,
    paste0(path, "$private$.values")
  )
  extra_trafo = .upgrade_paradox_binding(private, ".extra_trafo", path)
  constraint = .upgrade_paradox_binding(private, ".constraint", path)
  if (!is.null(extra_trafo) && !is.function(extra_trafo)) {
    .upgrade_paradox_abort(path, "legacy `extra_trafo` is malformed")
  }
  if (!is.null(constraint) && !is.function(constraint)) {
    .upgrade_paradox_abort(path, "legacy `constraint` is malformed")
  }

  tags_by_id = stats::setNames(vector("list", length(ids)), ids)
  for (index in seq_along(ids)) {
    tags_by_id[[index]] = .subset2(tags, "tag")[.subset2(tags, "id") == ids[[index]]]
  }
  trafo_by_id = stats::setNames(vector("list", length(ids)), ids)
  for (index in seq_along(ids)) {
    match_index = match(ids[[index]], .subset2(trafos, "id"), nomatch = 0L)
    trafo_by_id[index] = list(if (match_index) {
      .subset2(trafos, "trafo")[[match_index]]
    } else {
      NULL
    })
  }
  list(
    kind = "base",
    param_columns = param_columns,
    ids = ids,
    tags = tags,
    tags_by_id = tags_by_id,
    trafos = trafos,
    trafo_by_id = trafo_by_id,
    deps = deps,
    values = values,
    extra_trafo = extra_trafo,
    constraint = constraint,
    assert_values = .upgrade_paradox_binding(x, "assert_values", path)
  )
}

.upgrade_paradox_owner_enclosures = function(x, shell, entry, path) {
  observed_class = .upgrade_paradox_materialize_atomic(
    attr(x, "class", exact = TRUE)
  )
  if (!identical(observed_class, entry$legacy_class)) {
    .upgrade_paradox_abort(path, "owner-upgrader class authentication failed")
  }
  if (!environmentIsLocked(x)) {
    .upgrade_paradox_abort(path, "legacy owner R6 shell is not locked")
  }
  .upgrade_paradox_reject_owner_finalizer(x, shell, path)

  enclosures = list(shell$enclosing)
  enclosing = shell$enclosing
  repeat {
    super = .upgrade_paradox_binding(enclosing, "super", path, required = FALSE)
    if (is.null(super)) break
    if (!is.environment(super)) {
      .upgrade_paradox_abort(path, "legacy owner superclass proxy is malformed")
    }
    next_enclosing = .upgrade_paradox_binding(
      super,
      ".__enclos_env__",
      path
    )
    if (!is.environment(next_enclosing) ||
        any(vapply(enclosures, identical, logical(1L), y = next_enclosing)) ||
        !identical(
          .upgrade_paradox_binding(next_enclosing, "self", path),
          x
        ) ||
        !identical(
          .upgrade_paradox_binding(next_enclosing, "private", path),
          shell$private
        )) {
      .upgrade_paradox_abort(path, "legacy owner superclass enclosure is malformed")
    }
    enclosures[[length(enclosures) + 1L]] = next_enclosing
    enclosing = next_enclosing
  }
  if (length(enclosures) != 2L) {
    .upgrade_paradox_abort(
      path,
      "registered legacy owner shell must have exactly one ParamSet superclass"
    )
  }
  owner_namespace = parent.env(enclosures[[1L]])
  paradox_namespace = parent.env(enclosures[[2L]])
  if (!isNamespace(owner_namespace) ||
      !identical(environmentName(owner_namespace), entry$owner_package) ||
      !isNamespace(paradox_namespace) ||
      !identical(environmentName(paradox_namespace), "paradox")) {
    .upgrade_paradox_abort(
      path,
      "legacy owner methods do not have authenticated package provenance"
    )
  }

  owns_method = function(method) {
    (is.function(method) && any(vapply(
        enclosures,
        identical,
        logical(1L),
        y = environment(method)
      ))) ||
      .upgrade_paradox_resumable_method(
        method,
        x,
        list(entry$.owner_namespace, asNamespace("paradox"))
      )
  }
  public_names = setdiff(ls(x, all.names = TRUE), ".__enclos_env__")
  for (name in public_names) {
    if (bindingIsActive(name, x)) {
      binding = activeBindingFunction(name, x)
      if (!owns_method(binding) &&
          !(name %in% entry$retired_bindings &&
            .upgrade_paradox_is_retired_binding(
              binding,
              name,
              entry$owner_package
            ))) {
        .upgrade_paradox_abort(path, "owner binding `%s` was replaced", name)
      }
      next
    }
    value = .upgrade_paradox_binding(x, name, path)
    if (identical(name, "assert_values")) {
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        .upgrade_paradox_abort(path, "`assert_values` is malformed")
      }
    } else if (!owns_method(value)) {
      .upgrade_paradox_abort(path, "owner method `%s` was replaced", name)
    }
  }
  internal_methods = c(
    ".store_values", ".get_values", "deep_clone", "get_tune_ps",
    ".add_name_prefix", ".constraint_explicit", ".extra_trafo_explicit",
    ".get_constraint_detached", ".get_extra_trafo_detached"
  )
  for (name in intersect(internal_methods, ls(shell$private, all.names = TRUE))) {
    value = .upgrade_paradox_binding(shell$private, name, path)
    if (!owns_method(value)) {
      .upgrade_paradox_abort(path, "owner private method `%s` was replaced", name)
    }
  }
  enclosures
}

.upgrade_paradox_owner_info = function(x, shell, path, classes) {
  entry = .paradox_object_upgrader_lookup(classes)
  if (is.null(entry)) {
    .upgrade_paradox_abort(
      path,
      paste0(
        "legacy third-party subclasses are unsupported without an exact ",
        "registered owner upgrader (class is %s)"
      ),
      paste(classes, collapse = "/")
    )
  }
  .upgrade_paradox_owner_enclosures(x, shell, entry, path)
  assert_values = .upgrade_paradox_binding(x, "assert_values", path)
  inspector = .paradox_object_upgrader_resolve(entry, "inspector")
  descriptor = tryCatch(
    inspector(x),
    error = function(error) {
      .upgrade_paradox_abort(
        path,
        "owner inspector `%s::%s()` failed (%s)",
        entry$owner_package,
        entry$inspector,
        conditionMessage(error)
      )
    }
  )
  descriptor = tryCatch(
    .paradox_validate_object_upgrader_inspection(descriptor, entry),
    error = function(error) {
      .upgrade_paradox_abort(path, "%s", conditionMessage(error))
    }
  )
  dependencies = descriptor$dependencies
  base = if (identical(entry$migration_kind, "additive")) {
    .upgrade_paradox_base_info(x, shell, path, authenticate = FALSE)
  } else {
    NULL
  }
  list(
    kind = "owner",
    owner = entry,
    owner_state = descriptor$state,
    dependencies = dependencies,
    assert_values = assert_values,
    base = base
  )
}

.upgrade_paradox_collection_info = function(x, shell, path) {
  .upgrade_paradox_authenticate_legacy_shell(x, shell, "collection", path)
  private = shell$private
  required = c(
    ".params", ".values", ".tags", ".deps", ".trafos", ".sets",
    ".translation", ".extra_trafo", ".constraint"
  )
  if (!all(required %in% ls(private, all.names = TRUE))) {
    .upgrade_paradox_abort(path, "legacy COLLECTION private state is incomplete")
  }
  sets = .upgrade_paradox_binding(private, ".sets", path)
  if (!is.list(sets) || is.object(sets)) {
    .upgrade_paradox_abort(path, "legacy collection children must be a plain list")
  }
  sets = .upgrade_paradox_copy_list(sets)
  set_names = names(sets)
  if (is.null(set_names)) set_names = rep("", length(sets))
  if (length(set_names) != length(sets) || anyNA(set_names) ||
      anyDuplicated(set_names[nzchar(set_names)])) {
    .upgrade_paradox_abort(path, "legacy collection names are malformed")
  }
  names(sets) = set_names

  params = .upgrade_paradox_binding(private, ".params", path)
  param_columns = .upgrade_paradox_params(params, paste0(path, "$private$.params"))
  ids = param_columns$id
  values = .upgrade_paradox_binding(private, ".values", path)
  if (!is.list(values) || length(values)) {
    .upgrade_paradox_abort(path, "legacy collection contains noncanonical local values")
  }
  extra_trafo = .upgrade_paradox_binding(private, ".extra_trafo", path)
  constraint = .upgrade_paradox_binding(private, ".constraint", path)
  if (!is.null(extra_trafo) || !is.null(constraint)) {
    .upgrade_paradox_abort(path, "legacy collection contains noncanonical local callbacks")
  }

  translation = .upgrade_paradox_binding(private, ".translation", path)
  translation_columns = .upgrade_paradox_table(
    translation,
    c("id", "original_id", "owner_ps_index", "owner_name"),
    paste0(path, "$private$.translation")
  )
  if (!is.character(translation_columns$id) ||
      !is.character(translation_columns$original_id) ||
      !is.integer(translation_columns$owner_ps_index) ||
      !is.character(translation_columns$owner_name) ||
      anyNA(unlist(translation_columns, recursive = FALSE)) ||
      any(translation_columns$owner_ps_index < 1L) ||
      any(translation_columns$owner_ps_index > length(sets))) {
    .upgrade_paradox_abort(path, "legacy collection translation is malformed")
  }

  postfix = if (exists(".postfix", envir = private, inherits = FALSE)) {
    .upgrade_paradox_binding(private, ".postfix", path)
  } else {
    FALSE
  }
  if (!is.logical(postfix) || length(postfix) != 1L || is.na(postfix)) {
    .upgrade_paradox_abort(path, "legacy postfix rule is malformed")
  }

  list(
    kind = "collection",
    children = sets,
    param_columns = param_columns,
    ids = ids,
    tags = .upgrade_paradox_tags(
      .upgrade_paradox_binding(private, ".tags", path), ids,
      paste0(path, "$private$.tags")
    ),
    trafos = .upgrade_paradox_trafos(
      .upgrade_paradox_binding(private, ".trafos", path), ids,
      paste0(path, "$private$.trafos")
    ),
    deps = .upgrade_paradox_deps(
      .upgrade_paradox_binding(private, ".deps", path), ids,
      paste0(path, "$private$.deps")
    ),
    translation_columns = translation_columns,
    postfix = postfix,
    assert_values = .upgrade_paradox_binding(x, "assert_values", path)
  )
}

.upgrade_paradox_node_info = function(x, path) {
  shell = .upgrade_paradox_shell(x, path)
  current = .upgrade_paradox_current(x, shell, path)
  if (!is.null(current)) return(list(kind = "current", value = current))
  classes = .upgrade_paradox_materialize_atomic(
    attr(x, "class", exact = TRUE)
  )
  if (identical(classes, c("ParamSet", "R6"))) {
    .upgrade_paradox_base_info(x, shell, path)
  } else if (identical(classes, c("ParamSetCollection", "ParamSet", "R6"))) {
    .upgrade_paradox_collection_info(x, shell, path)
  } else {
    .upgrade_paradox_owner_info(x, shell, path, classes)
  }
}

.upgrade_paradox_domain_from_row = function(info, index, path) {
  columns = lapply(info$param_columns, function(column) {
    if (is.list(column)) list(column[[index]]) else column[index]
  })
  columns$.tags = list(info$tags_by_id[[index]])
  columns$.trafo = list(info$trafo_by_id[[index]])
  columns$.requirements = list(list())
  columns$.init_given = FALSE
  columns$.init = list(NULL)
  columns = columns[domain_names]
  result = .upgrade_paradox_domain_table(columns)
  class(result) = c(
    info$param_columns$cls[[index]], "Domain", "data.table", "data.frame"
  )
  .upgrade_paradox_domain(result, path)
}

.upgrade_paradox_build_base = function(info, path) {
  domains = lapply(seq_along(info$ids), function(index) {
    .upgrade_paradox_domain_from_row(
      info,
      index,
      sprintf("%s$domains[[%d]]", path, index)
    )
  })
  names(domains) = info$ids
  result = tryCatch(
    ParamSet$new(domains, allow_dangling_dependencies = TRUE),
    error = function(error) {
      .upgrade_paradox_abort(
        path,
        "malformed legacy BASE schema (%s)",
        conditionMessage(error)
      )
    }
  )
  private = result$.__enclos_env__$private
  param_set_core_replace(
    private,
    values = info$values,
    tags = info$tags,
    deps = info$deps,
    trafos = info$trafos,
    extra_trafo = info$extra_trafo,
    constraint = info$constraint
  )
  result$assert_values = info$assert_values
  result
}

.upgrade_paradox_columns_identical = function(expected, actual) {
  identical(names(expected), names(actual)) &&
    all(vapply(
      names(expected),
      function(name) identical(expected[[name]], .subset2(actual, name)),
      logical(1L)
    ))
}

.upgrade_paradox_build_collection = function(info, children, path) {
  result = tryCatch(
    ParamSetCollection$new(children, postfix_names = info$postfix),
    error = function(error) {
      .upgrade_paradox_abort(
        path,
        "malformed legacy COLLECTION graph (%s)",
        conditionMessage(error)
      )
    }
  )
  private = result$.__enclos_env__$private
  state = param_set_core_state(private)
  if (!.upgrade_paradox_columns_identical(info$param_columns, state$.params) ||
      !.upgrade_paradox_columns_identical(
        info$translation_columns,
        state$.translation
      )) {
    .upgrade_paradox_abort(
      path,
      "legacy collection caches disagree with the child graph"
    )
  }
  param_set_core_replace(
    private,
    tags = info$tags,
    deps = info$deps,
    trafos = info$trafos
  )
  result$assert_values = info$assert_values
  result
}

.upgrade_paradox_graph = function(x) {
  session = .upgrade_paradox_prepare_session(list(x), "x")
  session$prepared[[1L]]
}

.upgrade_paradox_info_dependencies = function(info) {
  if (identical(info$kind, "collection")) return(info$children)
  if (identical(info$kind, "owner")) return(info$dependencies)
  list()
}

.upgrade_paradox_build_owner = function(info, dependencies, path) {
  entry = info$owner
  base = if (identical(entry$migration_kind, "additive")) {
    .upgrade_paradox_build_base(info$base, path)
  } else {
    NULL
  }
  rebuilder = .paradox_object_upgrader_resolve(entry, "rebuilder")
  result = tryCatch(
    rebuilder(base, info$owner_state, dependencies),
    error = function(error) {
      .upgrade_paradox_abort(
        path,
        "owner upgrader `%s::%s()` failed (%s)",
        entry$owner_package,
        entry$rebuilder,
        conditionMessage(error)
      )
    }
  )
  expected_class = entry$legacy_class
  observed_class = .upgrade_paradox_materialize_atomic(
    attr(result, "class", exact = TRUE)
  )
  if (!is.environment(result) || !identical(observed_class, expected_class)) {
    .upgrade_paradox_abort(
      path,
      "owner upgrader `%s::%s()` returned class `%s`, expected `%s`",
      entry$owner_package,
      entry$rebuilder,
      paste(observed_class, collapse = "/"),
      paste(expected_class, collapse = "/")
    )
  }
  result_shell = .upgrade_paradox_shell(result, path)
  .upgrade_paradox_reject_owner_finalizer(
    result,
    result_shell,
    path,
    qualifier = "rebuilt"
  )

  if (identical(entry$migration_kind, "additive")) {
    base_private = .upgrade_paradox_shell(base, path)$private
    result_private = result_shell$private
    result_private$.core = .upgrade_paradox_binding(base_private, ".core", path)
  } else {
    result_private = result_shell$private
    result_core = .upgrade_paradox_binding(
      result_private,
      ".core",
      path
    )
    result_kind = tryCatch(
      .Call(C_param_set_core_kind, result_core),
      error = function(error) {
        .upgrade_paradox_abort(
          path,
          "replacement owner returned a corrupt current capsule (%s)",
          conditionMessage(error)
        )
      }
    )
    if (!identical(result_kind, 3L)) {
      .upgrade_paradox_abort(
        path,
        "replacement owner must return a current ParamSetShadow shell"
      )
    }
  }
  result$assert_values = info$assert_values
  .upgrade_paradox_validate_current_graph(result, path)
  result
}

.upgrade_paradox_build_prepared = function(info, dependencies, path) {
  switch(
    info$kind,
    current = info$value,
    base = .upgrade_paradox_build_base(info, path),
    collection = {
      names(dependencies) = names(info$children)
      .upgrade_paradox_build_collection(info, dependencies, path)
    },
    owner = {
      names(dependencies) = names(info$dependencies)
      .upgrade_paradox_build_owner(info, dependencies, path)
    },
    .upgrade_paradox_abort(path, "internal unknown migration node kind")
  )
}

# Build every replacement before touching a serialized shell. The session map
# is deliberately shared by all roots returned by the native host-graph
# crawler, so aliases and collection/shadow edges are rebuilt exactly once.
.upgrade_paradox_prepare_session = function(candidates, paths) {
  if (!is.list(candidates) || !is.character(paths) ||
      length(candidates) != length(paths)) {
    stop("Internal error: malformed Paradox migration candidate set", call. = FALSE)
  }

  nodes = list()
  infos = list()
  node_paths = character()
  status = integer()
  prepared = list()
  commit_order = integer()

  find_node = function(node) {
    for (index in seq_along(nodes)) {
      if (identical(nodes[[index]], node)) return(index)
    }
    0L
  }
  add_node = function(node, path) {
    index = find_node(node)
    if (index) return(index)
    index = length(nodes) + 1L
    nodes[[index]] <<- node
    infos[[index]] <<- .upgrade_paradox_node_info(node, path)
    node_paths[[index]] <<- path
    status[[index]] <<- 0L
    prepared[index] <<- list(NULL)
    index
  }

  for (root_position in seq_along(candidates)) {
    root = add_node(candidates[[root_position]], paths[[root_position]])
    if (status[[root]] == 2L) next
    status[[root]] = 1L
    frames = list(list(index = root, next_dependency = 0L))

    while (length(frames)) {
      frame_position = length(frames)
      frame = frames[[frame_position]]
      index = frame$index
      info = infos[[index]]
      dependencies = .upgrade_paradox_info_dependencies(info)

      if (frame$next_dependency < length(dependencies)) {
        dependency_position = frame$next_dependency + 1L
        frames[[frame_position]]$next_dependency = dependency_position
        dependency_name = names(dependencies)[dependency_position]
        if (is.na(dependency_name) || !nzchar(dependency_name)) {
          dependency_name = as.character(dependency_position)
        }
        dependency_path = if (identical(info$kind, "collection")) {
          sprintf(
            "%s$sets[[%d]]",
            node_paths[[index]],
            dependency_position
          )
        } else {
          sprintf(
            "%s$owner_dependencies[[%s]]",
            node_paths[[index]],
            dependency_name
          )
        }
        dependency_index = add_node(
          dependencies[[dependency_position]],
          dependency_path
        )
        if (status[[dependency_index]] == 1L) {
          .upgrade_paradox_abort(
            dependency_path,
            "cycle reaches active node at %s",
            node_paths[[dependency_index]]
          )
        }
        if (status[[dependency_index]] == 0L) {
          status[[dependency_index]] = 1L
          frames[[length(frames) + 1L]] = list(
            index = dependency_index,
            next_dependency = 0L
          )
        }
        next
      }

      dependency_indices = vapply(dependencies, find_node, integer(1L))
      if (length(dependency_indices) &&
          (any(!dependency_indices) || any(status[dependency_indices] != 2L))) {
        .upgrade_paradox_abort(
          node_paths[[index]],
          "internal graph traversal failure"
        )
      }
      dependency_values = prepared[dependency_indices]
      prepared[[index]] = .upgrade_paradox_build_prepared(
        info,
        dependency_values,
        node_paths[[index]]
      )
      if (!identical(info$kind, "current")) {
        commit_order = c(commit_order, index)
      }
      status[[index]] = 2L
      frames[[frame_position]] = NULL
    }
  }

  list(
    nodes = nodes,
    infos = infos,
    paths = node_paths,
    prepared = prepared,
    commit_order = commit_order
  )
}

.upgrade_paradox_enclosure_slices = function(x, path) {
  shell = .upgrade_paradox_shell(x, path)
  result = list(shell$enclosing)
  enclosing = shell$enclosing
  repeat {
    super = get0("super", envir = enclosing, inherits = FALSE)
    if (!is.environment(super) ||
        !exists(".__enclos_env__", envir = super, inherits = FALSE) ||
        bindingIsActive(".__enclos_env__", super)) {
      break
    }
    enclosing = get(".__enclos_env__", envir = super, inherits = FALSE)
    if (!is.environment(enclosing) ||
        any(vapply(result, identical, logical(1L), y = enclosing))) {
      .upgrade_paradox_abort(path, "malformed current R6 superclass enclosure")
    }
    result[[length(result) + 1L]] = enclosing
  }
  result
}

.upgrade_paradox_binding_shape = function(owner, names) {
  data.frame(
    name = names,
    active = vapply(names, bindingIsActive, logical(1L), env = owner),
    locked = vapply(names, bindingIsLocked, logical(1L), env = owner),
    stringsAsFactors = FALSE
  )
}

.upgrade_paradox_transplant_plan = function(
    legacy,
    current,
    path,
    retired_bindings = character(),
    owner_package = "paradox") {
  legacy_names = ls(legacy, all.names = TRUE)
  current_names = ls(current, all.names = TRUE)
  retired_bindings = unique(as.character(retired_bindings))
  collision = intersect(retired_bindings, current_names)
  if (length(collision)) {
    .upgrade_paradox_abort(
      path,
      "retired owner binding `%s` collides with the current shell",
      collision[[1L]]
    )
  }
  missing = setdiff(current_names, legacy_names)
  unexpected = setdiff(legacy_names, c(current_names, retired_bindings))
  absent_retired = setdiff(retired_bindings, legacy_names)
  if (length(missing)) {
    .upgrade_paradox_abort(
      path,
      "legacy shell cannot receive current binding `%s`",
      missing[[1L]]
    )
  }
  if (length(unexpected)) {
    .upgrade_paradox_abort(
      path,
      "legacy shell has undeclared owner binding `%s`",
      unexpected[[1L]]
    )
  }
  if (length(absent_retired)) {
    .upgrade_paradox_abort(
      path,
      "declared retired binding `%s` is absent",
      absent_retired[[1L]]
    )
  }
  nonactive_retired = retired_bindings[
    !vapply(retired_bindings, bindingIsActive, logical(1L), env = legacy)
  ]
  if (length(nonactive_retired)) {
    .upgrade_paradox_abort(
      path,
      "declared retired binding `%s` is not an active field",
      nonactive_retired[[1L]]
    )
  }

  legacy_shape = .upgrade_paradox_binding_shape(legacy, current_names)
  current_shape = .upgrade_paradox_binding_shape(current, current_names)
  shape_mismatch = legacy_shape$active != current_shape$active
  # `.__enclos_env__` and `assert_values` are ordinary mutable fields. Every
  # method's lock bit is refreshed from the current shell during commit.
  if (any(shape_mismatch)) {
    .upgrade_paradox_abort(
      path,
      "legacy/current binding kind differs for `%s`",
      current_names[which(shape_mismatch)[[1L]]]
    )
  }

  legacy_class = .upgrade_paradox_materialize_atomic(
    attr(legacy, "class", exact = TRUE)
  )
  current_class = .upgrade_paradox_materialize_atomic(
    attr(current, "class", exact = TRUE)
  )
  if (!identical(legacy_class, current_class)) {
    .upgrade_paradox_abort(
      path,
      "identity-preserving migration cannot change the shell class"
    )
  }
  enclosure_position = match(".__enclos_env__", current_names, nomatch = 0L)
  if (!enclosure_position ||
      sum(current_names == ".__enclos_env__") != 1L ||
      current_shape$active[[enclosure_position]]) {
    .upgrade_paradox_abort(path, "current shell has no ordinary R6 enclosure")
  }

  enclosures = .upgrade_paradox_enclosure_slices(current, path)
  retired_values = lapply(
    retired_bindings,
    .upgrade_paradox_retired_binding,
    owner_package = owner_package
  )
  active_metadata = NULL
  active_metadata_locked = FALSE
  if (length(retired_bindings)) {
    top_enclosure = enclosures[[1L]]
    if (!exists(".__active__", envir = top_enclosure, inherits = FALSE) ||
        bindingIsActive(".__active__", top_enclosure)) {
      .upgrade_paradox_abort(
        path,
        "current owner shell has malformed R6 active-binding metadata"
      )
    }
    active_metadata = get(
      ".__active__",
      envir = top_enclosure,
      inherits = FALSE
    )
    if (typeof(active_metadata) != "list" ||
        is.null(names(active_metadata)) ||
        anyDuplicated(names(active_metadata))) {
      .upgrade_paradox_abort(
        path,
        "current owner shell has malformed R6 active-binding metadata"
      )
    }
    active_metadata[retired_bindings] = retired_values
    active_metadata_locked = bindingIsLocked(
      ".__active__",
      top_enclosure
    )
  }

  list(
    legacy = legacy,
    current = current,
    path = path,
    current_class = current_class,
    current_names = current_names,
    current_shape = current_shape,
    enclosure_position = enclosure_position,
    current_values = lapply(seq_along(current_names), function(position) {
      name = current_names[[position]]
      if (current_shape$active[[position]]) {
        activeBindingFunction(name, current)
      } else {
        get(name, envir = current, inherits = FALSE)
      }
    }),
    retired_bindings = retired_bindings,
    retired_values = retired_values,
    enclosures = enclosures,
    active_metadata = active_metadata,
    active_metadata_locked = active_metadata_locked
  )
}

.upgrade_paradox_replace_binding = function(
    owner,
    name,
    value,
    active,
    locked) {
  if (bindingIsLocked(name, owner)) {
    # R CMD check treats every syntactic unlockBinding(name, owner) call as
    # possible foreign-namespace tampering because it cannot infer that owner
    # is an authenticated R6 shell or enclosure. Resolve the base primitive
    # explicitly on this cold migration path; this does not broaden which
    # environment can reach transplant planning.
    get("unlockBinding", envir = baseenv(), inherits = FALSE)(name, owner)
  }
  if (active) {
    makeActiveBinding(name, value, owner)
  } else {
    assign(name, value, envir = owner)
  }
  if (locked) lockBinding(name, owner)
  invisible(NULL)
}

.paradox_upgrade_retired_token = new.env(parent = emptyenv())

.upgrade_paradox_retired_binding = function(name, owner_package) {
  force(name)
  force(owner_package)
  token = .paradox_upgrade_retired_token
  force(token)
  function(value) {
    # Retain a per-session provenance token in this closure for interrupted
    # transplant authentication. Do not compare it while serving the retired
    # API diagnostic: ordinary serialization legitimately copies the closure
    # environment, and a fully upgraded roundtrip must remain usable.
    invisible(token)
    stop(
      sprintf(
        "`$%s` belonged to the Paradox 1 implementation in package '%s' and was retired in Paradox 2",
        name,
        owner_package
      ),
      call. = FALSE
    )
  }
}

.upgrade_paradox_is_retired_binding = function(
    binding,
    name,
    owner_package) {
  if (!is.function(binding) || !is.environment(environment(binding))) {
    return(FALSE)
  }
  enclosing = environment(binding)
  token = .upgrade_paradox_inert_binding_value(enclosing, "token")
  stored_name = .upgrade_paradox_inert_binding_value(enclosing, "name")
  stored_owner = .upgrade_paradox_inert_binding_value(
    enclosing,
    "owner_package"
  )
  token$ok && stored_name$ok && stored_owner$ok &&
    identical(token$value, .paradox_upgrade_retired_token) &&
    identical(stored_name$value, name) &&
    identical(stored_owner$value, owner_package)
}

.upgrade_paradox_transplant = function(plan) {
  legacy = plan$legacy

  for (enclosing in plan$enclosures) {
    assign("self", legacy, envir = enclosing)
  }
  if (length(plan$retired_bindings)) {
    .upgrade_paradox_replace_binding(
      plan$enclosures[[1L]],
      ".__active__",
      plan$active_metadata,
      FALSE,
      plan$active_metadata_locked
    )
  }
  ordinary_positions = setdiff(
    seq_along(plan$current_names),
    plan$enclosure_position
  )
  for (position in ordinary_positions) {
    name = plan$current_names[[position]]
    active = plan$current_shape$active[[position]]
    .upgrade_paradox_replace_binding(
      legacy,
      name,
      plan$current_values[[position]],
      active,
      plan$current_shape$locked[[position]]
    )
  }
  for (position in seq_along(plan$retired_bindings)) {
    name = plan$retired_bindings[[position]]
    .upgrade_paradox_replace_binding(
      legacy,
      name,
      plan$retired_values[[position]],
      TRUE,
      FALSE
    )
  }

  # This is the one completion point. Before it, the old enclosure/private
  # state remains authoritative and mixed refreshed methods are explicitly
  # re-authenticated on retry. After it, every public and retired binding and
  # every current enclosure slice is already installed.
  position = plan$enclosure_position
  .upgrade_paradox_replace_binding(
    legacy,
    plan$current_names[[position]],
    plan$current_values[[position]],
    FALSE,
    plan$current_shape$locked[[position]]
  )
  invisible(legacy)
}

.upgrade_paradox_rebase_prepared = function(info, prepared, originals, path) {
  if (identical(info$kind, "collection")) {
    private = .upgrade_paradox_shell(prepared, path)$private
    param_set_core_replace(private, sets = originals)
  } else if (identical(info$kind, "owner") &&
      identical(info$owner$migration_kind, "replacement") &&
      length(originals)) {
    private = .upgrade_paradox_shell(prepared, path)$private
    if (length(originals) != 1L ||
        !identical(names(originals), "origin")) {
      .upgrade_paradox_abort(
        path,
        "replacement owner requires exactly one dependency named `origin`"
      )
    }
    # The replacement result and dependency envelope were authenticated
    # during preflight. This defensive capsule-kind check protects internal
    # callers without leaving a predictable owner-shape failure until commit.
    template = .upgrade_paradox_binding(private, ".core", path)
    kind = .Call(C_param_set_core_kind, template)
    if (!identical(kind, 3L)) {
      .upgrade_paradox_abort(
        path,
        "replacement owner must return a current ParamSetShadow shell"
      )
    }
    # The shadow constructor authenticates that the template's stored origin
    # is the same shell it is asked to derive from. Repoint only this offside
    # prepared capsule before rebuilding its native signature.
    param_set_core_replace(private, sets = unname(originals))
    template = .upgrade_paradox_binding(private, ".core", path)
    private$.core = .Call(
      C_param_set_shadow_core_new,
      template,
      originals[[1L]]
    )
  }
  .upgrade_paradox_validate_current_graph(prepared, path)
  invisible(NULL)
}

.upgrade_paradox_commit_session = function(session) {
  plans = vector("list", length(session$commit_order))
  for (position in seq_along(session$commit_order)) {
    index = session$commit_order[[position]]
    info = session$infos[[index]]
    retired = if (identical(info$kind, "owner")) {
      info$owner$retired_bindings
    } else {
      character()
    }
    owner_package = if (identical(info$kind, "owner")) {
      info$owner$owner_package
    } else {
      "paradox"
    }
    plans[[position]] = .upgrade_paradox_transplant_plan(
      session$nodes[[index]],
      session$prepared[[index]],
      session$paths[[index]],
      retired,
      owner_package
    )
  }

  # Commit is monotonic and post-order. All semantic work and every shell
  # shape audit above completed before this point. If an allocation failure or
  # interrupt occurs, every already-committed shell is independently valid and
  # retrying the graph upgrade completes the remaining nodes.
  for (position in seq_along(session$commit_order)) {
    index = session$commit_order[[position]]
    info = session$infos[[index]]
    dependencies = .upgrade_paradox_info_dependencies(info)
    dependency_indices = vapply(dependencies, function(dependency) {
      for (candidate in seq_along(session$nodes)) {
        if (identical(session$nodes[[candidate]], dependency)) return(candidate)
      }
      0L
    }, integer(1L))
    originals = session$nodes[dependency_indices]
    names(originals) = names(dependencies)
    .upgrade_paradox_rebase_prepared(
      info,
      session$prepared[[index]],
      originals,
      session$paths[[index]]
    )
    base::suspendInterrupts(
      .upgrade_paradox_transplant(plans[[position]])
    )
    .upgrade_paradox_validate_current_graph(
      session$nodes[[index]],
      session$paths[[index]]
    )
  }
  invisible(NULL)
}

#' Upgrade every legacy ParamSet in an object graph
#'
#' `upgrade_paradox_object_graph()` finds Paradox 1 `ParamSet`-family R6
#' objects nested in `x` and upgrades their existing R6 shells in place. This
#' preserves aliases held by other R6 objects, locked fields, closures,
#' attributes, collections, and opaque current Paradox payload values. The
#' function returns `x` invisibly; it does not replace the containing objects.
#'
#' Discovery is iterative and identity-aware. It inspects ordinary environment
#' bindings, active-binding functions (never their values), closures and safe
#' enclosing environments, unforced binding and `...` promises without forcing
#' them, language objects, S4 attributes, and ordinary containers. R 4.3--4.5
#' can also inspect a detached promise; strict R 4.6 and newer expose no safe
#' API for that edge, so a promise reached outside a binding or `...` cell is
#' opaque. It does not enter
#' `.GlobalEnv`, package namespaces, attached package/import environments, the
#' search path, base/empty environments, or arbitrary external-pointer and
#' weak-reference internals. The protected ordinary-R payload of an
#' authenticated current Paradox capsule is traversed so a legacy object stored
#' as an opaque parameter value is not missed.
#'
#' All discovered nodes and registered owner migrations are semantically
#' inspected and rebuilt before the first legacy shell changes. Commit is
#' post-order and identity-preserving. The enclosure swap is each shell's final
#' completion point. If an allocation failure interrupts an individual binding
#' wave, the old enclosure remains authoritative and the mixed shell stays
#' authenticated for retry; completed nodes are valid current objects. Calling
#' the function again completes the remainder.
#'
#' The default first use of an unupgraded Paradox 1 method gives an actionable
#' error. Set `options(paradox.legacy_object_action = "upgrade")` to invoke
#' this graph migration silently on first use. Explicit calls to this function
#' do not consult that option.
#'
#' A legacy owner subclass can be migrated only while the package that
#' registered its exact bridge is loaded. Normally loading that package runs
#' the required registration from its `.onLoad()` hook.
#'
#' This recursive API migrates ParamSet-family shells only. Use
#' [upgrade_paradox_object()] for a standalone built-in legacy `Domain` or
#' `Condition`, or when a new, non-mutating top-level ParamSet object is
#' preferable.
#'
#' @param x Any R object that may contain legacy Paradox ParamSet-family
#'   objects.
#'
#' @return `x`, invisibly. Legacy ParamSet-family environments reachable under
#'   the documented traversal boundary are mutated in place.
#' @export
#' @family legacy migration
#' @examples
#' current = list(search_space = ps(x = p_dbl(0, 1)))
#' identical(upgrade_paradox_object_graph(current), current)
#' \dontrun{
#' model = readRDS("model-containing-paradox-1-objects.rds")
#' upgrade_paradox_object_graph(model)
#' }
upgrade_paradox_object_graph = function(x) {
  discovery = .Call(C_upgrade_graph_discover, x)
  if (!is.list(discovery) ||
      !identical(names(discovery), c("objects", "paths")) ||
      !is.list(discovery$objects) ||
      !is.character(discovery$paths) ||
      length(discovery$objects) != length(discovery$paths)) {
    stop("Internal error: malformed native Paradox graph discovery", call. = FALSE)
  }
  if (length(discovery$objects)) {
    session = .upgrade_paradox_prepare_session(
      discovery$objects,
      discovery$paths
    )
    .upgrade_paradox_commit_session(session)
  }
  invisible(x)
}

.paradox_upgrade_legacy_first_use = function(self) {
  upgrade_paradox_object_graph(self)
  if (!.paradox_gateway_current_core(self)) {
    stop(
      "Legacy Paradox first-use migration did not produce a current ParamSet shell.",
      call. = FALSE
    )
  }
  invisible(self)
}

#' Upgrade a serialized Paradox object explicitly
#'
#' Paradox 2 does not execute objects serialized with the Paradox 1 private
#' layout. `upgrade_paradox_object()` validates a canonical built-in legacy
#' object and returns a newly constructed object using the current versioned
#' state capsule. Current capsule-backed parameter sets are returned unchanged.
#'
#' The upgrader accepts package-owned `ParamSet`, `ParamSetCollection`, built-in
#' `Domain`, and built-in `Condition` objects. It preserves callbacks and shared
#' collection children without calling legacy methods or active bindings. The
#' input is never mutated. Cycles, malformed private state, replaced core R6
#' methods, custom Domain or Condition classes, and unregistered legacy
#' third-party R6 subclasses are rejected with a path-specific error. A current
#' capsule-backed additive subclass is validated and returned unchanged.
#' Direct owner subclasses covered by an exact
#' [register_paradox_object_upgrader()] bridge can also be rebuilt without
#' mutating the input; the recursive graph API uses the same bridge while
#' preserving the original shell identity.
#'
#' @param x A current or legacy Paradox object.
#'
#' @return A current Paradox object. For a current capsule-backed `ParamSet`,
#'   `ParamSetCollection`, or `ParamSetShadow`, this is `x` itself.
#' @export
#' @family legacy migration
#' @examples
#' current = ps(x = p_dbl(0, 1))
#' identical(upgrade_paradox_object(current), current)
#' \dontrun{
#' legacy = readRDS("legacy-paradox-object.rds")
#' current = upgrade_paradox_object(legacy)
#' # Only operate on `current`.
#' }
upgrade_paradox_object = function(x) {
  classes = .upgrade_paradox_materialize_atomic(
    attr(x, "class", exact = TRUE)
  )
  if (is.environment(x) && inherits(x, "ParamSet")) {
    return(.upgrade_paradox_graph(x))
  }
  if ("Domain" %in% classes) {
    return(.upgrade_paradox_domain(x, "x"))
  }
  if ("Condition" %in% classes) {
    return(.upgrade_paradox_condition(x, "x", allow_base = TRUE))
  }
  .upgrade_paradox_abort(
    "x",
    "unsupported object class `%s`",
    paste(classes, collapse = "/")
  )
}
