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
    is.function(method) && any(vapply(
      method_enclosures,
      identical,
      logical(1L),
      y = environment(method)
    ))
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
    } else if (!owns_method(value) ||
        !bindingIsLocked(name, x)) {
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

.upgrade_paradox_base_info = function(x, shell, path) {
  .upgrade_paradox_authenticate_legacy_shell(x, shell, "base", path)
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
    .upgrade_paradox_abort(
      path,
      "legacy third-party subclasses are unsupported (class is %s)",
      paste(classes, collapse = "/")
    )
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
  nodes = list()
  infos = list()
  paths = character()
  status = integer()
  rebuilt = list()

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
    paths[[index]] <<- path
    status[[index]] <<- 0L
    rebuilt[index] <<- list(NULL)
    index
  }

  root = add_node(x, "x")
  status[[root]] = 1L
  frames = list(list(index = root, next_child = 0L))
  while (length(frames)) {
    frame_index = length(frames)
    frame = frames[[frame_index]]
    index = frame$index
    info = infos[[index]]

    if (identical(info$kind, "current")) {
      rebuilt[[index]] = info$value
      status[[index]] = 2L
      frames[[frame_index]] = NULL
      next
    }
    if (identical(info$kind, "base")) {
      rebuilt[[index]] = .upgrade_paradox_build_base(info, paths[[index]])
      status[[index]] = 2L
      frames[[frame_index]] = NULL
      next
    }

    if (frame$next_child < length(info$children)) {
      child_position = frame$next_child + 1L
      frames[[frame_index]]$next_child = child_position
      child_path = sprintf(
        "%s$sets[[%d]]",
        paths[[index]],
        child_position
      )
      child_index = add_node(info$children[[child_position]], child_path)
      if (status[[child_index]] == 1L) {
        .upgrade_paradox_abort(
          child_path,
          "cycle reaches active node at %s",
          paths[[child_index]]
        )
      }
      if (status[[child_index]] == 0L) {
        status[[child_index]] = 1L
        frames[[length(frames) + 1L]] = list(
          index = child_index,
          next_child = 0L
        )
      }
      next
    }

    child_indices = vapply(info$children, find_node, integer(1L))
    if (any(!child_indices) || any(status[child_indices] != 2L)) {
      .upgrade_paradox_abort(paths[[index]], "internal graph traversal failure")
    }
    children = rebuilt[child_indices]
    names(children) = names(info$children)
    rebuilt[[index]] = .upgrade_paradox_build_collection(
      info,
      children,
      paths[[index]]
    )
    status[[index]] = 2L
    frames[[frame_index]] = NULL
  }
  rebuilt[[root]]
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
#' methods, custom Domain or Condition classes, and legacy third-party R6
#' subclasses are rejected with a path-specific error. A current capsule-backed
#' additive subclass is validated and returned unchanged. Legacy miesmuschel
#' shadows must be reconstructed by miesmuschel around an explicitly upgraded
#' origin.
#'
#' @param x A current or legacy Paradox object.
#'
#' @return A current Paradox object. For a current capsule-backed `ParamSet`,
#'   `ParamSetCollection`, or `ParamSetShadow`, this is `x` itself.
#' @export
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
