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

.upgrade_paradox_active_binding_accessor = function() {
  get0(
    "activeBindingFunction",
    envir = baseenv(),
    inherits = FALSE,
    mode = "function"
  )
}

.upgrade_paradox_active_binding_function = function(name, owner, path) {
  accessor = .upgrade_paradox_active_binding_accessor()
  if (is.null(accessor)) {
    .upgrade_paradox_abort(
      path,
      paste0(
        "safe active-binding inspection is unavailable on this R version; ",
        "legacy ParamSet migration requires R >= 4.0.0"
      )
    )
  }
  accessor(name, owner)
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

  snapshot = .paradox_plain_binding_snapshot(owner, name)
  if (!isTRUE(snapshot$ok)) {
    .upgrade_paradox_abort(path, "binding `%s` is delayed or malformed", name)
  }
  snapshot$value
}

.upgrade_paradox_assert_values = function(value, path) {
  if (!identical(.Call(C_param_set_assert_values_exact, value), TRUE)) {
    .upgrade_paradox_abort(path, "`assert_values` is malformed")
  }
  value
}

.upgrade_paradox_shell = function(x, path) {
  if (!is.environment(x)) {
    .upgrade_paradox_abort(path, "expected a ParamSet R6 object")
  }
  class_kind = .Call(C_param_set_class_kind, x)
  if (!(class_kind %in% 1:3)) {
    .upgrade_paradox_abort(
      path,
      "expected a well-formed ordinary ParamSet-family R6 class suffix"
    )
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
  list(enclosing = enclosing, private = private, class_kind = class_kind)
}

# Keep current-schema admission behind one helper. The read-only native
# validator walks BASE, COLLECTION, and SHADOW graphs without refreshing a
# stale Shadow or running constraints, utility checks, or transformations. It
# validates all ten capsule fields, callback shapes, child translations,
# sharing, and active-path cycles. This is deliberately separate from legacy
# reconstruction: current-state admission and legacy conversion have different
# contracts and neither is a fallback for the other.
.upgrade_paradox_validate_current_graph = function(
    x,
    path,
    selected_core = NULL,
    selected_private = NULL
) {
  if (is.null(selected_private)) {
    selected_private = .upgrade_paradox_shell(x, path)$private
  }
  if (is.null(selected_core)) {
    selected_core = .upgrade_paradox_binding(selected_private, ".core", path)
  }
  result = tryCatch(
    .Call(
      C_param_set_validate_current_graph,
      selected_private,
      x,
      selected_core
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

.upgrade_paradox_validate_current_roots = function(
    roots,
    failure = "Cannot commit Paradox migration"
) {
  result = tryCatch(
    .Call(C_param_set_validate_current_roots, roots, NULL),
    error = function(error) {
      stop(
        sprintf(
          "%s: prepared/current roots failed joint validation (%s)",
          failure,
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  if (!identical(result, TRUE)) {
    stop(
      sprintf("%s: joint current-root validation failed", failure),
      call. = FALSE
    )
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
  if (!identical(kind, shell$class_kind)) {
    .upgrade_paradox_abort(path, "state capsule kind disagrees with shell class")
  }
  .upgrade_paradox_validate_current_graph(x, path, core, private)
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
  if (!identical(namespace, asNamespace("paradox"))) {
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
      binding = .upgrade_paradox_active_binding_function(name, x, path)
      if (!owns_method(binding)) {
        .upgrade_paradox_abort(path, "core binding `%s` was replaced", name)
      }
      next
    }

    value = .upgrade_paradox_binding(x, name, path)
    if (identical(name, "assert_values")) {
      .upgrade_paradox_assert_values(value, path)
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

.upgrade_paradox_class_snapshot = function(x, path) {
  classes = .Call(C_upgrade_class_snapshot, x)
  if (identical(classes, FALSE)) {
    .upgrade_paradox_abort(path, "class metadata must be an ordinary vector")
  }
  classes
}

.upgrade_paradox_has_only_attributes = function(x, allowed) {
  observed = attributes(x)
  if (is.null(observed)) return(TRUE)
  observed_names = names(observed)
  !is.null(observed_names) &&
    !anyDuplicated(observed_names) &&
    all(observed_names %in% allowed)
}

.upgrade_paradox_is_ordinary_list = function(x) {
  identical(.Call(C_upgrade_structural_list_exact, x), TRUE)
}

# Base R answers `names<-` on a referenced list of at least 64 elements with a
# wrapper ALTREP shell (R >= 4.3). Every native structural gate, and the
# migration crawler in particular, rejects a structural list ALTREP before
# observing it, so package-owned and package-received list carriers are
# materialized in R. `.subset()` carries the names attribute and never
# dispatches an S3 `[` method; the remaining attributes are reinstalled one at a
# time because `attributes<-` re-wraps a list of this size. Element identities
# are unchanged, which is what every caller of this helper depends on.
.paradox_materialize_list_carrier = function(x) {
  copy = .subset(x, seq_along(x))
  stored = attributes(x)
  for (name in names(stored)) {
    if (identical(name, "names")) next
    attr(copy, name) = stored[[name]]
  }
  copy
}

# The recursive entry point is the one place that receives a caller-owned
# container of unbounded length, so it is the one place where an ordinary
# `names<-` can have produced a structural wrapper the crawler must not
# observe. The container itself is a carrier: discovery keys on the identity of
# the objects it holds, mutates those objects in place, and returns the
# caller's original argument, so replacing the carrier is invisible. Nested
# containers keep the native rejection; materializing them would mean invoking
# unknown ALTREP methods from inside the crawl. The standalone upgrader needs
# no such step: it admits only fixed-shape two- and sixteen-element Condition
# and Domain carriers, which are below R's wrapper threshold.
.upgrade_paradox_graph_root = function(x) {
  if (typeof(x) != "list" || isS4(x) ||
      .upgrade_paradox_is_ordinary_list(x)) {
    return(x)
  }
  .paradox_materialize_list_carrier(x)
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
  .paradox_plain_binding_snapshot(owner, name)
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

# Scope: the legacy ParamSetCollection children carrier only. Every other
# legacy list carrier is owned by the native table snapshot, which detaches its
# leaves under the schema-specific policy this helper does not know. Reusing it
# for a new embedded path would silently install a second, weaker owner.
.upgrade_paradox_copy_list = function(x) {
  if (!.upgrade_paradox_is_ordinary_list(x) || is.object(x)) return(x)
  stable = .Call(C_upgrade_carrier_list_snapshot, x)
  if (is.null(stable)) {
    stop("Legacy list changed while being snapshotted", call. = FALSE)
  }
  lapply(stable, function(value) {
    if (is.atomic(value)) .upgrade_paradox_materialize_atomic(value) else value
  })
}

.upgrade_paradox_table = function(
    x,
    columns,
    path,
    classes = c("data.table", "data.frame"),
    extra_attributes = character()) {
  allow_repr = identical(extra_attributes, "repr")
  snapshot = tryCatch(
    .Call(
      C_upgrade_table_list_snapshot,
      x,
      classes,
      allow_repr
    ),
    error = function(error) {
      .upgrade_paradox_abort(
        path,
        "could not snapshot legacy table structure (%s)",
        conditionMessage(error)
      )
    }
  )
  if (is.null(snapshot) ||
      !identical(names(snapshot$table), columns)) {
    .upgrade_paradox_abort(
      path,
      "expected canonical data.table columns `%s`",
      paste(columns, collapse = "`, `")
    )
  }
  materialized = unname(snapshot$table)
  names(materialized) = columns
  lengths = lengths(materialized)
  if (length(lengths) && any(lengths != lengths[[1L]])) {
    .upgrade_paradox_abort(path, "table columns have inconsistent lengths")
  }
  # The native snapshot owns every top-level column, every interpreted list
  # carrier, and each non-S4 atomic list leaf before its terminal generation
  # receipt. These ordinary lengths and payloads are therefore already the one
  # authoritative migration snapshot; R performs no second semantic copy.
  if (allow_repr) {
    attr(materialized, ".paradox_upgrade_repr") = snapshot$repr
  }
  materialized
}

.upgrade_paradox_internal_table = function(columns) {
  param_set_internal_table(columns)
}

.upgrade_paradox_domain_table = function(columns) {
  param_set_data_table_facade(columns)
}

# The closed native Condition engine is the single owner of the right-hand-side
# shape rule: dependency admission and `condition_test()` both reject anything
# but an attribute-free logical, integer, numeric, or character vector. A
# migrated object must be usable by those owners, so the two built-in kinds are
# admitted through the engine before they are returned.
.upgrade_paradox_admit_condition = function(cond, path) {
  admitted = tryCatch({
    condition_test(cond, NULL)
    TRUE
  }, error = function(error) FALSE)
  if (!isTRUE(admitted)) {
    .upgrade_paradox_abort(
      path,
      paste0(
        "legacy Condition right-hand side must be an attribute-free logical, ",
        "integer, numeric, or character vector"
      )
    )
  }
  cond
}

# Scope: the standalone `upgrade_paradox_object()` entry point only. Legacy
# Conditions embedded in a dependency table or a Domain requirement are
# admitted, detached, and rebuilt by the native table owner, which applies the
# strict engine rule directly. This validator is deliberately the more lenient
# of the two -- it also admits the untestable base `Condition` class -- so it
# must not become the owner of a new embedded path.
.upgrade_paradox_condition = function(cond, path, allow_base = FALSE) {
  classes = .upgrade_paradox_class_snapshot(cond, path)
  if (!.upgrade_paradox_is_ordinary_list(cond) || length(cond) != 2L ||
      !.upgrade_paradox_has_only_attributes(cond, c("names", "class")) ||
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
    return(.upgrade_paradox_admit_condition(CondEqual(rhs), path))
  }
  if (identical(classes, c("CondAnyOf", "Condition"))) {
    if (!is.atomic(rhs) || !length(rhs) || anyNA(plain_rhs) ||
        anyDuplicated(plain_rhs) ||
        !identical(format, "%s %%in%% {%s}")) {
      .upgrade_paradox_abort(path, "malformed legacy CondAnyOf")
    }
    return(.upgrade_paradox_admit_condition(CondAnyOf(rhs), path))
  }
  if (allow_base && identical(classes, "Condition")) {
    # The base `Condition` class is outside the closed engine by design: it is
    # constructible and printable but never testable, so there is no engine
    # rule to admit it against.
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

.upgrade_paradox_validate_domain_columns = function(columns, path) {
  scalar_character = c("id", "cls", "grouping", "storage_type")
  scalar_numeric = c("lower", "upper", "tolerance")
  list_columns = c("cargo", "levels", "special_vals", "default")
  for (name in scalar_character) {
    value = columns[[name]]
    if (!is.character(value) || anyNA(value)) {
      .upgrade_paradox_abort(path, "Domain column `%s` must be character", name)
    }
  }
  for (name in scalar_numeric) {
    # An integer column is canonical here -- `p_int()` stores integer bounds in
    # both Paradox 1 and 2, and the native capsule admits REALSXP or INTSXP --
    # so requiring a double refused to migrate an ordinary `p_int(1L, 10L)`.
    value = columns[[name]]
    if (!is.double(value) && !is.integer(value)) {
      .upgrade_paradox_abort(
        path, "Domain column `%s` must be numeric", name
      )
    }
  }
  for (name in list_columns) {
    if (!.upgrade_paradox_is_ordinary_list(columns[[name]])) {
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
    if (!is.null(cargo) &&
        (!.upgrade_paradox_is_ordinary_list(cargo) || is.object(cargo) ||
          !.upgrade_paradox_has_only_attributes(cargo, "names"))) {
      .upgrade_paradox_abort(
        sprintf("%s$cargo[[%d]]", path, index),
        "unsupported Domain cargo"
      )
    }
  }
  columns
}

.upgrade_paradox_crate_bindings = function(callback, binding_names, template) {
  wrapper_environment = environment(callback)
  if (!is.environment(wrapper_environment) ||
      !identical(parent.env(wrapper_environment), asNamespace("paradox")) ||
      !is.null(attributes(wrapper_environment)) ||
      environmentIsLocked(wrapper_environment) ||
      !identical(
        sort(ls(wrapper_environment, all.names = TRUE)),
        sort(binding_names)
      ) ||
      any(vapply(
        binding_names,
        bindingIsActive,
        logical(1L),
        env = wrapper_environment
      )) ||
      any(vapply(
        binding_names,
        bindingIsLocked,
        logical(1L),
        env = wrapper_environment
      )) ||
      length(setdiff(
        names(attributes(callback)),
        .paradox_srcref_attributes
      )) ||
      !identical(
        .paradox_strip_srcref_node(formals(callback)),
        .paradox_strip_srcref_node(formals(template))
      ) ||
      !identical(
        .paradox_strip_srcref_node(body(callback)),
        .paradox_strip_srcref_node(body(template))
      )) {
    return(NULL)
  }

  snapshots = lapply(binding_names, function(name) {
    .paradox_plain_binding_snapshot(wrapper_environment, name)
  })
  if (any(!vapply(snapshots, function(snapshot) {
    isTRUE(snapshot$ok)
  }, logical(1L)))) {
    return(NULL)
  }
  stats::setNames(lapply(snapshots, `[[`, "value"), binding_names)
}

.upgrade_paradox_fct_trafo_template = function(x) {
  x = levels[[x]]
  if (!is.null(trafo)) x = trafo(x)
  x
}

.upgrade_paradox_collection_in_tune_template = function(
    domain,
    param_vals
) {
  param_vals = param_vals[names(param_vals) %in% prefixed_set_ids]
  names(param_vals) = gsub(
    sprintf("^\\Q%s.\\E", prefix),
    "",
    names(param_vals)
  )
  in_tune_fn(domain, param_vals)
}

.upgrade_paradox_tune_trafo_template = function(x, param_set) {
  mlr3misc::set_names(
    checkmate::assert_list(
      trafo(x),
      len = 1,
      .var.name = sprintf(
        "Trafo for tuning ParamSet for parameter %s",
        pname
      )
    ),
    pname
  )
}

.upgrade_paradox_collection_extra_trafo_template = function(x)
  psc_extra_trafo(
    x,
    children_with_trafos,
    sets_with_trafos,
    translation,
    postfix
  )

.upgrade_paradox_collection_constraint_template = function(x)
  psc_constraint(
    x,
    children_with_constraints,
    sets_with_constraints,
    translation
  )

.upgrade_paradox_rebuild_legacy_crate = function(callback, bindings) {
  # Carrier rebinding must never mutate the authenticated legacy wrapper.
  # `environment<-` gives the closure a fresh environment; the ordinary
  # admission helper independently decides whether source metadata is removed
  # or retained under `paradox.strip_srcrefs`.
  rebuilt = .paradox_strip_srcref(callback)
  environment(rebuilt) = list2env(
    bindings,
    parent = asNamespace("paradox")
  )
  compiler::cmpfun(rebuilt)
}

.upgrade_paradox_strip_legacy_in_tune_fn = function(callback) {
  if (!isTRUE(getOption("paradox.strip_srcrefs", TRUE))) return(callback)
  bindings = .upgrade_paradox_crate_bindings(
    callback,
    c("in_tune_fn", "prefix", "prefixed_set_ids"),
    .upgrade_paradox_collection_in_tune_template
  )
  if (is.null(bindings) ||
      (!is.null(bindings$in_tune_fn) &&
        !is.function(bindings$in_tune_fn)) ||
      !is.character(bindings$prefix) ||
      length(bindings$prefix) != 1L ||
      is.na(bindings$prefix) ||
      !is.character(bindings$prefixed_set_ids) ||
      anyNA(bindings$prefixed_set_ids)) {
    return(.paradox_strip_srcref(callback))
  }
  param_set_collection_in_tune_fn_factory(
    .paradox_strip_srcref(bindings$in_tune_fn),
    bindings$prefix,
    bindings$prefixed_set_ids
  )
}

.upgrade_paradox_strip_cargo_callbacks = function(cargo) {
  if (is.null(cargo) || !.upgrade_paradox_is_ordinary_list(cargo) ||
      is.object(cargo) ||
      !.upgrade_paradox_has_only_attributes(cargo, "names")) {
    return(cargo)
  }
  cargo_names = names(cargo)
  if (is.null(cargo_names)) return(cargo)
  callback_indices = which(
    cargo_names %in% c("custom_check", "aggr", "in_tune_fn")
  )
  for (index in callback_indices) {
    callback = cargo[[index]]
    if (is.function(callback)) {
      cargo[[index]] = if (identical(
          cargo_names[[index]],
          "in_tune_fn"
        )) {
        .upgrade_paradox_strip_legacy_in_tune_fn(callback)
      } else {
        .paradox_strip_srcref(callback)
      }
    }
  }
  cargo
}

.upgrade_paradox_strip_domain_callbacks = function(columns) {
  columns$cargo = lapply(
    columns$cargo,
    .upgrade_paradox_strip_cargo_callbacks
  )
  if (".trafo" %in% names(columns)) {
    columns$.trafo = lapply(seq_along(columns$.trafo), function(index) {
      trafo = columns$.trafo[[index]]
      if (!is.function(trafo)) return(trafo)
      .upgrade_paradox_strip_legacy_fct_trafo(
        trafo,
        columns$id[[index]],
        columns
      )
    })
  }
  columns
}

.upgrade_paradox_domain = function(domain, path) {
  classes = .upgrade_paradox_class_snapshot(domain, path)
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
  repr = attr(columns, ".paradox_upgrade_repr", exact = TRUE)
  attr(columns, ".paradox_upgrade_repr") = NULL
  if (length(columns$id) != 1L || !identical(columns$cls, kind)) {
    .upgrade_paradox_abort(path, "Domain must contain one matching built-in row")
  }
  columns = .upgrade_paradox_validate_domain_columns(columns, path)
  if (!is.logical(columns$.init_given) || length(columns$.init_given) != 1L ||
      is.na(columns$.init_given) ||
      !.upgrade_paradox_is_ordinary_list(columns$.init) ||
      !.upgrade_paradox_is_ordinary_list(columns$.tags) ||
      !.upgrade_paradox_is_ordinary_list(columns$.trafo) ||
      !.upgrade_paradox_is_ordinary_list(columns$.requirements)) {
    .upgrade_paradox_abort(path, "malformed transient Domain columns")
  }
  # The native table snapshot has already rebuilt built-in requirements and
  # their Conditions through the canonical C owner. Keep that exact private
  # result; a second R constructor pass would be a duplicate semantic engine.
  domain_tags = columns$.tags[[1L]]
  if (!is.character(domain_tags) || !is.null(attributes(domain_tags)) ||
      anyNA(domain_tags) || anyDuplicated(domain_tags)) {
    .upgrade_paradox_abort(path, "malformed Domain tags")
  }
  columns$.tags[[1L]] = domain_tags
  if (!is.null(columns$.trafo[[1L]]) &&
      !is.function(columns$.trafo[[1L]])) {
    .upgrade_paradox_abort(path, "malformed Domain transformation")
  }
  columns = .upgrade_paradox_strip_domain_callbacks(columns)

  result = .upgrade_paradox_domain_table(columns)
  class(result) = classes
  repr = .paradox_strip_srcref(repr)
  if (!is.null(repr)) attr(result, "repr") = repr
  result
}

.upgrade_paradox_params = function(params, path) {
  columns = .upgrade_paradox_table(params, domain_names_permanent, path)
  columns = .upgrade_paradox_validate_domain_columns(columns, path)
  ids = columns$id
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    .upgrade_paradox_abort(path, "parameter IDs must be nonempty and unique")
  }
  .upgrade_paradox_strip_domain_callbacks(columns)
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

.upgrade_paradox_strip_legacy_fct_trafo = function(
    trafo,
    id,
    param_columns
) {
  if (!isTRUE(getOption("paradox.strip_srcrefs", TRUE))) return(trafo)
  param_index = match(id, param_columns$id, nomatch = 0L)
  if (!param_index ||
      !identical(param_columns$cls[[param_index]], "ParamFct")) {
    return(.paradox_strip_srcref(trafo))
  }

  bindings = .upgrade_paradox_crate_bindings(
    trafo,
    c("levels", "trafo"),
    .upgrade_paradox_fct_trafo_template
  )
  if (is.null(bindings)) {
    return(.paradox_strip_srcref(trafo))
  }

  if (!(is.atomic(bindings$levels) ||
      .upgrade_paradox_is_ordinary_list(bindings$levels)) ||
      (is.list(bindings$levels) &&
        !.upgrade_paradox_has_only_attributes(bindings$levels, "names")) ||
      !identical(
        .upgrade_paradox_materialize_atomic(names(bindings$levels)),
        param_columns$levels[[param_index]]
      ) ||
      (!is.null(bindings$trafo) &&
        !is.function(bindings$trafo))) {
    return(.paradox_strip_srcref(trafo))
  }

  .paradox_strip_srcref(.make_p_fct_trafo(
    bindings$levels,
    .paradox_strip_srcref(bindings$trafo)
  ))
}

.upgrade_paradox_strip_legacy_extra_trafo = function(callback) {
  if (isTRUE(getOption("paradox.strip_srcrefs", TRUE))) {
    tune_bindings = .upgrade_paradox_crate_bindings(
      callback,
      c("trafo", "pname"),
      .upgrade_paradox_tune_trafo_template
    )
    if (!is.null(tune_bindings) &&
        is.function(tune_bindings$trafo) &&
        is.character(tune_bindings$pname) &&
        length(tune_bindings$pname) == 1L &&
        !is.na(tune_bindings$pname)) {
      return(.make_tune_param_set_trafo(
        .paradox_strip_srcref(tune_bindings$trafo),
        tune_bindings$pname
      ))
    }
  }

  collection_bindings = .upgrade_paradox_crate_bindings(
    callback,
    c(
      "children_with_trafos", "sets_with_trafos", "translation",
      "psc_extra_trafo", "postfix"
    ),
    .upgrade_paradox_collection_extra_trafo_template
  )
  if (!is.null(collection_bindings) &&
      .upgrade_paradox_is_ordinary_list(
        collection_bindings$sets_with_trafos
      ) &&
      is.function(collection_bindings$psc_extra_trafo)) {
    collection_bindings$psc_extra_trafo = .paradox_strip_srcref(
      collection_bindings$psc_extra_trafo
    )
    return(.upgrade_paradox_rebuild_legacy_crate(
      callback,
      collection_bindings
    ))
  }

  .paradox_strip_srcref(callback)
}

.upgrade_paradox_strip_legacy_constraint = function(callback) {
  collection_bindings = .upgrade_paradox_crate_bindings(
    callback,
    c(
      "children_with_constraints", "sets_with_constraints",
      "translation", "psc_constraint"
    ),
    .upgrade_paradox_collection_constraint_template
  )
  if (!is.null(collection_bindings) &&
      .upgrade_paradox_is_ordinary_list(
        collection_bindings$sets_with_constraints
      ) &&
      is.function(collection_bindings$psc_constraint)) {
    collection_bindings$psc_constraint = .paradox_strip_srcref(
      collection_bindings$psc_constraint
    )
    return(.upgrade_paradox_rebuild_legacy_crate(
      callback,
      collection_bindings
    ))
  }
  .paradox_strip_srcref(callback)
}

.upgrade_paradox_callback_carrier_snapshot = function(
    carrier,
    field,
    path
) {
  stable = .Call(C_upgrade_carrier_list_snapshot, carrier)
  if (is.null(stable)) {
    .upgrade_paradox_abort(
      path,
      "legacy detached callback `%s` carriers must be an ordinary list",
      field
    )
  }
  stable
}

.upgrade_paradox_callback_carriers = function(
    extra_trafo,
    constraint,
    path
) {
  extra_bindings = if (is.function(extra_trafo)) {
    .upgrade_paradox_crate_bindings(
      extra_trafo,
      c(
        "children_with_trafos", "sets_with_trafos", "translation",
        "psc_extra_trafo", "postfix"
      ),
      .upgrade_paradox_collection_extra_trafo_template
    )
  } else {
    NULL
  }
  # A wrapper with the exact known crate shape remains a carrier candidate even
  # when its carrier shell is malformed.  The stripping helper deliberately
  # refuses to rebuild such a wrapper, but migration must reject it below
  # rather than silently retain a callback that still closes over legacy
  # children.  A valid carrier wrapper was rebuilt with a fresh environment, so
  # the later rebind cannot mutate the serialized object's closure environment.
  if (!is.null(extra_bindings) &&
      !is.function(extra_bindings$psc_extra_trafo)) {
    extra_bindings = NULL
  }
  constraint_bindings = if (is.function(constraint)) {
    .upgrade_paradox_crate_bindings(
      constraint,
      c(
        "children_with_constraints", "sets_with_constraints",
        "translation", "psc_constraint"
      ),
      .upgrade_paradox_collection_constraint_template
    )
  } else {
    NULL
  }
  if (!is.null(constraint_bindings) &&
      !is.function(constraint_bindings$psc_constraint)) {
    constraint_bindings = NULL
  }
  list(
    extra_trafo = if (is.null(extra_bindings)) {
      NULL
    } else {
      .upgrade_paradox_callback_carrier_snapshot(
        extra_bindings$sets_with_trafos,
        "sets_with_trafos",
        path
      )
    },
    constraint = if (is.null(constraint_bindings)) {
      NULL
    } else {
      .upgrade_paradox_callback_carrier_snapshot(
        constraint_bindings$sets_with_constraints,
        "sets_with_constraints",
        path
      )
    }
  )
}

.upgrade_paradox_callback_dependencies = function(carriers) {
  extra_trafo = unname(carriers$extra_trafo)
  constraint = unname(carriers$constraint)
  result = c(list(), extra_trafo, constraint)
  names(result) = c(
    sprintf("extra_trafo_%d", seq_along(extra_trafo)),
    sprintf("constraint_%d", seq_along(constraint))
  )
  result
}

.upgrade_paradox_rebind_callback_carriers = function(
    extra_trafo,
    constraint,
    carriers,
    dependencies,
    path
) {
  expected = length(carriers$extra_trafo) + length(carriers$constraint)
  if (length(dependencies) != expected) {
    .upgrade_paradox_abort(
      path,
      "internal detached-callback dependency mismatch"
    )
  }
  cursor = 0L
  if (!is.null(carriers$extra_trafo)) {
    count = length(carriers$extra_trafo)
    replacements = dependencies[seq.int(cursor + 1L, length.out = count)]
    names(replacements) = names(carriers$extra_trafo)
    assign(
      "sets_with_trafos",
      replacements,
      envir = environment(extra_trafo)
    )
    cursor = cursor + count
  }
  if (!is.null(carriers$constraint)) {
    count = length(carriers$constraint)
    replacements = dependencies[seq.int(cursor + 1L, length.out = count)]
    names(replacements) = names(carriers$constraint)
    assign(
      "sets_with_constraints",
      replacements,
      envir = environment(constraint)
    )
  }
  list(extra_trafo = extra_trafo, constraint = constraint)
}

.upgrade_paradox_trafos = function(trafos, param_columns, path) {
  columns = .upgrade_paradox_table(trafos, c("id", "trafo"), path)
  id = columns$id
  trafo = columns$trafo
  ids = param_columns$id
  if (!is.character(id) || anyNA(id) || any(id %nin% ids) ||
      anyDuplicated(id) || !.upgrade_paradox_is_ordinary_list(trafo) ||
      any(!vapply(trafo, is.function, logical(1L)))) {
    .upgrade_paradox_abort(path, "malformed legacy transformation table")
  }
  trafo = lapply(seq_along(trafo), function(index) {
    .upgrade_paradox_strip_legacy_fct_trafo(
      trafo[[index]],
      id[[index]],
      param_columns
    )
  })
  .upgrade_paradox_internal_table(list(id = id, trafo = trafo))
}

.upgrade_paradox_deps = function(deps, ids, path) {
  columns = .upgrade_paradox_table(deps, c("id", "on", "cond"), path)
  id = columns$id
  on = columns$on
  cond = columns$cond
  if (!is.character(id) || !is.character(on) ||
      !.upgrade_paradox_is_ordinary_list(cond) ||
      anyNA(id) || anyNA(on) || any(id %nin% ids)) {
    .upgrade_paradox_abort(path, "malformed legacy dependency table")
  }
  # Exact built-in Conditions were already admitted, detached, and rebuilt by
  # the native table owner. This R layer validates only the surrounding legacy
  # parameter references and constructs the canonical internal table.
  .upgrade_paradox_internal_table(list(id = id, on = on, cond = cond))
}

.upgrade_paradox_values = function(values, param_columns, path) {
  snapshot = tryCatch(
    .Call(
      C_upgrade_values_snapshot,
      values,
      param_columns$id,
      param_columns$cls
    ),
    error = function(error) {
      .upgrade_paradox_abort(
        path,
        "could not snapshot legacy values (%s)",
        conditionMessage(error)
      )
    }
  )
  if (identical(snapshot, FALSE)) {
    .upgrade_paradox_abort(path, "legacy values have invalid parameter names")
  }
  if (is.null(snapshot)) {
    .upgrade_paradox_abort(path, "legacy values must be a canonical named list")
  }
  ids = param_columns$id
  # Backstop only; the native snapshot is the authoritative owner of these
  # name rules and has already refused every shape rechecked here.
  value_names = names(snapshot)
  if (is.null(value_names)) value_names = character(length(snapshot))
  if (length(value_names) != length(snapshot) || anyNA(value_names) ||
      any(!nzchar(value_names)) || anyDuplicated(value_names) ||
      any(value_names %nin% ids)) {
    .upgrade_paradox_abort(path, "legacy values have invalid parameter names")
  }
  # Only a zero-length unnamed list reaches this point with `names()` NULL --
  # every longer unnamed list fails the nzchar check above. Paradox 1 stored
  # `named_list()` even when empty, but the current capsule requires the names
  # attribute outright: installed verbatim, an unnamed empty `.values` is
  # admitted at build time and rejected by every later graph validation.
  # Install the validated names unconditionally to keep the copy canonical.
  names(snapshot) = value_names
  snapshot[match(ids, value_names, nomatch = 0L)]
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
    .upgrade_paradox_binding(private, ".trafos", path), param_columns,
    paste0(path, "$private$.trafos")
  )
  deps = .upgrade_paradox_deps(
    .upgrade_paradox_binding(private, ".deps", path), ids,
    paste0(path, "$private$.deps")
  )
  values = .upgrade_paradox_values(
    .upgrade_paradox_binding(private, ".values", path), param_columns,
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
  if (is.function(extra_trafo)) {
    extra_trafo = .upgrade_paradox_strip_legacy_extra_trafo(extra_trafo)
  }
  if (is.function(constraint)) {
    constraint = .upgrade_paradox_strip_legacy_constraint(constraint)
  }
  callback_carriers = .upgrade_paradox_callback_carriers(
    extra_trafo,
    constraint,
    path
  )

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
    callback_carriers = callback_carriers,
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
  if (!identical(owner_namespace, entry$.owner_namespace) ||
      !identical(paradox_namespace, asNamespace("paradox"))) {
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
      binding = .upgrade_paradox_active_binding_function(name, x, path)
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
      .upgrade_paradox_assert_values(value, path)
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
  if (!.upgrade_paradox_is_ordinary_list(sets) || is.object(sets) ||
      !.upgrade_paradox_has_only_attributes(sets, "names")) {
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
  if (!.upgrade_paradox_is_ordinary_list(values) || length(values) ||
      !.upgrade_paradox_has_only_attributes(values, "names")) {
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
  # Paradox 1's `$add()` computed `length(sets) + 1`, so a collection that ever
  # grew carries a double owner index. Accept an integerish double and
  # normalize it rather than refusing to migrate the object.
  owner_index = translation_columns$owner_ps_index
  if (is.double(owner_index) && !anyNA(owner_index) &&
      all(owner_index == trunc(owner_index)) &&
      all(abs(owner_index) <= .Machine$integer.max)) {
    translation_columns$owner_ps_index = as.integer(owner_index)
  }
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
      .upgrade_paradox_binding(private, ".trafos", path), param_columns,
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
  classes = .upgrade_paradox_class_snapshot(x, path)
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
    if (.upgrade_paradox_is_ordinary_list(column)) {
      list(column[[index]])
    } else {
      column[index]
    }
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

.upgrade_paradox_build_base = function(info, path, dependencies = list()) {
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
  callbacks = .upgrade_paradox_rebind_callback_carriers(
    info$extra_trafo,
    info$constraint,
    info$callback_carriers,
    dependencies,
    path
  )
  param_set_core_replace(
    private,
    values = info$values,
    tags = info$tags,
    deps = info$deps,
    trafos = info$trafos,
    extra_trafo = callbacks$extra_trafo,
    constraint = callbacks$constraint
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
  state = param_set_core_state(private, result)
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
  # A Paradox-2 collection derives its tags from the sets it contains, so the
  # legacy table is only stable if the per-edge `tag_sets`/`tag_params` flags
  # that generated it are recovered too. Paradox 1 consumed those flags at
  # `$new()`/`$add()` and kept only their output, so they are read back off
  # that output: an edge carries a flag exactly when every parameter it
  # contributes has the tag the flag would have generated. An edge whose child
  # is empty contributes nothing to read, and Paradox 1 recorded nothing for it
  # either.
  param_set_core_replace(
    private,
    tags = info$tags,
    deps = info$deps,
    trafos = info$trafos,
    edges = .upgrade_paradox_collection_edges(state, info$tags)
  )
  result$assert_values = info$assert_values
  result
}

.upgrade_paradox_collection_edges = function(state, tags) {
  translation = state$.translation
  owner_names = names(state$.sets)
  ids = state$.params$id
  legacy = split(tags$tag, factor(tags$id, levels = ids))
  inherited = split(state$.tags$tag, factor(state$.tags$id, levels = ids))
  edge_count = length(state$.sets)
  tag_sets = logical(edge_count)
  tag_params = logical(edge_count)
  generated = stats::setNames(rep(list(character()), length(ids)), ids)
  for (edge in seq_len(edge_count)) {
    rows = translation$owner_ps_index == edge
    edge_ids = translation$id[rows]
    if (!length(edge_ids)) next
    owner = owner_names[[edge]]
    set_tag = if (nzchar(owner)) paste0("set_", owner)
    param_tags = paste0("param_", translation$original_id[rows])
    if (!is.null(set_tag)) {
      tag_sets[[edge]] = all(vapply(
        edge_ids,
        function(id) set_tag %in% legacy[[id]],
        logical(1L)
      ))
    }
    tag_params[[edge]] = all(vapply(
      seq_along(edge_ids),
      function(index) param_tags[[index]] %in% legacy[[edge_ids[[index]]]],
      logical(1L)
    ))
    for (index in seq_along(edge_ids)) {
      generated[[edge_ids[[index]]]] = c(
        if (isTRUE(tag_sets[[edge]])) set_tag,
        if (tag_params[[edge]]) param_tags[[index]]
      )
    }
  }
  # Whatever the recovered flags reproduce stays derived, so a later change to
  # a contained set still reaches it. Only the rows Paradox 1 held that no set
  # accounts for become this node's own answer.
  differs = vapply(
    ids,
    function(id) !setequal(legacy[[id]], c(inherited[[id]], generated[[id]])),
    logical(1L)
  )
  list(
    cores = state$.edges$cores,
    tag_sets = tag_sets,
    tag_params = tag_params,
    tag_override = if (any(differs)) {
      governed = ids[differs]
      list(
        ids = governed,
        tags = param_set_internal_table(
          tags[tags$id %in% governed, , drop = FALSE]
        )
      )
    }
  )
}

.upgrade_paradox_graph = function(x) {
  session = .upgrade_paradox_prepare_session(list(x), "x")
  # The recursive API proves every prepared root jointly before the first
  # original shell changes. The non-mutating API hands its prepared graph to
  # the caller instead of a transplant, so it needs the same barrier: without
  # it a preparation defect fails closed over there and fails open here, as a
  # returned object whose first native admission reports a corrupt capsule.
  .upgrade_paradox_validate_current_roots(
    session$prepared,
    "Cannot upgrade Paradox object at x"
  )
  session$prepared[[1L]]
}

.upgrade_paradox_info_dependencies = function(info) {
  if (identical(info$kind, "base")) {
    return(.upgrade_paradox_callback_dependencies(info$callback_carriers))
  }
  if (identical(info$kind, "collection")) return(info$children)
  if (identical(info$kind, "owner")) {
    base_dependencies = if (identical(
        info$owner$migration_kind,
        "additive"
      )) {
      .upgrade_paradox_callback_dependencies(
        info$base$callback_carriers
      )
    } else {
      list()
    }
    return(c(base_dependencies, info$dependencies))
  }
  list()
}

.upgrade_paradox_split_owner_dependencies = function(
    info,
    dependencies,
    path
) {
  base_count = if (identical(info$owner$migration_kind, "additive")) {
    length(.upgrade_paradox_callback_dependencies(
      info$base$callback_carriers
    ))
  } else {
    0L
  }
  owner_count = length(info$dependencies)
  if (length(dependencies) != base_count + owner_count) {
    .upgrade_paradox_abort(
      path,
      "internal owner dependency mismatch"
    )
  }
  base = dependencies[seq_len(base_count)]
  owner = dependencies[seq.int(
    base_count + 1L,
    length.out = owner_count
  )]
  names(owner) = names(info$dependencies)
  list(base = base, owner = owner)
}

.upgrade_paradox_mutable_shell_environments = function(
    x,
    path,
    qualifier
) {
  shell = .upgrade_paradox_shell(x, path)
  result = list(
    `public shell` = x,
    `private environment` = shell$private,
    `top enclosure` = shell$enclosing
  )
  enclosing = shell$enclosing
  depth = 0L
  repeat {
    super = .upgrade_paradox_binding(
      enclosing,
      "super",
      path,
      required = FALSE
    )
    if (is.null(super)) break
    if (!is.environment(super)) {
      .upgrade_paradox_abort(
        path,
        "%s has a malformed R6 superclass proxy",
        qualifier
      )
    }
    next_enclosing = .upgrade_paradox_binding(
      super,
      ".__enclos_env__",
      path
    )
    if (!is.environment(next_enclosing) ||
        any(vapply(result, identical, logical(1L), y = next_enclosing)) ||
        !identical(
          .upgrade_paradox_binding(next_enclosing, "self", path),
          x
        ) ||
        !identical(
          .upgrade_paradox_binding(next_enclosing, "private", path),
          shell$private
        )) {
      .upgrade_paradox_abort(
        path,
        "%s has a malformed R6 superclass enclosure",
        qualifier
      )
    }
    depth = depth + 1L
    result[[sprintf("superclass enclosure %d", depth)]] = next_enclosing
    enclosing = next_enclosing
  }

  # Every environment changed by owner preparation or transplant must be
  # individually owned. A distinct public shell with an aliased private or
  # enclosure environment is not fresh: installing `.core` or rebasing `self`
  # would mutate another object just as surely as reusing its public shell.
  if (length(result) > 1L) {
    for (right in seq.int(2L, length(result))) {
      for (left in seq_len(right - 1L)) {
        if (identical(result[[left]], result[[right]])) {
          .upgrade_paradox_abort(
            path,
            "%s aliases its `%s` and `%s`",
            qualifier,
            names(result)[[left]],
            names(result)[[right]]
          )
        }
      }
    }
  }
  result
}

.upgrade_paradox_require_fresh_owner_result = function(
    result,
    forbidden_shells,
    forbidden_paths,
    path,
    entry
) {
  if (!is.list(forbidden_shells) ||
      !is.character(forbidden_paths) ||
      length(forbidden_shells) != length(forbidden_paths)) {
    .upgrade_paradox_abort(path, "internal owner-alias preflight mismatch")
  }
  result_environments = .upgrade_paradox_mutable_shell_environments(
    result,
    path,
    "rebuilt owner shell"
  )
  for (index in seq_along(forbidden_shells)) {
    forbidden_environments = .upgrade_paradox_mutable_shell_environments(
      forbidden_shells[[index]],
      path,
      forbidden_paths[[index]]
    )
    for (result_position in seq_along(result_environments)) {
      for (forbidden_position in seq_along(forbidden_environments)) {
        if (identical(
            result_environments[[result_position]],
            forbidden_environments[[forbidden_position]]
          )) {
          .upgrade_paradox_abort(
            path,
            paste0(
              "owner upgrader `%s::%s()` must return a fresh shell; ",
              "its `%s` aliases %s's `%s`"
            ),
            entry$owner_package,
            entry$rebuilder,
            names(result_environments)[[result_position]],
            forbidden_paths[[index]],
            names(forbidden_environments)[[forbidden_position]]
          )
        }
      }
    }
  }
  invisible(NULL)
}

.upgrade_paradox_build_owner = function(
    info,
    dependencies,
    path,
    forbidden_shells,
    forbidden_paths
) {
  entry = info$owner
  dependencies = .upgrade_paradox_split_owner_dependencies(
    info,
    dependencies,
    path
  )
  base = if (identical(entry$migration_kind, "additive")) {
    .upgrade_paradox_build_base(info$base, path, dependencies$base)
  } else {
    NULL
  }
  rebuilder = .paradox_object_upgrader_resolve(entry, "rebuilder")
  result = tryCatch(
    rebuilder(base, info$owner_state, dependencies$owner),
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
  # A rebuilder result is modified below while it is still an off-side
  # replacement.  Reject every identity already owned by this migration
  # session before reading its shell or changing `.core`/`assert_values`.
  # Distinct owners likewise cannot share one prepared shell: the first
  # transplant would rebase that shell's enclosure and invalidate the second
  # plan.
  .upgrade_paradox_require_fresh_owner_result(
    result,
    forbidden_shells,
    forbidden_paths,
    path,
    entry
  )
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
    # This capsule-kind requirement composes with the class-vector check
    # above and with the graph validation below: the validator accepts the
    # shell only when its class kind equals its capsule kind, and a
    # registered class `c(<owner>, "ParamSet", "R6")` classes as SHADOW only
    # when <owner> is exactly "ParamSetShadow". A replacement bridge for any
    # other owner label therefore cannot ever satisfy both checks and fails
    # closed below; symmetrically, an additive bridge for the
    # "ParamSetShadow" label always fails the validation after its `.core`
    # is replaced with the BASE capsule. register_paradox_object_upgrader()
    # refuses both doomed combinations up front; these checks remain the
    # backstop for entries injected past registration.
  }
  result$assert_values = info$assert_values
  .upgrade_paradox_validate_current_graph(result, path)
  result
}

.upgrade_paradox_build_prepared = function(
    info,
    dependencies,
    path,
    forbidden_shells = list(),
    forbidden_paths = character()
) {
  switch(
    info$kind,
    current = info$value,
    base = .upgrade_paradox_build_base(info, path, dependencies),
    collection = {
      names(dependencies) = names(info$children)
      .upgrade_paradox_build_collection(info, dependencies, path)
    },
    owner = .upgrade_paradox_build_owner(
      info,
      dependencies,
      path,
      forbidden_shells,
      forbidden_paths
    ),
    .upgrade_paradox_abort(path, "internal unknown migration node kind")
  )
}

# Build every replacement before touching a serialized shell. The session map
# is deliberately shared by all roots returned by the native host-graph
# crawler, so aliases and collection/shadow edges are rebuilt exactly once.
# Identity lookup over the session node list. Both the preparation and the
# commit phase resolve dependency edges through this one helper; a zero result
# is an internal traversal failure the caller must guard.
.upgrade_paradox_match_node = function(nodes, node) {
  for (index in seq_along(nodes)) {
    if (identical(nodes[[index]], node)) return(index)
  }
  0L
}

.upgrade_paradox_prepare_session = function(candidates, paths) {
  if (!.upgrade_paradox_is_ordinary_list(candidates) ||
      !is.character(paths) ||
      length(candidates) != length(paths)) {
    stop("Internal error: malformed Paradox migration candidate set", call. = FALSE)
  }

  nodes = list()
  infos = list()
  node_paths = character()
  status = integer()
  prepared = list()
  commit_order = integer()

  find_node = function(node) .upgrade_paradox_match_node(nodes, node)
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

  # Register every host-graph root before running any owner rebuilder.  The
  # discovery pass below then registers every recursive ParamSet dependency
  # before the separate build pass starts.  Consequently an owner rebuilder
  # cannot smuggle any original/current session node back as its supposedly
  # fresh replacement, including a node reachable only below a later root.
  roots = vapply(
    seq_along(candidates),
    function(root_position) {
      add_node(candidates[[root_position]], paths[[root_position]])
    },
    integer(1L)
  )
  build_order = integer()

  for (root in roots) {
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
        } else if (identical(info$kind, "base")) {
          sprintf(
            "%s$callback_dependencies[[%s]]",
            node_paths[[index]],
            dependency_name
          )
        } else if (identical(
            info$owner$migration_kind,
            "additive"
          ) && dependency_position <= length(
            .upgrade_paradox_callback_dependencies(
              info$base$callback_carriers
            )
          )) {
          sprintf(
            "%s$base_callback_dependencies[[%s]]",
            node_paths[[index]],
            dependency_name
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

      build_order = c(build_order, index)
      status[[index]] = 2L
      frames[[frame_position]] = NULL
    }
  }

  # The post-order discovery above makes every dependency precede its owner.
  # Build off-side replacements only after the complete session identity set
  # is known.  Owner outputs are additionally forbidden from aliasing an
  # already prepared node, while ordinary shared dependencies and identity
  # reuse by already-current nodes remain valid.
  built = logical(length(nodes))
  prepared_indices = integer()
  for (index in build_order) {
    info = infos[[index]]
    dependencies = .upgrade_paradox_info_dependencies(info)
    dependency_indices = vapply(dependencies, find_node, integer(1L))
    if (length(dependency_indices) &&
        (any(!dependency_indices) || any(!built[dependency_indices]))) {
      .upgrade_paradox_abort(
        node_paths[[index]],
        "internal graph traversal failure"
      )
    }
    forbidden_shells = list()
    forbidden_paths = character()
    if (identical(info$kind, "owner")) {
      forbidden_shells = c(nodes, prepared[prepared_indices])
      forbidden_paths = c(
        sprintf("session node at %s", node_paths),
        sprintf(
          "prepared node for %s",
          node_paths[prepared_indices]
        )
      )
    }
    prepared[[index]] = .upgrade_paradox_build_prepared(
      info,
      prepared[dependency_indices],
      node_paths[[index]],
      forbidden_shells,
      forbidden_paths
    )
    prepared_indices = c(prepared_indices, index)
    if (!identical(info$kind, "current")) {
      commit_order = c(commit_order, index)
    }
    built[[index]] = TRUE
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

.upgrade_paradox_make_public_binding_receipt = function(
    owner,
    names,
    values,
    active,
    locked,
    selected_class = attr(owner, "class", exact = TRUE),
    validate = FALSE
) {
  symbols = unname(lapply(names, as.name))
  receipt = unname(list(
    owner,
    selected_class,
    symbols,
    unname(values),
    unname(active),
    unname(locked),
    environmentIsLocked(owner)
  ))
  if (validate) {
    .Call(C_upgrade_public_binding_receipts, list(receipt))
  }
  receipt
}

.upgrade_paradox_public_binding_snapshot = function(owner, path) {
  # The selected inventory is complete only for a locked R6 object
  # environment: R permits an unlocked binding in a locked environment to be
  # replaced, but it cannot add or remove a binding. Genuine Paradox shells
  # are locked at construction, so accepting an unlocked forged current shell
  # would weaken the terminal receipt without preserving supported state.
  if (!is.environment(owner) || !environmentIsLocked(owner)) {
    .upgrade_paradox_abort(path, "current public R6 shell must be locked")
  }
  names = ls(owner, all.names = TRUE)
  shape = .upgrade_paradox_binding_shape(owner, names)
  values = lapply(seq_along(names), function(position) {
    name = names[[position]]
    if (shape$active[[position]]) {
      .upgrade_paradox_active_binding_function(name, owner, path)
    } else {
      .upgrade_paradox_binding(owner, name, path)
    }
  })
  .upgrade_paradox_make_public_binding_receipt(
    owner,
    names,
    values,
    shape$active,
    shape$locked,
    selected_class = attr(owner, "class", exact = TRUE),
    validate = TRUE
  )
}

.upgrade_paradox_transplant_plan = function(
    legacy,
    current,
    path,
    retired_bindings = character(),
    owner_package = "paradox") {
  if (!is.environment(legacy) || !environmentIsLocked(legacy) ||
      !is.environment(current) || !environmentIsLocked(current)) {
    .upgrade_paradox_abort(
      path,
      "legacy and prepared current public R6 shells must be locked"
    )
  }
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
    if (!.upgrade_paradox_is_ordinary_list(active_metadata) ||
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

  current_values = lapply(seq_along(current_names), function(position) {
    name = current_names[[position]]
    if (current_shape$active[[position]]) {
      .upgrade_paradox_active_binding_function(name, current, path)
    } else {
      .upgrade_paradox_binding(current, name, path)
    }
  })
  # All allocation-capable selection is complete. Authenticate the exact
  # package-created public generation now; otherwise a pending finalizer could
  # make the transplant plan itself combine method values or lock bits which
  # never coexisted.
  .upgrade_paradox_make_public_binding_receipt(
    current,
    current_names,
    current_values,
    current_shape$active,
    current_shape$locked,
    selected_class = attr(current, "class", exact = TRUE),
    validate = TRUE
  )
  final_names = c(current_names, retired_bindings)
  final_values = c(current_values, retired_values)
  final_active = c(
    current_shape$active,
    rep(TRUE, length(retired_bindings))
  )
  final_locked = c(
    current_shape$locked,
    rep(FALSE, length(retired_bindings))
  )
  final_public_receipt = .upgrade_paradox_make_public_binding_receipt(
    legacy,
    final_names,
    final_values,
    final_active,
    final_locked,
    selected_class = attr(legacy, "class", exact = TRUE)
  )

  list(
    legacy = legacy,
    current = current,
    path = path,
    current_class = current_class,
    current_names = current_names,
    current_shape = current_shape,
    enclosure_position = enclosure_position,
    current_values = current_values,
    retired_bindings = retired_bindings,
    retired_values = retired_values,
    enclosures = enclosures,
    active_metadata = active_metadata,
    active_metadata_locked = active_metadata_locked,
    final_public_receipt = final_public_receipt
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
  if (identical(info$kind, "base") &&
      length(.upgrade_paradox_callback_dependencies(
        info$callback_carriers
      ))) {
    private = .upgrade_paradox_shell(prepared, path)$private
    state = param_set_core_state(private, prepared)
    callbacks = .upgrade_paradox_rebind_callback_carriers(
      state$.extra_trafo,
      state$.constraint,
      info$callback_carriers,
      originals,
      path
    )
    param_set_core_replace(
      private,
      extra_trafo = callbacks$extra_trafo,
      constraint = callbacks$constraint
    )
  } else if (identical(info$kind, "owner") &&
      identical(info$owner$migration_kind, "additive")) {
    dependencies = .upgrade_paradox_split_owner_dependencies(
      info,
      originals,
      path
    )
    if (length(dependencies$base)) {
      private = .upgrade_paradox_shell(prepared, path)$private
      state = param_set_core_state(private, prepared)
      callbacks = .upgrade_paradox_rebind_callback_carriers(
        state$.extra_trafo,
        state$.constraint,
        info$base$callback_carriers,
        dependencies$base,
        path
      )
      param_set_core_replace(
        private,
        extra_trafo = callbacks$extra_trafo,
        constraint = callbacks$constraint
      )
    }
  } else if (identical(info$kind, "collection")) {
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
  if (!length(session$commit_order)) {
    # A graph containing only current shells has no binding wave to protect.
    # Keep this public no-op available on R 3.6, where active-binding
    # functions cannot be retrieved through the public R API, and avoid
    # constructing transplant-only public receipts on every current graph.
    .upgrade_paradox_validate_current_roots(session$prepared)
    return(invisible(NULL))
  }

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

  # Select and validate all roots together before the first original shell
  # changes. Prepared parent graphs still point to their prepared current
  # children here; a parent can point to the identity-preserved original only
  # after post-order commit has upgraded that child.
  .upgrade_paradox_validate_current_roots(session$prepared)
  current_indices = which(vapply(
    session$infos,
    function(info) identical(info$kind, "current"),
    logical(1L)
  ))
  committed_roots = session$nodes[current_indices]
  current_public_receipts = lapply(current_indices, function(index) {
    .upgrade_paradox_public_binding_snapshot(
      session$nodes[[index]],
      session$paths[[index]]
    )
  })
  final_public_receipts = unname(c(
    current_public_receipts,
    lapply(plans, `[[`, "final_public_receipt")
  ))

  # Commit is monotonic and post-order. Repoint each offside parent capsule only
  # after its original children are current. A rebase can allocate, and a
  # replacement-owner rebase may execute package code, so validate *every*
  # already-current identity root plus the newly rebased prepared root jointly
  # before the corresponding transplant. An older prepared child loses shell
  # ownership when its enclosure is transplanted, so unrebased offside parents
  # that still point to it are templates, not live roots; they are readmitted
  # when their own turn rebases every dependency to an original. Once
  # transplanted, add the identity-preserved original to the committed roots and
  # repeat the joint validation. This closes the package-callback/rebase window
  # and proves that a successful return leaves the complete identity graph
  # current. R's `suspendInterrupts()` does not suppress pending finalizers: a
  # hostile external finalizer that mutates a selected root inside the R binding
  # wave is detected by the post-transplant capsule barriers or the final
  # allocation-free complete public-binding receipt, but that completed
  # transplant is not rolled back.
  for (position in seq_along(session$commit_order)) {
    index = session$commit_order[[position]]
    info = session$infos[[index]]
    dependencies = .upgrade_paradox_info_dependencies(info)
    dependency_indices = vapply(dependencies, function(dependency) {
      .upgrade_paradox_match_node(session$nodes, dependency)
    }, integer(1L))
    if (any(!dependency_indices)) {
      .upgrade_paradox_abort(
        session$paths[[index]],
        "internal graph traversal failure"
      )
    }
    originals = session$nodes[dependency_indices]
    names(originals) = names(dependencies)
    .upgrade_paradox_rebase_prepared(
      info,
      session$prepared[[index]],
      originals,
      session$paths[[index]]
    )
    .upgrade_paradox_validate_current_roots(c(
      committed_roots,
      list(session$prepared[[index]])
    ))
    base::suspendInterrupts(
      .upgrade_paradox_transplant(plans[[position]])
    )
    committed_roots[[length(committed_roots) + 1L]] =
      session$nodes[[index]]
    .upgrade_paradox_validate_current_roots(committed_roots)
  }
  # Keep graph and public-shell authentication in the same native operation.
  # After its allocation-capable graph construction, both receipt waves are
  # consecutive and allocation-free; this is the successful commit's terminal
  # expression.
  .Call(
    C_param_set_validate_current_roots,
    committed_roots,
    final_public_receipts
  )
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
#' them, language objects, S4 attributes, and ordinary containers. A native
#' direct-binding classifier distinguishes realized language/symbol values from
#' delayed promises without evaluating either. R 3.6--4.4 can also inspect a
#' detached promise. R 4.5 has no policy-compliant promise-inspection API, so
#' recursive migration fails closed on a reached promise and asks the caller to
#' migrate under R 4.0--4.4 or R >= 4.6. Ordinary callback factories can
#' retain formal promises in their lexical frames even when those formals are
#' already forced or unused, so this boundary does not require an explicit
#' `delayedAssign()`. R 4.6 and newer treat a detached promise reached
#' outside a binding or `...` cell as opaque. It does not enter
#' `.GlobalEnv`, package namespaces or package environments, authenticated
#' namespace-imports environments, attached search-path environments, or
#' base/empty environments. A namespace-imports boundary is recognized only
#' when the raw `name` attribute is an ordinary, attribute-free character scalar
#' beginning `imports:` and the direct parent is the base namespace; a merely
#' spoofed display name remains traversable. Arbitrary external-pointer and
#' weak-reference internals are opaque. The protected ordinary-R payload of an
#' authenticated current Paradox capsule is traversed so a legacy object stored
#' as an opaque parameter value is not missed.
#'
#' R 3.6 cannot retrieve an active binding's function. Exact built-in current
#' Paradox 2 shells are traversed through their authenticated native capsule,
#' but migration of a Paradox 1 ParamSet-family shell (including one nested in
#' another object) must be performed under R >= 4.0. Package active facades and
#' relocked method replacements on an exact built-in current shell are opaque:
#' these unsupported replacements cannot be distinguished from generated code
#' when every receipt available on R 3.6 remains unchanged. Their closures are
#' not traversed, and an active binding is never invoked. Additive shells and
#' modifications that fail exact authentication instead fail closed on R 3.6.
#' Standalone legacy `Domain` and `Condition` conversion has no such limitation.
#' Known package-generated Paradox 1 callback wrappers are authenticated
#' narrowly rather than treated as general object graphs. ParamSet children
#' captured by a detached collection transformation or constraint are migrated
#' as explicit dependencies, preserving shared identities. Other callback
#' environments are not traversed. During preparation, package-interpreted
#' callbacks follow `getOption("paradox.strip_srcrefs", TRUE)`. A detached
#' wrapper is nevertheless always rebuilt in a fresh environment so carrier
#' rebinding cannot mutate serialized input; disabling stripping preserves its
#' source metadata, not its wrapper identity. This source-reference
#' normalization does not alter opaque parameter payloads.
#'
#' All discovered nodes and registered owner migrations are semantically
#' inspected and rebuilt before the first legacy shell changes. Already-current
#' ParamSet-family shells are also validated during this preflight, although
#' they need no transplant. Current Shadows are checked against an authoritative
#' live preview without installing that preview. Every prepared/current root is
#' then validated jointly before the first transplant. Commit is post-order and
#' identity-preserving: after a child is current, its prepared parent is rebased
#' to the original child identity. All already-current identity roots plus that
#' newly rebased prepared root are jointly validated before the parent changes;
#' unrebased parents remain offside templates until their own turn. After each
#' transplant, the current identity roots are jointly validated again with the
#' transplanted original added. The enclosure swap is each shell's final
#' completion point. If an allocation failure interrupts an individual binding
#' wave, the old enclosure remains
#' authoritative and the mixed shell stays authenticated for retry; completed
#' nodes are valid current objects. Calling the function again completes the
#' remainder.
#'
#' Ordinary inspection, rebuilding, and package-callback failures happen before
#' mutation. Pending finalizers from unrelated user objects are not suspended by
#' R's interrupt guard. If such a finalizer mutates a selected Paradox root
#' during the binding wave, post-transplant joint capsule validation detects
#' topology/state changes. After its last allocating validation, one
#' allocation-free terminal receipt authenticates the exact complete public
#' binding and lock surface of every transplanted and already-current selected
#' shell whenever the graph contains a transplant. A graph containing only
#' current shells has no binding wave and returns after joint capsule
#' validation; this also keeps that no-op available on R 3.6, whose public API
#' cannot retrieve active-binding functions. A receipt mismatch errors, but a
#' completed transplant is not rolled back and retry is not promised for the
#' externally corrupted graph.
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
  discovery = .Call(C_upgrade_graph_discover, .upgrade_paradox_graph_root(x))
  if (!.upgrade_paradox_is_ordinary_list(discovery) ||
      !identical(names(discovery), c("objects", "paths")) ||
      !.upgrade_paradox_is_ordinary_list(discovery$objects) ||
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

.paradox_upgrade_legacy_first_use = function(self, expected_kind) {
  upgrade_paradox_object_graph(self)
  context = .paradox_gateway_current_context(self, expected_kind)
  if (!isTRUE(context$ok)) {
    stop(
      "Legacy Paradox first-use migration did not produce a current ParamSet shell.",
      call. = FALSE
    )
  }
  context
}

#' Upgrade a serialized Paradox object explicitly
#'
#' Paradox 2 does not execute objects serialized with the Paradox 1 private
#' layout. `upgrade_paradox_object()` validates a canonical built-in legacy
#' object and returns a newly constructed object using the current versioned
#' state capsule. Current capsule-backed parameter sets are returned unchanged.
#'
#' The upgrader accepts package-owned `ParamSet`, `ParamSetCollection`, built-in
#' `Domain`, and built-in `Condition` objects. It preserves callback behavior
#' and shared collection children without calling legacy methods or active
#' bindings. The input is never mutated. Cycles, malformed private state,
#' replaced core R6 methods, custom Domain or Condition classes, and
#' unregistered legacy third-party R6 subclasses are rejected with a
#' path-specific error. A current capsule-backed additive subclass is validated
#' and returned unchanged.
#' Legacy package-interpreted callbacks are normalized according to
#' `getOption("paradox.strip_srcrefs", TRUE)` during preparation; this
#' normalization does not modify opaque parameter values. Exact
#' package-generated callback wrappers can contribute captured ParamSet children
#' as migration dependencies; this does not traverse arbitrary callback
#' environments. Such a detached wrapper is always rebuilt with a fresh
#' environment before child rebinding; disabling source stripping preserves its
#' source metadata but not wrapper identity.
#' Direct owner subclasses covered by an exact
#' [register_paradox_object_upgrader()] bridge can also be rebuilt without
#' mutating the input; the recursive graph API uses the same bridge while
#' preserving the original shell identity.
#'
#' R 3.6 cannot safely retrieve an active binding's function. Current
#' capsule-backed objects are still returned unchanged, and standalone legacy
#' `Domain` and `Condition` objects can still be converted, but a legacy
#' ParamSet-family R6 object must be upgraded under R >= 4.0.
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
  if (is.environment(x)) {
    if (.Call(C_param_set_class_kind, x) %in% 1:3) {
      return(.upgrade_paradox_graph(x))
    }
    classes = .upgrade_paradox_class_snapshot(x, "x")
  } else {
    classes = .upgrade_paradox_class_snapshot(x, "x")
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
