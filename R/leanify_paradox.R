# R6 method bodies are kept once in the package namespace.  New Paradox 2
# shells call a versioned namespace target directly; the unversioned target
# names remain reserved for serialized objects made by Paradox 1 and by
# pre-release Paradox 2 builds.

.paradox_lean_target_name = function(classname, member) {
  sprintf(".__paradox2_%s__%s", classname, member)
}

.paradox_old_target_name = function(classname, member) {
  sprintf(".__%s__%s", classname, member)
}

.paradox_leanify_member = function(generator, member, namespace) {
  container = NULL
  method = NULL
  for (candidate in c("public_methods", "private_methods", "active")) {
    method = generator[[candidate]][[member]]
    if (!is.null(method)) {
      container = candidate
      break
    }
  }
  if (is.null(method)) {
    stop(
      sprintf(
        "Internal error: cannot find R6 member `%s` in class `%s`",
        member,
        generator$classname
      ),
      call. = FALSE
    )
  }

  original_formals = formals(method)
  original_attributes = attributes(method)
  formals(method) = c(
    pairlist(self = substitute(), private = substitute(), super = substitute()),
    original_formals
  )
  attributes(method) = original_attributes

  target_name = .paradox_lean_target_name(generator$classname, member)
  assign(target_name, method, envir = namespace)

  arguments = lapply(names(formals(method)), as.name)
  names(arguments) = names(formals(method))
  stub = eval(call(
    "function",
    original_formals,
    as.call(c(list(as.name(target_name)), arguments))
  ))
  environment(stub) = environment(method)
  original_attributes$srcref = NULL
  attributes(stub) = original_attributes

  kind = switch(
    container,
    public_methods = "public",
    private_methods = "private",
    active = "active"
  )
  generator$set(kind, member, stub, overwrite = TRUE)
  invisible(target_name)
}

.paradox_leanify_generator = function(generator, namespace) {
  targets = character()
  for (container in c("public_methods", "private_methods", "active")) {
    for (member in names(generator[[container]])) {
      target = .paradox_leanify_member(generator, member, namespace)
      targets[[.paradox_old_target_name(generator$classname, member)]] = target
    }
  }
  targets
}

# These are serialized target names, and therefore a compatibility ledger
# rather than a reflection of whichever members happen to exist today.
.paradox_legacy_paramset_members = c(
  ".get_values", ".state", ".store_values", "add_dep",
  "aggr_internal_tuned_values", "all_bounded", "all_categorical",
  "all_numeric", "assert", "assert_dt", "check", "check_dependencies",
  "check_dt", "class", "clone", "constraint",
  "convert_internal_search_space", "data", "deep_clone", "default", "deps",
  "disable_internal_tuning", "domains", "extra_trafo", "flatten", "format",
  "get_domain", "get_tune_ps", "get_values", "has_constraint", "has_deps",
  "has_extra_trafo", "has_trafo", "has_trafo_param", "ids", "initialize",
  "is_bounded", "is_categ", "is_empty", "is_logscale", "is_number",
  "length", "levels", "lower", "nlevels", "params", "print", "qunif",
  "search_space", "set_values", "special_vals", "storage_type", "subset",
  "subspaces", "tags", "test", "test_constraint", "test_constraint_dt",
  "test_dt", "trafo", "upper", "values"
)

.paradox_legacy_collection_members = c(
  ".add_name_prefix", ".children_with_constraints",
  ".children_with_trafos", ".constraint_explicit",
  ".extra_trafo_explicit", ".get_constraint_detached",
  ".get_extra_trafo_detached", ".get_values", ".store_values", "add",
  "clone", "constraint", "convert_internal_search_space", "deep_clone",
  "deps", "disable_internal_tuning", "extra_trafo", "flatten",
  "initialize", "params", "sets", "subset"
)

# ParamSetShadow first appeared during Paradox 2 development.  These
# unversioned names occur in pre-release serialized objects even though no
# Paradox 1 release supplied the class.
.paradox_prerelease_shadow_members = c(
  ".get_values", ".origin", "add_dep", "clone", "constraint",
  "deep_clone", "deps", "domains", "extra_trafo", "get_domain",
  "has_constraint", "initialize", "origin", "params", "subset",
  "subspaces", "tags"
)

.paradox_gateway_current_core = function(self) {
  inert_binding = function(owner, name) {
    if (!is.environment(owner) ||
        !exists(name, envir = owner, inherits = FALSE) ||
        bindingIsActive(name, owner)) {
      return(NULL)
    }
    value = eval(call("substitute", as.name(name), owner), envir = baseenv())
    if (is.language(value) || is.symbol(value)) NULL else value
  }

  if (!is.environment(self) ||
      !exists(".__enclos_env__", envir = self, inherits = FALSE) ||
      bindingIsActive(".__enclos_env__", self)) {
    return(FALSE)
  }
  enclosure = inert_binding(self, ".__enclos_env__")
  private = inert_binding(enclosure, "private")
  core = inert_binding(private, ".core")
  if (!is.environment(enclosure) || !is.environment(private) ||
      is.null(core)) return(FALSE)
  isTRUE(tryCatch({
    kind = .Call(C_param_set_core_kind, core)
    kind %in% 1:3
  }, error = function(...) FALSE))
}

.paradox_legacy_action = function() {
  action = getOption("paradox.legacy_object_action", "error")
  if (!is.character(action) || length(action) != 1L || is.na(action) ||
      !action %in% c("error", "upgrade")) {
    stop(
      "`paradox.legacy_object_action` must be exactly \"error\" or \"upgrade\".",
      call. = FALSE
    )
  }
  action
}

.paradox_legacy_use_error = function(target_name) {
  stop(
    sprintf(
      paste0(
        "A serialized Paradox 1 object tried to call `%s`. ",
        "Upgrade the containing object with ",
        "`upgrade_paradox_object_graph(x)`. To perform this migration ",
        "silently on first use, set ",
        "`options(paradox.legacy_object_action = \"upgrade\")`."
      ),
      target_name
    ),
    call. = FALSE
  )
}

.paradox_retired_target_error = function(target_name) {
  stop(
    sprintf(
      paste0(
        "The serialized object called the retired internal Paradox target ",
        "`%s`. Its ParamSet graph has been upgraded, but this unsupported ",
        "private operation no longer exists."
      ),
      target_name
    ),
    call. = FALSE
  )
}

.paradox_make_legacy_gateway = function(old_name, current_name, namespace) {
  force(old_name)
  force(current_name)
  force(namespace)
  function(self, private, super, ...) {
    current_core = .paradox_gateway_current_core(self)
    if (!current_core) {
      if (!identical(.paradox_legacy_action(), "upgrade")) {
        .paradox_legacy_use_error(old_name)
      }
      # This hook performs an identity-preserving graph migration and rewires
      # every R6 enclosure slice.  In particular, the still-lazy `private` and
      # `super` promises below resolve against the refreshed enclosure.
      .paradox_upgrade_legacy_first_use(self)
    }

    # Never forward the `private` and `super` promises supplied by the old
    # currently executing stub.  An identity-preserving migration replaces the
    # shell's enclosure slices; the detached old slice is intentionally left
    # inert.  Resolve the current context from the canonical shell instead.
    current_enclosure = get(
      ".__enclos_env__",
      envir = self,
      inherits = FALSE
    )
    private = get("private", envir = current_enclosure, inherits = FALSE)
    super = get0(
      "super",
      envir = current_enclosure,
      inherits = FALSE,
      ifnotfound = NULL
    )

    if (is.null(current_name)) {
      .paradox_retired_target_error(old_name)
    }
    target = get(current_name, envir = namespace, inherits = FALSE)
    target(self = self, private = private, super = super, ...)
  }
}

.paradox_install_paramset_gateways = function(
    classname,
    members,
    targets,
    namespace) {
  for (member in members) {
    old_name = .paradox_old_target_name(classname, member)
    current_name = unname(targets[old_name])
    if (!length(current_name) || is.na(current_name)) current_name = NULL
    assign(
      old_name,
      .paradox_make_legacy_gateway(old_name, current_name, namespace),
      envir = namespace
    )
  }
  invisible(NULL)
}

.paradox_leanify_package = function(namespace = parent.frame()) {
  if (nzchar(Sys.getenv("ROXYGEN_PKG"))) {
    return(invisible(NULL))
  }

  generators = Filter(
    R6::is.R6Class,
    mget(ls(namespace, all.names = TRUE), envir = namespace, inherits = FALSE)
  )
  target_maps = lapply(
    generators,
    .paradox_leanify_generator,
    namespace = namespace
  )
  targets = do.call(c, unname(target_maps))

  family_old_prefixes = c(
    ".__ParamSet__",
    ".__ParamSetCollection__",
    ".__ParamSetShadow__"
  )
  for (old_name in names(targets)) {
    if (!any(startsWith(old_name, family_old_prefixes))) {
      # Other Paradox R6 schemas did not undergo the ParamSet capsule
      # migration.  Their old stubs can call the current implementation
      # directly, without entering a migration gateway.
      assign(
        old_name,
        get(targets[[old_name]], envir = namespace, inherits = FALSE),
        envir = namespace
      )
    }
  }

  .paradox_install_paramset_gateways(
    "ParamSet",
    .paradox_legacy_paramset_members,
    targets,
    namespace
  )
  .paradox_install_paramset_gateways(
    "ParamSetCollection",
    .paradox_legacy_collection_members,
    targets,
    namespace
  )
  .paradox_install_paramset_gateways(
    "ParamSetShadow",
    .paradox_prerelease_shadow_members,
    targets,
    namespace
  )
  invisible(NULL)
}
