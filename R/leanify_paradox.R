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

.paradox_leanify_member = function(generator, container, member, namespace) {
  method = generator[[container]][[member]]
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
  target_call = as.call(c(list(as.name(target_name)), arguments))
  # Keep the established public default expressions for reflection/source
  # compatibility while letting current targets distinguish omission from an
  # explicit container. Ordinary forwarding forces a default before the
  # target sees it. These two cold operations need omission to select current
  # values/IDs inside one native graph transaction.
  missing_argument = if (
      identical(member, "subspaces") &&
      generator$classname %in% c("ParamSet", "ParamSetShadow")
  ) {
    "ids"
  } else if (
      identical(member, "search_space") &&
      identical(generator$classname, "ParamSet")
  ) {
    "values"
  }
  if (!is.null(missing_argument)) {
    omitted_arguments = arguments[names(arguments) != missing_argument]
    target_call = call(
      "if",
      call("missing", as.name(missing_argument)),
      as.call(c(list(as.name(target_name)), omitted_arguments)),
      target_call
    )
  }
  stub = eval(call(
    "function",
    original_formals,
    target_call
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
      target = .paradox_leanify_member(generator, container, member, namespace)
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

.paradox_plain_binding_snapshot = function(owner, name) {
  .Call(C_plain_binding_snapshot, owner, name)
}

.paradox_gateway_current_context = function(self, expected_kind = 0L) {
  .Call(C_gateway_context_snapshot, self, expected_kind)
}

.paradox_gateway_current_core = function(self) {
  isTRUE(.paradox_gateway_current_context(self)$ok)
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

.paradox_legacy_gateway_context = function(self, old_name, gateway_kind) {
  context = .paradox_gateway_current_context(self, gateway_kind)
  if (!isTRUE(context$ok)) {
    if (!identical(.paradox_legacy_action(), "upgrade")) {
      .paradox_legacy_use_error(old_name)
    }
    # This hook performs an identity-preserving graph migration and rewires
    # every R6 enclosure slice, then returns one authenticated current context
    # snapshot. The still-lazy legacy `private` and `super` promises are never
    # evaluated.
    context = .paradox_upgrade_legacy_first_use(self, gateway_kind)
  }
  context
}

.paradox_legacy_gateway_target = function(
    old_name,
    current_name,
    namespace) {
  if (is.null(current_name)) {
    .paradox_retired_target_error(old_name)
  }
  get(current_name, envir = namespace, inherits = FALSE)
}

.paradox_legacy_stub_argument_missing = function(frame, argument) {
  # Historical lean stubs always pass every formal to their unversioned
  # namespace target, so ordinary forwarding loses whether the caller omitted
  # a default. `missing()` can inspect the still-active stub call frame without
  # forcing that default promise. A direct call to the package-private gateway
  # has no such formal and therefore takes the explicit path.
  tryCatch(
    isTRUE(eval(call("missing", as.name(argument)), envir = frame)),
    error = function(...) FALSE
  )
}

.paradox_make_legacy_gateway = function(
    old_name,
    current_name,
    namespace,
    gateway_kind,
    missing_argument = NULL) {
  force(old_name)
  force(current_name)
  force(namespace)
  force(gateway_kind)
  force(missing_argument)

  if (identical(missing_argument, "ids")) {
    return(function(self, private, super, ids, ...) {
      omitted = .paradox_legacy_stub_argument_missing(parent.frame(), "ids")
      context = .paradox_legacy_gateway_context(
        self,
        old_name,
        gateway_kind
      )
      private = context$private
      super = context$super
      target = .paradox_legacy_gateway_target(
        old_name,
        current_name,
        namespace
      )
      if (omitted) {
        target(self = self, private = private, super = super, ...)
      } else {
        target(
          self = self,
          private = private,
          super = super,
          ids = ids,
          ...
        )
      }
    })
  }

  if (identical(missing_argument, "values")) {
    return(function(self, private, super, values, ...) {
      omitted = .paradox_legacy_stub_argument_missing(
        parent.frame(),
        "values"
      )
      context = .paradox_legacy_gateway_context(
        self,
        old_name,
        gateway_kind
      )
      private = context$private
      super = context$super
      target = .paradox_legacy_gateway_target(
        old_name,
        current_name,
        namespace
      )
      if (omitted) {
        target(self = self, private = private, super = super, ...)
      } else {
        target(
          self = self,
          private = private,
          super = super,
          values = values,
          ...
        )
      }
    })
  }

  function(self, private, super, ...) {
    context = .paradox_legacy_gateway_context(
      self,
      old_name,
      gateway_kind
    )
    # Never forward the `private` and `super` promises supplied by the old
    # currently executing stub.  An identity-preserving migration replaces the
    # shell's enclosure slices; the detached old slice is intentionally left
    # inert. Replay the values rooted by the allocation-safe native context
    # snapshot instead of rereading the shell after authentication.
    private = context$private
    super = context$super
    target = .paradox_legacy_gateway_target(
      old_name,
      current_name,
      namespace
    )
    target(self = self, private = private, super = super, ...)
  }
}

# Serialized Design and Sampler stubs can hand an embedded Paradox-1 ParamSet
# to current code before any method on that ParamSet reaches a family gateway.
# Select the ordinary public field inertly: a malformed historical wrapper must
# not execute an active binding merely to choose the default diagnostic.
.paradox_legacy_embedded_param_set = function(self, old_name) {
  snapshot = .paradox_plain_binding_snapshot(self, "param_set")
  param_set = if (isTRUE(snapshot$ok)) snapshot$value
  if (!.paradox_gateway_current_core(param_set)) {
    if (!identical(.paradox_legacy_action(), "upgrade")) {
      .paradox_legacy_use_error(old_name)
    }
    upgrade_paradox_object_graph(self)
    snapshot = .paradox_plain_binding_snapshot(self, "param_set")
    param_set = if (isTRUE(snapshot$ok)) snapshot$value
    if (!.paradox_gateway_current_core(param_set)) {
      stop(
        "Legacy Paradox first-use migration did not produce a current ParamSet shell.",
        call. = FALSE
      )
    }
  }
  param_set
}

# Current shells call their versioned target directly; only the historical
# unversioned names enter this cold bridge. The containing Design/Sampler
# layout is compatible and does not need a transplant, while graph migration
# upgrades every nested ParamSet in place. A graph-healed historical shell
# keeps calling its unversioned name forever, so forwarding an already-current
# embedded graph is load-bearing.
.paradox_make_embedded_paramset_gateway = function(
    old_name,
    current_name,
    namespace) {
  force(old_name)
  force(current_name)
  force(namespace)
  function(self, private, super, ...) {
    .paradox_legacy_embedded_param_set(self, old_name)
    target = get(current_name, envir = namespace, inherits = FALSE)
    target(self = self, private = private, super = super, ...)
  }
}

# Paradox 1 serialized the formals of every R6 stub. The current
# Sampler1DRfun/Sampler1DCateg private sampling bodies pass the selected Domain
# fields to `Sampler1D$as_dt_col()` and, for rejection sampling, to
# `Sampler1DRfun$sample_truncated()`. Calling those operations through a
# historical shell would fail before namespace dispatch because the old stubs
# accept only `x` and `n, rfun`, respectively. These two unversioned targets
# therefore replay the current algorithms while calling the versioned
# lower-level targets directly. Current samplers call their versioned `.sample`
# targets and never pay for this bridge.
.paradox_make_legacy_sampler_1d_gateway = function(
    old_name,
    sampler_kind,
    namespace) {
  force(old_name)
  force(sampler_kind)
  force(namespace)
  as_dt_col = get(
    .paradox_lean_target_name("Sampler1D", "as_dt_col"),
    envir = namespace,
    inherits = FALSE
  )
  # Both generated bodies contain the syntactic call below. Keep its lexical
  # target callable in both closure environments so R's package usage analysis
  # can prove the symbol is resolved; the exact `sampler_kind` branch means the
  # categorical gateway never invokes it.
  sample_truncated = get(
    .paradox_lean_target_name("Sampler1DRfun", "sample_truncated"),
    envir = namespace,
    inherits = FALSE
  )

  function(self, private, super, n) {
    param = .paradox_legacy_embedded_param_set(self, old_name)
    domain = .Call(
      C_param_set_domains,
      get_private(param),
      param
    )[[1L]]
    storage_type = domain$storage_type[[1L]]
    id = domain$id[[1L]]

    if (identical(sampler_kind, "rfun")) {
      lower = domain$lower[[1L]]
      upper = domain$upper[[1L]]
      rfun = self$rfun
      trunc = self$trunc
      sampled = if (n == 0L) {
        numeric()
      } else if (trunc) {
        sample_truncated(
          self = self,
          private = private,
          super = super,
          n = n,
          rfun = rfun,
          lower = lower,
          upper = upper
        )
      } else {
        rfun(n = n)
      }
    } else {
      levels = domain$levels[[1L]]
      if (!length(levels)) {
        if (n != 0L) {
          stop(
            "Cannot sample a factor parameter with no levels",
            call. = FALSE
          )
        }
        sampled = character()
      } else {
        sampled = sample(
          levels,
          n,
          replace = TRUE,
          prob = self$prob
        )
      }
    }

    as_dt_col(
      self = self,
      private = private,
      super = super,
      x = sampled,
      storage_type = storage_type,
      id = id
    )
  }
}

.paradox_install_paramset_gateways = function(
    classname,
    members,
    targets,
    namespace,
    gateway_kind) {
  for (member in members) {
    old_name = .paradox_old_target_name(classname, member)
    current_name = unname(targets[old_name])
    if (!length(current_name) || is.na(current_name)) current_name = NULL
    missing_argument = if (
        identical(member, "subspaces") &&
        classname %in% c("ParamSet", "ParamSetShadow")) {
      "ids"
    } else if (
        identical(member, "search_space") &&
        identical(classname, "ParamSet")) {
      "values"
    }
    assign(
      old_name,
      .paradox_make_legacy_gateway(
        old_name,
        current_name,
        namespace,
        gateway_kind,
        missing_argument
      ),
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
  embedded_paramset_gateways = c(
    .paradox_old_target_name("Design", "transpose"),
    .paradox_old_target_name("Sampler", "sample")
  )
  legacy_sampler_1d_gateways = c(
    ".__Sampler1DRfun__.sample" = "rfun",
    ".__Sampler1DCateg__.sample" = "categ"
  )
  for (old_name in names(targets)) {
    if (!any(startsWith(old_name, family_old_prefixes))) {
      if (old_name %in% embedded_paramset_gateways) {
        # These non-ParamSet-family operations can reach native code without
        # first invoking a method on their embedded ParamSet.
        assign(
          old_name,
          .paradox_make_embedded_paramset_gateway(
            old_name,
            targets[[old_name]],
            namespace
          ),
          envir = namespace
        )
        next
      }
      if (old_name %in% names(legacy_sampler_1d_gateways)) {
        assign(
          old_name,
          .paradox_make_legacy_sampler_1d_gateway(
            old_name,
            legacy_sampler_1d_gateways[[old_name]],
            namespace
          ),
          envir = namespace
        )
        next
      }
      # Other Paradox R6 schemas did not undergo the ParamSet capsule
      # migration.  Their old stubs can call the current implementation
      # directly, without entering a migration gateway: the public field
      # layout of Design and the Sampler family is unchanged since Paradox 1,
      # so today's remaining method bodies read a serialized Paradox 1 shell
      # correctly. The two operations that can cross directly into native code
      # before touching that ParamSet were intercepted above.
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
    namespace,
    1L
  )
  .paradox_install_paramset_gateways(
    "ParamSetCollection",
    .paradox_legacy_collection_members,
    targets,
    namespace,
    2L
  )
  .paradox_install_paramset_gateways(
    "ParamSetShadow",
    .paradox_prerelease_shadow_members,
    targets,
    namespace,
    3L
  )
  invisible(NULL)
}
