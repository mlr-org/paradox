# The registry contains data only. In particular, it never retains a function
# recovered from a serialized object (or even a direct reference to an owner
# package function). Hooks are resolved by name in the currently loaded,
# authenticated owner namespace immediately before they are called.
.paradox_object_upgrader_registry = new.env(
  hash = TRUE,
  parent = emptyenv()
)

.paradox_registry_abort = function(message, ...) {
  stop(
    sprintf(message, ...),
    call. = FALSE
  )
}

.paradox_registry_scalar_string = function(x, argument) {
  if (typeof(x) != "character" || length(x) != 1L ||
      !is.null(attributes(x)) || is.na(x) || !nzchar(x) ||
      identical(Encoding(x), "bytes")) {
    .paradox_registry_abort(
      "`%s` must be one non-missing, attribute-free string",
      argument
    )
  }
  x
}

# checkmate-style "strict" syntactic R name, shared by the scalar and vector
# registry name validators.
.paradox_registry_name_pattern = "^(?:[A-Za-z]|\\.[A-Za-z_.])[A-Za-z0-9._]*$"

.paradox_registry_symbol = function(x, argument) {
  x = .paradox_registry_scalar_string(x, argument)
  if (!grepl(.paradox_registry_name_pattern, x, perl = TRUE)) {
    .paradox_registry_abort(
      "`%s` must be an ordinary syntactic R name",
      argument
    )
  }
  x
}

.paradox_registry_name_vector = function(
  x,
  argument,
  allow_empty = FALSE
) {
  if (typeof(x) != "character" || !is.null(attributes(x)) ||
      (!allow_empty && length(x) == 0L) || anyNA(x) ||
      any(!nzchar(x)) || any(Encoding(x) == "bytes")) {
    qualifier = if (allow_empty) "possibly empty " else ""
    .paradox_registry_abort(
      "`%s` must be a %sattribute-free character vector of names",
      argument,
      qualifier
    )
  }
  if (length(x) && any(!grepl(.paradox_registry_name_pattern, x, perl = TRUE))) {
    .paradox_registry_abort(
      "Every element of `%s` must be an ordinary syntactic R name",
      argument
    )
  }
  if (anyDuplicated(x)) {
    .paradox_registry_abort("`%s` must not contain duplicates", argument)
  }
  x
}

.paradox_registry_class = function(
  x,
  argument = "legacy_class",
  require_param_set = FALSE
) {
  x = .paradox_registry_name_vector(x, argument)
  if (require_param_set &&
      (length(x) != 3L ||
        !identical(x[2:3], c("ParamSet", "R6")) ||
        x[[1L]] %in% c("ParamSet", "ParamSetCollection", "R6"))) {
    .paradox_registry_abort(
      paste0(
        "`%s` must be exactly `c(<owner class>, \"ParamSet\", \"R6\")`; ",
        "built-in ParamSet and ParamSetCollection classes cannot be registered"
      ),
      argument
    )
  }
  x
}

.paradox_registry_class_key = function(legacy_class) {
  # Registered labels are restricted to syntactic names, so this separator
  # cannot collide with a label. Vector order remains significant.
  paste(legacy_class, collapse = "\037")
}

.paradox_registry_caller_namespace = function(frame) {
  while (is.environment(frame)) {
    if (isNamespace(frame)) return(frame)
    if (identical(frame, emptyenv())) break
    frame = parent.env(frame)
  }
  NULL
}

.paradox_registry_owner_namespace = function(
  owner_package,
  caller_frame
) {
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", owner_package) ||
      endsWith(owner_package, ".")) {
    .paradox_registry_abort(
      "`owner_package` must be a valid package name"
    )
  }
  if (!owner_package %in% loadedNamespaces()) {
    .paradox_registry_abort(
      "Owner package '%s' must be loaded before it registers an upgrader",
      owner_package
    )
  }

  namespace = getNamespace(owner_package)
  caller_namespace = .paradox_registry_caller_namespace(caller_frame)
  if (!identical(caller_namespace, namespace)) {
    .paradox_registry_abort(
      paste0(
        "An upgrader for package '%s' must be registered by code ",
        "lexically owned by that package namespace"
      ),
      owner_package
    )
  }
  namespace
}

.paradox_registry_function = function(namespace, name, argument) {
  if (!exists(name, envir = namespace, inherits = FALSE)) {
    .paradox_registry_abort(
      "`%s` names no binding in owner namespace '%s'",
      argument,
      getNamespaceName(namespace)
    )
  }
  value = get(name, envir = namespace, inherits = FALSE)
  if (!is.function(value)) {
    .paradox_registry_abort(
      "`%s` must name a function in owner namespace '%s'",
      argument,
      getNamespaceName(namespace)
    )
  }
  invisible(NULL)
}

.paradox_validate_object_upgrader_entry = function(entry, key = NULL) {
  expected_names = c(
    "owner_package",
    "legacy_class",
    "migration_kind",
    "inspector",
    "rebuilder",
    "retired_bindings",
    ".owner_namespace"
  )
  entry_attributes = attributes(entry)
  if (!.upgrade_paradox_is_ordinary_list(entry) ||
      !identical(names(entry_attributes), "names") ||
      !identical(
        attr(entry, "names", exact = TRUE),
        expected_names
      )) {
    .paradox_registry_abort("The Paradox object-upgrader registry is corrupt")
  }

  owner_package = .paradox_registry_scalar_string(
    entry$owner_package,
    "owner_package"
  )
  legacy_class = .paradox_registry_class(
    entry$legacy_class,
    require_param_set = TRUE
  )
  migration_kind = .paradox_registry_scalar_string(
    entry$migration_kind,
    "migration_kind"
  )
  if (!migration_kind %in% c("additive", "replacement")) {
    .paradox_registry_abort("The Paradox object-upgrader registry is corrupt")
  }
  .paradox_registry_symbol(entry$inspector, "inspector")
  .paradox_registry_symbol(entry$rebuilder, "rebuilder")
  retired_bindings = .paradox_registry_name_vector(
    entry$retired_bindings,
    "retired_bindings",
    allow_empty = TRUE
  )
  if (!identical(retired_bindings, sort(retired_bindings, method = "radix")) ||
      (identical(migration_kind, "additive") &&
        length(retired_bindings))) {
    .paradox_registry_abort("The Paradox object-upgrader registry is corrupt")
  }
  if (!is.environment(entry$.owner_namespace) ||
      !isNamespace(entry$.owner_namespace) ||
      !identical(
        environmentName(entry$.owner_namespace),
        owner_package
      )) {
    .paradox_registry_abort("The Paradox object-upgrader registry is corrupt")
  }
  if (!is.null(key) &&
      !identical(key, .paradox_registry_class_key(legacy_class))) {
    .paradox_registry_abort("The Paradox object-upgrader registry is corrupt")
  }
  invisible(entry)
}

#' Register a package-owned legacy ParamSet upgrader
#'
#' Packages that owned a supported Paradox 1 `ParamSet` subclass can register
#' a narrow migration bridge for [upgrade_paradox_object_graph()]. Registration
#' is intended to happen in the owner package's `.onLoad()` hook.
#'
#' `legacy_class` must have exactly the shape
#' `c(<owner class>, "ParamSet", "R6")` and is matched as that exact, ordered
#' vector. Deeper owner inheritance and the built-in base/collection classes
#' are deliberately unsupported. There is no S3 dispatch, superclass search,
#' or partial match. Registration must be called by code whose lexical
#' environment belongs to `owner_package`, and both hook names must resolve to
#' functions directly in that package's currently loaded namespace.
#'
#' The migration kind and the owner label must agree: a `"replacement"`
#' upgrader can be registered only for the exact vector
#' `c("ParamSetShadow", "ParamSet", "R6")`, and an `"additive"` upgrader never
#' for it. Every other combination is provably unsatisfiable -- the rebuilt
#' shell must carry the registered class, admission requires its class kind to
#' equal its capsule kind, and only the `"ParamSetShadow"` label classes as a
#' Shadow -- so registration refuses it up front instead of failing per object
#' at migration time.
#'
#' The inspector is called as `inspector(x)`. Paradox authenticates the common
#' R6 shell and package provenance; the inspector must authenticate any
#' owner-specific state it reads without calling a serialized method, active
#' binding, or callback, and return exactly
#' `list(state = <owner state>, dependencies = <named plain list>)`.
#' An `"additive"` inspector must return an empty named plain list: `base` is
#' the sole inherited ParamSet dependency and is a prepared current `ParamSet`
#' carrying the authenticated legacy BASE state. A `"replacement"` inspector
#' must return exactly `list(origin = <ParamSet-family environment>)`; Paradox
#' prepares that origin before it calls
#' `rebuilder(base, state, dependencies)`, with `base = NULL`.
#'
#' A rebuilder returns a current R6 shell. Paradox authenticates that result,
#' installs the prepared base capsule for an additive migration, and restores
#' the legacy shell's public `assert_values` policy for both migration kinds.
#' A replacement must return the exact registered class backed by a canonical
#' current `ParamSetShadow` capsule; Paradox rebinds its single origin while
#' preserving the legacy shell identity. The returned shell's public, private,
#' and enclosure environments must be fresh: before changing it, Paradox
#' rejects aliases with every original/current node in the complete migration
#' session and with every other prepared result. Shared dependency nodes
#' remain supported. Registered owner
#' classes with public or private R6 finalizers are unsupported because moving
#' a finalizer between environment identities can prematurely or repeatedly
#' release live state. Hook functions are trusted code from the currently
#' loaded owner namespace. Paradox stores and resolves their names, never a
#' function recovered from a serialized object.
#'
#' `retired_bindings` declares legacy public names for which a replacement
#' migration deliberately installs informative retired-API errors. Additive
#' migrations cannot retire bindings.
#'
#' Registration controls explicit and recursively discovered migration; it
#' cannot intercept a serialized method stub whose target name belongs to the
#' owner package. An owner package that historically leanified its subclass
#' methods must keep those old namespace target names as cold gateways. Each
#' gateway must give the same actionable default error, or invoke
#' [upgrade_paradox_object_graph()] and replay the requested operation when
#' `getOption("paradox.legacy_object_action")` is `"upgrade"`. Current objects
#' should bypass that migration path.
#'
#' @param owner_package (`character(1)`)\cr
#'   Package that owns the legacy class and both migration hooks.
#' @param legacy_class (`character()`)\cr
#'   Exact full class vector of the legacy ParamSet-family R6 shell.
#' @param migration_kind (`character(1)`)\cr
#'   Either `"additive"` or `"replacement"`.
#' @param inspector (`character(1)`)\cr
#'   Namespace-local name of the read-only inspection function.
#' @param rebuilder (`character(1)`)\cr
#'   Namespace-local name of the current-shell rebuilding function.
#' @param retired_bindings (`character()`)\cr
#'   Exact public bindings retired by a replacement migration.
#'
#' @return `NULL`, invisibly.
#' @export
register_paradox_object_upgrader = function(
  owner_package,
  legacy_class,
  migration_kind = "additive",
  inspector,
  rebuilder,
  retired_bindings = character()
) {
  owner_package = .paradox_registry_scalar_string(
    owner_package,
    "owner_package"
  )
  legacy_class = .paradox_registry_class(
    legacy_class,
    require_param_set = TRUE
  )
  migration_kind = .paradox_registry_scalar_string(
    migration_kind,
    "migration_kind"
  )
  if (!migration_kind %in% c("additive", "replacement")) {
    .paradox_registry_abort(
      "`migration_kind` must be exactly \"additive\" or \"replacement\""
    )
  }
  # The owner migration composes three checks no registration can escape: the
  # rebuilt shell must carry the exact registered class, graph admission
  # accepts a shell only when its class kind equals its capsule kind, and
  # `c(<owner>, "ParamSet", "R6")` classes as a Shadow exactly when <owner> is
  # "ParamSetShadow". A replacement result must carry a Shadow capsule and an
  # additive result receives the prepared BASE capsule, so each migration kind
  # is satisfiable for exactly one side of that label test. Refuse the two
  # provably dead registrations here instead of letting every migrated object
  # fail later with a capsule-corruption error.
  shadow_label = identical(legacy_class[[1L]], "ParamSetShadow")
  if (identical(migration_kind, "replacement") && !shadow_label) {
    .paradox_registry_abort(
      paste0(
        "A replacement upgrader can only be registered for the legacy class ",
        "vector c(\"ParamSetShadow\", \"ParamSet\", \"R6\"): any other ",
        "registered class classes as a plain ParamSet shell, while a ",
        "replacement result must carry a current ParamSetShadow capsule"
      )
    )
  }
  if (identical(migration_kind, "additive") && shadow_label) {
    .paradox_registry_abort(
      paste0(
        "An additive upgrader cannot be registered for the legacy class ",
        "vector c(\"ParamSetShadow\", \"ParamSet\", \"R6\"): the rebuilt ",
        "shell classes as a ParamSetShadow, while an additive migration ",
        "installs the prepared base ParamSet capsule"
      )
    )
  }
  inspector = .paradox_registry_symbol(inspector, "inspector")
  rebuilder = .paradox_registry_symbol(rebuilder, "rebuilder")
  retired_bindings = .paradox_registry_name_vector(
    retired_bindings,
    "retired_bindings",
    allow_empty = TRUE
  )
  if (identical(migration_kind, "additive") &&
      length(retired_bindings)) {
    .paradox_registry_abort(
      "An additive upgrader cannot retire public bindings"
    )
  }
  retired_bindings = sort(retired_bindings, method = "radix")

  namespace = .paradox_registry_owner_namespace(
    owner_package,
    parent.frame()
  )
  .paradox_registry_function(namespace, inspector, "inspector")
  .paradox_registry_function(namespace, rebuilder, "rebuilder")

  key = .paradox_registry_class_key(legacy_class)
  existing = get0(
    key,
    envir = .paradox_object_upgrader_registry,
    inherits = FALSE
  )
  if (!is.null(existing)) {
    .paradox_validate_object_upgrader_entry(existing, key)
    if (!identical(existing$owner_package, owner_package)) {
      .paradox_registry_abort(
        paste0(
          "Legacy class vector <%s> overlaps an upgrader already ",
          "registered by package '%s'"
        ),
        paste(legacy_class, collapse = ", "),
        existing$owner_package
      )
    }
    if (identical(existing$.owner_namespace, namespace)) {
      .paradox_registry_abort(
        "Package '%s' already registered legacy class vector <%s>",
        owner_package,
        paste(legacy_class, collapse = ", ")
      )
    }
    # A namespace can be unloaded and loaded again while Paradox stays loaded.
    # The old registration cannot be invoked and is replaced only by the new
    # incarnation of the same authenticated owner package.
  }

  entry = list(
    owner_package = owner_package,
    legacy_class = legacy_class,
    migration_kind = migration_kind,
    inspector = inspector,
    rebuilder = rebuilder,
    retired_bindings = retired_bindings,
    .owner_namespace = namespace
  )
  .paradox_validate_object_upgrader_entry(entry, key)
  assign(
    key,
    entry,
    envir = .paradox_object_upgrader_registry
  )
  invisible(NULL)
}

.paradox_lookup_object_upgrader = function(legacy_class) {
  legacy_class = .paradox_registry_class(
    legacy_class,
    argument = "legacy_class"
  )
  key = .paradox_registry_class_key(legacy_class)
  entry = get0(
    key,
    envir = .paradox_object_upgrader_registry,
    inherits = FALSE
  )
  if (is.null(entry)) return(NULL)

  .paradox_validate_object_upgrader_entry(entry, key)
  if (!entry$owner_package %in% loadedNamespaces() ||
      !identical(
        getNamespace(entry$owner_package),
        entry$.owner_namespace
      )) {
    .paradox_registry_abort(
      paste0(
        "Owner package '%s' must be loaded in the namespace incarnation ",
        "that registered the upgrader for <%s>"
      ),
      entry$owner_package,
      paste(entry$legacy_class, collapse = ", ")
    )
  }
  entry
}

.paradox_registry_require_current = function(entry) {
  registered = .paradox_lookup_object_upgrader(entry$legacy_class)
  if (is.null(registered) || !identical(registered, entry)) {
    .paradox_registry_abort(
      "Object-upgrader metadata is not the currently registered entry"
    )
  }
  invisible(entry)
}

.paradox_resolve_object_upgrader_function = function(
  entry,
  which
) {
  .paradox_validate_object_upgrader_entry(entry)
  which = .paradox_registry_scalar_string(which, "which")
  if (!which %in% c("inspector", "rebuilder")) {
    .paradox_registry_abort(
      "`which` must be exactly \"inspector\" or \"rebuilder\""
    )
  }
  .paradox_registry_require_current(entry)

  namespace = getNamespace(entry$owner_package)
  name = entry[[which]]
  .paradox_registry_function(namespace, name, which)
  get(name, envir = namespace, inherits = FALSE)
}

.paradox_validate_object_upgrader_inspection = function(
  inspection,
  entry
) {
  .paradox_validate_object_upgrader_entry(entry)
  .paradox_registry_require_current(entry)
  inspection_attributes = attributes(inspection)
  inspection_names = attr(inspection, "names", exact = TRUE)
  if (!.upgrade_paradox_is_ordinary_list(inspection) ||
      !identical(names(inspection_attributes), "names") ||
      !identical(
        inspection_names,
        c("state", "dependencies")
      )) {
    .paradox_registry_abort(
      paste0(
        "Inspector '%s::%s' must return exactly ",
        "`list(state = ..., dependencies = ...)`"
      ),
      entry$owner_package,
      entry$inspector
    )
  }

  dependencies = inspection$dependencies
  dependency_attributes = attributes(dependencies)
  dependency_names = attr(dependencies, "names", exact = TRUE)
  if (!.upgrade_paradox_is_ordinary_list(dependencies) ||
      !identical(names(dependency_attributes), "names") ||
      typeof(dependency_names) != "character" ||
      !is.null(attributes(dependency_names)) ||
      length(dependency_names) != length(dependencies) ||
      anyNA(dependency_names) || any(!nzchar(dependency_names)) ||
      any(Encoding(dependency_names) == "bytes") ||
      anyDuplicated(dependency_names)) {
    .paradox_registry_abort(
      paste0(
        "Inspector '%s::%s' must return dependencies as a ",
        "uniquely named plain list"
      ),
      entry$owner_package,
      entry$inspector
    )
  }
  valid_dependency = vapply(
    dependencies,
    function(dependency) {
      if (!is.environment(dependency)) return(FALSE)
      classes = attr(dependency, "class", exact = TRUE)
      typeof(classes) == "character" &&
        length(classes) > 0L &&
        is.null(attributes(classes)) &&
        !anyNA(classes) &&
        any(classes == "ParamSet") &&
        identical(classes[[length(classes)]], "R6")
    },
    logical(1L)
  )
  if (!all(valid_dependency)) {
    .paradox_registry_abort(
      paste0(
        "Inspector '%s::%s' returned a dependency that is not a ",
        "ParamSet-family R6 environment"
      ),
      entry$owner_package,
      entry$inspector
    )
  }
  if (identical(entry$migration_kind, "additive") &&
      length(dependencies)) {
    .paradox_registry_abort(
      paste0(
        "Additive inspector '%s::%s' must return an empty named ",
        "dependency list"
      ),
      entry$owner_package,
      entry$inspector
    )
  }
  if (identical(entry$migration_kind, "replacement") &&
      !identical(dependency_names, "origin")) {
    .paradox_registry_abort(
      paste0(
        "Replacement inspector '%s::%s' must return exactly one ",
        "dependency named `origin`"
      ),
      entry$owner_package,
      entry$inspector
    )
  }
  inspection
}

# Stable package-internal integration names used by the migration session.
# They are also the seam the package's own tests rebind to mock registry
# resolution; keep the names stable even though they merely forward.
.paradox_object_upgrader_lookup = function(classes) {
  .paradox_lookup_object_upgrader(classes)
}

.paradox_object_upgrader_resolve = function(entry, which) {
  .paradox_resolve_object_upgrader_function(entry, which)
}

.paradox_registered_object_upgraders = function() {
  keys = sort(
    ls(.paradox_object_upgrader_registry, all.names = TRUE),
    method = "radix"
  )
  lapply(keys, function(key) {
    entry = get(
      key,
      envir = .paradox_object_upgrader_registry,
      inherits = FALSE
    )
    .paradox_validate_object_upgrader_entry(entry, key)
    entry
  })
}
