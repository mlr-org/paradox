registry_call_from = function(namespace, register, ...) {
  caller = function(
    register,
    owner_package,
    legacy_class,
    migration_kind,
    inspector,
    rebuilder,
    retired_bindings
  ) {
    register(
      owner_package = owner_package,
      legacy_class = legacy_class,
      migration_kind = migration_kind,
      inspector = inspector,
      rebuilder = rebuilder,
      retired_bindings = retired_bindings
    )
  }
  environment(caller) = namespace
  caller(register, ...)
}

test_that("owner upgrader registration has exact namespace provenance", {
  register = get(
    "register_paradox_object_upgrader",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  legacy_class = c("RegistryProbeParamSet", "ParamSet", "R6")

  if (is.null(paradox:::.paradox_lookup_object_upgrader(legacy_class))) {
    expect_invisible(registry_call_from(
      asNamespace("base"),
      register,
      "base",
      legacy_class,
      "additive",
      "identity",
      "identity",
      character()
    ))
  }

  entry = paradox:::.paradox_lookup_object_upgrader(legacy_class)
  expect_identical(entry$owner_package, "base")
  expect_identical(entry$legacy_class, legacy_class)
  expect_identical(entry$migration_kind, "additive")
  expect_identical(entry$retired_bindings, character())
  expect_identical(
    paradox:::.paradox_resolve_object_upgrader_function(
      entry,
      "inspector"
    ),
    base::identity
  )

  expect_error(
    registry_call_from(
      asNamespace("base"),
      register,
      "base",
      legacy_class,
      "additive",
      "identity",
      "identity",
      character()
    ),
    "already registered",
    fixed = TRUE
  )
  expect_error(
    registry_call_from(
      asNamespace("stats"),
      register,
      "stats",
      legacy_class,
      "additive",
      "model.frame",
      "model.matrix",
      character()
    ),
    "overlaps an upgrader",
    fixed = TRUE
  )
  expect_error(
    register(
      "base",
      c("UnownedRegistryProbe", "ParamSet", "R6"),
      "additive",
      "identity",
      "identity"
    ),
    "lexically owned",
    fixed = TRUE
  )
})

test_that("registry input is narrow and deterministic", {
  register = get(
    "register_paradox_object_upgrader",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  # The one satisfiable replacement class vector; any other owner label is
  # refused for "replacement" below.
  legacy_class = c("ParamSetShadow", "ParamSet", "R6")

  if (is.null(paradox:::.paradox_lookup_object_upgrader(legacy_class))) {
    expect_invisible(registry_call_from(
      asNamespace("base"),
      register,
      "base",
      legacy_class,
      "replacement",
      "identity",
      "identity",
      c("z_retired", "a_retired")
    ))
  }
  entry = paradox:::.paradox_lookup_object_upgrader(legacy_class)
  expect_identical(
    entry$retired_bindings,
    c("a_retired", "z_retired")
  )

  expect_error(
    registry_call_from(
      asNamespace("base"),
      register,
      "base",
      c("RegistryReplacementParamSet", "ParamSet", "R6"),
      "replacement",
      "identity",
      "identity",
      character()
    ),
    "A replacement upgrader can only be registered for the legacy class",
    fixed = TRUE
  )
  expect_error(
    registry_call_from(
      asNamespace("stats"),
      register,
      "stats",
      c("ParamSetShadow", "ParamSet", "R6"),
      "additive",
      "model.frame",
      "model.matrix",
      character()
    ),
    "An additive upgrader cannot be registered for the legacy class",
    fixed = TRUE
  )

  expect_error(
    registry_call_from(
      asNamespace("base"),
      register,
      "base",
      c("BadDuplicateClass", "ParamSet", "ParamSet", "R6"),
      "additive",
      "identity",
      "identity",
      character()
    ),
    "must not contain duplicates",
    fixed = TRUE
  )
  expect_error(
    registry_call_from(
      asNamespace("base"),
      register,
      "base",
      c("NotAParamSet", "R6"),
      "additive",
      "identity",
      "identity",
      character()
    ),
    "must be exactly",
    fixed = TRUE
  )
  expect_error(
    registry_call_from(
      asNamespace("base"),
      register,
      "base",
      c("TooDeepParamSet", "OwnerBase", "ParamSet", "R6"),
      "additive",
      "identity",
      "identity",
      character()
    ),
    "must be exactly",
    fixed = TRUE
  )
  expect_error(
    registry_call_from(
      asNamespace("base"),
      register,
      "base",
      c("BadKindParamSet", "ParamSet", "R6"),
      "later",
      "identity",
      "identity",
      character()
    ),
    "exactly \"additive\" or \"replacement\"",
    fixed = TRUE
  )
  expect_error(
    registry_call_from(
      asNamespace("base"),
      register,
      "base",
      c("BadRetirementParamSet", "ParamSet", "R6"),
      "additive",
      "identity",
      "identity",
      "old_name"
    ),
    "cannot retire",
    fixed = TRUE
  )
  expect_error(
    registry_call_from(
      asNamespace("base"),
      register,
      "base",
      c("MissingHookParamSet", "ParamSet", "R6"),
      "additive",
      ".paradox_missing_inspector",
      "identity",
      character()
    ),
    "names no binding",
    fixed = TRUE
  )

  entries = paradox:::.paradox_registered_object_upgraders()
  keys = vapply(
    entries,
    function(candidate) {
      paradox:::.paradox_registry_class_key(candidate$legacy_class)
    },
    character(1L)
  )
  expect_identical(keys, sort(keys, method = "radix"))
})

test_that("owner inspection has a migration-specific inert envelope", {
  additive_entry = paradox:::.paradox_lookup_object_upgrader(
    c("RegistryProbeParamSet", "ParamSet", "R6")
  )
  replacement_entry = paradox:::.paradox_lookup_object_upgrader(
    c("ParamSetShadow", "ParamSet", "R6")
  )
  dependency = new.env(parent = emptyenv())
  attr(dependency, "class") = c("ParamSet", "R6")
  additive_inspection = list(
    state = list(hidden = "x"),
    dependencies = structure(list(), names = character())
  )
  expect_identical(
    paradox:::.paradox_validate_object_upgrader_inspection(
      additive_inspection,
      additive_entry
    ),
    additive_inspection
  )

  replacement_inspection = list(
    state = list(hidden = "x"),
    dependencies = setNames(list(dependency), "origin")
  )
  expect_identical(
    paradox:::.paradox_validate_object_upgrader_inspection(
      replacement_inspection,
      replacement_entry
    ),
    replacement_inspection
  )

  expect_error(
    paradox:::.paradox_validate_object_upgrader_inspection(
      unname(additive_inspection),
      additive_entry
    ),
    "must return exactly",
    fixed = TRUE
  )
  expect_error(
    paradox:::.paradox_validate_object_upgrader_inspection(
      list(
        state = NULL,
        dependencies = list(origin = dependency, origin = dependency)
      ),
      replacement_entry
    ),
    "uniquely named plain list",
    fixed = TRUE
  )
  expect_error(
    paradox:::.paradox_validate_object_upgrader_inspection(
      list(
        state = NULL,
        dependencies = list(origin = list())
      ),
      replacement_entry
    ),
    "not a ParamSet-family",
    fixed = TRUE
  )
  expect_error(
    paradox:::.paradox_validate_object_upgrader_inspection(
      replacement_inspection,
      additive_entry
    ),
    "must return an empty named dependency list",
    fixed = TRUE
  )
  expect_error(
    paradox:::.paradox_validate_object_upgrader_inspection(
      list(
        state = NULL,
        dependencies = structure(list(), names = character())
      ),
      replacement_entry
    ),
    "exactly one dependency named `origin`",
    fixed = TRUE
  )
  expect_error(
    paradox:::.paradox_validate_object_upgrader_inspection(
      list(
        state = NULL,
        dependencies = list(source = dependency)
      ),
      replacement_entry
    ),
    "exactly one dependency named `origin`",
    fixed = TRUE
  )
})

test_that("registered owner migration rejects R6 finalizer surfaces", {
  PublicFinalizerBase = suppressMessages(suppressWarnings(R6::R6Class(
    "RegistryPublicFinalizerBase",
    public = list(finalize = function() invisible(NULL))
  )))
  PublicFinalizer = R6::R6Class(
    "RegistryPublicFinalizer",
    inherit = PublicFinalizerBase
  )
  public = suppressMessages(suppressWarnings(PublicFinalizer$new()))
  expect_error(
    paradox:::.upgrade_paradox_reject_owner_finalizer(
      public,
      list(private = new.env(parent = emptyenv())),
      "x"
    ),
    "R6 finalizer",
    fixed = TRUE
  )

  PrivateFinalizerBase = R6::R6Class(
    "RegistryPrivateFinalizerBase",
    private = list(finalize = function() invisible(NULL))
  )
  PrivateFinalizer = R6::R6Class(
    "RegistryPrivateFinalizer",
    inherit = PrivateFinalizerBase
  )
  private = PrivateFinalizer$new()
  expect_error(
    paradox:::.upgrade_paradox_reject_owner_finalizer(
      private,
      list(private = private$.__enclos_env__$private),
      "x"
    ),
    "R6 finalizer",
    fixed = TRUE
  )

  observed = 0L
  ActiveFinalizer = R6::R6Class(
    "RegistryActiveFinalizer",
    active = list(finalize = function(value) {
      observed <<- observed + 1L
      invisible(NULL)
    })
  )
  active = ActiveFinalizer$new()
  observed_before_rejection = observed
  expect_error(
    paradox:::.upgrade_paradox_reject_owner_finalizer(
      active,
      list(private = new.env(parent = emptyenv())),
      "x"
    ),
    "R6 finalizer",
    fixed = TRUE
  )
  expect_identical(observed, observed_before_rejection)
})
