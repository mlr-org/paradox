graph_candidate = function(label) {
  candidate = new.env(parent = emptyenv())
  candidate$label = label
  class(candidate) = c("ParamSet", "R6")
  candidate
}

discover_upgrade_candidates = function(x) {
  .Call(paradox:::C_upgrade_graph_discover, x)
}

same_environment_set = function(actual, expected) {
  length(actual) == length(expected) &&
    all(vapply(expected, function(target) {
      sum(vapply(actual, identical, logical(1L), y = target)) == 1L
    }, logical(1L)))
}

test_that("current ParamSet stubs bypass historical migration gateways", {
  parameter_set = ps(x = p_dbl())
  stub = paste(deparse(body(parameter_set$ids)), collapse = "\n")
  expect_match(stub, ".__paradox2_ParamSet__ids", fixed = TRUE)
  expect_false(grepl(".__ParamSet__ids", stub, fixed = TRUE))

  namespace = asNamespace("paradox")
  expect_true(exists(
    ".__paradox2_ParamSet__ids",
    envir = namespace,
    inherits = FALSE
  ))
  gateway = get(
    ".__ParamSet__ids",
    envir = namespace,
    inherits = FALSE
  )
  expect_match(
    paste(deparse(body(gateway)), collapse = "\n"),
    ".paradox_upgrade_legacy_first_use",
    fixed = TRUE
  )
})

test_that("pre-release Shadow gateways directly replay current capsules", {
  shadow = ParamSetShadow$new(
    ps(visible = p_dbl(0, 1), hidden = p_lgl()),
    "hidden"
  )
  gateway = get(
    ".__ParamSetShadow__clone",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  old = options(paradox.legacy_object_action = "error")
  on.exit(options(old), add = TRUE)

  clone = gateway(
    self = shadow,
    private = stop("detached private promise was forced"),
    super = stop("detached super promise was forced"),
    deep = FALSE
  )
  expect_s3_class(clone, "ParamSetShadow")
  expect_false(identical(clone, shadow))
  expect_identical(clone$ids(), shadow$ids())
  expect_identical(clone$origin, shadow$origin)
})

test_that("legacy gateways reject borrowed current enclosures inertly", {
  current = ps(x = p_dbl())
  current_context = paradox:::.paradox_gateway_current_context(current)
  expect_identical(
    names(current_context),
    c(
      "ok", "enclosure", "private", "super", "core", "class",
      "assert_values"
    )
  )
  expect_true(current_context$ok)
  expect_identical(current_context$enclosure$self, current)
  expect_identical(
    current_context$private,
    current$.__enclos_env__$private
  )
  expect_identical(current_context$core, current_context$private$.core)
  expect_identical(current_context$class, class(current))
  expect_identical(current_context$assert_values, TRUE)
  expect_null(current_context$super)

  borrowed = new.env(parent = emptyenv())
  class(borrowed) = class(current)
  borrowed$.__enclos_env__ = current$.__enclos_env__

  mismatched = new.env(parent = emptyenv())
  class(mismatched) = c("ParamSetCollection", "ParamSet", "R6")
  mismatched_enclosure = new.env(parent = emptyenv())
  mismatched_enclosure$self = mismatched
  mismatched_enclosure$private = current$.__enclos_env__$private
  mismatched$.__enclos_env__ = mismatched_enclosure

  collection = ParamSetCollection$new(list(owner = current))
  class(collection) = c(
    "ParamSetShadow", "ParamSetCollection", "ParamSet", "R6"
  )
  shadow = ParamSetShadow$new(current, character())
  class(shadow) = c(
    "ParamSetCollection", "ParamSetShadow", "ParamSet", "R6"
  )

  expect_false(paradox:::.paradox_gateway_current_core(borrowed))
  expect_false(paradox:::.paradox_gateway_current_core(mismatched))
  expect_false(paradox:::.paradox_gateway_current_core(collection))
  expect_false(paradox:::.paradox_gateway_current_core(shadow))

  missing_r6 = ps(x = p_dbl())
  class(missing_r6) = "ParamSet"
  expect_false(paradox:::.paradox_gateway_current_core(missing_r6))

  AdditiveBaseRoot = R6::R6Class("GatewayBaseRoot", inherit = ParamSet)
  AdditiveBaseLeaf = R6::R6Class(
    "GatewayBaseLeaf",
    inherit = AdditiveBaseRoot
  )
  AdditiveCollectionRoot = R6::R6Class(
    "GatewayCollectionRoot",
    inherit = ParamSetCollection
  )
  AdditiveCollectionLeaf = R6::R6Class(
    "GatewayCollectionLeaf",
    inherit = AdditiveCollectionRoot
  )
  AdditiveShadowRoot = R6::R6Class(
    "GatewayShadowRoot",
    inherit = ParamSetShadow
  )
  AdditiveShadowLeaf = R6::R6Class(
    "GatewayShadowLeaf",
    inherit = AdditiveShadowRoot
  )
  additive_base = AdditiveBaseLeaf$new(list(x = p_dbl()))
  additive_collection = AdditiveCollectionLeaf$new(
    list(owner = ps(x = p_dbl()))
  )
  additive_shadow = AdditiveShadowLeaf$new(
    ps(x = p_dbl()),
    character()
  )
  expect_true(paradox:::.paradox_gateway_current_core(additive_base))
  expect_true(paradox:::.paradox_gateway_current_core(additive_collection))
  expect_true(paradox:::.paradox_gateway_current_core(additive_shadow))
  expect_true(
    paradox:::.paradox_gateway_current_context(
      additive_collection,
      1L
    )$ok
  )
  expect_true(
    paradox:::.paradox_gateway_current_context(additive_shadow, 1L)$ok
  )
  collection_flatten_gateway = get(
    ".__ParamSetCollection__flatten",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  flattened = collection_flatten_gateway(
    self = additive_collection,
    private = stop("additive collection private promise was forced"),
    super = stop("additive collection super promise was forced")
  )
  expect_s3_class(flattened, "ParamSet")
  expect_identical(flattened$ids(), additive_collection$ids())

  shadow_params_gateway = get(
    ".__ParamSetShadow__params",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  expect_equal(
    shadow_params_gateway(
      self = additive_shadow,
      private = stop("additive Shadow private promise was forced"),
      super = stop("additive Shadow super promise was forced")
    ),
    additive_shadow$params
  )

  malformed_policy = ps(x = p_dbl())
  malformed_policy$assert_values = NA
  expect_false(paradox:::.paradox_gateway_current_core(malformed_policy))
  if (getRversion() < "4.0.0") {
    expect_error(
      upgrade_paradox_object_graph(malformed_policy),
      "cannot inspect an active binding on R 3.6",
      fixed = TRUE
    )
  } else {
    expect_error(
      upgrade_paradox_object_graph(malformed_policy),
      "assert_values",
      fixed = TRUE
    )
  }

  noncanonical = ps(x = p_dbl())
  noncanonical_private = noncanonical$.__enclos_env__$private
  noncanonical_core = noncanonical_private$.core
  attr(noncanonical_core, "forged") = TRUE
  noncanonical_private$.core = noncanonical_core
  expect_false(paradox:::.paradox_gateway_current_core(noncanonical))
  if (getRversion() < "4.0.0") {
    expect_error(
      upgrade_paradox_object_graph(noncanonical),
      "cannot inspect an active binding on R 3.6",
      fixed = TRUE
    )
  } else {
    expect_error(
      upgrade_paradox_object_graph(noncanonical),
      "noncanonical versioned core capsule",
      fixed = TRUE
    )
  }

  class_observations = new.env(parent = emptyenv())
  class_observations$count = 0L
  hostile_class = native_stateful_altrep(
    c("ParamSet", "R6"),
    c("ParamSet", "R6"),
    callback = function() {
      class_observations$count = class_observations$count + 1L
    },
    callback_after = c(0L, 0L)
  )
  hostile = ps(x = p_dbl())
  attr(hostile, "class") = hostile_class
  native_stateful_altrep_rearm(
    attr(hostile, "class", exact = TRUE),
    c(0L, 0L)
  )
  observations_before = class_observations$count
  expect_false(paradox:::.paradox_gateway_current_core(hostile))
  expect_identical(class_observations$count, observations_before)

  delayed_forced = FALSE
  delayed = new.env(parent = emptyenv())
  class(delayed) = class(current)
  delayed_enclosure = new.env(parent = emptyenv())
  delayed_enclosure$self = delayed
  delayedAssign(
    "private",
    {
      delayed_forced = TRUE
      current$.__enclos_env__$private
    },
    assign.env = delayed_enclosure
  )
  delayed$.__enclos_env__ = delayed_enclosure
  expect_false(paradox:::.paradox_gateway_current_core(delayed))
  expect_false(delayed_forced)

  gateway = get(
    ".__ParamSet__ids",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )
  old = options(paradox.legacy_object_action = NULL)
  on.exit(options(old), add = TRUE)
  private_forced = FALSE
  super_forced = FALSE
  expect_error(
    gateway(
      self = borrowed,
      private = {
        private_forced = TRUE
        stop("borrowed private promise was forced")
      },
      super = {
        super_forced = TRUE
        stop("borrowed super promise was forced")
      }
    ),
    "upgrade_paradox_object_graph",
    fixed = TRUE
  )
  expect_false(private_forced)
  expect_false(super_forced)
})

test_that("the serialized ParamSet-family target ledger is complete", {
  namespace = asNamespace("paradox")
  ledgers = list(
    ParamSet = paradox:::.paradox_legacy_paramset_members,
    ParamSetCollection = paradox:::.paradox_legacy_collection_members,
    ParamSetShadow = paradox:::.paradox_prerelease_shadow_members
  )
  generators = list(
    ParamSet = ParamSet,
    ParamSetCollection = ParamSetCollection,
    ParamSetShadow = ParamSetShadow
  )

  for (classname in names(ledgers)) {
    old_targets = paste0(".__", classname, "__", ledgers[[classname]])
    expect_identical(anyDuplicated(old_targets), 0L)
    expect_true(all(vapply(old_targets, function(target) {
      exists(target, envir = namespace, inherits = FALSE) &&
        is.function(get(target, envir = namespace, inherits = FALSE))
    }, logical(1L))))

    generator = generators[[classname]]
    current_members = c(
      generator$public_methods,
      generator$private_methods,
      generator$active
    )
    for (member in names(current_members)) {
      target = paste0(".__paradox2_", classname, "__", member)
      expect_true(exists(target, envir = namespace, inherits = FALSE))
      expect_match(
        paste(deparse(body(current_members[[member]])), collapse = "\n"),
        target,
        fixed = TRUE
      )
    }
  }
})

test_that("native graph discovery is iterative, identity-aware, and inert", {
  skip_if_no_active_binding_inspection()
  ordinary = graph_candidate("ordinary")
  active = graph_candidate("active")
  promised = graph_candidate("promised")
  dots_promised = graph_candidate("dots-promised")
  ordinary_dots = graph_candidate("ordinary-dots-binding")
  closure_parent = graph_candidate("closure-parent")
  attributed = graph_candidate("attributed")
  protected = graph_candidate("protected")
  boundary = graph_candidate("global-boundary")

  active_host = new.env(parent = emptyenv())
  active_environment = new.env(parent = emptyenv())
  active_environment$hidden = active
  active_function = function(value) stop("active binding was invoked")
  environment(active_function) = active_environment
  makeActiveBinding("danger", active_function, active_host)

  promise_environment = new.env(parent = baseenv())
  promise_environment$hidden = promised
  promise_host = new.env(parent = emptyenv())
  delayedAssign(
    "danger",
    stop("delayed binding was forced"),
    eval.env = promise_environment,
    assign.env = promise_host
  )

  capture_dots = function(...) environment()
  environment(capture_dots) = baseenv()
  dots_evaluation_environment = new.env(parent = baseenv())
  dots_evaluation_environment$capture_dots = capture_dots
  dots_evaluation_environment$hidden = dots_promised
  dots_frame = eval(
    quote(capture_dots(stop("dots promise was forced"))),
    envir = dots_evaluation_environment
  )

  dots_host = new.env(parent = emptyenv())
  assign("...", ordinary_dots, envir = dots_host)

  closure_ancestor = new.env(parent = emptyenv())
  closure_ancestor$hidden = closure_parent
  closure_environment = new.env(parent = closure_ancestor)
  closure = function() NULL
  environment(closure) = closure_environment

  attribute_environment = new.env(parent = emptyenv())
  attribute_environment$hidden = attributed
  attribute_function = function() hidden
  environment(attribute_function) = attribute_environment
  attribute_carrier = 1L
  attr(attribute_carrier, "hidden_function") = attribute_function

  current = ps(payload = p_uty())
  current$values = list(payload = protected)

  cyclic = new.env(parent = emptyenv())
  cyclic$self = cyclic
  cyclic$ordinary = ordinary
  cyclic$alias = ordinary

  global_name = ".paradox_upgrade_graph_boundary_probe"
  assign(global_name, boundary, envir = .GlobalEnv)
  on.exit(rm(list = global_name, envir = .GlobalEnv), add = TRUE)
  global_closure = function() NULL
  environment(global_closure) = .GlobalEnv

  root = list(
    cyclic,
    active_host,
    promise_host,
    dots_frame,
    dots_host,
    closure,
    attribute_carrier,
    current,
    global_closure
  )
  discovery = discover_upgrade_candidates(root)
  expected = list(
    ordinary,
    active,
    promised,
    dots_promised,
    ordinary_dots,
    closure_parent,
    attributed,
    current,
    protected
  )
  expect_true(same_environment_set(discovery$objects, expected))
  expect_false(any(vapply(
    discovery$objects,
    identical,
    logical(1L),
    y = boundary
  )))
  expect_true(any(grepl("\\.active", discovery$paths, fixed = FALSE)))
  expect_true(any(grepl("\\.promise\\.environment", discovery$paths)))
  expect_true(any(grepl("@attr", discovery$paths, fixed = TRUE)))
  expect_true(any(grepl("\\.protected", discovery$paths)))
  expect_error(promise_host$danger, "delayed binding was forced", fixed = TRUE)
  expect_error(
    eval(quote(..1), envir = dots_frame),
    "dots promise was forced",
    fixed = TRUE
  )

  expect_identical(discover_upgrade_candidates(.GlobalEnv)$objects, list())
  expect_identical(
    discover_upgrade_candidates(asNamespace("paradox"))$objects,
    list()
  )
})

test_that("user-database environments are opaque graph boundaries", {
  database = new.env(parent = emptyenv())
  namespace_reads = 0L
  makeActiveBinding(".__NAMESPACE__.", function(value) {
    if (!missing(value)) stop("namespace marker is read-only")
    namespace_reads <<- namespace_reads + 1L
    stop("namespace marker was invoked")
  }, database)
  database$hidden = graph_candidate("user-database-hidden")
  class(database) = "UserDefinedDatabase"

  discovery = discover_upgrade_candidates(database)
  expect_length(discovery$objects, 0L)
  expect_length(discovery$paths, 0L)
  expect_identical(namespace_reads, 0L)
})

test_that("native graph discovery does not consume the C or R stack", {
  candidate = graph_candidate("deep")
  root = candidate
  for (index in seq_len(5000L)) root = list(root)
  discovery = discover_upgrade_candidates(root)
  expect_length(discovery$objects, 1L)
  expect_identical(discovery$objects[[1L]], candidate)
})

test_that("graph discovery balances a self-duplicating ALTREP root", {
  skip_if_no_list_altrep()

  candidate = graph_candidate("self-duplicating-altrep")
  carrier = native_stateful_altrep(
    list(candidate),
    list(candidate),
    duplicate_returns_self = TRUE
  )

  diagnostics = capture.output(
    discovery <- discover_upgrade_candidates(carrier),
    type = "message"
  )
  expect_false(any(grepl("stack imbalance", diagnostics, fixed = TRUE)))
  expect_identical(discovery$objects, list(candidate))
  expect_identical(discovery$paths, "x[[1]]")
})

test_that("graph paths render multi-digit indices portably", {
  candidate = graph_candidate("multi-digit-index")
  carrier = rep(list(NULL), 12L)
  carrier[[12L]] = candidate

  discovery = discover_upgrade_candidates(carrier)
  expect_identical(discovery$objects, list(candidate))
  expect_identical(discovery$paths, "x[[12]]")
})

test_that("current ParamSet payloads expose current and nested candidates", {
  nested = graph_candidate("opaque-current-value")
  current = ps(payload = p_uty())
  current$values = list(payload = nested)
  discovery = discover_upgrade_candidates(current)
  expect_true(same_environment_set(discovery$objects, list(current, nested)))
  nested_position = which(vapply(
    discovery$objects,
    identical,
    logical(1L),
    y = nested
  ))
  expect_length(nested_position, 1L)
  expect_true(grepl("\\.protected", discovery$paths[[nested_position]]))
})

test_that("replaced current methods remain graph edges", {
  nested = graph_candidate("replaced-method-environment")
  closure_environment = new.env(parent = emptyenv())
  closure_environment$nested = nested
  replacement = function(...) character()
  environment(replacement) = closure_environment

  current = ps(x = p_dbl())
  unlockBinding("ids", current)
  current$ids = replacement

  discovery = discover_upgrade_candidates(current)
  expect_true(same_environment_set(discovery$objects, list(current, nested)))
})

test_that("R 3.6 keeps unsupported relocked method replacements opaque", {
  nested = graph_candidate("relocked-method-environment")
  closure_environment = new.env(parent = emptyenv())
  closure_environment$nested = nested
  replacement = function(...) character()
  environment(replacement) = closure_environment

  current = ps(x = p_dbl())
  unlockBinding("ids", current)
  current$ids = replacement
  lockBinding("ids", current)
  discovery = discover_upgrade_candidates(current)

  expected = if (getRversion() < "4.0.0") {
    list(current)
  } else {
    list(current, nested)
  }
  expect_true(same_environment_set(discovery$objects, expected))
})

test_that("R 3.6 keeps unsupported replaced active facades opaque", {
  nested = graph_candidate("replaced-active-environment")
  closure_environment = new.env(parent = baseenv())
  closure_environment$nested = nested
  closure_environment$calls = 0L
  replacement = function(value) {
    calls <<- calls + 1L
    stop("replacement active binding was invoked")
  }
  environment(replacement) = closure_environment

  current = ps(x = p_dbl())
  makeActiveBinding("values", replacement, current)
  discovery = discover_upgrade_candidates(current)

  expected = if (getRversion() < "4.0.0") {
    list(current)
  } else {
    list(current, nested)
  }
  expect_true(same_environment_set(discovery$objects, expected))
  expect_identical(closure_environment$calls, 0L)
})

test_that("current graph preflight does not refresh stale Shadows", {
  origin = ps(hidden = p_int(), visible = p_dbl())
  shadow = ParamSetShadow$new(origin, "hidden")
  private = mlr3misc::get_private(shadow)
  origin$values = list(hidden = 1L, visible = 0.5)
  before = serialize(private$.core, NULL)

  expect_identical(upgrade_paradox_object_graph(shadow), shadow)
  expect_identical(serialize(private$.core, NULL), before)

  corrupt = ps(x = p_dbl())
  paradox:::param_set_core_replace(
    mlr3misc::get_private(corrupt),
    values = list(unknown = 1)
  )
  expect_error(
    upgrade_paradox_object_graph(list(shadow = shadow, corrupt = corrupt)),
    "corrupt current state capsule"
  )
  expect_identical(serialize(private$.core, NULL), before)

  nested_origin = ps(x = p_dbl())
  nested = ParamSetShadow$new(nested_origin, character())
  collection = ParamSetCollection$new(list(nested = nested))
  outer = ParamSetShadow$new(collection, character())
  nested_origin$values = list(x = 0.25)
  nested_private = mlr3misc::get_private(nested)
  outer_private = mlr3misc::get_private(outer)
  nested_before = serialize(nested_private$.core, NULL)
  outer_before = serialize(outer_private$.core, NULL)
  expect_identical(upgrade_paradox_object_graph(outer), outer)
  expect_identical(serialize(nested_private$.core, NULL), nested_before)
  expect_identical(serialize(outer_private$.core, NULL), outer_before)

  malformed_child = ps(x = p_dbl())
  malformed_collection = ParamSetCollection$new(
    list(child = malformed_child)
  )
  class(malformed_child) = "ParamSet"
  if (getRversion() < "4.0.0") {
    expect_error(
      upgrade_paradox_object_graph(malformed_collection),
      "cannot inspect an active binding on R 3.6",
      fixed = TRUE
    )
  } else {
    expect_error(
      upgrade_paradox_object_graph(malformed_collection),
      "invalid ParamSet-family R6 class",
      fixed = TRUE
    )
  }
})

test_that("recursive graph upgrade is an identity-preserving no-op for current graphs", {
  parameter_set = ps(x = p_dbl(0, 1))
  host = new.env(parent = emptyenv())
  host$parameter_set = parameter_set
  host$self = host
  expect_identical(upgrade_paradox_object_graph(host), host)
  expect_identical(host$parameter_set, parameter_set)
  expect_identical(host$parameter_set$ids(), "x")
})

test_that("additive current graphs fail closed only without active inspection", {
  AdditiveSet = R6::R6Class(
    "GraphAdditiveSet",
    inherit = ParamSet
  )
  parameter_set = AdditiveSet$new(list(x = p_dbl(0, 1)))
  private = mlr3misc::get_private(parameter_set)
  before_core = serialize(private$.core, NULL)
  before_enclosure = parameter_set$.__enclos_env__

  if (getRversion() < "4.0.0") {
    expect_error(
      upgrade_paradox_object_graph(parameter_set),
      "cannot inspect an active binding on R 3.6",
      fixed = TRUE
    )
    expect_identical(parameter_set$.__enclos_env__, before_enclosure)
    expect_identical(serialize(private$.core, NULL), before_core)
  } else {
    expect_identical(
      upgrade_paradox_object_graph(parameter_set),
      parameter_set
    )
  }
})

test_that("authentic Paradox 1 shells upgrade everywhere by identity", {
  skip_if_no_active_binding_inspection()

  root = Sys.getenv("PARADOX_MBO_CONFIG_ROOT", "")
  skip_if(!nzchar(root), "PARADOX_MBO_CONFIG_ROOT is not configured")
  path = file.path(root, "mixed_search_space.rds")
  skip_if_not(file.exists(path), "authentic Paradox 1 fixture is unavailable")

  legacy = readRDS(path)
  expected_ids = as.character(
    legacy$.__enclos_env__$private$.params$id
  )
  alias = legacy
  host = new.env(parent = emptyenv())
  host$public = legacy
  host$private_like = new.env(parent = emptyenv())
  host$private_like$nested = legacy
  host$self = host
  closure_environment = new.env(parent = emptyenv())
  closure_environment$captured = legacy
  closure = function() captured
  environment(closure) = closure_environment
  attr(host, "captured_closure") = closure

  expect_identical(upgrade_paradox_object_graph(host), host)
  expect_identical(host$public, legacy)
  expect_identical(host$private_like$nested, legacy)
  expect_identical(alias, legacy)
  expect_identical(legacy$ids(), expected_ids)
  first_id = expected_ids[[1L]]
  first_domain = legacy$domains[[first_id]]
  first_class = first_domain$cls[[1L]]
  if (identical(first_class, "ParamLgl")) {
    value = TRUE
  } else if (first_class %in% c("ParamDbl", "ParamInt")) {
    value = if (is.finite(first_domain$lower[[1L]])) {
      first_domain$lower[[1L]]
    } else if (is.finite(first_domain$upper[[1L]])) {
      first_domain$upper[[1L]]
    } else {
      0
    }
    if (identical(first_class, "ParamInt")) value = as.integer(value)
  } else if (identical(first_class, "ParamFct")) {
    value = first_domain$levels[[1L]][[1L]]
  } else {
    value = list()
  }
  legacy$values = setNames(list(value), first_id)
  expect_identical(legacy$values[[first_id]], value)
  expect_identical(upgrade_paradox_object_graph(host), host)

  clone = legacy$clone(deep = TRUE)
  expect_identical(clone$ids(), legacy$ids())
  roundtrip_path = tempfile(fileext = ".rds")
  saveRDS(legacy, roundtrip_path)
  roundtrip = readRDS(roundtrip_path)
  expect_identical(roundtrip$ids(), legacy$ids())
})

test_that("legacy shells inside a current capsule are upgraded in place", {
  skip_if_no_active_binding_inspection()

  root = Sys.getenv("PARADOX_MBO_CONFIG_ROOT", "")
  skip_if(!nzchar(root), "PARADOX_MBO_CONFIG_ROOT is not configured")
  path = file.path(root, "numeric_search_space.rds")
  skip_if_not(file.exists(path), "authentic Paradox 1 fixture is unavailable")

  legacy = readRDS(path)
  expected_ids = as.character(
    legacy$.__enclos_env__$private$.params$id
  )
  current = ps(payload = p_uty())
  current$values = list(payload = legacy)
  host = list(current = current, legacy_alias = legacy)

  expect_identical(upgrade_paradox_object_graph(host), host)
  expect_identical(host$current, current)
  expect_identical(host$legacy_alias, legacy)
  expect_identical(current$values$payload, legacy)
  expect_identical(legacy$ids(), expected_ids)
})

test_that("graph migration preflight leaves every shell untouched on error", {
  root = Sys.getenv("PARADOX_MBO_CONFIG_ROOT", "")
  skip_if(!nzchar(root), "PARADOX_MBO_CONFIG_ROOT is not configured")
  path = file.path(root, "numeric_search_space.rds")
  skip_if_not(file.exists(path), "authentic Paradox 1 fixture is unavailable")

  legacy = readRDS(path)
  before = serialize(legacy, NULL)
  malformed = graph_candidate("malformed")
  if (getRversion() < "4.0.0") {
    expect_error(
      upgrade_paradox_object_graph(list(legacy, malformed)),
      "cannot inspect an active binding on R 3.6",
      fixed = TRUE
    )
  } else {
    expect_error(
      upgrade_paradox_object_graph(list(legacy, malformed)),
      "Cannot upgrade Paradox object",
      fixed = TRUE
    )
  }
  expect_identical(serialize(legacy, NULL), before)
})

test_that("legacy first use errors by default and can auto-upgrade", {
  root = Sys.getenv("PARADOX_MBO_CONFIG_ROOT", "")
  skip_if(!nzchar(root), "PARADOX_MBO_CONFIG_ROOT is not configured")
  path = file.path(root, "numeric_search_space.rds")
  skip_if_not(file.exists(path), "authentic Paradox 1 fixture is unavailable")

  legacy = readRDS(path)
  expected_ids = as.character(
    legacy$.__enclos_env__$private$.params$id
  )
  expect_error(
    legacy$ids(),
    "upgrade_paradox_object_graph",
    fixed = TRUE
  )

  old = options(paradox.legacy_object_action = NA_character_)
  on.exit(options(old), add = TRUE)
  invalid = readRDS(path)
  expect_error(
    invalid$ids(),
    "`paradox.legacy_object_action` must be exactly",
    fixed = TRUE
  )

  options(paradox.legacy_object_action = "upgrade")
  legacy = readRDS(path)
  identity = legacy
  if (getRversion() < "4.0.0") {
    before_enclosure = legacy$.__enclos_env__
    before_private = before_enclosure$private
    state_names = c(
      ".params", ".values", ".tags", ".deps", ".trafos",
      ".extra_trafo", ".constraint"
    )
    before_state = lapply(state_names, function(name) {
      serialize(get(name, envir = before_private, inherits = FALSE), NULL)
    })
    error = tryCatch(
      {
        legacy$ids()
        NULL
      },
      error = function(error) error
    )
    expect_s3_class(error, "error")
    expect_match(
      conditionMessage(error),
      "cannot inspect an active binding on R 3.6",
      fixed = TRUE
    )
    expect_match(conditionMessage(error), "R >= 4.0", fixed = TRUE)
    expect_identical(legacy$.__enclos_env__, before_enclosure)
    expect_identical(legacy$.__enclos_env__$private, before_private)
    expect_false(exists(
      ".core",
      envir = legacy$.__enclos_env__$private,
      inherits = FALSE
    ))
    after_state = lapply(state_names, function(name) {
      serialize(get(name, envir = before_private, inherits = FALSE), NULL)
    })
    expect_identical(after_state, before_state)
    expect_identical(identity, legacy)
    # R 3.6's default JIT may compile the invoked serialized method closure,
    # changing the byte serialization of the complete R6 graph even though
    # neither the gateway nor the upgrader changed its authoritative state.
  } else {
    ids = legacy$ids()
    expect_identical(ids, expected_ids)
    expect_identical(identity, legacy)
    check = legacy$check(list())
    expect_true(identical(check, TRUE) || is.character(check))
  }
})
