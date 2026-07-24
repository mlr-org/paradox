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

test_that("native graph discovery does not consume the C or R stack", {
  candidate = graph_candidate("deep")
  root = candidate
  for (index in seq_len(5000L)) root = list(root)
  discovery = discover_upgrade_candidates(root)
  expect_length(discovery$objects, 1L)
  expect_identical(discovery$objects[[1L]], candidate)
})

test_that("graph discovery balances a self-duplicating ALTREP root", {
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

test_that("current ParamSet payloads expose nested legacy candidates only", {
  nested = graph_candidate("opaque-current-value")
  current = ps(payload = p_uty())
  current$values = list(payload = nested)
  discovery = discover_upgrade_candidates(current)
  expect_identical(discovery$objects, list(nested))
  expect_true(grepl("\\.protected", discovery$paths[[1L]]))
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

test_that("authentic Paradox 1 shells upgrade everywhere by identity", {
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
  expect_error(
    upgrade_paradox_object_graph(list(legacy, malformed)),
    "Cannot upgrade Paradox object",
    fixed = TRUE
  )
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
  ids = legacy$ids()
  expect_identical(ids, expected_ids)
  expect_identical(identity, legacy)
  check = legacy$check(list())
  expect_true(identical(check, TRUE) || is.character(check))
})
