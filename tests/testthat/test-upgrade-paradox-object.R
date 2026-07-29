legacy_upgrader_generator = local({
  base = NULL
  collection = NULL

  active_stop = function(value) stop("legacy active binding executed")

  function(kind = c("base", "collection")) {
    kind = match.arg(kind)
    if (is.null(base)) {
      base <<- R6::R6Class(
        "ParamSet",
        parent_env = asNamespace("paradox"),
        public = list(
          assert_values = TRUE,
          initialize = function() invisible(self),
          ids = function(...) stop("legacy method executed"),
          marker = function() stop("legacy method executed")
        ),
        active = list(
          values = active_stop,
          tags = active_stop,
          params = active_stop,
          domains = active_stop,
          extra_trafo = active_stop,
          constraint = active_stop,
          deps = active_stop
        ),
        private = list(
          .params = NULL,
          .values = list(),
          .tags = NULL,
          .deps = NULL,
          .trafos = NULL,
          .extra_trafo = NULL,
          .constraint = NULL,
          .store_values = function(...) stop("legacy private method executed"),
          .get_values = function(...) stop("legacy private method executed"),
          deep_clone = function(...) stop("legacy private method executed"),
          get_tune_ps = function(...) stop("legacy private method executed")
        )
      )
    }
    if (kind == "base") return(base)

    if (is.null(collection)) {
      collection <<- R6::R6Class(
        "ParamSetCollection",
        inherit = base,
        # `inherit = base` is resolved lazily by R6 at `$new()`. Keep the
        # generator's closure environment until then; the resulting canonical
        # legacy enclosures are reparented to the paradox namespace below.
        parent_env = environment(),
        public = list(
          initialize = function() invisible(self),
          add = function(...) stop("legacy method executed")
        ),
        active = list(sets = active_stop),
        private = list(
          .sets = NULL,
          .translation = NULL,
          .postfix = FALSE,
          .children_with_trafos = integer(),
          .children_with_constraints = integer(),
          .add_name_prefix = function(...) stop("legacy private method executed")
        )
      )
    }
    collection
  }
})

as_legacy_table = function(x) {
  data.table::as.data.table(lapply(x, identity))
}

make_delayed_literal_binding = function(owner, name, value) {
  call = as.call(list(
    as.name("delayedAssign"),
    name,
    value,
    baseenv(),
    owner
  ))
  eval(call, envir = baseenv())
}

legacy_base_from_current = function(current) {
  state = paradox:::param_set_core_state(mlr3misc::get_private(current))
  result = legacy_upgrader_generator("base")$new()
  private = mlr3misc::get_private(result)
  private$.params = as_legacy_table(state$.params)
  private$.values = state$.values
  private$.tags = as_legacy_table(state$.tags)
  private$.deps = as_legacy_table(state$.deps)
  private$.trafos = as_legacy_table(state$.trafos)
  private$.extra_trafo = state$.extra_trafo
  private$.constraint = state$.constraint
  result$assert_values = current$assert_values
  result
}

legacy_collection_from_current = function(current, children) {
  state = paradox:::param_set_core_state(mlr3misc::get_private(current))
  result = legacy_upgrader_generator("collection")$new()
  enclosing = result$.__enclos_env__
  parent.env(enclosing) = asNamespace("paradox")
  parent.env(enclosing$super$.__enclos_env__) = asNamespace("paradox")
  private = mlr3misc::get_private(result)
  private$.params = as_legacy_table(state$.params)
  private$.values = structure(list(), names = character())
  private$.tags = as_legacy_table(state$.tags)
  private$.deps = as_legacy_table(state$.deps)
  private$.trafos = as_legacy_table(state$.trafos)
  private$.extra_trafo = NULL
  private$.constraint = NULL
  private$.sets = children
  private$.translation = as_legacy_table(state$.translation)
  private$.postfix = state$.postfix
  result$assert_values = current$assert_values
  result
}

transplantable_legacy_base_from_current = function(current) {
  state = paradox:::param_set_core_state(mlr3misc::get_private(current))
  result = current$clone(deep = TRUE)
  private = copy_legacy_private_environment(result, omit = ".core")
  private$.params = as_legacy_table(state$.params)
  private$.values = state$.values
  private$.tags = as_legacy_table(state$.tags)
  private$.deps = as_legacy_table(state$.deps)
  private$.trafos = as_legacy_table(state$.trafos)
  private$.extra_trafo = state$.extra_trafo
  private$.constraint = state$.constraint
  result
}

transplantable_legacy_collection_from_current = function(current, children) {
  state = paradox:::param_set_core_state(mlr3misc::get_private(current))
  result = current$clone(deep = FALSE)
  private = copy_legacy_private_environment(result, omit = ".core")
  private$.params = as_legacy_table(state$.params)
  private$.values = structure(list(), names = character())
  private$.tags = as_legacy_table(state$.tags)
  private$.deps = as_legacy_table(state$.deps)
  private$.trafos = as_legacy_table(state$.trafos)
  private$.extra_trafo = NULL
  private$.constraint = NULL
  private$.sets = children
  private$.translation = as_legacy_table(state$.translation)
  private$.postfix = state$.postfix
  result
}

legacy_detached_extra_trafo = function(sets_with_trafos) {
  children_with_trafos = seq_along(sets_with_trafos)
  translation = NULL
  psc_extra_trafo = function(x, ...) x
  environment(psc_extra_trafo) = baseenv()
  postfix = FALSE
  result = mlr3misc::crate(
    function(x) psc_extra_trafo(
      x,
      children_with_trafos,
      sets_with_trafos,
      translation,
      postfix
    ),
    children_with_trafos,
    sets_with_trafos,
    translation,
    psc_extra_trafo,
    postfix
  )
  parent.env(environment(result)) = asNamespace("paradox")
  result
}

legacy_detached_constraint = function(sets_with_constraints) {
  children_with_constraints = seq_along(sets_with_constraints)
  translation = NULL
  psc_constraint = function(x, ...) TRUE
  environment(psc_constraint) = baseenv()
  result = mlr3misc::crate(
    function(x) psc_constraint(
      x,
      children_with_constraints,
      sets_with_constraints,
      translation
    ),
    children_with_constraints,
    sets_with_constraints,
    translation,
    psc_constraint
  )
  parent.env(environment(result)) = asNamespace("paradox")
  result
}

copy_legacy_private_environment = function(x, omit = character()) {
  enclosing = x$.__enclos_env__
  source = mlr3misc::get_private(x)
  result = new.env(parent = parent.env(source))
  names = setdiff(ls(source, all.names = TRUE), omit)
  for (name in names) {
    assign(name, get(name, envir = source, inherits = FALSE), envir = result)
  }
  repeat {
    enclosing$private = result
    super = get0("super", envir = enclosing, inherits = FALSE)
    if (!is.environment(super) ||
        !exists(".__enclos_env__", envir = super, inherits = FALSE)) {
      break
    }
    enclosing = get(
      ".__enclos_env__",
      envir = super,
      inherits = FALSE
    )
  }
  result
}

with_replacement_owner_upgrader = function(code) {
  namespace = asNamespace("paradox")
  registry = get(
    ".paradox_object_upgrader_registry",
    envir = namespace,
    inherits = FALSE
  )
  legacy_class = c("ParamSetShadow", "ParamSet", "R6")
  key = paradox:::.paradox_registry_class_key(legacy_class)
  had_entry = exists(key, envir = registry, inherits = FALSE)
  old_entry = get0(key, envir = registry, inherits = FALSE)
  on.exit({
    if (had_entry) {
      assign(key, old_entry, envir = registry)
    } else if (exists(key, envir = registry, inherits = FALSE)) {
      rm(list = key, envir = registry)
    }
  }, add = TRUE)

  entry = list(
    owner_package = "paradox",
    legacy_class = legacy_class,
    migration_kind = "replacement",
    inspector = ".upgrade_paradox_node_info",
    rebuilder = ".upgrade_paradox_build_owner",
    retired_bindings = character(),
    .owner_namespace = namespace
  )
  paradox:::.paradox_validate_object_upgrader_entry(entry, key)
  assign(key, entry, envir = registry)

  resolver_name = ".paradox_object_upgrader_resolve"
  old_resolver = get(resolver_name, envir = namespace, inherits = FALSE)
  resolver_was_locked = bindingIsLocked(resolver_name, namespace)
  if (resolver_was_locked) unlockBinding(resolver_name, namespace)
  on.exit({
    if (bindingIsLocked(resolver_name, namespace)) {
      unlockBinding(resolver_name, namespace)
    }
    assign(resolver_name, old_resolver, envir = namespace)
    if (resolver_was_locked) lockBinding(resolver_name, namespace)
  }, add = TRUE)
  assign(
    resolver_name,
    function(entry, which) {
      if (identical(which, "inspector")) {
        return(function(x) {
          private = mlr3misc::get_private(x)
          list(
            state = list(shadowed = private$.shadowed),
            dependencies = list(origin = private$.set)
          )
        })
      }
      function(base, state, dependencies) {
        paradox::ParamSetShadow$new(
          dependencies$origin,
          state$shadowed
        )
      }
    },
    envir = namespace
  )
  if (resolver_was_locked) lockBinding(resolver_name, namespace)
  force(code)
}

replacement_owner_legacy = function() {
  origin = ps(visible = p_dbl(0, 1), hidden = p_lgl())
  result = ParamSetShadow$new(origin, "hidden")
  private = copy_legacy_private_environment(result, omit = ".core")
  private$.set = origin
  private$.shadowed = "hidden"
  result$assert_values = FALSE
  result
}

test_that("active-binding inspection is transparent or fails closed", {
  owner = new.env(parent = emptyenv())
  binding = function(value) {
    if (missing(value)) 1L else stop("read-only")
  }
  makeActiveBinding("value", binding, owner)

  if (getRversion() >= "4.0.0") {
    expect_identical(
      paradox:::.upgrade_paradox_active_binding_function(
        "value",
        owner,
        "<root>"
      ),
      activeBindingFunction("value", owner)
    )
  } else {
    expect_error(
      paradox:::.upgrade_paradox_active_binding_function(
        "value",
        owner,
        "<root>"
      ),
      "legacy ParamSet migration requires R >= 4.0.0",
      fixed = TRUE
    )
  }
})

test_that("legacy migration fails closed without active-binding inspection", {
  legacy = legacy_base_from_current(ps(x = p_dbl(0, 1)))
  private = mlr3misc::get_private(legacy)

  expect_error(
    testthat::with_mocked_bindings(
      upgrade_paradox_object(legacy),
      .upgrade_paradox_active_binding_accessor = function() NULL,
      .package = "paradox"
    ),
    "legacy ParamSet migration requires R >= 4.0.0",
    fixed = TRUE
  )
  expect_false(exists(".core", envir = private, inherits = FALSE))
})

test_that("legacy graph migration has a real old-R fail-closed boundary", {
  legacy = transplantable_legacy_base_from_current(ps(x = p_dbl(0, 1)))
  private = mlr3misc::get_private(legacy)
  before_enclosure = legacy$.__enclos_env__
  before_params = serialize(private$.params, NULL)
  before_values = serialize(private$.values, NULL)
  host = list(search_space = legacy)

  if (getRversion() < "4.0.0") {
    expect_error(
      upgrade_paradox_object_graph(host),
      "cannot inspect an active binding on R 3.6",
      fixed = TRUE
    )
    expect_identical(legacy$.__enclos_env__, before_enclosure)
    expect_identical(serialize(private$.params, NULL), before_params)
    expect_identical(serialize(private$.values, NULL), before_values)
    expect_false(exists(
      ".core",
      envir = mlr3misc::get_private(legacy),
      inherits = FALSE
    ))
  } else {
    expect_identical(upgrade_paradox_object_graph(host), host)
    expect_true(paradox:::.paradox_gateway_current_core(legacy))
  }
})

test_that("current capsule-backed objects upgrade idempotently", {
  base = ps(x = p_dbl(0, 1), hidden = p_lgl())
  collection = ParamSetCollection$new(list(a = base))
  shadow = ParamSetShadow$new(base, "hidden")
  AdditiveSet = R6::R6Class(
    "UpgradeAdditiveSet",
    inherit = ParamSet,
    public = list(
      label = NULL,
      initialize = function(params, label) {
        super$initialize(params)
        self$label = label
      }
    )
  )
  additive = AdditiveSet$new(list(score = p_dbl(0, 1)), "kept")

  expect_identical(upgrade_paradox_object(base), base)
  expect_identical(upgrade_paradox_object(collection), collection)
  expect_identical(upgrade_paradox_object(shadow), shadow)
  expect_identical(upgrade_paradox_object(additive), additive)
  expect_identical(additive$label, "kept")
  expect_identical(
    .Call(
      paradox:::C_param_set_core_kind,
      mlr3misc::get_private(shadow)
    ),
    3L
  )
})

test_that("replacement owners preserve assert_values in pure and graph migration", {
  skip_if_no_active_binding_inspection()
  with_replacement_owner_upgrader({
    pure_legacy = replacement_owner_legacy()
    pure_origin = mlr3misc::get_private(pure_legacy)$.set
    pure = upgrade_paradox_object(pure_legacy)
    expect_false(pure$assert_values)
    expect_false(pure_legacy$assert_values)
    expect_identical(pure$origin, pure_origin)

    graph_legacy = replacement_owner_legacy()
    graph_origin = mlr3misc::get_private(graph_legacy)$.set
    host = list(owner = graph_legacy, alias = graph_legacy)
    expect_identical(upgrade_paradox_object_graph(host), host)
    expect_identical(host$owner, graph_legacy)
    expect_identical(host$alias, graph_legacy)
    expect_false(graph_legacy$assert_values)
    expect_identical(graph_legacy$origin, graph_origin)
  })
})

test_that("legacy package provenance requires exact namespace identity", {
  namespace_spec = new.env(parent = emptyenv())
  namespace_spec$spec = "paradox"
  fake_namespace = new.env(parent = emptyenv())
  fake_namespace$.__NAMESPACE__. = namespace_spec
  expect_true(isNamespace(fake_namespace))
  expect_identical(environmentName(fake_namespace), "paradox")
  expect_false(identical(fake_namespace, asNamespace("paradox")))

  legacy = legacy_base_from_current(ps(x = p_dbl()))
  parent.env(legacy$.__enclos_env__) = fake_namespace
  expect_error(
    upgrade_paradox_object(legacy),
    "legacy methods do not originate in paradox"
  )

  with_replacement_owner_upgrader({
    owner = replacement_owner_legacy()
    parent.env(owner$.__enclos_env__) = fake_namespace
    expect_error(
      upgrade_paradox_object(owner),
      "legacy owner methods do not have authenticated package provenance"
    )
  })
})

test_that("built-in Domain and Condition objects are normalized", {
  domain = p_int(
    0,
    5,
    tags = "tag",
    depends = quote(parent == 1)
  )
  domain_copy = upgrade_paradox_object(domain)
  expect_identical(class(domain_copy), class(domain))
  expect_identical(
    lapply(names(domain), function(name) .subset2(domain_copy, name)),
    lapply(names(domain), function(name) .subset2(domain, name))
  )
  # R's `identical()` compares the external address of EXTPTRSXP values and
  # therefore reports two independently allocated data.table self-reference
  # shells as identical. Compare their R object addresses instead, then prove
  # that the normalized outward facade is detached from its input.
  copy_selfref = attr(domain_copy, ".internal.selfref", exact = TRUE)
  source_selfref = attr(domain, ".internal.selfref", exact = TRUE)
  expect_type(copy_selfref, "externalptr")
  expect_false(identical(
    data.table::address(copy_selfref),
    data.table::address(source_selfref)
  ))
  expect_false(identical(
    data.table::address(domain_copy),
    data.table::address(domain)
  ))
  data.table::set(domain_copy, 1L, "lower", 1)
  expect_identical(domain$lower, 0)
  expect_identical(domain_copy$lower, 1)

  equal = CondEqual(1L)
  any_of = CondAnyOf(c("a", "b"))
  expect_identical(upgrade_paradox_object(equal), equal)
  expect_identical(upgrade_paradox_object(any_of), any_of)
  expect_identical(
    upgrade_paradox_object(Condition(1, "%s ? %s")),
    Condition(1, "%s ? %s")
  )

  custom_domain = domain
  class(custom_domain) = c("CustomDomain", class(custom_domain))
  expect_error(
    upgrade_paradox_object(custom_domain),
    "unsupported Domain class `CustomDomain/ParamInt/Domain"
  )
  custom_condition = equal
  class(custom_condition) = c("CustomCondition", "Condition")
  expect_error(
    upgrade_paradox_object(custom_condition),
    "unsupported Condition class `CustomCondition/Condition`"
  )
})

test_that("canonical legacy BASE state is rebuilt without executing it", {
  skip_if_no_active_binding_inspection()
  callback_calls = 0L
  current = ps(
    x = p_dbl(0, 1, tags = "number", trafo = function(x) x + 1),
    y = p_fct(c("a", "b")),
    z = p_uty(custom_check = function(x) {
      callback_calls <<- callback_calls + 1L
      TRUE
    })
  )
  paradox:::param_set_core_replace(
    mlr3misc::get_private(current),
    values = list(x = 0.5, y = "a", z = "opaque")
  )
  current$add_dep("y", "x", CondEqual(0.5))
  current$extra_trafo = function(x) {
    callback_calls <<- callback_calls + 1L
    x
  }
  current$constraint = function(x) {
    callback_calls <<- callback_calls + 1L
    TRUE
  }
  legacy = legacy_base_from_current(current)
  # Construction and setter validation may legitimately exercise callbacks;
  # upgrading the already constructed legacy object must not.
  callback_calls = 0L

  upgraded = upgrade_paradox_object(legacy)
  state = paradox:::param_set_core_state(mlr3misc::get_private(upgraded))
  expect_s3_class(upgraded, "ParamSet")
  expect_identical(callback_calls, 0L)
  expect_true(all(vapply(
    state[c(".params", ".tags", ".deps", ".trafos")],
    function(table) identical(class(table), "data.frame"),
    logical(1L)
  )))
  expect_identical(upgraded$ids(), c("x", "y", "z"))
  expect_identical(state$.values, list(x = 0.5, y = "a", z = "opaque"))
  expect_identical(state$.tags$id, "x")
  expect_identical(state$.tags$tag, "number")
  expect_identical(state$.deps$id, "y")
  expect_identical(state$.deps$on, "x")
  expect_identical(class(state$.deps$cond[[1L]]), c("CondEqual", "Condition"))
  expect_identical(state$.extra_trafo, current$extra_trafo)
  expect_identical(state$.constraint, current$constraint)
})

test_that("legacy preparation normalizes callbacks but not opaque values", {
  skip_if_no_active_binding_inspection()
  source_function = function(text) {
    result = eval(parse(text = text, keep.source = TRUE)[[1L]],
      envir = new.env(parent = baseenv()))
    stopifnot(paradox:::.paradox_has_srcref(result))
    result
  }
  trafo = source_function("function(x) { # legacy trafo\n x + 1 }")
  custom_check = source_function(
    "function(x) { # legacy custom check\n TRUE }"
  )
  aggr = source_function("function(x) { # legacy aggr\n x[[1L]] }")
  in_tune_fn = source_function(
    "function(domain, param_vals) { # legacy internal tuning\n domain$upper }"
  )
  extra_trafo = source_function(
    "function(x, param_set) { # legacy extra trafo\n x }"
  )
  constraint = source_function(
    "function(x) { # legacy constraint\n TRUE }"
  )
  opaque_value = source_function(
    "function(x) { # legacy opaque value\n x }"
  )

  old = options(paradox.strip_srcrefs = FALSE)
  on.exit(options(old), add = TRUE)
  current = ps(
    numeric = p_dbl(0, 1, trafo = trafo),
    categorical = p_fct(list(a = 1, b = 2), trafo = trafo),
    payload = p_uty(
      custom_check = custom_check,
      tags = "internal_tuning",
      aggr = aggr,
      in_tune_fn = in_tune_fn,
      disable_in_tune = list(enabled = FALSE)
    )
  )
  current$extra_trafo = extra_trafo
  current$constraint = constraint
  current$values = list(payload = opaque_value)
  legacy = legacy_base_from_current(current)
  legacy_private = mlr3misc::get_private(legacy)
  categorical_index = match("categorical", legacy_private$.trafos$id)
  levels = list(a = 1, b = 2)
  legacy_fct_trafo = mlr3misc::crate(function(x) {
    x = levels[[x]]
    if (!is.null(trafo)) x = trafo(x)
    x
  }, levels, trafo)
  parent.env(environment(legacy_fct_trafo)) = asNamespace("paradox")
  legacy_private$.trafos$trafo[[categorical_index]] = legacy_fct_trafo

  options(paradox.strip_srcrefs = TRUE)
  upgraded = upgrade_paradox_object(legacy)
  state = paradox:::param_set_core_state(mlr3misc::get_private(upgraded))

  expect_true(all(vapply(
    state$.trafos$trafo,
    function(callback) !paradox:::.paradox_has_srcref(callback),
    logical(1L)
  )))
  categorical_trafo = state$.trafos$trafo[
    match("categorical", state$.trafos$id)
  ][[1L]]
  expect_false(paradox:::.paradox_has_srcref(
    get("trafo", envir = environment(categorical_trafo), inherits = FALSE)
  ))
  payload_index = match("payload", state$.params$id)
  payload_cargo = state$.params$cargo[[payload_index]]
  expect_false(paradox:::.paradox_has_srcref(payload_cargo$custom_check))
  expect_false(paradox:::.paradox_has_srcref(payload_cargo$aggr))
  expect_false(paradox:::.paradox_has_srcref(payload_cargo$in_tune_fn))
  expect_false(paradox:::.paradox_has_srcref(state$.extra_trafo))
  expect_false(paradox:::.paradox_has_srcref(state$.constraint))
  expect_true(paradox:::.paradox_has_srcref(state$.values$payload))
  expect_identical(
    data.table::address(state$.values$payload),
    data.table::address(opaque_value)
  )
  expect_identical(
    upgraded$trafo(list(numeric = 0, categorical = "b")),
    list(numeric = 1, categorical = 3)
  )

  fresh = ps(
    numeric = p_dbl(0, 1, trafo = trafo),
    categorical = p_fct(list(a = 1, b = 2), trafo = trafo),
    payload = p_uty(
      custom_check = custom_check,
      tags = "internal_tuning",
      aggr = aggr,
      in_tune_fn = in_tune_fn,
      disable_in_tune = list(enabled = FALSE)
    ),
    .extra_trafo = extra_trafo,
    .constraint = constraint
  )
  fresh$values = list(payload = opaque_value)
  expect_equal(upgraded, fresh)
})

test_that("standalone Domain upgrade normalizes callbacks and repr", {
  old = options(paradox.strip_srcrefs = FALSE)
  on.exit(options(old), add = TRUE)
  domain = eval(parse(
    text = paste(
      "p_dbl(0, 1, trafo = function(x) {",
      "# standalone legacy repr marker",
      "x + 1",
      "})",
      sep = "\n"
    ),
    keep.source = TRUE
  )[[1L]], envir = new.env(parent = asNamespace("paradox")))
  expect_true(paradox:::.paradox_has_srcref(
    .subset2(domain, ".trafo")[[1L]]
  ))
  expect_true(paradox:::.paradox_has_srcref(
    attr(domain, "repr", exact = TRUE)
  ))
  categorical_callback = eval(parse(
    text = "function(x) { # standalone categorical marker\n x + 1 }",
    keep.source = TRUE
  )[[1L]], envir = new.env(parent = baseenv()))
  levels = list(a = 1, b = 2)
  trafo = categorical_callback
  categorical = p_fct(levels)
  legacy_categorical_trafo = mlr3misc::crate(function(x) {
    x = levels[[x]]
    if (!is.null(trafo)) x = trafo(x)
    x
  }, levels, trafo)
  parent.env(environment(legacy_categorical_trafo)) =
    asNamespace("paradox")
  data.table::set(
    categorical,
    i = 1L,
    j = ".trafo",
    value = list(legacy_categorical_trafo)
  )
  categorical_trafo = .subset2(categorical, ".trafo")[[1L]]
  expect_true(paradox:::.paradox_has_srcref(
    get("trafo", envir = environment(categorical_trafo), inherits = FALSE)
  ))

  options(paradox.strip_srcrefs = TRUE)
  upgraded = upgrade_paradox_object(domain)
  expect_false(paradox:::.paradox_has_srcref(
    .subset2(upgraded, ".trafo")[[1L]]
  ))
  expect_false(paradox:::.paradox_has_srcref(
    attr(upgraded, "repr", exact = TRUE)
  ))
  expect_identical(.subset2(upgraded, ".trafo")[[1L]](1), 2)

  upgraded_categorical = upgrade_paradox_object(categorical)
  upgraded_categorical_trafo =
    .subset2(upgraded_categorical, ".trafo")[[1L]]
  expect_false(paradox:::.paradox_has_srcref(upgraded_categorical_trafo))
  expect_false(paradox:::.paradox_has_srcref(
    get(
      "trafo",
      envir = environment(upgraded_categorical_trafo),
      inherits = FALSE
    )
  ))
  expect_identical(upgraded_categorical_trafo("b"), 3)
})

test_that("authenticated legacy crate adapters normalize captured callbacks", {
  skip_if_no_active_binding_inspection()
  source_function = function(text) {
    result = eval(parse(text = text, keep.source = TRUE)[[1L]],
      envir = new.env(parent = baseenv()))
    stopifnot(paradox:::.paradox_has_srcref(result))
    result
  }

  current = ps(
    value = p_int(
      0, 20,
      tags = "internal_tuning",
      aggr = function(x) x[[1L]],
      in_tune_fn = function(domain, param_vals) domain$upper,
      disable_in_tune = list(gate = FALSE)
    ),
    gate = p_lgl()
  )
  legacy = legacy_base_from_current(current)
  legacy_private = mlr3misc::get_private(legacy)

  in_tune_fn = source_function(
    "function(domain, param_vals) domain$upper + as.integer(param_vals$gate)"
  )
  prefix = "child"
  prefixed_set_ids = c("child.value", "child.gate")
  flattened_wrapper = mlr3misc::crate(function(domain, param_vals) {
    param_vals = param_vals[names(param_vals) %in% prefixed_set_ids]
    names(param_vals) = gsub(
      sprintf("^\\Q%s.\\E", prefix),
      "",
      names(param_vals)
    )
    in_tune_fn(domain, param_vals)
  }, in_tune_fn, prefix, prefixed_set_ids)
  parent.env(environment(flattened_wrapper)) = asNamespace("paradox")
  value_index = match("value", legacy_private$.params$id)
  value_cargo = legacy_private$.params$cargo[[value_index]]
  value_cargo$in_tune_fn = flattened_wrapper
  legacy_private$.params$cargo[[value_index]] = value_cargo

  trafo = source_function("function(x) list(value = x$value)")
  pname = "answer"
  tuning_wrapper = mlr3misc::crate(function(x, param_set) {
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
  }, trafo, pname)
  parent.env(environment(tuning_wrapper)) = asNamespace("paradox")
  legacy_private$.extra_trafo = tuning_wrapper

  upgraded = upgrade_paradox_object(legacy)
  state = paradox:::param_set_core_state(mlr3misc::get_private(upgraded))
  upgraded_cargo = state$.params$cargo[[match("value", state$.params$id)]]
  upgraded_in_tune = upgraded_cargo$in_tune_fn
  expect_false(paradox:::.paradox_has_srcref(upgraded_in_tune))
  expect_false(paradox:::.paradox_has_srcref(get(
    "in_tune_fn",
    envir = environment(upgraded_in_tune),
    inherits = FALSE
  )))
  expect_identical(
    upgraded_in_tune(
      upgraded$domains$value,
      list(child.gate = TRUE, unrelated = 100L)
    ),
    21
  )
  expect_false(paradox:::.paradox_has_srcref(upgraded$extra_trafo))
  expect_false(paradox:::.paradox_has_srcref(get(
    "trafo",
    envir = environment(upgraded$extra_trafo),
    inherits = FALSE
  )))
  expect_identical(
    upgraded$extra_trafo(list(value = 2L), upgraded),
    list(answer = 2L)
  )

  collection_legacy = legacy_base_from_current(ps(x = p_int()))
  collection_private = mlr3misc::get_private(collection_legacy)
  children_with_trafos = integer()
  sets_with_trafos = list()
  translation = NULL
  psc_extra_trafo = source_function("function(x, ...) x")
  postfix = FALSE
  collection_private$.extra_trafo = mlr3misc::crate(
    function(x) psc_extra_trafo(
      x,
      children_with_trafos,
      sets_with_trafos,
      translation,
      postfix
    ),
    children_with_trafos,
    sets_with_trafos,
    translation,
    psc_extra_trafo,
    postfix
  )
  parent.env(environment(collection_private$.extra_trafo)) =
    asNamespace("paradox")

  children_with_constraints = integer()
  sets_with_constraints = list()
  psc_constraint = source_function("function(x, ...) TRUE")
  collection_private$.constraint = mlr3misc::crate(
    function(x) psc_constraint(
      x,
      children_with_constraints,
      sets_with_constraints,
      translation
    ),
    children_with_constraints,
    sets_with_constraints,
    translation,
    psc_constraint
  )
  parent.env(environment(collection_private$.constraint)) =
    asNamespace("paradox")

  upgraded_collection = upgrade_paradox_object(collection_legacy)
  expect_false(paradox:::.paradox_has_srcref(get(
    "psc_extra_trafo",
    envir = environment(upgraded_collection$extra_trafo),
    inherits = FALSE
  )))
  expect_false(paradox:::.paradox_has_srcref(get(
    "psc_constraint",
    envir = environment(upgraded_collection$constraint),
    inherits = FALSE
  )))
  expect_identical(
    upgraded_collection$extra_trafo(list(x = 1L)),
    list(x = 1L)
  )
  expect_true(upgraded_collection$constraint(list(x = 1L)))
})

test_that("option-off detached callback migration does not mutate legacy carriers", {
  skip_if_no_active_binding_inspection()
  old = options(paradox.strip_srcrefs = FALSE)
  on.exit(options(old), add = TRUE)

  source_function = function(text, environment) {
    result = eval(
      parse(text = text, keep.source = TRUE)[[1L]],
      envir = environment
    )
    stopifnot(paradox:::.paradox_has_srcref(result))
    compiler::cmpfun(result)
  }

  child = legacy_base_from_current(ps(y = p_int()))
  legacy = legacy_base_from_current(ps(x = p_int()))
  private = mlr3misc::get_private(legacy)

  extra_bindings = list(
    children_with_trafos = 1L,
    sets_with_trafos = list(child = child),
    translation = NULL,
    psc_extra_trafo = source_function(
      "function(x, ...) x",
      new.env(parent = baseenv())
    ),
    postfix = FALSE
  )
  extra_environment = list2env(
    extra_bindings,
    parent = asNamespace("paradox")
  )
  private$.extra_trafo = source_function(
    paste(
      "function(x) psc_extra_trafo(",
      "  x, children_with_trafos, sets_with_trafos, translation, postfix",
      ")",
      sep = "\n"
    ),
    extra_environment
  )

  constraint_bindings = list(
    children_with_constraints = 1L,
    sets_with_constraints = list(child = child),
    translation = NULL,
    psc_constraint = source_function(
      "function(x, ...) TRUE",
      new.env(parent = baseenv())
    )
  )
  constraint_environment = list2env(
    constraint_bindings,
    parent = asNamespace("paradox")
  )
  private$.constraint = source_function(
    paste(
      "function(x) psc_constraint(",
      "  x, children_with_constraints, sets_with_constraints, translation",
      ")",
      sep = "\n"
    ),
    constraint_environment
  )

  extra_alias = private$.extra_trafo
  constraint_alias = private$.constraint
  child_alias = child
  legacy_bytes = serialize(legacy, NULL)
  extra_bytes = serialize(extra_alias, NULL)
  constraint_bytes = serialize(constraint_alias, NULL)

  upgraded = upgrade_paradox_object(legacy)

  expect_identical(serialize(legacy, NULL), legacy_bytes)
  expect_identical(private$.extra_trafo, extra_alias)
  expect_identical(private$.constraint, constraint_alias)
  expect_identical(serialize(extra_alias, NULL), extra_bytes)
  expect_identical(serialize(constraint_alias, NULL), constraint_bytes)
  expect_identical(
    get(
      "sets_with_trafos",
      envir = extra_environment,
      inherits = FALSE
    )[[1L]],
    child_alias
  )
  expect_identical(
    get(
      "sets_with_constraints",
      envir = constraint_environment,
      inherits = FALSE
    )[[1L]],
    child_alias
  )

  upgraded_extra = upgraded$extra_trafo
  upgraded_constraint = upgraded$constraint
  expect_true(paradox:::.paradox_has_srcref(upgraded_extra))
  expect_true(paradox:::.paradox_has_srcref(upgraded_constraint))
  expect_true(paradox:::.paradox_has_srcref(get(
    "psc_extra_trafo",
    envir = environment(upgraded_extra),
    inherits = FALSE
  )))
  expect_true(paradox:::.paradox_has_srcref(get(
    "psc_constraint",
    envir = environment(upgraded_constraint),
    inherits = FALSE
  )))
  expect_false(identical(
    environment(upgraded_extra),
    extra_environment
  ))
  expect_false(identical(
    environment(upgraded_constraint),
    constraint_environment
  ))
  upgraded_extra_child = get(
    "sets_with_trafos",
    envir = environment(upgraded_extra),
    inherits = FALSE
  )[[1L]]
  upgraded_constraint_child = get(
    "sets_with_constraints",
    envir = environment(upgraded_constraint),
    inherits = FALSE
  )[[1L]]
  expect_false(identical(upgraded_extra_child, child_alias))
  expect_identical(upgraded_extra_child, upgraded_constraint_child)
  expect_identical(upgraded_extra(list(x = 1L)), list(x = 1L))
  expect_true(upgraded_constraint(list(x = 1L)))
})

test_that("additive owner migration composes inherited callback carriers", {
  legacy_child = legacy_base_from_current(ps(y = p_int()))
  carrier_info = list(
    extra_trafo = list(child = legacy_child),
    constraint = NULL
  )
  info = list(
    kind = "owner",
    owner = list(migration_kind = "additive"),
    base = list(callback_carriers = carrier_info),
    dependencies = structure(list(), names = character())
  )

  dependencies = paradox:::.upgrade_paradox_info_dependencies(info)
  expect_identical(names(dependencies), "extra_trafo_1")
  expect_identical(dependencies[[1L]], legacy_child)
  split = paradox:::.upgrade_paradox_split_owner_dependencies(
    info,
    dependencies,
    "x"
  )
  expect_identical(split$base, dependencies)
  expect_identical(split$owner, structure(list(), names = character()))

  current_child = ps(y = p_int())
  callback_environment = list2env(
    list(sets_with_trafos = list(child = legacy_child)),
    parent = baseenv()
  )
  callback = eval(
    quote(function(x) x),
    envir = callback_environment
  )
  prepared = ps(x = p_int(), .extra_trafo = callback)
  paradox:::.upgrade_paradox_rebase_prepared(
    info,
    prepared,
    list(extra_trafo_1 = current_child),
    "x"
  )
  rebound = get(
    "sets_with_trafos",
    envir = environment(prepared$extra_trafo),
    inherits = FALSE
  )
  expect_identical(rebound, list(child = current_child))
})

test_that("graph migration rebinds shared callback carrier identities", {
  skip_if_no_active_binding_inspection()
  child = transplantable_legacy_base_from_current(ps(y = p_int()))
  parent = transplantable_legacy_base_from_current(ps(x = p_int()))
  private = mlr3misc::get_private(parent)
  private$.extra_trafo = legacy_detached_extra_trafo(
    list(child = child)
  )
  private$.constraint = legacy_detached_constraint(
    list(child = child)
  )
  parent_alias = parent
  child_alias = child
  host = list(parent = parent, child = child)

  expect_identical(upgrade_paradox_object_graph(host), host)
  expect_identical(host$parent, parent_alias)
  expect_identical(host$child, child_alias)
  expect_true(exists(
    ".core",
    envir = mlr3misc::get_private(parent_alias),
    inherits = FALSE
  ))
  expect_true(exists(
    ".core",
    envir = mlr3misc::get_private(child_alias),
    inherits = FALSE
  ))
  expect_identical(
    get(
      "sets_with_trafos",
      envir = environment(parent_alias$extra_trafo),
      inherits = FALSE
    ),
    list(child = child_alias)
  )
  expect_identical(
    get(
      "sets_with_constraints",
      envir = environment(parent_alias$constraint),
      inherits = FALSE
    ),
    list(child = child_alias)
  )
  expect_identical(parent_alias$extra_trafo(list(x = 1L)), list(x = 1L))
  expect_true(parent_alias$constraint(list(x = 1L)))

  cyclic = transplantable_legacy_base_from_current(ps(x = p_int()))
  cyclic_private = mlr3misc::get_private(cyclic)
  cyclic_private$.extra_trafo = legacy_detached_extra_trafo(
    list(self = cyclic)
  )
  cyclic_bytes = serialize(cyclic, NULL)
  expect_error(
    upgrade_paradox_object_graph(cyclic),
    "callback_dependencies.*cycle reaches active node"
  )
  expect_identical(serialize(cyclic, NULL), cyclic_bytes)
  expect_false(exists(
    ".core",
    envir = cyclic_private,
    inherits = FALSE
  ))
})

test_that("legacy callback carrier shells reject dispatch and S4", {
  skip_if_no_active_binding_inspection()
  child = legacy_base_from_current(ps(y = p_int()))
  class_dispatches = 0L
  length.paradox_hostile_carriers = function(x) {
    class_dispatches <<- class_dispatches + 1L
    NextMethod()
  }
  classed_carriers = structure(
    list(child = child),
    class = "paradox_hostile_carriers"
  )
  classed = legacy_base_from_current(ps(x = p_int()))
  classed_private = mlr3misc::get_private(classed)
  classed_private$.extra_trafo =
    legacy_detached_extra_trafo(classed_carriers)
  expect_error(
    upgrade_paradox_object(classed),
    "sets_with_trafos.*ordinary list"
  )
  expect_identical(class_dispatches, 0L)

  s4 = legacy_base_from_current(ps(x = p_int()))
  s4_private = mlr3misc::get_private(s4)
  s4_private$.extra_trafo = legacy_detached_extra_trafo(
    asS4(list(child = child))
  )
  expect_error(
    upgrade_paradox_object(s4),
    "sets_with_trafos.*ordinary list"
  )
})

test_that("legacy callback carrier shells reject ALTREP", {
  skip_if_no_list_altrep()
  child = legacy_base_from_current(ps(y = p_int()))
  altrep_observations = 0L
  altrep_carriers = native_stateful_altrep(
    list(child = child),
    list(child = child),
    callback = function() {
      altrep_observations <<- altrep_observations + 1L
    },
    callback_after = 0L,
    duplicate_returns_self = TRUE
  )
  altrep = legacy_base_from_current(ps(x = p_int()))
  altrep_private = mlr3misc::get_private(altrep)
  altrep_private$.extra_trafo =
    legacy_detached_extra_trafo(altrep_carriers)
  native_stateful_altrep_rearm(altrep_carriers, callback_after = 0L)
  expect_error(
    upgrade_paradox_object(altrep),
    "sets_with_trafos.*ordinary list"
  )
  expect_identical(altrep_observations, 0L)
})

test_that("legacy collection sharing is preserved and cycles are rejected", {
  skip_if_no_active_binding_inspection()
  child = ps(x = p_int(0, 4, tags = "shared"))
  child$values = list(x = 3L)
  current = ParamSetCollection$new(
    list(a = child, b = child),
    tag_sets = TRUE,
    tag_params = TRUE
  )
  paradox:::param_set_core_replace(
    mlr3misc::get_private(current),
    deps = data.table::data.table(
      id = "a.x",
      on = "b.x",
      cond = list(CondEqual(3L))
    )
  )

  legacy_child = legacy_base_from_current(child)
  legacy = legacy_collection_from_current(
    current,
    list(a = legacy_child, b = legacy_child)
  )
  upgraded = upgrade_paradox_object(legacy)
  state = paradox:::param_set_core_state(mlr3misc::get_private(upgraded))
  expect_s3_class(upgraded, "ParamSetCollection")
  expect_identical(state$.sets[[1L]], state$.sets[[2L]])
  expect_identical(
    paradox:::param_set_core_state(
      mlr3misc::get_private(state$.sets[[1L]])
    )$.values,
    list(x = 3L)
  )
  expect_identical(state$.deps$id, "a.x")
  expect_identical(state$.deps$on, "b.x")
  expect_identical(
    state$.tags$tag[state$.tags$id == "a.x"],
    c("shared", "set_a", "param_x")
  )

  legacy_private = mlr3misc::get_private(legacy)
  legacy_private$.sets[[1L]] = legacy
  expect_error(
    upgrade_paradox_object(legacy),
    "x\\$sets\\[\\[1\\]\\].*cycle reaches active node at x"
  )
})

test_that("recursive migration transplants shared legacy collections in place", {
  skip_if_no_active_binding_inspection()
  child = ps(x = p_int(0, 4, init = 2L), enabled = p_lgl(init = TRUE))
  child$values = list(x = 3L, enabled = TRUE)
  current = ParamSetCollection$new(list(left = child, right = child))
  legacy_child = transplantable_legacy_base_from_current(child)
  legacy = transplantable_legacy_collection_from_current(
    current,
    list(left = legacy_child, right = legacy_child)
  )
  collection_alias = legacy
  child_alias = legacy_child
  host = list(collection = legacy, child = legacy_child)

  expect_identical(upgrade_paradox_object_graph(host), host)
  expect_identical(host$collection, collection_alias)
  expect_identical(host$child, child_alias)
  expect_identical(legacy$sets[[1L]], child_alias)
  expect_identical(legacy$sets[[2L]], child_alias)
  expect_identical(
    legacy$values,
    list(
      left.x = 3L,
      left.enabled = TRUE,
      right.x = 3L,
      right.enabled = TRUE
    )
  )

  clone = legacy$clone(deep = TRUE)
  expect_false(identical(clone, legacy))
  expect_identical(clone$values, legacy$values)
  restored = unserialize(serialize(legacy, NULL))
  expect_identical(restored$values, legacy$values)
})

test_that("a partially refreshed shell remains discoverable and retryable", {
  skip_if_no_active_binding_inspection()
  legacy = transplantable_legacy_base_from_current(
    ps(x = p_dbl(0, 1))
  )
  old_enclosure = legacy$.__enclos_env__
  session = paradox:::.upgrade_paradox_prepare_session(list(legacy), "x")
  index = session$commit_order[[1L]]
  plan = paradox:::.upgrade_paradox_transplant_plan(
    legacy,
    session$prepared[[index]],
    "x"
  )

  # Model a catastrophic allocation failure between two public binding
  # replacements. The refreshed closure already points at a canonical current
  # private capsule, but the legacy shell's enclosure is still the authoritative
  # completion marker.
  for (enclosing in plan$enclosures) {
    assign("self", legacy, envir = enclosing)
  }
  position = match("ids", plan$current_names)
  paradox:::.upgrade_paradox_replace_binding(
    legacy,
    "ids",
    plan$current_values[[position]],
    plan$current_shape$active[[position]],
    plan$current_shape$locked[[position]]
  )
  # Also model failure after replacement but before the lock bit is restored.
  unlockBinding("ids", legacy)
  expect_false(bindingIsLocked("ids", legacy))
  expect_identical(legacy$.__enclos_env__, old_enclosure)

  discovery = .Call(paradox:::C_upgrade_graph_discover, legacy)
  expect_identical(discovery$objects, list(legacy))
  expect_identical(upgrade_paradox_object_graph(legacy), legacy)
  expect_identical(legacy$ids(), "x")
  expect_true(bindingIsLocked("ids", legacy))
  expect_false(identical(legacy$.__enclos_env__, old_enclosure))
})

test_that("legacy extensions, replacements, and malformed state fail closed", {
  skip_if_no_active_binding_inspection()
  hostile_flag_dispatches = 0L
  length.paradox_hostile_flag = function(x) {
    hostile_flag_dispatches <<- hostile_flag_dispatches + 1L
    1L
  }
  is.na.paradox_hostile_flag = function(x) {
    hostile_flag_dispatches <<- hostile_flag_dispatches + 1L
    FALSE
  }
  hostile_flag = transplantable_legacy_base_from_current(ps(x = p_dbl()))
  hostile_flag$assert_values = structure(
    TRUE,
    class = "paradox_hostile_flag"
  )
  expect_error(
    upgrade_paradox_object(hostile_flag),
    "`assert_values` is malformed",
    fixed = TRUE
  )
  expect_identical(hostile_flag_dispatches, 0L)

  legacy = legacy_base_from_current(ps(x = p_dbl()))
  unlockBinding("ids", legacy)
  legacy$ids = function(...) "replacement"
  lockBinding("ids", legacy)
  expect_error(
    upgrade_paradox_object(legacy),
    "core method `ids` was replaced"
  )

  base_generator = legacy_upgrader_generator("base")
  subclass_generator = R6::R6Class(
    "LegacySubclass",
    inherit = base_generator,
    parent_env = environment()
  )
  subclass = subclass_generator$new()
  expect_error(
    upgrade_paradox_object(subclass),
    "legacy third-party subclasses are unsupported"
  )

  unknown_condition = legacy_base_from_current(
    ps(x = p_dbl(), y = p_dbl())$add_dep("y", "x", CondEqual(1))
  )
  unknown_private = mlr3misc::get_private(unknown_condition)
  class(unknown_private$.deps$cond[[1L]]) = c(
    "UnknownCondition",
    "Condition"
  )
  expect_error(
    upgrade_paradox_object(unknown_condition),
    "x\\$private\\$\\.deps\\$cond\\[\\[1\\]\\].*unsupported Condition"
  )

  observed = 0L
  active_state = legacy_base_from_current(ps(x = p_dbl()))
  active_private = copy_legacy_private_environment(active_state)
  rm(".values", envir = active_private)
  makeActiveBinding(".values", function() {
    observed <<- observed + 1L
    list()
  }, active_private)
  expect_error(
    upgrade_paradox_object(active_state),
    "binding `\\.values` must not be active"
  )
  expect_identical(observed, 0L)

  delayed_state = legacy_base_from_current(ps(x = p_dbl()))
  delayed_private = copy_legacy_private_environment(delayed_state)
  rm(".values", envir = delayed_private)
  delayedAssign(
    ".values",
    {
      observed <<- observed + 1L
      list()
    },
    assign.env = delayed_private,
    eval.env = environment()
  )
  expect_error(
    upgrade_paradox_object(delayed_state),
    "binding `\\.values` is delayed or malformed"
  )
  expect_identical(observed, 0L)

  literal_values = list(
    logical = TRUE,
    null = NULL,
    environment = new.env(parent = emptyenv()),
    closure = function() NULL,
    externalptr = new("externalptr")
  )
  for (index in seq_along(literal_values)) {
    literal_state = legacy_base_from_current(ps(x = p_dbl()))
    literal_private = copy_legacy_private_environment(literal_state)
    rm(".values", envir = literal_private)
    make_delayed_literal_binding(
      literal_private,
      ".values",
      literal_values[[index]]
    )
    expect_error(
      upgrade_paradox_object(literal_state),
      "binding `\\.values` is delayed or malformed",
      info = names(literal_values)[[index]]
    )
  }

  corrupt = ps(x = p_dbl())
  corrupt_private = mlr3misc::get_private(corrupt)
  corrupt_private$.core = list(forged = TRUE)
  expect_error(
    upgrade_paradox_object(corrupt),
    "unknown or corrupt current state capsule"
  )

  forged = ps(x = p_dbl())
  paradox:::param_set_core_replace(
    mlr3misc::get_private(forged),
    values = list(unknown = 1)
  )
  expect_error(
    upgrade_paradox_object(forged),
    "corrupt current state capsule"
  )

  expect_error(
    upgrade_paradox_object_graph(forged),
    "corrupt current state capsule"
  )

  wrong_kind = ps(x = p_dbl())
  class(wrong_kind) = c("ParamSetCollection", "ParamSet", "R6")
  expect_error(
    upgrade_paradox_object_graph(wrong_kind),
    "state capsule kind disagrees with shell class"
  )

  hybrid_collection = ParamSetCollection$new(list(ps(x = p_dbl())))
  class(hybrid_collection) = c(
    "ParamSetShadow", "ParamSetCollection", "ParamSet", "R6"
  )
  expect_error(
    upgrade_paradox_object_graph(hybrid_collection),
    "expected a well-formed ordinary ParamSet-family R6 class suffix"
  )

  hybrid_shadow = ParamSetShadow$new(ps(x = p_dbl()), character())
  class(hybrid_shadow) = c(
    "ParamSetCollection", "ParamSetShadow", "ParamSet", "R6"
  )
  expect_error(
    upgrade_paradox_object_graph(hybrid_shadow),
    "expected a well-formed ordinary ParamSet-family R6 class suffix"
  )
})

test_that("joint commit validation protects earlier preflighted roots", {
  skip_if_no_active_binding_inspection()
  legacy = transplantable_legacy_base_from_current(ps(x = p_dbl()))
  old_enclosure = legacy$.__enclos_env__
  before = serialize(legacy, NULL)
  current = ps(x = p_dbl())
  session = paradox:::.upgrade_paradox_prepare_session(
    list(legacy, current),
    c("legacy", "current")
  )

  paradox:::param_set_core_replace(
    mlr3misc::get_private(current),
    values = list(unknown = 1)
  )
  expect_error(
    paradox:::.upgrade_paradox_commit_session(session),
    "joint validation",
    fixed = TRUE
  )
  expect_identical(serialize(legacy, NULL), before)
  expect_identical(legacy$.__enclos_env__, old_enclosure)
})

test_that("per-rebase joint validation protects unrelated live roots", {
  skip_if_no_active_binding_inspection()
  legacy = transplantable_legacy_base_from_current(ps(x = p_dbl()))
  old_enclosure = legacy$.__enclos_env__
  before = serialize(legacy, NULL)
  current = ps(x = p_dbl())
  session = paradox:::.upgrade_paradox_prepare_session(
    list(legacy, current),
    c("legacy", "current")
  )

  namespace = asNamespace("paradox")
  binding = ".upgrade_paradox_rebase_prepared"
  original = get(binding, envir = namespace, inherits = FALSE)
  was_locked = bindingIsLocked(binding, namespace)
  if (was_locked) unlockBinding(binding, namespace)
  on.exit({
    if (bindingIsLocked(binding, namespace)) {
      unlockBinding(binding, namespace)
    }
    assign(binding, original, envir = namespace)
    if (was_locked) lockBinding(binding, namespace)
  }, add = TRUE)
  assign(
    binding,
    function(...) {
      paradox:::param_set_core_replace(
        mlr3misc::get_private(current),
        values = list(unknown = 1)
      )
      original(...)
    },
    envir = namespace
  )
  if (was_locked) lockBinding(binding, namespace)

  expect_error(
    paradox:::.upgrade_paradox_commit_session(session),
    "joint validation",
    fixed = TRUE
  )
  expect_identical(serialize(legacy, NULL), before)
  expect_identical(legacy$.__enclos_env__, old_enclosure)
})

test_that("post-transplant validation detects external mutation without rollback", {
  skip_if_no_active_binding_inspection()
  legacy = transplantable_legacy_base_from_current(ps(x = p_dbl()))
  old_enclosure = legacy$.__enclos_env__
  current = ps(x = p_dbl())
  session = paradox:::.upgrade_paradox_prepare_session(
    list(legacy, current),
    c("legacy", "current")
  )

  namespace = asNamespace("paradox")
  binding = ".upgrade_paradox_transplant"
  original = get(binding, envir = namespace, inherits = FALSE)
  was_locked = bindingIsLocked(binding, namespace)
  if (was_locked) unlockBinding(binding, namespace)
  on.exit({
    if (bindingIsLocked(binding, namespace)) {
      unlockBinding(binding, namespace)
    }
    assign(binding, original, envir = namespace)
    if (was_locked) lockBinding(binding, namespace)
  }, add = TRUE)
  assign(
    binding,
    function(plan) {
      result = original(plan)
      paradox:::param_set_core_replace(
        mlr3misc::get_private(current),
        values = list(unknown = 1)
      )
      result
    },
    envir = namespace
  )
  if (was_locked) lockBinding(binding, namespace)

  expect_error(
    paradox:::.upgrade_paradox_commit_session(session),
    "joint validation",
    fixed = TRUE
  )
  expect_false(identical(legacy$.__enclos_env__, old_enclosure))
  expect_identical(
    .Call(
      paradox:::C_param_set_core_kind,
      mlr3misc::get_private(legacy)
    ),
    1L
  )
})

test_that("corrupt current roots abort graph preflight before legacy mutation", {
  skip_if_no_active_binding_inspection()
  legacy = transplantable_legacy_base_from_current(ps(x = p_dbl()))
  old_enclosure = legacy$.__enclos_env__
  before = serialize(legacy, NULL)

  origin = ps(hidden = p_lgl(), visible = p_int(), flag = p_lgl())
  shadow = ParamSetShadow$new(origin, "hidden")
  origin$add_dep("hidden", "flag", CondEqual(TRUE))
  expect_error(
    upgrade_paradox_object_graph(list(legacy = legacy, shadow = shadow)),
    "reach across shadow bounds"
  )
  expect_identical(serialize(legacy, NULL), before)
  expect_identical(legacy$.__enclos_env__, old_enclosure)

  corrupt = ps(x = p_dbl())
  paradox:::param_set_core_replace(
    mlr3misc::get_private(corrupt),
    values = list(unknown = 1)
  )
  expect_error(
    upgrade_paradox_object_graph(list(legacy = legacy, corrupt = corrupt)),
    "corrupt current state capsule"
  )
  expect_identical(serialize(legacy, NULL), before)
  expect_identical(legacy$.__enclos_env__, old_enclosure)
})

test_that("pinned mbo_config search spaces are explicit upgrade fixtures", {
  root = Sys.getenv("PARADOX_MBO_CONFIG_ROOT", unset = "")
  skip_if(!nzchar(root), "PARADOX_MBO_CONFIG_ROOT is not set")
  files = file.path(root, c("mixed_search_space.rds", "numeric_search_space.rds"))
  expect_true(all(file.exists(files)))

  expected_ids = c(12L, 17L)
  expected_deps = c(3L, 9L)
  for (index in seq_along(files)) {
    legacy = readRDS(files[[index]])
    before = serialize(legacy, NULL)
    before_enclosure = legacy$.__enclos_env__
    if (getRversion() < "4.0.0") {
      error = tryCatch(
        {
          upgrade_paradox_object(legacy)
          NULL
        },
        error = function(error) error
      )
      expect_s3_class(error, "error")
      expect_match(
        conditionMessage(error),
        "legacy ParamSet migration requires R >= 4.0.0",
        fixed = TRUE
      )
      expect_identical(serialize(legacy, NULL), before)
      expect_identical(legacy$.__enclos_env__, before_enclosure)
    } else {
      upgraded = upgrade_paradox_object(legacy)
      expect_identical(serialize(legacy, NULL), before)
      expect_length(upgraded$ids(), expected_ids[[index]])
      expect_equal(nrow(upgraded$deps), expected_deps[[index]])
    }
  }
})

test_that("Paradox 1 numeric bound storage is normalized, not rejected", {
  # Paradox 1 stored the bound arguments verbatim, so `p_int(1L, 10L)` kept an
  # integer `lower`/`upper`/`tolerance`. Refusing that shape made an ordinary
  # legacy Domain unmigratable.
  domain = p_int(1L, 10L)
  legacy = setDT(copy(unclass(domain)))
  for (name in c("lower", "upper", "tolerance")) {
    set(legacy, j = name, value = as.integer(legacy[[name]]))
  }
  class(legacy) = class(domain)
  expect_identical(typeof(legacy$lower), "integer")

  upgraded = upgrade_paradox_object(legacy)
  expect_identical(upgraded$lower, 1L)
  expect_identical(upgraded$upper, 10L)
  expect_identical(upgraded$id, domain$id)
})

test_that("upgrading a Domain without depends is shape-preserving", {
  # A Domain without `depends` stores NULL requirements, which is what the
  # current constructors produce; an empty list denormalized the column.
  for (domain in list(p_dbl(0, 1), p_int(1L, 10L), p_fct(c("a", "b")),
      p_lgl(), p_uty())) {
    expect_identical(upgrade_paradox_object(domain), domain)
  }
})

test_that("upgraded collections keep generating the tags Paradox 1 recorded", {
  child = ps(x = p_int(), y = p_lgl())
  other = ps(z = p_dbl())
  current = ParamSetCollection$new(
    list(a = child, b = other),
    tag_sets = TRUE,
    tag_params = TRUE
  )
  legacy = legacy_collection_from_current(
    current,
    list(a = legacy_base_from_current(child), b = legacy_base_from_current(other))
  )
  upgraded = upgrade_paradox_object(legacy)
  expect_setequal(upgraded$tags$a.x, c("set_a", "param_x"))
  expect_setequal(upgraded$tags$b.z, c("set_b", "param_z"))

  # Paradox 1 consumed the flags and kept only their output, so the upgrade has
  # to read them back: once a contained set changes, the tags are derived again
  # and a lost flag would silently drop them.
  state = paradox:::param_set_core_state(mlr3misc::get_private(upgraded))
  state$.sets[[1L]]$tags = list(x = "kept", y = character())
  expect_setequal(upgraded$tags$a.x, c("set_a", "param_x", "kept"))
  expect_setequal(upgraded$tags$b.z, c("set_b", "param_z"))
})
