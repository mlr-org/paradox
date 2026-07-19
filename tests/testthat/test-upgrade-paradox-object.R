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

copy_legacy_private_environment = function(x) {
  enclosing = x$.__enclos_env__
  source = mlr3misc::get_private(x)
  result = new.env(parent = parent.env(source))
  names = ls(source, all.names = TRUE)
  for (name in names) {
    assign(name, get(name, envir = source, inherits = FALSE), envir = result)
  }
  enclosing$private = result
  result
}

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

test_that("legacy collection sharing is preserved and cycles are rejected", {
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

test_that("legacy extensions, replacements, and malformed state fail closed", {
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
    upgraded = upgrade_paradox_object(legacy)
    expect_identical(serialize(legacy, NULL), before)
    expect_length(upgraded$ids(), expected_ids[[index]])
    expect_equal(nrow(upgraded$deps), expected_deps[[index]])
  }
})
