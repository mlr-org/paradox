sampler_shell_symbols = function() {
  namespace = asNamespace("paradox")
  list(
    factory = get(
      "C_sampler_1d_unif_bulk_shells",
      envir = namespace,
      inherits = FALSE
    ),
    auth = get(
      "C_sampler_1d_unif_bulk_auth",
      envir = namespace,
      inherits = FALSE
    ),
    token_probe = get(
      "C_param_set_adopt_subset_state",
      envir = namespace,
      inherits = FALSE
    )
  )
}

sampler_shell_generators = function() {
  get(
    "sampler_1d_unif_generators",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )()
}

sampler_shell_available = function() {
  namespace = asNamespace("paradox")
  symbols = c(
    "C_sampler_1d_unif_bulk_shells",
    "C_sampler_1d_unif_bulk_auth",
    "C_param_set_adopt_subset_state"
  )
  all(vapply(
    symbols,
    exists,
    logical(1L),
    envir = namespace,
    inherits = FALSE
  )) && isTRUE(.Call(
    sampler_shell_symbols()$auth,
    sampler_shell_generators()
  ))
}

sampler_shell_plans = function(param_set, ids = param_set$ids()) {
  get(
    "param_set_subspace_plans",
    envir = asNamespace("paradox"),
    inherits = FALSE
  )(
    param_set,
    param_set$.__enclos_env__$private,
    ids,
    param_set$values
  )
}

sampler_shell_call = function(param_set, ids = param_set$ids(),
    generators = sampler_shell_generators(), values = param_set$values) {
  plans = sampler_shell_plans(param_set, ids)
  list(
    plans = plans,
    result = .Call(
      sampler_shell_symbols()$factory,
      get("ParamSet", envir = asNamespace("paradox"), inherits = FALSE),
      generators,
      plans,
      values
    )
  )
}

sampler_shell_reference = function(param_set, ids = param_set$ids()) {
  lapply(param_set$subspaces(ids), Sampler1DUnif$new)
}

sampler_shell_expect_table = function(observed, expected) {
  expect_identical(names(observed), names(expected))
  expect_identical(class(observed), class(expected))
  expect_identical(nrow(observed), nrow(expected))
  expect_identical(lapply(observed, identity), lapply(expected, identity))
  expect_identical(data.table::key(observed), data.table::key(expected))
  expect_identical(data.table::indices(observed), data.table::indices(expected))
  expect_identical(
    data.table:::selfrefok(observed, FALSE),
    data.table:::selfrefok(expected, FALSE)
  )
}

sampler_shell_expect_param_set = function(observed, expected) {
  expect_identical(class(observed), c("ParamSet", "R6"))
  expect_identical(class(observed), class(expected))
  observed_private = observed$.__enclos_env__$private
  expected_private = expected$.__enclos_env__$private
  for (field in c(".params", ".tags", ".trafos", ".deps")) {
    sampler_shell_expect_table(
      observed_private[[field]],
      expected_private[[field]]
    )
  }
  expect_identical(observed_private$.values, expected_private$.values)
  expect_identical(observed$assert_values, expected$assert_values)
  expect_identical(observed$constraint, expected$constraint)
  expect_identical(observed$extra_trafo, expected$extra_trafo)
}

sampler_shell_graph = function(object) {
  self = list()
  enclosure = list()
  cursor = object
  repeat {
    self[[length(self) + 1L]] = cursor
    current_enclosure = get(
      ".__enclos_env__",
      envir = cursor,
      inherits = FALSE
    )
    enclosure[[length(enclosure) + 1L]] = current_enclosure
    if (!exists("super", envir = current_enclosure, inherits = FALSE)) {
      break
    }
    cursor = get("super", envir = current_enclosure, inherits = FALSE)
    if (length(self) > 8L) stop("unexpected R6 inheritance cycle")
  }
  list(
    self = self,
    enclosure = enclosure,
    private = get("private", envir = enclosure[[1L]], inherits = FALSE)
  )
}

sampler_shell_core_environments = function(graph) {
  result = list()
  for (index in seq_along(graph$self)) {
    result[[sprintf("self.%d", index)]] = graph$self[[index]]
    result[[sprintf("enclosure.%d", index)]] = graph$enclosure[[index]]
  }
  result$private = graph$private
  result
}

sampler_shell_binding_value = function(environment, name) {
  if (bindingIsActive(name, environment)) {
    activeBindingFunction(name, environment)
  } else {
    get(name, envir = environment, inherits = FALSE)
  }
}

sampler_shell_environment_occurrences = function(graph) {
  result = list()
  for (index in seq_along(graph$self)) {
    self = graph$self[[index]]
    enclosure = graph$enclosure[[index]]
    prefix = sprintf("slice.%d", index)
    result[[paste0(prefix, "/self")]] = self
    result[[paste0(prefix, "/self/.__enclos_env__")]] = get(
      ".__enclos_env__",
      envir = self,
      inherits = FALSE
    )
    result[[paste0(prefix, "/enclosure")]] = enclosure
    result[[paste0(prefix, "/enclosure/self")]] = get(
      "self",
      envir = enclosure,
      inherits = FALSE
    )
    result[[paste0(prefix, "/enclosure/private")]] = get(
      "private",
      envir = enclosure,
      inherits = FALSE
    )
    if (exists("super", envir = enclosure, inherits = FALSE)) {
      result[[paste0(prefix, "/enclosure/super")]] = get(
        "super",
        envir = enclosure,
        inherits = FALSE
      )
    }
    if (exists("param", envir = self, inherits = FALSE) &&
        bindingIsActive("param", self)) {
      result[[paste0(prefix, "/active-param-value")]] = get(
        "param",
        envir = self,
        inherits = FALSE
      )
    }
  }
  result[["root/param_set"]] = get(
    "param_set",
    envir = graph$self[[1L]],
    inherits = FALSE
  )
  result
}

sampler_shell_pointer_matrix = function(objects) {
  addresses = vapply(objects, data.table::address, character(1L))
  outer(addresses, addresses, `==`)
}

sampler_shell_closure_environment = function(closure, graph) {
  environment = environment(closure)
  core = sampler_shell_core_environments(graph)
  for (name in names(core)) {
    if (identical(environment, core[[name]])) return(name)
  }
  namespace = asNamespace("paradox")
  if (identical(environment, namespace)) return("namespace:paradox")
  if (identical(environment, baseenv())) return("namespace:base")
  if (identical(environment, emptyenv())) return("empty")
  paste0("external:", environmentName(environment))
}

sampler_shell_closures = function(graph) {
  result = list()
  core = sampler_shell_core_environments(graph)
  for (environment_name in names(core)) {
    environment = core[[environment_name]]
    binding_names = ls(environment, all.names = TRUE, sorted = FALSE)
    for (binding_name in binding_names) {
      value = sampler_shell_binding_value(environment, binding_name)
      path = paste(environment_name, binding_name, sep = "/")
      if (is.function(value)) result[[path]] = value
      if (identical(binding_name, ".__active__")) {
        for (active_name in names(value)) {
          result[[paste(path, active_name, sep = "/")]] = value[[active_name]]
        }
      }
    }
  }
  result
}

sampler_shell_expect_closure = function(observed, expected,
    observed_graph, expected_graph, info = NULL) {
  expect_identical(typeof(observed), typeof(expected), info = info)
  expect_identical(formals(observed), formals(expected), info = info)
  expect_identical(body(observed), body(expected), info = info)
  expect_identical(attributes(observed), attributes(expected), info = info)
  expect_identical(
    sampler_shell_closure_environment(observed, observed_graph),
    sampler_shell_closure_environment(expected, expected_graph),
    info = info
  )
}

sampler_shell_expect_environment = function(observed, expected,
    observed_graph, expected_graph, info = NULL) {
  expect_identical(attributes(observed), attributes(expected), info = info)
  expect_identical(class(observed), class(expected), info = info)
  expect_identical(parent.env(observed), parent.env(expected), info = info)
  expect_identical(
    environmentIsLocked(observed),
    environmentIsLocked(expected),
    info = info
  )
  expect_identical(env.profile(observed), env.profile(expected), info = info)
  observed_names = ls(observed, all.names = TRUE, sorted = FALSE)
  expected_names = ls(expected, all.names = TRUE, sorted = FALSE)
  expect_identical(observed_names, expected_names, info = info)
  expect_identical(
    ls(observed, all.names = TRUE, sorted = TRUE),
    ls(expected, all.names = TRUE, sorted = TRUE),
    info = info
  )

  for (name in observed_names) {
    binding_info = paste(info, name, sep = "/")
    expect_identical(
      bindingIsActive(name, observed),
      bindingIsActive(name, expected),
      info = binding_info
    )
    expect_identical(
      bindingIsLocked(name, observed),
      bindingIsLocked(name, expected),
      info = binding_info
    )
    observed_value = sampler_shell_binding_value(observed, name)
    expected_value = sampler_shell_binding_value(expected, name)
    expect_identical(typeof(observed_value), typeof(expected_value),
      info = binding_info)
    if (is.function(expected_value)) {
      sampler_shell_expect_closure(
        observed_value,
        expected_value,
        observed_graph,
        expected_graph,
        binding_info
      )
    } else if (is.environment(expected_value)) {
      expect_identical(class(observed_value), class(expected_value),
        info = binding_info)
    } else if (identical(name, ".__active__")) {
      expect_identical(names(observed_value), names(expected_value),
        info = binding_info)
      expect_identical(attributes(observed_value), attributes(expected_value),
        info = binding_info)
      expect_identical(
        vapply(observed_value, typeof, character(1L)),
        vapply(expected_value, typeof, character(1L)),
        info = binding_info
      )
    } else {
      expect_identical(observed_value, expected_value, info = binding_info)
    }
  }
}

sampler_shell_expect_graph = function(observed, expected) {
  observed_graph = sampler_shell_graph(observed)
  expected_graph = sampler_shell_graph(expected)
  expect_length(observed_graph$self, 3L)
  expect_length(observed_graph$self, length(expected_graph$self))

  observed_core = sampler_shell_core_environments(observed_graph)
  expected_core = sampler_shell_core_environments(expected_graph)
  expect_identical(names(observed_core), names(expected_core))
  for (name in names(observed_core)) {
    sampler_shell_expect_environment(
      observed_core[[name]],
      expected_core[[name]],
      observed_graph,
      expected_graph,
      name
    )
  }

  observed_occurrences = sampler_shell_environment_occurrences(observed_graph)
  expected_occurrences = sampler_shell_environment_occurrences(expected_graph)
  expect_identical(names(observed_occurrences), names(expected_occurrences))
  expect_identical(
    sampler_shell_pointer_matrix(observed_occurrences),
    sampler_shell_pointer_matrix(expected_occurrences)
  )

  observed_closures = sampler_shell_closures(observed_graph)
  expected_closures = sampler_shell_closures(expected_graph)
  expect_identical(names(observed_closures), names(expected_closures))
  for (name in names(observed_closures)) {
    sampler_shell_expect_closure(
      observed_closures[[name]],
      expected_closures[[name]],
      observed_graph,
      expected_graph,
      name
    )
  }
  expect_identical(
    sampler_shell_pointer_matrix(observed_closures),
    sampler_shell_pointer_matrix(expected_closures)
  )

  observed_param = get(
    "param_set",
    envir = observed_graph$self[[1L]],
    inherits = FALSE
  )
  expected_param = get(
    "param_set",
    envir = expected_graph$self[[1L]],
    inherits = FALSE
  )
  expect_identical(observed$param, observed_param)
  expect_identical(expected$param, expected_param)
  sampler_shell_expect_param_set(observed_param, expected_param)
}

sampler_shell_fixture = function() {
  transform = function(x) x + 10
  result = ps(
    double = p_dbl(
      -2,
      3,
      tags = c("numeric", "shared"),
      trafo = transform
    ),
    integer = p_int(-3, 4, tags = c("numeric", "shared")),
    factor = p_fct(c("slow", "fast"), tags = "choice"),
    logical = p_lgl(tags = "switch")
  )
  result$values = list(
    double = 0.5,
    integer = 2L,
    factor = "fast",
    logical = TRUE
  )
  result
}

test_that("reviewed R and R6 admit specialized Sampler1DUnif shells", {
  if (getRversion() >= "4.6.0" &&
      packageVersion("R6") == package_version("2.6.1")) {
    expect_true(sampler_shell_available())
  }
})

test_that("native Sampler1DUnif shells reproduce the complete R6 graph", {
  skip_if_not(sampler_shell_available())
  param_set = sampler_shell_fixture()
  ids = c("logical", "double", "factor", "integer")
  expected = sampler_shell_reference(param_set, ids)
  observed = sampler_shell_call(param_set, ids)$result

  expect_type(observed, "list")
  expect_named(observed, ids)
  for (index in seq_along(ids)) {
    sampler_shell_expect_graph(observed[[index]], expected[[index]])
    set.seed(1200L + index)
    expected_sample = expected[[index]]$sample(7L)$data
    set.seed(1200L + index)
    observed_sample = observed[[index]]$sample(7L)$data
    expect_identical(observed_sample, expected_sample, info = ids[[index]])
  }

  public = SamplerUnif$new(param_set)
  expect_named(public$samplers, param_set$ids())
  expected_public = sampler_shell_reference(param_set)
  for (index in seq_along(public$samplers)) {
    sampler_shell_expect_graph(
      public$samplers[[index]],
      expected_public[[index]]
    )
  }
})

test_that("native shells preserve historical clone and serialization graphs", {
  skip_if_not(sampler_shell_available())
  param_set = ps(x = p_dbl(-2, 3, init = 0.5, tags = "numeric"))
  observed = sampler_shell_call(param_set)$result[[1L]]
  expected = sampler_shell_reference(param_set)[[1L]]
  sampler_shell_expect_graph(observed, expected)

  variants = list(
    shallow = function(object) object$clone(deep = FALSE),
    deep = function(object) object$clone(deep = TRUE),
    serialized = function(object) {
      unserialize(serialize(object, NULL, version = 3L))
    }
  )
  for (name in names(variants)) {
    observed_variant = variants[[name]](observed)
    expected_variant = variants[[name]](expected)
    sampler_shell_expect_graph(observed_variant, expected_variant)
    expect_identical(observed_variant$param$values, list(x = 0.5),
      info = name)
  }
})

test_that("bulk shell siblings and their source mutate independently", {
  skip_if_not(sampler_shell_available())
  param_set = ps(
    x = p_dbl(-2, 3, init = 0.5),
    y = p_int(-3, 4, init = 2L)
  )
  observed = sampler_shell_call(param_set, c("x", "x", "y"))$result
  expect_length(observed, 3L)
  expect_false(identical(observed[[1L]], observed[[2L]]))
  expect_false(identical(observed[[1L]]$param, observed[[2L]]$param))

  first_private = observed[[1L]]$param$.__enclos_env__$private
  second_private = observed[[2L]]$param$.__enclos_env__$private
  data.table::set(first_private$.params, 1L, "lower", -100)
  first_param = observed[[1L]]$param
  first_param$values = list(x = 1)
  first_param$assert_values = FALSE

  expect_identical(second_private$.params$lower, -2)
  expect_identical(observed[[2L]]$param$values, list(x = 0.5))
  expect_true(observed[[2L]]$param$assert_values)
  expect_identical(param_set$lower[["x"]], -2)
  expect_identical(param_set$values$x, 0.5)
})

test_that("unsupported owned state declines to the complete R constructor", {
  skip_if_not(sampler_shell_available())
  token_probe = sampler_shell_symbols()$token_probe

  extra = ps(x = p_dbl(0, 1, init = 0.25))
  extra$extra_trafo = function(x, param_set) x
  extra_call = sampler_shell_call(extra)
  expect_null(extra_call$result)
  expect_true(.Call(token_probe, NULL, extra_call$plans[[1L]]$state))
  extra_fallback = Sampler1DUnif$new(extra_call$plans[[1L]]$state)
  expect_identical(extra_fallback$param$extra_trafo, extra$extra_trafo)

  Box = R6::R6Class(
    "NativeSamplerShellValueBox",
    public = list(
      value = NULL,
      initialize = function(value) self$value = value
    )
  )
  box = Box$new(42L)
  special = ps(x = p_dbl(0, 1, special_vals = list(box)))
  special$values = list(x = box)
  special_call = sampler_shell_call(special)
  expect_null(special_call$result)
  expect_true(.Call(token_probe, NULL, special_call$plans[[1L]]$state))
  special_fallback = Sampler1DUnif$new(special_call$plans[[1L]]$state)
  expect_false(identical(special_fallback$param$values$x, box))
  expect_identical(special_fallback$param$values$x$value, 42L)

  for (unsupported in list(
    ps(x = p_dbl()),
    ps(x = p_uty(custom_check = function(x) TRUE))
  )) {
    declined = sampler_shell_call(unsupported)
    expect_null(declined$result)
    expect_true(.Call(token_probe, NULL, declined$plans[[1L]]$state))
  }
})

test_that("malformed bulk requests decline atomically", {
  skip_if_not(sampler_shell_available())
  namespace = asNamespace("paradox")
  symbols = sampler_shell_symbols()
  param_set = ps(
    x = p_dbl(0, 1),
    y = p_int(0, 2),
    z = p_lgl()
  )
  generators = sampler_shell_generators()
  values = param_set$values

  plans = sampler_shell_plans(param_set)
  duplicate = structure(
    list(plans[[1L]], plans[[1L]]),
    names = c("x", "x")
  )
  expect_null(.Call(
    symbols$factory,
    get("ParamSet", envir = namespace, inherits = FALSE),
    generators,
    duplicate,
    values
  ))
  expect_true(.Call(symbols$token_probe, NULL, plans[[1L]]$state))

  plans = sampler_shell_plans(param_set)
  invalid = plans
  invalid[[2L]]$state = new("externalptr")
  expect_null(.Call(
    symbols$factory,
    get("ParamSet", envir = namespace, inherits = FALSE),
    generators,
    invalid,
    values
  ))
  for (plan in plans) {
    expect_true(.Call(symbols$token_probe, NULL, plan$state))
  }

  plans = sampler_shell_plans(param_set)
  invisible(ParamSet$new(plans[[2L]]$state))
  expect_null(.Call(
    symbols$factory,
    get("ParamSet", envir = namespace, inherits = FALSE),
    generators,
    plans,
    values
  ))
  expect_true(.Call(symbols$token_probe, NULL, plans[[1L]]$state))
  expect_true(.Call(symbols$token_probe, NULL, plans[[3L]]$state))
})

test_that("altered R6 generators decline without invoking replacements", {
  skip_if_not(sampler_shell_available())
  namespace = asNamespace("paradox")
  symbols = sampler_shell_symbols()
  param_set = ps(x = p_dbl(0, 1))
  plans = sampler_shell_plans(param_set)
  generator = get("Sampler1DUnif", envir = namespace, inherits = FALSE)
  original = get("new", envir = generator, inherits = FALSE)
  calls = 0L
  replacement = function(...) {
    calls <<- calls + 1L
    stop("altered generator ran", call. = FALSE)
  }
  on.exit(assign("new", original, envir = generator), add = TRUE)
  assign("new", replacement, envir = generator)

  expect_null(.Call(
    symbols$factory,
    get("ParamSet", envir = namespace, inherits = FALSE),
    sampler_shell_generators(),
    plans,
    param_set$values
  ))
  expect_identical(calls, 0L)
  expect_true(.Call(symbols$token_probe, NULL, plans[[1L]]$state))

  assign("new", original, envir = generator)
  expect_true(sampler_shell_available())
})

test_that("altered namespace and R6 capsule helpers decline inertly", {
  skip_if_not(sampler_shell_available())
  skip_if_not_installed("callr")

  for (target in c("initializer", "capsule")) {
    observed = callr::r(
      function(target) {
        library(paradox)
        namespace = asNamespace("paradox")
        param_generator = get(
          "ParamSet",
          envir = namespace,
          inherits = FALSE
        )
        generators = get(
          "sampler_1d_unif_generators",
          envir = namespace,
          inherits = FALSE
        )()
        auth = get(
          "C_sampler_1d_unif_bulk_auth",
          envir = namespace,
          inherits = FALSE
        )
        factory = get(
          "C_sampler_1d_unif_bulk_shells",
          envir = namespace,
          inherits = FALSE
        )
        token_probe = get(
          "C_param_set_adopt_subset_state",
          envir = namespace,
          inherits = FALSE
        )
        calls = 0L
        replacement = function(...) {
          calls <<- calls + 1L
          stop("altered helper ran", call. = FALSE)
        }

        if (identical(target, "initializer")) {
          name = ".__Sampler1DUnif__initialize"
          unlockBinding(name, namespace)
          assign(name, replacement, envir = namespace)
          lockBinding(name, namespace)
        } else {
          capsule = parent.env(param_generator)
          name = "all_named"
          if (bindingIsLocked(name, capsule)) unlockBinding(name, capsule)
          assign(name, replacement, envir = capsule)
        }

        param_set = ps(x = p_dbl(0, 1))
        values = param_set$values
        private = param_set$.__enclos_env__$private
        plans = get(
          "param_set_subspace_plans",
          envir = namespace,
          inherits = FALSE
        )(param_set, private, param_set$ids(), values)
        result = .Call(
          factory,
          param_generator,
          generators,
          plans,
          values
        )
        c(
          auth_declined = !isTRUE(.Call(auth, generators)),
          factory_declined = is.null(result),
          replacement_inert = identical(calls, 0L),
          token_intact = isTRUE(.Call(
            token_probe,
            NULL,
            plans[[1L]]$state
          ))
        )
      },
      args = list(target = target),
      libpath = .libPaths(),
      timeout = 20
    )
    expect_true(all(observed), info = target)
  }
})

test_that("specialized shell assembly survives allocation torture", {
  skip_if_not(sampler_shell_available())
  skip_on_cran()
  param_set = sampler_shell_fixture()
  plans = sampler_shell_plans(param_set)
  previous = gctorture2(25L, wait = 0L)
  on.exit(gctorture2(previous), add = TRUE)
  observed = .Call(
    sampler_shell_symbols()$factory,
    get("ParamSet", envir = asNamespace("paradox"), inherits = FALSE),
    sampler_shell_generators(),
    plans,
    param_set$values
  )
  restored = unserialize(serialize(observed[[1L]], NULL, version = 3L))
  cloned = observed[[2L]]$clone(deep = TRUE)
  gctorture2(previous)

  expect_named(observed, param_set$ids())
  expect_identical(restored$param$ids(), "double")
  expect_identical(cloned$param$ids(), "integer")
  expect_identical(restored$param$values, list(double = 0.5))
  expect_identical(cloned$param$values, list(integer = 2L))
})
