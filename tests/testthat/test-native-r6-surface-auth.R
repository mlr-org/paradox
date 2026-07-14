surface_auth_symbol = function() {
  getDLLRegisteredRoutines(
    getLoadedDLLs()[["paradox"]]
  )$.Call[["param_set_surface_auth"]]
}

surface_auth_replace_private = function(object, name, kind, state) {
  private = object$.__enclos_env__$private
  original = private[[name]]
  if (kind == "delayed") {
    evaluation_environment = new.env(parent = baseenv())
    evaluation_environment$original = original
    evaluation_environment$state = state
    delayedAssign(
      name,
      {
        state$reads = state$reads + 1L
        original
      },
      assign.env = private,
      eval.env = evaluation_environment
    )
  } else {
    # R6 locks the private environment against adding or removing names. Make
    # an equivalent unlocked enclosure so the ordinary binding can be replaced
    # with an active one. Generated public/private methods resolve `private`
    # through the R6 enclosure, so point both collection layers at the copy.
    private = list2env(
      as.list.environment(private, all.names = TRUE),
      parent = parent.env(private)
    )
    rm(list = name, envir = private)
    makeActiveBinding(
      name,
      local({
        value = original
        function(replacement) {
          if (!missing(replacement)) stop("test backing store is read-only")
          state$reads = state$reads + 1L
          value
        }
      }),
      private
    )
    enclosure = object$.__enclos_env__
    assign("private", private, envir = enclosure)
    if (exists("super", envir = enclosure, inherits = FALSE)) {
      assign(
        "private",
        private,
        envir = enclosure$super$.__enclos_env__
      )
    }
  }
  invisible(object)
}

test_that("canonical R6 surfaces are admitted only for their exact owner", {
  symbol = surface_auth_symbol()
  param_set = ps(x = p_dbl(0, 1, trafo = sqrt))
  collection = ParamSetCollection$new(list(component = ps(x = p_dbl(0, 1))))

  expect_identical(
    vapply(1:4, function(mode) .Call(symbol, param_set, mode), logical(1L)),
    rep(TRUE, 4L)
  )
  expect_identical(
    vapply(1:4, function(mode) .Call(symbol, collection, mode), logical(1L)),
    c(FALSE, FALSE, FALSE, TRUE)
  )

  copies = list(
    param_set$clone(deep = TRUE),
    unserialize(serialize(param_set, NULL, version = 3L))
  )
  for (copy in copies) {
    expect_identical(
      vapply(1:4, function(mode) .Call(symbol, copy, mode), logical(1L)),
      rep(TRUE, 4L)
    )
  }
  collection_copies = list(
    collection$clone(deep = TRUE),
    unserialize(serialize(collection, NULL, version = 3L))
  )
  for (copy in collection_copies) {
    expect_identical(
      vapply(1:4, function(mode) .Call(symbol, copy, mode), logical(1L)),
      c(FALSE, FALSE, FALSE, TRUE)
    )
  }

  Subclass = R6::R6Class(
    "NativeSurfaceAuthenticationSubclass",
    inherit = ParamSet
  )
  subclass = Subclass$new(list(x = p_dbl(0, 1)))
  expect_identical(
    vapply(1:4, function(mode) .Call(symbol, subclass, mode), logical(1L)),
    rep(FALSE, 4L)
  )
  expect_false(.Call(symbol, param_set, 0L))
  expect_false(.Call(symbol, param_set, 5L))
  expect_false(.Call(symbol, param_set, NA_integer_))
  expect_false(.Call(symbol, param_set, 1))
})

test_that("surface authentication rejects canonical-looking reparented methods", {
  symbol = surface_auth_symbol()
  param_set = ps(x = p_dbl(0, 1, trafo = identity))
  method = param_set$trafo
  method_environment = new.env(parent = asNamespace("paradox"))
  method_environment$self = param_set
  method_environment$private = param_set$.__enclos_env__$private
  environment(method) = method_environment

  unlockBinding("trafo", param_set)
  assign("trafo", method, envir = param_set)
  lockBinding("trafo", param_set)
  expect_false(.Call(symbol, param_set, 1L))
})

test_that("surface authentication never forces private backing stores", {
  symbol = surface_auth_symbol()
  cases = list(
    design = list(
      make = function() ps(x = p_dbl(0, 1, trafo = identity)),
      mode = 1L,
      fields = ".trafos"
    ),
    check = list(
      make = function() ps(x = p_dbl(0, 1)),
      mode = 2L,
      fields = c(".params", ".constraint", ".deps")
    ),
    check_dt = list(
      make = function() ps(x = p_dbl(0, 1)),
      mode = 3L,
      fields = c(".params", ".constraint", ".deps")
    ),
    random_set = list(
      make = function() ps(x = p_dbl(0, 1)),
      mode = 4L,
      fields = ".params"
    ),
    random_collection = list(
      make = function() ParamSetCollection$new(list(
        component = ps(x = p_dbl(0, 1))
      )),
      mode = 4L,
      fields = c(".params", ".sets")
    )
  )

  for (case_name in names(cases)) {
    case = cases[[case_name]]
    for (field in case$fields) {
      for (kind in c("delayed", "active")) {
        state = new.env(parent = emptyenv())
        state$reads = 0L
        object = case$make()
        surface_auth_replace_private(object, field, kind, state)
        info = paste(case_name, field, kind)
        expect_false(.Call(symbol, object, case$mode), info = info)
        expect_identical(state$reads, 0L, info = info)
      }
    }
  }
})

test_that("surface authentication does not observe ALTREP scalar controls", {
  symbol = surface_auth_symbol()

  param_set = ps(x = p_dbl(0, 1))
  private = param_set$.__enclos_env__$private
  original_params = private$.params
  mode_reads = 0L
  mode = native_stateful_altrep(
    2L,
    2L,
    callback = function() {
      mode_reads <<- mode_reads + 1L
      private$.params = list(callback_was_forced = TRUE)
      invisible(gc())
    },
    callback_after = c(NA_integer_, 0L)
  )
  expect_identical(mode_reads, 0L)
  expect_false(.Call(symbol, param_set, mode))
  expect_identical(mode_reads, 0L)
  expect_identical(private$.params, original_params)

  replace_formal = function(object, method_name, formal_name, value) {
    method = object[[method_name]]
    method_formals = formals(method)
    method_formals[[formal_name]] = value
    formals(method) = method_formals
    unlockBinding(method_name, object)
    assign(method_name, method, envir = object)
    lockBinding(method_name, object)
    invisible(object)
  }

  cases = list(
    check_strict = list(
      formal = "check_strict",
      first = TRUE,
      later = TRUE
    ),
    presence = list(
      formal = "presence",
      first = "none",
      later = "none"
    )
  )
  for (case_name in names(cases)) {
    case = cases[[case_name]]
    object = ps(x = p_dbl(0, 1))
    object_private = object$.__enclos_env__$private
    original_constraint = object_private$.constraint
    reads = 0L
    value = native_stateful_altrep(
      case$first,
      case$later,
      callback = function() {
        reads <<- reads + 1L
        object_private$.constraint = function(x) FALSE
        invisible(gc())
      },
      callback_after = c(NA_integer_, 0L)
    )
    replace_formal(object, "check", case$formal, value)
    expect_identical(reads, 0L, info = case_name)
    expect_false(.Call(symbol, object, 2L), info = case_name)
    expect_identical(reads, 0L, info = case_name)
    expect_identical(
      object_private$.constraint,
      original_constraint,
      info = case_name
    )
  }
})

test_that("surface authentication does not observe ALTREP active registries", {
  symbol = surface_auth_symbol()

  param_set = ps(x = p_dbl(0, 1, trafo = identity))
  enclosure = param_set$.__enclos_env__
  private = enclosure$private
  active = get(".__active__", envir = enclosure, inherits = FALSE)
  original_trafos = private$.trafos
  registry_reads = 0L
  stateful_active = native_stateful_altrep(
    active,
    active,
    callback = function() {
      registry_reads <<- registry_reads + 1L
      private$.trafos = list(callback_was_forced = TRUE)
      invisible(gc())
    },
    # Returning and assigning an ALTREP list account for the first two Length
    # observations.  The next one would therefore be the authenticator's.
    callback_after = c(NA_integer_, 2L)
  )
  expect_identical(registry_reads, 0L)
  assign(".__active__", stateful_active, envir = enclosure)
  expect_false(.Call(symbol, param_set, 1L))
  expect_identical(registry_reads, 0L)
  expect_identical(private$.trafos, original_trafos)

  param_set = ps(x = p_dbl(0, 1, trafo = identity))
  enclosure = param_set$.__enclos_env__
  active = as.list(get(".__active__", envir = enclosure, inherits = FALSE))
  # A shallow ordinary copy still contains the exact registered closures.
  assign(".__active__", active, envir = enclosure)
  expect_true(.Call(symbol, param_set, 1L))
})

test_that("surface authentication roots canonical closure graphs", {
  symbol = surface_auth_symbol()
  param_set = ps(x = p_dbl(0, 1, trafo = identity))
  collection = ParamSetCollection$new(list(
    component = ps(x = p_dbl(0, 1))
  ))
  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed_set = vapply(
    1:4,
    function(mode) .Call(symbol, param_set, mode),
    logical(1L)
  )
  observed_collection = .Call(symbol, collection, 4L)
  gctorture(previous)

  expect_identical(observed_set, rep(TRUE, 4L))
  expect_true(observed_collection)
})
