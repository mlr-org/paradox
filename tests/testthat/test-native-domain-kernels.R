native_domain_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"))
}

native_domain_bind = function(domains) {
  paradox:::recover_domain(data.table::rbindlist(
    domains,
    use.names = TRUE,
    fill = TRUE
  ))
}

test_that("closed Domain kernels have forced registered entry points", {
  routines = c(
    domain_check_builtin = 3L,
    domain_property_builtin = 2L,
    domain_qunif_builtin = 2L,
    domain_sanitize_builtin = 2L,
    test_domain_admission_reentry = 4L
  )
  for (routine in names(routines)) {
    symbol = native_domain_symbol(routine)
    expect_s3_class(symbol, "NativeSymbolInfo")
    expect_identical(symbol$numParameters, routines[[routine]])
    expect_error(
      .Call(routine, list(), list(), PACKAGE = "paradox"),
      "not available"
    )
  }
  expect_false(getLoadedDLLs()[["paradox"]][["dynamicLookup"]])
})

test_that("outward Domain metadata capture rejects unsupported tags", {
  domain = p_dbl(0, 1)
  attr(domain, "repr") = NULL
  attr(domain, "paradox.domain.unexpected") = TRUE

  expect_error(
    domain_check(domain, list(0.5)),
    "outer metadata must be ordinary and bounded",
    fixed = TRUE
  )
})

test_that("outward Domain metadata capture rejects duplicate tags", {
  domain = p_dbl(0, 1)
  attr(domain, "repr") = NULL
  attr(domain, "zzzzz") = "duplicate class tag"
  bytes = serialize(domain, NULL, version = 2L)
  marker = charToRaw("zzzzz")
  offsets = which(vapply(
    seq_len(length(bytes) - length(marker) + 1L),
    function(offset) {
      identical(
        bytes[offset:(offset + length(marker) - 1L)],
        marker
      )
    },
    logical(1L)
  ))
  expect_length(offsets, 1L)
  bytes[offsets[[1L]]:(offsets[[1L]] + length(marker) - 1L)] =
    charToRaw("class")
  domain = unserialize(bytes)
  expect_identical(
    names(attributes(domain)),
    c("class", "row.names", ".internal.selfref", "names", "class")
  )

  expect_error(
    domain_check(domain, list(0.5)),
    "Unsupported Domain class",
    fixed = TRUE
  )
})

test_that("Domain admission rejects finalizer changes after all-row capture", {
  admission_reentry = native_domain_symbol("test_domain_admission_reentry")
  column_mutator = native_domain_symbol("test_gc_column_mutator")
  attribute_mutator = native_domain_symbol("test_gc_attribute_mutator")
  param_fct_kind = 3L
  interpret_all = 63L

  mutate_at_barrier = function(target, index, replacement) {
    pointer = .Call(
      column_mutator,
      target,
      as.integer(index - 1L),
      replacement
    )
    rm(pointer)
    for (iteration in 1:3) {
      invisible(gc(full = TRUE))
    }
    if (!identical(target[[index]], replacement)) {
      stop("Domain admission finalizer fixture did not run")
    }
    invisible(NULL)
  }
  mutate_attribute_at_barrier = function(target, name, replacement) {
    pointer = .Call(attribute_mutator, target, name, replacement)
    rm(pointer)
    for (iteration in 1:3) {
      invisible(gc(full = TRUE))
    }
    observed = attr(target, name, exact = TRUE)
    expected = if (identical(name, "row.names")) {
      seq_len(abs(replacement[[2L]]))
    } else {
      replacement
    }
    if (!identical(observed, expected)) {
      stop("Domain admission attribute-finalizer fixture did not run")
    }
    invisible(NULL)
  }

  domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  levels_index = match("levels", names(domain))
  replacement_column = list(c("c", "d"), c("u", "v"))
  expect_error(
    .Call(
      admission_reentry,
      domain,
      param_fct_kind,
      interpret_all,
      function() {
        mutate_at_barrier(domain, levels_index, replacement_column)
      }
    ),
    "Domain changed during admission",
    fixed = TRUE
  )
  expect_identical(domain$levels, replacement_column)

  # Replacing one nested row in place keeps the selected top-level column
  # pointer unchanged. The terminal receipt must therefore cover interpreted
  # payload, not merely the outward table/column spine.
  domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  levels_column = domain$levels
  replacement_row = c("c", "d")
  expect_error(
    .Call(
      admission_reentry,
      domain,
      param_fct_kind,
      interpret_all,
      function() {
        mutate_at_barrier(levels_column, 1L, replacement_row)
      }
    ),
    "Domain changed during admission",
    fixed = TRUE
  )
  expect_identical(domain$levels[[1L]], replacement_row)
  expect_identical(domain$levels[[2L]], c("a", "b"))

  # An in-place change that occurs before its row is owned may define the
  # later generation selected by the terminal all-row barrier. It is safe to
  # accept only because every admitted field then equals that one complete
  # current generation; this is not first-pointer-observation semantics.
  domain = native_domain_bind(list(
    p_fct(c("a", "b"), special_vals = list("s1")),
    p_fct(c("a", "b"), special_vals = list("s2"))
  ))
  later_special_values = domain$special_vals[[2L]]
  replacement_special_value = "changed"
  expect_null(.Call(
    admission_reentry,
    domain,
    param_fct_kind,
    interpret_all,
    function() {
      mutate_at_barrier(
        later_special_values,
        1L,
        replacement_special_value
      )
    }
  ))
  expect_identical(domain$special_vals[[2L]], list(replacement_special_value))

  # Dispatch and row-count metadata are part of the same outward Domain
  # generation as its columns. Neither may change after the adapter captured
  # the row payload it will admit and execute.
  metadata_replacements = list(
    class = c("ParamInt", "Domain", "data.table", "data.frame"),
    row.names = .set_row_names(3L)
  )
  for (attribute in names(metadata_replacements)) {
    domain = native_domain_bind(list(
      p_fct(c("a", "b")),
      p_fct(c("a", "b"))
    ))
    replacement = metadata_replacements[[attribute]]
    expect_error(
      .Call(
        admission_reentry,
        domain,
        param_fct_kind,
        interpret_all,
        function() {
          mutate_attribute_at_barrier(domain, attribute, replacement)
        }
      ),
      "Domain changed during admission",
      fixed = TRUE,
      info = attribute
    )
    expected = if (identical(attribute, "row.names")) {
      seq_len(abs(replacement[[2L]]))
    } else {
      replacement
    }
    expect_identical(attr(domain, attribute, exact = TRUE), expected)
  }

  # Every column shell remains structural even when this operation does not
  # interpret that column's cargo. Same-pointer attribute mutation must not
  # evade the terminal generation gate.
  column_shell_cases = list(
    default = list(
      kind = param_fct_kind,
      make = function() native_domain_bind(list(
        p_fct(c("a", "b")),
        p_fct(c("a", "b"))
      ))
    ),
    id = list(
      kind = param_fct_kind,
      make = function() native_domain_bind(list(
        p_fct(c("a", "b")),
        p_fct(c("a", "b"))
      ))
    ),
    lower = list(
      kind = 1L,
      make = function() native_domain_bind(list(
        p_dbl(0, 1),
        p_dbl(2, 3)
      ))
    )
  )
  for (column in names(column_shell_cases)) {
    case = column_shell_cases[[column]]
    domain = case$make()
    target = domain[[column]]
    expect_error(
      .Call(
        admission_reentry,
        domain,
        case$kind,
        interpret_all,
        function() {
          mutate_attribute_at_barrier(
            target,
            "paradox.test.attribute",
            TRUE
          )
        }
      ),
      "Domain changed during admission",
      fixed = TRUE,
      info = column
    )
    expect_identical(attr(target, "paradox.test.attribute"), TRUE)
  }

  # The second deterministic phase runs after every selected nested field has
  # been admitted and, where required, detached into its operation-local
  # owner.  Mutating the live source then must invalidate that owned receipt
  # for every compact row slot, including exact-identity transformations.
  post_ownership_cases = list(
    levels = c("c", "d"),
    special_vals = list("changed"),
    cargo = list(marker = TRUE),
    .tags = "changed",
    .trafo = identity
  )
  for (column in names(post_ownership_cases)) {
    domain = native_domain_bind(list(
      p_fct(c("a", "b"), special_vals = list("special"), tags = "tag"),
      p_fct(c("a", "b"), special_vals = list("special"), tags = "tag")
    ))
    target = domain[[column]]
    replacement = post_ownership_cases[[column]]
    expect_error(
      .Call(
        admission_reentry,
        domain,
        param_fct_kind,
        interpret_all,
        list(
          NULL,
          function() {
            mutate_at_barrier(target, 1L, replacement)
          }
        )
      ),
      "Domain changed during admission",
      fixed = TRUE,
      info = column
    )
    expect_identical(target[[1L]], replacement)
  }

  # Both phase seams use the same production orchestrator. A collection at
  # the first seam must leave every indexed root live for the ownership phase;
  # a later source mutation must still be rejected by the terminal receipt.
  phase_state = new.env(parent = emptyenv())
  phase_state$seen = character()
  domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  levels_column = domain$levels
  replacement_row = c("c", "d")
  expect_error(
    .Call(
      admission_reentry,
      domain,
      param_fct_kind,
      interpret_all,
      list(
        function() {
          phase_state$seen = c(phase_state$seen, "capture")
          invisible(gc(full = TRUE))
        },
        function() {
          phase_state$seen = c(phase_state$seen, "ownership")
          mutate_at_barrier(levels_column, 1L, replacement_row)
        }
      )
    ),
    "Domain changed during admission",
    fixed = TRUE
  )
  expect_identical(phase_state$seen, c("capture", "ownership"))
  expect_identical(levels_column[[1L]], replacement_row)

  # Empty special-value shells are intentionally not copied, but optional
  # names presence is still exact generation state. Because the live source
  # and admitted shell alias at length zero, an explicit scalar receipt must
  # reject a finalizer that adds names after semantic admission.
  for (initial_names in list(NULL, character())) {
    domain = p_fct(c("a", "b"))
    empty_special_values = domain$special_vals[[1L]]
    if (!is.null(initial_names)) {
      data.table::setattr(empty_special_values, "names", initial_names)
    }
    expect_identical(names(empty_special_values), initial_names)
    replacement_names = if (is.null(initial_names)) character() else NULL
    expect_error(
      .Call(
        admission_reentry,
        domain,
        param_fct_kind,
        interpret_all,
        list(
          NULL,
          function() {
            mutate_attribute_at_barrier(
              empty_special_values,
              "names",
              replacement_names
            )
          }
        )
      ),
      "Domain changed during admission",
      fixed = TRUE
    )
    expect_identical(names(empty_special_values), replacement_names)
  }

  # Canonical-name selection and the physical layout are one generation.
  # Reordering the paired columns or only their labels after capture must not
  # let a terminal selection authenticate a different pairing.
  domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  replacement_order = rev(names(domain))
  expect_error(
    .Call(
      admission_reentry,
      domain,
      param_fct_kind,
      interpret_all,
      function() data.table::setcolorder(domain, replacement_order)
    ),
    "Domain changed during admission",
    fixed = TRUE
  )
  expect_identical(names(domain), replacement_order)

  domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  replacement_names = names(domain)
  replacement_names[1:2] = replacement_names[2:1]
  expect_error(
    .Call(
      admission_reentry,
      domain,
      param_fct_kind,
      interpret_all,
      function() data.table::setnames(domain, replacement_names)
    ),
    "Domain changed during admission",
    fixed = TRUE
  )
  expect_identical(names(domain), replacement_names)

  # The remaining outer metadata identities are opaque, but their exact
  # presence and identity still belong to the admitted table generation.
  for (attribute in c(".internal.selfref", "repr")) {
    domain = native_domain_bind(list(
      p_fct(c("a", "b")),
      p_fct(c("a", "b"))
    ))
    replacement = if (identical(attribute, "repr")) "changed" else NULL
    expect_error(
      .Call(
        admission_reentry,
        domain,
        param_fct_kind,
        interpret_all,
        list(
          NULL,
          function() data.table::setattr(domain, attribute, replacement)
        )
      ),
      "Domain changed during admission",
      fixed = TRUE,
      info = attribute
    )
    expect_identical(attr(domain, attribute, exact = TRUE), replacement)
  }

  # A terminal receipt must also fail closed when a callback replaces the
  # supported outer generation with an overlong attribute spine. The bound is
  # structural: it prevents old-R compatibility selectors from walking
  # malformed or cyclic metadata, without adding work to ordinary Domains.
  domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  attribute_names = sprintf(
    "paradox.domain.outer.%03d",
    seq_len(65L)
  )
  expect_error(
    .Call(
      admission_reentry,
      domain,
      param_fct_kind,
      interpret_all,
      list(
        NULL,
        function() {
          for (index in seq_along(attribute_names)) {
            data.table::setattr(
              domain,
              attribute_names[[index]],
              index
            )
          }
        }
      )
    ),
    "Domain changed during admission",
    fixed = TRUE
  )
})

test_that("mixed-encoding Domain grouping has an exact rare receipt", {
  utf8 = enc2utf8("gr\u00fcppe")
  Encoding(utf8) = "UTF-8"
  latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(latin1),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(latin1) = "latin1"

  domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  data.table::set(
    domain,
    i = 1:2,
    j = "grouping",
    value = c(utf8, latin1)
  )
  expect_identical(Encoding(domain$grouping), c("UTF-8", "latin1"))
  expect_true(domain_check(domain, list("a", "b")))

  # Equality may translate encodings before the terminal barrier. Once that
  # grouped generation is captured, even a semantically equal representation
  # replacement must not splice a different generation into the operation.
  expect_error(
    .Call(
      native_domain_symbol("test_domain_admission_reentry"),
      domain,
      3L,
      63L,
      function() {
        data.table::set(
          domain,
          i = 2L,
          j = "grouping",
          value = utf8
        )
      }
    ),
    "rows must share one grouping",
    fixed = TRUE
  )
  expect_identical(Encoding(domain$grouping), c("UTF-8", "UTF-8"))
})

test_that("Domain grouping is rechecked after row-name Length reentry", {
  namespace = asNamespace("paradox")
  skip_if_not(
    exists(
      "C_test_stateful_altrep_row_names_rearm",
      envir = namespace,
      inherits = FALSE
    ),
    "the internal raw row-name ALTREP fixture is unavailable"
  )

  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  row_names = native_stateful_altrep(
    c("row-a", "row-b"),
    c("row-a", "row-b"),
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::set(
        state$domain,
        i = 2L,
        j = "grouping",
        value = "changed-group"
      )
    }
  )
  state$domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  attr(state$domain, "row.names") = row_names
  invisible(.Call(
    get("C_test_stateful_altrep_row_names_rearm", envir = namespace),
    state$domain,
    c(NA_integer_, 0L)
  ))

  expect_error(
    domain_check(state$domain, list("a", "x")),
    "rows must share one grouping",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(
    state$domain$grouping,
    c("\"a\",\"b\"", "changed-group")
  )
})

test_that("Domain row-name Length follows complete column-shell admission", {
  namespace = asNamespace("paradox")
  skip_if_not(
    exists(
      "C_test_stateful_altrep_row_names_rearm",
      envir = namespace,
      inherits = FALSE
    ),
    "the internal raw row-name ALTREP fixture is unavailable"
  )

  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  row_names = native_stateful_altrep(
    "row-a",
    "row-a",
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::set(state$domain, j = "lower", value = 0)
    }
  )
  state$domain = p_dbl(0, 1)
  data.table::set(state$domain, j = "lower", value = list(list(0)))
  expect_identical(typeof(state$domain$lower), "list")
  attr(state$domain, "row.names") = row_names
  invisible(.Call(
    get("C_test_stateful_altrep_row_names_rearm", envir = namespace),
    state$domain,
    c(NA_integer_, 0L)
  ))

  expect_error(
    domain_check(state$domain, list(0.5)),
    "`lower` must be numeric",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 0L)
  expect_identical(typeof(state$domain$lower), "list")
})

test_that("Domain admission rejects row-name replacement during Length", {
  namespace = asNamespace("paradox")
  attribute_mutator = get(
    "C_test_gc_attribute_mutator",
    envir = namespace
  )
  skip_if_not(
    exists(
      "C_test_stateful_altrep_row_names_rearm",
      envir = namespace,
      inherits = FALSE
    ),
    "the internal raw row-name ALTREP fixture is unavailable"
  )

  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$replacement = native_stateful_altrep(
    c("replacement-a", "replacement-b", "replacement-c"),
    c("replacement-a", "replacement-b", "replacement-c")
  )
  row_names = native_stateful_altrep(
    c("row-a", "row-b"),
    c("row-a", "row-b"),
    callback = function() {
      state$callbacks = state$callbacks + 1L
      pointer = .Call(
        attribute_mutator,
        state$domain,
        "row.names",
        state$replacement
      )
      rm(pointer)
      for (iteration in 1:3) {
        invisible(gc(full = TRUE))
      }
      if (!identical(
        attr(state$domain, "row.names", exact = TRUE),
        state$replacement
      )) {
        stop("Domain row-name replacement fixture did not run")
      }
    }
  )
  state$domain = native_domain_bind(list(
    p_fct(c("a", "b")),
    p_fct(c("a", "b"))
  ))
  attr(state$domain, "row.names") = row_names
  invisible(.Call(
    get("C_test_stateful_altrep_row_names_rearm", envir = namespace),
    state$domain,
    c(NA_integer_, 0L)
  ))

  expect_error(
    domain_check(state$domain, list("a", "x")),
    "Domain changed during admission",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(
    attr(state$domain, "row.names", exact = TRUE),
    c("replacement-a", "replacement-b", "replacement-c")
  )
})

test_that("empty Domain semantics are owned by the native kernels", {
  empty = paradox:::empty_domain
  expect_true(domain_check(empty, list()))
  expect_true(domain_check(p_dbl(0, 1), list()))
  expect_identical(domain_nlevels(empty), integer())
  expect_identical(domain_is_bounded(empty), logical())
  expect_identical(domain_is_number(empty), logical())
  expect_identical(domain_is_categ(empty), logical())
  expect_identical(domain_qunif(empty, c(-1, 2)), logical())
  expect_identical(domain_sanitize(empty, numeric()), numeric())
  expect_identical(domain_sanitize(empty, list()), list())

  zero_double = p_dbl(0, 1)[0]
  expect_true(domain_check(zero_double, list()))
  expect_identical(domain_nlevels(zero_double), integer())
  expect_identical(domain_qunif(zero_double, 0.5), logical())

  check = native_domain_symbol("domain_check_builtin")
  property = native_domain_symbol("domain_property_builtin")
  qunif = native_domain_symbol("domain_qunif_builtin")
  sanitize = native_domain_symbol("domain_sanitize_builtin")
  expect_true(.Call(check, empty, list(), FALSE))
  expect_identical(.Call(property, empty, 0L), integer())
  expect_identical(.Call(qunif, empty, 0.5), logical())
  expect_identical(.Call(sanitize, empty, list()), list())

  malformed = data.table::copy(empty)
  data.table::set(malformed, j = "storage_type", value = NULL)
  expect_error(.Call(property, malformed, 0L), "Corrupt empty Domain")
  malformed = data.table::copy(empty)
  data.table::setattr(malformed, "paradox.rogue.metadata", TRUE)
  expect_error(.Call(property, malformed, 0L), "Corrupt empty Domain")
  unknown = structure(empty, class = c("UnknownDomain", "Domain", "data.table", "data.frame"))
  expect_error(.Call(check, unknown, list(), FALSE), "Unsupported Domain class")
})

test_that("built-in Domain checks are authoritative", {
  expect_true(domain_check(p_dbl(-1, 1), list(0.25)))
  expect_true(domain_check(p_int(-2, 2), list(1L)))
  expect_true(domain_check(p_fct(c("slow", "fast")), list("fast")))
  expect_true(domain_check(p_lgl(), list(FALSE)))
  expect_true(domain_check(p_uty(), list(list(payload = 1))))

  expect_match(
    domain_check(p_dbl(0, 1), list(TRUE)),
    "Must be of type 'number', not 'logical'",
    fixed = TRUE
  )
  expect_match(
    domain_check(p_dbl(0, 1), list(NA_real_)),
    "May not be NA",
    fixed = TRUE
  )
  expect_match(
    domain_check(p_int(0, 2), list(0.5)),
    "single integerish value",
    fixed = TRUE
  )
  expect_match(
    domain_check(p_fct(c("a", "b")), list("c")),
    "Must be element of set {'a','b'}, but is 'c'",
    fixed = TRUE
  )
  expect_match(
    domain_check(p_lgl(), list(NA)),
    "May not be NA",
    fixed = TRUE
  )
  expect_error(domain_check(p_dbl(), 1), "ordinary list")
  expect_error(domain_check(p_dbl(), list(1), internal = 1), "TRUE or FALSE")

  special = p_dbl(0, 1, special_vals = list("automatic", NULL))
  expect_true(domain_check(special, list("automatic")))
  expect_true(domain_check(special, list(NULL)))
  expect_match(
    domain_check(special, list("automatic"), internal = TRUE),
    "Must be of type 'number', not 'character'",
    fixed = TRUE
  )
})

test_that("ParamUty is the supported custom validation callback", {
  calls = 0L
  utility = p_uty(custom_check = function(value) {
    calls <<- calls + 1L
    if (is.character(value)) TRUE else "must be character"
  })
  # p_uty() validates the callback once at construction.
  expect_identical(calls, 1L)
  expect_true(domain_check(utility, list("ok")))
  expect_match(domain_check(utility, list(1)), "must be character")
  expect_gte(calls, 3L)

  malformed_result = p_uty(custom_check = function(value) TRUE)
  malformed_result$cargo[[1L]]$custom_check = function(value) FALSE
  expect_match(
    domain_check(malformed_result, list(1)),
    "must return TRUE or one non-missing string"
  )
})

test_that("ParamUty observes a character callback diagnostic exactly once", {
  skip_if_not(
    exists("C_test_stateful_altrep", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP test class is unavailable"
  )
  answer = NULL
  utility = p_uty(custom_check = function(value) {
    if (is.null(answer)) TRUE else answer
  })
  answer = native_stateful_altrep(
    structure("first reason", class = "paradox_custom_check_probe"),
    structure("second reason", class = "paradox_custom_check_probe"),
    elt_switch_after = 1L
  )

  observed = domain_check(utility, list(1L))
  expect_match(observed, "first reason", fixed = TRUE)
  expect_false(grepl("second reason", observed, fixed = TRUE))
})

test_that("translated ParamUty diagnostics survive forced collection", {
  skip_on_cran()
  utf8_reason = enc2utf8("caf\u00e9 required")
  latin1_reason = iconv(utf8_reason, from = "UTF-8", to = "latin1")
  skip_if(
    is.na(latin1_reason),
    "this platform cannot represent the latin1 fixture"
  )
  Encoding(latin1_reason) = "latin1"
  utility = p_uty(custom_check = function(value) latin1_reason)
  expected = paste0(enc2utf8(utility$id), ": ", utf8_reason)

  previous = gctorture(TRUE)
  on.exit(gctorture(previous), add = TRUE)
  observed = domain_check(utility, list(NULL))
  gctorture(previous)

  expect_identical(observed, expected)
  expect_identical(Encoding(observed), "UTF-8")
})

test_that("callback and argument snapshots bound one Domain operation", {
  observed = character()
  domain = p_uty(custom_check = function(value) {
    observed <<- c(observed, paste0("old-", value))
    TRUE
  })
  observed = character()
  combined = native_domain_bind(rep(list(domain), 2L))
  old = combined$cargo[[2L]]$custom_check
  combined$cargo[[1L]]$custom_check = function(value) {
    observed <<- c(observed, paste0("mutating-", value))
    data.table::set(
      combined,
      i = 2L,
      j = "cargo",
      value = list(list(custom_check = function(value) {
        observed <<- c(observed, paste0("new-", value))
        TRUE
      }, repr = "NoDefault"))
    )
    TRUE
  }
  combined$cargo[[2L]]$custom_check = old

  expect_true(domain_check(combined, list("a", "b")))
  expect_identical(observed, c("mutating-a", "old-b"))
  expect_true(domain_check(combined, list("c", "d")))
  expect_identical(observed, c("mutating-a", "old-b", "mutating-c", "new-d"))
})

test_that("Domain checks re-admit row shape after ALTREP materialization", {
  callbacks = 0L
  domain = p_dbl(0, 1)
  value = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      callbacks <<- callbacks + 1L
      pointer = .Call(
        get("C_test_gc_column_mutator", envir = asNamespace("paradox")),
        domain,
        match("id", names(domain)) - 1L,
        c("x", "y")
      )
      rm(pointer)
      for (index in 1:3) invisible(gc(full = TRUE))
    },
    callback_after = 0L
  )

  expect_error(
    domain_check(domain, list(value)),
    "Domain shape changed|must have type .* length 2"
  )
  expect_identical(callbacks, 1L)
  expect_identical(domain$id, c("x", "y"))
})

test_that("Domain checks revalidate the outer value shell after reentry", {
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$value = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setattr(
        state$values,
        "class",
        "paradox_reentered_values"
      )
    },
    callback_after = 0L
  )
  state$values = list(state$value)

  expect_error(
    domain_check(p_dbl(0, 1), state$values),
    "ordinary list",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(class(state$values), "paradox_reentered_values")

  namespace = asNamespace("paradox")
  state$admission_callbacks = 0L
  state$values = list(0.5)
  row_names = native_stateful_altrep(
    "row-a",
    "row-a",
    callback = function() {
      state$admission_callbacks = state$admission_callbacks + 1L
      data.table::setattr(
        state$values,
        "class",
        "paradox_admission_values"
      )
    }
  )
  state$domain = p_dbl(0, 1)
  attr(state$domain, "row.names") = row_names
  invisible(.Call(
    get("C_test_stateful_altrep_row_names_rearm", envir = namespace),
    state$domain,
    c(NA_integer_, 0L)
  ))
  expect_error(
    domain_check(state$domain, state$values),
    "ordinary list",
    fixed = TRUE
  )
  expect_identical(state$admission_callbacks, 1L)
  expect_identical(class(state$values), "paradox_admission_values")
})

test_that("Domain checks reject cross-row value-generation splices", {
  column_mutator = native_domain_symbol("test_gc_column_mutator")
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$later = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      pointer = .Call(
        column_mutator,
        state$values,
        0L,
        0.75
      )
      rm(pointer)
      for (iteration in 1:3) {
        invisible(gc(full = TRUE))
      }
    },
    callback_after = 0L
  )
  state$values = list(0.25, state$later)
  domain = native_domain_bind(list(
    p_dbl(0, 0.5),
    p_dbl(0, 1)
  ))

  expect_error(
    domain_check(domain, state$values),
    "`values` changed during Domain value admission",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(state$values[[1L]], 0.75)
})

test_that("Domain checks snapshot one atomic ALTREP payload-metadata generation", {
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$value = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setattr(
        state$value,
        "class",
        "paradox_domain_value_reentry"
      )
    },
    callback_after = 0L
  )

  expect_error(
    domain_check(p_dbl(0, 1), list(state$value)),
    "changed",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(class(state$value), "paradox_domain_value_reentry")
})

test_that("Domain checks reject structural ALTREP list shells", {
  skip_if_no_list_altrep()
  callbacks = 0L
  values = native_stateful_altrep(
    list(0.5),
    list(0.5),
    callback = function() {
      callbacks <<- callbacks + 1L
    },
    callback_after = c(NA_integer_, 0L)
  )
  expect_error(
    domain_check(p_dbl(0, 1), values),
    "ordinary list",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("typed Domain checks reject ALTREP list leaves without observing them", {
  skip_if_no_list_altrep()
  callbacks = 0L
  value = native_stateful_altrep(
    list(0.5),
    list(0.5),
    callback = function() {
      callbacks <<- callbacks + 1L
    },
    callback_after = c(0L, 0L)
  )
  expect_error(
    domain_check(p_dbl(0, 1), list(value)),
    "ALTREP only for atomic vectors",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("Domain properties are closed native operations", {
  domains = list(
    p_dbl(0, 1),
    p_int(0, 2),
    p_fct(c("a", "b")),
    p_lgl(),
    p_uty()
  )
  expected = list(
    c(Inf, TRUE, TRUE, FALSE),
    c(3, TRUE, TRUE, FALSE),
    c(2, TRUE, FALSE, TRUE),
    c(2, TRUE, FALSE, TRUE),
    c(Inf, FALSE, FALSE, FALSE)
  )
  for (index in seq_along(domains)) {
    observed = c(
      domain_nlevels(domains[[index]]),
      domain_is_bounded(domains[[index]]),
      domain_is_number(domains[[index]]),
      domain_is_categ(domains[[index]])
    )
    expect_equal(observed, expected[[index]])
  }

  grouped = native_domain_bind(list(p_int(0, 1), p_int(10, 12)))
  expect_identical(domain_nlevels(grouped), c(2, 3))
  expect_identical(domain_is_bounded(grouped), c(TRUE, TRUE))
  expect_identical(domain_is_number(grouped), TRUE)
})

test_that("Domain sanitization has no R replay path", {
  domain = native_domain_bind(list(
    p_dbl(0, 1),
    p_dbl(-2, 2),
    p_dbl(10, 20)
  ))
  expect_identical(
    domain_sanitize(domain, list(-0.1, 2.1, 21)),
    list(0, 2, 20)
  )
  expect_warning(
    expect_identical(
      domain_sanitize(domain[1:2], list(-2, 20, 3)),
      list(0, 2, 1)
    ),
    "not a multiple"
  )
  expect_identical(
    domain_sanitize(
      p_int(),
      list(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5, TRUE, NA_real_, NaN)
    ),
    list(-2L, -2L, 0L, 0L, 2L, 2L, 1L, NA_integer_, NA_integer_)
  )
  expect_warning(
    expect_identical(domain_sanitize(p_int(), list(-Inf, Inf)), list(NA_integer_, NA_integer_)),
    "NAs introduced"
  )
  opaque = list(list(payload = 1))
  expect_identical(domain_sanitize(p_uty(), opaque), opaque)
  classed_opaque = structure(list(payload = 1), class = "opaque_payload")
  expect_identical(
    domain_sanitize(p_uty(), classed_opaque),
    classed_opaque
  )
  expect_error(domain_sanitize(p_dbl(), c("1", "2")), "numeric vector or list")
})

test_that("Domain sanitizers reject structural list ALTREP without observation", {
  skip_if_no_list_altrep()
  callbacks = 0L
  values = native_stateful_altrep(
    list(0.5),
    list(0.5),
    callback = function() {
      callbacks <<- callbacks + 1L
    },
    callback_after = c(0L, 0L)
  )
  expect_error(
    domain_sanitize(p_fct(c("a", "b")), values),
    "unclassed numeric vector or list",
    fixed = TRUE
  )
  expect_identical(callbacks, 0L)
})

test_that("Domain sanitizers reject cross-row value-generation splices", {
  column_mutator = native_domain_symbol("test_gc_column_mutator")
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$later = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      pointer = .Call(
        column_mutator,
        state$values,
        0L,
        0.75
      )
      rm(pointer)
      for (iteration in 1:3) {
        invisible(gc(full = TRUE))
      }
    },
    callback_after = 0L
  )
  state$values = list(0.25, state$later)
  domain = native_domain_bind(list(
    p_dbl(0, 0.5),
    p_dbl(0, 1)
  ))

  expect_error(
    domain_sanitize(domain, state$values),
    "`values` changed during Domain sanitization",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(state$values[[1L]], 0.75)
})

test_that("Domain sanitizers reject same-pointer scalar mutation", {
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$later = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setattr(
        state$values,
        "class",
        c("data.table", "data.frame")
      )
      data.table::setattr(
        state$values,
        "row.names",
        .set_row_names(1L)
      )
      data.table::set(
        state$values,
        i = 1L,
        j = 1L,
        value = 0.75
      )
      data.table::setattr(state$values, "class", NULL)
      data.table::setattr(state$values, "row.names", NULL)
    },
    callback_after = 0L
  )
  state$values = list(first = 0.25, second = state$later)
  first = state$values[[1L]]
  domain = native_domain_bind(list(
    p_dbl(0, 0.5),
    p_dbl(0, 1)
  ))

  expect_error(
    domain_sanitize(domain, state$values),
    "`values` changed during Domain sanitization",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(state$values[[1L]], first)
  expect_identical(state$values[[1L]], 0.75)
})

test_that("Domain sanitizers revalidate atomic ALTREP after Length", {
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$values = native_stateful_altrep(
    c(0.25, 0.75),
    c(0.25, 0.75),
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setattr(
        state$values,
        "class",
        "paradox_sanitize_reentry"
      )
    },
    callback_after = c(NA_integer_, 0L)
  )

  expect_error(
    domain_sanitize(p_dbl(0, 1), state$values),
    "unclassed numeric vector or list",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(class(state$values), "paradox_sanitize_reentry")
})

test_that("Domain sanitizers revalidate inner ALTREP scalars after Length", {
  state = new.env(parent = emptyenv())
  state$callbacks = 0L
  state$value = native_stateful_altrep(
    0.5,
    0.5,
    callback = function() {
      state$callbacks = state$callbacks + 1L
      data.table::setattr(
        state$value,
        "class",
        "paradox_sanitize_leaf_reentry"
      )
    },
    callback_after = c(NA_integer_, 0L)
  )

  expect_error(
    domain_sanitize(p_dbl(0, 1), list(state$value)),
    "numeric scalar values",
    fixed = TRUE
  )
  expect_identical(state$callbacks, 1L)
  expect_identical(class(state$value), "paradox_sanitize_leaf_reentry")
})

test_that("Domain sanitizers retain the Domain selected before value callbacks", {
  callback_controls = list(
    length = c(NA_integer_, 0L),
    element = c(0L, NA_integer_)
  )

  for (callback_kind in names(callback_controls)) {
    state = new.env(parent = emptyenv())
    state$callbacks = 0L
    state$domain = p_dbl(0, 1)
    state$values = native_stateful_altrep(
      0.75,
      0.75,
      callback = function() {
        state$callbacks = state$callbacks + 1L
        data.table::set(
          state$domain,
          i = 1L,
          j = "upper",
          value = 0.25
        )
      },
      callback_after = callback_controls[[callback_kind]]
    )

    expect_identical(
      domain_sanitize(state$domain, state$values),
      list(0.75),
      info = callback_kind
    )
    expect_identical(state$callbacks, 1L, info = callback_kind)
    expect_identical(state$domain$upper, 0.25, info = callback_kind)
  }
})

test_that("quantile kernels preserve formulas and portable rounding barriers", {
  double_domain = native_domain_bind(list(p_dbl(-1, 1), p_dbl(10, 20)))
  integer_domain = native_domain_bind(list(p_int(-2, 2), p_int(10, 12)))
  factor_domain = native_domain_bind(rep(list(p_fct(c("a", "b", "c"))), 2L))
  logical_domain = native_domain_bind(rep(list(p_lgl()), 2L))
  x = c(0, 0, 0.25, 0.5, 0.75, 1)

  expect_identical(domain_qunif(double_domain, x), c(-1, 10, -0.5, 15, 0.5, 20))
  expect_identical(domain_qunif(integer_domain, x), c(-2L, 10L, -1L, 11L, 1L, 12L))
  expect_identical(domain_qunif(factor_domain, x), c("a", "a", "a", "b", "c", "c"))
  expect_identical(domain_qunif(logical_domain, x), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))

  expect_identical(domain_qunif(p_dbl(4, 4), c(0, 0.5, 1)), rep(4, 3L))
  expect_identical(domain_qunif(p_int(4, 4), c(0, 0.5, 1)), rep(4L, 3L))
  expect_identical(domain_qunif(p_dbl(), c(0, 0.5, 1)), c(-Inf, NaN, Inf))
  expect_identical(domain_qunif(p_int(), c(0, 0.5, 1)), rep(NA_integer_, 3L))

  named = structure(c(0, 0.5, 1), names = letters[1:3], extra = "kept")
  expect_identical(attributes(domain_qunif(p_dbl(0, 1), named)), attributes(named))
  expect_identical(names(domain_qunif(p_lgl(), named)), names(named))

  marker = new.env(parent = emptyenv())
  attributed = structure(c(0.25, 0.75), metadata = marker)
  attributed_result = domain_qunif(p_dbl(0, 1), attributed)
  expect_identical(attr(attributed_result, "metadata", exact = TRUE), marker)

  structured = structure(
    c(0.25, 0.75),
    dim = c(1L, 2L),
    dimnames = list("row", c("left", "right")),
    ignored = marker
  )
  structured_result = domain_qunif(p_lgl(), structured)
  expect_identical(dim(structured_result), dim(structured))
  expect_identical(dimnames(structured_result), dimnames(structured))
  expect_null(attr(structured_result, "ignored", exact = TRUE))

  expect_error(domain_qunif(p_dbl(), c(-0.1, 1)), "between zero and one")
  expect_error(domain_qunif(p_uty(), 0.5), "undefined for ParamUty")

  expect_identical(
    domain_qunif(p_dbl(-10, 10), 0.499),
    -0x1.47ae147ae14p-6
  )
  expect_identical(
    domain_qunif(p_int(-2, 2), 0x1.9999999999999p-3),
    -1L
  )
})

test_that("quantile metadata selection bounds the top-level attribute spine", {
  labels = sprintf("paradox.qunif.attribute.%03d", seq_len(65L))
  values = stats::setNames(as.list(seq_along(labels)), labels)

  for (domain in list(p_dbl(0, 1), p_lgl())) {
    x = c(0.25, 0.75)
    attributes(x) = values
    expect_error(
      domain_qunif(domain, x),
      "ordinary, acyclic, bounded metadata",
      fixed = TRUE
    )
  }
})

test_that("Domain kernel public metadata selectors are bounded", {
  labels = sprintf("paradox.kernel.attribute.%03d", seq_len(65L))

  domain = p_dbl(0, 1)
  for (index in seq_along(labels)) {
    data.table::setattr(domain, labels[[index]], index)
  }
  for (operation in list(
      function() domain_check(domain, list(0.5)),
      function() domain_nlevels(domain),
      function() domain_sanitize(domain, list(0.5))
    )) {
    expect_error(operation(), "Unsupported Domain class")
  }

  values = list(0.5)
  attributes(values) = stats::setNames(
    as.list(seq_along(labels)),
    labels
  )
  expect_error(
    domain_check(p_dbl(0, 1), values),
    "ordinary list with one element per Domain row",
    fixed = TRUE
  )
  expect_error(
    domain_sanitize(p_dbl(0, 1), values),
    "unclassed numeric vector or list",
    fixed = TRUE
  )

  scalar = native_stateful_altrep(0.5, 0.5)
  for (index in seq_along(labels)) {
    data.table::setattr(scalar, labels[[index]], index)
  }
  expect_error(
    domain_sanitize(p_dbl(0, 1), list(scalar)),
    "numeric scalar values",
    fixed = TRUE
  )
})

test_that("quantile kernels revalidate callback-capable input shells", {
  state = new.env(parent = emptyenv())
  state$length_callbacks = 0L
  state$x = native_stateful_altrep(
    c(0.25, 0.75),
    c(0.25, 0.75),
    callback = function() {
      state$length_callbacks = state$length_callbacks + 1L
      data.table::setattr(state$x, "class", "paradox_length_reentry")
    },
    callback_after = c(NA_integer_, 0L)
  )
  expect_error(
    domain_qunif(p_dbl(0, 1), state$x),
    "unclassed numeric vector",
    fixed = TRUE
  )
  expect_identical(state$length_callbacks, 1L)
  expect_identical(class(state$x), "paradox_length_reentry")

  namespace = asNamespace("paradox")
  skip_if_not(
    exists(
      "C_test_stateful_altrep_row_names_rearm",
      envir = namespace,
      inherits = FALSE
    ),
    "the internal raw row-name ALTREP fixture is unavailable"
  )
  state$admission_callbacks = 0L
  state$x = c(0.25, 0.75)
  row_names = native_stateful_altrep(
    c("row-a", "row-b"),
    c("row-a", "row-b"),
    callback = function() {
      state$admission_callbacks = state$admission_callbacks + 1L
      data.table::setattr(state$x, "class", "paradox_admission_reentry")
    }
  )
  state$domain = native_domain_bind(rep(list(p_dbl(0, 1)), 2L))
  attr(state$domain, "row.names") = row_names
  invisible(.Call(
    get("C_test_stateful_altrep_row_names_rearm", envir = namespace),
    state$domain,
    c(NA_integer_, 0L)
  ))
  expect_error(
    domain_qunif(state$domain, state$x),
    "unclassed numeric vector",
    fixed = TRUE
  )
  expect_identical(state$admission_callbacks, 1L)
  expect_identical(class(state$x), "paradox_admission_reentry")

  # Once the attribute carrier has been selected, a later Elt callback may
  # mutate the caller's shell but cannot leak that class to the result.
  state$elt_callbacks = 0L
  state$x = native_stateful_altrep(
    c(0.25, 0.75),
    c(0.25, 0.75),
    callback = function() {
      state$elt_callbacks = state$elt_callbacks + 1L
      data.table::setattr(state$x, "class", "paradox_late_reentry")
    },
    callback_after = 0L
  )
  result = domain_qunif(p_dbl(0, 1), state$x)
  expect_identical(result, c(0.25, 0.75))
  expect_identical(state$elt_callbacks, 1L)
  expect_identical(class(state$x), "paradox_late_reentry")
  expect_null(attr(result, "class", exact = TRUE))
})

test_that("zero-level factor Domains map only empty quantile inputs", {
  domain = p_fct(character())

  expect_identical(domain_nlevels(domain), 0)
  expect_identical(domain_qunif(domain, numeric()), character())
  expect_error(
    domain_qunif(domain, 0.5),
    "Cannot map quantiles for a factor Domain with no levels",
    fixed = TRUE
  )
})

test_that("common ALTREP inputs are consumed safely once", {
  expect_true(domain_check(p_int(1, 1), as.list(1:1)))
  expect_identical(domain_qunif(p_int(0, 9), 0:1), c(0L, 9L))
  expect_identical(domain_sanitize(p_int(), 1:4), as.list(1:4))
})

test_that("third-party Domain seams are closed", {
  forged = p_dbl(0, 1)
  class(forged)[[1L]] = "ParamExtension"
  for (operation in list(
    function() domain_check(forged, list(0.5)),
    function() domain_nlevels(forged),
    function() domain_qunif(forged, 0.5),
    function() domain_sanitize(forged, list(0.5))
  )) {
    expect_error(operation(), "Unsupported Domain class")
  }

  registerS3method(
    "domain_check",
    "ParamExtension",
    function(...) stop("third-party method ran"),
    envir = asNamespace("paradox")
  )
  expect_error(domain_check(forged, list(0.5)), "Unsupported Domain class")
})

test_that("Domain kernels reject S4 structural tables and metadata", {
  domain = p_int(0L, 2L)
  operations = list(
    function(value) domain_check(value, list(1L)),
    function(value) domain_nlevels(value),
    function(value) domain_is_bounded(value),
    function(value) domain_is_number(value),
    function(value) domain_qunif(value, 0.5),
    function(value) domain_sanitize(value, list(1L))
  )
  for (operation in operations) {
    expect_error(operation(asS4(domain)), "Unsupported Domain class")
  }

  malformed_class = data.table::copy(domain)
  data.table::setattr(
    malformed_class,
    "class",
    asS4(class(malformed_class))
  )
  expect_error(domain_nlevels(malformed_class), "Unsupported Domain class")
})

test_that("malformed built-in Domains error instead of restarting in R", {
  corrupt = function(domain, column, value) {
    domain = data.table::copy(domain)
    data.table::set(domain, j = column, value = value)
    domain
  }
  cases = list(
    corrupt(p_dbl(0, 1), "storage_type", "integer"),
    corrupt(p_dbl(0, 1), "lower", NA_real_),
    corrupt(p_fct(c("a", "b")), "levels", list(1:2)),
    corrupt(p_lgl(), "levels", list(c(FALSE, TRUE)))
  )
  operations = list(
    function(domain) domain_check(domain, list(1)),
    function(domain) domain_nlevels(domain)
  )
  for (domain in cases) {
    for (operation in operations) {
      expect_error(operation(domain), "Corrupt Domain storage")
    }
  }
  for (template in list(p_dbl(0, 1), p_int(0L, 1L))) {
    for (column in c("lower", "upper", "tolerance")) {
      for (invalid in list(NA_real_, NaN)) {
        expect_error(
          domain_check(corrupt(template, column, invalid), list(0)),
          "invalid numeric bounds or tolerance",
          fixed = TRUE,
          info = paste(class(template)[[1L]], column, deparse(invalid))
        )
      }
    }
  }
  # Cargo is interpreted by `check` alone; `nlevels` does not read it.
  malformed_cargo = corrupt(p_uty(), "cargo", list(list(custom_check = 1)))
  expect_error(domain_check(malformed_cargo, list(1)), "Corrupt Domain storage")
  expect_identical(domain_nlevels(malformed_cargo), Inf)
})

test_that("large closed Domain loops remain interruptible and correct", {
  size = 65537L
  domain = native_domain_bind(rep(list(p_dbl(0, 1, tolerance = 0)), size))
  expect_true(domain_check(domain, as.list(rep(0.5, size))))
  expect_identical(domain_qunif(domain, rep(0.5, size)), rep(0.5, size))
  expect_identical(domain_sanitize(domain, rep(0.5, size)), as.list(rep(0.5, size)))
})
