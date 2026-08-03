test_that("typed special values retain ordinary equality but make S4 exact", {
  ordinary_default = list(payload = list(code = 1L, label = "legacy"))
  ordinary_init = list(payload = list(code = 2L, label = "legacy"))
  ordinary_equal = unserialize(serialize(ordinary_default, NULL))
  expect_identical(ordinary_equal, ordinary_default)
  expect_false(identical(
    data.table::address(ordinary_equal),
    data.table::address(ordinary_default)
  ))

  ordinary = p_dbl(
    0,
    1,
    special_vals = list(ordinary_default, ordinary_init),
    default = ordinary_equal,
    init = unserialize(serialize(ordinary_init, NULL))
  )
  expect_true(domain_test(
    ordinary,
    list(unserialize(serialize(ordinary_default, NULL)))
  ))

  s4_default = asS4(0.5)
  s4_init = asS4(0.75)
  s4_equal = unserialize(serialize(s4_default, NULL))
  expect_identical(s4_equal, s4_default)
  expect_false(identical(
    data.table::address(s4_equal),
    data.table::address(s4_default)
  ))

  exact = p_dbl(
    0,
    1,
    special_vals = list(s4_default, s4_init),
    default = s4_default,
    init = s4_init
  )
  expect_true(domain_test(exact, list(s4_default)))
  expect_false(domain_test(exact, list(s4_equal)))
  expect_error(
    p_dbl(0, 1, special_vals = list(s4_default), default = s4_equal),
    "Must be of type 'number', not 'double'",
    fixed = TRUE
  )
  expect_error(
    p_dbl(0, 1, special_vals = list(s4_default), init = s4_equal),
    "Must be of type 'number', not 'double'",
    fixed = TRUE
  )

  formal_class = "ParadoxAdversarialFormalSpecial"
  if (!methods::isClass(formal_class)) {
    methods::setClass(formal_class, slots = c(payload = "integer"))
  }
  formal_default = methods::new(formal_class, payload = 1L)
  formal_init = methods::new(formal_class, payload = 2L)
  formal = p_int(
    special_vals = list(formal_default, formal_init),
    default = formal_default,
    init = formal_init
  )
  expect_identical(formal$default[[1L]], formal_default)
  expect_identical(formal$.init[[1L]], formal_init)

  # The package-owned NoDefault marker is an exact ordinary S3 shape, not an
  # inheritance spelling. A formal S4 class with the same outward name remains
  # an opaque semantic leaf through both constructor admission and ParamSet
  # table detachment.
  class_where = new.env(parent = emptyenv())
  methods::setClass(
    "NoDefault",
    slots = c(payload = "integer"),
    where = class_where
  )
  on.exit(
    suppressWarnings(methods::removeClass("NoDefault", where = class_where)),
    add = TRUE
  )
  formal_no_default = methods::new(
    methods::getClassDef("NoDefault", where = class_where),
    payload = 3L
  )
  formal_no_default_init = methods::new(
    methods::getClassDef("NoDefault", where = class_where),
    payload = 4L
  )
  named_formal = p_uty(
    special_vals = list(formal_no_default, formal_no_default_init),
    default = formal_no_default,
    init = formal_no_default_init
  )
  named_formal_set = ps(value = named_formal)
  expect_identical(named_formal$default[[1L]], formal_no_default)
  expect_identical(named_formal$.init[[1L]], formal_no_default_init)
  expect_identical(
    named_formal_set$params$default[[1L]],
    formal_no_default
  )
  expect_identical(
    named_formal_set$params$.init[[1L]],
    formal_no_default_init
  )
  if (requireNamespace("knitr", quietly = TRUE)) {
    expect_match(
      rd_info(named_formal_set),
      "formal_no_default",
      fixed = TRUE
    )
  }

  typed_set = ps(value = exact)
  expect_true(typed_set$test(list(value = s4_default)))
  expect_false(typed_set$test(list(value = s4_equal)))
  ordinary_set = ps(value = ordinary)
  expect_true(ordinary_set$test(list(value = ordinary_equal)))

  other_typed = list(
    integer = list(
      value = 1L,
      domain = function(value) p_int(0L, 2L, special_vals = list(value))
    ),
    factor = list(
      value = "level",
      domain = function(value) p_fct("level", special_vals = list(value))
    ),
    logical = list(
      value = TRUE,
      domain = function(value) p_lgl(special_vals = list(value))
    )
  )
  for (kind in names(other_typed)) {
    special = asS4(other_typed[[kind]]$value)
    equal = unserialize(serialize(special, NULL))
    domain = other_typed[[kind]]$domain(special)
    expect_true(domain_test(domain, list(special)), info = kind)
    expect_false(domain_test(domain, list(equal)), info = kind)
  }
})

test_that("ParamUty special matching preserves opaque identity semantics", {
  calls = 0L
  checker = function(value) {
    calls <<- calls + 1L
    if (identical(value, 1)) TRUE else "custom check reached"
  }

  list_special = list(payload = list(code = 1L))
  list_equal = unserialize(serialize(list_special, NULL))
  s4_special = asS4(list(payload = 2L))
  s4_equal = unserialize(serialize(s4_special, NULL))
  environment_special = new.env(parent = emptyenv())
  environment_special$payload = 3L
  environment_equal = new.env(parent = emptyenv())
  environment_equal$payload = 3L

  expect_identical(list_equal, list_special)
  expect_identical(s4_equal, s4_special)
  expect_false(identical(environment_equal, environment_special))

  utility = p_uty(
    custom_check = checker,
    special_vals = list(
      list_special,
      s4_special,
      environment_special,
      NULL
    ),
    default = list_equal,
    init = s4_equal
  )
  # Construction validates the callback once with its documented probe.  The
  # structurally equal opaque specials bypass it for default and initial-value
  # checks as well.
  expect_identical(calls, 1L)

  formal_class = "ParadoxAdversarialFormalUtility"
  if (!methods::isClass(formal_class)) {
    methods::setClass(formal_class, slots = c(payload = "integer"))
  }
  formal_default = methods::new(formal_class, payload = 1L)
  formal_init = methods::new(formal_class, payload = 2L)
  formal_utility = p_uty(default = formal_default, init = formal_init)
  expect_identical(formal_utility$default[[1L]], formal_default)
  expect_identical(formal_utility$.init[[1L]], formal_init)

  calls = 0L
  expect_true(domain_test(utility, list(list_equal)))
  expect_true(domain_test(utility, list(s4_equal)))
  expect_true(domain_test(utility, list(environment_special)))
  expect_true(domain_test(utility, list(NULL)))
  expect_identical(calls, 0L)

  expect_false(domain_test(utility, list(environment_equal)))
  expect_identical(calls, 1L)
  expect_false(domain_test(utility, list(asS4(list(payload = 4L)))))
  expect_identical(calls, 2L)

  param_set = ps(payload = utility)
  calls = 0L
  expect_true(param_set$test(list(payload = list_equal)))
  expect_true(param_set$test(list(payload = s4_equal)))
  expect_true(param_set$test(list(payload = environment_special)))
  expect_true(param_set$test(list(payload = NULL)))
  expect_identical(calls, 0L)

  param_set$values = list(payload = list_equal)
  expect_identical(param_set$values, list(payload = list_equal))
  expect_identical(calls, 0L)
  expect_false(param_set$test(list(payload = environment_equal)))
  expect_identical(calls, 1L)
})

# Only the first `inspect` line describes the object itself; later lines are its
# attributes, whose representation is a separate question. `names<-` on an
# atomic vector materializes the value and leaves a deferred-string names
# attribute, so the value and its metadata must be judged apart.
special_leaf_is_altrep = function(value) {
  grepl(
    "compact|deferred|wrapper",
    capture.output(.Internal(inspect(value)))[[1L]]
  )
}

special_wrapper_recipe = function(value) {
  names(value) = as.character(seq_along(value))
  value
}

test_that("typed special values admit stable ALTREP spellings", {
  # A typed special value is a semantic value, not an identity token, so the
  # representation its caller happened to build is not part of the Domain.
  altrep_specials = list(
    compact_integer = list(domain = function(s) p_int(0L, 1000L, special_vals = s),
      leaf = 1:5),
    compact_real = list(domain = function(s) p_dbl(0, 1000, special_vals = s),
      leaf = as.numeric(1:3)),
    deferred_string = list(domain = function(s) p_fct(c("a", "b"), special_vals = s),
      leaf = as.character(1:3))
  )
  for (name in names(altrep_specials)) {
    case = altrep_specials[[name]]
    expect_true(special_leaf_is_altrep(case$leaf), info = name)
    domain = case$domain(list(case$leaf))
    stored = domain$special_vals[[1L]][[1L]]
    # The admitted leaf is materialized once; nothing ALTREP is retained.
    expect_false(special_leaf_is_altrep(stored), info = name)
    expect_identical(stored, case$leaf, info = name)
  }

  # The `names<-` wrapper recipe at the repository's boundary sizes. For an
  # atomic leaf the recipe materializes the value itself and leaves only the
  # names attribute deferred. Newer R releases leave the character form as a
  # deferred string too; R 3.6 materializes that value while installing names,
  # which is an equally valid ordinary construction input.
  for (size in c(63L, 64L, 200L)) {
    label = paste("size", size)
    numeric_leaf = special_wrapper_recipe(as.numeric(seq_len(size)))
    integer_leaf = special_wrapper_recipe(seq_len(size))
    character_leaf = special_wrapper_recipe(as.character(seq_len(size)))
    if (getRversion() >= "4.0.0") {
      # R >= 4.0 keeps the character recipe a deferred string at every
      # boundary size, proving the loop exercises live ALTREP ingress there.
      # R 3.6 materializes it while installing names (and wraps all three
      # kinds from 64 elements), which the same admissions below still cover.
      expect_true(special_leaf_is_altrep(character_leaf), info = label)
    }
    numeric_domain = p_dbl(0, 1e6, special_vals = list(numeric_leaf))
    integer_domain = p_int(0L, 1000000L, special_vals = list(integer_leaf))
    character_domain = p_fct(c("a", "b"), special_vals = list(character_leaf))
    for (pair in list(
      list(domain = numeric_domain, leaf = numeric_leaf),
      list(domain = integer_domain, leaf = integer_leaf),
      list(domain = character_domain, leaf = character_leaf)
    )) {
      stored = pair$domain$special_vals[[1L]][[1L]]
      expect_false(special_leaf_is_altrep(stored), info = label)
      expect_identical(stored, pair$leaf, info = label)
    }
  }
})

test_that("an ALTREP special value is exactly its materialized twin", {
  altrep = p_int(0L, 10L, special_vals = list(1:5))
  ordinary = p_int(0L, 10L, special_vals = list(c(1L, 2L, 3L, 4L, 5L)))
  # Representation is not Domain state: the two Domains are one value.
  expect_identical(altrep, ordinary)

  for (value in list(1:5, c(1L, 2L, 3L, 4L, 5L), 3L, 99L, c(1L, 2L))) {
    label = paste(deparse(value), collapse = "")
    from_altrep = tryCatch(
      domain_check(altrep, list(value)),
      error = function(condition) conditionMessage(condition)
    )
    from_ordinary = tryCatch(
      domain_check(ordinary, list(value)),
      error = function(condition) conditionMessage(condition)
    )
    expect_identical(from_altrep, from_ordinary, info = label)
  }
  expect_true(domain_check(altrep, list(1:5)))
  expect_true(domain_check(altrep, list(3L)))
  expect_identical(domain_qunif(altrep, 0.5), 5L)
  expect_identical(domain_nlevels(altrep), 11)

  # The special survives a serialization round trip as an ordinary value.
  restored = unserialize(serialize(altrep, NULL))
  expect_true(domain_check(restored, list(1:5)))
  expect_false(special_leaf_is_altrep(restored$special_vals[[1L]][[1L]]))

  # The BASE-ParamSet construction ingress reaches the same state.
  set = ps(a = p_int(0L, 10L, special_vals = list(1:5)))
  expect_true(set$check(list(a = 1:5)))
  expect_false(
    special_leaf_is_altrep(set$params$special_vals[[1L]][[1L]])
  )
})

test_that("masked admission rejects an ALTREP special installed by reference", {
  # A constructed Domain stores the materialized leaf, so an ALTREP leaf in a
  # live table can only have been written by reference afterwards. Operation
  # time may not observe it, so it stays a structural rejection there.
  mutator = get("C_test_gc_column_mutator", envir = asNamespace("paradox"))
  install_special_leaf = function(domain, leaf) {
    pointer = .Call(
      mutator,
      domain,
      match("special_vals", names(domain)) - 1L,
      list(list(leaf))
    )
    rm(pointer)
    for (iteration in 1:3) {
      invisible(gc(full = TRUE))
    }
    domain
  }

  # Labels are fixed strings: `deparse()` and friends read every element and
  # materialize the leaf in place, which would disarm the fixture before it is
  # installed.
  tampered_leaves = list(
    compact_integer = 1:5,
    deferred_string = as.character(1:3),
    compact_real = as.numeric(1:3)
  )
  for (label in names(tampered_leaves)) {
    domain = install_special_leaf(
      p_int(0L, 10L, special_vals = list(c(1L, 2L))),
      tampered_leaves[[label]]
    )
    expect_true(
      special_leaf_is_altrep(domain$special_vals[[1L]][[1L]]),
      info = label
    )
    expect_error(
      domain_check(domain, list(3L)),
      "Corrupt Domain storage: `special_vals` is not canonical",
      fixed = TRUE,
      info = label
    )
    # Operations whose mask does not interpret `special_vals` are unaffected:
    # the rejection belongs to the rule, not to the table.
    expect_identical(domain_sanitize(domain, list(3L)), list(3L), info = label)
    expect_identical(domain_qunif(domain, 0.5), 5L, info = label)
    expect_identical(domain_nlevels(domain), 11, info = label)
  }

  # The same by-reference write with an ordinary leaf is admitted, so the
  # rejection above is about the representation and nothing else.
  ordinary = install_special_leaf(
    p_int(0L, 10L, special_vals = list(c(1L, 2L))),
    c(7L, 8L)
  )
  expect_true(domain_check(ordinary, list(3L)))
  expect_true(domain_check(ordinary, list(c(7L, 8L))))
})

test_that("ALTREP specials the value owner cannot materialize stay rejected", {
  # The canonical value-leaf owner materializes non-S4 atomic leaves only, and
  # returns everything else by identity. A leaf it would return unchanged must
  # not be admitted, or the Domain would store a live ALTREP.
  s4_altrep = asS4(as.character(1:3))
  expect_true(isS4(s4_altrep))
  if (special_leaf_is_altrep(s4_altrep)) {
    expect_error(
      p_fct(c("a", "b"), special_vals = list(s4_altrep)),
      "Invalid built-in Domain final state in field `special_vals`",
      fixed = TRUE
    )
  } else {
    # R 3.6 materializes a deferred string when the S4 bit is installed. That
    # result is an ordinary S4 identity leaf and follows the established
    # pointer-special contract; this runtime cannot spell the rejected case.
    ordinary_s4 = p_fct(c("a", "b"), special_vals = list(s4_altrep))
    stored_s4 = ordinary_s4$special_vals[[1L]][[1L]]
    expect_identical(stored_s4, s4_altrep)
    expect_identical(
      data.table::address(stored_s4),
      data.table::address(s4_altrep)
    )
  }

  # The same leaf without the S4 bit is admitted, so the rejection is the S4
  # identity contract rather than the representation.
  expect_silent(p_fct(c("a", "b"), special_vals = list(as.character(1:3))))

  # An ordinary S4 leaf keeps its established identity semantics.
  expect_silent(p_int(0L, 10L, special_vals = list(asS4(c(1L, 2L)))))
})

test_that("a Dataptr-less ALTREP special fails closed at the public boundary", {
  # This fixture implements Elt and Length but no Dataptr, so R's own argument
  # handling in the public constructor refuses it before the native ingress is
  # reached. The point of the test is that the special-value role is no weaker
  # than its established peers: the same provider is rejected identically as a
  # default and as an initial value.
  provider = function() {
    native_stateful_altrep(c(1, 2, 3), c(9, 9, 9), elt_switch_after = 1L)
  }
  expect_error(p_dbl(0, 100, special_vals = list(provider())))
  expect_error(p_dbl(0, 100, default = provider()))
  expect_error(p_dbl(0, 100, init = provider()))
})
