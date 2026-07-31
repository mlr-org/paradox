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
