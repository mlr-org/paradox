context("wrapper ALTREP public inputs")

# Base R answers `names<-` (and every other attribute setter) on a referenced
# list of at least 64 elements with a wrapper ALTREP shell, so
# `setNames(x, ids)` is an ordinary public spelling of a named list. Every
# public list boundary materializes such a shell exactly once before its
# ordinary-list gate; the fixtures below are built the way user code builds
# them, so on runtimes without the wrapper they are ordinary lists and the
# assertions still describe the required behavior.

wrapper_input_sizes = c(63L, 64L, 200L)

wrapper_domains = function(size) {
  rep(
    list(p_dbl(0, 1), p_int(0L, 5L), p_fct(c("a", "b")), p_lgl()),
    length.out = size
  )
}

wrapper_values = function(size) {
  rep(list(0.5, 2L, "a", TRUE), length.out = size)
}

wrapper_ids = function(size) {
  sprintf("x%d", seq_len(size))
}

# `stats::setNames()` names its already-referenced argument, which is exactly
# the base-R shape that produces the wrapper.
wrapper_named = function(elements, ids) {
  stats::setNames(elements, ids)
}

ordinary_named = function(elements, ids) {
  copy = elements[seq_along(elements)]
  names(copy) = ids
  copy
}

test_that("ParamSet construction accepts a base names<- wrapper list", {
  for (size in wrapper_input_sizes) {
    ids = wrapper_ids(size)
    domains = wrapper_domains(size)
    expected = ParamSet$new(ordinary_named(domains, ids))
    actual = ParamSet$new(wrapper_named(domains, ids))
    expect_identical(actual$ids(), ids)
    expect_identical(actual$params, expected$params)
    expect_identical(actual$class, expected$class)
  }
})

test_that("value assignment and checking accept a base names<- wrapper list", {
  for (size in wrapper_input_sizes) {
    ids = wrapper_ids(size)
    set = ParamSet$new(ordinary_named(wrapper_domains(size), ids))
    values = wrapper_values(size)
    expected = ordinary_named(values, ids)

    set$values = wrapper_named(values, ids)
    expect_identical(set$values, expected)
    expect_identical(set$check(wrapper_named(values, ids)), TRUE)
    expect_true(set$test(wrapper_named(values, ids)))
    expect_identical(set$check_dependencies(wrapper_named(values, ids)), TRUE)
    expect_true(set$test_constraint(wrapper_named(values, ids)))
    expect_invisible(set$assert(wrapper_named(values, ids)))
    expect_identical(set$trafo(wrapper_named(values, ids)), expected)

    set$values = list()
    set$set_values(.values = wrapper_named(values, ids))
    expect_identical(set$values, expected)

    # A rejected point is still diagnosed after materialization.
    broken = values
    broken[[1L]] = 2
    expect_match(set$check(wrapper_named(broken, ids)), "x1")
    expect_false(set$test(wrapper_named(broken, ids)))
  }
})

test_that("tags, search spaces, and collections accept a base names<- wrapper list", {
  for (size in wrapper_input_sizes) {
    ids = wrapper_ids(size)
    set = ParamSet$new(ordinary_named(wrapper_domains(size), ids))

    tags = rep(list("tagged"), size)
    set$tags = wrapper_named(tags, ids)
    expect_identical(set$tags, ordinary_named(tags, ids))

    tokens = rep(list(to_tune()), size)
    expect_identical(set$search_space(wrapper_named(tokens, ids))$ids(), ids)

    sets = lapply(seq_len(size), function(index) ps(a = p_int()))
    collection = ParamSetCollection$new(wrapper_named(sets, ids))
    expect_identical(collection$ids(), paste0(ids, ".a"))
  }
})

test_that("a wrapper list is materialized as a copy of its current elements", {
  ids = wrapper_ids(64L)
  set = ParamSet$new(ordinary_named(wrapper_domains(64L), ids))
  values = wrapper_values(64L)
  input = wrapper_named(values, ids)
  set$values = input
  input[[1L]] = 0.25
  expect_identical(set$values$x1, 0.5)
  expect_identical(set$values, ordinary_named(values, ids))
})

test_that("an ALTREP list input is observed exactly once per element", {
  skip_if_no_list_altrep()
  skip_if_not(
    exists("C_test_stateful_altrep_calls", asNamespace("paradox"), inherits = FALSE),
    "the internal stateful ALTREP call counter is unavailable"
  )
  calls = function(value) {
    .Call(
      get("C_test_stateful_altrep_calls", envir = asNamespace("paradox")),
      value
    )
  }
  ids = c("a", "b", "c")
  set = ps(a = p_int(0L, 5L), b = p_int(0L, 5L), c = p_int(0L, 5L))
  first = structure(list(1L, 2L, 3L), names = ids)
  later = structure(list(9L, 9L, 9L), names = ids)

  # Every element is delivered before the provider switches, and the switch
  # never becomes visible: the operation works on the captured generation.
  point = native_stateful_altrep(first, later, elt_switch_after = 3L)
  expect_identical(set$check(point), TRUE)
  expect_identical(calls(point)[["elt"]], 3L)

  values = native_stateful_altrep(first, later, elt_switch_after = 3L)
  set$values = values
  expect_identical(calls(values)[["elt"]], 3L)
  expect_identical(set$values, ordinary_named(list(1L, 2L, 3L), ids))

  domains = native_stateful_altrep(
    structure(list(p_int(0L, 1L), p_dbl(0, 1)), names = c("p", "q")),
    structure(list(p_int(7L, 8L), p_dbl(7, 8)), names = c("p", "q")),
    elt_switch_after = 2L
  )
  constructed = ParamSet$new(domains)
  expect_identical(calls(domains)[["elt"]], 2L)
  expect_identical(constructed$lower, c(p = 0, q = 0))
})
