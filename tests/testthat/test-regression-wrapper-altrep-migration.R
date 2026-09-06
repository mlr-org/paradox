context("wrapper ALTREP migration")

# Base R answers `names<-` on a referenced list of at least 64 elements with a
# wrapper ALTREP shell from R 4.3 on. Every fixture below is built the way
# ordinary user code builds it, so on older runtimes the same fixture is simply
# an ordinary list and the assertions still describe the required behavior.

wrapper_threshold_sizes = c(63L, 64L, 200L)

wrapper_carrier_candidate = function(label) {
  candidate = new.env(parent = emptyenv())
  candidate$label = label
  class(candidate) = c("ParamSet", "R6")
  candidate
}

wrapper_named_carrier = function(size, tail) {
  elements = lapply(seq_len(size), function(index) list(index = index))
  elements[[size]] = tail
  # `stats::setNames()` assigns names to its already-referenced argument, which
  # is exactly the base-R shape that produces the wrapper.
  stats::setNames(elements, paste0("c", seq_len(size)))
}

test_that("graph migration traverses a base names<- carrier at every size", {
  for (size in wrapper_threshold_sizes) {
    carrier = wrapper_named_carrier(size, wrapper_carrier_candidate("tail"))

    # Reaching the last element proves the whole carrier was traversed rather
    # than rejected up front.
    expect_error(
      upgrade_paradox_object_graph(carrier),
      sprintf(
        "Cannot upgrade Paradox object at x[[%d]]: missing canonical R6 enclosure",
        size
      ),
      fixed = TRUE
    )
  }
})

test_that("graph migration returns a base names<- carrier unchanged", {
  for (size in wrapper_threshold_sizes) {
    current = ps(x = p_dbl(0, 1))
    carrier = wrapper_named_carrier(size, current)
    names_before = names(carrier)

    expect_identical(upgrade_paradox_object_graph(carrier), carrier)
    expect_identical(carrier[[size]], current)
    expect_identical(names(carrier), names_before)
    expect_identical(.subset2(carrier, 1L), list(index = 1L))
  }
})

test_that("graph migration keeps carrier attributes and classes discoverable", {
  carrier = wrapper_named_carrier(64L, list(index = 64L))
  attr(carrier, "hidden") = wrapper_carrier_candidate("attribute")
  class(carrier) = "paradox_wrapper_carrier"
  `[.paradox_wrapper_carrier` = function(x, ...) {
    stop("carrier subset method dispatched")
  }

  expect_error(
    upgrade_paradox_object_graph(carrier),
    "Cannot upgrade Paradox object at x@attr[[\"hidden\"]]",
    fixed = TRUE
  )
  expect_identical(
    sort(names(attributes(carrier))),
    c("class", "hidden", "names")
  )
})

test_that("graph migration accepts a mixed current and legacy wrapper graph", {
  legacy = structure(
    list(rhs = 1L, condition_format_string = "%s == %s"),
    class = c("CondEqual", "Condition")
  )
  for (size in wrapper_threshold_sizes) {
    levels = as.list(seq_len(size))
    names(levels) = paste0("L", seq_len(size))
    current = ps(f = p_fct(levels))
    carrier = wrapper_named_carrier(size, list(a = current, b = legacy))

    expect_identical(upgrade_paradox_object_graph(carrier), carrier)
    expect_identical(current$levels$f, paste0("L", seq_len(size)))
  }
})

test_that("p_fct materializes a list levels carrier before capturing it", {
  for (size in wrapper_threshold_sizes) {
    levels = as.list(seq_len(size))
    names(levels) = paste0("L", seq_len(size))
    domain = p_fct(levels)
    callback_environment = environment(domain$.trafo[[1L]])
    captured = paradox:::.paradox_plain_binding_snapshot(
      callback_environment,
      "levels"
    )
    captured_trafo = paradox:::.paradox_plain_binding_snapshot(
      callback_environment,
      "trafo"
    )

    expect_true(captured$ok)
    expect_true(paradox:::.upgrade_paradox_is_ordinary_list(captured$value))
    expect_identical(names(captured$value), paste0("L", seq_len(size)))
    expect_identical(captured_trafo, list(ok = TRUE, value = NULL))
    expect_identical(domain$.trafo[[1L]]("L5"), 5L)
    expect_identical(domain$levels[[1L]], paste0("L", seq_len(size)))
  }
})

test_that("p_fct materializes an unnamed list levels carrier", {
  for (size in wrapper_threshold_sizes) {
    levels = as.list(seq_len(size))
    domain = p_fct(levels)
    captured = get("levels", envir = environment(domain$.trafo[[1L]]))

    expect_true(paradox:::.upgrade_paradox_is_ordinary_list(captured))
    expect_identical(names(captured), as.character(seq_len(size)))
    expect_identical(domain$.trafo[[1L]]("5"), 5L)
  }
})

test_that("p_fct keeps every levels carrier attribute it was given", {
  levels = as.list(1:64)
  names(levels) = paste0("L", 1:64)
  attr(levels, "provenance") = "fixture"
  domain = p_fct(levels)
  captured = get("levels", envir = environment(domain$.trafo[[1L]]))

  expect_true(paradox:::.upgrade_paradox_is_ordinary_list(captured))
  expect_identical(attr(captured, "provenance"), "fixture")
  expect_identical(domain$.trafo[[1L]]("L64"), 64L)
})

test_that("the migration carrier helper never dispatches a subset method", {
  dispatches = 0L
  `[.paradox_carrier_probe` = function(x, ...) {
    dispatches <<- dispatches + 1L
    NextMethod()
  }
  carrier = structure(
    stats::setNames(as.list(1:64), paste0("c", 1:64)),
    class = "paradox_carrier_probe",
    marker = "kept"
  )

  copy = paradox:::.paradox_materialize_list_carrier(carrier)

  expect_identical(dispatches, 0L)
  expect_true(paradox:::.upgrade_paradox_is_ordinary_list(copy))
  expect_identical(names(copy), paste0("c", 1:64))
  expect_identical(attr(copy, "marker"), "kept")
  expect_identical(class(copy), "paradox_carrier_probe")
  expect_identical(.subset2(copy, 7L), .subset2(carrier, 7L))
})
