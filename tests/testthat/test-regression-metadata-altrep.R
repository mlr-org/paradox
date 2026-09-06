metadata_altrep_copy = function(value) {
  .Call(
    get(
      "C_test_builtin_metadata_copy_reentry",
      envir = asNamespace("paradox")
    ),
    value,
    NULL
  )
}

metadata_altrep_symbol = function(name) {
  get(paste0("C_", name), envir = asNamespace("paradox"), inherits = FALSE)
}

metadata_altrep_helpers_available = function() {
  namespace = asNamespace("paradox")
  exists("C_test_stateful_altrep", namespace, inherits = FALSE)
}

metadata_altrep_skip_without_helpers = function() {
  skip_if_not(
    metadata_altrep_helpers_available(),
    "the internal stateful ALTREP test class is unavailable"
  )
}

# Ordinary base-R output uses ALTREP for compact sequences and deferred string
# conversions. The ALTREP-aware serialization format records the provider, so
# comparing bytes against a known-ordinary equivalent is the portable way to
# assert that a stored attribute really was materialized.
metadata_altrep_bytes = function(value) serialize(value, NULL, version = 3L)

metadata_altrep_plain_integers = function(n) {
  result = integer(n)
  result[seq_len(n)] = seq_len(n)
  result
}

metadata_altrep_plain_characters = function(n) {
  result = character(n)
  result[seq_len(n)] = as.character(seq_len(n))
  result
}

# `names<-` on a compact ALTREP sequence produces a wrapper ALTREP once the
# sequence itself is compact, which base R does from 64 elements upwards.
metadata_altrep_named_doubles = function(n) {
  install = function(value) {
    names(value) = as.character(seq_len(n))
    value
  }
  install(as.numeric(seq_len(n)))
}

test_that("ordinary base-R ALTREP attribute values construct and commit", {
  special = p_int(
    0L,
    5L,
    special_vals = list(structure(3L, labels = as.character(1:3)))
  )
  expect_identical(as.vector(special$special_vals[[1L]][[1L]]), 3L)

  compact_special = p_int(
    0L,
    5L,
    special_vals = list(structure(3L, idx = 1:100))
  )
  expect_identical(as.vector(compact_special$special_vals[[1L]][[1L]]), 3L)

  defaulted = p_int(0L, 5L, default = structure(3L, labels = as.character(1:3)))
  expect_identical(as.vector(defaulted$default[[1L]]), 3L)

  initialized = p_int(0L, 5L, init = structure(3L, idx = 1:100))
  expect_identical(as.vector(initialized$.init[[1L]]), 3L)
  expect_identical(attr(initialized$.init[[1L]], "idx"), 1:100)

  compact_values = ps(x = p_dbl(0, 10))
  compact_values$values = list(x = structure(2.5, idx = 1:100))
  expect_identical(unname(compact_values$values$x), 2.5)

  named_value = 2.5
  names(named_value) = as.character(1)
  named_values = ps(x = p_dbl(0, 10))
  named_values$values = list(x = named_value)
  expect_identical(unname(named_values$values$x), 2.5)
})

test_that("stored ALTREP attribute values become ordinary metadata", {
  special = p_int(
    0L,
    5L,
    special_vals = list(structure(3L, labels = as.character(1:3)))
  )
  stored_special = special$special_vals[[1L]][[1L]]
  expect_identical(attr(stored_special, "labels"), as.character(1:3))
  expect_identical(
    metadata_altrep_bytes(attr(stored_special, "labels")),
    metadata_altrep_bytes(metadata_altrep_plain_characters(3L))
  )

  defaulted = p_int(0L, 5L, default = structure(3L, idx = 1:100))
  stored_default = defaulted$default[[1L]]
  expect_identical(attr(stored_default, "idx"), 1:100)
  expect_identical(
    metadata_altrep_bytes(attr(stored_default, "idx")),
    metadata_altrep_bytes(metadata_altrep_plain_integers(100L))
  )

  # The raw store keeps the assigned leaf verbatim, so it is the value path
  # that can show the stored attribute itself.
  raw = ps(x = p_dbl(0, 10))
  raw$assert_values = FALSE
  raw$values = list(x = structure(2.5, idx = 1:100))
  stored_value = raw$values$x
  expect_identical(as.vector(stored_value), 2.5)
  expect_identical(attr(stored_value, "idx"), 1:100)
  expect_identical(
    metadata_altrep_bytes(attr(stored_value, "idx")),
    metadata_altrep_bytes(metadata_altrep_plain_integers(100L))
  )

  named_value = 2.5
  names(named_value) = as.character(1)
  raw_names = ps(x = p_dbl(0, 10))
  raw_names$assert_values = FALSE
  raw_names$values = list(x = named_value)
  stored_names = raw_names$values$x
  expect_identical(unname(as.vector(stored_names)), 2.5)
  expect_identical(names(stored_names), "1")
  expect_identical(
    metadata_altrep_bytes(attr(stored_names, "names")),
    metadata_altrep_bytes("1")
  )
})

test_that("an unstable ALTREP attribute value still fails closed", {
  metadata_altrep_skip_without_helpers()

  unstable = native_stateful_altrep(
    c(1, 2, 3),
    c(9, 9, 9),
    elt_switch_after = 3L
  )
  value = 4L
  attr(value, "meta") = unstable
  expect_error(
    metadata_altrep_copy(value),
    "Built-in metadata ALTREP changed while being snapshotted",
    fixed = TRUE
  )

  stable = native_stateful_altrep(c(1, 2, 3), c(1, 2, 3))
  accepted = 4L
  attr(accepted, "meta") = stable
  owned = metadata_altrep_copy(accepted)
  expect_identical(as.vector(owned), 4L)
  expect_identical(attr(owned, "meta"), c(1, 2, 3))
})

test_that("structural ALTREP attribute values reject as ALTREP", {
  skip_if_no_list_altrep()
  metadata_altrep_skip_without_helpers()

  observations = 0L
  shell = native_stateful_altrep(
    list(1),
    list(2),
    callback = function() observations <<- observations + 1L,
    callback_after = 0L
  )
  value = 3L
  attr(value, "meta") = shell
  expect_error(
    metadata_altrep_copy(value),
    "Structural ALTREP metadata cannot be materialized as a built-in attribute",
    fixed = TRUE
  )
  expect_identical(observations, 0L)
})

test_that("bounded metadata accepts its documented limits exactly", {
  attribute_carrier = function(count) {
    value = 3L
    labels = sprintf("attribute_%03d", seq_len(count))
    attributes(value) = stats::setNames(as.list(seq_len(count)), labels)
    value
  }
  accepted_attributes = metadata_altrep_copy(attribute_carrier(64L))
  expect_length(attributes(accepted_attributes), 64L)
  expect_error(
    metadata_altrep_copy(attribute_carrier(65L)),
    "metadata must be ordinary, acyclic, and bounded"
  )

  # The carrier occupies the first of the 64 recursive frames, so 63 further
  # nested attribute values are admissible and the 64th is not.
  nested_carrier = function(frames) {
    node = 1L
    for (index in seq_len(frames - 1L)) {
      node = structure(2L, nest = node)
    }
    structure(3L, nest = node)
  }
  accepted_depth = metadata_altrep_copy(nested_carrier(63L))
  expect_identical(as.vector(accepted_depth), 3L)
  expect_error(
    metadata_altrep_copy(nested_carrier(64L)),
    "metadata must be ordinary, acyclic, and bounded"
  )

  # The carrier, its one stored attribute edge, and the list itself occupy
  # three of the 65,536 nodes; its elements occupy the rest.
  node_carrier = function(nodes) {
    structure(3L, nest = as.list(rep(1L, nodes - 3L)))
  }
  accepted_nodes = metadata_altrep_copy(node_carrier(65536L))
  expect_length(attr(accepted_nodes, "nest"), 65533L)
  expect_error(
    metadata_altrep_copy(node_carrier(65537L)),
    "metadata must be ordinary, acyclic, and bounded"
  )
})

test_that("wrapper ALTREP vectors pass semantic ingress at 63, 64, and 200", {
  for (size in c(63L, 64L, 200L)) {
    wrapped = metadata_altrep_named_doubles(size)

    expect_identical(
      .Call(
        metadata_altrep_symbol("domain_sanitize_builtin"),
        p_dbl(0, 1000),
        wrapped
      ),
      as.list(as.numeric(seq_len(size))),
      info = as.character(size)
    )

    units = metadata_altrep_named_doubles(size)
    units[] = 0.25
    expect_identical(
      unname(.Call(
        metadata_altrep_symbol("domain_qunif_builtin"),
        p_dbl(0, 8),
        units
      )),
      rep(2, size),
      info = as.character(size)
    )

    # The same carrier is also ordinary presentation metadata: a typed leaf
    # write must materialize it and keep both its payload and its names.
    carrier = 2.5
    attr(carrier, "carrier") = wrapped
    raw = ps(x = p_dbl(0, 10))
    raw$assert_values = FALSE
    raw$values = list(x = carrier)
    stored = attr(raw$values$x, "carrier")
    expect_identical(
      unname(stored),
      as.numeric(seq_len(size)),
      info = as.character(size)
    )
    expect_identical(
      names(stored),
      as.character(seq_len(size)),
      info = as.character(size)
    )
    ordinary = numeric(size)
    names(ordinary) = metadata_altrep_plain_characters(size)
    # R 3.6 may wrap an already referenced ordinary vector merely to install
    # names. Filling the payload afterwards materializes that representation,
    # giving the byte comparison the ordinary twin it claims to use.
    ordinary[seq_len(size)] = seq_len(size)
    expect_identical(
      metadata_altrep_bytes(stored),
      metadata_altrep_bytes(ordinary),
      info = as.character(size)
    )
  }
})

test_that("the unchecked value store refuses unclassifiable class metadata", {
  set = ps(v = p_uty())
  set$assert_values = FALSE
  expect_error(
    set$values <- list(v = structure(list(), class = c("mycls", NA))),
    "ParamSet value 'v' has class metadata that no ParamSet reader can classify",
    fixed = TRUE
  )
  expect_identical(set$values, structure(list(), names = character(0)))

  # An ordinary class shape is still committed without any value checking.
  set$values = list(v = structure(list(), class = c("mycls", "list")))
  expect_identical(class(set$values$v), c("mycls", "list"))
  expect_true(set$check(set$values))
})
