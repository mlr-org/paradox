context("regression: detached data.table facade ownership")

test_that("data.table finalization never mutates an aliased input shell", {
  source = data.table::data.table(
    alpha = 1:3,
    beta = c("a", "b", "c")
  )
  alias = source
  source_bytes = serialize(source, NULL, version = 2L)
  source_address = data.table::address(source)
  source_names_address = data.table::address(names(source))
  source_column_addresses = vapply(source, data.table::address, character(1L))

  finalized = .Call(paradox:::C_finalize_data_table, source)

  expect_identical(alias, source)
  expect_identical(serialize(source, NULL, version = 2L), source_bytes)
  expect_identical(data.table::address(source), source_address)
  expect_identical(data.table::address(names(source)), source_names_address)
  expect_identical(
    vapply(source, data.table::address, character(1L)),
    source_column_addresses
  )
  expect_identical(data.table:::selfrefok(finalized, verbose = FALSE), 1L)
  expect_false(identical(
    data.table::address(finalized),
    data.table::address(source)
  ))
  expect_false(identical(
    data.table::address(names(finalized)),
    data.table::address(names(source))
  ))
  finalized_column_addresses = vapply(
    finalized,
    data.table::address,
    character(1L)
  )
  expect_true(all(finalized_column_addresses != source_column_addresses))
  expect_identical(as.list(finalized), as.list(source))

  data.table::setnames(finalized, c("first", "second"))
  expect_identical(names(source), c("alpha", "beta"))
  expect_identical(names(alias), c("alpha", "beta"))
})

test_that("named facade columns are owned before names are removed", {
  source = data.table::data.table(value = 1:3)
  data.table::setattr(source$value, "names", c("i", "j", "k"))
  source_bytes = serialize(source, NULL, version = 2L)
  source_column_address = data.table::address(source$value)

  finalized = .Call(paradox:::C_finalize_data_table, source)

  expect_identical(serialize(source, NULL, version = 2L), source_bytes)
  expect_identical(names(source$value), c("i", "j", "k"))
  expect_false(identical(
    data.table::address(finalized$value),
    source_column_address
  ))
  expect_null(names(finalized$value))
  expect_identical(data.table:::selfrefok(finalized, verbose = FALSE), 1L)
})

test_that("facade key and index metadata are detached", {
  source = data.table::data.table(
    key_id = c(1L, 2L),
    value = c(2L, 1L)
  )
  data.table::setkeyv(source, "key_id")
  data.table::setindexv(source, "value")
  source_bytes = serialize(source, NULL, version = 2L)
  source_sorted = attr(source, "sorted", exact = TRUE)
  source_index = attr(source, "index", exact = TRUE)

  finalized = .Call(paradox:::C_finalize_data_table, source)
  expect_identical(serialize(source, NULL, version = 2L), source_bytes)
  expect_identical(attr(finalized, "sorted", exact = TRUE), source_sorted)
  expect_identical(attr(finalized, "index", exact = TRUE), source_index)
  expect_false(identical(
    data.table::address(attr(finalized, "sorted", exact = TRUE)),
    data.table::address(source_sorted)
  ))
  expect_false(identical(
    data.table::address(attr(finalized, "index", exact = TRUE)),
    data.table::address(source_index)
  ))
})
