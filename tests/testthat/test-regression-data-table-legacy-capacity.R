context("regression: legacy data.table pointer capacity")

expect_legacy_data_table_ready = function(table) {
  expect_s3_class(table, "data.table")
  expect_identical(data.table:::selfrefok(table, verbose = FALSE), 1L)
  expect_true(data.table::truelength(table) >= length(table))
  observed = expect_no_warning(table[, names(table), with = FALSE])
  expect_identical(names(observed), names(table))
  invisible(table)
}

expect_legacy_data_table_growable = function(table) {
  expect_legacy_data_table_ready(table)
  expect_true(data.table::truelength(table) > length(table))
  expect_no_warning(data.table::set(
    table,
    j = ".capacity_probe",
    value = seq_len(nrow(table))
  ))
  expect_true(".capacity_probe" %in% names(table))
  invisible(table)
}

test_that("data.table finalization never mutates an aliased input shell", {
  source = data.table::data.table(
    alpha = 1:3,
    beta = c("a", "b", "c")
  )
  alias = source
  source_bytes = serialize(source, NULL, version = 2L)
  source_attributes = names(attributes(source))
  source_address = data.table::address(source)
  source_names_address = data.table::address(names(source))
  source_column_addresses = vapply(source, data.table::address, character(1L))

  finalized = .Call(paradox:::C_finalize_data_table, source)

  expect_identical(alias, source)
  expect_identical(serialize(source, NULL, version = 2L), source_bytes)
  expect_identical(names(attributes(source)), source_attributes)
  expect_identical(data.table::address(source), source_address)
  expect_identical(data.table::address(names(source)), source_names_address)
  expect_identical(
    vapply(source, data.table::address, character(1L)),
    source_column_addresses
  )
  expect_identical(data.table:::selfrefok(source, verbose = FALSE), 1L)

  expect_false(identical(
    data.table::address(finalized),
    data.table::address(source)
  ))
  expect_false(identical(
    data.table::address(names(finalized)),
    data.table::address(names(source))
  ))
  expect_identical(
    vapply(finalized, data.table::address, character(1L)),
    source_column_addresses
  )
  expect_identical(
    names(attributes(finalized)),
    c(
      setdiff(source_attributes, c(".internal.selfref", "names")),
      ".internal.selfref",
      "names"
    )
  )
  expect_identical(data.table:::selfrefok(finalized, verbose = FALSE), 1L)

  data.table::setnames(finalized, c("first", "second"))
  expect_identical(names(source), c("alpha", "beta"))
  expect_identical(names(alias), c("alpha", "beta"))

  named_source = data.table::data.table(value = 1:3)
  data.table::setattr(named_source$value, "names", c("i", "j", "k"))
  named_alias = named_source
  named_bytes = serialize(named_source, NULL, version = 2L)
  named_address = data.table::address(named_source)
  named_column_address = data.table::address(named_source$value)
  named_finalized = .Call(paradox:::C_finalize_data_table, named_source)
  expect_identical(named_alias, named_source)
  expect_identical(serialize(named_source, NULL, version = 2L), named_bytes)
  expect_identical(data.table::address(named_source), named_address)
  expect_identical(data.table::address(named_source$value), named_column_address)
  expect_identical(names(named_source$value), c("i", "j", "k"))
  expect_identical(data.table:::selfrefok(named_source, verbose = FALSE), 1L)
  expect_false(identical(
    data.table::address(named_finalized$value),
    named_column_address
  ))
  expect_null(names(named_finalized$value))
  expect_identical(
    data.table:::selfrefok(named_finalized, verbose = FALSE),
    1L
  )

  indexed_source = data.table::data.table(
    key_id = c(1L, 2L),
    value = c(2L, 1L)
  )
  data.table::setkeyv(indexed_source, "key_id")
  data.table::setindexv(indexed_source, "value")
  indexed_bytes = serialize(indexed_source, NULL, version = 2L)
  source_sorted = attr(indexed_source, "sorted", exact = TRUE)
  source_index = attr(indexed_source, "index", exact = TRUE)
  indexed = .Call(paradox:::C_finalize_data_table, indexed_source)
  expect_identical(serialize(indexed_source, NULL, version = 2L), indexed_bytes)
  expect_identical(attr(indexed, "sorted", exact = TRUE), source_sorted)
  expect_identical(attr(indexed, "index", exact = TRUE), source_index)
  expect_false(identical(
    data.table::address(attr(indexed, "sorted", exact = TRUE)),
    data.table::address(source_sorted)
  ))
  expect_false(identical(
    data.table::address(attr(indexed, "index", exact = TRUE)),
    data.table::address(source_index)
  ))
  data.table::setattr(attr(indexed, "sorted", exact = TRUE), "probe", TRUE)
  data.table::setattr(attr(indexed, "index", exact = TRUE), "probe", TRUE)
  expect_null(attr(source_sorted, "probe", exact = TRUE))
  expect_null(attr(source_index, "probe", exact = TRUE))
  expect_identical(serialize(indexed_source, NULL, version = 2L), indexed_bytes)
})

test_that("native table shells remain usable with data.table before 1.18", {
  skip_if(
    utils::packageVersion("data.table") >= package_version("1.18.0"),
    "legacy data.table compatibility bridge is inactive"
  )

  param_set = expect_no_warning(ps(
    width = p_dbl(0, 4, tags = "numeric", trafo = function(x) x * 2),
    count = p_int(1, 5),
    mode = p_fct(c("a", "b"))
  ))
  private = param_set$.__enclos_env__$private
  params = expect_no_warning(param_set$params)
  quantiles = expect_no_warning(param_set$qunif(data.table::data.table(
    width = 0.5,
    count = 0.5,
    mode = 0.5
  )))
  subset = expect_no_warning(param_set$subset(c("width", "mode")))

  other = expect_no_warning(ps(
    flag = p_lgl(),
    depth = p_int(0, 3)
  ))
  collection = expect_no_warning(ParamSetCollection$new(list(
    left = param_set,
    right = other
  )))
  collection_private = collection$.__enclos_env__$private
  collection_params = expect_no_warning(collection$params)
  collection_deps = expect_no_warning(collection$deps)

  tables = list(
    private_params = private$.params,
    private_tags = private$.tags,
    private_trafos = private$.trafos,
    params = params,
    quantiles = quantiles,
    subset_params = subset$.__enclos_env__$private$.params,
    collection_private_params = collection_private$.params,
    collection_private_tags = collection_private$.tags,
    collection_private_trafos = collection_private$.trafos,
    collection_params = collection_params,
    collection_deps = collection_deps
  )
  lapply(tables, expect_legacy_data_table_ready)
  lapply(
    list(params, quantiles, collection_params, collection_deps),
    expect_legacy_data_table_growable
  )

  domain = expect_no_warning(param_set$get_domain("width"))
  expect_legacy_data_table_ready(domain)
  expect_identical(
    names(attributes(domain)),
    c("class", "row.names", ".internal.selfref", "names")
  )
  expect_legacy_data_table_growable(domain)
  domains = expect_no_warning(param_set$domains)
  second_domains = expect_no_warning(param_set$domains)
  lapply(domains, expect_legacy_data_table_ready)
  expect_false(identical(
    data.table::address(names(domains)),
    data.table::address(private$.params$id)
  ))
  expect_false(identical(
    data.table::address(names(domains)),
    data.table::address(names(second_domains))
  ))

  old_capacity = getOption("datatable.alloccol")
  on.exit(options(datatable.alloccol = old_capacity), add = TRUE)
  options(datatable.alloccol = 0L)
  zero_capacity = expect_no_warning(ps(x = p_dbl())$params)
  expect_legacy_data_table_ready(zero_capacity)
  expect_identical(data.table::truelength(zero_capacity), length(zero_capacity))
})
