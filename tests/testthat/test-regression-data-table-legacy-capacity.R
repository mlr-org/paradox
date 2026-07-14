context("regression: legacy data.table pointer capacity")

expect_legacy_data_table_ready = function(table) {
  expect_s3_class(table, "data.table")
  expect_identical(data.table:::selfrefok(table, verbose = FALSE), 1L)
  expect_true(data.table::truelength(table) >= length(table))
  observed = expect_no_warning(table[, names(table), with = FALSE])
  expect_identical(names(observed), names(table))
  invisible(table)
}

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

  domain = expect_no_warning(param_set$get_domain("width"))
  expect_no_warning(domain[, names(domain), with = FALSE])
  domains = expect_no_warning(param_set$domains)
  lapply(domains, function(item) {
    expect_no_warning(item[, names(item), with = FALSE])
  })
})
