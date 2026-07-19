#!/usr/bin/env Rscript

# Exercise the two serialized, real-world optimization spaces pinned from
# mlr-org/mbo_config.  Keep this independent of mlr3mbo so that it isolates the
# paradox serialization, construction, design, quantile and mutation surface.

fail <- function(...) stop(..., call. = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  fail("usage: mbo-config.R SOURCE_DIRECTORY ROWS")
}
source_directory <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
rows <- suppressWarnings(as.integer(args[[2L]]))
if (is.na(rows) || !identical(as.character(rows), args[[2L]]) || rows < 1L) {
  fail("ROWS must be a positive integer")
}

suppressPackageStartupMessages(library(paradox))
if (!identical(as.character(utils::packageVersion("paradox")), "2.0.0")) {
  fail("the workload did not load the paradox 2.0.0 candidate")
}

fixtures <- list(
  mixed = c(
    "input_trafo", "output_trafo", "init", "init_size_fraction",
    "random_interleave_iter", "trees", "variance_estimator", "acqf",
    "lambda", "epsilon_decay", "lambda_decay", "acqopt"
  ),
  numeric = c(
    "input_trafo", "output_trafo", "init", "init_size_fraction",
    "random_interleave_iter", "surrogate", "extratrees", "trees",
    "variance_estimator", "kernel", "nugget", "scaling", "acqf",
    "lambda", "epsilon_decay", "lambda_decay", "acqopt"
  )
)

results <- lapply(seq_along(fixtures), function(index) {
  fixture <- names(fixtures)[[index]]
  expected_ids <- fixtures[[index]]
  path <- file.path(
    source_directory, "common", paste0(fixture, "_search_space.rds")
  )
  if (!file.exists(path) || dir.exists(path) || nzchar(Sys.readlink(path))) {
    fail("missing or symbolic mbo_config fixture: ", path)
  }

  legacy_parameter_set <- readRDS(path)
  legacy_bytes <- serialize(legacy_parameter_set, NULL, version = 3L)
  parameter_set <- upgrade_paradox_object(legacy_parameter_set)
  if (!identical(
    serialize(legacy_parameter_set, NULL, version = 3L),
    legacy_bytes
  )) {
    fail("upgrading the serialized ", fixture, " search space mutated its input")
  }
  ids <- parameter_set$ids()
  if (!inherits(parameter_set, "ParamSet") ||
      !identical(ids, expected_ids) ||
      !identical(parameter_set$length, length(expected_ids)) ||
      !identical(parameter_set$params$id, expected_ids) ||
      !identical(names(parameter_set$domains), expected_ids)) {
    fail("serialized ", fixture, " search space has unexpected structure")
  }

  set.seed(20260714L + index)
  design <- generate_design_random(parameter_set, rows)
  if (!inherits(design, "Design") ||
      !inherits(design$data, "data.table") ||
      !identical(dim(design$data), c(rows, length(expected_ids))) ||
      !identical(names(design$data), expected_ids)) {
    fail("random design for ", fixture, " has unexpected structure")
  }
  transposed <- design$transpose()
  if (!is.list(transposed) || length(transposed) != rows ||
      any(!vapply(transposed, is.list, logical(1L)))) {
    fail("transposed random design for ", fixture, " is malformed")
  }

  probabilities <- matrix(
    seq(0.01, 0.99, length.out = rows * length(expected_ids)),
    nrow = rows,
    dimnames = list(NULL, expected_ids)
  )
  quantiles <- parameter_set$qunif(as.data.frame(probabilities))
  if (!inherits(quantiles, "data.table") ||
      !identical(dim(quantiles), c(rows, length(expected_ids))) ||
      !identical(names(quantiles), expected_ids)) {
    fail("quantile transform for ", fixture, " is invalid")
  }

  roundtrip <- unserialize(serialize(parameter_set, NULL, version = 3L))
  if (!inherits(roundtrip, "ParamSet") ||
      !identical(roundtrip$ids(), expected_ids) ||
      !identical(roundtrip$params, parameter_set$params)) {
    fail("serialization round-trip for ", fixture, " changed the space")
  }
  subset_ids <- expected_ids[seq_len(min(6L, length(expected_ids)))]
  subset <- roundtrip$subset(subset_ids)
  subset_design <- generate_design_random(subset, min(rows, 16L))
  if (!inherits(subset, "ParamSet") ||
      !identical(subset$ids(), subset_ids) ||
      !identical(names(subset_design$data), subset_ids) ||
      length(parameter_set$values) != 0L) {
    fail("subset operation for ", fixture, " changed the serialized space")
  }

  data.frame(
    fixture = fixture,
    parameters = length(expected_ids),
    rows = rows,
    serialized_sha256 = unname(tools::sha256sum(path)),
    stringsAsFactors = FALSE
  )
})

utils::write.table(
  do.call(rbind, results), stdout(), sep = "\t", quote = FALSE,
  row.names = FALSE
)
