#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "usage: run-runtime-matrix-tests.R SNAPSHOT CANDIDATE_LIBRARY",
    call. = FALSE
  )
}

snapshot <- normalizePath(args[[1L]], mustWork = TRUE)
candidate_library <- normalizePath(args[[2L]], mustWork = TRUE)
.libPaths(unique(c(candidate_library, .libPaths())))
installed <- normalizePath(find.package("paradox", lib.loc = candidate_library),
  mustWork = TRUE)
if (!startsWith(installed, paste0(candidate_library, .Platform$file.sep))) {
  stop("paradox did not resolve from the run-specific library", call. = FALSE)
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("the exact runtime lock does not provide testthat", call. = FALSE)
}

Sys.setenv(NOT_CRAN = "false")
options(
  warn = 1L,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)
library("paradox", character.only = TRUE, lib.loc = candidate_library)

cat("runtime=", as.character(getRversion()), "\n", sep = "")
cat("installed_path=", installed, "\n", sep = "")
cat("not_cran=", Sys.getenv("NOT_CRAN"), "\n", sep = "")
cat("test_scope=full-practical-source-suite\n")

results <- testthat::test_dir(
  file.path(snapshot, "tests", "testthat"),
  reporter = "summary",
  load_helpers = TRUE,
  stop_on_failure = TRUE,
  stop_on_warning = TRUE,
  package = "paradox",
  load_package = "none"
)
summary <- as.data.frame(results)
failed <- sum(summary$failed)
warnings <- sum(summary$warning)
errors <- sum(summary$error)
passed <- sum(summary$passed)
skipped <- sum(summary$skipped)
cat("expectations_passed=", passed, "\n", sep = "")
cat("expectations_failed=", failed, "\n", sep = "")
cat("warnings=", warnings, "\n", sep = "")
cat("errors=", errors, "\n", sep = "")
cat("test_blocks_skipped=", skipped, "\n", sep = "")
if (failed != 0L || warnings != 0L || errors != 0L || passed < 1000L) {
  stop("runtime-matrix source suite did not meet its clean minimum", call. = FALSE)
}
cat("runtime_matrix_source_tests=passed\n")
