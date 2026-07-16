#!/usr/bin/env Rscript

# Execute one isolated native-test task against an already installed Paradox
# candidate.  The coordinator, not testthat's parallel queue, launches this
# script: testthat 3.3.2 silently changes load_package = "none" to "source" in
# that queue, which would let pkgload compile and load the source snapshot.

fail <- function(...) stop(..., call. = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 6L) {
  fail(paste(
    "usage: run-native-test-worker.R SNAPSHOT INSTALLED_LIBRARY",
    "focused|full EXPECTED_DSO_SHA256 OUTPUT_RDS TEST_FILE [TEST_FILE ...]"
  ))
}

snapshot <- normalizePath(args[[1L]], mustWork = TRUE)
library_path <- normalizePath(args[[2L]], mustWork = TRUE)
selection <- match.arg(args[[3L]], c("focused", "full"))
expected_dso_sha256 <- args[[4L]]
output_path <- normalizePath(args[[5L]], winslash = "/", mustWork = FALSE)
files <- args[-seq_len(5L)]

is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) & nzchar(target)
}

if (!grepl("^[0-9a-f]{64}$", expected_dso_sha256)) {
  fail("expected DSO SHA-256 is malformed")
}
if (anyDuplicated(files) || any(!grepl("^test[^/\\\\]*\\.[rR]$", files))) {
  fail("worker test inventory contains an unsafe or duplicate file name")
}
test_directory <- file.path(snapshot, "tests", "testthat")
test_paths <- file.path(test_directory, files)
if (any(!file.exists(test_paths)) || any(file.info(test_paths)$isdir) ||
    any(is_symbolic(test_paths))) {
  fail("worker test inventory contains an absent, non-regular, or symbolic file")
}
output_parent <- dirname(output_path)
if (!dir.exists(output_parent) || is_symbolic(output_parent) ||
    file.exists(output_path) || is_symbolic(output_path)) {
  fail("worker output path is unsafe or already exists")
}

.libPaths(unique(c(library_path, .libPaths())))
installed <- normalizePath(
  find.package("paradox", lib.loc = library_path),
  mustWork = TRUE
)
if (!startsWith(installed, paste0(library_path, .Platform$file.sep))) {
  fail("paradox did not resolve from the isolated worker library")
}

candidate_dso <- function() {
  loaded <- getLoadedDLLs()[["paradox"]]
  if (is.null(loaded) || is.null(loaded[["path"]])) {
    fail("worker did not retain one loaded paradox native library")
  }
  path <- normalizePath(loaded[["path"]], mustWork = TRUE)
  installed_prefix <- paste0(installed, .Platform$file.sep)
  link_target <- Sys.readlink(path)
  if (!startsWith(path, installed_prefix) || dir.exists(path) ||
      (!is.na(link_target) && nzchar(link_target))) {
    fail("worker paradox DSO escaped the isolated installation")
  }
  sha256 <- unname(tools::sha256sum(path))
  if (!identical(sha256, expected_dso_sha256)) {
    fail("worker paradox DSO differs from the expected SHA-256")
  }
  list(path = path, sha256 = sha256)
}

library("paradox", character.only = TRUE, lib.loc = library_path)
dso_before <- candidate_dso()

if (!requireNamespace("testthat", quietly = TRUE)) {
  fail("testthat is not installed in a repository-local library")
}

Sys.setenv(NOT_CRAN = "true")
options(
  warn = 2L,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  mc.cores = 1L
)

cat("native_worker_selection=", selection, "\n", sep = "")
cat("native_worker_dso_sha256_before=", dso_before$sha256, "\n", sep = "")
results <- setNames(vector("list", length(files)), files)
for (index in seq_along(files)) {
  cat("native_worker_file=", files[[index]], "\n", sep = "")
  results[[index]] <- testthat::test_file(
    test_paths[[index]],
    reporter = "summary",
    package = "paradox",
    load_helpers = TRUE,
    stop_on_failure = FALSE,
    stop_on_warning = FALSE,
    load_package = "none"
  )
  candidate_dso()
}
dso_after <- candidate_dso()
cat("native_worker_dso_sha256_after=", dso_after$sha256, "\n", sep = "")

payload <- list(
  schema = 1L,
  selection = selection,
  files = files,
  dso_path = dso_before$path,
  dso_sha256_before = dso_before$sha256,
  dso_sha256_after = dso_after$sha256,
  results = results
)
temporary <- paste0(output_path, ".new.", Sys.getpid())
on.exit(unlink(temporary), add = TRUE)
saveRDS(payload, temporary, version = 3L, compress = FALSE)
if (!file.rename(temporary, output_path)) {
  fail("could not publish native-test worker result")
}
