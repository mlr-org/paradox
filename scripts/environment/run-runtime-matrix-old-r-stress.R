#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    paste(
      "usage: run-runtime-matrix-old-r-stress.R",
      "SNAPSHOT CANDIDATE_LIBRARY SCOPE_DIRECTORY"
    ),
    call. = FALSE
  )
}

is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

byte_sort <- function(value) {
  value[order(value, method = "radix")]
}

runner_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(runner_file) != 1L) {
  stop("could not identify the old-runtime stress runner", call. = FALSE)
}
runner_file <- normalizePath(
  sub("^--file=", "", runner_file[[1L]]),
  winslash = "/",
  mustWork = TRUE
)
harness_directory <- dirname(runner_file)
test_support_helper <- file.path(
  harness_directory, "runtime-matrix-test-support.R"
)
test_tree_receipt_helper <- file.path(
  harness_directory, "runtime-matrix-prefix-receipt"
)
if (!file.exists(test_support_helper) || dir.exists(test_support_helper) ||
    is_symbolic(test_support_helper)) {
  stop("runtime test-support helper is absent or symbolic", call. = FALSE)
}
sys.source(test_support_helper, envir = environment())

snapshot <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
candidate_library <- normalizePath(
  args[[2L]], winslash = "/", mustWork = TRUE
)
scope_parent <- normalizePath(
  dirname(args[[3L]]), winslash = "/", mustWork = TRUE
)
scope_directory <- file.path(scope_parent, basename(args[[3L]]))
if (file.exists(scope_directory) || is_symbolic(scope_directory)) {
  stop("old-runtime stress scope already exists or is symbolic",
    call. = FALSE)
}
runtime <- as.character(getRversion())
if (!runtime %in% c("3.6.3", "4.0.5")) {
  stop("old-runtime stress slice only supports R 3.6.3 and R 4.0.5",
    call. = FALSE)
}

.libPaths(unique(c(candidate_library, .libPaths())))
installed <- normalizePath(
  find.package("paradox", lib.loc = candidate_library),
  winslash = "/",
  mustWork = TRUE
)
if (!startsWith(installed, paste0(candidate_library, .Platform$file.sep))) {
  stop("paradox did not resolve from the run-specific library", call. = FALSE)
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("the exact runtime lock does not provide testthat", call. = FALSE)
}

policy_helper <- file.path(
  snapshot, "scripts", "environment",
  "runtime-matrix-old-r-stress-policy.R"
)
audit_helper <- file.path(
  snapshot, "scripts", "environment", "runtime-matrix-testthat-audit.R"
)
for (path in c(policy_helper, audit_helper)) {
  if (!file.exists(path) || dir.exists(path) || is_symbolic(path)) {
    stop("old-runtime stress helper is absent or symbolic", call. = FALSE)
  }
}
source(policy_helper, local = TRUE)
source(audit_helper, local = TRUE)
policy <- runtime_matrix_validate_old_r_stress_policy(snapshot)

Sys.setenv(NOT_CRAN = "true")
Sys.unsetenv("PARADOX_SKIP_CHARACTERIZATION_GCT")
# The process starts under LC_ALL=C.UTF-8, which establishes its deterministic
# UTF-8 locale. testthat/withr changes LC_* temporarily while formatting
# expectations; clear the overriding environment selector after startup.
# The stage launcher sets LANG=C so old testthat does not request thousands of
# no-op language changes before this point.
Sys.unsetenv("LC_ALL")
if (nzchar(Sys.getenv("LC_ALL", unset = ""))) {
  stop("could not clear LC_ALL for the old-runtime stress process",
    call. = FALSE)
}
options(
  warn = 1L,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)
library("paradox", character.only = TRUE, lib.loc = candidate_library)

test_directory <- file.path(snapshot, "tests", "testthat")
selected_files <- byte_sort(unique(policy$file))
selected_paths <- file.path(test_directory, selected_files)
if (any(!file.exists(selected_paths)) || any(dir.exists(selected_paths)) ||
    any(vapply(selected_paths, is_symbolic, logical(1L)))) {
  stop("old-runtime stress source inventory changed after policy validation",
    call. = FALSE)
}
all_test_files <- dir(
  test_directory,
  pattern = "^test.*\\.[rR]$",
  full.names = FALSE
)

runtime_matrix_test_tree_create_private_directory(scope_directory)
staged_directory <- file.path(scope_directory, "testthat")
runtime_matrix_test_tree_create_private_directory(staged_directory)
staging <- runtime_matrix_test_tree_stage(
  test_directory,
  staged_directory,
  selected_files
)
if (!identical(
    byte_sort(runtime_matrix_test_tree_discover(test_directory)$tests),
    byte_sort(all_test_files)
  )) {
  stop("shared test-support discovery differs from testthat discovery",
    call. = FALSE)
}
filter_file <- "helper_zz_runtime_old_r_stress_filter.R"
filter_path <- file.path(staged_directory, filter_file)
if (filter_file %in% c(staging$support_files, selected_files) ||
    file.exists(filter_path) ||
    is_symbolic(filter_path)) {
  stop("old-runtime stress title filter collides with source support",
    call. = FALSE)
}
writeLines(
  runtime_matrix_old_r_stress_filter_lines(policy),
  con = filter_path,
  useBytes = TRUE
)
if (!file.exists(filter_path) || dir.exists(filter_path) ||
    is_symbolic(filter_path)) {
  stop("could not stage the source-derived old-runtime title filter",
    call. = FALSE)
}
runtime_matrix_old_r_stress_filter_self_test(scope_directory)
staged_tests <- dir(
  staged_directory,
  pattern = "^test.*\\.[rR]$",
  full.names = FALSE
)
if (!identical(byte_sort(staged_tests), selected_files)) {
  stop("staged old-runtime stress files differ from policy", call. = FALSE)
}
test_tree_receipt <- file.path(
  scope_directory, "testthat-tree.manifest.tsv"
)
runtime_matrix_test_tree_receipt(
  test_tree_receipt_helper,
  "create",
  staged_directory,
  test_tree_receipt
)

selection_path <- file.path(scope_directory, "selection.tsv")
write.table(
  policy,
  file = selection_path,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)

cat("runtime=", runtime, "\n", sep = "")
cat("installed_path=", installed, "\n", sep = "")
cat("not_cran=", Sys.getenv("NOT_CRAN"), "\n", sep = "")
cat("lc_all=unset\n")
cat("stress_scope=old-runtime-source-derived-selection\n")
cat(
  "stress_manifest=environment/runtime-matrix-old-r-stress.tsv\n",
  sep = ""
)
cat("stress_selection=old-r-stress-scope/selection.tsv\n", sep = "")
cat("selected_file_count=", length(selected_files), "\n", sep = "")
cat("selected_target_count=", nrow(policy), "\n", sep = "")
cat("selection_filter=", filter_file, "\n", sep = "")
cat("selection_filter_self_test=passed\n")
cat(
  "staged_test_support_file_count=",
  length(staging$support_files) + 1L,
  "\n",
  sep = ""
)
cat("staged_test_tree_receipt=testthat-tree.manifest.tsv\n")
cat("staged_test_tree_receipt_created=passed\n")

results <- testthat::test_dir(
  staged_directory,
  reporter = "summary",
  load_helpers = TRUE,
  stop_on_failure = FALSE,
  stop_on_warning = FALSE,
  package = "paradox",
  load_package = "none"
)
runtime_matrix_test_tree_receipt(
  test_tree_receipt_helper,
  "verify",
  staged_directory,
  test_tree_receipt
)
cat("staged_test_tree_receipt_verified=passed\n")
summary <- as.data.frame(results)
audit <- runtime_matrix_audit_testthat_results(results, summary)
raw_results <- audit$raw_results
if (anyNA(summary$file) || anyNA(summary$test) ||
    any(!nzchar(as.character(summary$file))) ||
    any(!nzchar(as.character(summary$test))) ||
    any(grepl("[\r\n\t]", as.character(summary$file))) ||
    any(grepl("[\r\n\t]", as.character(summary$test)))) {
  stop("old-runtime stress result identities are malformed", call. = FALSE)
}
reported_files <- byte_sort(unique(as.character(summary$file)))
if (!identical(reported_files, selected_files)) {
  stop("old-runtime stress results do not cover the selected files",
    call. = FALSE)
}
write.table(
  data.frame(file = reported_files, stringsAsFactors = FALSE),
  file = file.path(scope_directory, "testthat-reported-files.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)

expectation_classes <- c(
  success = "expectation_success",
  skip = "expectation_skip",
  failure = "expectation_failure",
  warning = "expectation_warning",
  error = "expectation_error"
)
block_counts <- lapply(raw_results, function(block) {
  vapply(
    expectation_classes,
    function(class) sum(vapply(block, inherits, logical(1L), what = class)),
    integer(1L)
  )
})
block_counts <- do.call(rbind, block_counts)
if (is.null(block_counts)) {
  stop("old-runtime stress returned no test blocks", call. = FALSE)
}
target_key <- paste(policy$file, policy$test, sep = "\t")
result_key <- paste(as.character(summary$file), as.character(summary$test),
  sep = "\t")
if (anyDuplicated(result_key)) {
  stop("old-runtime stress returned ambiguous test blocks", call. = FALSE)
}
target_position <- match(result_key, target_key)
targeted <- !is.na(target_position)
if (any(!targeted)) {
  stop(
    paste(
      "old-runtime stress executed a block outside the exact source-derived",
      "selection"
    ),
    call. = FALSE
  )
}
mode <- rep("-", length(result_key))
coverage <- rep("-", length(result_key))
mode[targeted] <- policy$mode[target_position[targeted]]
coverage[targeted] <- policy$coverage[target_position[targeted]]
status <- ifelse(
  rowSums(block_counts[, c("failure", "warning", "error"), drop = FALSE]) > 0L,
  "failed",
  ifelse(block_counts[, "skip"] > 0L, "skipped", "passed")
)
ledger <- data.frame(
  file = as.character(summary$file),
  test = as.character(summary$test),
  targeted = ifelse(targeted, "true", "false"),
  mode = mode,
  coverage = coverage,
  success = as.integer(block_counts[, "success"]),
  skip = as.integer(block_counts[, "skip"]),
  failure = as.integer(block_counts[, "failure"]),
  warning = as.integer(block_counts[, "warning"]),
  error = as.integer(block_counts[, "error"]),
  status = status,
  stringsAsFactors = FALSE
)
ledger <- ledger[do.call(order, c(
  ledger[c("file", "test")], list(method = "radix")
)), , drop = FALSE]
row.names(ledger) <- NULL
write.table(
  ledger,
  file = file.path(scope_directory, "results.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)

matched_targets <- match(target_key, result_key)
if (anyNA(matched_targets)) {
  stop("old-runtime stress omitted a source-derived target", call. = FALSE)
}
target_rows <- ledger[paste(ledger$file, ledger$test, sep = "\t") %in%
  target_key, , drop = FALSE]
if (nrow(ledger) != nrow(policy) ||
    nrow(target_rows) != nrow(policy) ||
    any(target_rows$targeted != "true") ||
    any(target_rows$status != "passed") ||
    any(target_rows$success < 1L) ||
    any(target_rows$skip != 0L) ||
    any(target_rows$failure != 0L) ||
    any(target_rows$warning != 0L) ||
    any(target_rows$error != 0L)) {
  stop("one or more old-runtime stress targets did not pass", call. = FALSE)
}
if (any(ledger$failure != 0L) || any(ledger$warning != 0L) ||
    any(ledger$error != 0L) ||
    sum(audit$expectation_types == "success") < 1L) {
  stop("old-runtime stress slice was not clean and nonempty", call. = FALSE)
}

non_target_skip_count <- sum(
  ledger$targeted == "false" & ledger$status == "skipped"
)
if (non_target_skip_count != 0L) {
  stop("old-runtime stress retained a non-target result", call. = FALSE)
}
cat("testthat_reported_file_count=", length(reported_files), "\n", sep = "")
cat("test_block_count=", nrow(ledger), "\n", sep = "")
cat("target_pass_count=", nrow(target_rows), "\n", sep = "")
cat("non_target_skip_count=", non_target_skip_count, "\n", sep = "")
cat(
  "expectations_passed=",
  sum(audit$expectation_types == "success"),
  "\n",
  sep = ""
)
cat("expectations_failed=0\nwarnings=0\nerrors=0\n")
cat("runtime_matrix_old_r_stress=passed\n")
