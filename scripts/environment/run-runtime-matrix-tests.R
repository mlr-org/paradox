#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    paste(
      "usage: run-runtime-matrix-tests.R",
      "SNAPSHOT CANDIDATE_LIBRARY SCOPE_DIRECTORY"
    ),
    call. = FALSE
  )
}

is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) & nzchar(target)
}

byte_sort <- function(value) {
  value[order(value, method = "radix")]
}

snapshot <- normalizePath(args[[1L]], mustWork = TRUE)
candidate_library <- normalizePath(args[[2L]], mustWork = TRUE)
scope_parent <- normalizePath(dirname(args[[3L]]), mustWork = TRUE)
scope_directory <- file.path(scope_parent, basename(args[[3L]]))
if (file.exists(scope_directory) || is_symbolic(scope_directory)) {
  stop("test-scope directory already exists or is symbolic", call. = FALSE)
}
.libPaths(unique(c(candidate_library, .libPaths())))
installed <- normalizePath(find.package("paradox", lib.loc = candidate_library),
  mustWork = TRUE)
if (!startsWith(installed, paste0(candidate_library, .Platform$file.sep))) {
  stop("paradox did not resolve from the run-specific library", call. = FALSE)
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("the exact runtime lock does not provide testthat", call. = FALSE)
}
if (!requireNamespace("rlang", quietly = TRUE)) {
  stop("the exact runtime lock does not provide rlang", call. = FALSE)
}
mbo_config_root <- Sys.getenv("PARADOX_MBO_CONFIG_ROOT", unset = "")
if (!nzchar(mbo_config_root)) {
  stop("PARADOX_MBO_CONFIG_ROOT is required by the complete runtime suite",
    call. = FALSE)
}
mbo_config_root <- normalizePath(mbo_config_root, mustWork = TRUE)
mbo_config_files <- file.path(
  mbo_config_root,
  c("mixed_search_space.rds", "numeric_search_space.rds")
)
if (any(!file.exists(mbo_config_files)) || any(dir.exists(mbo_config_files)) ||
    any(is_symbolic(mbo_config_files))) {
  stop("retained mbo_config upgrade fixtures are absent or symbolic",
    call. = FALSE)
}
skip_policy_helper <- file.path(
  snapshot, "scripts", "environment", "runtime-matrix-skip-policy.R"
)
if (!file.exists(skip_policy_helper) || dir.exists(skip_policy_helper) ||
    is_symbolic(skip_policy_helper)) {
  stop("runtime skip-policy validator is absent or symbolic", call. = FALSE)
}
source(skip_policy_helper, local = TRUE)
result_audit_helper <- file.path(
  snapshot, "scripts", "environment", "runtime-matrix-testthat-audit.R"
)
if (!file.exists(result_audit_helper) || dir.exists(result_audit_helper) ||
    is_symbolic(result_audit_helper)) {
  stop("runtime result auditor is absent or symbolic", call. = FALSE)
}
source(result_audit_helper, local = TRUE)

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
cat("mbo_config_fixture_count=", length(mbo_config_files), "\n", sep = "")
cat("mbo_config_fixture_source=retained-reviewed-git-objects\n")

test_directory <- file.path(snapshot, "tests", "testthat")
test_files <- dir(
  test_directory,
  pattern = "^test.*\\.[rR]$",
  full.names = FALSE
)
test_paths <- file.path(test_directory, test_files)
test_contexts <- sub(
  "[.][Rr]$", "", sub("^test[-_]", "", test_files)
)
valid_test <- file.exists(test_paths) & !dir.exists(test_paths) &
  !is_symbolic(test_paths)
if (length(test_files) == 0L || !all(valid_test) ||
    anyDuplicated(test_files) || anyDuplicated(test_contexts)) {
  stop("testthat source discovery is empty, symbolic, or ambiguous", call. = FALSE)
}

whole_skip_relative <- file.path(
  "environment", "runtime-matrix-whole-file-skips.tsv"
)
whole_skip_path <- file.path(snapshot, whole_skip_relative)
if (!file.exists(whole_skip_path) || dir.exists(whole_skip_path) ||
    is_symbolic(whole_skip_path)) {
  stop("whole-file skip manifest is absent or symbolic", call. = FALSE)
}
whole_skips <- read.delim(
  whole_skip_path,
  header = TRUE,
  quote = "",
  comment.char = "",
  colClasses = "character",
  check.names = FALSE
)
if (!identical(
      names(whole_skips),
      c("file", "context", "missing_package", "leading_guards", "reason")
    ) || nrow(whole_skips) != 2L || anyNA(whole_skips) ||
    any(!nzchar(whole_skips$reason)) || anyDuplicated(whole_skips$file) ||
    !identical(whole_skips$file, byte_sort(whole_skips$file)) ||
    any(!grepl("^test[-_][A-Za-z0-9_-]+[.]R$", whole_skips$file)) ||
    any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", whole_skips$missing_package)) ||
    any(!grepl("^[A-Za-z0-9_:.,-]+$", whole_skips$leading_guards))) {
  stop("whole-file skip manifest is malformed", call. = FALSE)
}
whole_skip_positions <- match(whole_skips$file, test_files)
if (anyNA(whole_skip_positions) || !identical(
    test_contexts[whole_skip_positions], whole_skips$context
  )) {
  stop("whole-file skip manifest contains an unknown or stale test", call. = FALSE)
}
guard_label <- function(expression) {
  if (!is.call(expression) || !is.name(expression[[1L]])) {
    return(NA_character_)
  }
  name <- as.character(expression[[1L]])
  if (identical(name, "skip_on_cran") && length(expression) == 1L) {
    return("skip_on_cran")
  }
  if (identical(name, "skip_if_not_installed") &&
      length(expression) == 2L && is.character(expression[[2L]]) &&
      length(expression[[2L]]) == 1L && !is.na(expression[[2L]]) &&
      grepl("^[A-Za-z][A-Za-z0-9.]*$", expression[[2L]])) {
    return(paste0("skip_if_not_installed:", expression[[2L]]))
  }
  NA_character_
}

observed_guard_sequences <- character(nrow(whole_skips))
for (i in seq_len(nrow(whole_skips))) {
  expressions <- parse(test_paths[[whole_skip_positions[[i]]]])
  first_test <- which(vapply(
    expressions,
    function(expression) {
      is.call(expression) && is.name(expression[[1L]]) &&
        identical(as.character(expression[[1L]]), "test_that")
    },
    logical(1L)
  ))
  if (length(first_test) == 0L || first_test[[1L]] <= 1L) {
    stop("whole-file skip test has no guarded test_that block", call. = FALSE)
  }
  leading <- expressions[seq_len(first_test[[1L]] - 1L)]
  observed <- vapply(leading, guard_label, character(1L))
  if (anyNA(observed)) {
    stop(
      "whole-file skip test has an unreviewed expression before test_that",
      call. = FALSE
    )
  }
  observed_guard_sequences[[i]] <- paste(observed, collapse = ",")
  if (!identical(
      observed_guard_sequences[[i]], whole_skips$leading_guards[[i]]
    )) {
    stop("whole-file skip leading guards differ from policy", call. = FALSE)
  }
  package_guards <- observed[startsWith(observed, "skip_if_not_installed:")]
  guarded_packages <- sub(
    "^skip_if_not_installed:", "", package_guards
  )
  missing_position <- which(
    guarded_packages == whole_skips$missing_package[[i]]
  )
  if (length(missing_position) != 1L) {
    stop("whole-file skip policy does not name its exact package guard",
      call. = FALSE)
  }
  availability <- vapply(
    guarded_packages,
    requireNamespace,
    logical(1L),
    quietly = TRUE
  )
  if ((missing_position[[1L]] > 1L &&
      !all(availability[seq_len(missing_position[[1L]] - 1L)])) ||
      availability[[missing_position[[1L]]]]) {
    stop(
      "whole-file skip package availability does not reach the reviewed guard",
      call. = FALSE
    )
  }
}

result_skip_relative <- file.path(
  "environment", "runtime-matrix-result-skips.tsv"
)
result_skip_policy <- runtime_matrix_validate_result_skip_policy(
  snapshot, test_files = test_files, test_paths = test_paths
)
runtime_version <- as.character(getRversion())
expected_result_skips <- result_skip_policy[
  result_skip_policy$runtime == runtime_version,
  ,
  drop = FALSE
]
row.names(expected_result_skips) <- NULL

exclusions <- data.frame(context = character(), reason = character())
if (getRversion() < "4.6.0") {
  manifest_relative <- file.path(
    "environment", "runtime-matrix-pre46-exclusions.tsv"
  )
  exclusions <- runtime_matrix_validate_pre46_exclusion_policy(
    snapshot, test_files = test_files, test_contexts = test_contexts
  )
  cat("test_scope=complete-source-suite\n")
  cat("pre46_exclusion_manifest=", manifest_relative, "\n", sep = "")
  cat("pre46_excluded_context_count=", nrow(exclusions), "\n", sep = "")
  for (i in seq_len(nrow(exclusions))) {
    cat(
      "pre46_excluded_context=", exclusions$context[[i]],
      "\treason=", exclusions$reason[[i]], "\n", sep = ""
    )
  }
} else {
  cat("test_scope=complete-source-suite\n")
}

excluded <- test_contexts %in% exclusions$context
executed_files <- test_files[!excluded]
executed_contexts <- test_contexts[!excluded]
if (!all(whole_skips$file %in% executed_files)) {
  stop("whole-file skip manifest overlaps an excluded test", call. = FALSE)
}
scope <- data.frame(
  state = ifelse(excluded, "excluded", "executed"),
  file = file.path("tests", "testthat", test_files),
  context = test_contexts,
  reason = "",
  stringsAsFactors = FALSE
)
scope$reason[excluded] <- exclusions$reason[
  match(test_contexts[excluded], exclusions$context)
]
source_entries <- dir(
  test_directory,
  all.files = TRUE,
  no.. = TRUE,
  full.names = FALSE
)
support_files <- setdiff(source_entries, test_files)
support_paths <- file.path(test_directory, support_files)
valid_support <- file.exists(support_paths) & !dir.exists(support_paths) &
  !is_symbolic(support_paths)
if (!all(valid_support) || anyDuplicated(support_files)) {
  stop("testthat support inputs are not regular unambiguous files", call. = FALSE)
}
if (!dir.create(scope_directory, mode = "0700") ||
    is_symbolic(scope_directory)) {
  stop("unable to create an owned test-scope directory", call. = FALSE)
}
scope_output <- file.path(scope_directory, "scope.tsv")
staged_directory <- file.path(scope_directory, "testthat")
if (!dir.create(staged_directory, mode = "0700")) {
  stop("unable to create the staged testthat directory", call. = FALSE)
}
copy_files <- c(support_files, executed_files)
copied <- file.copy(
  file.path(test_directory, copy_files),
  staged_directory,
  copy.mode = TRUE,
  copy.date = FALSE
)
if (length(copied) != length(copy_files) || !all(copied)) {
  stop("unable to stage the exact supported test files", call. = FALSE)
}
staged_tests <- dir(
  staged_directory,
  pattern = "^test.*\\.[rR]$",
  full.names = FALSE
)
if (!identical(byte_sort(staged_tests), byte_sort(executed_files)) ||
    any(is_symbolic(file.path(staged_directory, copy_files)))) {
  stop("staged test inventory differs from the retained scope", call. = FALSE)
}
write.table(
  scope,
  file = scope_output,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)
cat("test_scope_manifest=", file.path(basename(scope_directory), "scope.tsv"),
  "\n", sep = "")
cat("discovered_test_file_count=", length(test_files), "\n", sep = "")
cat("discovered_test_context_count=", length(test_contexts), "\n", sep = "")
cat("executed_test_file_count=", length(executed_files), "\n", sep = "")
cat("executed_test_context_count=", length(executed_contexts), "\n", sep = "")
cat("excluded_test_file_count=", sum(excluded), "\n", sep = "")
cat("excluded_test_context_count=", length(unique(test_contexts[excluded])),
  "\n", sep = "")
cat("staged_test_support_file_count=", length(support_files), "\n", sep = "")
cat("whole_file_skip_manifest=", whole_skip_relative, "\n", sep = "")
cat("expected_whole_file_skip_count=", nrow(whole_skips), "\n", sep = "")
for (i in seq_len(nrow(whole_skips))) {
  cat(
    "expected_whole_file_skip=", whole_skips$file[[i]],
    "\tmissing_package=", whole_skips$missing_package[[i]],
    "\tleading_guards=", whole_skips$leading_guards[[i]],
    "\treason=", whole_skips$reason[[i]], "\n", sep = ""
  )
  cat(
    "observed_whole_file_skip_guard_sequence=", whole_skips$file[[i]],
    "\tleading_guards=", observed_guard_sequences[[i]], "\n", sep = ""
  )
}
cat("result_skip_manifest=", result_skip_relative, "\n", sep = "")
cat("expected_result_test_block_skip_count=", nrow(expected_result_skips),
  "\n", sep = "")

# Parallel testthat is a per-stage capability, not a default: it needs a
# testthat whose TESTTHAT_PARALLEL environment override exists (3.2+) and
# callr in this stage's sealed library, so the old runtimes keep the exact
# serial path automatically.  The worker count is pinned through
# options(Ncpus) as well as the environment because the staged Rprofile's
# Ncpus default would otherwise widen the pool to eight and multiply the
# four concurrent stages far beyond the admitted envelope.  Parallel workers
# resolve the loaded package namespace themselves, so load_package stays
# "none" in both modes.
testthat_parallel <- getRversion() >= "4.1.0" &&
  tryCatch(
    utils::packageVersion("testthat") >= "3.2.0",
    error = function(...) FALSE
  ) &&
  requireNamespace("callr", quietly = TRUE)
testthat_workers <- if (testthat_parallel) 2L else 1L
if (testthat_parallel) {
  Sys.setenv(TESTTHAT_PARALLEL = "TRUE", TESTTHAT_CPUS = "2")
  options(Ncpus = testthat_workers)
} else {
  Sys.setenv(TESTTHAT_PARALLEL = "FALSE", TESTTHAT_CPUS = "1")
}
cat("testthat_parallel=", if (testthat_parallel) "true" else "false",
  "\n", sep = "")
cat("testthat_workers=", testthat_workers, "\n", sep = "")

results <- testthat::test_dir(
  staged_directory,
  reporter = "summary",
  load_helpers = TRUE,
  # Complete the exact retained inventory before enforcing the clean-suite
  # policy below.  A costly old-R run should report every ordinary failure and
  # warning in one batch instead of revealing them one invocation at a time.
  stop_on_failure = FALSE,
  stop_on_warning = FALSE,
  package = "paradox",
  load_package = "none"
)
summary <- as.data.frame(results)
result_audit <- runtime_matrix_audit_testthat_results(results, summary)
raw_results <- result_audit$raw_results
expectation_types <- result_audit$expectation_types
reported_files <- unique(as.character(summary$file))
if (anyNA(reported_files) || any(!nzchar(reported_files)) ||
    !identical(reported_files, basename(reported_files))) {
  stop("pinned testthat did not report unambiguous file basenames", call. = FALSE)
}
actual_files <- byte_sort(reported_files)
write.table(
  data.frame(file = actual_files),
  file = file.path(scope_directory, "testthat-reported-files.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)
cat("testthat_reported_file_form=basename\n")
cat("testthat_reported_file_count=", length(actual_files), "\n", sep = "")
missing_result_files <- byte_sort(setdiff(executed_files, actual_files))
unexpected_result_files <- byte_sort(setdiff(actual_files, executed_files))
if (!identical(missing_result_files, byte_sort(whole_skips$file)) ||
    length(unexpected_result_files) != 0L) {
  stop(
    paste0(
      "testthat did not report the exact retained test-file scope; missing=",
      paste(missing_result_files, collapse = ","),
      "; unexpected=",
      paste(unexpected_result_files, collapse = ",")
    ),
    call. = FALSE
  )
}
observed_whole_skips <- whole_skips
observed_whole_skips$leading_guards <- observed_guard_sequences
write.table(
  observed_whole_skips,
  file = file.path(scope_directory, "observed-whole-file-skips.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)
cat("observed_whole_file_skip_count=", length(missing_result_files),
  "\n", sep = "")

skipped_rows <- which(vapply(
  raw_results,
  function(block) {
    any(vapply(block, inherits, logical(1L), what = "expectation_skip"))
  },
  logical(1L)
))
observed_result_skips <- lapply(skipped_rows, function(i) {
  skipped_expectations <- raw_results[[i]][vapply(
    raw_results[[i]],
    inherits,
    logical(1L),
    what = "expectation_skip"
  )]
  if (length(skipped_expectations) != 1L) {
    stop("a skipped test block does not contain one exact skip", call. = FALSE)
  }
  data.frame(
    runtime = runtime_version,
    file = as.character(summary$file[[i]]),
    test = as.character(summary$test[[i]]),
    reason = conditionMessage(skipped_expectations[[1L]]),
    stringsAsFactors = FALSE
  )
})
observed_result_skips <- do.call(rbind, observed_result_skips)
if (is.null(observed_result_skips)) {
  observed_result_skips <- result_skip_policy[FALSE, , drop = FALSE]
}
observed_result_skips <- observed_result_skips[do.call(order, c(
  observed_result_skips[c("runtime", "file", "test", "reason")],
  list(method = "radix")
)), , drop = FALSE]
row.names(observed_result_skips) <- NULL
write.table(
  observed_result_skips,
  file = file.path(scope_directory, "observed-result-skips.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)
if (!identical(observed_result_skips, expected_result_skips)) {
  stop("observed result skips differ from the reviewed runtime policy",
    call. = FALSE)
}
for (i in seq_len(nrow(observed_result_skips))) {
  cat(
    "observed_result_skip=", observed_result_skips$file[[i]],
    "\ttest=", observed_result_skips$test[[i]],
    "\treason=", observed_result_skips$reason[[i]], "\n", sep = ""
  )
}
failed <- sum(expectation_types == "failure")
warnings <- sum(expectation_types == "warning")
errors <- sum(expectation_types == "error")
passed <- sum(expectation_types == "success")
skipped <- nrow(observed_result_skips)
cat("expectations_passed=", passed, "\n", sep = "")
cat("expectations_failed=", failed, "\n", sep = "")
cat("warnings=", warnings, "\n", sep = "")
cat("errors=", errors, "\n", sep = "")
cat("result_test_blocks_skipped=", skipped, "\n", sep = "")
cat("total_test_skips=", skipped + length(missing_result_files), "\n", sep = "")
if (failed != 0L || warnings != 0L || errors != 0L || passed == 0L) {
  stop("runtime-matrix source suite was not clean and nonempty", call. = FALSE)
}
cat("runtime_matrix_source_tests=passed\n")
