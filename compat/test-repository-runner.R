#!/usr/bin/env Rscript

# Synthetic-only regression gate for the reusable repository row engine.  It
# deliberately uses three tiny local repositories and tiny fake libraries; it
# never fingerprints or runs a real consumer/candidate installation.

if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT", unset = ""),
    normalizePath(getwd(), winslash = "/", mustWork = TRUE))) {
  stop("run from an activated repository root", call. = FALSE)
}
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
sys.source(file.path(root, "compat", "repository-runner.R"),
  envir = environment(), keep.source = FALSE)
sys.source(file.path(root, "compat", "fingerprint.R"),
  envir = environment(), keep.source = FALSE)
sys.source(file.path(root, "compat", "repository-evidence.R"),
  envir = environment(), keep.source = FALSE)

repository_runner_selftest <- function() {
scratch_parent <- file.path(root, ".local", "tmp")
invisible(repository_runner_require_directory(scratch_parent,
  "synthetic temporary root"))
scratch <- tempfile("repository-runner-selftest-", tmpdir = scratch_parent)
if (!dir.create(scratch, recursive = FALSE, showWarnings = FALSE)) {
  stop("could not reserve synthetic self-test root", call. = FALSE)
}
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

git <- file.path(root, ".local", "toolchain", "bin", "git")
rscript <- file.path(root, ".local", "toolchain", "bin", "Rscript")
git_environment <- repository_runner_git_environment()
git_run <- function(repository, arguments) {
  result <- processx::run(git, arguments, wd = repository,
    env = git_environment, stdout = "|", stderr = "|", error_on_status = FALSE,
    cleanup_tree = TRUE)
  if (!identical(result$status, 0L)) {
    stop("synthetic Git command failed: ", result$stderr, call. = FALSE)
  }
  invisible(trimws(result$stdout))
}
write_file <- function(path, value) {
  if (file.exists(path) || dir.exists(path) || repository_runner_is_symbolic(path)) {
    stop("synthetic output already exists: ", path, call. = FALSE)
  }
  writeLines(value, path, useBytes = TRUE)
  invisible(path)
}
expect_error <- function(expression, pattern = NULL) {
  value <- tryCatch({
    force(expression)
    NULL
  }, error = identity)
  if (!inherits(value, "error") || (!is.null(pattern) &&
      !grepl(pattern, conditionMessage(value), fixed = TRUE))) {
    stop("expected synthetic failure", call. = FALSE)
  }
  invisible(value)
}

# Exercise the shared shell authenticator against a detached source while the
# real primary checkout contains the caller's unrelated worktree changes.
auth_commit <- git_run(root, c("rev-parse", "--verify", "HEAD^{commit}"))
auth_tree <- git_run(root, c("rev-parse", "--verify", "HEAD^{tree}"))
auth_ref <- paste0("refs/paradox-selftest/repository-runner-", Sys.getpid())
auth_source <- file.path(scratch, "detached-auth-source")
auth_cleaned <- FALSE
on.exit({
  if (!auth_cleaned) {
    processx::run(git, c("worktree", "remove", "--force", auth_source),
      wd = root, env = git_environment, stdout = NULL, stderr = NULL,
      error_on_status = FALSE)
    processx::run(git, c("update-ref", "-d", auth_ref), wd = root,
      env = git_environment, stdout = NULL, stderr = NULL,
      error_on_status = FALSE)
  }
}, add = TRUE)
git_run(root, c("update-ref", auth_ref, auth_commit))
git_run(root, c("worktree", "add", "--quiet", "--detach", auth_source,
  auth_commit))
auth_result <- processx::run(file.path(root, "compat",
  "authenticate-candidate-git"),
  c(root, auth_ref, auth_commit, auth_tree, auth_source),
  wd = root, stdout = "|", stderr = "|", error_on_status = FALSE,
  cleanup_tree = TRUE)
if (!identical(auth_result$status, 0L) || !identical(trimws(auth_result$stdout),
    "candidate_git_authentication=passed")) {
  stop("five-argument detached candidate authentication failed: ",
    auth_result$stderr, call. = FALSE)
}
git_run(root, c("worktree", "remove", "--force", auth_source))
git_run(root, c("update-ref", "-d", auth_ref))
auth_cleaned <- TRUE

candidate_primary <- file.path(scratch, "candidate-primary")
dir.create(candidate_primary)
git_run(candidate_primary, c("init", "--quiet"))
git_run(candidate_primary, c("config", "user.name", "Repository Runner"))
git_run(candidate_primary, c("config", "user.email", "runner@example.invalid"))
git_run(candidate_primary, c("remote", "add", "origin",
  "https://example.invalid/paradox.git"))
write_file(file.path(candidate_primary, "DESCRIPTION"), c(
  "Package: paradox", "Version: 2.0.0", "Title: Synthetic",
  "Description: Synthetic candidate.", "License: MIT"
))
dir.create(file.path(candidate_primary, "R"))
write_file(file.path(candidate_primary, "R", "candidate.R"), "candidate <- 1L")
git_run(candidate_primary, c("add", "--", "DESCRIPTION", "R/candidate.R"))
git_run(candidate_primary, c("commit", "--quiet", "-m", "candidate"))
candidate_commit <- git_run(candidate_primary,
  c("rev-parse", "--verify", "HEAD^{commit}"))
candidate_tree <- git_run(candidate_primary,
  c("rev-parse", "--verify", "HEAD^{tree}"))
candidate_ref <- "refs/paradox-release/synthetic"
git_run(candidate_primary, c("update-ref", candidate_ref, candidate_commit))
candidate_source <- file.path(scratch, "candidate-source")
git_run(candidate_primary, c("worktree", "add", "--quiet", "--detach",
  candidate_source, candidate_commit))
# A dirty primary must not invalidate the authenticated detached source.
write_file(file.path(candidate_primary, "unrelated-primary-dirt"), "dirty")

candidate_library <- file.path(scratch, "candidate-library")
candidate_package <- file.path(candidate_library, "paradox")
dependency_library <- file.path(scratch, "dependency-library")
extra_library <- file.path(scratch, "extra-library")
dir.create(candidate_library)
dir.create(candidate_package)
dir.create(dependency_library)
dir.create(extra_library)
write_file(file.path(candidate_package, "DESCRIPTION"), c(
  "Package: paradox", "Version: 2.0.0", "Title: Synthetic",
  "Description: Synthetic installed candidate.", "License: MIT"
))
write_file(file.path(candidate_package, "payload"), "candidate")
write_file(file.path(dependency_library, "dependency"), "dependency")
write_file(file.path(extra_library, "extra"), "extra")
candidate_content <- compat_tree_content_sha256(candidate_package)
dependency_content <- compat_tree_content_sha256(dependency_library)
write_file(file.path(candidate_library, ".paradox-candidate-content-sha256"),
  candidate_content)

consumer_root <- file.path(scratch, "consumers")
dir.create(consumer_root)
selection_rows <- lapply(seq_len(3L), function(index) {
  name <- paste0("consumer", index)
  checkout <- file.path(consumer_root, name)
  dir.create(checkout)
  git_run(checkout, c("init", "--quiet"))
  git_run(checkout, c("config", "user.name", "Repository Runner"))
  git_run(checkout, c("config", "user.email", "runner@example.invalid"))
  origin <- paste0("https://example.invalid/", name, ".git")
  git_run(checkout, c("remote", "add", "origin", origin))
  write_file(file.path(checkout, "DESCRIPTION"), c(
    paste0("Package: ", name), "Version: 1.0.0", "Title: Synthetic",
    "Description: No-test synthetic consumer.", "License: MIT"
  ))
  git_run(checkout, c("add", "--", "DESCRIPTION"))
  git_run(checkout, c("commit", "--quiet", "-m", name))
  commit <- git_run(checkout, c("rev-parse", "--verify", "HEAD^{commit}"))
  tree <- git_run(checkout, c("rev-parse", "--verify", "HEAD^{tree}"))
  data.frame(position = as.character(index), repository = name,
    priority = as.character(index - 1L), origin = origin, commit = commit,
    tree = tree, stringsAsFactors = FALSE)
})
selection_raw <- do.call(rbind, selection_rows)
selection <- repository_runner_selection(selection_raw)

stage_parent <- file.path(scratch, "stages")
dir.create(stage_parent)
resource_log <- file.path(scratch, "resource-invocations.log")
resource_helper <- file.path(scratch, "resource-jobs")
write_file(resource_helper, c(
  "#!/bin/sh",
  "set -eu",
  paste0("printf '%s\\n' \"$*\" >> '", resource_log, "'"),
  "test \"${1-}\" = consumer",
  "shift",
  "jobs=4",
  "operator=none",
  "while test $# -gt 0; do",
  "  case $1 in",
  "    --max-jobs) operator=$2; jobs=$2; shift 2 ;;",
  "    --report) shift ;;",
  "    *) exit 64 ;;",
  "  esac",
  "done",
  "printf '%s\\n' 'field\tvalue' 'schema\t1' 'profile\tconsumer' \\",
  "  'platform\tlinux' 'online_cpus\t64' 'affinity_cpus\t64' \\",
  "  'cgroup_cpu_limit\tunlimited' 'cpu_limit\t64' 'cpu_reserve\t2' \\",
  "  'cpu_per_job\t2' 'cpu_jobs\t31' 'memory_source\tcgroup' \\",
  "  'memory_available_mib\t65536' 'cgroup_memory_available_mib\t65536' \\",
  "  'memory_reserve_mib\t16384' 'memory_mib_per_job\t8192' \\",
  "  'memory_jobs\t6' 'profile_max_jobs\t4' \\",
  "  \"operator_max_jobs\t$operator\" \"jobs\t$jobs\""
))
Sys.chmod(resource_helper, "0755")
tool_files <- c(
  "repository-runner.R" = file.path(root, "compat", "repository-runner.R"),
  "repository-test-child.R" = file.path(root, "compat",
    "repository-test-child.R"),
  "repository-wave-worker.R" = file.path(root, "compat",
    "repository-wave-worker.R"),
  "fingerprint.R" = file.path(root, "compat", "fingerprint.R"),
  "repository-evidence.R" = file.path(root, "compat", "repository-evidence.R")
  , "resource-jobs" = resource_helper
)
config <- list(
  root = candidate_primary, stage = file.path(stage_parent, "stage"),
  run_id = "synthetic-three-row", consumer_root = consumer_root,
  timeout_seconds = 60, candidate_origin =
    "https://example.invalid/paradox.git", candidate_ref = candidate_ref,
  candidate_commit = candidate_commit, candidate_tree = candidate_tree,
  candidate_source = candidate_source, candidate_version = "2.0.0",
  candidate_library = candidate_library, candidate_package = candidate_package,
  candidate_content_sha256 = candidate_content,
  candidate_provenance_sha256 = paste(rep("1", 64L), collapse = ""),
  candidate_installer_archive_sha256 = paste(rep("2", 64L), collapse = ""),
  dependency_library = dependency_library,
  dependency_content_sha256 = dependency_content,
  extra_libraries = extra_library, git = git, rscript = rscript,
  tool_files = tool_files, base_environment = character()
)

fingerprint_calls <- new.env(parent = emptyenv())
fingerprint <- function(path) {
  key <- normalizePath(path, winslash = "/", mustWork = TRUE)
  old <- if (exists(key, fingerprint_calls, inherits = FALSE)) {
    get(key, fingerprint_calls, inherits = FALSE)
  } else 0L
  assign(key, old + 1L, fingerprint_calls)
  compat_tree_content_sha256(key)
}

context <- repository_runner_initialize(config, selection, fingerprint)
if (length(ls(fingerprint_calls)) != 4L ||
    any(vapply(ls(fingerprint_calls), function(key) get(key, fingerprint_calls),
      integer(1L)) != 1L)) {
  stop("initial boundary did not fingerprint every synthetic path exactly once",
    call. = FALSE)
}
tampered_resource <- context$initial_resource$lines
tampered_resource[grepl("^memory_jobs\\t", tampered_resource)] <-
  "memory_jobs\t5"
expect_error(
  repository_runner_parse_resource_report(tampered_resource),
  "violates consumer policy"
)
noncanonical_resource <- context$initial_resource$lines
noncanonical_resource[grepl("^jobs\\t", noncanonical_resource)] <- "jobs\t04"
expect_error(
  repository_runner_parse_resource_report(noncanonical_resource),
  "canonical positive integer"
)

row_two <- repository_runner_reserve_directory(file.path(context$stage, "rows"),
  repository_runner_row_name(selection[2L, , drop = FALSE]),
  "synthetic interrupted row")
partial <- repository_runner_reserve_directory(row_two, "attempt-000001",
  "synthetic interrupted attempt")
write_file(file.path(partial, "interrupted"), "partial")
interrupted_wave <- repository_runner_reserve_directory(file.path(context$stage,
  "scheduler", "waves"), "wave-000001", "synthetic interrupted wave")
write_file(file.path(interrupted_wave, "interrupted"), "partial")

expect_error(repository_runner_resource_decision(config, "5",
  retained_max = context$initial_resource$jobs), "may lower")
timing <- file.path(scratch, "wave-timing")
dir.create(timing)
timed_fixture <- list(delays = c(consumer1 = 0.25, consumer2 = 0.25,
  consumer3 = 0.25), fail = character(), timing_directory = timing)
lock <- repository_runner_parent_lock_acquire(context)
outcomes <- tryCatch(repository_runner_run_rows_bounded(context, selection,
  operator_max = "2", worker_fixture = timed_fixture), error = identity)
repository_runner_parent_lock_release(lock)
if (inherits(outcomes, "condition") || !identical(outcomes,
    rep("accepted", 3L))) {
  stop("bounded synthetic repository waves did not accept all rows", call. = FALSE)
}
read_time <- function(name) as.numeric(readLines(file.path(timing, name),
  warn = FALSE))
if (!(read_time("consumer1-start") < read_time("consumer2-end") &&
      read_time("consumer2-start") < read_time("consumer1-end"))) {
  stop("two synthetic repository workers did not overlap", call. = FALSE)
}
if (!dir.exists(file.path(row_two, "attempt-000001")) ||
    !dir.exists(file.path(row_two, "attempt-000002")) ||
    !length(list.files(file.path(context$stage, "scheduler", "quarantine"),
      pattern = "^wave-000001-interrupted-"))) {
  stop("interrupted row or wave did not resume append-only", call. = FALSE)
}
lock <- repository_runner_parent_lock_acquire(context)
resumed <- repository_runner_run_rows_bounded(context, selection,
  operator_max = "2")
repository_runner_parent_lock_release(lock)
if (!identical(resumed, rep("reused", 3L))) {
  stop("accepted bounded rows were not resumed", call. = FALSE)
}

# One infrastructure failure must not cancel or discard its successful
# siblings.  The complete wave is sealed, successful rows are accepted in
# selection order, and a resume executes only the failed row.
failure_config <- config
failure_config$stage <- file.path(stage_parent, "failure-stage")
failure_config$run_id <- "synthetic-worker-failure"
failure_context <- repository_runner_initialize(failure_config, selection,
  compat_tree_content_sha256)
failure_timing <- file.path(scratch, "failure-timing")
dir.create(failure_timing)
failure_fixture <- list(delays = c(consumer1 = 0.35, consumer2 = 0.15,
  consumer3 = 0.35), fail = "consumer2", timing_directory = failure_timing)
failure_lock <- repository_runner_parent_lock_acquire(failure_context)
failure <- tryCatch(repository_runner_run_rows_bounded(failure_context,
  selection, operator_max = "3", worker_fixture = failure_fixture), error = identity)
repository_runner_parent_lock_release(failure_lock)
if (!inherits(failure, "condition") ||
    !grepl("wave retained", conditionMessage(failure), fixed = TRUE)) {
  stop("synthetic worker failure did not retain a complete wave", call. = FALSE)
}
failure_wave <- repository_runner_verify_wave(failure_context, file.path(
  failure_context$stage, "scheduler", "waves", "wave-000001"))
if (!identical(failure_wave$result$state,
    c("ready", "worker_failed", "ready")) ||
    !file.exists(file.path(failure_context$stage, "rows",
      repository_runner_row_name(selection[1L, , drop = FALSE]), "accepted.tsv")) ||
    file.exists(file.path(failure_context$stage, "rows",
      repository_runner_row_name(selection[2L, , drop = FALSE]), "accepted.tsv")) ||
    !file.exists(file.path(failure_context$stage, "rows",
      repository_runner_row_name(selection[3L, , drop = FALSE]), "accepted.tsv"))) {
  stop("failed wave did not retain every sibling outcome", call. = FALSE)
}
failure_lock <- repository_runner_parent_lock_acquire(failure_context)
failure_resume <- repository_runner_run_rows_bounded(failure_context,
  selection, operator_max = "3")
repository_runner_parent_lock_release(failure_lock)
if (!identical(failure_resume, c("reused", "accepted", "reused"))) {
  stop("failed wave resume reran a successful sibling", call. = FALSE)
}

promotion_cases <- file.path(scratch, "promotion-cases")
dir.create(promotion_cases)
invalid_row <- file.path(promotion_cases, "invalid-promotion")
dir.create(invalid_row)
invalid_attempt <- file.path(invalid_row, "attempt-000001")
dir.create(invalid_attempt)
write_file(file.path(invalid_attempt, "artifact"), "invalid")
repository_runner_seal_directory(invalid_attempt)
expect_error(repository_runner_promote_attempt(context,
  selection[1L, , drop = FALSE], invalid_row, invalid_attempt,
  validator = function(...) stop("injected semantic rejection", call. = FALSE)),
  "failed semantic validation")
if (file.exists(file.path(invalid_row, "accepted.tsv")) ||
    !dir.exists(file.path(invalid_row, "quarantine", "attempt-000001"))) {
  stop("invalid promotion was accepted or not quarantined", call. = FALSE)
}

partial_row <- file.path(promotion_cases, "partial-promotion")
dir.create(partial_row)
write_file(file.path(partial_row, "accepted.tsv"), "partial")
expect_error(repository_runner_quarantine_partial_acceptance(partial_row),
  "partial acceptance was quarantined")
if (file.exists(file.path(partial_row, "accepted.tsv"))) {
  stop("partial acceptance remained live", call. = FALSE)
}

counts_parent <- file.path(scratch, "counts")
dir.create(counts_parent)
valid_counts <- file.path(counts_parent, "valid.tsv")
repository_runner_write_tsv(repository_runner_map_frame(c(
  schema = "1", availability = "complete_testthat", test_cases = "2",
  expectations = "3", passed = "2", failed = "0", skipped = "1",
  errors = "0", warnings = "0"
)), valid_counts)
invisible(repository_runner_validate_counts(valid_counts, "testthat", 0L,
  FALSE))
invalid_counts <- file.path(counts_parent, "invalid.tsv")
repository_runner_write_tsv(repository_runner_map_frame(c(
  schema = "1", availability = "complete_testthat", test_cases = "1",
  expectations = "2", passed = "1", failed = "0", skipped = "0",
  errors = "0", warnings = "0"
)), invalid_counts)
expect_error(repository_runner_validate_counts(invalid_counts, "testthat", 0L,
  FALSE), "inconsistent")

child_checkout <- file.path(scratch, "child-consumer")
dir.create(child_checkout)
write_file(file.path(child_checkout, "DESCRIPTION"), c(
  "Package: syntheticconsumer", "Version: 1.0.0", "Title: Synthetic",
  "Description: Synthetic failing child-count fixture.", "License: MIT",
  "Encoding: UTF-8", "Suggests: testthat", "Config/testthat/edition: 3"
))
write_file(file.path(child_checkout, "NAMESPACE"), character())
dir.create(file.path(child_checkout, "R"))
write_file(file.path(child_checkout, "R", "consumer.R"), "fixture <- 1L")
dir.create(file.path(child_checkout, "tests"))
dir.create(file.path(child_checkout, "tests", "testthat"))
write_file(file.path(child_checkout, "tests", "testthat", "test-counts.R"), c(
  "testthat::test_that(\"collect every reachable failure\", {",
  "  testthat::expect_true(FALSE)",
  "  testthat::expect_equal(1L, 2L)",
  "  testthat::expect_true(TRUE)",
  "  testthat::expect_identical(Sys.getenv(\"CMAKE_BUILD_PARALLEL_LEVEL\"), \"1\")",
  "  testthat::expect_identical(Sys.getenv(\"_R_CHECK_LIMIT_CORES_\"), \"true\")",
  "  testthat::expect_identical(Sys.getenv(\"MC_CORES\"), \"1\")",
  "  testthat::expect_identical(Sys.getenv(\"R_FUTURE_PLAN\"), \"sequential\")",
  "  testthat::expect_identical(Sys.getenv(\"R_FUTURE_FORK_ENABLE\"), \"false\")",
  "  testthat::expect_identical(Sys.getenv(\"R_PARALLELLY_FORK_ENABLE\"), \"false\")",
  "  testthat::expect_identical(Sys.getenv(\"R_PARALLELLY_AVAILABLECORES_FALLBACK\"), \"1\")",
  "  testthat::expect_identical(Sys.getenv(\"R_FUTURE_AVAILABLECORES_FALLBACK\"), \"1\")",
  "})"
))
development_library <- normalizePath(file.path(root, ".local", "R", "library"),
  winslash = "/", mustWork = TRUE)
child_context <- context
child_context$config$extra_libraries <- c(extra_library, development_library)
child_state <- file.path(scratch, "child-state")
dir.create(child_state)
child_environment <- repository_runner_process_environment(child_context,
  child_state)
nested_parallel <- repository_runner_nested_parallel_environment()
if (!identical(unname(child_environment[names(nested_parallel)]),
    unname(nested_parallel))) {
  stop("row child environment omitted a nested-parallelism control",
    call. = FALSE)
}
child_log <- file.path(scratch, "child.log")
child_counts_path <- file.path(scratch, "child-counts.tsv")
child_process <- repository_runner_run_child(child_context, child_checkout,
  "testthat", child_log, child_counts_path, child_state, child_environment)
child_counts <- repository_runner_validate_counts(child_counts_path, "testthat",
  child_process$status, child_process$timed_out)
if (!identical(child_process$status, 1L) ||
    !identical(child_counts[["availability"]], "complete_testthat") ||
    !identical(child_counts[["expectations"]], "11") ||
    !identical(child_counts[["failed"]], "2") ||
    !identical(child_counts[["passed"]], "9")) {
  stop("external child did not retain complete multi-failure counts",
    call. = FALSE)
}

receipt_attempt <- file.path(scratch, "environment-receipt-attempt")
dir.create(receipt_attempt)
dir.create(file.path(receipt_attempt, "work"))
receipt_state <- file.path(receipt_attempt, "work", "row-state")
dir.create(receipt_state)
receipt_environment <- repository_runner_process_environment(context,
  receipt_state)
receipt <- repository_runner_environment_receipt(receipt_environment)
invisible(repository_runner_validate_environment_receipt(receipt,
  receipt_attempt, context))
tampered_receipt <- receipt
tampered_receipt$value[tampered_receipt$name == "R_FUTURE_PLAN"] <- "multisession"
expect_error(repository_runner_validate_environment_receipt(tampered_receipt,
  receipt_attempt, context), "does not prove row isolation")

external_worker_state <- file.path(scratch, "external-worker-environment")
dir.create(external_worker_state)
external_worker_environment <- repository_runner_worker_environment(
  external_worker_state)
if (!identical(as.character(external_worker_environment[names(nested_parallel)]),
    unname(nested_parallel))) {
  stop("external worker environment omitted a nested-parallelism control",
    call. = FALSE)
}

isolation_root <- file.path(scratch, "isolation")
dir.create(isolation_root)
isolation_environment <- repository_runner_process_environment(context,
  isolation_root)
python <- file.path(root, ".local", "toolchain", "bin", "python")
if (file.exists(python)) {
  module_root <- file.path(scratch, "python-module")
  dir.create(module_root)
  write_file(file.path(module_root, "isolated_module.py"), "VALUE = 42")
  python_result <- processx::run(python,
    c("-c", "import isolated_module; assert isolated_module.VALUE == 42"),
    wd = module_root, env = isolation_environment, stdout = "|", stderr = "|",
    error_on_status = FALSE, cleanup_tree = TRUE)
  if (!identical(python_result$status, 0L) ||
      dir.exists(file.path(module_root, "__pycache__")) ||
      length(list.files(module_root, pattern = "[.]pyc$", recursive = TRUE))) {
    stop("Python bytecode cache escaped the row-local state", call. = FALSE)
  }
}

mutation_candidate_library <- file.path(scratch, "mutation-candidate-library")
mutation_candidate_package <- file.path(mutation_candidate_library, "paradox")
mutation_dependency <- file.path(scratch, "mutation-dependency")
dir.create(mutation_candidate_library)
dir.create(mutation_candidate_package)
dir.create(mutation_dependency)
write_file(file.path(mutation_candidate_package, "payload"), "candidate")
write_file(file.path(mutation_dependency, "payload"), "dependency")
mutation_config <- config
mutation_config$candidate_library <- mutation_candidate_library
mutation_config$candidate_package <- mutation_candidate_package
mutation_config$dependency_library <- mutation_dependency
mutation_config$extra_libraries <- character()
metadata_before <- repository_runner_protected_metadata(mutation_config)
write_file(file.path(mutation_dependency, "escaped.pyc"), "cache")
metadata_after <- repository_runner_protected_metadata(mutation_config)
if (identical(metadata_before, metadata_after)) {
  stop("metadata protection did not detect a synthetic .pyc mutation",
    call. = FALSE)
}

completion_lock <- repository_runner_parent_lock_acquire(context)
result <- repository_runner_complete(context, fingerprint,
  repository_seal_evidence, repository_verify_evidence)
repository_runner_parent_lock_release(completion_lock)
call_counts <- vapply(ls(fingerprint_calls), function(key) {
  get(key, fingerprint_calls)
}, integer(1L))
if (length(result$rows) != 3L || any(call_counts != 2L) ||
    !identical(result$completion[["protected_library_full_hash_boundaries"]],
      "2")) {
  stop("completion did not retain arbitrary rows with exactly two boundaries",
    call. = FALSE)
}
remaining_parallel_children <- get("children", envir =
  asNamespace("parallel"), inherits = FALSE)()
if (length(remaining_parallel_children)) {
  stop("bounded scheduler left supervised workers registered", call. = FALSE)
}

cat("repository_runner_selftest=passed\n",
  "selected_rows=3\n",
  "accepted_row_resume=passed\n",
  "interrupted_row_resume=passed\n",
  "protected_full_hash_calls=", sum(call_counts), "\n",
  "protected_paths=", length(call_counts), "\n",
  "full_hash_boundaries_per_path=2\n",
  "detached_candidate_authentication=passed\n",
  "cache_isolation=passed\n",
  "semantic_counts=passed\n",
  "child_multi_failure_counts=passed\n",
  "nested_parallel_controls=passed\n",
  "nested_parallel_receipt_binding=passed\n",
  "promotion_fail_closed=passed\n", sep = "")
}

repository_runner_selftest()
