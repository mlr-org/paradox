#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = TRUE)
quiet <- FALSE
help <- FALSE
positionals <- character()
for (argument in arguments) {
  if (identical(argument, "--quiet")) {
    quiet <- TRUE
  } else if (argument %in% c("-h", "--help")) {
    help <- TRUE
  } else if (startsWith(argument, "--")) {
    stop("unknown option: ", argument, call. = FALSE)
  } else {
    positionals <- c(positionals, argument)
  }
}
if (help) {
  cat("Usage: Rscript compat/verify-reverse-dependency-evidence.R STAGE [--quiet]\n")
  quit(save = "no", status = 0L)
}
if (length(positionals) != 1L) {
  stop("usage: verify-reverse-dependency-evidence.R STAGE [--quiet]",
    call. = FALSE)
}
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("could not identify verifier", call. = FALSE)
script <- normalizePath(sub("^--file=", "", script_argument),
  winslash = "/", mustWork = TRUE)
sys.source(file.path(dirname(script), "reverse-runner.R"), envir = environment())

stage <- rr_require_directory(positionals[[1L]], "reverse evidence")
metadata <- file.path(stage, "metadata")
run_table <- rr_read_tsv(file.path(metadata, "run.tsv"), c("field", "value"))
run <- rr_validate_reverse_run_metadata(run_table)
retained_runner <- rr_require_file(
  file.path(metadata, "reverse-runner.R"), "retained reverse runner library"
)
retained_worker <- rr_require_file(
  file.path(metadata, "reverse-install-worker.R"), "retained install worker"
)
retained_wave_worker <- rr_require_file(
  file.path(metadata, "reverse-wave-worker.R"), "retained reverse wave worker"
)
retained_worker_group <- rr_require_file(
  file.path(metadata, "reverse-worker-group"),
  "retained reverse worker group supervisor"
)
if (file.access(retained_worker_group, mode = 1L) != 0L) {
  rr_fail("retained reverse worker group supervisor is not executable")
}
retained_verifier <- rr_require_file(
  file.path(metadata, "verify-reverse-dependency-evidence.R"),
  "retained reverse evidence verifier"
)
retained_resource_helper <- rr_require_file(
  file.path(metadata, "resource-jobs"), "retained resource scheduler helper"
)
resource_report_path <- rr_require_file(
  file.path(metadata, "resource-jobs-initial.tsv"),
  "initial resource scheduler report"
)
if (!identical(rr_sha256(retained_runner), run[["runner_library_sha256"]]) ||
    !identical(rr_sha256(retained_worker), run[["install_worker_sha256"]]) ||
    !identical(rr_sha256(retained_wave_worker), run[["wave_worker_sha256"]]) ||
    !identical(rr_sha256(retained_worker_group),
      run[["worker_group_sha256"]]) ||
    !identical(rr_sha256(retained_verifier),
      run[["reverse_evidence_verifier_sha256"]]) ||
    !identical(rr_sha256(retained_resource_helper),
      run[["resource_jobs_helper_sha256"]]) ||
    !identical(rr_sha256(resource_report_path),
      run[["resource_jobs_initial_report_sha256"]])) {
  rr_fail("retained runner, worker, verifier, or scheduler differs from run metadata")
}
resource_report_table <- rr_read_tsv(resource_report_path, c("field", "value"))
resource_report <- rr_validate_resource_report(resource_report_table, "consumer")
if (!identical(as.character(resource_report$jobs),
    run[["scheduler_initial_ceiling"]])) {
  rr_fail("initial resource scheduler report disagrees with run metadata")
}

plan_columns <- c(
  "package", "source", "relation", "priority", "version", "repository",
  "archive", "archive_name", "checksum_type", "declared_checksum",
  "archive_sha256", "archive_metadata_sha256", "notes"
)
plan <- rr_read_tsv(file.path(stage, "plan.tsv"), plan_columns)
if (anyDuplicated(plan$package) ||
    any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", plan$package)) ||
    any(!grepl("^[0-9a-f]{64}$", plan$archive_sha256)) ||
    any(!grepl("^[0-9a-f]{64}$", plan$archive_metadata_sha256))) {
  rr_fail("retained reverse plan is malformed")
}
completion_table <- rr_read_tsv(
  file.path(metadata, "completion.tsv"), c("field", "value")
)
completion <- rr_validate_reverse_completion_metadata(completion_table, run)
full_ledger <- file.path(metadata, "protected-library-full-hash-passes.tsv")

if (identical(run[["plan_only"]], "TRUE")) {
  rr_verify_stage(stage)
  rr_validate_full_hash_ledger(full_ledger, character())
  accepted <- rr_validate_reverse_acceptance(
    file.path(metadata, "accepted.tsv"), plan$package,
    file.path(stage, "packages")
  )
  waves <- rr_validate_reverse_waves(
    file.path(metadata, "waves.tsv"), plan$package
  )
  invisible(rr_validate_composite_children(
    stage, plan$package, character(), allow_interrupted = FALSE
  ))
  if (!identical(completion[["status"]], "planned") ||
      !identical(completion[["packages"]], as.character(nrow(plan))) ||
      nrow(accepted) || nrow(waves)) {
    rr_fail("plan-only reverse evidence contains execution results")
  }
  if (!quiet) {
    cat("reverse_evidence=planned\n")
    cat("protected_library_full_hash_passes=0\n")
  }
  quit(save = "no", status = 0L)
}

rr_verify_composite_stage(stage)
expected_hashes <- c(
  run[["candidate_library_content_sha256"]],
  run[["dependency_library_content_sha256"]]
)
rr_validate_full_hash_ledger(
  full_ledger, c("stage_start", "stage_completion_postflight"), expected_hashes
)
results_path <- rr_require_file(file.path(stage, "results.tsv"), "results ledger")
results <- rr_read_tsv(results_path, rr_reverse_result_columns())
accepted <- rr_validate_reverse_acceptance(
  file.path(metadata, "accepted.tsv"), plan$package,
  file.path(stage, "packages"), require_complete = TRUE
)
invisible(rr_validate_composite_children(
  stage, plan$package, accepted$package
))
waves <- rr_validate_reverse_waves(
  file.path(metadata, "waves.tsv"), plan$package
)
wave_report_root <- rr_require_directory(
  file.path(metadata, "resource-jobs-waves"),
  "per-wave resource scheduler reports"
)
wave_report_files <- list.files(
  wave_report_root, all.files = TRUE, full.names = TRUE, no.. = TRUE
)
if (length(wave_report_files) &&
    (any(file.info(wave_report_files, extra_cols = FALSE)$isdir) ||
      any(vapply(wave_report_files, rr_is_symbolic, logical(1L))) ||
      any(!grepl("^attempt-[0-9]{6}\\.tsv$", basename(wave_report_files))))) {
  rr_fail("per-wave resource scheduler report inventory is unsafe")
}
invisible(rr_validate_reverse_wave_report_inventory(wave_report_files, waves))
for (report_file in wave_report_files) {
  invisible(rr_validate_resource_report(
    rr_read_tsv(report_file, c("field", "value")), "consumer"
  ))
}
pending_for_wave <- seq_len(nrow(plan))
for (index in seq_len(nrow(waves))) {
  expected_relative <- file.path(
    "metadata", "resource-jobs-waves",
    basename(waves$resource_report[[index]])
  )
  report_file <- file.path(stage, waves$resource_report[[index]])
  if (!identical(waves$resource_report[[index]], expected_relative) ||
      !identical(rr_sha256(report_file),
        waves$resource_report_sha256[[index]])) {
    rr_fail("reverse wave resource scheduler report changed")
  }
  report <- rr_validate_resource_report(
    rr_read_tsv(report_file, c("field", "value")), "consumer"
  )
  if (!identical(as.character(report$jobs),
      waves$automatic_ceiling[[index]])) {
    rr_fail("reverse wave automatic ceiling disagrees with its retained report")
  }
  pending_for_wave <- rr_validate_reverse_wave_admission(
    waves[index, , drop = FALSE], pending_for_wave,
    as.integer(run[["scheduler_initial_jobs"]]), report$jobs
  )
}
archive_postflight_table <- rr_read_tsv(
  file.path(metadata, "source-archive-postflight.tsv"), c("field", "value")
)
archive_postflight <- setNames(
  archive_postflight_table$value, archive_postflight_table$field
)
if (!identical(archive_postflight[["status"]], "passed") ||
    !identical(archive_postflight[["plan_sha256"]],
      rr_sha256(file.path(stage, "plan.tsv"))) ||
    !identical(archive_postflight[["packages"]], as.character(nrow(plan)))) {
  rr_fail("source-archive postflight receipt disagrees with the plan")
}
if (nrow(results) != nrow(plan) || !identical(results$package, plan$package) ||
    any(!results$status %in% c("passed", "failed")) ||
    any(!results$count_coverage %in% c("exact", "partial", "unavailable"))) {
  rr_fail("results are not one ordered accepted row per planned package")
}
protected_state_table <- rr_read_tsv(
  file.path(metadata, "protected-state.tsv"), c("field", "value")
)
protected_state <- setNames(protected_state_table$value, protected_state_table$field)
protected_combined <- protected_state[["protected_library_metadata_sha256"]]
if (!grepl("^[0-9a-f]{64}$", protected_combined) ||
    any(results$protected_metadata_before != protected_combined) ||
    any(results$protected_metadata_after != protected_combined) ||
    any(results$candidate_content_sha256 != run[["candidate_content_sha256"]]) ||
    any(results$candidate_library_content_sha256 != expected_hashes[[1L]]) ||
    any(results$dependency_library_content_sha256 != expected_hashes[[2L]])) {
  rr_fail("accepted rows disagree with protected stage identity")
}
if (!nrow(waves) ||
    any(as.integer(waves$worker_limit) >
      as.integer(run[["scheduler_initial_jobs"]])) ||
    any(waves$protected_metadata_before != protected_combined) ||
    any(waves$protected_metadata_after != protected_combined)) {
  rr_fail("reverse waves disagree with scheduler or protected stage identity")
}
accepted_result_indices <- match(accepted$package, results$package)
if (anyNA(accepted_result_indices) ||
    !identical(accepted$plan_index,
      as.character(match(accepted$package, plan$package))) ||
    !identical(accepted$status, results$status[accepted_result_indices]) ||
    !identical(accepted$classification,
      results$classification[accepted_result_indices])) {
  rr_fail("acceptance ledger disagrees with aggregate results")
}
invisible(rr_validate_reverse_acceptance_waves(accepted, waves))

canonical_count <- function(value) grepl("^(0|[1-9][0-9]*)$", value)
nested_controls <- rr_reverse_nested_controls()
locale_environment <- rr_consumer_locale_environment()
for (index in seq_len(nrow(results))) {
  result <- results[index, , drop = FALSE]
  package <- result$package[[1L]]
  row_stage <- file.path(stage, "packages", package)
  rr_verify_stage(row_stage)
  row <- rr_read_tsv(file.path(row_stage, "result.tsv"),
    rr_reverse_result_columns())
  row.names(row) <- NULL
  row.names(result) <- NULL
  if (!identical(row, result)) rr_fail("row result differs from aggregate: ", package)
  acceptance_row <- accepted[accepted$package == package, , drop = FALSE]
  wave_number <- as.integer(acceptance_row$wave[[1L]])
  wave <- waves[wave_number, , drop = FALSE]
  wave_indices <- as.integer(strsplit(
    wave$plan_indices[[1L]], ",", fixed = TRUE
  )[[1L]])
  plan_index <- match(package, plan$package)
  position <- match(plan_index, wave_indices)
  task_path <- file.path(row_stage, "metadata", "worker-task.tsv")
  task <- rr_read_reverse_worker_task(task_path)
  if (is.na(position) ||
      !identical(task$plan_index[[1L]], as.character(plan_index)) ||
      !identical(task$package[[1L]], package) ||
      !identical(task$wave[[1L]], as.character(wave_number)) ||
      !identical(task$position[[1L]], as.character(position)) ||
      !identical(task$worker_limit[[1L]], wave$worker_limit[[1L]]) ||
      !identical(task$automatic_ceiling[[1L]],
        wave$automatic_ceiling[[1L]]) ||
      !identical(task$operator_limit[[1L]], wave$operator_limit[[1L]]) ||
      !identical(task$resource_report[[1L]], wave$resource_report[[1L]]) ||
      !identical(task$resource_report_sha256[[1L]],
        wave$resource_report_sha256[[1L]]) ||
      !identical(task$protected_metadata_before[[1L]], protected_combined) ||
      !identical(task$worker_script_sha256[[1L]],
        run[["wave_worker_sha256"]]) ||
      !identical(task$worker_group_sha256[[1L]],
        run[["worker_group_sha256"]])) {
    rr_fail("durable worker task receipt disagrees with row: ", package)
  }
  worker_result_path <- file.path(
    row_stage, "metadata", "worker-result.rds"
  )
  worker_launch_path <- file.path(
    row_stage, "metadata", "worker-launch.tsv"
  )
  invisible(rr_read_worker_launch(
    worker_launch_path, position, task$task_sha256[[1L]]
  ))
  worker_completion_path <- file.path(
    row_stage, "metadata", "worker-completion.tsv"
  )
  worker_result <- rr_read_worker_result(
    worker_result_path, position, task$task_sha256[[1L]],
    worker_completion_path
  )
  worker_row <- worker_result$value
  if (!isTRUE(worker_result$ok) || !is.data.frame(worker_row) ||
      nrow(worker_row) != 1L) {
    rr_fail("accepted row lacks a successful durable worker result: ", package)
  }
  worker_row$protected_metadata_after[[1L]] <- protected_combined
  if (!identical(worker_row, row)) {
    rr_fail("durable worker payload disagrees with accepted row: ", package)
  }
  row_completion_table <- rr_read_tsv(
    file.path(row_stage, "metadata", "completion.tsv"), c("field", "value")
  )
  row_completion <- setNames(
    row_completion_table$value, row_completion_table$field
  )
  if (!identical(row_completion_table$field, c(
      "status", "classification", "plan_index", "wave",
      "worker_task_sha256", "worker_launch_sha256", "worker_result_sha256",
      "worker_completion_sha256", "finished_utc"
    )) || !identical(row_completion[["status"]], result$status[[1L]]) ||
      !identical(row_completion[["classification"]],
        result$classification[[1L]]) ||
      !identical(row_completion[["plan_index"]], as.character(plan_index)) ||
      !identical(row_completion[["wave"]], as.character(wave_number)) ||
      !identical(row_completion[["worker_task_sha256"]], rr_sha256(task_path)) ||
      !identical(row_completion[["worker_launch_sha256"]],
        rr_sha256(worker_launch_path)) ||
      !identical(row_completion[["worker_result_sha256"]],
        rr_sha256(worker_result_path)) ||
      !identical(row_completion[["worker_completion_sha256"]],
        rr_sha256(worker_completion_path)) ||
      !grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        row_completion[["finished_utc"]]
      )) {
    rr_fail("durable worker completion disagrees with row: ", package)
  }
  rr_require_file(
    file.path(row_stage, "worker-transport.log"),
    "retained external worker transport log"
  )
  command_path <- file.path(
    row_stage,
    if (identical(result$installed_content_sha256[[1L]], "-")) {
      "install-command.tsv"
    } else "command.tsv"
  )
  command <- rr_read_tsv(command_path, c("kind", "name", "value"))
  timeout_rows <- command$kind == "timeout_seconds" & command$name == "timeout"
  expected_command_timeout <- if (identical(
      result$installed_content_sha256[[1L]], "-")) {
    run[["install_timeout_seconds"]]
  } else run[["check_timeout_seconds"]]
  if (sum(timeout_rows) != 1L ||
      !identical(command$value[timeout_rows], expected_command_timeout) ||
      !identical(result$install_timeout_seconds[[1L]],
        run[["install_timeout_seconds"]]) ||
      !identical(result$check_timeout_seconds[[1L]],
        run[["check_timeout_seconds"]])) {
    rr_fail("row command/result timeout disagrees with run metadata: ", package)
  }
  command_environment <- command[command$kind == "environment", , drop = FALSE]
  command_values <- setNames(command_environment$value, command_environment$name)
  if (anyDuplicated(command_environment$name) ||
      any(!names(locale_environment) %in% names(command_values)) ||
      !identical(unname(command_values[names(locale_environment)]),
        unname(locale_environment))) {
    rr_fail("row did not use the deterministic consumer locale: ", package)
  }
  command_controls <- if (identical(
      result$installed_content_sha256[[1L]], "-")) {
    nested_controls
  } else {
    rr_reverse_check_nested_controls(package)
  }
  if (any(!names(command_controls) %in% names(command_values)) ||
      !identical(unname(command_values[names(command_controls)]),
        unname(command_controls))) {
    rr_fail("row did not use its exact bounded parallel controls: ", package)
  }
  total <- result$count_files_total[[1L]]
  parsed <- result$count_files_parsed[[1L]]
  if (!canonical_count(total) || !canonical_count(parsed)) {
    rr_fail("row count coverage is malformed: ", package)
  }
  total <- as.integer(total)
  parsed <- as.integer(parsed)
  counts <- unlist(result[c("test_fail", "test_warn", "test_skip", "test_pass")],
    use.names = FALSE)
  if (identical(result$count_coverage[[1L]], "unavailable")) {
    if (parsed != 0L || any(counts != "-")) {
      rr_fail("unavailable row invented test counts: ", package)
    }
  } else {
    if (any(!canonical_count(counts)) ||
        (identical(result$count_coverage[[1L]], "exact") &&
          (total == 0L || parsed != total)) ||
        (identical(result$count_coverage[[1L]], "partial") &&
          (parsed == 0L || parsed >= total))) {
      rr_fail("row count coverage label is inconsistent: ", package)
    }
  }
  if (!identical(result$installed_content_sha256[[1L]], "-")) {
    receipt_paths <- file.path(row_stage, "metadata", c(
      "cache_manifest.tsv", "cache_seal.tsv", "cache_inputs.tsv",
      "cache_completion.tsv"
    ))
    receipt_hashes <- unname(tools::sha256sum(receipt_paths))
    if (!identical(receipt_hashes[[1L]], result$cache_manifest_sha256[[1L]]) ||
        !identical(receipt_hashes[[2L]], result$cache_seal_sha256[[1L]])) {
      rr_fail("retained cache receipt hashes disagree with row: ", package)
    }
    inputs <- rr_read_tsv(receipt_paths[[3L]], c("field", "value"))
    input_values <- setNames(inputs$value, inputs$field)
    input_locale_names <- paste0(
      "environment.", names(locale_environment)
    )
    if (any(!input_locale_names %in% names(input_values)) ||
        !identical(unname(input_values[input_locale_names]),
          unname(locale_environment))) {
      rr_fail("install-cache key omitted the deterministic consumer locale: ",
        package)
    }
    input_control_names <- paste0("environment.", names(nested_controls))
    if (any(!input_control_names %in% names(input_values)) ||
        !identical(unname(input_values[input_control_names]),
          unname(nested_controls))) {
      rr_fail("install-cache key omitted nested parallel controls: ", package)
    }
    cache_completion <- rr_read_tsv(receipt_paths[[4L]], c("field", "value"))
    cache_completion <- setNames(cache_completion$value, cache_completion$field)
    cache_manifest <- rr_read_tsv(
      receipt_paths[[1L]], c("path", "size", "sha256")
    )
    cache_manifest_hashes <- setNames(cache_manifest$sha256, cache_manifest$path)
    retained_members <- c(
      "metadata/cache-inputs.tsv" = rr_sha256(receipt_paths[[3L]]),
      "metadata/completion.tsv" = rr_sha256(receipt_paths[[4L]]),
      "install.log" = rr_sha256(file.path(row_stage, "install.log"))
    )
    derived_installed_hash <- rr_installed_hash_from_manifest(
      cache_manifest, package
    )
    expected_seal <- paste0(
      "evidence_manifest_sha256=", rr_sha256(receipt_paths[[1L]])
    )
    if (!identical(rr_install_cache_key(inputs), result$cache_key[[1L]]) ||
        !identical(readLines(receipt_paths[[2L]], warn = FALSE), expected_seal) ||
        !identical(cache_completion[["cache_key"]], result$cache_key[[1L]]) ||
        !identical(cache_completion[["installed_content_sha256"]],
          result$installed_content_sha256[[1L]]) ||
        !identical(derived_installed_hash,
          result$installed_content_sha256[[1L]]) ||
        !identical(unname(cache_manifest_hashes[names(retained_members)]),
          unname(retained_members))) {
      rr_fail("retained cache semantics disagree with row: ", package)
    }
  }
}

if (!identical(completion[["packages_planned"]], as.character(nrow(plan))) ||
    !identical(completion[["packages_completed"]], as.character(nrow(results))) ||
    !identical(completion[["results_sha256"]], rr_sha256(results_path)) ||
    !identical(completion[["acceptance_ledger_sha256"]],
      rr_sha256(file.path(metadata, "accepted.tsv"))) ||
    !identical(completion[["wave_ledger_sha256"]],
      rr_sha256(file.path(metadata, "waves.tsv"))) ||
    !identical(completion[["waves_completed"]], as.character(nrow(waves))) ||
    !identical(completion[["protected_library_full_hash_passes"]], "2") ||
    !identical(completion[["status"]],
      if (all(results$status == "passed")) "passed" else "completed_with_failures")) {
  rr_fail("reverse completion metadata disagrees with accepted rows")
}
if (!quiet) {
  cat("reverse_evidence=", completion[["status"]], "\n", sep = "")
  cat("packages=", nrow(results), "\n", sep = "")
  cat("protected_library_full_hash_passes=2\n")
}
