#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)

usage <- function() {
  cat(paste0(
    "Usage: scripts/environment/verify-portability-ci-evidence.R EVIDENCE [OPTIONS]\n",
    "\n",
    "Verify retained GitHub Actions portability evidence without network access.\n",
    "\n",
    "Options:\n",
    "  --run-id ID              Expected run ID (inferred from EVIDENCE name).\n",
    "  --run-attempt N          Expected run attempt (default: 1).\n",
    "  --harness-commit SHA     Required workflow companion commit.\n",
    "  --harness-tag TAG        Required workflow companion tag.\n",
    "  --candidate-commit SHA   Required checked-out package commit.\n",
    "  --workflow-sha256 SHA    Required executed workflow file SHA-256.\n",
    "  -h, --help               Show this help.\n"
  ))
}

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  fail("could not identify portability evidence verifier")
}
script_path <- normalizePath(
  sub("^--file=", "", script_argument), winslash = "/", mustWork = TRUE
)
root <- normalizePath(
  file.path(dirname(script_path), "..", ".."),
  winslash = "/", mustWork = TRUE
)

require_plain_file <- function(path, label, nonempty = TRUE) {
  if (!file.exists(path) || dir.exists(path) || nzchar(Sys.readlink(path))) {
    fail(label, " is absent, non-regular, or symbolic: ", path)
  }
  info <- file.info(path, extra_cols = FALSE)
  if (nrow(info) != 1L || is.na(info$size) || (nonempty && info$size <= 0)) {
    fail(label, " has an invalid size: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

require_plain_directory <- function(path, label) {
  if (!dir.exists(path) || nzchar(Sys.readlink(path))) {
    fail(label, " is absent, non-directory, or symbolic: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

sha256_file <- function(path) {
  value <- unname(tools::sha256sum(path))
  if (length(value) != 1L || is.na(value) ||
      !grepl("^[0-9a-f]{64}$", value)) {
    fail("could not compute SHA-256 for retained file: ", path)
  }
  value
}

scalar_character <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    fail(label, " is not one JSON string")
  }
  value
}

scalar_logical <- function(value, label) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    fail(label, " is not one JSON logical")
  }
  value
}

integer_string <- function(value, label, allow_zero = FALSE) {
  if (is.character(value) && length(value) == 1L && !is.na(value) &&
      grepl("^[0-9]+$", value)) {
    result <- sub("^0+(?=[0-9])", "", value, perl = TRUE)
  } else if (is.numeric(value) && length(value) == 1L && is.finite(value) &&
      value == floor(value) && abs(value) <= 2^53) {
    result <- sprintf("%.0f", value)
  } else {
    fail(label, " is not one exact nonnegative JSON integer")
  }
  if (!allow_zero && identical(result, "0")) {
    fail(label, " must be positive")
  }
  result
}

expect_string <- function(value, expected, label) {
  observed <- scalar_character(value, label)
  if (!identical(observed, expected)) {
    fail(label, " differs: expected `", expected, "`, observed `", observed, "`")
  }
  invisible(observed)
}

read_json <- function(path, label) {
  require_plain_file(path, label)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    fail("repository-local jsonlite package is required")
  }
  tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(error) fail(label, " is not valid JSON: ", conditionMessage(error))
  )
}

read_provenance <- function(path) {
  require_plain_file(path, "artifact provenance")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  expected_keys <- c(
    "workflow_sha", "workflow_ref", "checked_out_sha",
    "runner_os", "runner_arch", "r_platform"
  )
  matches <- regexec("^([a-z_]+)=(.*)$", lines, perl = TRUE)
  pieces <- regmatches(lines, matches)
  if (length(lines) != length(expected_keys) ||
      any(lengths(pieces) != 3L)) {
    fail("artifact provenance does not contain six exact key/value rows: ", path)
  }
  keys <- vapply(pieces, `[[`, character(1L), 2L)
  values <- vapply(pieces, `[[`, character(1L), 3L)
  if (!identical(keys, expected_keys)) {
    fail("artifact provenance keys or ordering differ: ", path)
  }
  setNames(values, keys)
}

verify_check_log <- function(path) {
  require_plain_file(path, "artifact R CMD check log")
  info <- file.info(path, extra_cols = FALSE)
  if (info$size > 16 * 1024^2) {
    fail("artifact R CMD check log exceeds the reviewed 16-MiB bound: ", path)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  status <- grep("^Status:", lines, value = TRUE)
  nonempty <- lines[nzchar(trimws(lines))]
  if (any(grepl("Execution halted", lines, fixed = TRUE)) ||
      !identical(status, "Status: OK") ||
      !length(nonempty) ||
      !identical(tail(nonempty, 1L), "Status: OK")) {
    fail("artifact R CMD check log lacks one sole, final clean status: ", path)
  }
  invisible(TRUE)
}

resolve_manifest_member <- function(member, evidence, manifest) {
  if (!nzchar(member) || grepl("[[:cntrl:]]", member) ||
      grepl("^/|\\\\|(^|/)\\.\\.?(/|$)", member, perl = TRUE)) {
    fail("SHA-256 manifest contains an unsafe path: ", manifest)
  }
  candidate <- file.path(evidence, member)
  if (!file.exists(candidate) || dir.exists(candidate)) {
    fail("SHA-256 manifest member is absent: ", member, " in ", manifest)
  }
  if (nzchar(Sys.readlink(candidate))) {
    fail("SHA-256 manifest member is symbolic: ", member)
  }
  path <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
  evidence_prefix <- paste0(evidence, "/")
  if (!startsWith(path, evidence_prefix)) {
    fail("SHA-256 manifest member escapes evidence: ", member)
  }
  require_plain_file(path, "SHA-256 manifest member", nonempty = FALSE)
}

verify_sha_manifest <- function(manifest, evidence) {
  manifest <- require_plain_file(manifest, "SHA-256 manifest")
  lines <- readLines(manifest, warn = FALSE, encoding = "UTF-8")
  if (!length(lines) || any(!nzchar(lines))) {
    fail("SHA-256 manifest is empty or contains blank rows: ", manifest)
  }
  matches <- regexec("^([0-9a-f]{64})  (.+)$", lines, perl = TRUE)
  pieces <- regmatches(lines, matches)
  if (any(lengths(pieces) != 3L)) {
    fail("SHA-256 manifest has a malformed row: ", manifest)
  }
  expected <- vapply(pieces, `[[`, character(1L), 2L)
  members <- vapply(pieces, `[[`, character(1L), 3L)
  paths <- vapply(
    members, resolve_manifest_member, character(1L),
    evidence = evidence, manifest = manifest
  )
  if (anyDuplicated(paths) || manifest %in% paths) {
    fail("SHA-256 manifest contains a duplicate or self-reference: ", manifest)
  }
  observed <- unname(tools::sha256sum(paths))
  if (anyNA(observed) || !identical(observed, expected)) {
    bad <- which(is.na(observed) | observed != expected)
    fail("SHA-256 manifest content differs at row ", bad[[1L]], ": ", manifest)
  }
  members
}

relative_regular_files <- function(directory, evidence, label) {
  entries <- list.files(
    directory,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = TRUE
  )
  if (length(entries) && any(nzchar(Sys.readlink(entries)))) {
    fail(label, " contains a symbolic entry")
  }
  files <- entries[!dir.exists(entries)]
  if (length(files)) {
    invisible(vapply(
      files, require_plain_file, character(1L), label = label,
      nonempty = FALSE
    ))
  }
  prefix <- paste0(evidence, "/")
  normalized <- normalizePath(
    files, winslash = "/", mustWork = length(files) > 0L
  )
  if (length(normalized) && any(!startsWith(normalized, prefix))) {
    fail(label, " contains a file outside the evidence directory")
  }
  substring(normalized, nchar(prefix) + 1L)
}

verify_archive_extraction <- function(archive, retained_root, label) {
  listing <- tryCatch(
    utils::unzip(archive, list = TRUE),
    error = function(error) fail(label, " is not a readable ZIP archive: ",
      conditionMessage(error))
  )
  members <- listing$Name
  if (!is.character(members) || !length(members) || any(!nzchar(members)) ||
      any(grepl("[[:cntrl:]]|^/|\\\\|(^|/)\\.\\.?(/|$)", members,
        perl = TRUE)) || anyDuplicated(members)) {
    fail(label, " contains an unsafe or duplicated ZIP member")
  }
  file_members <- members[!endsWith(members, "/")]
  if (!length(file_members)) {
    fail(label, " contains no regular-file members")
  }

  scratch <- tempfile("portability-artifact-")
  if (!dir.create(scratch, recursive = FALSE, showWarnings = FALSE)) {
    fail("could not create a temporary artifact extraction directory")
  }
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  tryCatch(
    suppressWarnings(utils::unzip(
      archive, exdir = scratch, unzip = "internal"
    )),
    error = function(error) fail(label, " could not be extracted: ",
      conditionMessage(error))
  )

  extracted_members <- relative_regular_files(
    scratch, scratch, paste0(label, " extracted tree")
  )
  retained_members <- relative_regular_files(
    retained_root, retained_root, paste0(label, " retained tree")
  )
  if (!identical(sort(file_members), sort(extracted_members)) ||
      !identical(sort(file_members), sort(retained_members))) {
    fail(label, " file inventory differs from its retained extracted tree")
  }
  ordered_members <- sort(file_members)
  extracted_hashes <- unname(tools::sha256sum(file.path(
    scratch, ordered_members
  )))
  retained_hashes <- unname(tools::sha256sum(file.path(
    retained_root, ordered_members
  )))
  if (anyNA(extracted_hashes) || anyNA(retained_hashes) ||
      !identical(extracted_hashes, retained_hashes)) {
    fail(label, " content differs from its retained extracted tree")
  }
  invisible(length(ordered_members))
}

verify_portability_ci_evidence <- function(
    evidence,
    run_id,
    run_attempt,
    harness_commit,
    harness_tag,
    candidate_commit,
    workflow_sha256) {
  evidence <- require_plain_directory(evidence, "portability evidence directory")
  expected_workflow_ref <- paste0("refs/tags/", harness_tag)

  run_path <- file.path(evidence, "run.json")
  jobs_path <- file.path(evidence, "jobs.json")
  artifacts_path <- file.path(evidence, "artifacts.json")
  workflow_path <- require_plain_file(
    file.path(evidence, "r-cmd-check.yml"), "executed workflow"
  )
  if (!identical(sha256_file(workflow_path), workflow_sha256)) {
    fail("executed workflow SHA-256 differs from the expected companion workflow")
  }

  run <- read_json(run_path, "retained REST run metadata")
  if (!identical(integer_string(run$id, "run.id"), run_id) ||
      !identical(integer_string(run$run_attempt, "run.run_attempt"), run_attempt)) {
    fail("retained REST run identity or attempt differs")
  }
  expect_string(run$event, "workflow_dispatch", "run.event")
  expect_string(run$status, "completed", "run.status")
  expect_string(run$conclusion, "success", "run.conclusion")
  expect_string(run$head_branch, harness_tag, "run.head_branch")
  expect_string(run$head_sha, harness_commit, "run.head_sha")
  expect_string(run$path, ".github/workflows/r-cmd-check.yml", "run.path")
  expect_string(run$name, "r-cmd-check", "run.name")
  expect_string(
    run$url,
    paste0("https://api.github.com/repos/mlr-org/paradox/actions/runs/", run_id),
    "run.url"
  )
  expect_string(
    run$html_url,
    paste0("https://github.com/mlr-org/paradox/actions/runs/", run_id),
    "run.html_url"
  )
  expect_string(run$repository$full_name, "mlr-org/paradox", "run.repository")
  expect_string(
    run$head_repository$full_name, "mlr-org/paradox", "run.head_repository"
  )

  jobs_response <- read_json(jobs_path, "retained REST jobs metadata")
  jobs <- jobs_response$jobs
  if (!is.list(jobs) || length(jobs) != 2L ||
      !identical(integer_string(
        jobs_response$total_count, "jobs.total_count", allow_zero = TRUE
      ), "2")) {
    fail("retained REST jobs metadata does not contain exactly two jobs")
  }
  job_names <- vapply(jobs, function(job) {
    scalar_character(job$name, "job.name")
  }, character(1L))
  expected_job_names <- c(
    "macos-15 / arm64 (release)",
    "windows-latest / x86_64 (release)"
  )
  if (!identical(sort(job_names), sort(expected_job_names)) ||
      anyDuplicated(job_names)) {
    fail("retained REST job names differ from the two-platform release matrix")
  }
  expected_steps <- c(
    "Verify frozen candidate checkout" = "3",
    "Verify runner and R architecture" = "6",
    "Verify native source compilation" = "7",
    "Run R CMD check" = "8",
    "Verify R CMD check completion" = "9",
    "Retain run provenance" = "10",
    "Upload check evidence" = "11"
  )
  job_ids <- character(length(jobs))
  for (index in seq_along(jobs)) {
    job <- jobs[[index]]
    job_id <- integer_string(job$id, paste0("jobs[", index, "].id"))
    job_ids[[index]] <- job_id
    if (!identical(integer_string(job$run_id, "job.run_id"), run_id) ||
        !identical(integer_string(job$run_attempt, "job.run_attempt"), run_attempt)) {
      fail("REST job does not belong to the expected run and attempt: ", job_id)
    }
    expect_string(job$head_sha, harness_commit, "job.head_sha")
    expect_string(job$status, "completed", "job.status")
    expect_string(job$conclusion, "success", "job.conclusion")
    expect_string(
      job$url,
      paste0("https://api.github.com/repos/mlr-org/paradox/actions/jobs/", job_id),
      "job.url"
    )
    expect_string(
      job$html_url,
      paste0("https://github.com/mlr-org/paradox/actions/runs/", run_id,
        "/job/", job_id),
      "job.html_url"
    )
    steps <- job$steps
    if (!is.list(steps) || !length(steps)) {
      fail("REST job has no retained steps: ", job_id)
    }
    step_names <- vapply(steps, function(step) {
      scalar_character(step$name, "job step name")
    }, character(1L))
    for (step_name in names(expected_steps)) {
      matches <- which(step_names == step_name)
      if (length(matches) != 1L) {
        fail("required workflow step is absent or duplicated: ", step_name)
      }
      step <- steps[[matches]]
      if (!identical(
          integer_string(step$number, paste0(step_name, " step number")),
          unname(expected_steps[[step_name]])
        )) {
        fail("required workflow step number differs: ", step_name)
      }
      expect_string(step$status, "completed", paste0(step_name, " status"))
      expect_string(step$conclusion, "success", paste0(step_name, " conclusion"))
    }
    for (step in steps) {
      expect_string(step$status, "completed", "job step status")
      expect_string(step$conclusion, "success", "job step conclusion")
    }
  }
  if (anyDuplicated(job_ids)) {
    fail("retained REST job IDs are duplicated")
  }
  expected_job_logs <- sort(paste0("job-", job_ids, ".log"))
  actual_job_logs <- sort(list.files(
    evidence, pattern = "^job-[0-9]+\\.log$", full.names = FALSE
  ))
  if (!identical(actual_job_logs, expected_job_logs)) {
    fail("retained job-log inventory differs from REST job IDs")
  }
  invisible(vapply(
    file.path(evidence, actual_job_logs), require_plain_file, character(1L),
    label = "retained job log"
  ))

  artifacts_response <- read_json(
    artifacts_path, "retained REST artifacts metadata"
  )
  artifacts <- artifacts_response$artifacts
  if (!is.list(artifacts) || length(artifacts) != 2L ||
      !identical(integer_string(
        artifacts_response$total_count, "artifacts.total_count", allow_zero = TRUE
      ), "2")) {
    fail("retained REST artifacts metadata does not contain exactly two artifacts")
  }
  expected_artifacts <- list(
    "paradox-2.0.0-portability-macos-15-arm64" = c(
      runner_os = "macOS",
      runner_arch = "ARM64",
      r_platform = "aarch64-apple-darwin23"
    ),
    "paradox-2.0.0-portability-windows-latest-x86_64" = c(
      runner_os = "Windows",
      runner_arch = "X64",
      r_platform = "x86_64-w64-mingw32"
    )
  )
  artifact_names <- vapply(artifacts, function(artifact) {
    scalar_character(artifact$name, "artifact.name")
  }, character(1L))
  if (!identical(sort(artifact_names), sort(names(expected_artifacts))) ||
      anyDuplicated(artifact_names)) {
    fail("retained REST artifact names differ from the two-platform inventory")
  }
  artifact_ids <- character(length(artifacts))
  for (index in seq_along(artifacts)) {
    artifact <- artifacts[[index]]
    artifact_name <- artifact_names[[index]]
    artifact_id <- integer_string(
      artifact$id, paste0("artifact ", artifact_name, " id")
    )
    artifact_ids[[index]] <- artifact_id
    size <- integer_string(
      artifact$size_in_bytes, paste0("artifact ", artifact_name, " size")
    )
    if (identical(size, "0") ||
        scalar_logical(artifact$expired, "artifact.expired")) {
      fail("artifact is empty or expired: ", artifact_name)
    }
    digest <- scalar_character(artifact$digest, "artifact.digest")
    if (!grepl("^sha256:[0-9a-f]{64}$", digest)) {
      fail("artifact digest is not an exact SHA-256: ", artifact_name)
    }
    archive_path <- require_plain_file(
      file.path(evidence, "archives", paste0(artifact_name, ".zip")),
      paste0("retained artifact archive ", artifact_name)
    )
    if (!identical(sha256_file(archive_path), sub("^sha256:", "", digest))) {
      fail("retained artifact archive differs from its REST digest: ",
        artifact_name)
    }
    if (!identical(
        sprintf("%.0f", file.info(archive_path, extra_cols = FALSE)$size),
        size
      )) {
      fail("retained artifact archive size differs from REST metadata: ",
        artifact_name)
    }
    expect_string(
      artifact$url,
      paste0("https://api.github.com/repos/mlr-org/paradox/actions/artifacts/",
        artifact_id),
      "artifact.url"
    )
    expect_string(
      artifact$archive_download_url,
      paste0("https://api.github.com/repos/mlr-org/paradox/actions/artifacts/",
        artifact_id, "/zip"),
      "artifact.archive_download_url"
    )
    if (!identical(
        integer_string(artifact$workflow_run$id, "artifact.workflow_run.id"),
        run_id
      )) {
      fail("artifact is bound to a different workflow run: ", artifact_name)
    }
    expect_string(
      artifact$workflow_run$head_branch, harness_tag,
      "artifact.workflow_run.head_branch"
    )
    expect_string(
      artifact$workflow_run$head_sha, harness_commit,
      "artifact.workflow_run.head_sha"
    )

    artifact_root <- require_plain_directory(
      file.path(evidence, "artifacts", artifact_name),
      paste0("extracted artifact ", artifact_name)
    )
    provenance <- read_provenance(
      file.path(artifact_root, "ci-evidence", "provenance.txt")
    )
    expected_platform <- expected_artifacts[[artifact_name]]
    expected_provenance <- c(
      workflow_sha = harness_commit,
      workflow_ref = expected_workflow_ref,
      checked_out_sha = candidate_commit,
      expected_platform
    )
    if (!identical(provenance, expected_provenance)) {
      fail("artifact provenance identity or platform differs: ", artifact_name)
    }
    expected_log <- normalizePath(
      file.path(artifact_root, "check", "paradox.Rcheck", "00check.log"),
      winslash = "/", mustWork = FALSE
    )
    check_logs <- list.files(
      artifact_root,
      pattern = "^00check\\.log$",
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    if (length(check_logs) != 1L ||
        !identical(
          normalizePath(check_logs, winslash = "/", mustWork = TRUE),
          expected_log
        )) {
      fail("artifact does not contain one exact expected check log: ", artifact_name)
    }
    verify_check_log(expected_log)
    verify_archive_extraction(
      archive_path, artifact_root,
      paste0("retained artifact archive ", artifact_name)
    )
  }
  if (anyDuplicated(artifact_ids)) {
    fail("retained REST artifact IDs are duplicated")
  }
  archive_root <- require_plain_directory(
    file.path(evidence, "archives"), "retained artifact archives directory"
  )
  actual_archives <- sort(list.files(
    archive_root, pattern = "\\.zip$", full.names = FALSE, all.files = TRUE,
    no.. = TRUE
  ))
  expected_archives <- sort(paste0(names(expected_artifacts), ".zip"))
  if (!identical(actual_archives, expected_archives)) {
    fail("retained artifact archive inventory differs from REST metadata")
  }
  artifacts_directory <- require_plain_directory(
    file.path(evidence, "artifacts"), "extracted artifacts directory"
  )
  actual_artifact_directories <- sort(basename(list.dirs(
    artifacts_directory, recursive = FALSE, full.names = TRUE
  )))
  if (!identical(actual_artifact_directories, sort(names(expected_artifacts)))) {
    fail("extracted artifact directory inventory differs from REST metadata")
  }

  expected_manifest_names <- c(
    "ARCHIVE-SHA256SUMS",
    "ARTIFACT-SHA256SUMS",
    "EVIDENCE-SHA256SUMS",
    "JOB-LOG-SHA256SUMS",
    "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
  manifest_paths <- sort(list.files(
    evidence,
    pattern = "(^|-)SHA256SUMS$",
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  ))
  if (!identical(sort(basename(manifest_paths)), sort(expected_manifest_names))) {
    fail("SHA-256 manifest inventory differs from the six required manifests")
  }
  manifest_members <- setNames(
    lapply(manifest_paths, verify_sha_manifest, evidence = evidence),
    basename(manifest_paths)
  )
  expect_manifest_members <- function(name, expected) {
    observed <- manifest_members[[name]]
    if (!identical(sort(observed), sort(expected))) {
      fail(name, " does not cover its exact required file inventory")
    }
  }
  expect_manifest_members("ARCHIVE-SHA256SUMS", file.path(
    "archives", expected_archives
  ))
  expect_manifest_members("ARTIFACT-SHA256SUMS", relative_regular_files(
    artifacts_directory, evidence, "extracted artifact inventory"
  ))
  expect_manifest_members("JOB-LOG-SHA256SUMS", expected_job_logs)
  expect_manifest_members(
    "METADATA-SHA256SUMS",
    c("run.json", "jobs.json", "artifacts.json", "r-cmd-check.yml")
  )
  expect_manifest_members(
    "EVIDENCE-SHA256SUMS",
    setdiff(expected_manifest_names, "EVIDENCE-SHA256SUMS")
  )
  verifier_log <- "verifier-acceptance.log"
  retained_verifier <- require_plain_file(
    file.path(evidence, "verify-portability-ci-evidence.R"),
    "retained portability evidence verifier"
  )
  if (!identical(sha256_file(retained_verifier), sha256_file(script_path))) {
    fail("retained portability evidence verifier differs from the executing verifier")
  }
  expect_manifest_members(
    "VERIFIER-SHA256SUMS",
    c("verify-portability-ci-evidence.R", verifier_log)
  )

  acceptance_lines <- c(
    "portability_ci_evidence=passed",
    paste0("run_id=", run_id),
    paste0("run_attempt=", run_attempt),
    "jobs=2",
    "artifacts=2",
    paste0("sha_manifests=", length(manifest_paths)),
    paste0("sha_manifest_members=", sum(lengths(manifest_members)))
  )
  acceptance_path <- require_plain_file(
    file.path(evidence, verifier_log), "verifier acceptance receipt"
  )
  observed_acceptance <- readLines(
    acceptance_path, warn = FALSE, encoding = "UTF-8"
  )
  if (!identical(observed_acceptance, acceptance_lines)) {
    fail("verifier acceptance receipt differs from deterministic output")
  }
  cat(paste0(acceptance_lines, collapse = "\n"), "\n", sep = "")
  invisible(TRUE)
}

main <- function() {
  arguments <- commandArgs(trailingOnly = TRUE)
  options <- list(
    evidence = NULL,
    run_id = NULL,
    run_attempt = "1",
    harness_commit = NULL,
    harness_tag = NULL,
    candidate_commit = NULL,
    workflow_sha256 = NULL
  )
  positionals <- character()
  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (argument %in% c("-h", "--help")) {
      usage()
      return(invisible(TRUE))
    }
    option_names <- c(
      "--run-id" = "run_id",
      "--run-attempt" = "run_attempt",
      "--harness-commit" = "harness_commit",
      "--harness-tag" = "harness_tag",
      "--candidate-commit" = "candidate_commit",
      "--workflow-sha256" = "workflow_sha256"
    )
    if (argument %in% names(option_names)) {
      if (index == length(arguments)) {
        fail(argument, " requires a value")
      }
      options[[option_names[[argument]]]] <- arguments[[index + 1L]]
      index <- index + 2L
      next
    }
    if (startsWith(argument, "--")) {
      fail("unknown portability evidence verifier option: ", argument)
    }
    positionals <- c(positionals, argument)
    index <- index + 1L
  }
  if (length(positionals) != 1L) {
    fail("expected exactly one evidence directory")
  }
  evidence_argument <- positionals[[1L]]
  if (!grepl("^/", evidence_argument)) {
    evidence_argument <- file.path(root, evidence_argument)
  }
  evidence <- normalizePath(
    evidence_argument, winslash = "/", mustWork = FALSE
  )
  if (is.null(options$run_id)) {
    match <- regexec("^r-cmd-check-([0-9]+)$", basename(evidence), perl = TRUE)
    pieces <- regmatches(basename(evidence), match)[[1L]]
    if (length(pieces) != 2L) {
      fail("--run-id is required when the evidence directory does not end in its run ID")
    }
    options$run_id <- pieces[[2L]]
  }
  if (!is.character(options$run_id) || length(options$run_id) != 1L ||
      !is.character(options$harness_commit) ||
      length(options$harness_commit) != 1L ||
      !is.character(options$harness_tag) || length(options$harness_tag) != 1L ||
      !is.character(options$candidate_commit) ||
      length(options$candidate_commit) != 1L ||
      !is.character(options$workflow_sha256) ||
      length(options$workflow_sha256) != 1L ||
      !grepl("^[0-9]+$", options$run_id) ||
      !grepl("^[1-9][0-9]*$", options$run_attempt) ||
      !grepl("^[0-9a-f]{40}$", options$harness_commit) ||
      !grepl("^[0-9a-f]{40}$", options$candidate_commit) ||
      !grepl("^[0-9a-f]{64}$", options$workflow_sha256) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", options$harness_tag)) {
    fail("expected run, commit, tag, attempt, or workflow identity is malformed")
  }
  if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT"), root)) {
    fail("activate the exact repository-local toolchain first: . scripts/activate")
  }
  verify_portability_ci_evidence(
    evidence = evidence,
    run_id = options$run_id,
    run_attempt = options$run_attempt,
    harness_commit = options$harness_commit,
    harness_tag = options$harness_tag,
    candidate_commit = options$candidate_commit,
    workflow_sha256 = options$workflow_sha256
  )
}

if (sys.nframe() == 0L) {
  main()
}
