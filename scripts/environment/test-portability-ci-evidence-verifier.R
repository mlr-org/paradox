#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  fail("could not identify portability evidence verifier test")
}
script_path <- normalizePath(
  sub("^--file=", "", script_argument), winslash = "/", mustWork = TRUE
)
root <- normalizePath(
  file.path(dirname(script_path), "..", ".."),
  winslash = "/", mustWork = TRUE
)
if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT"), root)) {
  fail("activate the exact repository-local toolchain first: . scripts/activate")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  fail("repository-local jsonlite package is required")
}

verifier <- file.path(
  root, "scripts", "environment", "verify-portability-ci-evidence.R"
)
if (!file.exists(verifier) || nzchar(Sys.readlink(verifier))) {
  fail("portability evidence verifier is absent or symbolic")
}

fixture <- tempfile("portability-ci-evidence-")
dir.create(fixture)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

run_id <- "123456789"
run_attempt <- "1"
harness_commit <- paste(rep("a", 40L), collapse = "")
candidate_commit <- paste(rep("b", 40L), collapse = "")
harness_tag <- "paradox-test-harness-aaaaaaaa"
workflow_ref <- paste0("refs/tags/", harness_tag)
workflow <- file.path(fixture, "r-cmd-check.yml")
writeLines(c("name: fixture", "on: workflow_dispatch"), workflow)
workflow_sha256 <- unname(tools::sha256sum(workflow))

write_json <- function(value, path) {
  jsonlite::write_json(
    value, path, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
}

run <- list(
  id = as.numeric(run_id),
  run_attempt = 1L,
  event = "workflow_dispatch",
  status = "completed",
  conclusion = "success",
  head_branch = harness_tag,
  head_sha = harness_commit,
  path = ".github/workflows/r-cmd-check.yml",
  name = "r-cmd-check",
  url = paste0(
    "https://api.github.com/repos/mlr-org/paradox/actions/runs/", run_id
  ),
  html_url = paste0(
    "https://github.com/mlr-org/paradox/actions/runs/", run_id
  ),
  repository = list(full_name = "mlr-org/paradox"),
  head_repository = list(full_name = "mlr-org/paradox")
)
write_json(run, file.path(fixture, "run.json"))

required_steps <- c(
  "Verify frozen candidate checkout" = 3L,
  "Verify runner and R architecture" = 6L,
  "Verify native source compilation" = 7L,
  "Run R CMD check" = 8L,
  "Verify R CMD check completion" = 9L,
  "Retain run provenance" = 10L,
  "Upload check evidence" = 11L
)
make_steps <- function() {
  unname(lapply(seq_along(required_steps), function(index) {
    list(
      name = names(required_steps)[[index]],
      number = unname(required_steps[[index]]),
      status = "completed",
      conclusion = "success"
    )
  }))
}
job_specs <- list(
  list(id = 2001, name = "macos-15 / arm64 (release)"),
  list(id = 2002, name = "windows-latest / x86_64 (release)")
)
jobs <- lapply(job_specs, function(spec) {
  list(
    id = spec$id,
    run_id = as.numeric(run_id),
    run_attempt = 1L,
    head_sha = harness_commit,
    name = spec$name,
    status = "completed",
    conclusion = "success",
    url = paste0(
      "https://api.github.com/repos/mlr-org/paradox/actions/jobs/", spec$id
    ),
    html_url = paste0(
      "https://github.com/mlr-org/paradox/actions/runs/", run_id,
      "/job/", spec$id
    ),
    steps = make_steps()
  )
})
write_json(
  list(total_count = length(jobs), jobs = jobs),
  file.path(fixture, "jobs.json")
)
for (spec in job_specs) {
  writeLines("retained fixture job log", file.path(
    fixture, paste0("job-", spec$id, ".log")
  ))
}

artifact_specs <- list(
  list(
    id = 3001,
    name = "paradox-2.0.0-portability-macos-15-arm64",
    runner_os = "macOS",
    runner_arch = "ARM64",
    r_platform = "aarch64-apple-darwin23"
  ),
  list(
    id = 3002,
    name = "paradox-2.0.0-portability-windows-latest-x86_64",
    runner_os = "Windows",
    runner_arch = "X64",
    r_platform = "x86_64-w64-mingw32"
  )
)
artifacts <- lapply(artifact_specs, function(spec) {
  artifact_root <- file.path(fixture, "artifacts", spec$name)
  dir.create(file.path(artifact_root, "ci-evidence"), recursive = TRUE)
  dir.create(file.path(
    artifact_root, "check", "paradox.Rcheck"
  ), recursive = TRUE)
  writeLines(c(
    paste0("workflow_sha=", harness_commit),
    paste0("workflow_ref=", workflow_ref),
    paste0("checked_out_sha=", candidate_commit),
    paste0("runner_os=", spec$runner_os),
    paste0("runner_arch=", spec$runner_arch),
    paste0("r_platform=", spec$r_platform)
  ), file.path(artifact_root, "ci-evidence", "provenance.txt"))
  writeLines(c("* DONE", "Status: OK"), file.path(
    artifact_root, "check", "paradox.Rcheck", "00check.log"
  ))
  archive_root <- file.path(fixture, "archives")
  dir.create(archive_root, showWarnings = FALSE)
  archive_path <- file.path(archive_root, paste0(spec$name, ".zip"))
  python <- file.path(root, ".local", "toolchain", "bin", "python")
  if (!file.exists(python)) {
    fail("the pinned local Python is required for the portability verifier fixture")
  }
  archive_inputs <- list.files(
    artifact_root,
    recursive = FALSE,
    full.names = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )
  old_working_directory <- setwd(artifact_root)
  zip_status <- tryCatch(
    system2(
      python,
      args = c(
        "-m", "zipfile", "-c", shQuote(archive_path), shQuote(archive_inputs)
      ),
      stdout = FALSE,
      stderr = FALSE
    ),
    finally = setwd(old_working_directory)
  )
  if (!file.exists(archive_path) || zip_status != 0L) {
    fail("could not create a synthetic portability artifact ZIP")
  }
  list(
    id = spec$id,
    name = spec$name,
    size_in_bytes = unname(file.info(archive_path, extra_cols = FALSE)$size),
    expired = FALSE,
    digest = paste0("sha256:", unname(tools::sha256sum(archive_path))),
    url = paste0(
      "https://api.github.com/repos/mlr-org/paradox/actions/artifacts/",
      spec$id
    ),
    archive_download_url = paste0(
      "https://api.github.com/repos/mlr-org/paradox/actions/artifacts/",
      spec$id, "/zip"
    ),
    workflow_run = list(
      id = as.numeric(run_id),
      head_branch = harness_tag,
      head_sha = harness_commit
    )
  )
})
write_json(
  list(total_count = length(artifacts), artifacts = artifacts),
  file.path(fixture, "artifacts.json")
)

retained_verifier <- file.path(fixture, "verify-portability-ci-evidence.R")
if (!file.copy(verifier, retained_verifier)) {
  fail("could not retain the synthetic verifier source")
}
write_manifest <- function(name, members) {
  paths <- file.path(fixture, members)
  if (any(!file.exists(paths)) || any(dir.exists(paths))) {
    fail("synthetic manifest member is absent: ", name)
  }
  writeLines(
    paste0(unname(tools::sha256sum(paths)), "  ", members),
    file.path(fixture, name)
  )
}
archive_members <- file.path("archives", paste0(
  vapply(artifact_specs, `[[`, character(1L), "name"), ".zip"
))
artifact_files <- list.files(
  file.path(fixture, "artifacts"),
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  no.. = TRUE
)
artifact_files <- artifact_files[!dir.exists(artifact_files)]
artifact_members <- substring(
  normalizePath(artifact_files, winslash = "/", mustWork = TRUE),
  nchar(normalizePath(fixture, winslash = "/", mustWork = TRUE)) + 2L
)
job_members <- paste0(
  "job-", vapply(job_specs, `[[`, numeric(1L), "id"), ".log"
)
metadata_members <- c(
  "run.json", "jobs.json", "artifacts.json", "r-cmd-check.yml"
)
manifest_member_count <- length(archive_members) + length(artifact_members) +
  length(job_members) + length(metadata_members) + 2L + 5L
writeLines(c(
  "portability_ci_evidence=passed",
  paste0("run_id=", run_id),
  paste0("run_attempt=", run_attempt),
  "jobs=2",
  "artifacts=2",
  "sha_manifests=6",
  paste0("sha_manifest_members=", manifest_member_count)
), file.path(fixture, "verifier-acceptance.log"))
write_manifest("ARCHIVE-SHA256SUMS", archive_members)
write_manifest("ARTIFACT-SHA256SUMS", artifact_members)
write_manifest("JOB-LOG-SHA256SUMS", job_members)
write_manifest("METADATA-SHA256SUMS", metadata_members)
write_manifest(
  "VERIFIER-SHA256SUMS",
  c("verify-portability-ci-evidence.R", "verifier-acceptance.log")
)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

arguments <- c(
  verifier,
  fixture,
  "--run-id", run_id,
  "--run-attempt", run_attempt,
  "--harness-commit", harness_commit,
  "--harness-tag", harness_tag,
  "--candidate-commit", candidate_commit,
  "--workflow-sha256", workflow_sha256
)
invoke <- function() {
  suppressWarnings(system2(
    file.path(root, ".local", "toolchain", "bin", "Rscript"),
    args = c("--vanilla", shQuote(arguments)),
    stdout = TRUE,
    stderr = TRUE
  ))
}
`%||%` <- function(left, right) if (is.null(left)) right else left
status <- function(result) attr(result, "status") %||% 0L

result <- invoke()
if (status(result) != 0L ||
    !any(result == "portability_ci_evidence=passed")) {
  fail("valid synthetic portability evidence was rejected:\n", paste(
    result, collapse = "\n"
  ))
}

mac_log <- file.path(
  fixture, "artifacts", artifact_specs[[1L]]$name,
  "check", "paradox.Rcheck", "00check.log"
)
writeLines(c("Status: OK", "unexpected trailing output"), mac_log)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("lacks one sole, final clean status", result, fixed = TRUE))) {
  fail("verifier accepted a check log with output after Status: OK")
}
writeLines(c("* DONE", "Status: OK"), mac_log)

mac_archive <- file.path(
  fixture, "archives", paste0(artifact_specs[[1L]]$name, ".zip")
)
archive_bytes <- readBin(mac_archive, what = "raw", n = file.info(mac_archive)$size)
writeBin(c(archive_bytes, as.raw(0L)), mac_archive)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("differs from its REST digest", result, fixed = TRUE))) {
  fail("verifier accepted an artifact archive with the wrong digest")
}
writeBin(archive_bytes, mac_archive)

job_log <- file.path(fixture, "job-2001.log")
writeLines(c(readLines(job_log), "tamper"), job_log)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("manifest content differs", result, fixed = TRUE))) {
  fail("verifier accepted a file that differs from its SHA-256 manifest")
}
writeLines("retained fixture job log", job_log)

writeLines(c("* ALTERED BUT VALID", "Status: OK"), mac_log)
write_manifest("ARTIFACT-SHA256SUMS", artifact_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("content differs from its retained extracted tree", result,
      fixed = TRUE))) {
  fail("verifier did not bind the extracted artifact tree to its raw ZIP")
}
writeLines(c("* DONE", "Status: OK"), mac_log)
write_manifest("ARTIFACT-SHA256SUMS", artifact_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

wrong_size_artifacts <- artifacts
wrong_size_artifacts[[1L]]$size_in_bytes <-
  wrong_size_artifacts[[1L]]$size_in_bytes + 1
write_json(
  list(total_count = length(wrong_size_artifacts), artifacts = wrong_size_artifacts),
  file.path(fixture, "artifacts.json")
)
write_manifest("METADATA-SHA256SUMS", metadata_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("archive size differs from REST metadata", result,
      fixed = TRUE))) {
  fail("verifier accepted an artifact archive with the wrong REST size")
}
write_json(
  list(total_count = length(artifacts), artifacts = artifacts),
  file.path(fixture, "artifacts.json")
)
write_manifest("METADATA-SHA256SUMS", metadata_members)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

acceptance_log <- file.path(fixture, "verifier-acceptance.log")
acceptance_lines <- readLines(acceptance_log, warn = FALSE)
writeLines(c(acceptance_lines, "unexpected trailing output"), acceptance_log)
write_manifest(
  "VERIFIER-SHA256SUMS",
  c("verify-portability-ci-evidence.R", "verifier-acceptance.log")
)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("acceptance receipt differs from deterministic output", result,
      fixed = TRUE))) {
  fail("verifier accepted a non-deterministic success receipt")
}
writeLines(acceptance_lines, acceptance_log)
write_manifest(
  "VERIFIER-SHA256SUMS",
  c("verify-portability-ci-evidence.R", "verifier-acceptance.log")
)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)

evidence_manifest <- file.path(fixture, "EVIDENCE-SHA256SUMS")
evidence_manifest_bytes <- readBin(
  evidence_manifest, what = "raw", n = file.info(evidence_manifest)$size
)
unlink(evidence_manifest)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("inventory differs from the six required", result, fixed = TRUE))) {
  fail("verifier accepted a missing required SHA-256 manifest")
}
writeBin(evidence_manifest_bytes, evidence_manifest)

writeLines(
  paste0(paste(rep("0", 64L), collapse = ""), "  run.json"),
  file.path(fixture, "EXTRA-SHA256SUMS")
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("inventory differs from the six required", result, fixed = TRUE))) {
  fail("verifier accepted an extra SHA-256 manifest")
}
unlink(file.path(fixture, "EXTRA-SHA256SUMS"))

artifact_manifest <- file.path(fixture, "ARTIFACT-SHA256SUMS")
artifact_manifest_lines <- readLines(artifact_manifest, warn = FALSE)
writeLines(head(artifact_manifest_lines, -1L), artifact_manifest)
write_manifest(
  "EVIDENCE-SHA256SUMS",
  c(
    "ARCHIVE-SHA256SUMS", "ARTIFACT-SHA256SUMS",
    "JOB-LOG-SHA256SUMS", "METADATA-SHA256SUMS",
    "VERIFIER-SHA256SUMS"
  )
)
result <- invoke()
if (status(result) == 0L ||
    !any(grepl("does not cover its exact required", result, fixed = TRUE))) {
  fail("verifier accepted an incomplete artifact SHA-256 manifest")
}

cat("portability CI evidence verifier tests passed\n")
