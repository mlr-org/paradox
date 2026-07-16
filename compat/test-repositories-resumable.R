# Candidate-neutral, resumable repository compatibility gate.  This file is
# sourced by test-repositories.R by default; the old schema-3 implementation is
# retained there only for historical evidence replay.

arguments <- commandArgs(trailingOnly = TRUE)
usage <- paste(
  "usage: test-repositories.R",
  "[ROOT [MAX_PRIORITY [CANDIDATE_LIBRARY [DEPENDENCY_LIBRARY]]]]",
  "--run-id ID [--plan-only|--verify]",
  "[--candidate-source PATH] [--candidate-origin URL]",
  "[--repositories NAME[,NAME...]] [--timeout-seconds N] [--jobs N]"
)

positionals <- character()
seen_value_options <- character()
options <- list(
  run_id = NULL, plan_only = FALSE, verify = FALSE,
  candidate_source = NULL, candidate_origin = NULL,
  repositories = NULL, timeout_seconds = 3600, jobs = NULL
)
take_value <- function(argument, name, index) {
  prefix <- paste0("--", name, "=")
  if (identical(argument, paste0("--", name))) {
    if (index == length(arguments)) stop(usage, call. = FALSE)
    return(list(value = arguments[[index + 1L]], next_index = index + 2L))
  }
  if (startsWith(argument, prefix)) {
    return(list(value = substring(argument, nchar(prefix) + 1L),
      next_index = index + 1L))
  }
  NULL
}

index <- 1L
while (index <= length(arguments)) {
  argument <- arguments[[index]]
  matched <- FALSE
  for (name in c("run-id", "candidate-source", "candidate-origin",
      "repositories", "timeout-seconds", "jobs")) {
    parsed <- take_value(argument, name, index)
    if (!is.null(parsed)) {
      key <- gsub("-", "_", name, fixed = TRUE)
      if (key %in% seen_value_options) {
        stop(usage, call. = FALSE)
      }
      seen_value_options <- c(seen_value_options, key)
      options[[key]] <- parsed$value
      index <- parsed$next_index
      matched <- TRUE
      break
    }
  }
  if (matched) next
  if (identical(argument, "--plan-only")) {
    if (isTRUE(options$plan_only)) stop(usage, call. = FALSE)
    options$plan_only <- TRUE
    index <- index + 1L
  } else if (identical(argument, "--verify")) {
    if (isTRUE(options$verify)) stop(usage, call. = FALSE)
    options$verify <- TRUE
    index <- index + 1L
  } else if (startsWith(argument, "--")) {
    stop(usage, call. = FALSE)
  } else {
    positionals <- c(positionals, argument)
    index <- index + 1L
  }
}
if (length(positionals) > 4L || is.null(options$run_id) ||
    (options$plan_only && options$verify)) {
  stop(usage, call. = FALSE)
}

run_id <- options$run_id
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", run_id) ||
    run_id %in% c(".", "..")) {
  stop("--run-id must be one safe name of at most 128 characters", call. = FALSE)
}
candidate_run_id <- Sys.getenv("PARADOX_CANDIDATE_RUN_ID", unset = "")
if (!identical(candidate_run_id, run_id)) {
  stop("PARADOX_CANDIDATE_RUN_ID must equal --run-id", call. = FALSE)
}

root <- if (length(positionals)) {
  normalizePath(positionals[[1L]], winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
max_priority <- if (length(positionals) >= 2L) {
  suppressWarnings(as.integer(positionals[[2L]]))
} else 0L
if (length(max_priority) != 1L || is.na(max_priority) || max_priority < 0L ||
    !identical(as.character(max_priority), if (length(positionals) >= 2L)
      positionals[[2L]] else "0")) {
  stop("MAX_PRIORITY must be one non-negative integer", call. = FALSE)
}
timeout_seconds <- suppressWarnings(as.numeric(options$timeout_seconds))
if (length(timeout_seconds) != 1L || is.na(timeout_seconds) ||
    !is.finite(timeout_seconds) || timeout_seconds < 1 ||
    timeout_seconds > 21600) {
  stop("--timeout-seconds must be between 1 and 21600", call. = FALSE)
}
operator_jobs <- options$jobs
if (!is.null(operator_jobs) && (!grepl("^[1-9][0-9]*$", operator_jobs) ||
    !identical(as.character(as.integer(operator_jobs)), operator_jobs))) {
  stop("--jobs must be one canonical positive integer", call. = FALSE)
}

if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT", unset = ""), root)) {
  stop("activate the repository-local environment first: . scripts/activate",
    call. = FALSE)
}
expected_r <- file.path(root, ".local", "toolchain", "bin", "R")
expected_rscript <- file.path(root, ".local", "toolchain", "bin", "Rscript")
expected_git <- file.path(root, ".local", "toolchain", "bin", "git")
if (!identical(unname(Sys.which("R")), expected_r) ||
    !identical(unname(Sys.which("Rscript")), expected_rscript) ||
    !identical(unname(Sys.which("git")), expected_git) ||
    !identical(normalizePath(R.home(), winslash = "/", mustWork = TRUE),
      file.path(root, ".local", "toolchain", "lib", "R"))) {
  stop("the exact repository-local R, Rscript, and Git must be active",
    call. = FALSE)
}

run_directory <- normalizePath(file.path(root, ".local", "compat", "runs",
  run_id), winslash = "/", mustWork = TRUE)
expected_run_parent <- normalizePath(file.path(root, ".local", "compat", "runs"),
  winslash = "/", mustWork = TRUE)
if (!identical(dirname(run_directory), expected_run_parent)) {
  stop("candidate run directory escaped the managed run root", call. = FALSE)
}
candidate_library <- if (length(positionals) >= 3L) {
  normalizePath(positionals[[3L]], winslash = "/", mustWork = TRUE)
} else normalizePath(file.path(run_directory, "library-candidate"),
  winslash = "/", mustWork = TRUE)
dependency_library <- if (length(positionals) >= 4L) {
  normalizePath(positionals[[4L]], winslash = "/", mustWork = TRUE)
} else normalizePath(file.path(root, ".local", "compat", "R",
  "library-dependencies"), winslash = "/", mustWork = TRUE)
if (!identical(candidate_library, file.path(run_directory, "library-candidate")) ||
    identical(candidate_library, dependency_library)) {
  stop("candidate library must be run-local and distinct from dependencies",
    call. = FALSE)
}

source_paths <- c(
  runner = file.path(root, "compat", "repository-runner.R"),
  child = file.path(root, "compat", "repository-test-child.R"),
  fingerprint = file.path(root, "compat", "fingerprint.R"),
  evidence = file.path(root, "compat", "repository-evidence.R"),
  compat_system = file.path(root, "compat", "compat-system-evidence.R"),
  resource_jobs = file.path(root, "scripts", "environment", "resource-jobs")
)
for (path in source_paths) {
  if (!file.exists(path) || dir.exists(path) || nzchar(Sys.readlink(path))) {
    stop("required repository runner input is absent, non-regular, or symbolic: ",
      path, call. = FALSE)
  }
}
sys.source(source_paths[["runner"]], envir = environment(), keep.source = FALSE)
sys.source(source_paths[["fingerprint"]], envir = environment(), keep.source = FALSE)
sys.source(source_paths[["evidence"]], envir = environment(), keep.source = FALSE)
sys.source(source_paths[["compat_system"]], envir = environment(),
  keep.source = FALSE)

repository_runner_assert_git_environment()

candidate_ref <- Sys.getenv("PARADOX_CANDIDATE_REF", unset = "")
candidate_commit <- Sys.getenv("PARADOX_CANDIDATE_COMMIT", unset = "")
candidate_tree <- Sys.getenv("PARADOX_CANDIDATE_TREE", unset = "")
candidate_source_argument <- options$candidate_source
if (is.null(candidate_source_argument)) {
  candidate_source_argument <- Sys.getenv("PARADOX_CANDIDATE_SOURCE", unset = "")
}
if (!nzchar(candidate_source_argument)) {
  stop(paste(
    "a clean detached candidate worktree is required; pass --candidate-source",
    "or set PARADOX_CANDIDATE_SOURCE"
  ), call. = FALSE)
}
candidate_source <- normalizePath(candidate_source_argument, winslash = "/",
  mustWork = TRUE)
candidate_origin <- options$candidate_origin
if (is.null(candidate_origin)) {
  candidate_origin <- Sys.getenv("PARADOX_CANDIDATE_ORIGIN", unset = "")
}
if (!nzchar(candidate_origin)) {
  candidate_origin <- repository_runner_git_value(expected_git, candidate_source,
    c("remote", "get-url", "origin"), "reading candidate origin")
}
if (!grepl("^https://[^[:space:]]+[.]git$", candidate_origin) ||
    !grepl("^refs/[A-Za-z0-9][A-Za-z0-9._/-]*$", candidate_ref) ||
    !grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", candidate_commit) ||
    !grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", candidate_tree)) {
  stop("candidate origin, full ref, commit, or tree is malformed", call. = FALSE)
}
repository_runner_git_run(expected_git, candidate_source,
  c("check-ref-format", candidate_ref), "validating full candidate ref")

candidate_package <- normalizePath(file.path(candidate_library, "paradox"),
  winslash = "/", mustWork = TRUE)
if (!identical(dirname(candidate_package), candidate_library)) {
  stop("candidate paradox package escaped its dedicated library", call. = FALSE)
}
candidate_description <- repository_runner_require_file(file.path(candidate_package,
  "DESCRIPTION"), "installed candidate DESCRIPTION")
candidate_version <- unname(read.dcf(candidate_description,
  fields = "Version")[[1L, "Version"]])
if (length(candidate_version) != 1L || is.na(candidate_version) ||
    !nzchar(candidate_version) || grepl("[\r\n\t]", candidate_version)) {
  stop("installed candidate version is malformed", call. = FALSE)
}

candidate_entries <- sort(list.files(candidate_library, all.files = TRUE,
  no.. = TRUE), method = "radix")
expected_candidate_entries <- sort(c(
  ".paradox-candidate-content-sha256",
  ".paradox-candidate-provenance.sha256",
  ".paradox-candidate-provenance.tsv", "paradox"
), method = "radix")
if (!identical(candidate_entries, expected_candidate_entries)) {
  stop("candidate library has an unexpected top-level inventory", call. = FALSE)
}

candidate_sentinel <- repository_runner_require_file(file.path(candidate_library,
  ".paradox-candidate-content-sha256"), "candidate content sentinel")
candidate_content_sha256 <- readLines(candidate_sentinel, warn = FALSE)
if (length(candidate_content_sha256) != 1L ||
    !grepl("^[0-9a-f]{64}$", candidate_content_sha256)) {
  stop("candidate content sentinel is malformed", call. = FALSE)
}
declared_content <- Sys.getenv("PARADOX_CANDIDATE_CONTENT_SHA256", unset = "")
if (nzchar(declared_content) &&
    !identical(declared_content, candidate_content_sha256)) {
  stop("candidate content environment pin differs from its sentinel",
    call. = FALSE)
}

candidate_provenance_path <- repository_runner_require_file(file.path(
  candidate_library, ".paradox-candidate-provenance.tsv"),
  "candidate provenance receipt")
candidate_provenance_seal <- repository_runner_require_file(file.path(
  candidate_library, ".paradox-candidate-provenance.sha256"),
  "candidate provenance seal")
candidate_provenance_frame <- utils::read.delim(candidate_provenance_path,
  header = TRUE, sep = "\t", quote = "", comment.char = "",
  colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE)
candidate_provenance_fields <- c(
  "schema", "candidate_run_id", "candidate_ref", "candidate_commit",
  "candidate_tree", "candidate_version", "candidate_library",
  "dependency_library", "dependency_library_content_sha256",
  "source_archive_sha256", "candidate_content_sha256", "installer_sha256",
  "git_authenticator_sha256"
)
if (!identical(names(candidate_provenance_frame), c("key", "value")) ||
    !identical(candidate_provenance_frame$key, candidate_provenance_fields) ||
    anyNA(candidate_provenance_frame) ||
    any(!nzchar(candidate_provenance_frame$value))) {
  stop("candidate provenance receipt has an unexpected schema", call. = FALSE)
}
candidate_provenance <- stats::setNames(candidate_provenance_frame$value,
  candidate_provenance_frame$key)
candidate_provenance_sha256 <- repository_runner_sha256(candidate_provenance_path)
if (!identical(readLines(candidate_provenance_seal, warn = FALSE), paste0(
    candidate_provenance_sha256, "  .paradox-candidate-provenance.tsv"))) {
  stop("candidate provenance seal is malformed", call. = FALSE)
}
expected_provenance <- c(
  schema = "2", candidate_run_id = run_id, candidate_ref = candidate_ref,
  candidate_commit = candidate_commit, candidate_tree = candidate_tree,
  candidate_version = candidate_version, candidate_library = candidate_library,
  dependency_library = dependency_library,
  candidate_content_sha256 = candidate_content_sha256
)
if (!identical(unname(candidate_provenance[names(expected_provenance)]),
    unname(expected_provenance)) || any(!grepl("^[0-9a-f]{64}$",
      candidate_provenance[c("dependency_library_content_sha256",
        "source_archive_sha256", "installer_sha256",
        "git_authenticator_sha256")]))) {
  stop("candidate provenance disagrees with the selected candidate",
    call. = FALSE)
}

installer_path <- repository_runner_require_file(file.path(root, "compat",
  "install-candidate"), "candidate installer")
authenticator_path <- repository_runner_require_file(file.path(root, "compat",
  "authenticate-candidate-git"), "candidate Git authenticator")
if (file.access(installer_path, mode = 1L) != 0L ||
    file.access(authenticator_path, mode = 1L) != 0L) {
  stop("candidate installer and Git authenticator must remain executable",
    call. = FALSE)
}
if (!identical(candidate_provenance[["installer_sha256"]],
      repository_runner_sha256(installer_path)) ||
    !identical(candidate_provenance[["git_authenticator_sha256"]],
      repository_runner_sha256(authenticator_path))) {
  stop("candidate provenance names different installer or Git authenticator bytes",
    call. = FALSE)
}

candidate_config_stub <- list(
  root = root, git = expected_git, candidate_source = candidate_source,
  candidate_origin = candidate_origin, candidate_ref = candidate_ref,
  candidate_commit = candidate_commit, candidate_tree = candidate_tree
)
candidate_authentication <- repository_runner_authenticate_candidate(
  candidate_config_stub)
installer_archive <- tempfile("repository-candidate-installer-", fileext = ".tar")
on.exit(unlink(installer_archive), add = TRUE)
repository_runner_git_run(expected_git, candidate_source,
  c("-c", "tar.umask=0002", "archive", "--format=tar", candidate_commit),
  "reproducing candidate installer archive", stdout = installer_archive)
if (!identical(repository_runner_sha256(installer_archive),
    candidate_provenance[["source_archive_sha256"]])) {
  stop("candidate installer archive is not reproducible from the detached source",
    call. = FALSE)
}

manifest_path <- file.path(root, "compat", "github-repositories.tsv")
snapshot_path <- file.path(root, "compat", "github-snapshot.tsv")
manifest <- repository_runner_read_tsv(manifest_path,
  c("repository", "url", "relation", "priority", "action", "notes"),
  label = "GitHub repository manifest")
snapshot <- repository_runner_read_tsv(snapshot_path,
  c("repository", "url", "priority", "commit", "commit_date", "branch"),
  label = "GitHub repository snapshot")
if (anyDuplicated(manifest$repository) || anyDuplicated(snapshot$repository)) {
  stop("GitHub manifests contain duplicate repositories", call. = FALSE)
}
manifest_priority <- suppressWarnings(as.integer(manifest$priority))
snapshot_priority <- suppressWarnings(as.integer(snapshot$priority))
if (anyNA(manifest_priority) || anyNA(snapshot_priority) ||
    !identical(as.character(manifest_priority), manifest$priority) ||
    !identical(as.character(snapshot_priority), snapshot$priority) ||
    any(manifest_priority < 0L) || any(snapshot_priority < 0L)) {
  stop("GitHub manifests contain malformed priorities", call. = FALSE)
}
manifest$priority <- manifest_priority
snapshot$priority <- snapshot_priority
selected <- manifest[manifest$action == "clone" &
  manifest$relation %in% c("Depends", "Imports", "Suggests") &
  manifest$priority <= max_priority, , drop = FALSE]
dependency_selected <- selected
if (!is.null(options$repositories)) {
  requested <- strsplit(options$repositories, ",", fixed = TRUE)[[1L]]
  if (!length(requested) || any(!grepl(
      "^[A-Za-z0-9][A-Za-z0-9._-]*$", requested)) || anyDuplicated(requested)) {
    stop("--repositories must be a unique comma-separated safe-name list",
      call. = FALSE)
  }
  absent <- setdiff(requested, selected$repository)
  if (length(absent)) {
    stop("requested repositories are outside the selected priority scope: ",
      paste(absent, collapse = ", "), call. = FALSE)
  }
  selected <- selected[selected$repository %in% requested, , drop = FALSE]
}
if (!nrow(selected)) stop("repository selection is empty", call. = FALSE)
snapshot_index <- match(selected$repository, snapshot$repository)
if (anyNA(snapshot_index)) {
  stop("selected repositories are absent from the pinned snapshot", call. = FALSE)
}
selected_snapshot <- snapshot[snapshot_index, , drop = FALSE]
if (!identical(selected$repository, selected_snapshot$repository) ||
    !identical(selected$url, selected_snapshot$url) ||
    !identical(selected$priority, selected_snapshot$priority) ||
    any(!grepl("^([0-9a-f]{40}|[0-9a-f]{64})$",
      selected_snapshot$commit))) {
  stop("GitHub snapshot disagrees with the reviewed consumer selection",
    call. = FALSE)
}
dependency_snapshot_index <- match(dependency_selected$repository,
  snapshot$repository)
if (anyNA(dependency_snapshot_index)) {
  stop("dependency-scope repositories are absent from the pinned snapshot",
    call. = FALSE)
}
dependency_snapshot <- snapshot[dependency_snapshot_index, , drop = FALSE]
if (!identical(dependency_selected$repository, dependency_snapshot$repository) ||
    !identical(dependency_selected$url, dependency_snapshot$url) ||
    !identical(dependency_selected$priority, dependency_snapshot$priority)) {
  stop("dependency scope disagrees with the reviewed snapshot", call. = FALSE)
}

consumer_root <- normalizePath(file.path(root, ".local", "compat", "github"),
  winslash = "/", mustWork = TRUE)
selection_rows <- lapply(seq_len(nrow(selected)), function(row_index) {
  checkout <- repository_runner_require_directory(file.path(consumer_root,
    selected$repository[[row_index]]), "consumer checkout")
  tree <- repository_runner_git_value(expected_git, checkout,
    c("rev-parse", "--verify", paste0(selected_snapshot$commit[[row_index]],
      "^{tree}")), "resolving pinned consumer tree")
  data.frame(position = as.character(row_index),
    repository = selected$repository[[row_index]],
    priority = as.character(selected$priority[[row_index]]),
    origin = selected_snapshot$url[[row_index]],
    commit = selected_snapshot$commit[[row_index]], tree = tree,
    stringsAsFactors = FALSE)
})
selection <- repository_runner_selection(do.call(rbind, selection_rows))

selection_config_stub <- list(git = expected_git, consumer_root = consumer_root)
for (row_index in seq_len(nrow(selection))) {
  repository_runner_authenticate_consumer(selection_config_stub,
    selection[row_index, , drop = FALSE])
}

# The dependency-preparation stage is immutable prerequisite evidence.  Its
# declared terminal content hash feeds the candidate receipt and the start
# boundary below; plan mode never recomputes the live dependency tree.
dependency_stage <- normalizePath(file.path(run_directory, sprintf(
  "repository-dependencies-priority-%d", max_priority)), winslash = "/",
  mustWork = TRUE)
if (!identical(dirname(dependency_stage), run_directory)) {
  stop("dependency evidence escaped the candidate run directory", call. = FALSE)
}
dependency_evidence <- repository_verify_evidence(dependency_stage)
dependency_metadata <- repository_runner_require_directory(file.path(
  dependency_stage, "metadata"), "dependency evidence metadata")
dependency_run_fields <- c(
  "schema", "stage_kind", "run_id", "started_utc", "root", "max_priority",
  "dependency_library", "dependency_library_content_before", "r",
  "r_version", "github_manifest_sha256", "github_snapshot_sha256",
  "harness_sha256", "fingerprint_sha256", "evidence_helper_sha256",
  "evidence_verifier_sha256", "checkout_preflight_sha256",
  "fallback_checkout_preflight_sha256", "result_ledger"
)
dependency_completion_fields <- c(
  "schema", "stage_kind", "run_id", "max_priority", "finished_utc", "status",
  "result_rows", "failed_rows", "result_sha256", "checkout_postflight_sha256",
  "fallback_checkout_postflight_sha256", "dependency_library_content_after"
)
dependency_run <- repository_runner_read_map(file.path(dependency_metadata,
  "run.tsv"), dependency_run_fields, "dependency run metadata")
dependency_completion <- repository_runner_read_map(file.path(
  dependency_metadata, "completion.tsv"), dependency_completion_fields,
  "dependency completion metadata")
dependency_result_path <- file.path(dependency_stage, sprintf(
  "dependency-install-priority-%d.tsv", max_priority))
dependency_result <- repository_runner_read_tsv(dependency_result_path,
  c("run_id", "repository", "commit", "priority", "status",
    "elapsed_seconds", "local_sources", "error"),
  label = "dependency result ledger")
dependency_harness <- file.path(root, "compat",
  "install-repository-test-dependencies.R")
evidence_verifier <- file.path(root, "compat",
  "verify-repository-evidence.R")
expected_dependency_run <- c(
  schema = "3", stage_kind = "repository_dependencies", run_id = run_id,
  root = root, max_priority = as.character(max_priority),
  dependency_library = dependency_library,
  r = normalizePath(file.path(R.home(), "bin", "R"), winslash = "/",
    mustWork = TRUE),
  r_version = as.character(getRversion()),
  github_manifest_sha256 = repository_runner_sha256(manifest_path),
  github_snapshot_sha256 = repository_runner_sha256(snapshot_path),
  harness_sha256 = repository_runner_sha256(dependency_harness),
  fingerprint_sha256 = repository_runner_sha256(source_paths[["fingerprint"]]),
  evidence_helper_sha256 = repository_runner_sha256(source_paths[["evidence"]]),
  evidence_verifier_sha256 = repository_runner_sha256(evidence_verifier),
  result_ledger = dependency_result_path
)
expected_dependency_completion <- c(
  schema = "3", stage_kind = "repository_dependencies", run_id = run_id,
  max_priority = as.character(max_priority), status = "passed",
  result_rows = as.character(nrow(dependency_selected)), failed_rows = "0",
  result_sha256 = repository_runner_sha256(dependency_result_path),
  dependency_library_content_after =
    candidate_provenance[["dependency_library_content_sha256"]]
)
dependency_checkout_files <- c(
  checkout_preflight_sha256 = file.path(dependency_metadata,
    "checkout-preflight.tsv"),
  fallback_checkout_preflight_sha256 = file.path(dependency_metadata,
    "fallback-checkout-preflight.tsv"),
  checkout_postflight_sha256 = file.path(dependency_metadata,
    "checkout-postflight.tsv"),
  fallback_checkout_postflight_sha256 = file.path(dependency_metadata,
    "fallback-checkout-postflight.tsv")
)
dependency_checkout_files <- vapply(seq_along(dependency_checkout_files),
  function(file_index) repository_runner_require_file(
    dependency_checkout_files[[file_index]],
    paste0("dependency ", names(dependency_checkout_files)[[file_index]])),
  character(1L))
names(dependency_checkout_files) <- c(
  "checkout_preflight_sha256", "fallback_checkout_preflight_sha256",
  "checkout_postflight_sha256", "fallback_checkout_postflight_sha256"
)
dependency_checkout_hashes <- repository_runner_sha256(
  dependency_checkout_files)
names(dependency_checkout_hashes) <- names(dependency_checkout_files)
dependency_copied_inputs <- c(
  "github-repositories.tsv" = manifest_path,
  "github-snapshot.tsv" = snapshot_path,
  "install-repository-test-dependencies.R" = dependency_harness,
  "fingerprint.R" = source_paths[["fingerprint"]],
  "repository-evidence.R" = source_paths[["evidence"]],
  "resource-jobs" = source_paths[["resource_jobs"]],
  "verify-repository-evidence.R" = evidence_verifier
)
copied_dependency_paths <- file.path(dependency_metadata,
  names(dependency_copied_inputs))
copied_dependency_paths <- vapply(seq_along(copied_dependency_paths),
  function(file_index) repository_runner_require_file(
    copied_dependency_paths[[file_index]], "copied dependency evidence input"),
  character(1L))
if (!identical(unname(dependency_run[names(expected_dependency_run)]),
      unname(expected_dependency_run)) ||
    !identical(unname(dependency_completion[names(expected_dependency_completion)]),
      unname(expected_dependency_completion)) ||
    !grepl("^[0-9a-f]{64}$",
      dependency_run[["dependency_library_content_before"]]) ||
    !identical(dependency_result$run_id,
      rep(run_id, nrow(dependency_selected))) ||
    !identical(dependency_result$repository, dependency_selected$repository) ||
    !identical(dependency_result$commit, dependency_snapshot$commit) ||
    !identical(dependency_result$priority,
      as.character(dependency_selected$priority)) ||
    any(dependency_result$status != "passed") ||
    !identical(unname(dependency_run[names(dependency_checkout_hashes)[1:2]]),
      unname(dependency_checkout_hashes[1:2])) ||
    !identical(unname(dependency_completion[
      names(dependency_checkout_hashes)[3:4]]),
      unname(dependency_checkout_hashes[3:4])) ||
    !identical(unname(repository_runner_sha256(copied_dependency_paths)),
      unname(repository_runner_sha256(dependency_copied_inputs)))) {
  stop("dependency evidence does not cover the exact selected consumers",
    call. = FALSE)
}

extra_text <- Sys.getenv("PARADOX_CONSUMER_EXTRA_LIBS", unset = "")
extra_libraries <- if (nzchar(extra_text)) {
  extra_values <- strsplit(extra_text, .Platform$path.sep, fixed = TRUE)[[1L]]
  if (!length(extra_values) || any(!nzchar(extra_values))) {
    stop("PARADOX_CONSUMER_EXTRA_LIBS contains an empty path", call. = FALSE)
  }
  unname(vapply(extra_values, normalizePath, character(1L), winslash = "/",
    mustWork = TRUE))
} else character()
if (anyDuplicated(c(candidate_library, extra_libraries, dependency_library))) {
  stop("candidate, extra, and dependency libraries must be distinct",
    call. = FALSE)
}

compat_environment <- compat_system_child_environment()
compat_active <- nzchar(Sys.getenv("PARADOX_COMPAT_SYSTEM_ACTIVE_ROOT",
  unset = ""))
compat_tool_files <- character()
compat_bootstrap <- NULL
if (compat_active) {
  compat_bootstrap <- file.path(root, "scripts", "bootstrap-compat-system")
  compat_inputs <- c(
    "compat-system-receipt.tsv" = file.path(root, ".local", "receipts",
      "compat-system", "receipt.tsv"),
    "compat-system-receipt.sha256" = file.path(root, ".local", "receipts",
      "compat-system", "receipt.sha256"),
    "compat-system-geo-linux-64.lock" = file.path(root, "environment",
      "compat-system-geo-linux-64.lock"),
    "compat-system-p1-linux-64.lock" = file.path(root, "environment",
      "compat-system-p1-linux-64.lock"),
    "bootstrap-compat-system" = compat_bootstrap,
    "activate-compat-system" = file.path(root, "scripts",
      "activate-compat-system"),
    "compat-system-Makevars" = file.path(root, ".local", "compat", "system",
      "Makevars")
  )
  compat_tool_files <- vapply(seq_along(compat_inputs), function(input_index) {
    repository_runner_require_file(compat_inputs[[input_index]],
      paste0("compatibility-system input ", names(compat_inputs)[[input_index]]))
  }, character(1L))
  names(compat_tool_files) <- names(compat_inputs)
}

stage <- file.path(run_directory, sprintf("repository-tests-priority-%d",
  max_priority))
tool_files <- c(
  "repository-runner.R" = source_paths[["runner"]],
  "repository-test-child.R" = source_paths[["child"]],
  "repository-wave-worker.R" = file.path(root, "compat",
    "repository-wave-worker.R"),
  "fingerprint.R" = source_paths[["fingerprint"]],
  "test-repositories-resumable.R" = file.path(root, "compat",
    "test-repositories-resumable.R"),
  "test-repositories.R" = file.path(root, "compat", "test-repositories.R"),
  "repository-evidence.R" = source_paths[["evidence"]],
  "verify-repository-evidence.R" = evidence_verifier,
  "compat-system-evidence.R" = source_paths[["compat_system"]],
  "github-repositories.tsv" = manifest_path,
  "github-snapshot.tsv" = snapshot_path,
  "install-repository-test-dependencies.R" = dependency_harness,
  "install-candidate" = installer_path,
  "authenticate-candidate-git" = authenticator_path,
  "candidate-provenance.tsv" = candidate_provenance_path,
  "candidate-provenance.sha256" = candidate_provenance_seal,
  "candidate-content-sentinel" = candidate_sentinel,
  "dependency-evidence-manifest.tsv" = dependency_evidence$manifest,
  "dependency-evidence-completion.seal" = dependency_evidence$seal,
  "dependency-run.tsv" = file.path(dependency_metadata, "run.tsv"),
  "dependency-completion.tsv" = file.path(dependency_metadata,
    "completion.tsv"),
  "dependency-result.tsv" = dependency_result_path,
  compat_tool_files
)
config <- list(
  root = root, stage = stage, run_id = run_id, consumer_root = consumer_root,
  timeout_seconds = timeout_seconds, candidate_origin = candidate_origin,
  candidate_ref = candidate_ref, candidate_commit = candidate_commit,
  candidate_tree = candidate_tree, candidate_source = candidate_source,
  candidate_version = candidate_version, candidate_library = candidate_library,
  candidate_package = candidate_package,
  candidate_content_sha256 = candidate_content_sha256,
  candidate_provenance_sha256 = candidate_provenance_sha256,
  candidate_installer_archive_sha256 =
    candidate_provenance[["source_archive_sha256"]],
  dependency_library = dependency_library,
  dependency_content_sha256 =
    candidate_provenance[["dependency_library_content_sha256"]],
  extra_libraries = extra_libraries, git = expected_git,
  rscript = expected_rscript, tool_files = tool_files,
  base_environment = compat_environment
)
config <- repository_runner_assert_config(config)

if (options$plan_only) {
  resource_plan <- repository_runner_resource_decision(config, operator_jobs)
  plan <- data.frame(
    run_id = run_id, stage = stage, stage_exists = dir.exists(stage),
    position = selection$position, repository = selection$repository,
    priority = selection$priority, origin = selection$origin,
    commit = selection$commit, tree = selection$tree,
    candidate_source = candidate_source, candidate_ref = candidate_ref,
    candidate_commit = candidate_commit, candidate_tree = candidate_tree,
    candidate_content_sha256 = candidate_content_sha256,
    candidate_authentication_sha256 = candidate_authentication$output_sha256,
    dependency_stage_manifest_sha256 = dependency_evidence$manifest_sha256,
    protected_full_content_hashes = "0", row_resume = "accepted-row",
    cache_scope = "row-local-disposable", compat_system_active = compat_active,
    automatic_consumer_jobs = resource_plan$automatic_jobs,
    effective_consumer_jobs = resource_plan$jobs,
    operator_consumer_jobs = if (is.null(operator_jobs)) "none" else operator_jobs,
    stringsAsFactors = FALSE
  )
  utils::write.table(plan, stdout(), quote = FALSE, sep = "\t",
    row.names = FALSE)
  quit(save = "no", status = 0L, runLast = FALSE)
}

if (options$verify) {
  result <- repository_runner_verify_stage(config, repository_verify_evidence)
  cat("repository_evidence=passed\n",
    "rows=", length(result$rows), "\n",
    "stage_manifest_sha256=", result$stage_evidence$manifest_sha256, "\n",
    sep = "")
  quit(save = "no", status = 0L, runLast = FALSE)
}

verify_compat_boundary <- function(label) {
  if (!compat_active) return(invisible(TRUE))
  result <- processx::run(compat_bootstrap, "--verify", stdout = "|",
    stderr_to_stdout = TRUE, error_on_status = FALSE, cleanup_tree = TRUE,
    timeout = 21600)
  if (!identical(result$status, 0L)) {
    stop("compatibility-system verification failed at ", label, ": ",
      paste(utils::tail(strsplit(result$stdout, "\n", fixed = TRUE)[[1L]], 40L),
        collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

new_stage <- !dir.exists(stage)
if (new_stage) verify_compat_boundary("repository stage start")
context <- repository_runner_initialize(config, selection,
  compat_tree_content_sha256)
if (file.exists(file.path(context$stage, "metadata", "completion.seal"))) {
  result <- repository_runner_verify_stage(config, repository_verify_evidence)
  statuses <- vapply(result$rows, function(value) value$summary[["status"]],
    character(1L))
  cat("repository_evidence=already_completed\n",
    "rows=", length(statuses), "\n",
    "reused_rows=", length(statuses), "\n",
    "failed_rows=", sum(statuses %in% c("failed", "timed_out")), "\n",
    "stage_manifest_sha256=", result$stage_evidence$manifest_sha256, "\n",
    sep = "")
  quit(save = "no",
    status = if (any(statuses %in% c("failed", "timed_out"))) 1L else 0L,
    runLast = FALSE)
}
run_active_stage <- function() {
  lock <- repository_runner_parent_lock_acquire(context)
  on.exit(repository_runner_parent_lock_release(lock), add = TRUE)
  row_outcomes <- repository_runner_run_rows_bounded(context, selection,
    operator_max = operator_jobs)
  verify_compat_boundary("repository stage final boundary")
  result <- repository_runner_complete(context, compat_tree_content_sha256,
    repository_seal_evidence, repository_verify_evidence)
  list(row_outcomes = row_outcomes, result = result)
}
active <- run_active_stage()
row_outcomes <- active$row_outcomes
result <- active$result
statuses <- vapply(result$rows, function(value) value$summary[["status"]],
  character(1L))
cat("repository_evidence=completed\n",
  "rows=", length(statuses), "\n",
  "reused_rows=", sum(row_outcomes == "reused"), "\n",
  "failed_rows=", sum(statuses %in% c("failed", "timed_out")), "\n",
  "stage_manifest_sha256=", result$stage_evidence$manifest_sha256, "\n",
  sep = "")
quit(save = "no", status = if (any(statuses %in% c("failed", "timed_out"))) 1L else 0L,
  runLast = FALSE)
