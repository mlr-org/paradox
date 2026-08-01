# Candidate-neutral, resumable repository compatibility gate.  This file is
# sourced by test-repositories.R by default; the old schema-3 implementation is
# retained there only for historical evidence replay.

arguments <- commandArgs(trailingOnly = TRUE)
usage <- paste(
  "usage: test-repositories.R",
  "[ROOT [MAX_PRIORITY [CANDIDATE_LIBRARY [DEPENDENCY_LIBRARY]]]]",
  "--run-id ID [--plan-only|--verify]",
  "[--candidate-source PATH] [--candidate-origin URL]",
  "[--repositories NAME[,NAME...]] [--timeout-seconds N] [--jobs N]",
  "[--evidence-profile NAME] [--paradox-axis paradox2|paradox1]"
)

positionals <- character()
seen_value_options <- character()
options <- list(
  run_id = NULL, plan_only = FALSE, verify = FALSE,
  candidate_source = NULL, candidate_origin = NULL,
  repositories = NULL, timeout_seconds = 3600, jobs = NULL,
  evidence_profile = "default", paradox_axis = "paradox2"
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
      "repositories", "timeout-seconds", "jobs", "evidence-profile",
      "paradox-axis")) {
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
evidence_profile <- options$evidence_profile
if (length(evidence_profile) != 1L || is.na(evidence_profile) ||
    !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$", evidence_profile) ||
    evidence_profile %in% c(".", "..")) {
  stop("--evidence-profile must be one safe name of at most 64 characters",
    call. = FALSE)
}
paradox_axis <- options$paradox_axis
if (!paradox_axis %in% c("paradox2", "paradox1")) {
  stop("--paradox-axis must be paradox2 or paradox1", call. = FALSE)
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
  resource_jobs = file.path(root, "scripts", "environment", "resource-jobs"),
  profile = file.path(root, "compat", "downstream-evidence-profile.R")
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
sys.source(source_paths[["profile"]], envir = environment(), keep.source = FALSE)

profile <- downstream_evidence_profile(root, evidence_profile, paradox_axis)

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
candidate_major <- strsplit(candidate_version, ".", fixed = TRUE)[[1L]][[1L]]
if (!identical(candidate_major, profile$version_major)) {
  stop("installed paradox version does not match --paradox-axis", call. = FALSE)
}
downstream_evidence_assert_candidate(profile, candidate_ref, candidate_commit,
  candidate_tree, candidate_version)

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

profile_registry_path <- profile$registry
manifest_path <- profile$repository_manifest
snapshot_path <- profile$snapshot
dependency_manifest_path <- profile$dependency_repository_manifest
dependency_snapshot_path <- profile$dependency_snapshot
manifest <- repository_runner_read_tsv(manifest_path,
  c("repository", "url", "relation", "priority", "action", "notes"),
  label = "GitHub repository manifest")
snapshot <- repository_runner_read_tsv(snapshot_path,
  c("repository", "url", "priority", "commit", "commit_date", "branch"),
  label = "GitHub repository snapshot")
dependency_manifest <- repository_runner_read_tsv(dependency_manifest_path,
  c("repository", "url", "relation", "priority", "action", "notes"),
  label = "dependency GitHub repository manifest")
dependency_snapshot_all <- repository_runner_read_tsv(dependency_snapshot_path,
  c("repository", "url", "priority", "commit", "commit_date", "branch"),
  label = "dependency GitHub repository snapshot")
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
dependency_priority <- suppressWarnings(as.integer(dependency_manifest$priority))
dependency_snapshot_priority <- suppressWarnings(as.integer(
  dependency_snapshot_all$priority))
if (anyDuplicated(dependency_manifest$repository) ||
    anyDuplicated(dependency_snapshot_all$repository) ||
    anyNA(dependency_priority) || anyNA(dependency_snapshot_priority) ||
    !identical(as.character(dependency_priority), dependency_manifest$priority) ||
    !identical(as.character(dependency_snapshot_priority),
      dependency_snapshot_all$priority) ||
    any(dependency_priority < 0L) || any(dependency_snapshot_priority < 0L)) {
  stop("dependency GitHub manifest contains malformed priorities",
    call. = FALSE)
}
dependency_manifest$priority <- dependency_priority
dependency_snapshot_all$priority <- dependency_snapshot_priority
dependency_selected <- downstream_evidence_dependency_selection(
  dependency_manifest,
  max_priority
)
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
# Positions are assigned after a heaviest-first sort so the longest checks
# launch first and continuous refill packs the tail; the hint table is
# scheduling data only and an unhinted repository keeps its reviewed
# inventory position after the hinted rows.
heavy_order <- repository_runner_heaviest_first_order(selected$repository)
selected <- selected[heavy_order, , drop = FALSE]
selected_snapshot <- selected_snapshot[heavy_order, , drop = FALSE]

consumer_root <- normalizePath(profile$consumer_root,
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
  "repository-dependencies-priority-%d%s", max_priority,
  profile$profile_suffix)), winslash = "/",
  mustWork = TRUE)
if (!identical(dirname(dependency_stage), run_directory)) {
  stop("dependency evidence escaped the candidate run directory", call. = FALSE)
}
dependency_evidence <- repository_verify_evidence(dependency_stage)
dependency_metadata <- repository_runner_require_directory(file.path(
  dependency_stage, "metadata"), "dependency evidence metadata")
dependency_run_path <- file.path(dependency_metadata, "run.tsv")
dependency_probe <- repository_runner_read_tsv(
  dependency_run_path, c("field", "value"), label = "dependency run schema probe"
)
dependency_schema_index <- match("schema", dependency_probe$field)
if (is.na(dependency_schema_index)) {
  stop("dependency run metadata omits its schema", call. = FALSE)
}
dependency_schema <- dependency_probe$value[[dependency_schema_index]]
legacy_dependency <- identical(dependency_schema, "3") &&
  identical(evidence_profile, "default")
deterministic_provider_dependency <- identical(dependency_schema, "5")
if (!legacy_dependency &&
    !(dependency_schema %in% c("4", "5"))) {
  stop("dependency evidence uses an unsupported profile schema", call. = FALSE)
}
if (legacy_dependency) {
  dependency_selected <- dependency_manifest[
    dependency_manifest$action == "clone" &
      dependency_manifest$relation %in% c("Depends", "Imports", "Suggests") &
      dependency_manifest$priority <= max_priority,
    ,
    drop = FALSE
  ]
}
dependency_snapshot_index <- match(dependency_selected$repository,
  dependency_snapshot_all$repository)
if (anyNA(dependency_snapshot_index)) {
  stop("dependency-scope repositories are absent from the pinned snapshot",
    call. = FALSE)
}
dependency_snapshot <- dependency_snapshot_all[dependency_snapshot_index, , drop = FALSE]
if (!identical(dependency_selected$repository, dependency_snapshot$repository) ||
    !identical(dependency_selected$url, dependency_snapshot$url) ||
    !identical(dependency_selected$priority, dependency_snapshot$priority) ||
    any(!grepl("^([0-9a-f]{40}|[0-9a-f]{64})$",
      dependency_snapshot$commit))) {
  stop("dependency scope disagrees with the reviewed snapshot", call. = FALSE)
}
if (legacy_dependency) {
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
} else {
  dependency_run_fields <- c(
    "schema", "stage_kind", "run_id", "evidence_profile", "started_utc",
    "root", "max_priority", "dependency_library",
    "dependency_library_content_before", "r", "r_version",
    "profile_registry_sha256", "axis_registry_sha256", "profile_helper_sha256",
    "github_manifest_sha256", "github_snapshot_sha256", "harness_sha256",
    "fingerprint_sha256", "evidence_helper_sha256", "evidence_verifier_sha256",
    "repository_runner_sha256", "checkout_preflight_sha256",
    "fallback_checkout_preflight_sha256", "exact_provider_sources_sha256",
    "result_ledger"
  )
  dependency_completion_fields <- c(
    "schema", "stage_kind", "run_id", "evidence_profile", "max_priority",
    "finished_utc", "status", "result_rows", "failed_rows", "result_sha256",
    "checkout_postflight_sha256", "fallback_checkout_postflight_sha256",
    "dependency_library_content_after"
  )
}
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
  schema = dependency_schema, stage_kind = "repository_dependencies", run_id = run_id,
  root = root, max_priority = as.character(max_priority),
  dependency_library = dependency_library,
  r = normalizePath(file.path(R.home(), "bin", "R"), winslash = "/",
    mustWork = TRUE),
  r_version = as.character(getRversion()),
  github_manifest_sha256 = repository_runner_sha256(dependency_manifest_path),
  github_snapshot_sha256 = repository_runner_sha256(dependency_snapshot_path),
  harness_sha256 = repository_runner_sha256(dependency_harness),
  fingerprint_sha256 = repository_runner_sha256(source_paths[["fingerprint"]]),
  evidence_helper_sha256 = repository_runner_sha256(source_paths[["evidence"]]),
  evidence_verifier_sha256 = repository_runner_sha256(evidence_verifier),
  result_ledger = dependency_result_path
)
if (!legacy_dependency) {
  expected_dependency_run <- append(
    expected_dependency_run,
    c(evidence_profile = evidence_profile), after = 3L
  )
  profile_fields <- c(
    profile_registry_sha256 = repository_runner_sha256(profile_registry_path),
    axis_registry_sha256 = repository_runner_sha256(profile$axis_registry),
    profile_helper_sha256 = repository_runner_sha256(source_paths[["profile"]])
  )
  expected_dependency_run <- append(expected_dependency_run, profile_fields,
    after = match("r_version", names(expected_dependency_run)))
  expected_dependency_run <- append(
    expected_dependency_run,
    c(repository_runner_sha256 =
      repository_runner_sha256(source_paths[["runner"]])),
    after = match("evidence_verifier_sha256", names(expected_dependency_run))
  )
}
expected_dependency_completion <- c(
  schema = dependency_schema, stage_kind = "repository_dependencies", run_id = run_id,
  max_priority = as.character(max_priority), status = "passed",
  result_rows = as.character(nrow(dependency_selected)), failed_rows = "0",
  result_sha256 = repository_runner_sha256(dependency_result_path),
  dependency_library_content_after =
    candidate_provenance[["dependency_library_content_sha256"]]
)
if (!legacy_dependency) {
  expected_dependency_completion <- append(
    expected_dependency_completion,
    c(evidence_profile = evidence_profile), after = 3L
  )
}
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
dependency_fallback_preflight <- repository_runner_read_tsv(
  dependency_checkout_files[["fallback_checkout_preflight_sha256"]],
  c(
    "package", "repository", "checkout", "expected_commit", "observed_commit",
    "expected_origin", "observed_origin", "clean", "status", "valid"
  ),
  label = "dependency fallback checkout preflight"
)
provider_rows <- integer()
expected_provider_sources <- character()
exact_provider_hashes_match <- TRUE
if (!legacy_dependency) {
  provider_rows <- which(dependency_selected$relation == "ExactDependency")
  provider_preflight_index <- match(
    dependency_selected$repository[provider_rows],
    dependency_fallback_preflight$repository
  )
  if (anyNA(provider_preflight_index)) {
    stop("exact dependency provider is absent from its retained preflight",
      call. = FALSE)
  }
  expected_provider_sources <- paste0(
    dependency_fallback_preflight$package[provider_preflight_index],
    "@",
    dependency_snapshot$commit[provider_rows]
  )
  exact_provider_sources_path <- repository_runner_require_file(
    file.path(dependency_metadata, "exact-provider-sources.tsv"),
    "exact dependency provider source ledger"
  )
  exact_provider_sources <- repository_runner_read_tsv(
    exact_provider_sources_path,
    c(
      "repository", "package", "commit", "tree", "archive_sha256",
      "tree_manifest_sha256", "source",
      if (deterministic_provider_dependency) "install_source"
    ),
    allow_empty = TRUE,
    label = "exact dependency provider source ledger"
  )
  exact_provider_installs_path <- repository_runner_require_file(
    file.path(dependency_metadata, "exact-provider-installs.tsv"),
    "exact dependency provider installation ledger"
  )
  if (deterministic_provider_dependency) {
    exact_provider_installs <- repository_runner_read_tsv(
      exact_provider_installs_path,
      c(
        "repository", "package", "version", "install_method", "commit",
        "tree", "archive_sha256", "built_timestamp",
        "install_source", "initial_content_sha256", "final_content_sha256"
      ),
      allow_empty = TRUE,
      label = "deterministic exact dependency provider installation ledger"
    )
    if (!identical(
        exact_provider_sources$repository,
        dependency_selected$repository[provider_rows]
      ) ||
        !identical(
          exact_provider_sources$package,
          dependency_fallback_preflight$package[provider_preflight_index]
        ) ||
        !identical(
          exact_provider_sources$commit,
          dependency_snapshot$commit[provider_rows]
        ) ||
        any(!grepl("^([0-9a-f]{40}|[0-9a-f]{64})$",
          exact_provider_sources$tree)) ||
        any(!grepl("^[0-9a-f]{64}$", c(
          exact_provider_sources$archive_sha256,
          exact_provider_sources$tree_manifest_sha256
        ))) ||
        !identical(exact_provider_installs$repository,
          exact_provider_sources$repository) ||
        !identical(exact_provider_installs$package,
          exact_provider_sources$package) ||
        any(exact_provider_installs$install_method !=
          "r_cmd_install_built_timestamp") ||
        !identical(exact_provider_installs$commit,
          exact_provider_sources$commit) ||
        !identical(exact_provider_installs$tree,
          exact_provider_sources$tree) ||
        !identical(exact_provider_installs$archive_sha256,
          exact_provider_sources$archive_sha256) ||
        !identical(exact_provider_installs$install_source,
          exact_provider_sources$install_source) ||
        any(!grepl("^[0-9a-f]{64}$", c(
          exact_provider_installs$initial_content_sha256,
          exact_provider_installs$final_content_sha256
        ))) ||
        !identical(exact_provider_installs$initial_content_sha256,
          exact_provider_installs$final_content_sha256)) {
      stop("deterministic exact dependency provider evidence is incomplete or malformed",
        call. = FALSE)
    }
  } else {
    exact_provider_installs <- repository_runner_read_tsv(
      exact_provider_installs_path,
      c(
        "repository", "package", "version", "remote_type", "remote_pkg_ref",
        "initial_content_sha256", "final_content_sha256"
      ),
      allow_empty = TRUE,
      label = "exact dependency provider installation ledger"
    )
    if (!identical(
        exact_provider_sources$repository,
        dependency_selected$repository[provider_rows]
      ) ||
        !identical(
          exact_provider_sources$package,
          dependency_fallback_preflight$package[provider_preflight_index]
        ) ||
        !identical(
          exact_provider_sources$commit,
          dependency_snapshot$commit[provider_rows]
        ) ||
        any(!grepl("^([0-9a-f]{40}|[0-9a-f]{64})$",
          exact_provider_sources$tree)) ||
        any(!grepl("^[0-9a-f]{64}$", c(
          exact_provider_sources$archive_sha256,
          exact_provider_sources$tree_manifest_sha256
        ))) ||
        !identical(exact_provider_installs$repository,
          exact_provider_sources$repository) ||
        !identical(exact_provider_installs$package,
          exact_provider_sources$package) ||
        any(exact_provider_installs$remote_type != "local") ||
        any(!grepl("^[0-9a-f]{64}$", c(
          exact_provider_installs$initial_content_sha256,
          exact_provider_installs$final_content_sha256
        ))) ||
        !identical(exact_provider_installs$initial_content_sha256,
          exact_provider_installs$final_content_sha256)) {
      stop("exact dependency provider evidence is incomplete or malformed",
        call. = FALSE)
    }
  }

  deterministic_built_timestamp <- function(value) {
    if (length(value) != 1L || is.na(value) ||
        !grepl(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(Z|[+-][0-9]{2}:[0-9]{2})$",
          value
        )) {
      stop("exact dependency provider commit date is not canonical",
        call. = FALSE)
    }
    local_text <- substr(value, 1L, 19L)
    local_time <- strptime(
      local_text,
      format = "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    )
    second <- suppressWarnings(as.integer(substr(local_text, 18L, 19L)))
    if (is.na(second) || second > 59L || is.na(local_time) || !identical(
        format(local_time, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        local_text)) {
      stop("exact dependency provider commit date is not a real calendar time",
        call. = FALSE)
    }
    offset_seconds <- 0
    offset_sign <- "+"
    if (!endsWith(value, "Z")) {
      size <- nchar(value)
      offset_sign <- substr(value, 20L, 20L)
      offset_hour <- suppressWarnings(
        as.integer(substr(value, size - 4L, size - 3L))
      )
      offset_minute <- suppressWarnings(
        as.integer(substr(value, size - 1L, size))
      )
      if (is.na(offset_hour) || is.na(offset_minute) ||
          offset_hour > 23L || offset_minute > 59L) {
        stop("exact dependency provider commit date has an invalid offset",
          call. = FALSE)
      }
      offset_seconds <- offset_hour * 3600 + offset_minute * 60
    }
    instant <- as.POSIXct(local_time, tz = "UTC")
    if (identical(offset_sign, "+")) {
      instant <- instant - offset_seconds
    } else {
      instant <- instant + offset_seconds
    }
    if (length(instant) != 1L || is.na(instant)) {
      stop("exact dependency provider commit date cannot be parsed",
        call. = FALSE)
    }
    format(instant, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  }

  installed_tree_contains <- function(path, needles) {
    needles <- unique(needles[nzchar(needles)])
    if (!length(needles)) return(FALSE)
    patterns <- lapply(enc2utf8(needles), charToRaw)
    overlap <- max(vapply(patterns, length, integer(1L))) - 1L
    entries <- repository_runner_entries(
      path,
      "installed exact dependency provider",
      allow_symlinks = FALSE
    )
    files <- entries$absolute[entries$type == "file"]
    if (!length(files)) return(FALSE)
    for (file in files) {
      connection <- file(file, open = "rb")
      on.exit(close(connection), add = TRUE)
      carry <- raw()
      repeat {
        chunk <- readBin(connection, what = "raw", n = 1024L * 1024L)
        if (!length(chunk)) break
        bytes <- c(carry, chunk)
        if (any(vapply(patterns, function(pattern) {
          length(grepRaw(pattern, bytes, fixed = TRUE)) != 0L
        }, logical(1L)))) {
          close(connection)
          on.exit(NULL, add = FALSE)
          return(TRUE)
        }
        carry <- if (overlap > 0L && length(bytes) > overlap) {
          tail(bytes, overlap)
        } else {
          bytes
        }
      }
      close(connection)
      on.exit(NULL, add = FALSE)
    }
    FALSE
  }
  provider_directory <- repository_runner_require_directory(
    file.path(dependency_stage, "exact-provider-sources"),
    "exact dependency provider source directory"
  )
  for (provider_index in seq_len(nrow(exact_provider_sources))) {
    repository <- exact_provider_sources$repository[[provider_index]]
    selection_index <- provider_rows[[provider_index]]
    expected_source <- repository_runner_require_directory(
      file.path(provider_directory, repository, "source"),
      "exact dependency provider source"
    )
    archive <- repository_runner_require_file(
      file.path(provider_directory, paste0(repository, ".tar")),
      "exact dependency provider archive"
    )
    tree_manifest <- repository_runner_require_file(
      file.path(provider_directory, paste0(repository, "-tree.tsv")),
      "exact dependency provider tree manifest"
    )
    retained_tree <- repository_runner_read_tsv(
      tree_manifest,
      c("mode", "object", "path", "size", "sha256"),
      label = "exact dependency provider tree manifest"
    )
    authentication <- repository_runner_authenticate_consumer(
      list(git = expected_git,
        consumer_root = profile$dependency_consumer_root),
      data.frame(
        repository = repository,
        origin = dependency_snapshot$url[[selection_index]],
        commit = dependency_snapshot$commit[[selection_index]],
        tree = exact_provider_sources$tree[[provider_index]],
        stringsAsFactors = FALSE
      )
    )
    observed_tree <- repository_runner_validate_extraction(
      authentication,
      expected_source
    )
    expected_install_source <- NULL
    installed_source_tree <- NULL
    if (deterministic_provider_dependency) {
      canonical_provider_root_path <- file.path(
        root, ".local", "compat", "exact-provider-sources-v1"
      )
      canonical_provider_root <- repository_runner_require_directory(
        canonical_provider_root_path,
        "canonical exact dependency provider source root"
      )
      if (!identical(canonical_provider_root, canonical_provider_root_path)) {
        stop("canonical exact dependency provider source root is indirect",
          call. = FALSE)
      }
      canonical_generation_path <- file.path(
        canonical_provider_root,
        paste0(repository, "-", dependency_snapshot$commit[[selection_index]])
      )
      canonical_generation <- repository_runner_require_directory(
        canonical_generation_path,
        "canonical exact dependency provider generation"
      )
      if (!identical(canonical_generation, canonical_generation_path)) {
        stop("canonical exact dependency provider generation is indirect",
          call. = FALSE)
      }
      expected_install_source_path <- file.path(canonical_generation, "source")
      expected_install_source <- repository_runner_require_directory(
        expected_install_source_path,
        "canonical exact dependency provider installation source"
      )
      if (!identical(expected_install_source, expected_install_source_path)) {
        stop("canonical exact dependency provider installation source is indirect",
          call. = FALSE)
      }
      canonical_inventory <- list.files(
        canonical_generation,
        all.files = TRUE,
        full.names = FALSE,
        recursive = FALSE,
        no.. = TRUE
      )
      canonical_entries <- repository_runner_entries(
        canonical_generation,
        "canonical exact dependency provider generation",
        allow_symlinks = TRUE
      )
      nonsymbolic <- canonical_entries$type != "symlink"
      canonical_modes <- c(
        as.integer(file.info(
          canonical_generation,
          extra_cols = FALSE
        )$mode),
        strtoi(canonical_entries$mode[nonsymbolic], base = 8L)
      )
      regular_entries <- canonical_entries$type == "file"
      if (!identical(canonical_inventory, "source") ||
          anyNA(canonical_modes) ||
          any(bitwAnd(canonical_modes, 146L) != 0L) ||
          anyNA(canonical_entries$hard_links[regular_entries]) ||
          any(canonical_entries$hard_links[regular_entries] != 1)) {
        stop(
          "canonical exact dependency provider generation is writable, ",
          "aliased, or has an unexpected inventory",
          call. = FALSE
        )
      }
      installed_source_tree <- repository_runner_validate_extraction(
        authentication,
        expected_install_source
      )
    }
    reproduced_archive_sha256 <- local({
      path <- repository_runner_tempfile(
        "exact-provider-replay-", fileext = ".tar"
      )
      on.exit(unlink(path), add = TRUE)
      repository_runner_create_archive(authentication, path)$sha256
    })
    source_description <- read.dcf(
      repository_runner_require_file(
        file.path(expected_source, "DESCRIPTION"),
        "exact dependency provider source DESCRIPTION"
      ),
      fields = c("Package", "Version", "NeedsCompilation")
    )
    source_needs_compilation <- unname(
      source_description[[1L, "NeedsCompilation"]]
    )
    source_for_build_contract <- if (deterministic_provider_dependency) {
      expected_install_source
    } else {
      expected_source
    }
    source_src <- file.path(source_for_build_contract, "src")
    source_scripts <- file.path(
      source_for_build_contract,
      c("configure", "configure.win", "cleanup", "cleanup.win")
    )
    if (deterministic_provider_dependency &&
        ((!is.na(source_needs_compilation) &&
          !identical(tolower(source_needs_compilation), "no")) ||
        file.exists(source_src) || dir.exists(source_src) ||
        repository_runner_is_symbolic(source_src) ||
        any(file.exists(source_scripts)) ||
        any(dir.exists(source_scripts)) ||
        any(vapply(
          source_scripts,
          repository_runner_is_symbolic,
          logical(1L)
        )))) {
      stop(
        "deterministic exact dependency provider is not a pure-R package",
        call. = FALSE
      )
    }
    installed_path <- repository_runner_require_directory(
      file.path(dependency_library,
        exact_provider_installs$package[[provider_index]]),
      "installed exact dependency provider"
    )
    installed_description_path <- repository_runner_require_file(
      file.path(installed_path, "DESCRIPTION"),
      "installed exact dependency provider DESCRIPTION"
    )
    if (deterministic_provider_dependency) {
      installed_description <- read.dcf(installed_description_path)
      expected_built_timestamp <- deterministic_built_timestamp(
        dependency_snapshot$commit_date[[selection_index]]
      )
      expected_installed_description <- c(
        Package = unname(source_description[[1L, "Package"]]),
        Version = unname(source_description[[1L, "Version"]]),
        Built = paste0(
          "R ", as.character(getRversion()), "; ; ",
          expected_built_timestamp, "; ", .Platform$OS.type
        )
      )
      forbidden_fields <- c("RemoteType", "RemotePkgRef", "Packaged")
      if (any(forbidden_fields %in% colnames(installed_description)) ||
          !all(names(expected_installed_description) %in%
            colnames(installed_description)) ||
          !identical(exact_provider_sources$source[[provider_index]],
            expected_source) ||
          !identical(exact_provider_sources$install_source[[provider_index]],
            expected_install_source) ||
          !identical(repository_runner_sha256(archive),
            exact_provider_sources$archive_sha256[[provider_index]]) ||
          !identical(reproduced_archive_sha256,
            exact_provider_sources$archive_sha256[[provider_index]]) ||
          !identical(repository_runner_sha256(tree_manifest),
            exact_provider_sources$tree_manifest_sha256[[provider_index]]) ||
          !identical(retained_tree, observed_tree) ||
          !identical(installed_source_tree, observed_tree) ||
          !identical(exact_provider_installs$package[[provider_index]],
            expected_installed_description[["Package"]]) ||
          !identical(exact_provider_installs$version[[provider_index]],
            expected_installed_description[["Version"]]) ||
          !identical(exact_provider_installs$commit[[provider_index]],
            exact_provider_sources$commit[[provider_index]]) ||
          !identical(exact_provider_installs$tree[[provider_index]],
            exact_provider_sources$tree[[provider_index]]) ||
          !identical(exact_provider_installs$archive_sha256[[provider_index]],
            exact_provider_sources$archive_sha256[[provider_index]]) ||
          !identical(exact_provider_installs$install_source[[provider_index]],
            expected_install_source) ||
          !identical(exact_provider_installs$built_timestamp[[provider_index]],
            expected_built_timestamp) ||
          !identical(unname(installed_description[1L,
            names(expected_installed_description)]),
            unname(expected_installed_description)) ||
          installed_tree_contains(
            installed_path,
            c(dirname(dependency_stage), dependency_stage, expected_source)
          ) ||
          !identical(compat_tree_content_sha256(installed_path),
            exact_provider_installs$final_content_sha256[[provider_index]])) {
        stop("retained deterministic exact dependency provider evidence disagrees",
          call. = FALSE)
      }
    } else {
      installed_description <- read.dcf(
        installed_description_path,
        fields = c("Package", "Version", "RemoteType", "RemotePkgRef")
      )
      expected_installed_description <- c(
        Package = unname(source_description[[1L, "Package"]]),
        Version = unname(source_description[[1L, "Version"]]),
        RemoteType = "local",
        RemotePkgRef = paste0("local::", expected_source)
      )
      if (!identical(exact_provider_sources$source[[provider_index]],
            expected_source) ||
          !identical(repository_runner_sha256(archive),
            exact_provider_sources$archive_sha256[[provider_index]]) ||
          !identical(reproduced_archive_sha256,
            exact_provider_sources$archive_sha256[[provider_index]]) ||
          !identical(repository_runner_sha256(tree_manifest),
            exact_provider_sources$tree_manifest_sha256[[provider_index]]) ||
          !identical(retained_tree, observed_tree) ||
          !identical(exact_provider_installs$package[[provider_index]],
            expected_installed_description[["Package"]]) ||
          !identical(exact_provider_installs$version[[provider_index]],
            expected_installed_description[["Version"]]) ||
          !identical(exact_provider_installs$remote_pkg_ref[[provider_index]],
            expected_installed_description[["RemotePkgRef"]]) ||
          !identical(unname(installed_description[1L,
            names(expected_installed_description)]),
            unname(expected_installed_description)) ||
          !identical(compat_tree_content_sha256(installed_path),
            exact_provider_installs$final_content_sha256[[provider_index]])) {
        stop("retained exact dependency provider evidence disagrees",
          call. = FALSE)
      }
    }
  }
  exact_provider_hashes_match <- identical(
    dependency_run[["exact_provider_sources_sha256"]],
    repository_runner_sha256(exact_provider_sources_path)
  )
}
dependency_copied_inputs <- if (legacy_dependency) {
  c(dependency_manifest_path, dependency_snapshot_path, dependency_harness,
    source_paths[["fingerprint"]], source_paths[["evidence"]], evidence_verifier)
} else {
  c(profile_registry_path, profile$axis_registry, source_paths[["profile"]],
    dependency_manifest_path, dependency_snapshot_path,
    dependency_harness, source_paths[["fingerprint"]], source_paths[["evidence"]],
    evidence_verifier, source_paths[["runner"]])
}
names(dependency_copied_inputs) <- basename(dependency_copied_inputs)
if (anyDuplicated(names(dependency_copied_inputs))) {
  stop("dependency evidence input basenames collide", call. = FALSE)
}
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
    !exact_provider_hashes_match ||
    !identical(dependency_result$run_id,
      rep(run_id, nrow(dependency_selected))) ||
    !identical(dependency_result$repository, dependency_selected$repository) ||
    !identical(dependency_result$commit, dependency_snapshot$commit) ||
    !identical(dependency_result$priority,
      as.character(dependency_selected$priority)) ||
    any(dependency_result$status != "passed") ||
    !identical(
      dependency_result$local_sources[provider_rows],
      expected_provider_sources
    ) ||
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
bridge_installer <- file.path(root, "compat", "install-downstream-bridges")
bridge_library <- file.path(
  root, ".local", "compat", "runs", run_id,
  paste0("library-downstream-bridges", profile$suffix)
)
bridge_evidence <- file.path(
  root, ".local", "compat", "runs", run_id,
  paste0("downstream-bridges", profile$suffix)
)
if (!length(extra_libraries) || !identical(extra_libraries[[1L]], bridge_library)) {
  stop(
    "the candidate-specific downstream bridge library must be the first extra library: ",
    bridge_library,
    call. = FALSE
  )
}
bridge_inputs <- c(
  "install-downstream-bridges" = bridge_installer,
  "downstream-bridges-completion.tsv" = file.path(
    bridge_evidence, "metadata", "completion.tsv"
  ),
  "downstream-bridges-packages.tsv" = file.path(
    bridge_evidence, "metadata", "packages.tsv"
  ),
  "downstream-bridges-evidence-manifest.tsv" = file.path(
    bridge_evidence, "metadata", "evidence-manifest.tsv"
  ),
  "downstream-bridges-completion.seal" = file.path(
    bridge_evidence, "metadata", "completion.seal"
  )
)
bridge_inputs <- vapply(seq_along(bridge_inputs), function(index) {
  repository_runner_require_file(
    bridge_inputs[[index]], paste0("downstream bridge input ", names(bridge_inputs)[[index]])
  )
}, character(1L))
names(bridge_inputs) <- c(
  "install-downstream-bridges", "downstream-bridges-completion.tsv",
  "downstream-bridges-packages.tsv", "downstream-bridges-evidence-manifest.tsv",
  "downstream-bridges-completion.seal"
)

compat_environment <- c(
  repository_runner_base_child_environment(root,
    compat_system_child_environment()),
  PARADOX_EVIDENCE_PROFILE = evidence_profile,
  PARADOX_EVIDENCE_AXIS = paradox_axis
)
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

stage <- file.path(run_directory, sprintf("repository-tests-priority-%d%s",
  max_priority, profile$suffix))
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
  "resource-jobs" = source_paths[["resource_jobs"]],
  "downstream-evidence-profile.R" = source_paths[["profile"]],
  "downstream-evidence-profiles.tsv" = profile_registry_path,
  "paradox-evidence-axes.tsv" = profile$axis_registry,
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
  bridge_inputs,
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
    run_id = run_id, evidence_profile = evidence_profile,
    paradox_axis = paradox_axis, stage = stage, stage_exists = dir.exists(stage),
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

# This hook precedes the repository stage's first protected-library content
# boundary, so the bridge verifier performs its own candidate/dependency check.
bridge_verification <- processx::run(
  bridge_installer,
  c("--verify", "--candidate-source", candidate_source,
    "--evidence-profile", evidence_profile, "--paradox-axis", paradox_axis),
  stdout = "|", stderr_to_stdout = TRUE, error_on_status = FALSE,
  cleanup_tree = TRUE, timeout = 600
)
if (!identical(bridge_verification$status, 0L) ||
    !grepl("downstream_bridge_evidence=passed", bridge_verification$stdout,
      fixed = TRUE)) {
  stop(
    "candidate-specific downstream bridge verification failed: ",
    bridge_verification$stdout,
    call. = FALSE
  )
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
