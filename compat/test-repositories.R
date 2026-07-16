if (!identical(Sys.getenv("PARADOX_REPOSITORY_LEGACY_SCHEMA3", unset = ""),
    "true")) {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE),
    value = TRUE)
  if (length(file_argument) != 1L) {
    stop("could not resolve the repository test harness path", call. = FALSE)
  }
  harness <- normalizePath(sub("^--file=", "", file_argument), winslash = "/",
    mustWork = TRUE)
  sys.source(file.path(dirname(harness), "test-repositories-resumable.R"),
    envir = globalenv(), keep.source = TRUE)
  stop("resumable repository harness returned unexpectedly", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
usage <- paste(
  "usage: test-repositories.R",
  "[ROOT [MAX_PRIORITY [CANDIDATE_LIBRARY [DEPENDENCY_LIBRARY]]]]",
  "--run-id ID [--plan-only]"
)
positionals <- character()
run_id <- NULL
plan_only_argument <- FALSE
index <- 1L
while (index <= length(args)) {
  argument <- args[[index]]
  if (identical(argument, "--run-id")) {
    if (!is.null(run_id) || index == length(args)) stop(usage, call. = FALSE)
    run_id <- args[[index + 1L]]
    index <- index + 2L
  } else if (startsWith(argument, "--run-id=")) {
    if (!is.null(run_id)) stop(usage, call. = FALSE)
    run_id <- substring(argument, nchar("--run-id=") + 1L)
    index <- index + 1L
  } else if (identical(argument, "--plan-only")) {
    if (plan_only_argument) stop(usage, call. = FALSE)
    plan_only_argument <- TRUE
    index <- index + 1L
  } else if (startsWith(argument, "--")) {
    stop(usage, call. = FALSE)
  } else {
    positionals <- c(positionals, argument)
    index <- index + 1L
  }
}
if (length(positionals) > 4L || is.null(run_id)) stop(usage, call. = FALSE)
if (length(run_id) != 1L ||
    !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", run_id) ||
    run_id %in% c(".", "..")) {
  stop("--run-id must be a safe name of at most 128 characters", call. = FALSE)
}
candidate_run_id <- Sys.getenv("PARADOX_CANDIDATE_RUN_ID", unset = "")
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", candidate_run_id) ||
    candidate_run_id %in% c(".", "..") ||
    !identical(candidate_run_id, run_id)) {
  stop(
    "PARADOX_CANDIDATE_RUN_ID must equal the repository evidence --run-id",
    call. = FALSE
  )
}
root <- if (length(positionals) >= 1L) {
  normalizePath(positionals[[1L]], winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
max_priority <- if (length(positionals) >= 2L) as.integer(positionals[[2L]]) else 0L
if (length(max_priority) != 1L || is.na(max_priority) || max_priority < 0L) {
  stop("MAX_PRIORITY must be one non-negative integer", call. = FALSE)
}
candidate_library <- if (length(positionals) >= 3L) {
  normalizePath(positionals[[3L]], winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(root, ".local", "compat", "R", "library-candidate"),
    winslash = "/", mustWork = TRUE)
}
dependency_library <- if (length(positionals) >= 4L) {
  normalizePath(positionals[[4L]], winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(root, ".local", "compat", "R", "library-dependencies"),
    winslash = "/", mustWork = TRUE)
}
if (identical(candidate_library, dependency_library)) {
  stop("candidate and dependency libraries must be distinct", call. = FALSE)
}
if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT", unset = ""), root)) {
  stop("activate the repository-local environment first: . scripts/activate", call. = FALSE)
}
expected_r_home <- normalizePath(
  file.path(root, ".local", "toolchain", "lib", "R"),
  winslash = "/",
  mustWork = TRUE
)
if (!identical(normalizePath(R.home(), winslash = "/", mustWork = TRUE), expected_r_home)) {
  stop("consumer tests are not running under repository-local R", call. = FALSE)
}
plan_only <- plan_only_argument

require_plain_directory <- function(path, label) {
  link <- Sys.readlink(path)
  if (!dir.exists(path) || length(link) != 1L || is.na(link) || nzchar(link)) {
    stop(label, " is missing, not a directory, or symbolic: ", path, call. = FALSE)
  }
  invisible(path)
}

is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

plain_child_directory <- function(parent, name, label) {
  parent <- require_plain_directory(parent, paste0(label, " parent"))
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  path <- file.path(parent, name)
  require_plain_directory(path, label)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(dirname(path), parent)) {
    stop(label, " escaped its plain parent", call. = FALSE)
  }
  path
}

reserve_plain_child_directory <- function(parent, name, label) {
  parent <- require_plain_directory(parent, paste0(label, " parent"))
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  path <- file.path(parent, name)
  if (file.exists(path) || dir.exists(path) || is_symbolic(path) ||
      !dir.create(path, recursive = FALSE, showWarnings = FALSE)) {
    stop(label, " already exists or cannot be reserved: ", path, call. = FALSE)
  }
  require_plain_directory(path, label)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(dirname(path), parent)) {
    stop(label, " escaped its plain parent", call. = FALSE)
  }
  path
}

local_root <- require_plain_directory(
  file.path(root, ".local"),
  "repository-local state root"
)
local_root <- normalizePath(local_root, winslash = "/", mustWork = TRUE)
compat_root <- plain_child_directory(local_root, "compat", "compatibility state root")
runs_root <- plain_child_directory(compat_root, "runs", "retained compatibility root")
run_directory <- file.path(runs_root, run_id)
require_plain_directory(run_directory, "run directory created by dependency preparation")
run_directory <- normalizePath(run_directory, winslash = "/", mustWork = TRUE)
if (!identical(dirname(run_directory), runs_root)) {
  stop("run directory escaped .local/compat/runs", call. = FALSE)
}
expected_candidate_library <- file.path(run_directory, "library-candidate")
if (!identical(candidate_library, expected_candidate_library)) {
  stop(
    "candidate library does not belong to PARADOX_CANDIDATE_RUN_ID: ",
    expected_candidate_library,
    call. = FALSE
  )
}
stage_name <- sprintf("repository-tests-priority-%d", max_priority)
stage_directory <- file.path(run_directory, stage_name)
if (file.exists(stage_directory) || dir.exists(stage_directory) ||
    is_symbolic(stage_directory)) {
  stop("repository test evidence already exists: ", stage_directory, call. = FALSE)
}
result_path <- file.path(
  stage_directory,
  sprintf("test-results-priority-%d.tsv", max_priority)
)
started_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

read_manifest <- function(path, expected_columns) {
  value <- utils::read.delim(
    path,
    header = TRUE,
    sep = "\t",
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!identical(names(value), expected_columns) || !nrow(value) || anyNA(value)) {
    stop("manifest has an unexpected shape: ", path, call. = FALSE)
  }
  if (any(vapply(value, function(column) any(!nzchar(column)), logical(1L)))) {
    stop("manifest contains empty fields: ", path, call. = FALSE)
  }
  value
}

git_output <- function(repository, arguments, label) {
  error_file <- tempfile("paradox-consumer-git-")
  on.exit(unlink(error_file), add = TRUE)
  output <- suppressWarnings(system2(
    "git",
    c("-C", shQuote(repository), arguments),
    stdout = TRUE,
    stderr = error_file
  ))
  status <- attr(output, "status") %||% 0L
  if (!identical(status, 0L)) {
    detail <- if (file.exists(error_file)) {
      paste(readLines(error_file, warn = FALSE), collapse = "\n")
    } else {
      ""
    }
    stop("git failed while ", label, ": ", detail, call. = FALSE)
  }
  output
}

single_git_value <- function(repository, arguments, label) {
  output <- git_output(repository, arguments, label)
  if (length(output) != 1L || !nzchar(output[[1L]])) {
    stop("git returned an unexpected result while ", label, call. = FALSE)
  }
  output[[1L]]
}

require_regular_provenance_file <- function(path, label) {
  link <- Sys.readlink(path)
  if (!file.exists(path) || dir.exists(path) || length(link) != 1L ||
      is.na(link) || nzchar(link)) {
    stop(label, " is missing, not a regular file, or symbolic: ", path, call. = FALSE)
  }
  invisible(path)
}

validate_candidate_provenance <- function(
  root,
  candidate_run_id,
  candidate_library,
  dependency_library,
  dependency_library_content_sha256,
  candidate_ref,
  candidate_commit,
  candidate_tree,
  candidate_version,
  candidate_content_sha256
) {
  provenance_path <- file.path(
    candidate_library,
    ".paradox-candidate-provenance.tsv"
  )
  seal_path <- file.path(
    candidate_library,
    ".paradox-candidate-provenance.sha256"
  )
  require_regular_provenance_file(provenance_path, "candidate provenance receipt")
  require_regular_provenance_file(seal_path, "candidate provenance seal")

  provenance_lines <- readLines(provenance_path, warn = FALSE)
  expected_keys <- c(
    "schema", "candidate_run_id", "candidate_ref", "candidate_commit",
    "candidate_tree", "candidate_version", "candidate_library",
    "dependency_library", "dependency_library_content_sha256",
    "source_archive_sha256", "candidate_content_sha256", "installer_sha256",
    "git_authenticator_sha256"
  )
  if (length(provenance_lines) != length(expected_keys) + 1L ||
      !identical(provenance_lines[[1L]], "key\tvalue")) {
    stop("candidate provenance receipt has an unexpected schema", call. = FALSE)
  }
  fields <- strsplit(provenance_lines[-1L], "\t", fixed = TRUE)
  if (any(lengths(fields) != 2L)) {
    stop("candidate provenance receipt has malformed rows", call. = FALSE)
  }
  keys <- vapply(fields, `[[`, character(1L), 1L)
  values <- vapply(fields, `[[`, character(1L), 2L)
  if (!identical(keys, expected_keys) || any(!nzchar(values))) {
    stop("candidate provenance receipt keys, order, or values are invalid", call. = FALSE)
  }
  names(values) <- keys
  if (!identical(values[["schema"]], "2")) {
    stop("candidate provenance receipt schema is unsupported", call. = FALSE)
  }

  provenance_sha256 <- unname(tools::sha256sum(provenance_path))
  expected_seal <- paste0(
    provenance_sha256,
    "  .paradox-candidate-provenance.tsv"
  )
  seal_lines <- readLines(seal_path, warn = FALSE)
  if (!identical(seal_lines, expected_seal)) {
    stop("candidate provenance receipt seal is malformed or does not match", call. = FALSE)
  }

  declared_values <- c(
    candidate_run_id = candidate_run_id,
    candidate_ref = candidate_ref,
    candidate_commit = candidate_commit,
    candidate_tree = candidate_tree,
    candidate_version = candidate_version,
    candidate_library = candidate_library,
    dependency_library = dependency_library,
    dependency_library_content_sha256 = dependency_library_content_sha256,
    candidate_content_sha256 = candidate_content_sha256
  )
  if (!identical(unname(values[names(declared_values)]), unname(declared_values))) {
    stop(
      "candidate provenance receipt disagrees with declared or installed candidate values",
      call. = FALSE
    )
  }
  if (!grepl("^[0-9a-f]{64}$", values[["source_archive_sha256"]]) ||
      !grepl("^[0-9a-f]{64}$", values[["installer_sha256"]]) ||
      !grepl("^[0-9a-f]{64}$", values[["git_authenticator_sha256"]])) {
    stop("candidate provenance receipt contains a malformed SHA-256", call. = FALSE)
  }

  installer_path <- file.path(root, "compat", "install-candidate")
  require_regular_provenance_file(installer_path, "candidate installer")
  installer_sha256 <- unname(tools::sha256sum(installer_path))
  if (!identical(values[["installer_sha256"]], installer_sha256)) {
    stop("candidate provenance receipt was written by a different installer", call. = FALSE)
  }
  git_authenticator_path <- file.path(root, "compat", "authenticate-candidate-git")
  require_regular_provenance_file(
    git_authenticator_path, "candidate Git authenticator"
  )
  git_authenticator_sha256 <- unname(tools::sha256sum(git_authenticator_path))
  if (!identical(
      values[["git_authenticator_sha256"]], git_authenticator_sha256
    )) {
    stop(
      "candidate provenance receipt names a different Git authenticator",
      call. = FALSE
    )
  }

  archive_path <- tempfile("paradox-candidate-provenance-", fileext = ".tar")
  error_path <- tempfile("paradox-candidate-provenance-git-")
  on.exit(unlink(c(archive_path, error_path)), add = TRUE)
  archive_status <- suppressWarnings(system2(
    "git",
    c(
      "--no-replace-objects", "-c", "core.attributesFile=/dev/null",
      "-c", "tar.umask=0002",
      "-C", shQuote(root),
      "archive", "--format=tar", "-o",
      shQuote(archive_path), shQuote(candidate_commit)
    ),
    env = c(
      "GIT_ATTR_NOSYSTEM=1", "GIT_CONFIG_GLOBAL=/dev/null",
      "GIT_CONFIG_SYSTEM=/dev/null", "GIT_CONFIG_NOSYSTEM=1",
      "GIT_NO_REPLACE_OBJECTS=1"
    ),
    stdout = FALSE,
    stderr = error_path
  ))
  archive_status <- archive_status %||% 0L
  if (!identical(archive_status, 0L) || !file.exists(archive_path)) {
    detail <- if (file.exists(error_path)) {
      paste(readLines(error_path, warn = FALSE), collapse = "\n")
    } else {
      ""
    }
    stop("could not reproduce candidate source archive: ", detail, call. = FALSE)
  }
  archive_sha256 <- unname(tools::sha256sum(archive_path))
  if (!identical(values[["source_archive_sha256"]], archive_sha256)) {
    stop("candidate provenance source archive is not reproducible", call. = FALSE)
  }

  list(
    path = provenance_path,
    seal_path = seal_path,
    receipt_sha256 = provenance_sha256,
    source_archive_sha256 = archive_sha256,
    installer_sha256 = installer_sha256,
    git_authenticator_sha256 = git_authenticator_sha256
  )
}

extra_libraries_text <- Sys.getenv("PARADOX_CONSUMER_EXTRA_LIBS", unset = "")
extra_libraries <- if (nzchar(extra_libraries_text)) {
  paths <- strsplit(extra_libraries_text, .Platform$path.sep, fixed = TRUE)[[1L]]
  vapply(
    paths[nzchar(paths)],
    normalizePath,
    character(1L),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  character()
}
consumer_libpaths <- unique(c(candidate_library, extra_libraries, dependency_library, .Library))
consumer_library_environment <- paste(consumer_libpaths, collapse = .Platform$path.sep)
fingerprint_path <- file.path(root, "compat", "fingerprint.R")
harness_path <- file.path(root, "compat", "test-repositories.R")
dependency_harness_path <- file.path(
  root, "compat", "install-repository-test-dependencies.R"
)
evidence_helper_path <- file.path(root, "compat", "repository-evidence.R")
evidence_verifier_path <- file.path(root, "compat", "verify-repository-evidence.R")
git_authenticator_path <- file.path(root, "compat", "authenticate-candidate-git")
compat_system_evidence_path <- file.path(
  root, "compat", "compat-system-evidence.R"
)
sys.source(fingerprint_path, envir = environment())
sys.source(evidence_helper_path, envir = environment())
sys.source(compat_system_evidence_path, envir = environment())
require_regular_provenance_file(
  git_authenticator_path, "candidate Git authenticator"
)
if (file.access(git_authenticator_path, mode = 1L) != 0L) {
  stop("candidate Git authenticator is not executable", call. = FALSE)
}

authenticate_candidate_git <- function() {
  output <- suppressWarnings(system2(
    git_authenticator_path,
    vapply(
      c(root, candidate_ref, candidate_commit, candidate_tree),
      shQuote,
      character(1L)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- as.integer(attr(output, "status") %||% 0L)
  if (!identical(status, 0L) ||
      !identical(output, "candidate_git_authentication=passed")) {
    stop(
      "candidate Git authentication failed",
      if (length(output)) paste0(": ", paste(output, collapse = "\n")) else "",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
extra_library_content_sha256 <- if (length(extra_libraries)) {
  vapply(extra_libraries, compat_tree_content_sha256, character(1L))
} else {
  character()
}

candidate_package <- normalizePath(
  find.package("paradox", lib.loc = candidate_library),
  winslash = "/",
  mustWork = TRUE
)
if (!identical(dirname(candidate_package), candidate_library)) {
  stop("paradox did not resolve from the dedicated candidate library", call. = FALSE)
}
candidate_version <- as.character(utils::packageVersion("paradox", lib.loc = candidate_library))
candidate_ref <- Sys.getenv("PARADOX_CANDIDATE_REF", unset = "")
candidate_commit <- Sys.getenv("PARADOX_CANDIDATE_COMMIT", unset = "")
candidate_tree <- Sys.getenv("PARADOX_CANDIDATE_TREE", unset = "")
legacy_candidate_fingerprint <- Sys.getenv(
  "PARADOX_CANDIDATE_LIBRARY_SHA256",
  unset = ""
)
candidate_content_sha256 <- compat_tree_content_sha256(candidate_package)
declared_candidate_content_sha256 <- Sys.getenv(
  "PARADOX_CANDIDATE_CONTENT_SHA256",
  unset = ""
)
fingerprint_file <- file.path(candidate_library, ".paradox-candidate-content-sha256")
require_regular_provenance_file(
  fingerprint_file, "candidate content fingerprint sentinel"
)
expected_candidate_entries <- sort(c(
  ".paradox-candidate-content-sha256",
  ".paradox-candidate-provenance.sha256",
  ".paradox-candidate-provenance.tsv",
  "paradox"
))
observed_candidate_entries <- sort(list.files(
  candidate_library, all.files = TRUE, no.. = TRUE
))
if (!identical(observed_candidate_entries, expected_candidate_entries)) {
  stop(
    "candidate library has an unexpected top-level inventory: ",
    paste(observed_candidate_entries, collapse = ", "),
    call. = FALSE
  )
}
if (!nzchar(declared_candidate_content_sha256) && file.exists(fingerprint_file)) {
  declared_candidate_content_sha256 <- trimws(readLines(fingerprint_file, n = 1L, warn = FALSE))
}
if (!grepl("^[0-9a-f]{64}$", declared_candidate_content_sha256)) {
  stop(
    "candidate content fingerprint is missing or malformed; install with compat/install-candidate",
    call. = FALSE
  )
}
if (!identical(declared_candidate_content_sha256, candidate_content_sha256)) {
  stop("installed candidate does not match its declared content fingerprint", call. = FALSE)
}

object_hash_pattern <- "^([0-9a-f]{40}|[0-9a-f]{64})$"
ref_status <- if (grepl("^refs/", candidate_ref)) {
  suppressWarnings(system2(
    "git",
    c("-C", shQuote(root), "check-ref-format", shQuote(candidate_ref)),
    stdout = FALSE,
    stderr = FALSE
  ))
} else {
  1L
}
if (!identical(ref_status, 0L)) {
  stop("PARADOX_CANDIDATE_REF must be one valid full Git ref", call. = FALSE)
}
if (!grepl(object_hash_pattern, candidate_commit)) {
  stop("PARADOX_CANDIDATE_COMMIT must be one lowercase Git object hash", call. = FALSE)
}
if (!grepl(object_hash_pattern, candidate_tree)) {
  stop("PARADOX_CANDIDATE_TREE must be one lowercase Git object hash", call. = FALSE)
}

candidate_source_state <- function() {
  authenticate_candidate_git()
  list(
    ref_commit = single_git_value(
      root,
      c("rev-parse", "--verify", shQuote(paste0(candidate_ref, "^{commit}"))),
      "resolving the candidate ref commit"
    ),
    ref_tree = single_git_value(
      root,
      c("rev-parse", "--verify", shQuote(paste0(candidate_ref, "^{tree}"))),
      "resolving the candidate ref tree"
    ),
    commit_tree = single_git_value(
      root,
      c("rev-parse", "--verify", shQuote(paste0(candidate_commit, "^{tree}"))),
      "resolving the candidate commit tree"
    )
  )
}

candidate_source_matches <- function(state) {
  identical(candidate_commit, state$ref_commit) &&
    identical(candidate_tree, state$ref_tree) &&
    identical(candidate_tree, state$commit_tree)
}

initial_candidate_source <- candidate_source_state()
if (!candidate_source_matches(initial_candidate_source)) {
  stop("candidate ref, commit, and tree do not identify the same source", call. = FALSE)
}
candidate_provenance <- validate_candidate_provenance(
  root = root,
  candidate_run_id = candidate_run_id,
  candidate_library = candidate_library,
  dependency_library = dependency_library,
  dependency_library_content_sha256 = compat_tree_content_sha256(
    dependency_library
  ),
  candidate_ref = candidate_ref,
  candidate_commit = candidate_commit,
  candidate_tree = candidate_tree,
  candidate_version = candidate_version,
  candidate_content_sha256 = candidate_content_sha256
)
candidate_library_content_sha256 <- compat_tree_content_sha256(candidate_library)
dependency_library_content_sha256 <- compat_tree_content_sha256(dependency_library)

manifest_path <- file.path(root, "compat", "github-repositories.tsv")
snapshot_path <- file.path(root, "compat", "github-snapshot.tsv")
manifest <- read_manifest(
  manifest_path,
  c("repository", "url", "relation", "priority", "action", "notes")
)
snapshot <- read_manifest(
  snapshot_path,
  c("repository", "url", "priority", "commit", "commit_date", "branch")
)
if (anyDuplicated(manifest$repository) || anyDuplicated(snapshot$repository)) {
  stop("GitHub manifests contain duplicate repositories", call. = FALSE)
}
manifest_priority <- suppressWarnings(as.integer(manifest$priority))
snapshot_priority <- suppressWarnings(as.integer(snapshot$priority))
if (anyNA(manifest_priority) || anyNA(snapshot_priority) ||
    !identical(as.character(manifest_priority), manifest$priority) ||
    !identical(as.character(snapshot_priority), snapshot$priority) ||
    any(manifest_priority < 0L) || any(snapshot_priority < 0L)) {
  stop("GitHub manifests contain invalid priorities", call. = FALSE)
}
manifest$priority <- manifest_priority
snapshot$priority <- snapshot_priority
selected <- manifest[
  manifest$action == "clone" &
    manifest$priority <= max_priority &
    manifest$relation %in% c("Depends", "Imports", "Suggests"),
  ,
  drop = FALSE
]
if (!nrow(selected)) stop("GitHub consumer selection is empty", call. = FALSE)
snapshot_index <- match(selected$repository, snapshot$repository)
if (anyNA(snapshot_index)) {
  stop("selected repositories are absent from github-snapshot.tsv", call. = FALSE)
}
selected_snapshot <- snapshot[snapshot_index, , drop = FALSE]
if (!identical(selected$repository, selected_snapshot$repository) ||
    !identical(selected$url, selected_snapshot$url) ||
    !identical(selected$priority, selected_snapshot$priority)) {
  stop("GitHub snapshot disagrees with the reviewed repository manifest", call. = FALSE)
}
if (any(!grepl(object_hash_pattern, selected_snapshot$commit))) {
  stop("GitHub snapshot contains a malformed commit", call. = FALSE)
}

read_metadata_map <- function(path, expected_fields, label) {
  value <- utils::read.delim(
    path,
    header = TRUE,
    sep = "\t",
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!identical(names(value), c("field", "value")) || anyNA(value) ||
      !identical(value$field, expected_fields) || any(!nzchar(value$value))) {
    stop(label, " has an unexpected schema or contents: ", path, call. = FALSE)
  }
  setNames(value$value, value$field)
}

dependency_stage_directory <- file.path(
  run_directory,
  sprintf("repository-dependencies-priority-%d", max_priority)
)
require_plain_directory(dependency_stage_directory, "repository dependency evidence")
dependency_stage_directory <- normalizePath(
  dependency_stage_directory,
  winslash = "/",
  mustWork = TRUE
)
if (!identical(dirname(dependency_stage_directory), run_directory)) {
  stop("repository dependency evidence escaped its run directory", call. = FALSE)
}
dependency_evidence <- repository_verify_evidence(dependency_stage_directory)
dependency_metadata_directory <- plain_child_directory(
  dependency_stage_directory,
  "metadata",
  "dependency evidence metadata directory"
)
dependency_run_path <- file.path(dependency_metadata_directory, "run.tsv")
dependency_completion_path <- file.path(
  dependency_metadata_directory,
  "completion.tsv"
)
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
dependency_run <- read_metadata_map(
  dependency_run_path,
  dependency_run_fields,
  "dependency run metadata"
)
dependency_completion <- read_metadata_map(
  dependency_completion_path,
  dependency_completion_fields,
  "dependency completion metadata"
)
dependency_result_path <- file.path(
  dependency_stage_directory,
  sprintf("dependency-install-priority-%d.tsv", max_priority)
)
dependency_result <- utils::read.delim(
  dependency_result_path,
  header = TRUE,
  sep = "\t",
  quote = "",
  comment.char = "",
  colClasses = "character",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
expected_dependency_result_columns <- c(
  "run_id", "repository", "commit", "priority", "status",
  "elapsed_seconds", "local_sources", "error"
)
if (!identical(names(dependency_result), expected_dependency_result_columns) ||
    anyNA(dependency_result) || !nrow(dependency_result)) {
  stop("dependency result ledger has an unexpected schema", call. = FALSE)
}
dependency_preflight_path <- file.path(
  dependency_metadata_directory,
  "checkout-preflight.tsv"
)
dependency_postflight_path <- file.path(
  dependency_metadata_directory,
  "checkout-postflight.tsv"
)
dependency_fallback_preflight_path <- file.path(
  dependency_metadata_directory,
  "fallback-checkout-preflight.tsv"
)
dependency_fallback_postflight_path <- file.path(
  dependency_metadata_directory,
  "fallback-checkout-postflight.tsv"
)
for (path in c(
  dependency_run_path, dependency_completion_path, dependency_result_path,
  dependency_preflight_path, dependency_postflight_path,
  dependency_fallback_preflight_path, dependency_fallback_postflight_path
)) {
  require_regular_provenance_file(path, "dependency evidence input")
}
read_dependency_checkout_observations <- function(path, label) {
  value <- utils::read.delim(
    path,
    header = TRUE,
    sep = "\t",
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expected_columns <- c(
    "repository", "checkout", "expected_commit", "observed_commit",
    "expected_origin", "observed_origin", "clean", "status", "valid"
  )
  expected_checkouts <- vapply(
    file.path(root, ".local", "compat", "github", selected$repository),
    normalizePath,
    character(1L),
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(names(value), expected_columns) || anyNA(value) ||
      !identical(value$repository, selected$repository) ||
      !identical(value$checkout, unname(expected_checkouts)) ||
      !identical(value$expected_commit, selected_snapshot$commit) ||
      !identical(value$observed_commit, selected_snapshot$commit) ||
      !identical(value$expected_origin, selected_snapshot$url) ||
      !identical(value$observed_origin, selected_snapshot$url) ||
      any(value$clean != "TRUE") || any(value$valid != "TRUE") ||
      any(value$status != "-")) {
    stop(label, " does not describe the pinned clean checkout selection", call. = FALSE)
  }
  value
}
dependency_preflight <- read_dependency_checkout_observations(
  dependency_preflight_path,
  "dependency checkout preflight"
)
dependency_postflight <- read_dependency_checkout_observations(
  dependency_postflight_path,
  "dependency checkout postflight"
)

clone_manifest <- manifest[manifest$action == "clone", , drop = FALSE]
clone_snapshot_index <- match(clone_manifest$repository, snapshot$repository)
if (anyNA(clone_snapshot_index)) {
  stop("cloned fallback providers are absent from github-snapshot.tsv", call. = FALSE)
}
clone_snapshot <- snapshot[clone_snapshot_index, , drop = FALSE]
if (!identical(clone_manifest$repository, clone_snapshot$repository) ||
    !identical(clone_manifest$url, clone_snapshot$url) ||
    !identical(clone_manifest$priority, clone_snapshot$priority) ||
    any(!grepl(object_hash_pattern, clone_snapshot$commit))) {
  stop("GitHub snapshot disagrees with cloned fallback providers", call. = FALSE)
}
fallback_rows <- list()
seen_fallback_packages <- character()
for (index in seq_len(nrow(clone_manifest))) {
  repository <- clone_manifest$repository[[index]]
  checkout <- file.path(root, ".local", "compat", "github", repository)
  description <- file.path(checkout, "DESCRIPTION")
  if (!file.exists(description)) next
  if (dir.exists(description) || is_symbolic(description)) {
    stop("Fallback DESCRIPTION is not one regular file: ", description, call. = FALSE)
  }
  package <- unname(read.dcf(description, fields = "Package")[[1L, "Package"]])
  if (length(package) != 1L || is.na(package) || !nzchar(package) ||
      package %in% seen_fallback_packages) {
    stop("Fallback package provider is invalid or duplicated: ", repository,
      call. = FALSE)
  }
  seen_fallback_packages <- c(seen_fallback_packages, package)
  fallback_rows[[length(fallback_rows) + 1L]] <- data.frame(
    package = package,
    repository = repository,
    checkout = normalizePath(checkout, winslash = "/", mustWork = TRUE),
    expected_commit = clone_snapshot$commit[[index]],
    expected_origin = clone_snapshot$url[[index]],
    stringsAsFactors = FALSE
  )
}
if (!length(fallback_rows)) {
  stop("No pinned local fallback package providers were found", call. = FALSE)
}
expected_fallback_providers <- do.call(rbind, fallback_rows)
read_fallback_checkout_observations <- function(path, label) {
  value <- utils::read.delim(
    path,
    header = TRUE,
    sep = "\t",
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expected_columns <- c(
    "package", "repository", "checkout", "expected_commit", "observed_commit",
    "expected_origin", "observed_origin", "clean", "status", "valid"
  )
  if (!identical(names(value), expected_columns) || anyNA(value) ||
      !identical(value$package, expected_fallback_providers$package) ||
      !identical(value$repository, expected_fallback_providers$repository) ||
      !identical(value$checkout, expected_fallback_providers$checkout) ||
      !identical(value$expected_commit, expected_fallback_providers$expected_commit) ||
      !identical(value$observed_commit, expected_fallback_providers$expected_commit) ||
      !identical(value$expected_origin, expected_fallback_providers$expected_origin) ||
      !identical(value$observed_origin, expected_fallback_providers$expected_origin) ||
      any(value$clean != "TRUE") || any(value$valid != "TRUE") ||
      any(value$status != "-")) {
    stop(label, " does not describe every pinned clean fallback provider",
      call. = FALSE)
  }
  value
}
dependency_fallback_preflight <- read_fallback_checkout_observations(
  dependency_fallback_preflight_path,
  "dependency fallback checkout preflight"
)
dependency_fallback_postflight <- read_fallback_checkout_observations(
  dependency_fallback_postflight_path,
  "dependency fallback checkout postflight"
)
dependency_copied_inputs <- c(
  github_manifest = file.path(dependency_metadata_directory, basename(manifest_path)),
  github_snapshot = file.path(dependency_metadata_directory, basename(snapshot_path)),
  dependency_harness = file.path(
    dependency_metadata_directory,
    basename(dependency_harness_path)
  ),
  fingerprint = file.path(dependency_metadata_directory, basename(fingerprint_path)),
  evidence_helper = file.path(
    dependency_metadata_directory,
    basename(evidence_helper_path)
  ),
  evidence_verifier = file.path(
    dependency_metadata_directory,
    basename(evidence_verifier_path)
  )
)
for (path in dependency_copied_inputs) {
  require_regular_provenance_file(path, "copied dependency evidence input")
}
dependency_copied_hashes <- unname(tools::sha256sum(dependency_copied_inputs))
dependency_current_hashes <- unname(tools::sha256sum(c(
  manifest_path, snapshot_path, dependency_harness_path, fingerprint_path,
  evidence_helper_path, evidence_verifier_path
)))
if (!identical(dependency_copied_hashes, dependency_current_hashes)) {
  stop("copied dependency evidence inputs differ from the current frozen inputs",
    call. = FALSE)
}
expected_dependency_values <- c(
  schema = "3",
  stage_kind = "repository_dependencies",
  run_id = run_id,
  root = root,
  max_priority = as.character(max_priority),
  dependency_library = dependency_library,
  r = normalizePath(file.path(R.home(), "bin", "R"), winslash = "/", mustWork = TRUE),
  r_version = as.character(getRversion()),
  github_manifest_sha256 = unname(tools::sha256sum(manifest_path)),
  github_snapshot_sha256 = unname(tools::sha256sum(snapshot_path)),
  harness_sha256 = unname(tools::sha256sum(dependency_harness_path)),
  fingerprint_sha256 = unname(tools::sha256sum(fingerprint_path)),
  evidence_helper_sha256 = unname(tools::sha256sum(evidence_helper_path)),
  evidence_verifier_sha256 = unname(tools::sha256sum(evidence_verifier_path)),
  checkout_preflight_sha256 = unname(tools::sha256sum(dependency_preflight_path)),
  fallback_checkout_preflight_sha256 = unname(tools::sha256sum(
    dependency_fallback_preflight_path
  )),
  result_ledger = dependency_result_path
)
if (!identical(
  unname(dependency_run[names(expected_dependency_values)]),
  unname(expected_dependency_values)
)) {
  stop("dependency run metadata does not match this checkout-test run", call. = FALSE)
}
expected_completion_values <- c(
  schema = "3",
  stage_kind = "repository_dependencies",
  run_id = run_id,
  max_priority = as.character(max_priority),
  status = "passed",
  result_rows = as.character(nrow(selected)),
  failed_rows = "0",
  result_sha256 = unname(tools::sha256sum(dependency_result_path)),
  checkout_postflight_sha256 = unname(tools::sha256sum(dependency_postflight_path)),
  fallback_checkout_postflight_sha256 = unname(tools::sha256sum(
    dependency_fallback_postflight_path
  )),
  dependency_library_content_after = dependency_library_content_sha256
)
if (!identical(
  unname(dependency_completion[names(expected_completion_values)]),
  unname(expected_completion_values)
)) {
  stop("dependency completion metadata is not a successful matching stage", call. = FALSE)
}
if (!identical(dependency_result$run_id, rep(run_id, nrow(selected))) ||
    !identical(dependency_result$repository, selected$repository) ||
    !identical(dependency_result$commit, selected_snapshot$commit) ||
    !identical(dependency_result$priority, as.character(selected$priority)) ||
    any(dependency_result$status != "passed")) {
  stop("dependency result ledger does not cover the pinned selection", call. = FALSE)
}
if (!grepl("^[0-9a-f]{64}$", dependency_run[["dependency_library_content_before"]])) {
  stop("dependency run metadata has a malformed initial library hash", call. = FALSE)
}

checkout_state <- function(index) {
  repository <- selected$repository[[index]]
  checkout <- file.path(root, ".local", "compat", "github", repository)
  if (!file.exists(file.path(checkout, ".git"))) {
    stop("Git checkout is missing for ", repository, call. = FALSE)
  }
  head <- single_git_value(checkout, c("rev-parse", "--verify", "HEAD^{commit}"),
    paste0("reading ", repository, " HEAD"))
  status <- git_output(
    checkout,
    c("status", "--porcelain=v1", "--untracked-files=all"),
    paste0("reading ", repository, " worktree status")
  )
  origin <- single_git_value(
    checkout,
    c("remote", "get-url", "origin"),
    paste0("reading ", repository, " origin")
  )
  list(
    checkout = normalizePath(checkout, winslash = "/", mustWork = TRUE),
    head = head,
    clean = !length(status),
    status = paste(status, collapse = "\n"),
    origin = origin,
    valid = identical(head, selected_snapshot$commit[[index]]) &&
      !length(status) && identical(origin, selected_snapshot$url[[index]])
  )
}

initial_checkout_states <- lapply(seq_len(nrow(selected)), checkout_state)
invalid_checkouts <- selected$repository[!vapply(
  initial_checkout_states,
  `[[`,
  logical(1L),
  "valid"
)]
if (length(invalid_checkouts)) {
  stop(
    "GitHub checkout commit, origin, or clean-tree validation failed: ",
    paste(invalid_checkouts, collapse = ", "),
    call. = FALSE
  )
}
if (!identical(compat_tree_content_sha256(candidate_library), candidate_library_content_sha256) ||
    !identical(compat_tree_content_sha256(dependency_library), dependency_library_content_sha256)) {
  stop("protected libraries changed during GitHub preflight", call. = FALSE)
}

write_tsv <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(temporary) || is_symbolic(temporary)) {
    stop("temporary evidence path already exists: ", temporary, call. = FALSE)
  }
  on.exit(unlink(temporary), add = TRUE)
  utils::write.table(
    value,
    temporary,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    na = "-",
    fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) {
    stop("could not atomically write retained evidence: ", path, call. = FALSE)
  }
  invisible(path)
}

if (plan_only) {
  plan <- data.frame(
    run_id = run_id,
    result_ledger = result_path,
    repository = selected$repository,
    priority = selected$priority,
    commit = selected_snapshot$commit,
    checkout = vapply(initial_checkout_states, `[[`, character(1L), "checkout"),
    clean = vapply(initial_checkout_states, `[[`, logical(1L), "clean"),
    candidate_run_id = candidate_run_id,
    candidate_ref = candidate_ref,
    candidate_commit = candidate_commit,
    candidate_tree = candidate_tree,
    candidate_content_sha256 = candidate_content_sha256,
    candidate_provenance_sha256 = candidate_provenance$receipt_sha256,
    candidate_source_archive_sha256 = candidate_provenance$source_archive_sha256,
    candidate_installer_sha256 = candidate_provenance$installer_sha256,
    candidate_git_authenticator_sha256 =
      candidate_provenance$git_authenticator_sha256,
    candidate_library_content_sha256 = candidate_library_content_sha256,
    dependency_library_content_sha256 = dependency_library_content_sha256,
    dependency_stage = dependency_stage_directory,
    dependency_stage_manifest_sha256 = dependency_evidence$manifest_sha256,
    dependency_stage_seal_sha256 = dependency_evidence$seal_sha256,
    dependency_stage_completion_sha256 = unname(tools::sha256sum(
      dependency_completion_path
    )),
    github_manifest_sha256 = unname(tools::sha256sum(manifest_path)),
    github_snapshot_sha256 = unname(tools::sha256sum(snapshot_path)),
    stringsAsFactors = FALSE
  )
  utils::write.table(plan, stdout(), quote = FALSE, sep = "\t", row.names = FALSE)
  quit(save = "no", status = 0L)
}

stage_directory <- reserve_plain_child_directory(
  run_directory,
  stage_name,
  "repository test evidence"
)
metadata_directory <- reserve_plain_child_directory(
  stage_directory,
  "metadata",
  "repository test metadata directory"
)
Sys.setenv(NOT_CRAN = "true")
metadata_inputs <- c(
  manifest_path, snapshot_path, fingerprint_path, harness_path,
  evidence_helper_path, evidence_verifier_path, compat_system_evidence_path,
  candidate_provenance$path, candidate_provenance$seal_path,
  file.path(root, "compat", "install-candidate"), git_authenticator_path
)
copied_metadata <- file.copy(
  metadata_inputs,
  metadata_directory,
  copy.mode = TRUE,
  copy.date = TRUE
)
if (!all(copied_metadata)) stop("could not retain repository test inputs", call. = FALSE)
compat_system_evidence <- compat_system_capture_evidence(root, metadata_directory)
run_metadata <- data.frame(
  field = c(
    "schema", "stage_kind", "run_id", "started_utc", "root", "max_priority",
    "candidate_run_id", "candidate_ref", "candidate_commit", "candidate_tree",
    "candidate_version",
    "candidate_library", "candidate_package", "candidate_content_sha256",
    "candidate_library_content_sha256", "dependency_library",
    "dependency_library_content_sha256", "extra_libraries",
    "extra_library_content_sha256", "r", "r_version", "not_cran",
    "github_manifest_sha256", "github_snapshot_sha256", "harness_sha256",
    "fingerprint_sha256", "candidate_provenance_sha256",
    "candidate_source_archive_sha256", "candidate_installer_sha256",
    "candidate_git_authenticator_sha256",
    "evidence_helper_sha256", "evidence_verifier_sha256",
    "dependency_stage", "dependency_stage_manifest_sha256",
    "dependency_stage_seal_sha256", "dependency_stage_run_sha256",
    "dependency_stage_completion_sha256", "dependency_stage_result_sha256",
    "result_ledger"
  ),
  value = c(
    "3", "repository_tests", run_id, started_utc, root,
    as.character(max_priority), candidate_run_id, candidate_ref,
    candidate_commit, candidate_tree, candidate_version, candidate_library,
    candidate_package, candidate_content_sha256, candidate_library_content_sha256,
    dependency_library, dependency_library_content_sha256,
    if (length(extra_libraries)) paste(extra_libraries, collapse = .Platform$path.sep) else "-",
    if (length(extra_library_content_sha256)) {
      paste(paste0(names(extra_library_content_sha256), "=", extra_library_content_sha256),
        collapse = ";")
    } else {
      "-"
    },
    normalizePath(file.path(R.home(), "bin", "R"), winslash = "/", mustWork = TRUE),
    as.character(getRversion()), Sys.getenv("NOT_CRAN", unset = ""),
    unname(tools::sha256sum(manifest_path)), unname(tools::sha256sum(snapshot_path)),
    unname(tools::sha256sum(harness_path)), unname(tools::sha256sum(fingerprint_path)),
    candidate_provenance$receipt_sha256,
    candidate_provenance$source_archive_sha256,
    candidate_provenance$installer_sha256,
    candidate_provenance$git_authenticator_sha256,
    unname(tools::sha256sum(evidence_helper_path)),
    unname(tools::sha256sum(evidence_verifier_path)),
    dependency_stage_directory, dependency_evidence$manifest_sha256,
    dependency_evidence$seal_sha256,
    unname(tools::sha256sum(dependency_run_path)),
    unname(tools::sha256sum(dependency_completion_path)),
    unname(tools::sha256sum(dependency_result_path)), result_path
  ),
  stringsAsFactors = FALSE
)
write_tsv(run_metadata, file.path(metadata_directory, "run.tsv"))

run_repository_tests <- function(
  checkout,
  framework,
  libpaths,
  library_environment,
  candidate_library,
  candidate_version,
  candidate_content_sha256,
  fingerprint_tree
) {
  child_environment <- compat_system_child_environment()
  child_environment[c(
    "NOT_CRAN", "TESTTHAT_PARALLEL", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE",
    "TMPDIR", "XDG_RUNTIME_DIR", "XDG_CACHE_HOME", "CCACHE_DIR",
    "CCACHE_TEMPDIR"
  )] <- c(
    "true", "false", library_environment, library_environment, "",
    Sys.getenv("TMPDIR"), Sys.getenv("XDG_RUNTIME_DIR"),
    Sys.getenv("XDG_CACHE_HOME"), Sys.getenv("CCACHE_DIR"),
    Sys.getenv("CCACHE_TEMPDIR")
  )
  callr::r(
    function(
      checkout,
      framework,
      library_environment,
      candidate_library,
      candidate_version,
      candidate_content_sha256,
      fingerprint_tree
    ) {
      Sys.setenv(NOT_CRAN = "true", TESTTHAT_PARALLEL = "false")
      if (!identical(Sys.getenv("R_LIBS"), library_environment) ||
          !identical(Sys.getenv("R_LIBS_USER"), library_environment)) {
        stop("child process did not receive the isolated library environment", call. = FALSE)
      }
      resolved_candidate <- normalizePath(find.package("paradox"), mustWork = TRUE)
      if (!identical(normalizePath(dirname(resolved_candidate)), candidate_library)) {
        stop("child process did not resolve paradox from the candidate library", call. = FALSE)
      }
      if (!identical(as.character(utils::packageVersion("paradox")), candidate_version)) {
        stop("child process resolved an unexpected paradox version", call. = FALSE)
      }
      if (!identical(fingerprint_tree(resolved_candidate), candidate_content_sha256)) {
        stop("child process resolved modified candidate contents", call. = FALSE)
      }
      if (framework == "testthat") {
        test_results <- testthat::test_local(
          checkout,
          reporter = "summary",
          stop_on_failure = FALSE,
          stop_on_warning = FALSE
        )
        # Preserve strict row failure while collecting the whole consumer suite
        # first, so an expensive isolated checkout exposes all ordinary test
        # failures and warnings in one invocation.
        testthat:::test_files_check(
          test_results,
          stop_on_failure = TRUE,
          stop_on_warning = TRUE
        )
      } else if (framework == "tinytest") {
        pkgload::load_all(checkout, helpers = FALSE, export_all = FALSE, quiet = TRUE)
        tinytest_result <- tinytest::run_test_dir(
          file.path(checkout, "inst", "tinytest"),
          at_home = TRUE,
          verbose = 1L,
          lc_collate = "C"
        )
        failed <- vapply(tinytest_result, isFALSE, logical(1L))
        if (any(failed)) {
          details <- vapply(tinytest_result[failed], format, character(1L), type = "long")
          stop(
            sum(failed), " out of ", length(failed), " tests failed:\n",
            paste(details, collapse = "\n"),
            call. = FALSE
          )
        }
      } else if (framework == "base") {
        pkgload::load_all(checkout, helpers = FALSE, export_all = FALSE, quiet = TRUE)
        test_files <- list.files(
          file.path(checkout, "tests"),
          pattern = "[.][Rr]$",
          full.names = TRUE
        )
        for (test_file in test_files) {
          sys.source(test_file, envir = new.env(parent = globalenv()), keep.source = TRUE)
        }
      }
      invisible(TRUE)
    },
    args = list(
      checkout = checkout,
      framework = framework,
      library_environment = library_environment,
      candidate_library = candidate_library,
      candidate_version = candidate_version,
      candidate_content_sha256 = candidate_content_sha256,
      fingerprint_tree = fingerprint_tree
    ),
    libpath = libpaths,
    env = child_environment,
    stdout = "|",
    stderr = "2>&1",
    spinner = FALSE,
    show = TRUE
  )
}

empty_results <- function() {
  data.frame(
    run_id = character(),
    repository = character(),
    expected_commit = character(),
    observed_commit_before = character(),
    observed_commit_after = character(),
    checkout_origin = character(),
    checkout_clean_before = logical(),
    checkout_clean_after = logical(),
    priority = integer(),
    framework = character(),
    paradox_version = character(),
    candidate_ref = character(),
    candidate_commit = character(),
    candidate_tree = character(),
    candidate_library_sha256 = character(),
    observed_candidate_library_sha256 = character(),
    declared_legacy_candidate_library_sha256 = character(),
    candidate_library_content_before = character(),
    candidate_library_content_after = character(),
    dependency_library_content_before = character(),
    dependency_library_content_after = character(),
    candidate_content_sha256 = character(),
    observed_candidate_content_before = character(),
    observed_candidate_content_after = character(),
    observed_ref_commit_before = character(),
    observed_ref_commit_after = character(),
    observed_ref_tree_before = character(),
    observed_ref_tree_after = character(),
    candidate_provenance = character(),
    not_cran = character(),
    status = character(),
    classification = character(),
    elapsed_seconds = numeric(),
    error = character(),
    stringsAsFactors = FALSE
  )
}

compact_error <- function(message, limit = 4000L) {
  message <- gsub("[\r\n\t]+", " ", message)
  if (nchar(message, type = "chars") <= limit) return(message)

  side <- as.integer((limit - 80L) / 2L)
  paste0(
    substr(message, 1L, side),
    " [... verbose child output omitted; decisive tail follows ...] ",
    substr(message, nchar(message, type = "chars") - side + 1L, nchar(message, type = "chars"))
  )
}

write_results <- function(result_list) {
  result_list <- Filter(Negate(is.null), result_list)
  combined <- if (length(result_list)) do.call(rbind, result_list) else empty_results()
  combined$error <- vapply(combined$error, compact_error, character(1L))
  output <- combined
  for (field in names(output)) {
    if (is.character(output[[field]])) {
      output[[field]][is.na(output[[field]]) | !nzchar(output[[field]])] <- "-"
    }
  }
  write_tsv(output, result_path)
  combined
}

results <- vector("list", nrow(selected))

classify_consumer_failure <- function(error) {
  dependency_patterns <- paste(c(
    "there is no package called",
    "package required but not available",
    "package .* required by .* could not be found",
    "namespace .* is being loaded, but .* is required",
    "dependencies? .* (is|are) not available"
  ), collapse = "|")
  system_patterns <- paste(c(
    "cannot find -l",
    "library not found for -l",
    "fatal error: .*: No such file or directory",
    "configuration failed for package",
    "command not found",
    "system requirements .* not available"
  ), collapse = "|")
  if (grepl(dependency_patterns, error, ignore.case = TRUE, perl = TRUE)) {
    return("environmental_dependency_failure")
  }
  if (grepl(system_patterns, error, ignore.case = TRUE, perl = TRUE)) {
    return("environmental_system_dependency_failure")
  }
  "candidate_or_consumer_test_failure"
}

for (i in seq_len(nrow(selected))) {
  repository <- selected$repository[[i]]
  checkout_before <- checkout_state(i)
  source_before <- candidate_source_state()
  candidate_before <- compat_tree_content_sha256(candidate_package)
  candidate_library_before <- compat_tree_content_sha256(candidate_library)
  dependency_library_before <- compat_tree_content_sha256(dependency_library)
  if (!checkout_before$valid) {
    stop("checkout provenance changed before testing ", repository, call. = FALSE)
  }
  if (!candidate_source_matches(source_before)) {
    stop("candidate source ref changed before testing ", repository, call. = FALSE)
  }
  if (!identical(candidate_before, candidate_content_sha256) ||
      !identical(candidate_library_before, candidate_library_content_sha256) ||
      !identical(dependency_library_before, dependency_library_content_sha256)) {
    stop("protected library changed before testing ", repository, call. = FALSE)
  }
  checkout <- checkout_before$checkout
  message("Testing ", repository)
  started <- Sys.time()
  framework <- if (dir.exists(file.path(checkout, "tests", "testthat"))) {
    "testthat"
  } else if (dir.exists(file.path(checkout, "inst", "tinytest"))) {
    "tinytest"
  } else if (length(list.files(file.path(checkout, "tests"), pattern = "[.][Rr]$"))) {
    "base"
  } else {
    "none"
  }

  error <- tryCatch({
    if (!file.exists(file.path(checkout, "DESCRIPTION"))) {
      stop("Checkout or DESCRIPTION is missing: ", checkout)
    }
    if (framework != "none") {
      run_repository_tests(
        checkout,
        framework,
        consumer_libpaths,
        consumer_library_environment,
        candidate_library,
        candidate_version,
        candidate_content_sha256,
        compat_tree_content_sha256
      )
    }
    ""
  }, error = function(condition) conditionMessage(condition))

  candidate_after <- compat_tree_content_sha256(candidate_package)
  candidate_library_after <- compat_tree_content_sha256(candidate_library)
  dependency_library_after <- compat_tree_content_sha256(dependency_library)
  source_after_error <- ""
  source_after <- tryCatch(
    candidate_source_state(),
    error = function(condition) {
      source_after_error <<- conditionMessage(condition)
      list(ref_commit = "", ref_tree = "", commit_tree = "")
    }
  )
  checkout_after_error <- ""
  checkout_after <- tryCatch(
    checkout_state(i),
    error = function(condition) {
      checkout_after_error <<- conditionMessage(condition)
      list(head = "", clean = FALSE, status = "", origin = "", valid = FALSE)
    }
  )
  protected_library_changed <-
    !identical(candidate_after, candidate_content_sha256) ||
    !identical(candidate_library_after, candidate_library_content_sha256) ||
    !identical(dependency_library_after, dependency_library_content_sha256)
  candidate_source_changed <- nzchar(source_after_error) ||
    !candidate_source_matches(source_after)
  checkout_changed <- nzchar(checkout_after_error) || !checkout_after$valid
  provenance_errors <- c(
    if (protected_library_changed) {
      paste0("Protected library contents changed while testing ", repository)
    } else {
      character()
    },
    if (candidate_source_changed) {
      paste0("Candidate source ref changed while testing ", repository)
    } else {
      character()
    },
    if (checkout_changed) {
      paste0(
        "Checkout provenance changed while testing ", repository,
        if (nzchar(checkout_after$status)) paste0(": ", checkout_after$status) else ""
      )
    } else {
      character()
    },
    source_after_error,
    checkout_after_error
  )
  provenance_errors <- provenance_errors[nzchar(provenance_errors)]
  if (length(provenance_errors)) {
    error <- paste(c(error[nzchar(error)], provenance_errors), collapse = "\n")
  }
  provenance_failed <- protected_library_changed || candidate_source_changed || checkout_changed
  classification <- if (protected_library_changed) {
    "protected_library_provenance_violation"
  } else if (candidate_source_changed) {
    "candidate_source_provenance_violation"
  } else if (checkout_changed) {
    "checkout_provenance_violation"
  } else if (nzchar(error)) {
    classify_consumer_failure(error)
  } else if (framework == "none") {
    "harness_no_tests"
  } else {
    "passed"
  }

  results[[i]] <- data.frame(
    run_id = run_id,
    repository = repository,
    expected_commit = selected_snapshot$commit[[i]],
    observed_commit_before = checkout_before$head,
    observed_commit_after = checkout_after$head,
    checkout_origin = checkout_after$origin,
    checkout_clean_before = checkout_before$clean,
    checkout_clean_after = checkout_after$clean,
    priority = selected$priority[[i]],
    framework = framework,
    paradox_version = candidate_version,
    candidate_ref = candidate_ref,
    candidate_commit = candidate_commit,
    candidate_tree = candidate_tree,
    candidate_library_sha256 = candidate_library_content_sha256,
    observed_candidate_library_sha256 = candidate_library_after,
    declared_legacy_candidate_library_sha256 = legacy_candidate_fingerprint,
    candidate_library_content_before = candidate_library_before,
    candidate_library_content_after = candidate_library_after,
    dependency_library_content_before = dependency_library_before,
    dependency_library_content_after = dependency_library_after,
    candidate_content_sha256 = candidate_content_sha256,
    observed_candidate_content_before = candidate_before,
    observed_candidate_content_after = candidate_after,
    observed_ref_commit_before = source_before$ref_commit,
    observed_ref_commit_after = source_after$ref_commit,
    observed_ref_tree_before = source_before$ref_tree,
    observed_ref_tree_after = source_after$ref_tree,
    candidate_provenance = if (provenance_failed) {
      "invalid_changed_during_repository"
    } else {
      "verified_installer_receipt_ref_tree_and_content"
    },
    not_cran = Sys.getenv("NOT_CRAN", unset = ""),
    status = if (provenance_failed || nzchar(error)) {
      "failed"
    } else if (framework == "none") {
      "skipped_no_tests"
    } else {
      "passed"
    },
    classification = classification,
    elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    error = error,
    stringsAsFactors = FALSE
  )
  write_results(results)
  if (provenance_failed) break
}

results <- write_results(results)

final_candidate_source <- candidate_source_state()
final_dependency_evidence <- repository_verify_evidence(dependency_stage_directory)
compat_system_verify_evidence(
  compat_system_evidence,
  "during repository test completion"
)
if (!candidate_source_matches(final_candidate_source) ||
    !identical(final_dependency_evidence$manifest_sha256,
      dependency_evidence$manifest_sha256) ||
    !identical(final_dependency_evidence$seal_sha256, dependency_evidence$seal_sha256) ||
    !identical(compat_tree_content_sha256(candidate_package), candidate_content_sha256) ||
    !identical(compat_tree_content_sha256(candidate_library), candidate_library_content_sha256) ||
    !identical(compat_tree_content_sha256(dependency_library), dependency_library_content_sha256)) {
  stop("protected GitHub consumer inputs changed during the run", call. = FALSE)
}

completion <- data.frame(
  field = c(
    "schema", "stage_kind", "run_id", "max_priority", "finished_utc",
    "status", "result_rows",
    "failed_rows", "skipped_rows", "result_sha256",
    "candidate_library_content_after", "dependency_library_content_after",
    "dependency_stage_manifest_sha256"
  ),
  value = c(
    "2", "repository_tests", run_id, as.character(max_priority),
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    if (any(results$status == "failed")) "failed" else "passed",
    as.character(nrow(results)), as.character(sum(results$status == "failed")),
    as.character(sum(results$status == "skipped_no_tests")),
    unname(tools::sha256sum(result_path)),
    compat_tree_content_sha256(candidate_library),
    compat_tree_content_sha256(dependency_library),
    dependency_evidence$manifest_sha256
  ),
  stringsAsFactors = FALSE
)
write_tsv(completion, file.path(metadata_directory, "completion.tsv"))
repository_seal_evidence(stage_directory)

if (any(results$status == "failed")) quit(save = "no", status = 1L)
