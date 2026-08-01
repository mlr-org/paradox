#!/usr/bin/env Rscript

# Focused regression gate for the reusable repository row engine.  It uses
# tiny local repositories and fake libraries except for one bounded repeated
# installation of the pinned exact provider; it never runs a real consumer or
# candidate.

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

# The activated interactive PATH may legitimately retain additional host
# entries, while compatibility children receive the deliberately restricted
# path declared by activate-compat-system. Both paths must independently retain
# the repository-local toolchain, and the child must retain activated Makevars.
environment_names <- c(
  "PATH", "R_MAKEVARS_USER", "PARADOX_COMPAT_SYSTEM_CHILD_PATH"
)
environment_before <- Sys.getenv(environment_names, unset = NA_character_)
names(environment_before) <- environment_names
restore_environment <- function() {
  present <- !is.na(environment_before)
  if (any(present)) {
    do.call(Sys.setenv, as.list(environment_before[present]))
  }
  if (any(!present)) Sys.unsetenv(environment_names[!present])
}
on.exit(restore_environment(), add = TRUE)
toolchain_bin <- normalizePath(file.path(root, ".local", "toolchain", "bin"),
  winslash = "/", mustWork = TRUE)
extra_bin <- file.path(scratch, "activation-extra-bin")
dir.create(extra_bin)
restricted_child_path <- paste(c(toolchain_bin, "/usr/bin", "/bin"),
  collapse = .Platform$path.sep)
activation_fixture_path <- paste(c(toolchain_bin, extra_bin, "/usr/bin", "/bin"),
  collapse = .Platform$path.sep)
activated_makevars <- environment_before[["R_MAKEVARS_USER"]]
if (is.na(activated_makevars) || !nzchar(activated_makevars)) {
  stop("repository-runner fixture lacks activated Makevars", call. = FALSE)
}
Sys.setenv(
  PATH = activation_fixture_path,
  R_MAKEVARS_USER = activated_makevars,
  PARADOX_COMPAT_SYSTEM_CHILD_PATH = restricted_child_path
)
compat_child <- c(
  PATH = restricted_child_path,
  R_MAKEVARS_USER = activated_makevars,
  PARADOX_COMPAT_FIXTURE = "restricted-child"
)
if (!identical(
    repository_runner_base_child_environment(root, compat_child),
    compat_child
  )) {
  stop("restricted compatibility child environment was not preserved",
    call. = FALSE)
}
tampered_child_path <- compat_child
tampered_child_path[["PATH"]] <- activation_fixture_path
expect_error(
  repository_runner_base_child_environment(root, tampered_child_path),
  "compatibility-system child PATH differs from its declared value"
)
Sys.setenv(PARADOX_COMPAT_SYSTEM_CHILD_PATH = paste(c("/usr/bin", "/bin"),
  collapse = .Platform$path.sep))
tampered_child_path[["PATH"]] <- Sys.getenv(
  "PARADOX_COMPAT_SYSTEM_CHILD_PATH"
)
expect_error(
  repository_runner_base_child_environment(root, tampered_child_path),
  "repository-local toolchain is absent from compatibility-system child PATH"
)
Sys.setenv(PARADOX_COMPAT_SYSTEM_CHILD_PATH = restricted_child_path)
tampered_makevars <- compat_child
tampered_makevars[["R_MAKEVARS_USER"]] <- "/dev/null"
expect_error(
  repository_runner_base_child_environment(root, tampered_makevars),
  "compatibility-system child Makevars differs from activation"
)
Sys.setenv(PATH = paste(c(extra_bin, "/usr/bin", "/bin"),
  collapse = .Platform$path.sep))
expect_error(
  repository_runner_base_child_environment(root, compat_child),
  "repository-local toolchain is absent from activated PATH"
)
restore_environment()

# Exercise the shared shell authenticator against a detached source while its
# primary checkout contains unrelated worktree changes.  Keep the complete Git
# graph below this test's scratch root: verification workers mount the real
# checkout and its .git directory read-only.
auth_primary <- file.path(scratch, "auth-primary")
auth_toolchain <- file.path(auth_primary, ".local", "toolchain")
dir.create(file.path(auth_toolchain, "bin"), recursive = TRUE)
auth_git <- file.path(auth_toolchain, "bin", "git")
if (!file.copy(git, auth_git, copy.mode = TRUE) ||
    !file.symlink(file.path(root, ".local", "toolchain", "lib"),
      file.path(auth_toolchain, "lib"))) {
  stop("could not construct the detached-authentication toolchain fixture",
    call. = FALSE)
}
git_run(auth_primary, c("init", "--quiet"))
git_run(auth_primary, c("config", "user.name", "Repository Runner"))
git_run(auth_primary, c("config", "user.email", "runner@example.invalid"))
write_file(file.path(auth_primary, ".gitignore"), ".local/")
write_file(file.path(auth_primary, "candidate"), "candidate")
git_run(auth_primary, c("add", "--", ".gitignore", "candidate"))
git_run(auth_primary, c("commit", "--quiet", "-m", "candidate"))
auth_commit <- git_run(auth_primary,
  c("rev-parse", "--verify", "HEAD^{commit}"))
auth_tree <- git_run(auth_primary,
  c("rev-parse", "--verify", "HEAD^{tree}"))
auth_ref <- "refs/paradox-selftest/repository-runner"
auth_source <- file.path(scratch, "detached-auth-source")
git_run(auth_primary, c("update-ref", auth_ref, auth_commit))
git_run(auth_primary, c("worktree", "add", "--quiet", "--detach", auth_source,
  auth_commit))
write_file(file.path(auth_primary, "unrelated-primary-dirt"), "dirty")
auth_path <- paste(c(dirname(auth_git), Sys.getenv("PATH")),
  collapse = .Platform$path.sep)
auth_result <- processx::run(file.path(root, "compat",
  "authenticate-candidate-git"),
  c(auth_primary, auth_ref, auth_commit, auth_tree, auth_source),
  wd = auth_primary, env = c(PATH = auth_path), stdout = "|", stderr = "|",
  error_on_status = FALSE, cleanup_tree = TRUE)
if (!identical(auth_result$status, 0L) || !identical(trimws(auth_result$stdout),
    "candidate_git_authentication=passed")) {
  stop("five-argument detached candidate authentication failed: ",
    auth_result$stderr, call. = FALSE)
}

# Reproduce the exact-provider property that matters across independent
# dependency runs.  Two run-local evidence paths authenticate the same pinned
# Git tree.  Both must resolve to one repository/commit-keyed installation
# source, and the commit timestamp fixes the only intentional time-bearing
# Installed DESCRIPTION field.  Differing HOME, TMPDIR, and run-local source
# paths must therefore still produce byte-identical installs at the same
# shared-library endpoint.
# Use the release profile's real, pure-R exact provider.  Generic R lazy-load
# databases are not promised to be reproducible for arbitrary packages, while
# this is the exact tree whose repeatability the P1/P2 dependency boundary
# requires and whose source restrictions the producer checks.
provider_consumer_root <- normalizePath(
  file.path(root, ".local", "compat", "github"),
  winslash = "/",
  mustWork = TRUE
)
provider_repository <- "rush"
provider_checkout <- file.path(provider_consumer_root, provider_repository)
provider_origin <- "https://github.com/mlr-org/rush.git"
provider_commit <- "939886b43d5e48afacf2f0e1b06a45ab3c006e19"
provider_tree <- "ca34e7a22145816437161e79a84b7b7a0eb1a0f4"
provider_commit_date <- "2026-07-28T11:15:25+02:00"
observed_commit_date <- git_run(provider_checkout,
  c("show", "-s", "--format=%cI", provider_commit))
if (!identical(observed_commit_date, provider_commit_date)) {
  stop("exact-provider fixture commit timestamp is not fixed: ",
    observed_commit_date, call. = FALSE)
}
provider_authentication <- repository_runner_authenticate_consumer(
  list(git = git, consumer_root = provider_consumer_root),
  data.frame(
    repository = provider_repository,
    origin = provider_origin,
    commit = provider_commit,
    tree = provider_tree,
    stringsAsFactors = FALSE
  )
)

provider_runs <- file.path(scratch, c("provider-run-a", "provider-run-b"))
if (!all(vapply(provider_runs, dir.create, logical(1L)))) {
  stop("could not create exact-provider run fixtures", call. = FALSE)
}
provider_archives <- file.path(provider_runs, "provider.tar")
archive_receipts <- lapply(provider_archives, function(path) {
  repository_runner_create_archive(provider_authentication, path)
})
if (!identical(
    vapply(archive_receipts, `[[`, character(1L), "sha256"),
    rep(archive_receipts[[1L]]$sha256, 2L)
  )) {
  stop("equivalent exact-provider checkouts produced different archives",
    call. = FALSE)
}
provider_extractions <- Map(function(receipt, parent) {
  repository_runner_extract_archive(receipt$path, parent, "retained")$source
}, archive_receipts, provider_runs)
provider_run_trees <- lapply(provider_extractions, function(source) {
  repository_runner_validate_extraction(provider_authentication, source)
})
if (identical(provider_extractions[[1L]], provider_extractions[[2L]]) ||
    !identical(provider_run_trees[[1L]], provider_run_trees[[2L]])) {
  stop("run-local exact-provider sources are not distinct authenticated equivalents",
    call. = FALSE)
}

canonical_provider_parent <- file.path(
  scratch, "exact-provider-sources-v1"
)
dir.create(canonical_provider_parent)
canonical_provider_key <- paste0(provider_repository, "-", provider_commit)
canonical_provider_directory <- file.path(
  canonical_provider_parent, canonical_provider_key
)
canonicalize_provider <- function(archive, run_tree) {
  if (!dir.exists(canonical_provider_directory)) {
    source <- repository_runner_extract_archive(
      archive, canonical_provider_parent, canonical_provider_key
    )$source
  } else {
    source <- repository_runner_require_directory(
      file.path(canonical_provider_directory, "source"),
      "synthetic canonical exact-provider source"
    )
  }
  canonical_tree <- repository_runner_validate_extraction(
    provider_authentication, source
  )
  if (!identical(canonical_tree, run_tree)) {
    stop("canonical exact-provider source differs from run-local evidence",
      call. = FALSE)
  }
  source
}
provider_install_sources <- Map(
  canonicalize_provider,
  provider_archives,
  provider_run_trees
)
canonical_provider_source <- normalizePath(
  file.path(canonical_provider_directory, "source"),
  winslash = "/",
  mustWork = TRUE
)
canonical_chain <- c(
  canonical_provider_parent,
  canonical_provider_directory,
  canonical_provider_source
)
if (!identical(
      unname(unlist(provider_install_sources, use.names = FALSE)),
      rep(canonical_provider_source, 2L)
    ) ||
    any(canonical_provider_source %in% provider_extractions) ||
    !identical(basename(dirname(canonical_provider_source)),
      canonical_provider_key) ||
    any(vapply(canonical_chain, repository_runner_is_symbolic, logical(1L)))) {
  stop("exact-provider installation source is not one plain canonical path",
    call. = FALSE)
}

r_command <- file.path(root, ".local", "toolchain", "bin", "R")
repository_runner_require_file(r_command, "repository-local R executable")
provider_library <- file.path(scratch, "provider-library")
provider_libraries <- rep(provider_library, 2L)
provider_homes <- file.path(scratch, c("provider-home-a", "provider-home-b"))
provider_temps <- file.path(scratch, c("provider-tmp-a", "provider-tmp-b"))
for (path in unique(c(provider_libraries, provider_homes, provider_temps))) {
  dir.create(path)
}
provider_dependency_library <- normalizePath(
  file.path(root, ".local", "compat", "R", "library-dependencies"),
  winslash = "/",
  mustWork = TRUE
)
provider_built_timestamp <- "2026-07-28 09:15:25 UTC"
provider_install_logs <- file.path(provider_runs, "install.log")
provider_hashes <- character(length(provider_libraries))
provider_run_path_leaks <- logical(length(provider_libraries))
provider_built_fields <- character(length(provider_libraries))
installed_tree_contains <- function(path, needle) {
  pattern <- charToRaw(enc2utf8(needle))
  files <- list.files(path, all.files = TRUE, full.names = TRUE,
    recursive = TRUE, include.dirs = FALSE, no.. = TRUE)
  any(vapply(files, function(file) {
    size <- file.info(file, extra_cols = FALSE)$size
    connection <- file(file, open = "rb")
    on.exit(close(connection), add = TRUE)
    bytes <- readBin(connection, what = "raw", n = size)
    length(grepRaw(pattern, bytes, fixed = TRUE)) != 0L
  }, logical(1L)))
}
for (index in seq_along(provider_libraries)) {
  library <- normalizePath(provider_libraries[[index]], winslash = "/",
    mustWork = TRUE)
  library_path <- paste(c(library, provider_dependency_library),
    collapse = .Platform$path.sep)
  install_environment <- c(
    HOME = provider_homes[[index]],
    USER = "paradox",
    LOGNAME = "paradox",
    PATH = restricted_child_path,
    TMPDIR = provider_temps[[index]],
    R_LIBS = library_path,
    R_LIBS_USER = library_path,
    R_LIBS_SITE = "",
    R_ENVIRON = "/dev/null",
    R_ENVIRON_USER = "/dev/null",
    R_PROFILE = "/dev/null",
    R_PROFILE_USER = "/dev/null",
    R_TESTS = "",
    R_MAKEVARS_USER = "/dev/null",
    LC_ALL = "C.UTF-8",
    LANG = "C.UTF-8",
    LANGUAGE = "C",
    TZ = "UTC"
  )
  install <- processx::run(
    r_command,
    c(
      "CMD", "INSTALL", "--preclean", "--clean", "--no-multiarch",
      "--use-vanilla", "--without-keep.source",
      paste0("--built-timestamp=", provider_built_timestamp),
      paste0("--library=", library),
      provider_install_sources[[index]]
    ),
    wd = provider_runs[[index]],
    env = install_environment,
    timeout = 3600L,
    stdout = provider_install_logs[[index]],
    stderr = "2>&1",
    error_on_status = FALSE,
    cleanup_tree = TRUE
  )
  if (!identical(install$status, 0L)) {
    stop("canonical exact-provider installation failed: ",
      paste(readLines(provider_install_logs[[index]], warn = FALSE),
        collapse = "\n"),
      call. = FALSE)
  }
  installed_provider <- repository_runner_require_directory(
    file.path(library, provider_repository),
    "installed exact provider reproducibility target"
  )
  provider_hashes[[index]] <- compat_tree_content_sha256(installed_provider)
  provider_run_path_leaks[[index]] <- any(vapply(
    provider_extractions,
    function(run_source) installed_tree_contains(installed_provider, run_source),
    logical(1L)
  ))
  installed_description <- read.dcf(repository_runner_require_file(
    file.path(installed_provider, "DESCRIPTION"),
    "installed exact-provider DESCRIPTION"
  ))
  provider_built_fields[[index]] <- if (
      "Built" %in% colnames(installed_description)) {
    installed_description[[1L, "Built"]]
  } else {
    ""
  }
  if (any(c("RemoteType", "RemotePkgRef", "Packaged") %in%
      colnames(installed_description))) {
    stop("exact-provider install has unstable provenance metadata",
      call. = FALSE)
  }
}
expected_provider_built <- paste0(
  "R ", as.character(getRversion()), "; ; ",
  provider_built_timestamp, "; ", .Platform$OS.type
)
if (!identical(provider_built_fields, rep(expected_provider_built, 2L)) ||
    !identical(provider_hashes, rep(provider_hashes[[1L]], 2L)) ||
    any(provider_run_path_leaks)) {
  stop(
    "exact-provider installed trees are path-dependent or retain run-local paths: ",
    paste(provider_hashes, collapse = ","), "; leaks=",
    paste(provider_run_path_leaks, collapse = ","), "; Built=",
    paste(provider_built_fields, collapse = "|"),
    call. = FALSE
  )
}

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
  "printf '%s\\n' 'field\tvalue' 'schema\t2' 'profile\tconsumer' \\",
  "  'containment\tdirect' \\",
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
  tool_files = tool_files,
  base_environment = repository_runner_base_child_environment(root)
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
worker_resource <- c(
  "field\tvalue", "schema\t2", "profile\tconsumer",
  "containment\tworker-hard", "platform\tLinux", "online_cpus\t4",
  "affinity_cpus\t4", "cgroup_cpu_limit\t4", "cpu_limit\t4",
  "cpu_reserve\t1", "cpu_per_job\t2", "cpu_jobs\t1",
  "memory_source\tproc_memavailable+cgroup",
  "memory_available_mib\t8192", "cgroup_memory_available_mib\t8192",
  "memory_reserve_mib\t2048", "memory_mib_per_job\t2048",
  "memory_jobs\t3", "profile_max_jobs\t8",
  "operator_max_jobs\tnone", "jobs\t1"
)
invisible(repository_runner_parse_resource_report(worker_resource))

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
# Refill keeps the two-job concurrency bound while packing all three rows
# into one wave: the third worker may start only after a sibling's slot
# freed, and the scheduler needs no second wave for the remainder.
if (read_time("consumer3-start") <
    min(read_time("consumer1-end"), read_time("consumer2-end"))) {
  stop("refill started a third worker before a two-job slot freed",
    call. = FALSE)
}
observed_waves <- list.files(file.path(context$stage, "scheduler", "waves"))
if (length(observed_waves) != 1L) {
  stop("three rows under a two-job ceiling did not form one refill wave: ",
    paste(observed_waves, collapse = ","), call. = FALSE)
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
  "  testthat::expect_identical(Sys.getenv(\"LC_ALL\"), \"C.UTF-8\")",
  "  testthat::expect_identical(Sys.getenv(\"LANG\"), \"C.UTF-8\")",
  "  testthat::expect_identical(Sys.getenv(\"LANGUAGE\"), \"C\")",
  "  testthat::expect_identical(Sys.getenv(\"TZ\"), \"UTC\")",
  "  testthat::expect_identical(nchar(capture.output(cat(\"\\U2208\")), type = \"chars\"), 1L)",
  "  cc <- system2(file.path(R.home(\"bin\"), \"R\"), c(\"CMD\", \"config\", \"CC\"), stdout = TRUE)",
  "  cc <- strsplit(cc[[1L]], \"[[:space:]]+\")[[1L]][[1L]]",
  "  testthat::expect_true(nzchar(Sys.which(cc)))",
  "  testthat::expect_true(file.exists(Sys.getenv(\"R_MAKEVARS_USER\")))",
  "})"
))
development_library <- normalizePath(file.path(root, ".local", "R", "library"),
  winslash = "/", mustWork = TRUE)
child_context <- context
child_context$config$extra_libraries <- c(extra_library, development_library)
child_state <- file.path(scratch, "child-state")
dir.create(child_state)
child_environment <- repository_runner_process_environment(child_context,
  child_state, "syntheticconsumer")
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
    !identical(child_counts[["expectations"]], "18") ||
    !identical(child_counts[["failed"]], "2") ||
    !identical(child_counts[["passed"]], "16")) {
  stop("external child did not retain complete multi-failure counts",
    call. = FALSE)
}

mlr3_receipt_attempt <- file.path(scratch, "mlr3-environment-receipt-attempt")
dir.create(mlr3_receipt_attempt)
dir.create(file.path(mlr3_receipt_attempt, "work"))
mlr3_state <- file.path(mlr3_receipt_attempt, "work", "row-state")
dir.create(mlr3_state)
mlr3_environment <- repository_runner_process_environment(context, mlr3_state,
  "mlr3")
ordinary_parallel <- repository_runner_child_parallel_environment(
  "syntheticconsumer")
mlr3_parallel <- repository_runner_child_parallel_environment("mlr3")
changed_parallel_names <- names(ordinary_parallel)[ordinary_parallel != mlr3_parallel]
expected_changed_parallel_names <- c(
  "MC_CORES", "R_PARALLELLY_AVAILABLECORES_FALLBACK",
  "R_FUTURE_AVAILABLECORES_FALLBACK", "OMP_NUM_THREADS", "OMP_THREAD_LIMIT"
)
fixed_controls <- c(
  TESTTHAT_PARALLEL = "false", TESTTHAT_CPUS = "1", MAKEFLAGS = "-j1"
)
if (!identical(changed_parallel_names, expected_changed_parallel_names) ||
    !identical(unname(mlr3_parallel[expected_changed_parallel_names]),
      rep("2", length(expected_changed_parallel_names))) ||
    !identical(mlr3_parallel[setdiff(names(mlr3_parallel),
      expected_changed_parallel_names)], ordinary_parallel[setdiff(
      names(ordinary_parallel), expected_changed_parallel_names)]) ||
    !identical(mlr3_environment[names(fixed_controls)], fixed_controls)) {
  stop("mlr3's bounded two-CPU child exception is not isolated", call. = FALSE)
}

receipt_attempt <- file.path(scratch, "environment-receipt-attempt")
dir.create(receipt_attempt)
dir.create(file.path(receipt_attempt, "work"))
receipt_state <- file.path(receipt_attempt, "work", "row-state")
dir.create(receipt_state)
receipt_environment <- repository_runner_process_environment(context,
  receipt_state, "syntheticconsumer")
receipt <- repository_runner_environment_receipt(receipt_environment)
invisible(repository_runner_validate_environment_receipt(receipt,
  receipt_attempt, context, "syntheticconsumer"))
mlr3_receipt <- repository_runner_environment_receipt(mlr3_environment)
invisible(repository_runner_validate_environment_receipt(mlr3_receipt,
  mlr3_receipt_attempt, context, "mlr3"))
expect_error(repository_runner_validate_environment_receipt(mlr3_receipt,
  mlr3_receipt_attempt, context, "syntheticconsumer"),
  "does not prove row isolation")
tampered_receipt <- receipt
tampered_receipt$value[tampered_receipt$name == "R_FUTURE_PLAN"] <- "multisession"
expect_error(repository_runner_validate_environment_receipt(tampered_receipt,
  receipt_attempt, context, "syntheticconsumer"), "does not prove row isolation")

external_worker_state <- file.path(scratch, "external-worker-environment")
dir.create(external_worker_state)
external_worker_environment <- repository_runner_worker_environment(
  external_worker_state)
if (!identical(as.character(external_worker_environment[names(nested_parallel)]),
    unname(nested_parallel))) {
  stop("external worker environment omitted a nested-parallelism control",
    call. = FALSE)
}
worker_locale <- c(LC_ALL = "C.UTF-8", LANG = "C.UTF-8", LANGUAGE = "C",
  TZ = "UTC")
if (!identical(unname(as.character(external_worker_environment[
      names(worker_locale)])), unname(worker_locale))) {
  stop("external worker environment is not locale-stable", call. = FALSE)
}

isolation_root <- file.path(scratch, "isolation")
dir.create(isolation_root)
isolation_environment <- repository_runner_process_environment(context,
  isolation_root, "syntheticconsumer")
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
  "exact_provider_reproducibility=passed\n",
  "cache_isolation=passed\n",
  "semantic_counts=passed\n",
  "child_multi_failure_counts=passed\n",
  "nested_parallel_controls=passed\n",
  "nested_parallel_receipt_binding=passed\n",
  "promotion_fail_closed=passed\n", sep = "")
}

repository_runner_selftest()
