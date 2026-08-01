usage <- paste(
  "usage: install-repository-test-dependencies.R",
  "[ROOT [MAX_PRIORITY [LIBRARY]]] --run-id ID",
  "[--evidence-profile NAME]"
)

args <- commandArgs(trailingOnly = TRUE)
positionals <- character()
run_id <- NULL
evidence_profile <- "default"
profile_seen <- FALSE
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
  } else if (identical(argument, "--evidence-profile")) {
    if (profile_seen || index == length(args)) stop(usage, call. = FALSE)
    evidence_profile <- args[[index + 1L]]
    profile_seen <- TRUE
    index <- index + 2L
  } else if (startsWith(argument, "--evidence-profile=")) {
    if (profile_seen) stop(usage, call. = FALSE)
    evidence_profile <- substring(
      argument, nchar("--evidence-profile=") + 1L
    )
    profile_seen <- TRUE
    index <- index + 1L
  } else if (startsWith(argument, "--")) {
    stop(usage, call. = FALSE)
  } else {
    positionals <- c(positionals, argument)
    index <- index + 1L
  }
}
if (length(positionals) > 3L || is.null(run_id)) stop(usage, call. = FALSE)
if (length(run_id) != 1L ||
    !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", run_id) ||
    run_id %in% c(".", "..")) {
  stop("--run-id must be a safe name of at most 128 characters", call. = FALSE)
}
if (length(evidence_profile) != 1L || is.na(evidence_profile) ||
    !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$", evidence_profile) ||
    evidence_profile %in% c(".", "..")) {
  stop("--evidence-profile must be one safe name of at most 64 characters",
    call. = FALSE)
}

root <- if (length(positionals) >= 1L) {
  normalizePath(positionals[[1L]], winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
max_priority <- if (length(positionals) >= 2L) as.integer(positionals[[2L]]) else 0L
if (length(max_priority) != 1L || is.na(max_priority) || max_priority < 0L ||
    (length(positionals) >= 2L &&
      !identical(as.character(max_priority), positionals[[2L]]))) {
  stop("MAX_PRIORITY must be one non-negative integer", call. = FALSE)
}
library <- if (length(positionals) >= 3L) {
  positionals[[3L]]
} else {
  file.path(root, ".local", "compat", "R", "library-dependencies")
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
  stop("dependency installation is not running under repository-local R", call. = FALSE)
}

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

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

plain_child_directory <- function(parent, name, label, create, must_be_new = FALSE) {
  parent <- require_plain_directory(parent, paste0(label, " parent"))
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  path <- file.path(parent, name)
  if (dir.exists(path)) {
    if (must_be_new) {
      stop(label, " already exists and cannot be reused: ", path, call. = FALSE)
    }
    require_plain_directory(path, label)
  } else {
    if (file.exists(path) || is_symbolic(path)) {
      stop(label, " exists but is not a plain directory: ", path, call. = FALSE)
    }
    if (!create || !dir.create(path, recursive = FALSE, showWarnings = FALSE)) {
      stop("could not create ", label, ": ", path, call. = FALSE)
    }
    require_plain_directory(path, label)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(dirname(path), parent)) {
    stop(label, " escaped its plain parent", call. = FALSE)
  }
  path
}

profile_helper_path <- file.path(
  root, "compat", "downstream-evidence-profile.R"
)
if (!file.exists(profile_helper_path) || dir.exists(profile_helper_path) ||
    is_symbolic(profile_helper_path)) {
  stop("downstream evidence profile helper is absent or symbolic",
    call. = FALSE)
}
sys.source(profile_helper_path, envir = environment(), keep.source = FALSE)
profile <- downstream_evidence_profile(root, evidence_profile)

local_root <- require_plain_directory(
  file.path(root, ".local"),
  "repository-local state root"
)
local_root <- normalizePath(local_root, winslash = "/", mustWork = TRUE)
compat_root <- plain_child_directory(local_root, "compat", "compatibility state root", TRUE)
runs_root <- plain_child_directory(compat_root, "runs", "retained compatibility root", TRUE)
run_directory <- plain_child_directory(
  runs_root, run_id, "run directory", TRUE, must_be_new = TRUE
)
stage_name <- sprintf(
  "repository-dependencies-priority-%d%s", max_priority, profile$profile_suffix
)
stage_directory <- plain_child_directory(
  run_directory,
  stage_name,
  "repository dependency evidence",
  TRUE,
  must_be_new = TRUE
)
metadata_directory <- plain_child_directory(
  stage_directory,
  "metadata",
  "dependency evidence metadata directory",
  TRUE,
  must_be_new = TRUE
)
result_path <- file.path(
  stage_directory,
  sprintf("dependency-install-priority-%d.tsv", max_priority)
)

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

started_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
profile_registry_path <- profile$registry
axis_registry_path <- profile$axis_registry
manifest_path <- profile$dependency_repository_manifest
snapshot_path <- profile$dependency_snapshot
consumer_root <- profile$dependency_consumer_root
harness_path <- file.path(root, "compat", "install-repository-test-dependencies.R")
fingerprint_path <- file.path(root, "compat", "fingerprint.R")
evidence_helper_path <- file.path(root, "compat", "repository-evidence.R")
evidence_verifier_path <- file.path(root, "compat", "verify-repository-evidence.R")
repository_runner_path <- file.path(root, "compat", "repository-runner.R")
compat_system_evidence_path <- file.path(
  root, "compat", "compat-system-evidence.R"
)
sys.source(fingerprint_path, envir = environment())
sys.source(evidence_helper_path, envir = environment())
sys.source(repository_runner_path, envir = environment())
sys.source(compat_system_evidence_path, envir = environment())
expected_git <- file.path(root, ".local", "toolchain", "bin", "git")
if (!file.exists(expected_git) || dir.exists(expected_git) ||
    is_symbolic(expected_git) ||
    !identical(unname(Sys.which("git")), expected_git)) {
  stop("dependency preparation requires the activated repository-local Git",
    call. = FALSE)
}
repository_runner_assert_git_environment()

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
  output <- repository_runner_git_run(
    expected_git,
    repository,
    arguments,
    label
  )$stdout
  if (!nzchar(output)) character() else
    strsplit(sub("\n$", "", output), "\n", fixed = TRUE)[[1L]]
}

single_git_value <- function(repository, arguments, label) {
  repository_runner_git_value(expected_git, repository, arguments, label)
}

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

selected <- downstream_evidence_dependency_selection(manifest, max_priority)
if (!nrow(selected)) stop("GitHub dependency selection is empty", call. = FALSE)
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
object_hash_pattern <- "^([0-9a-f]{40}|[0-9a-f]{64})$"
if (any(!grepl(object_hash_pattern, selected_snapshot$commit))) {
  stop("GitHub snapshot contains a malformed commit", call. = FALSE)
}

repository_checkout_state <- function(repository, expected_commit, expected_origin) {
  checkout <- file.path(consumer_root, repository)
  if (!file.exists(file.path(checkout, ".git"))) {
    stop("Git checkout is missing for ", repository, call. = FALSE)
  }
  if (is_symbolic(checkout)) {
    stop("Git checkout is symbolic for ", repository, call. = FALSE)
  }
  checkout <- normalizePath(checkout, winslash = "/", mustWork = TRUE)
  expected_tree <- single_git_value(
    checkout,
    c("rev-parse", "--verify", paste0(expected_commit, "^{tree}")),
    paste0("reading ", repository, " tree")
  )
  authentication <- repository_runner_authenticate_consumer(
    list(git = expected_git, consumer_root = consumer_root),
    data.frame(
      repository = repository,
      origin = expected_origin,
      commit = expected_commit,
      tree = expected_tree,
      stringsAsFactors = FALSE
    )
  )
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
  data.frame(
    repository = repository,
    checkout = checkout,
    expected_commit = expected_commit,
    observed_commit = authentication$commit,
    expected_origin = expected_origin,
    observed_origin = origin,
    clean = !length(status),
    status = gsub("[\r\n\t]+", " ", paste(status, collapse = " | ")),
    valid = identical(authentication$commit, expected_commit) &&
      !length(status) && identical(origin, expected_origin),
    stringsAsFactors = FALSE
  )
}

checkout_state <- function(index) {
  repository_checkout_state(
    selected$repository[[index]],
    selected_snapshot$commit[[index]],
    selected_snapshot$url[[index]]
  )
}

# This complete checkout preflight deliberately precedes creation or mutation
# of the shared dependency library.
checkout_states <- lapply(seq_len(nrow(selected)), checkout_state)
checkout_preflight <- do.call(rbind, checkout_states)
invalid_checkouts <- checkout_preflight$repository[!checkout_preflight$valid]
if (length(invalid_checkouts)) {
  stop(
    "GitHub checkout commit, origin, or clean-tree validation failed: ",
    paste(invalid_checkouts, collapse = ", "),
    call. = FALSE
  )
}
checkout_preflight$status[!nzchar(checkout_preflight$status)] <- "-"

clone_manifest <- manifest[manifest$action == "clone", , drop = FALSE]
clone_snapshot_index <- match(clone_manifest$repository, snapshot$repository)
if (anyNA(clone_snapshot_index)) {
  stop("cloned repositories are absent from github-snapshot.tsv", call. = FALSE)
}
clone_snapshot <- snapshot[clone_snapshot_index, , drop = FALSE]
if (!identical(clone_manifest$repository, clone_snapshot$repository) ||
    !identical(clone_manifest$url, clone_snapshot$url) ||
    !identical(clone_manifest$priority, clone_snapshot$priority) ||
    any(!grepl(object_hash_pattern, clone_snapshot$commit))) {
  stop("GitHub snapshot disagrees with cloned fallback providers", call. = FALSE)
}

fallback_provider_rows <- list()
local_packages <- list()
local_repository_packages <- character()
for (index in seq_len(nrow(clone_manifest))) {
  repository <- clone_manifest$repository[[index]]
  checkout <- file.path(consumer_root, repository)
  description <- file.path(checkout, "DESCRIPTION")
  if (!file.exists(description)) next
  if (dir.exists(description) || is_symbolic(description)) {
    stop("Fallback DESCRIPTION is not one regular file: ", description, call. = FALSE)
  }
  fields <- read.dcf(description, fields = "Package")
  package <- unname(fields[[1L, "Package"]])
  if (length(package) != 1L || is.na(package) ||
      !grepl("^[A-Za-z][A-Za-z0-9.]*$", package)) {
    stop("Fallback checkout has an invalid Package field: ", repository, call. = FALSE)
  }
  if (!is.null(local_packages[[package]])) {
    stop("Multiple reviewed checkouts provide package ", package, call. = FALSE)
  }
  state <- repository_checkout_state(
    repository,
    clone_snapshot$commit[[index]],
    clone_snapshot$url[[index]]
  )
  fallback_provider_rows[[length(fallback_provider_rows) + 1L]] <- cbind(
    data.frame(package = package, stringsAsFactors = FALSE),
    state
  )
  local_packages[[package]] <- state$checkout[[1L]]
  local_repository_packages[[repository]] <- package
}
if (!length(fallback_provider_rows)) {
  stop("No pinned local fallback package providers were found", call. = FALSE)
}
fallback_checkout_preflight <- do.call(rbind, fallback_provider_rows)
invalid_fallbacks <- fallback_checkout_preflight$repository[
  !fallback_checkout_preflight$valid
]
if (length(invalid_fallbacks)) {
  stop(
    "Fallback checkout commit, origin, or clean-tree validation failed: ",
    paste(invalid_fallbacks, collapse = ", "),
    call. = FALSE
  )
}
fallback_checkout_preflight$status[!nzchar(fallback_checkout_preflight$status)] <- "-"

exact_provider_directory <- plain_child_directory(
  stage_directory,
  "exact-provider-sources",
  "exact dependency provider source directory",
  TRUE,
  must_be_new = TRUE
)
provider_source_roots <- character()
provider_source_rows <- list()
exact_provider_indices <- which(selected$relation == "ExactDependency")
for (index in exact_provider_indices) {
  repository <- selected$repository[[index]]
  package <- unname(local_repository_packages[[repository]])
  checkout <- file.path(consumer_root, repository)
  tree <- single_git_value(
    checkout,
    c("rev-parse", "--verify",
      paste0(selected_snapshot$commit[[index]], "^{tree}")),
    paste0("reading exact provider tree for ", repository)
  )
  authentication <- repository_runner_authenticate_consumer(
    list(git = expected_git, consumer_root = consumer_root),
    data.frame(
      repository = repository,
      origin = selected_snapshot$url[[index]],
      commit = selected_snapshot$commit[[index]],
      tree = tree,
      stringsAsFactors = FALSE
    )
  )
  archive <- repository_runner_create_archive(
    authentication,
    file.path(exact_provider_directory, paste0(repository, ".tar"))
  )
  extraction <- repository_runner_extract_archive(
    archive$path,
    exact_provider_directory,
    repository
  )
  tree_manifest <- repository_runner_validate_extraction(
    authentication,
    extraction$source
  )
  tree_manifest_path <- file.path(
    exact_provider_directory,
    paste0(repository, "-tree.tsv")
  )
  write_tsv(tree_manifest, tree_manifest_path)
  source_package <- unname(read.dcf(
    file.path(extraction$source, "DESCRIPTION"),
    fields = "Package"
  )[[1L, "Package"]])
  if (length(package) != 1L || is.na(package) || !nzchar(package) ||
      !identical(source_package, package)) {
    stop("exact provider package identity differs from its authenticated source",
      call. = FALSE)
  }
  provider_source_roots[[repository]] <- extraction$source
  provider_source_rows[[length(provider_source_rows) + 1L]] <- data.frame(
    repository = repository,
    package = package,
    commit = authentication$commit,
    tree = authentication$tree,
    archive_sha256 = archive$sha256,
    tree_manifest_sha256 = unname(tools::sha256sum(tree_manifest_path)),
    source = extraction$source,
    stringsAsFactors = FALSE
  )
}
exact_provider_sources <- if (length(provider_source_rows)) {
  do.call(rbind, provider_source_rows)
} else {
  data.frame(
    repository = character(), package = character(), commit = character(),
    tree = character(), archive_sha256 = character(),
    tree_manifest_sha256 = character(), source = character(),
    stringsAsFactors = FALSE
  )
}
exact_provider_sources_path <- file.path(
  metadata_directory,
  "exact-provider-sources.tsv"
)
write_tsv(exact_provider_sources, exact_provider_sources_path)
exact_provider_packages <- unname(local_repository_packages[
  selected$repository[exact_provider_indices]
])

if (dir.exists(library)) {
  require_plain_directory(library, "dependency library")
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)
} else {
  if (file.exists(library) || is_symbolic(library)) {
    stop("dependency library exists but is not a plain directory: ", library, call. = FALSE)
  }
  library_parent <- dirname(library)
  require_plain_directory(library_parent, "dependency library parent")
  library_parent <- normalizePath(library_parent, winslash = "/", mustWork = TRUE)
  if (!startsWith(library_parent, paste0(local_root, "/"))) {
    stop(
      "dependency library parent must remain below the repository-local state root",
      call. = FALSE
    )
  }
  library <- file.path(library_parent, basename(library))
  if (!dir.create(library, recursive = FALSE, showWarnings = FALSE)) {
    stop("could not create dependency library: ", library, call. = FALSE)
  }
  require_plain_directory(library, "dependency library")
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)
}
if (!startsWith(library, paste0(local_root, "/"))) {
  stop("dependency library must remain below the repository-local state root", call. = FALSE)
}
dependency_content_before <- compat_tree_content_sha256(library)

checkout_preflight_path <- file.path(metadata_directory, "checkout-preflight.tsv")
write_tsv(checkout_preflight, checkout_preflight_path)
fallback_checkout_preflight_path <- file.path(
  metadata_directory,
  "fallback-checkout-preflight.tsv"
)
write_tsv(fallback_checkout_preflight, fallback_checkout_preflight_path)

copied <- file.copy(
  c(
    profile_registry_path, axis_registry_path, profile_helper_path, manifest_path,
    snapshot_path,
    harness_path, fingerprint_path,
    evidence_helper_path, evidence_verifier_path, repository_runner_path,
    compat_system_evidence_path
  ),
  metadata_directory,
  copy.mode = TRUE,
  copy.date = TRUE
)
if (!all(copied)) stop("could not retain dependency evidence inputs", call. = FALSE)
compat_system_evidence <- compat_system_capture_evidence(root, metadata_directory)
run_metadata <- data.frame(
  field = c(
    "schema", "stage_kind", "run_id", "evidence_profile", "started_utc",
    "root", "max_priority",
    "dependency_library", "dependency_library_content_before", "r",
    "r_version", "profile_registry_sha256", "axis_registry_sha256",
    "profile_helper_sha256",
    "github_manifest_sha256", "github_snapshot_sha256",
    "harness_sha256", "fingerprint_sha256", "evidence_helper_sha256",
    "evidence_verifier_sha256", "repository_runner_sha256",
    "checkout_preflight_sha256", "fallback_checkout_preflight_sha256",
    "exact_provider_sources_sha256", "result_ledger"
  ),
  value = c(
    "4", "repository_dependencies", run_id, evidence_profile, started_utc, root,
    as.character(max_priority), library,
    dependency_content_before,
    normalizePath(file.path(R.home(), "bin", "R"), winslash = "/", mustWork = TRUE),
    as.character(getRversion()),
    unname(tools::sha256sum(profile_registry_path)),
    unname(tools::sha256sum(axis_registry_path)),
    unname(tools::sha256sum(profile_helper_path)),
    unname(tools::sha256sum(manifest_path)),
    unname(tools::sha256sum(snapshot_path)), unname(tools::sha256sum(harness_path)),
    unname(tools::sha256sum(fingerprint_path)),
    unname(tools::sha256sum(evidence_helper_path)),
    unname(tools::sha256sum(evidence_verifier_path)),
    unname(tools::sha256sum(repository_runner_path)),
    unname(tools::sha256sum(checkout_preflight_path)),
    unname(tools::sha256sum(fallback_checkout_preflight_path)),
    unname(tools::sha256sum(exact_provider_sources_path)), result_path
  ),
  stringsAsFactors = FALSE
)
write_tsv(run_metadata, file.path(metadata_directory, "run.tsv"))

checkout_commit <- function(checkout) {
  if (!file.exists(file.path(checkout, ".git"))) return(NA_character_)
  single_git_value(
    checkout,
    c("rev-parse", "--verify", "HEAD^{commit}"),
    paste0("reading checkout commit for ", checkout)
  )
}

missing_package_names <- function(message) {
  matches <- regmatches(
    message,
    gregexpr("Can't find package called [[:alnum:].]+", message, perl = TRUE)
  )[[1L]]
  if (identical(matches, character(0L)) || identical(matches, "")) return(character())
  unique(sub("[.]$", "", sub("^Can't find package called ", "", matches)))
}

compact_error <- function(message, limit = 4000L) {
  message <- gsub("[\r\n\t]+", " ", message)
  if (nchar(message, type = "chars") <= limit) return(message)

  side <- as.integer((limit - 80L) / 2L)
  paste0(
    substr(message, 1L, side),
    " [... verbose pak build output omitted; decisive tail follows ...] ",
    substr(message, nchar(message, type = "chars") - side + 1L, nchar(message, type = "chars"))
  )
}

install_development_dependencies <- function(checkout) {
  installed_local_sources <- character()

  repeat {
    condition <- tryCatch({
      pak::local_install_dev_deps(
        root = checkout,
        lib = library,
        upgrade = FALSE,
        ask = FALSE,
        dependencies = TRUE
      )
      NULL
    }, error = identity)

    if (is.null(condition)) {
      return(list(error = "", local_sources = installed_local_sources))
    }

    missing <- setdiff(missing_package_names(conditionMessage(condition)), installed_local_sources)
    available_locally <- missing[
      missing %in% names(local_packages) &
        !missing %in% exact_provider_packages
    ]
    if (!length(available_locally)) {
      return(list(error = conditionMessage(condition), local_sources = installed_local_sources))
    }

    package <- available_locally[[1L]]
    local_checkout <- local_packages[[package]]
    message("Resolver cannot discover ", package, "; installing pinned local checkout")

    local_condition <- tryCatch({
      # This package is a dependency of the selected consumer, not another
      # consumer test target. Its hard dependencies are sufficient here; its
      # own development dependencies are prepared when its priority is run.
      pak::local_install(
        root = local_checkout,
        lib = library,
        upgrade = FALSE,
        ask = FALSE,
        dependencies = NA
      )
      NULL
    }, error = identity)

    installed_local_sources <- c(installed_local_sources, package)
    if (!is.null(local_condition)) {
      error <- paste0(
        conditionMessage(condition),
        " Local checkout fallback for ", package, " failed: ",
        conditionMessage(local_condition)
      )
      return(list(error = error, local_sources = installed_local_sources))
    }
  }
}

validate_exact_dependency_provider_install <- function(
    repository, expected_content_sha256 = NULL) {
  package <- unname(local_repository_packages[[repository]])
  source <- unname(provider_source_roots[[repository]])
  if (length(package) != 1L || is.na(package) || !nzchar(package) ||
      length(source) != 1L || is.na(source) || !nzchar(source)) {
    stop("exact dependency provider is absent from its source map")
  }
  source_description_path <- repository_runner_require_file(
    file.path(source, "DESCRIPTION"),
    "exact dependency provider source DESCRIPTION"
  )
  source_description <- read.dcf(
    source_description_path,
    fields = c("Package", "Version")
  )
  installed <- file.path(library, package)
  require_plain_directory(installed, "installed exact dependency provider")
  installed_description <- file.path(installed, "DESCRIPTION")
  if (!file.exists(installed_description) ||
      dir.exists(installed_description) ||
      is_symbolic(installed_description)) {
    stop("installed provider DESCRIPTION is absent or not regular")
  }
  observed <- read.dcf(
    installed_description,
    fields = c("Package", "Version", "RemoteType", "RemotePkgRef")
  )
  expected <- c(
    Package = unname(source_description[[1L, "Package"]]),
    Version = unname(source_description[[1L, "Version"]]),
    RemoteType = "local",
    RemotePkgRef = paste0("local::", source)
  )
  if (!identical(unname(observed[1L, names(expected)]), unname(expected))) {
    stop(
      "installed provider identity or local-source provenance does not match ",
      "its exact source"
    )
  }
  content_sha256 <- compat_tree_content_sha256(installed)
  if (!is.null(expected_content_sha256) &&
      !identical(content_sha256, expected_content_sha256)) {
    stop("installed exact dependency provider changed after installation")
  }
  data.frame(
    repository = repository,
    package = package,
    version = expected[["Version"]],
    remote_type = expected[["RemoteType"]],
    remote_pkg_ref = expected[["RemotePkgRef"]],
    content_sha256 = content_sha256,
    stringsAsFactors = FALSE
  )
}

install_exact_dependency_provider <- function(repository, checkout) {
  package <- unname(local_repository_packages[[repository]])
  source <- unname(provider_source_roots[[repository]])
  if (length(package) != 1L || is.na(package) || !nzchar(package) ||
      is.null(local_packages[[package]]) ||
      !identical(local_packages[[package]], checkout) ||
      length(source) != 1L || is.na(source) || !nzchar(source)) {
    return(list(
      error = paste0(
        "Exact dependency provider is absent from the authenticated local ",
        "package map: ", repository
      ),
      local_sources = character()
    ))
  }
  receipt <- NULL
  condition <- tryCatch({
    pak::local_install(
      root = source,
      lib = library,
      upgrade = FALSE,
      ask = FALSE,
      dependencies = NA
    )
    NULL
  }, error = identity)
  if (is.null(condition)) {
    condition <- tryCatch({
      receipt <- validate_exact_dependency_provider_install(repository)
      NULL
    }, error = identity)
  }
  list(
    error = if (is.null(condition)) "" else conditionMessage(condition),
    local_sources = package,
    provider_receipt = receipt
  )
}

empty_results <- function() {
  data.frame(
    run_id = character(),
    repository = character(),
    commit = character(),
    priority = integer(),
    status = character(),
    elapsed_seconds = numeric(),
    local_sources = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
}

results <- vector("list", nrow(selected))
provider_install_receipts <- list()
for (i in seq_len(nrow(selected))) {
  repository <- selected$repository[[i]]
  checkout <- file.path(consumer_root, repository)
  message("Installing development dependencies for ", repository)

  started <- Sys.time()
  outcome <- tryCatch({
    if (!file.exists(file.path(checkout, "DESCRIPTION"))) {
      stop("Checkout or DESCRIPTION is missing: ", checkout)
    }
    if (identical(selected$relation[[i]], "ExactDependency")) {
      install_exact_dependency_provider(repository, checkout)
    } else {
      install_development_dependencies(checkout)
    }
  }, error = function(condition) {
    list(error = conditionMessage(condition), local_sources = character())
  })
  checkout_after_error <- ""
  checkout_after <- tryCatch(
    checkout_state(i),
    error = function(condition) {
      checkout_after_error <<- conditionMessage(condition)
      NULL
    }
  )
  if (is.null(checkout_after) || !isTRUE(checkout_after$valid[[1L]])) {
    provenance_error <- paste0(
      "Checkout provenance changed while preparing ", repository,
      if (nzchar(checkout_after_error)) paste0(": ", checkout_after_error) else ""
    )
    outcome$error <- paste(c(outcome$error[nzchar(outcome$error)], provenance_error),
      collapse = "\n")
  }
  if (identical(selected$relation[[i]], "ExactDependency") &&
      is.data.frame(outcome$provider_receipt) &&
      nrow(outcome$provider_receipt) == 1L) {
    provider_install_receipts[[repository]] <- outcome$provider_receipt
  }

  results[[i]] <- data.frame(
    run_id = run_id,
    repository = repository,
    commit = checkout_commit(checkout),
    priority = selected$priority[[i]],
    status = if (nzchar(outcome$error)) "failed" else "passed",
    elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    local_sources = paste(vapply(outcome$local_sources, function(package) {
      paste0(package, "@", checkout_commit(local_packages[[package]]))
    }, character(1L)), collapse = ","),
    error = compact_error(outcome$error),
    stringsAsFactors = FALSE
  )
}

results <- if (length(results)) do.call(rbind, results) else empty_results()
provider_terminal_rows <- list()
for (index in exact_provider_indices) {
  repository <- selected$repository[[index]]
  provider_index <- match(repository, exact_provider_sources$repository)
  condition <- tryCatch({
    if (is.na(provider_index)) {
      stop("exact dependency provider is absent from its source ledger")
    }
    initial_receipt <- provider_install_receipts[[repository]]
    if (!is.data.frame(initial_receipt) || nrow(initial_receipt) != 1L) {
      stop("exact dependency provider has no initial installation receipt")
    }
    source_row <- exact_provider_sources[provider_index, , drop = FALSE]
    expected_source <- normalizePath(
      file.path(exact_provider_directory, repository, "source"),
      winslash = "/",
      mustWork = TRUE
    )
    archive_path <- file.path(
      exact_provider_directory,
      paste0(repository, ".tar")
    )
    tree_manifest_path <- file.path(
      exact_provider_directory,
      paste0(repository, "-tree.tsv")
    )
    if (!identical(source_row$source[[1L]], expected_source) ||
        !identical(
          unname(tools::sha256sum(archive_path)),
          source_row$archive_sha256[[1L]]
        ) ||
        !identical(
          unname(tools::sha256sum(tree_manifest_path)),
          source_row$tree_manifest_sha256[[1L]]
        )) {
      stop("retained exact dependency provider source changed")
    }
    authentication <- repository_runner_authenticate_consumer(
      list(git = expected_git, consumer_root = consumer_root),
      data.frame(
        repository = repository,
        origin = selected_snapshot$url[[index]],
        commit = selected_snapshot$commit[[index]],
        tree = source_row$tree[[1L]],
        stringsAsFactors = FALSE
      )
    )
    retained_tree <- read_manifest(
      tree_manifest_path,
      c("mode", "object", "path", "size", "sha256")
    )
    observed_tree <- repository_runner_validate_extraction(
      authentication,
      expected_source
    )
    if (!identical(retained_tree, observed_tree)) {
      stop("exact dependency provider extraction changed after installation")
    }
    final_receipt <- validate_exact_dependency_provider_install(
      repository,
      initial_receipt$content_sha256[[1L]]
    )
    if (!identical(
        initial_receipt[, c(
          "repository", "package", "version", "remote_type", "remote_pkg_ref"
        ), drop = FALSE],
        final_receipt[, c(
          "repository", "package", "version", "remote_type", "remote_pkg_ref"
        ), drop = FALSE]
      )) {
      stop("exact dependency provider identity changed after installation")
    }
    provider_terminal_rows[[length(provider_terminal_rows) + 1L]] <- data.frame(
      repository = repository,
      package = initial_receipt$package[[1L]],
      version = initial_receipt$version[[1L]],
      remote_type = initial_receipt$remote_type[[1L]],
      remote_pkg_ref = initial_receipt$remote_pkg_ref[[1L]],
      initial_content_sha256 = initial_receipt$content_sha256[[1L]],
      final_content_sha256 = final_receipt$content_sha256[[1L]],
      stringsAsFactors = FALSE
    )
    NULL
  }, error = identity)
  if (!is.null(condition)) {
    result_index <- match(repository, results$repository)
    results$status[[result_index]] <- "failed"
    results$error[[result_index]] <- compact_error(paste(
      c(
        results$error[[result_index]][nzchar(results$error[[result_index]])],
        conditionMessage(condition)
      ),
      collapse = "\n"
    ))
  }
}
exact_provider_installs <- if (length(provider_terminal_rows)) {
  do.call(rbind, provider_terminal_rows)
} else {
  data.frame(
    repository = character(), package = character(), version = character(),
    remote_type = character(), remote_pkg_ref = character(),
    initial_content_sha256 = character(), final_content_sha256 = character(),
    stringsAsFactors = FALSE
  )
}
exact_provider_installs_path <- file.path(
  metadata_directory,
  "exact-provider-installs.tsv"
)
write_tsv(exact_provider_installs, exact_provider_installs_path)
output <- results
for (field in c("commit", "local_sources", "error")) {
  output[[field]][is.na(output[[field]]) | !nzchar(output[[field]])] <- "-"
}
write_tsv(output, result_path)
dependency_content_after <- compat_tree_content_sha256(library)
checkout_postflight <- do.call(rbind, lapply(seq_len(nrow(selected)), checkout_state))
checkout_postflight$status[!nzchar(checkout_postflight$status)] <- "-"
checkout_postflight_path <- file.path(metadata_directory, "checkout-postflight.tsv")
write_tsv(checkout_postflight, checkout_postflight_path)
checkout_postflight_failed <- any(!checkout_postflight$valid)
fallback_checkout_postflight <- do.call(rbind, lapply(
  seq_len(nrow(fallback_checkout_preflight)),
  function(index) {
    state <- repository_checkout_state(
      fallback_checkout_preflight$repository[[index]],
      fallback_checkout_preflight$expected_commit[[index]],
      fallback_checkout_preflight$expected_origin[[index]]
    )
    cbind(
      data.frame(
        package = fallback_checkout_preflight$package[[index]],
        stringsAsFactors = FALSE
      ),
      state
    )
  }
))
fallback_checkout_postflight$status[!nzchar(fallback_checkout_postflight$status)] <- "-"
fallback_checkout_postflight_path <- file.path(
  metadata_directory,
  "fallback-checkout-postflight.tsv"
)
write_tsv(fallback_checkout_postflight, fallback_checkout_postflight_path)
fallback_checkout_postflight_failed <- any(!fallback_checkout_postflight$valid)
compat_system_verify_evidence(
  compat_system_evidence,
  "during repository dependency completion"
)
completion <- data.frame(
  field = c(
    "schema", "stage_kind", "run_id", "evidence_profile", "max_priority",
    "finished_utc", "status",
    "result_rows", "failed_rows", "result_sha256",
    "checkout_postflight_sha256", "fallback_checkout_postflight_sha256",
    "dependency_library_content_after"
  ),
  value = c(
    "4", "repository_dependencies", run_id, evidence_profile,
    as.character(max_priority),
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    if (any(results$status == "failed") || checkout_postflight_failed ||
        fallback_checkout_postflight_failed) "failed" else "passed",
    as.character(nrow(results)), as.character(sum(results$status == "failed")),
    unname(tools::sha256sum(result_path)),
    unname(tools::sha256sum(checkout_postflight_path)),
    unname(tools::sha256sum(fallback_checkout_postflight_path)),
    dependency_content_after
  ),
  stringsAsFactors = FALSE
)
write_tsv(completion, file.path(metadata_directory, "completion.tsv"))
repository_seal_evidence(stage_directory)

if (any(results$status == "failed") || checkout_postflight_failed ||
    fallback_checkout_postflight_failed) {
  quit(save = "no", status = 1L)
}
