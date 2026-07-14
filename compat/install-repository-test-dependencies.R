usage <- paste(
  "usage: install-repository-test-dependencies.R",
  "[ROOT [MAX_PRIORITY [LIBRARY]]] --run-id ID"
)

args <- commandArgs(trailingOnly = TRUE)
positionals <- character()
run_id <- NULL
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
stage_name <- sprintf("repository-dependencies-priority-%d", max_priority)
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
manifest_path <- file.path(root, "compat", "github-repositories.tsv")
snapshot_path <- file.path(root, "compat", "github-snapshot.tsv")
harness_path <- file.path(root, "compat", "install-repository-test-dependencies.R")
fingerprint_path <- file.path(root, "compat", "fingerprint.R")
evidence_helper_path <- file.path(root, "compat", "repository-evidence.R")
evidence_verifier_path <- file.path(root, "compat", "verify-repository-evidence.R")
compat_system_evidence_path <- file.path(
  root, "compat", "compat-system-evidence.R"
)
sys.source(fingerprint_path, envir = environment())
sys.source(evidence_helper_path, envir = environment())
sys.source(compat_system_evidence_path, envir = environment())

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
  error_file <- tempfile("paradox-dependency-git-")
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
  checkout <- file.path(root, ".local", "compat", "github", repository)
  if (!file.exists(file.path(checkout, ".git"))) {
    stop("Git checkout is missing for ", repository, call. = FALSE)
  }
  if (is_symbolic(checkout)) {
    stop("Git checkout is symbolic for ", repository, call. = FALSE)
  }
  checkout <- normalizePath(checkout, winslash = "/", mustWork = TRUE)
  head <- single_git_value(
    checkout,
    c("rev-parse", "--verify", "HEAD^{commit}"),
    paste0("reading ", repository, " HEAD")
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
    observed_commit = head,
    expected_origin = expected_origin,
    observed_origin = origin,
    clean = !length(status),
    status = gsub("[\r\n\t]+", " ", paste(status, collapse = " | ")),
    valid = identical(head, expected_commit) &&
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
for (index in seq_len(nrow(clone_manifest))) {
  repository <- clone_manifest$repository[[index]]
  checkout <- file.path(root, ".local", "compat", "github", repository)
  description <- file.path(checkout, "DESCRIPTION")
  if (!file.exists(description)) next
  if (dir.exists(description) || is_symbolic(description)) {
    stop("Fallback DESCRIPTION is not one regular file: ", description, call. = FALSE)
  }
  fields <- read.dcf(description, fields = "Package")
  package <- unname(fields[[1L, "Package"]])
  if (length(package) != 1L || is.na(package) || !nzchar(package)) {
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
    manifest_path, snapshot_path, harness_path, fingerprint_path,
    evidence_helper_path, evidence_verifier_path, compat_system_evidence_path
  ),
  metadata_directory,
  copy.mode = TRUE,
  copy.date = TRUE
)
if (!all(copied)) stop("could not retain dependency evidence inputs", call. = FALSE)
compat_system_evidence <- compat_system_capture_evidence(root, metadata_directory)
run_metadata <- data.frame(
  field = c(
    "schema", "stage_kind", "run_id", "started_utc", "root", "max_priority",
    "dependency_library", "dependency_library_content_before", "r",
    "r_version", "github_manifest_sha256", "github_snapshot_sha256",
    "harness_sha256", "fingerprint_sha256", "evidence_helper_sha256",
    "evidence_verifier_sha256", "checkout_preflight_sha256",
    "fallback_checkout_preflight_sha256", "result_ledger"
  ),
  value = c(
    "3", "repository_dependencies", run_id, started_utc, root,
    as.character(max_priority), library,
    dependency_content_before,
    normalizePath(file.path(R.home(), "bin", "R"), winslash = "/", mustWork = TRUE),
    as.character(getRversion()), unname(tools::sha256sum(manifest_path)),
    unname(tools::sha256sum(snapshot_path)), unname(tools::sha256sum(harness_path)),
    unname(tools::sha256sum(fingerprint_path)),
    unname(tools::sha256sum(evidence_helper_path)),
    unname(tools::sha256sum(evidence_verifier_path)),
    unname(tools::sha256sum(checkout_preflight_path)),
    unname(tools::sha256sum(fallback_checkout_preflight_path)), result_path
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
    available_locally <- missing[missing %in% names(local_packages)]
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
for (i in seq_len(nrow(selected))) {
  repository <- selected$repository[[i]]
  checkout <- file.path(root, ".local", "compat", "github", repository)
  message("Installing development dependencies for ", repository)

  started <- Sys.time()
  outcome <- tryCatch({
    if (!file.exists(file.path(checkout, "DESCRIPTION"))) {
      stop("Checkout or DESCRIPTION is missing: ", checkout)
    }
    install_development_dependencies(checkout)
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
    "schema", "stage_kind", "run_id", "max_priority", "finished_utc", "status",
    "result_rows", "failed_rows", "result_sha256",
    "checkout_postflight_sha256", "fallback_checkout_postflight_sha256",
    "dependency_library_content_after"
  ),
  value = c(
    "3", "repository_dependencies", run_id, as.character(max_priority),
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
