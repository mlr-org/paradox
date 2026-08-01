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

canonical_snapshot_timestamp <- function(value) {
  if (length(value) != 1L || is.na(value) || !grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(Z|[+-][0-9]{2}:[0-9]{2})$",
      value)) {
    stop("exact provider snapshot timestamp is malformed", call. = FALSE)
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
    stop("exact provider snapshot timestamp is not a real calendar time",
      call. = FALSE)
  }
  offset_seconds <- 0
  offset_sign <- "+"
  if (!endsWith(value, "Z")) {
    count <- nchar(value)
    offset_sign <- substr(value, 20L, 20L)
    hour <- suppressWarnings(as.integer(substr(value, count - 4L, count - 3L)))
    minute <- suppressWarnings(as.integer(substr(value, count - 1L, count)))
    if (is.na(hour) || is.na(minute) || hour > 23L || minute > 59L) {
      stop("exact provider snapshot timestamp has an invalid UTC offset",
        call. = FALSE)
    }
    offset_seconds <- hour * 3600 + minute * 60
  }
  instant <- as.POSIXct(local_time, tz = "UTC")
  if (identical(offset_sign, "+")) {
    instant <- instant - offset_seconds
  } else {
    instant <- instant + offset_seconds
  }
  if (length(instant) != 1L || is.na(instant)) {
    stop("exact provider snapshot timestamp could not be represented",
      call. = FALSE)
  }
  format(instant, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

acquire_dependency_writer_lock <- function(parent, library, run_id, started_utc) {
  parent <- require_plain_directory(parent, "dependency writer-lock parent")
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  lock_path <- file.path(parent, ".repository-dependency-writer-lock")
  # Acquisition is deliberately nonblocking.  A pre-existing lock may belong
  # to a live or interrupted producer, so it is never reaped automatically;
  # a maintainer must authenticate and remove a stale lock manually.
  if (file.exists(lock_path) || dir.exists(lock_path) || is_symbolic(lock_path)) {
    stop(
      "dependency writer lock already exists: ",
      lock_path,
      call. = FALSE
    )
  }
  if (!dir.create(lock_path, recursive = FALSE, mode = "0700",
      showWarnings = FALSE)) {
    stop("dependency writer lock cannot be acquired: ", lock_path,
      call. = FALSE)
  }
  rollback_active <- TRUE
  rollback_owner <- NULL
  rollback_device <- NULL
  rollback_inode <- NULL
  rollback_acquisition <- function() {
    if (!rollback_active) return(invisible(NULL))
    problem <- tryCatch({
      require_plain_directory(lock_path, "incomplete dependency writer lock")
      if (!is.null(rollback_device) && !is.null(rollback_inode)) {
        identity <- fs::file_info(lock_path)
        device <- format(
          as.numeric(identity$device_id), scientific = FALSE, trim = TRUE
        )
        inode <- format(
          as.numeric(identity$inode), scientific = FALSE, trim = TRUE
        )
        if (!identical(device, rollback_device) ||
            !identical(inode, rollback_inode)) {
          stop("incomplete dependency writer-lock identity changed",
            call. = FALSE)
        }
      }
      entries <- list.files(
        lock_path,
        all.files = TRUE,
        full.names = FALSE,
        recursive = FALSE,
        no.. = TRUE
      )
      if (!length(entries)) {
        if (!file.remove(lock_path)) {
          stop("could not remove empty incomplete dependency writer lock",
            call. = FALSE)
        }
      } else if (identical(entries, "owner.tsv") &&
          is.data.frame(rollback_owner)) {
        observed_owner <- read_manifest(
          file.path(lock_path, "owner.tsv"), c("field", "value")
        )
        if (!identical(observed_owner, rollback_owner) ||
            !file.remove(file.path(lock_path, "owner.tsv")) ||
            !file.remove(lock_path)) {
          stop("could not remove owned incomplete dependency writer lock",
            call. = FALSE)
        }
      } else {
        stop(
          "incomplete dependency writer lock is not exactly empty or owned",
          call. = FALSE
        )
      }
      if (file.exists(lock_path) || dir.exists(lock_path) ||
          is_symbolic(lock_path)) {
        stop("incomplete dependency writer lock remains", call. = FALSE)
      }
      NULL
    }, error = identity)
    if (!is.null(problem)) {
      warning(
        "acquisition rollback preserved a writer lock for manual review: ",
        conditionMessage(problem),
        call. = FALSE,
        immediate. = TRUE
      )
    }
    invisible(NULL)
  }
  on.exit(rollback_acquisition(), add = TRUE)
  require_plain_directory(lock_path, "dependency writer lock")
  initial_lock_identity <- fs::file_info(lock_path)
  rollback_device <- format(
    as.numeric(initial_lock_identity$device_id),
    scientific = FALSE,
    trim = TRUE
  )
  rollback_inode <- format(
    as.numeric(initial_lock_identity$inode),
    scientific = FALSE,
    trim = TRUE
  )
  if (is.na(rollback_device) || !nzchar(rollback_device) ||
      is.na(rollback_inode) || !nzchar(rollback_inode)) {
    stop("dependency writer lock has no initial filesystem identity",
      call. = FALSE)
  }
  Sys.chmod(lock_path, mode = "0700", use_umask = FALSE)
  lock_mode <- as.integer(file.info(lock_path, extra_cols = FALSE)$mode)
  if (length(lock_mode) != 1L || is.na(lock_mode) ||
      bitwAnd(lock_mode, 511L) != 448L) {
    stop("dependency writer lock is not private", call. = FALSE)
  }
  entropy_path <- "/dev/urandom"
  if (!file.exists(entropy_path) || dir.exists(entropy_path) ||
      is_symbolic(entropy_path)) {
    stop("dependency writer lock requires an operating-system entropy source",
      call. = FALSE)
  }
  entropy_connection <- file(entropy_path, open = "rb", raw = TRUE)
  token_bytes <- tryCatch(
    readBin(entropy_connection, what = raw(), n = 32L),
    finally = close(entropy_connection)
  )
  if (length(token_bytes) != 32L) {
    stop("could not read a complete dependency writer-lock token",
      call. = FALSE)
  }
  token <- paste(sprintf("%02x", as.integer(token_bytes)), collapse = "")
  lock_identity <- fs::file_info(lock_path)
  lock_device <- format(
    as.numeric(lock_identity$device_id), scientific = FALSE, trim = TRUE
  )
  lock_inode <- format(
    as.numeric(lock_identity$inode), scientific = FALSE, trim = TRUE
  )
  if (is.na(lock_device) || !nzchar(lock_device) ||
      is.na(lock_inode) || !nzchar(lock_inode) ||
      !identical(lock_device, rollback_device) ||
      !identical(lock_inode, rollback_inode)) {
    stop("dependency writer lock has no stable filesystem identity",
      call. = FALSE)
  }
  owner_path <- file.path(lock_path, "owner.tsv")
  owner <- data.frame(
    field = c(
      "schema", "run_id", "pid", "started_utc", "library", "token",
      "lock_device", "lock_inode"
    ),
    value = c(
      "1", run_id, as.character(Sys.getpid()), started_utc, library, token,
      lock_device, lock_inode
    ),
    stringsAsFactors = FALSE
  )
  rollback_owner <- owner
  write_tsv(owner, owner_path)
  owner_sha256 <- unname(tools::sha256sum(owner_path))
  active <- TRUE
  release <- function(strict = TRUE) {
    if (!active) return(invisible(NULL))
    problem <- tryCatch({
      require_plain_directory(lock_path, "dependency writer lock")
      entries <- list.files(
        lock_path,
        all.files = TRUE,
        full.names = FALSE,
        recursive = FALSE,
        no.. = TRUE
      )
      observed_identity <- fs::file_info(lock_path)
      observed_device <- format(
        as.numeric(observed_identity$device_id), scientific = FALSE, trim = TRUE
      )
      observed_inode <- format(
        as.numeric(observed_identity$inode), scientific = FALSE, trim = TRUE
      )
      if (!identical(observed_device, lock_device) ||
          !identical(observed_inode, lock_inode) ||
          !identical(entries, "owner.tsv") ||
          !file.exists(owner_path) || dir.exists(owner_path) ||
          is_symbolic(owner_path) ||
          !identical(unname(tools::sha256sum(owner_path)), owner_sha256)) {
        stop("dependency writer-lock ownership changed", call. = FALSE)
      }
      if (unlink(owner_path, recursive = FALSE, force = FALSE) != 0L ||
          file.exists(owner_path) || is_symbolic(owner_path)) {
        stop("could not remove dependency writer-lock owner", call. = FALSE)
      }
      # file.remove() removes an empty directory without traversing it.  If a
      # path appears after the exact inventory check, removal therefore fails
      # closed instead of deleting that unowned path.
      if (!file.remove(lock_path) ||
          file.exists(lock_path) || dir.exists(lock_path) ||
          is_symbolic(lock_path)) {
        stop("could not release dependency writer lock", call. = FALSE)
      }
      active <<- FALSE
      NULL
    }, error = identity)
    if (!is.null(problem)) {
      if (strict) stop(conditionMessage(problem), call. = FALSE)
      warning(conditionMessage(problem), call. = FALSE, immediate. = TRUE)
    }
    invisible(NULL)
  }
  rollback_active <- FALSE
  list(path = lock_path, release = release)
}

make_canonical_source_read_only <- function(entry, retained_tree) {
  entry <- require_plain_directory(entry, "private canonical provider entry")
  source <- require_plain_directory(
    file.path(entry, "source"),
    "private canonical provider source"
  )
  entries <- repository_runner_entries(
    entry, "private canonical provider entry", allow_symlinks = TRUE
  )
  files <- entries$type == "file"
  if (any(files)) {
    prefix <- "source/"
    relative <- entries$path[files]
    if (any(!startsWith(relative, prefix))) {
      stop("private canonical provider entry contains an unexpected file",
        call. = FALSE)
    }
    relative <- substring(relative, nchar(prefix) + 1L)
    tree_index <- match(relative, retained_tree$path)
    if (anyNA(tree_index) ||
        any(retained_tree$mode[tree_index] == "120000")) {
      stop("private canonical provider file is absent from its Git tree",
        call. = FALSE)
    }
    modes <- ifelse(
      retained_tree$mode[tree_index] == "100755", "0555", "0444"
    )
    for (index in seq_along(modes)) {
      Sys.chmod(
        entries$absolute[files][[index]],
        mode = modes[[index]],
        use_umask = FALSE
      )
    }
  }
  directories <- entries$type == "directory"
  if (any(directories)) {
    order <- order(
      nchar(entries$absolute[directories], type = "chars"),
      decreasing = TRUE
    )
    Sys.chmod(
      entries$absolute[directories][order],
      mode = "0555",
      use_umask = FALSE
    )
  }
  Sys.chmod(entry, mode = "0555", use_umask = FALSE)
  invisible(source)
}

assert_canonical_source_read_only <- function(entry) {
  entry <- require_plain_directory(entry, "canonical provider entry")
  entries <- repository_runner_entries(
    entry, "canonical provider entry", allow_symlinks = TRUE
  )
  nonsymbolic <- entries$type != "symlink"
  regular <- entries$type == "file"
  modes <- c(
    as.integer(file.info(entry, extra_cols = FALSE)$mode),
    strtoi(entries$mode[nonsymbolic], base = 8L)
  )
  if (anyNA(modes) || any(bitwAnd(modes, 146L) != 0L)) {
    stop("canonical provider source is writable", call. = FALSE)
  }
  if (any(regular) && any(
      is.na(entries$hard_links[regular]) | entries$hard_links[regular] != 1
    )) {
    stop("canonical provider source contains a hard-linked file",
      call. = FALSE)
  }
  invisible(entry)
}

file_contains_fixed_bytes <- function(path, value) {
  pattern <- charToRaw(enc2utf8(value))
  if (!length(pattern)) stop("cannot scan for an empty byte sequence", call. = FALSE)
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  carry <- raw()
  repeat {
    chunk <- readBin(connection, what = raw(), n = 65536L)
    if (!length(chunk)) return(FALSE)
    bytes <- c(carry, chunk)
    if (length(grepRaw(pattern, bytes, fixed = TRUE))) return(TRUE)
    retained <- min(length(pattern) - 1L, length(bytes))
    carry <- if (retained) tail(bytes, retained) else raw()
  }
}

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
provider_install_roots <- character()
provider_built_timestamps <- character()
provider_install_environment <- character()
provider_install_work_directory <- NULL
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
    install_source = character(),
    stringsAsFactors = FALSE
  )
}
exact_provider_sources_path <- file.path(
  metadata_directory,
  "exact-provider-sources.tsv"
)
exact_provider_packages <- unname(local_repository_packages[
  selected$repository[exact_provider_indices]
])

if (dir.exists(library)) {
  require_plain_directory(library, "dependency library")
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)
  library_parent <- dirname(library)
} else {
  if (file.exists(library) || is_symbolic(library)) {
    stop("dependency library exists but is not a plain directory: ", library,
      call. = FALSE)
  }
  library_parent <- require_plain_directory(
    dirname(library), "dependency library parent"
  )
  library_parent <- normalizePath(library_parent, winslash = "/", mustWork = TRUE)
  library <- file.path(library_parent, basename(library))
}
if (!startsWith(library_parent, paste0(local_root, "/")) ||
    !startsWith(library, paste0(local_root, "/"))) {
  stop("dependency library must remain below the repository-local state root",
    call. = FALSE)
}

local({
  writer_lock <- acquire_dependency_writer_lock(
    compat_root, library, run_id, started_utc
  )
  on.exit(writer_lock$release(strict = FALSE), add = TRUE)

  if (dir.exists(library)) {
    observed_library <- normalizePath(
      require_plain_directory(library, "dependency library"),
      winslash = "/",
      mustWork = TRUE
    )
    if (!identical(observed_library, library)) {
      stop("dependency library identity changed before locking", call. = FALSE)
    }
  } else {
    if (file.exists(library) || is_symbolic(library)) {
      stop("dependency library appeared as a non-directory", call. = FALSE)
    }
    if (!dir.create(library, recursive = FALSE, showWarnings = FALSE)) {
      stop("could not create dependency library: ", library, call. = FALSE)
    }
    require_plain_directory(library, "dependency library")
    library <- normalizePath(library, winslash = "/", mustWork = TRUE)
  }
  if (!identical(dirname(library), library_parent)) {
    stop("dependency library escaped its locked parent", call. = FALSE)
  }

  if (nrow(exact_provider_sources)) {
    exact_provider_sources$install_source <- rep(
      NA_character_, nrow(exact_provider_sources)
    )
    canonical_root <- plain_child_directory(
      compat_root,
      "exact-provider-sources-v1",
      "canonical exact-provider source registry",
      TRUE
    )
    install_state_root <- plain_child_directory(
      compat_root,
      "exact-provider-install-state-v1",
      "canonical exact-provider installation state",
      TRUE
    )
    install_state_names <- c(
      home = "home", tmp = "tmp", cache = "cache", config = "config",
      data = "data", state = "state", runtime = "runtime", work = "work",
      r_cache = "r-cache"
    )
    install_state <- vapply(names(install_state_names), function(name) {
      plain_child_directory(
        install_state_root,
        install_state_names[[name]],
        paste0("exact-provider installation ", name),
        TRUE
      )
    }, character(1L))
    Sys.chmod(install_state, mode = "0700", use_umask = FALSE)
    install_state_modes <- as.integer(file.info(
      install_state, extra_cols = FALSE
    )$mode)
    if (anyNA(install_state_modes) ||
        any(bitwAnd(install_state_modes, 511L) != 448L)) {
      stop("exact-provider installation state is not private", call. = FALSE)
    }
    expected_makevars <- repository_runner_require_file(
      file.path(root, "environment", "Makevars"),
      "repository-local installation Makevars"
    )
    observed_makevars <- normalizePath(
      Sys.getenv("R_MAKEVARS_USER", unset = ""),
      winslash = "/",
      mustWork = TRUE
    )
    if (!identical(observed_makevars, expected_makevars)) {
      stop("activated installation Makevars is not repository-local",
        call. = FALSE)
    }
    toolchain_bin <- normalizePath(
      file.path(root, ".local", "toolchain", "bin"),
      winslash = "/",
      mustWork = TRUE
    )
    restricted_child_path <- paste(
      c(toolchain_bin, "/usr/bin", "/bin"),
      collapse = .Platform$path.sep
    )
    provider_install_environment <- c(
      HOME = install_state[["home"]],
      USER = "paradox",
      LOGNAME = "paradox",
      PATH = restricted_child_path,
      TMPDIR = install_state[["tmp"]],
      TMP = install_state[["tmp"]],
      TEMP = install_state[["tmp"]],
      XDG_CACHE_HOME = install_state[["cache"]],
      XDG_CONFIG_HOME = install_state[["config"]],
      XDG_DATA_HOME = install_state[["data"]],
      XDG_STATE_HOME = install_state[["state"]],
      XDG_RUNTIME_DIR = install_state[["runtime"]],
      R_USER_CACHE_DIR = install_state[["r_cache"]],
      R_LIBS = library,
      R_LIBS_USER = library,
      R_LIBS_SITE = "",
      R_ENVIRON = "/dev/null",
      R_ENVIRON_USER = "/dev/null",
      R_PROFILE = "/dev/null",
      R_PROFILE_USER = "/dev/null",
      R_HISTFILE = "/dev/null",
      R_TESTS = "",
      R_MAKEVARS_SITE = "/dev/null",
      R_MAKEVARS_USER = expected_makevars,
      MAKEFLAGS = "-j1",
      LC_ALL = "C.UTF-8",
      LANG = "C.UTF-8",
      LANGUAGE = "C",
      TZ = "UTC"
    )
    provider_install_work_directory <- install_state[["work"]]
    for (index in exact_provider_indices) {
      repository <- selected$repository[[index]]
      source_index <- match(repository, exact_provider_sources$repository)
      if (is.na(source_index)) {
        stop("exact provider is absent from its retained source ledger",
          call. = FALSE)
      }
      source_row <- exact_provider_sources[source_index, , drop = FALSE]
      raw_source <- normalizePath(
        file.path(exact_provider_directory, repository, "source"),
        winslash = "/",
        mustWork = TRUE
      )
      archive_path <- repository_runner_require_file(
        file.path(exact_provider_directory, paste0(repository, ".tar")),
        "retained exact-provider archive"
      )
      tree_manifest_path <- repository_runner_require_file(
        file.path(exact_provider_directory, paste0(repository, "-tree.tsv")),
        "retained exact-provider tree manifest"
      )
      if (!identical(source_row$source[[1L]], raw_source) ||
          !identical(
            unname(tools::sha256sum(archive_path)),
            source_row$archive_sha256[[1L]]
          ) ||
          !identical(
            unname(tools::sha256sum(tree_manifest_path)),
            source_row$tree_manifest_sha256[[1L]]
          )) {
        stop("retained exact-provider source evidence changed", call. = FALSE)
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
      raw_tree <- repository_runner_validate_extraction(
        authentication, raw_source
      )
      if (!identical(retained_tree, raw_tree)) {
        stop("retained exact-provider extraction changed", call. = FALSE)
      }

      entry_name <- paste0(repository, "-", source_row$commit[[1L]])
      repository_runner_safe_name(entry_name, "canonical provider entry")
      canonical_entry <- file.path(canonical_root, entry_name)
      if (file.exists(canonical_entry) || dir.exists(canonical_entry) ||
          is_symbolic(canonical_entry)) {
        require_plain_directory(canonical_entry, "canonical provider entry")
      } else {
        canonical_entry <- local({
          private_name <- basename(tempfile(
            paste0(repository, "-", substr(source_row$commit[[1L]], 1L, 12L),
              "-new-"),
            tmpdir = canonical_root
          ))
          private <- repository_runner_reserve_directory(
            canonical_root, private_name, "private canonical provider entry"
          )
          # A failed authenticated extraction is retained at this unique path
          # for manual inspection.  This producer never recursively removes a
          # staging tree; only a fully validated tree is atomically renamed.
          repository_runner_archive_entries(archive_path)
          suppressWarnings(utils::untar(
            archive_path, exdir = private, tar = "internal"
          ))
          private_source <- require_plain_directory(
            file.path(private, "source"),
            "private canonical provider source"
          )
          private_inventory <- list.files(
            private,
            all.files = TRUE,
            full.names = FALSE,
            recursive = FALSE,
            no.. = TRUE
          )
          if (!identical(private_inventory, "source")) {
            stop("private canonical provider entry has unexpected contents",
              call. = FALSE)
          }
          private_tree <- repository_runner_validate_extraction(
            authentication, private_source
          )
          if (!identical(retained_tree, private_tree)) {
            stop("canonical provider candidate differs from retained source",
              call. = FALSE)
          }
          make_canonical_source_read_only(private, retained_tree)
          immutable_tree <- repository_runner_validate_extraction(
            authentication, private_source
          )
          if (!identical(retained_tree, immutable_tree)) {
            stop("read-only canonical provider candidate changed",
              call. = FALSE)
          }
          assert_canonical_source_read_only(private)
          if (file.exists(canonical_entry) || dir.exists(canonical_entry) ||
              is_symbolic(canonical_entry) ||
              !file.rename(private, canonical_entry)) {
            stop("could not atomically publish canonical provider source",
              call. = FALSE)
          }
          canonical_entry
        })
      }
      canonical_entry <- normalizePath(
        require_plain_directory(canonical_entry, "canonical provider entry"),
        winslash = "/",
        mustWork = TRUE
      )
      if (!identical(dirname(canonical_entry), canonical_root)) {
        stop("canonical provider entry escaped its registry", call. = FALSE)
      }
      canonical_inventory <- list.files(
        canonical_entry,
        all.files = TRUE,
        full.names = FALSE,
        recursive = FALSE,
        no.. = TRUE
      )
      if (!identical(canonical_inventory, "source")) {
        stop("canonical provider entry has unexpected contents", call. = FALSE)
      }
      install_source <- normalizePath(
        require_plain_directory(
          file.path(canonical_entry, "source"),
          "canonical provider source"
        ),
        winslash = "/",
        mustWork = TRUE
      )
      canonical_tree <- repository_runner_validate_extraction(
        authentication, install_source
      )
      if (!identical(retained_tree, canonical_tree)) {
        stop("canonical provider source differs from retained evidence",
          call. = FALSE)
      }
      assert_canonical_source_read_only(canonical_entry)
      exact_provider_sources$install_source[[source_index]] <- install_source
      provider_install_roots[[repository]] <- install_source
      provider_built_timestamps[[repository]] <- canonical_snapshot_timestamp(
        selected_snapshot$commit_date[[index]]
      )
    }
  }
  if (anyNA(exact_provider_sources) ||
      any(vapply(exact_provider_sources, function(column) {
        any(!nzchar(column))
      }, logical(1L)))) {
    stop("exact-provider source ledger is incomplete", call. = FALSE)
  }
  write_tsv(exact_provider_sources, exact_provider_sources_path)

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
    "5", "repository_dependencies", run_id, evidence_profile, started_utc, root,
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

exact_dependency_provider_source <- function(repository) {
  package <- unname(local_repository_packages[[repository]])
  raw_source <- unname(provider_source_roots[[repository]])
  install_source <- unname(provider_install_roots[[repository]])
  built_timestamp <- unname(provider_built_timestamps[[repository]])
  if (length(package) != 1L || is.na(package) || !nzchar(package) ||
      length(raw_source) != 1L || is.na(raw_source) || !nzchar(raw_source) ||
      length(install_source) != 1L || is.na(install_source) ||
      !nzchar(install_source) ||
      length(built_timestamp) != 1L || is.na(built_timestamp) ||
      !nzchar(built_timestamp)) {
    stop("exact dependency provider is absent from its source map",
      call. = FALSE)
  }
  source_index <- match(repository, exact_provider_sources$repository)
  if (is.na(source_index)) {
    stop("exact dependency provider is absent from its source ledger",
      call. = FALSE)
  }
  source_row <- exact_provider_sources[source_index, , drop = FALSE]
  if (!identical(source_row$source[[1L]], raw_source) ||
      !identical(source_row$install_source[[1L]], install_source)) {
    stop("exact dependency provider source maps disagree", call. = FALSE)
  }
  source_description_path <- repository_runner_require_file(
    file.path(install_source, "DESCRIPTION"),
    "exact dependency provider source DESCRIPTION"
  )
  source_description <- read.dcf(
    source_description_path,
    fields = c("Package", "Version", "NeedsCompilation")
  )
  source_package <- unname(source_description[[1L, "Package"]])
  source_version <- unname(source_description[[1L, "Version"]])
  needs_compilation <- unname(source_description[[1L, "NeedsCompilation"]])
  source_src <- file.path(install_source, "src")
  unsupported_scripts <- file.path(
    install_source, c("configure", "configure.win", "cleanup", "cleanup.win")
  )
  if (!identical(source_package, package) || is.na(source_version) ||
      !nzchar(source_version) ||
      (!is.na(needs_compilation) &&
        !identical(tolower(needs_compilation), "no")) ||
      file.exists(source_src) || dir.exists(source_src) ||
      is_symbolic(source_src) ||
      any(file.exists(unsupported_scripts)) ||
      any(dir.exists(unsupported_scripts)) ||
      any(vapply(unsupported_scripts, is_symbolic, logical(1L)))) {
    stop(
      "exact provider must be an identified pure-R package without build scripts",
      call. = FALSE
    )
  }
  list(
    repository = repository,
    package = package,
    version = source_version,
    raw_source = raw_source,
    install_source = install_source,
    built_timestamp = built_timestamp,
    source_row = source_row
  )
}

validate_exact_dependency_provider_install <- function(
    repository, expected_content_sha256 = NULL) {
  source <- exact_dependency_provider_source(repository)
  package <- source$package
  raw_source <- source$raw_source
  install_source <- source$install_source
  built_timestamp <- source$built_timestamp
  source_version <- source$version
  source_row <- source$source_row
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
    fields = c(
      "Package", "Version", "Built", "Packaged", "RemoteType", "RemotePkgRef"
    )
  )
  expected_identity <- c(
    Package = package,
    Version = source_version,
    Built = paste0(
      "R ", as.character(getRversion()), "; ; ", built_timestamp, "; ",
      .Platform$OS.type
    )
  )
  if (!identical(
      unname(observed[1L, names(expected_identity)]),
      unname(expected_identity)
    ) || any(!is.na(observed[1L, c(
      "Packaged", "RemoteType", "RemotePkgRef"
    )]))) {
    stop(
      "installed provider identity or deterministic build metadata does not ",
      "match its exact source"
    )
  }
  installed_entries <- repository_runner_entries(
    installed, "installed exact dependency provider"
  )
  installed_files <- installed_entries$absolute[
    installed_entries$type == "file"
  ]
  forbidden <- c(
    stage_directory = stage_directory,
    run_directory = run_directory,
    run_local_source = raw_source
  )
  forbidden <- forbidden[!duplicated(unname(forbidden))]
  for (label in names(forbidden)) {
    matches <- vapply(
      installed_files,
      file_contains_fixed_bytes,
      logical(1L),
      value = forbidden[[label]]
    )
    if (any(matches)) {
      stop(
        "installed exact provider retains forbidden ", label,
        " bytes in ", installed_entries$path[
          installed_entries$type == "file"
        ][which(matches)[[1L]]],
        call. = FALSE
      )
    }
  }
  content_sha256 <- compat_tree_content_sha256(installed)
  if (!is.null(expected_content_sha256) &&
      !identical(content_sha256, expected_content_sha256)) {
    stop("installed exact dependency provider changed after installation")
  }
  data.frame(
    repository = repository,
    package = package,
    version = source_version,
    install_method = "r_cmd_install_built_timestamp",
    commit = source_row$commit[[1L]],
    tree = source_row$tree[[1L]],
    archive_sha256 = source_row$archive_sha256[[1L]],
    built_timestamp = built_timestamp,
    install_source = install_source,
    content_sha256 = content_sha256,
    stringsAsFactors = FALSE
  )
}

install_exact_dependency_provider <- function(repository, checkout) {
  package <- unname(local_repository_packages[[repository]])
  source <- unname(provider_install_roots[[repository]])
  built_timestamp <- unname(provider_built_timestamps[[repository]])
  if (length(package) != 1L || is.na(package) || !nzchar(package) ||
      is.null(local_packages[[package]]) ||
      !identical(local_packages[[package]], checkout) ||
      length(source) != 1L || is.na(source) || !nzchar(source) ||
      length(built_timestamp) != 1L || is.na(built_timestamp) ||
      !nzchar(built_timestamp)) {
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
    source_identity <- exact_dependency_provider_source(repository)
    if (!identical(source_identity$package, package) ||
        !identical(source_identity$install_source, source) ||
        !identical(source_identity$built_timestamp, built_timestamp)) {
      stop("exact dependency provider preflight changed its source identity",
        call. = FALSE)
    }
    pak::local_install_deps(
      root = source,
      lib = library,
      upgrade = FALSE,
      ask = FALSE,
      dependencies = NA
    )
    expected_r <- normalizePath(
      file.path(R.home(), "bin", "R"), winslash = "/", mustWork = TRUE
    )
    installation <- processx::run(
      expected_r,
      args = c(
        "CMD", "INSTALL", "--preclean", "--clean", "--no-multiarch",
        "--use-vanilla", "--without-keep.source",
        paste0("--built-timestamp=", built_timestamp),
        paste0("--library=", library),
        source
      ),
      stdout = "|",
      stderr_to_stdout = TRUE,
      error_on_status = FALSE,
      timeout = 3600000L,
      cleanup_tree = TRUE,
      wd = provider_install_work_directory,
      env = provider_install_environment
    )
    if (!identical(installation$status, 0L)) {
      stop(
        "exact provider R CMD INSTALL failed: ",
        compact_error(paste(installation$stdout, collapse = "\n")),
        call. = FALSE
      )
    }
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
    expected_install_source <- unname(provider_install_roots[[repository]])
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
          source_row$install_source[[1L]], expected_install_source
        ) ||
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
    observed_install_tree <- repository_runner_validate_extraction(
      authentication,
      expected_install_source
    )
    if (!identical(retained_tree, observed_install_tree)) {
      stop("canonical exact dependency provider source changed after installation")
    }
    assert_canonical_source_read_only(dirname(expected_install_source))
    final_receipt <- validate_exact_dependency_provider_install(
      repository,
      initial_receipt$content_sha256[[1L]]
    )
    identity_fields <- c(
      "repository", "package", "version", "install_method", "commit", "tree",
      "archive_sha256", "built_timestamp", "install_source"
    )
    if (!identical(
        initial_receipt[, identity_fields, drop = FALSE],
        final_receipt[, identity_fields, drop = FALSE]
      )) {
      stop("exact dependency provider identity changed after installation")
    }
    provider_terminal_rows[[length(provider_terminal_rows) + 1L]] <- data.frame(
      repository = repository,
      package = initial_receipt$package[[1L]],
      version = initial_receipt$version[[1L]],
      install_method = initial_receipt$install_method[[1L]],
      commit = initial_receipt$commit[[1L]],
      tree = initial_receipt$tree[[1L]],
      archive_sha256 = initial_receipt$archive_sha256[[1L]],
      built_timestamp = initial_receipt$built_timestamp[[1L]],
      install_source = initial_receipt$install_source[[1L]],
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
    install_method = character(), commit = character(), tree = character(),
    archive_sha256 = character(), built_timestamp = character(),
    install_source = character(),
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
dependency_content_after_locked <- compat_tree_content_sha256(library)
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
# A successful or failed evidence receipt is published only after strict lock
# release.  Release is nonblocking and never reaps another process's lock; a
# preserved lock requires manual authentication and cleanup.  Re-fingerprinting
# immediately after release closes the bounded handoff and fails if another
# writer reached the shared library before completion could be recorded.
writer_lock$release(strict = TRUE)
dependency_content_after <- compat_tree_content_sha256(library)
if (!identical(dependency_content_after, dependency_content_after_locked)) {
  stop("dependency library changed during writer-lock release handoff",
    call. = FALSE)
}
completion <- data.frame(
  field = c(
    "schema", "stage_kind", "run_id", "evidence_profile", "max_priority",
    "finished_utc", "status",
    "result_rows", "failed_rows", "result_sha256",
    "checkout_postflight_sha256", "fallback_checkout_postflight_sha256",
    "dependency_library_content_after"
  ),
  value = c(
    "5", "repository_dependencies", run_id, evidence_profile,
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
})
