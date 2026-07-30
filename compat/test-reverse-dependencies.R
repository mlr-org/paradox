args <- commandArgs(trailingOnly = TRUE)

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

reverse_usage <- function() {
  cat(paste0(
    "Pinned source-package reverse-dependency gate\n\n",
    "Usage:\n",
    "  Rscript compat/test-reverse-dependencies.R [options]\n\n",
    "Required options:\n",
    "  --root PATH                 paradox repository root\n",
    "  --max-priority N            include inventory priorities <= N\n",
    "  --candidate-library PATH    immutable library containing paradox\n",
    "  --dependency-library PATH   immutable shared dependency library\n\n",
    "Candidate provenance (options override the corresponding environment):\n",
    "  PARADOX_CANDIDATE_RUN_ID   installation run that owns the library\n",
    "  --candidate-ref REF         full Git ref (PARADOX_CANDIDATE_REF)\n",
    "  --candidate-commit HASH     commit (PARADOX_CANDIDATE_COMMIT)\n",
    "  --candidate-tree HASH       tree (PARADOX_CANDIDATE_TREE)\n",
    "  --candidate-source PATH      clean detached worktree (PARADOX_CANDIDATE_SOURCE)\n",
    "  --candidate-content HASH    installed package content SHA-256\n",
    "                              (PARADOX_CANDIDATE_CONTENT_SHA256)\n\n",
    "  The candidate library must contain the sealed receipt written by\n",
    "  compat/install-candidate with the same ref, commit, and tree.\n\n",
    "Other options:\n",
    "  --run-id ID                 output below .local/compat/reverse-runs/ID\n",
    "  --reserved-run-directory PATH\n",
    "                              coordinator-precreated output reservation;\n",
    "                              must equal the path derived from --run-id\n",
    "  --package NAME              select one package; repeatable\n",
    "  --install-timeout SECONDS   bounded consumer install (default 1800)\n",
    "  --check-timeout SECONDS     bounded complete check (default 3600)\n",
    "  PARADOX_REVERSE_JOBS=N      conservatively lower automatic concurrency\n",
    "  --plan-only                 validate and retain a plan without checking\n",
    "  --resume                    continue the matching unsealed run\n",
    "  -h, --help                  show this help\n"
  ))
}

reverse_take_value <- function(arguments, index, option) {
  argument <- arguments[[index]]
  prefix <- paste0(option, "=")
  if (startsWith(argument, prefix)) {
    value <- substring(argument, nchar(prefix) + 1L)
    if (!nzchar(value)) stop(option, " requires a value", call. = FALSE)
    return(list(value = value, next_index = index + 1L))
  }
  if (!identical(argument, option)) return(NULL)
  if (index == length(arguments)) stop(option, " requires a value", call. = FALSE)
  list(value = arguments[[index + 1L]], next_index = index + 2L)
}

reverse_parse_arguments <- function(arguments) {
  values <- list(
    root = NULL,
    max_priority = NULL,
    candidate_library = NULL,
    dependency_library = NULL,
    candidate_ref = Sys.getenv("PARADOX_CANDIDATE_REF", unset = ""),
    candidate_commit = Sys.getenv("PARADOX_CANDIDATE_COMMIT", unset = ""),
    candidate_tree = Sys.getenv("PARADOX_CANDIDATE_TREE", unset = ""),
    candidate_source = Sys.getenv("PARADOX_CANDIDATE_SOURCE", unset = ""),
    candidate_content = Sys.getenv("PARADOX_CANDIDATE_CONTENT_SHA256", unset = ""),
    run_id = NULL,
    reserved_run_directory = NULL,
    packages = character(),
    install_timeout = "1800",
    check_timeout = "3600",
    plan_only = FALSE,
    resume = FALSE,
    help = FALSE
  )
  value_options <- c(
    "--root", "--max-priority", "--candidate-library",
    "--dependency-library", "--candidate-ref", "--candidate-commit",
    "--candidate-tree", "--candidate-source", "--candidate-content",
    "--run-id", "--reserved-run-directory", "--package",
    "--install-timeout", "--check-timeout"
  )

  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (argument %in% c("-h", "--help")) {
      values$help <- TRUE
      index <- index + 1L
      next
    }
    if (identical(argument, "--plan-only")) {
      values$plan_only <- TRUE
      index <- index + 1L
      next
    }
    if (identical(argument, "--resume")) {
      values$resume <- TRUE
      index <- index + 1L
      next
    }

    matched <- FALSE
    for (option in value_options) {
      taken <- reverse_take_value(arguments, index, option)
      if (is.null(taken)) next
      value <- taken$value
      index <- taken$next_index
      matched <- TRUE
      if (identical(option, "--root")) values$root <- value
      if (identical(option, "--max-priority")) values$max_priority <- value
      if (identical(option, "--candidate-library")) values$candidate_library <- value
      if (identical(option, "--dependency-library")) values$dependency_library <- value
      if (identical(option, "--candidate-ref")) values$candidate_ref <- value
      if (identical(option, "--candidate-commit")) values$candidate_commit <- value
      if (identical(option, "--candidate-tree")) values$candidate_tree <- value
      if (identical(option, "--candidate-source")) values$candidate_source <- value
      if (identical(option, "--candidate-content")) values$candidate_content <- value
      if (identical(option, "--run-id")) values$run_id <- value
      if (identical(option, "--reserved-run-directory")) {
        values$reserved_run_directory <- value
      }
      if (identical(option, "--package")) values$packages <- c(values$packages, value)
      if (identical(option, "--install-timeout")) values$install_timeout <- value
      if (identical(option, "--check-timeout")) values$check_timeout <- value
      break
    }
    if (!matched) stop("Unknown option '", argument, "' (see --help)", call. = FALSE)
  }
  values
}

arguments <- reverse_parse_arguments(args)
if (arguments$help) {
  reverse_usage()
  quit(save = "no", status = 0L)
}

required_arguments <- c(
  "root", "max_priority", "candidate_library", "dependency_library", "run_id"
)
missing_arguments <- required_arguments[vapply(
  required_arguments,
  function(name) is.null(arguments[[name]]) || !nzchar(arguments[[name]]),
  logical(1L)
)]
if (length(missing_arguments)) {
  stop(
    "Missing required option(s): ",
    paste(paste0("--", gsub("_", "-", missing_arguments)), collapse = ", "),
    call. = FALSE
  )
}

max_priority <- suppressWarnings(as.integer(arguments$max_priority))
if (length(max_priority) != 1L || is.na(max_priority) || max_priority < 0L ||
    !identical(as.character(max_priority), arguments$max_priority)) {
  stop("--max-priority must be one non-negative integer", call. = FALSE)
}
reverse_positive_integer <- function(value, option) {
  parsed <- suppressWarnings(as.integer(value))
  if (length(parsed) != 1L || is.na(parsed) || parsed <= 0L ||
      !identical(as.character(parsed), value)) {
    stop(option, " must be one positive integer", call. = FALSE)
  }
  parsed
}
install_timeout <- reverse_positive_integer(
  arguments$install_timeout, "--install-timeout"
)
check_timeout <- reverse_positive_integer(arguments$check_timeout, "--check-timeout")
worker_timeout <- as.double(install_timeout) + as.double(check_timeout) + 900
if (!is.finite(worker_timeout) || worker_timeout > 2147483647) {
  stop("combined external-worker timeout is too large", call. = FALSE)
}
if (arguments$plan_only && arguments$resume) {
  stop("--plan-only and --resume are mutually exclusive", call. = FALSE)
}

root <- normalizePath(arguments$root, winslash = "/", mustWork = TRUE)
runner_library_script <- file.path(root, "compat", "reverse-runner.R")
if (!file.exists(runner_library_script) || dir.exists(runner_library_script) ||
    nzchar(Sys.readlink(runner_library_script))) {
  stop("tracked reverse-runner library is absent or symbolic", call. = FALSE)
}
sys.source(runner_library_script, envir = environment())
install_worker_script <- file.path(root, "compat", "reverse-install-worker.R")
rr_require_file(install_worker_script, "tracked reverse install worker")
sys.source(install_worker_script, envir = environment())
wave_worker_script <- file.path(root, "compat", "reverse-wave-worker.R")
rr_require_file(wave_worker_script, "tracked external reverse wave worker")
worker_group_script <- file.path(root, "compat", "reverse-worker-group")
rr_require_file(worker_group_script, "tracked external reverse worker supervisor")
if (file.access(worker_group_script, mode = 1L) != 0L) {
  stop("tracked external reverse worker supervisor is not executable",
    call. = FALSE)
}
reverse_require_plain_local_directory <- function(path, label) {
  path <- sub("/+$", "", path)
  local_root <- file.path(root, ".local")
  if (!startsWith(path, paste0(local_root, "/"))) {
    stop(label, " must be an absolute path below ", local_root, call. = FALSE)
  }
  relative <- substring(path, nchar(root) + 2L)
  components <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  if (any(!nzchar(components) | components %in% c(".", ".."))) {
    stop(label, " has a non-canonical path component", call. = FALSE)
  }
  current <- root
  for (component in components) {
    current <- file.path(current, component)
    link <- Sys.readlink(current)
    if (length(link) != 1L || is.na(link) || nzchar(link) ||
        !dir.exists(current)) {
      stop(label, " has a missing, non-directory, or symbolic component: ",
        current, call. = FALSE)
    }
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(normalized, path)) {
    stop(label, " must use its canonical absolute path", call. = FALSE)
  }
  normalized
}
candidate_library <- reverse_require_plain_local_directory(
  arguments$candidate_library, "candidate library"
)
dependency_library <- reverse_require_plain_local_directory(
  arguments$dependency_library, "dependency library"
)
if (identical(candidate_library, dependency_library) ||
    startsWith(candidate_library, paste0(dependency_library, "/")) ||
    startsWith(dependency_library, paste0(candidate_library, "/"))) {
  stop("candidate and dependency libraries must be disjoint", call. = FALSE)
}
if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT", unset = ""), root)) {
  stop("activate the repository-local environment first: . scripts/activate", call. = FALSE)
}
expected_r_home <- normalizePath(
  file.path(root, ".local", "toolchain", "lib", "R"),
  winslash = "/",
  mustWork = TRUE
)
actual_r_home <- normalizePath(R.home(), winslash = "/", mustWork = TRUE)
actual_r <- file.path(root, ".local", "toolchain", "bin", "R")
actual_rscript <- file.path(root, ".local", "toolchain", "bin", "Rscript")
reverse_runtime_tools <- c(R = actual_r, Rscript = actual_rscript)
reverse_runtime_links <- Sys.readlink(reverse_runtime_tools)
if (!identical(actual_r_home, expected_r_home) ||
    any(!file.exists(reverse_runtime_tools)) ||
    any(dir.exists(reverse_runtime_tools)) ||
    any(is.na(reverse_runtime_links) | nzchar(reverse_runtime_links)) ||
    !identical(unname(Sys.which(names(reverse_runtime_tools))),
      unname(reverse_runtime_tools))) {
  stop(
    "the reverse-dependency gate requires the exact activated top-level R and Rscript",
    call. = FALSE
  )
}
rm(reverse_runtime_tools, reverse_runtime_links)

run_id <- arguments$run_id
if (length(run_id) != 1L ||
    !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", run_id) ||
    run_id %in% c(".", "..")) {
  stop("--run-id must be a safe name of at most 128 characters", call. = FALSE)
}
candidate_run_id <- Sys.getenv("PARADOX_CANDIDATE_RUN_ID", unset = "")
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", candidate_run_id) ||
    candidate_run_id %in% c(".", "..")) {
  stop("PARADOX_CANDIDATE_RUN_ID is missing or malformed", call. = FALSE)
}
expected_candidate_library <- file.path(
  root, ".local", "compat", "runs", candidate_run_id, "library-candidate"
)
if (!identical(candidate_library, expected_candidate_library)) {
  stop(
    "candidate library does not belong to PARADOX_CANDIDATE_RUN_ID: ",
    expected_candidate_library,
    call. = FALSE
  )
}

reverse_is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}
reverse_require_plain_directory <- function(path, label) {
  if (!dir.exists(path) || reverse_is_symbolic(path)) {
    stop(label, " is absent, not a directory, or symbolic: ", path, call. = FALSE)
  }
  invisible(path)
}
reverse_directory_identity <- function(path, label) {
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("fs is required to authenticate ", label, call. = FALSE)
  }
  info <- fs::file_info(path, fail = TRUE, follow = FALSE)
  if (nrow(info) != 1L || !identical(as.character(info$type), "directory") ||
      length(info$device_id) != 1L || is.na(info$device_id) ||
      length(info$inode) != 1L || is.na(info$inode)) {
    stop(label, " is absent, non-directory, symbolic, or lacks identity metadata",
      call. = FALSE)
  }
  number <- function(value) {
    format(as.numeric(value), scientific = FALSE, trim = TRUE, digits = 17L)
  }
  c(device = number(info$device_id), inode = number(info$inode))
}
reverse_directory_is_empty <- function(path, label) {
  entries <- fs::dir_ls(
    path, all = TRUE, recurse = FALSE, type = "any", fail = TRUE
  )
  if (length(entries)) {
    stop(label, " must be empty for a fresh run", call. = FALSE)
  }
  invisible(TRUE)
}
reverse_assert_reserved_run_directory <- function(
  path, identity, require_empty, context
) {
  observed_path <- reverse_require_plain_local_directory(
    path, "reserved run directory"
  )
  if (!identical(observed_path, path)) {
    stop("reserved run directory path changed ", context, call. = FALSE)
  }
  observed <- reverse_directory_identity(path, "reserved run directory")
  if (!identical(observed, identity)) {
    stop("reserved run directory identity changed ", context, call. = FALSE)
  }
  if (require_empty) {
    reverse_directory_is_empty(path, "reserved run directory")
    observed_after_empty_check <- reverse_directory_identity(
      path, "reserved run directory"
    )
    if (!identical(observed_after_empty_check, identity)) {
      stop(
        "reserved run directory identity changed while checking emptiness ",
        context,
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}
for (path in c(file.path(root, ".local"), file.path(root, ".local", "compat"))) {
  reverse_require_plain_directory(path, "reverse-dependency evidence parent")
}
reverse_runs_root <- file.path(root, ".local", "compat", "reverse-runs")
if (dir.exists(reverse_runs_root)) {
  reverse_require_plain_directory(reverse_runs_root, "reverse-dependency evidence root")
  reverse_runs_root <- normalizePath(reverse_runs_root, winslash = "/", mustWork = TRUE)
  if (!identical(dirname(reverse_runs_root), file.path(root, ".local", "compat"))) {
    stop("reverse-dependency evidence root escaped the repository", call. = FALSE)
  }
} else if (file.exists(reverse_runs_root) || reverse_is_symbolic(reverse_runs_root)) {
  stop("reverse-dependency evidence root is not a plain directory", call. = FALSE)
}
derived_run_directory <- file.path(
  root, ".local", "compat", "reverse-runs", run_id
)
reserved_run_directory <- arguments$reserved_run_directory
reserved_run_identity <- NULL
if (!is.null(reserved_run_directory)) {
  if (!identical(reserved_run_directory, derived_run_directory)) {
    stop(
      "--reserved-run-directory must exactly equal the output derived from ",
      "--root and --run-id: ", derived_run_directory,
      call. = FALSE
    )
  }
  reserved_run_directory <- reverse_require_plain_local_directory(
    reserved_run_directory, "reserved run directory"
  )
  reserved_run_identity <- reverse_directory_identity(
    reserved_run_directory, "reserved run directory"
  )
  if (!arguments$resume) {
    reverse_assert_reserved_run_directory(
      reserved_run_directory,
      reserved_run_identity,
      require_empty = TRUE,
      context = "during initial admission"
    )
  }
  run_directory <- reserved_run_directory
} else {
  run_directory <- file.path(reverse_runs_root, run_id)
  if (!arguments$resume && (file.exists(run_directory) ||
      dir.exists(run_directory) || reverse_is_symbolic(run_directory))) {
    stop("run directory already exists: ", run_directory, call. = FALSE)
  }
  if (arguments$resume && (!dir.exists(run_directory) ||
      reverse_is_symbolic(run_directory))) {
    stop("--resume run is absent or symbolic: ", run_directory, call. = FALSE)
  }
}
reverse_read_tsv <- function(path, expected_columns) {
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
  for (column in expected_columns) {
    if (any(!nzchar(value[[column]]))) {
      stop("manifest contains an empty ", column, " field: ", path, call. = FALSE)
    }
  }
  value
}

reverse_git <- function(arguments, label) {
  error_file <- tempfile("paradox-reverse-git-")
  on.exit(unlink(error_file), add = TRUE)
  output <- suppressWarnings(system2(
    "git",
    c("-C", shQuote(root), arguments),
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

reverse_single_git_value <- function(arguments, label) {
  output <- reverse_git(arguments, label)
  if (length(output) != 1L || !nzchar(output[[1L]])) {
    stop("git returned an unexpected result while ", label, call. = FALSE)
  }
  output[[1L]]
}

reverse_require_regular_provenance_file <- function(path, label) {
  link <- Sys.readlink(path)
  regular_status <- suppressWarnings(system2(
    "/usr/bin/test", c("-f", shQuote(path)), stdout = FALSE, stderr = FALSE
  ))
  if (!identical(as.integer(regular_status), 0L) ||
      !file.exists(path) || dir.exists(path) ||
      length(link) != 1L ||
      is.na(link) || nzchar(link)) {
    stop(label, " is missing, not a regular file, or symbolic: ", path, call. = FALSE)
  }
  invisible(path)
}

reverse_retain_plan_compat_input <- function(source, destination) {
  source <- reverse_require_regular_provenance_file(
    source, "plan compatibility-system source input"
  )
  destination_exists <- file.exists(destination) || dir.exists(destination) ||
    reverse_is_symbolic(destination)
  if (destination_exists) {
    destination <- reverse_require_regular_provenance_file(
      destination, "pre-retained plan compatibility-system input"
    )
    if (!identical(
        unname(tools::sha256sum(source)),
        unname(tools::sha256sum(destination))
      )) {
      stop("pre-retained plan compatibility-system input differs from source",
        call. = FALSE)
    }
    return(destination)
  }

  if (!file.copy(source, destination, copy.mode = TRUE, copy.date = TRUE)) {
    stop("could not retain plan compatibility-system input", call. = FALSE)
  }
  destination <- reverse_require_regular_provenance_file(
    destination, "retained plan compatibility-system input"
  )
  if (!identical(
      unname(tools::sha256sum(source)),
      unname(tools::sha256sum(destination))
    )) {
    stop("retained plan compatibility-system input differs from source",
      call. = FALSE)
  }
  destination
}

reverse_validate_candidate_provenance <- function(
  root,
  candidate_run_id,
  candidate_library,
  dependency_library,
  dependency_library_content_sha256,
  candidate_ref,
  candidate_commit,
  candidate_tree,
  candidate_version,
  candidate_content
) {
  provenance_path <- file.path(
    candidate_library,
    ".paradox-candidate-provenance.tsv"
  )
  seal_path <- file.path(
    candidate_library,
    ".paradox-candidate-provenance.sha256"
  )
  reverse_require_regular_provenance_file(
    provenance_path,
    "candidate provenance receipt"
  )
  reverse_require_regular_provenance_file(seal_path, "candidate provenance seal")

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
    candidate_content_sha256 = candidate_content
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
  reverse_require_regular_provenance_file(installer_path, "candidate installer")
  installer_sha256 <- unname(tools::sha256sum(installer_path))
  if (!identical(values[["installer_sha256"]], installer_sha256)) {
    stop("candidate provenance receipt was written by a different installer", call. = FALSE)
  }
  git_authenticator_path <- file.path(root, "compat", "authenticate-candidate-git")
  reverse_require_regular_provenance_file(
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

candidate_ref <- arguments$candidate_ref
candidate_commit <- arguments$candidate_commit
candidate_tree <- arguments$candidate_tree
candidate_content <- arguments$candidate_content
object_hash_pattern <- "^([0-9a-f]{40}|[0-9a-f]{64})$"
candidate_source_argument <- arguments$candidate_source
if (!nzchar(candidate_source_argument) && grepl(object_hash_pattern, candidate_commit)) {
  candidate_source_argument <- file.path(
    root, ".local", "compat", "candidate-snapshots", candidate_commit
  )
}
candidate_source <- reverse_require_plain_local_directory(
  candidate_source_argument, "detached candidate source"
)
git_authenticator_script <- file.path(
  root, "compat", "authenticate-candidate-git"
)
reverse_require_regular_provenance_file(
  git_authenticator_script, "candidate Git authenticator"
)
if (file.access(git_authenticator_script, mode = 1L) != 0L) {
  stop("candidate Git authenticator is not executable", call. = FALSE)
}
if (!grepl("^refs/", candidate_ref) ||
    !identical(
      suppressWarnings(system2(
        "git",
        c("-C", shQuote(root), "check-ref-format", shQuote(candidate_ref)),
        stdout = FALSE,
        stderr = FALSE
      )),
      0L
    )) {
  stop("candidate ref must be one valid full Git ref", call. = FALSE)
}
if (!grepl(object_hash_pattern, candidate_commit)) {
  stop("candidate commit must be a lowercase Git object hash", call. = FALSE)
}
if (!grepl(object_hash_pattern, candidate_tree)) {
  stop("candidate tree must be a lowercase Git object hash", call. = FALSE)
}
if (!grepl("^[0-9a-f]{64}$", candidate_content)) {
  stop("candidate content fingerprint must be one lowercase SHA-256", call. = FALSE)
}

reverse_candidate_source_state <- function() {
  authentication <- suppressWarnings(system2(
    git_authenticator_script,
    vapply(
      c(root, candidate_ref, candidate_commit, candidate_tree, candidate_source),
      shQuote,
      character(1L)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  authentication_status <- as.integer(attr(authentication, "status") %||% 0L)
  if (!identical(authentication_status, 0L) ||
      !identical(authentication, "candidate_git_authentication=passed")) {
    stop(
      "candidate Git authentication failed",
      if (length(authentication)) {
        paste0(": ", paste(authentication, collapse = "\n"))
      } else {
        ""
      },
      call. = FALSE
    )
  }
  list(
    ref_commit = reverse_single_git_value(
      c("rev-parse", "--verify", shQuote(paste0(candidate_ref, "^{commit}"))),
      "resolving the candidate ref commit"
    ),
    ref_tree = reverse_single_git_value(
      c("rev-parse", "--verify", shQuote(paste0(candidate_ref, "^{tree}"))),
      "resolving the candidate ref tree"
    ),
    commit_tree = reverse_single_git_value(
      c("rev-parse", "--verify", shQuote(paste0(candidate_commit, "^{tree}"))),
      "resolving the declared candidate commit tree"
    )
  )
}

reverse_candidate_source_matches <- function(state) {
  identical(candidate_commit, state$ref_commit) &&
    identical(candidate_tree, state$ref_tree) &&
    identical(candidate_tree, state$commit_tree)
}

candidate_source_initial <- reverse_candidate_source_state()
if (!reverse_candidate_source_matches(candidate_source_initial)) {
  stop("candidate ref, commit, and tree do not identify the same Git source", call. = FALSE)
}

fingerprint_script <- file.path(root, "compat", "fingerprint.R")
harness_script <- file.path(root, "compat", "test-reverse-dependencies.R")
evidence_helper_script <- file.path(root, "compat", "repository-evidence.R")
evidence_verifier_script <- file.path(root, "compat", "verify-repository-evidence.R")
reverse_evidence_verifier_script <- file.path(
  root, "compat", "verify-reverse-dependency-evidence.R"
)
compat_system_evidence_script <- file.path(
  root, "compat", "compat-system-evidence.R"
)
tinytex_manifest_path <- file.path(
  root, "environment", "tinytex-linux-x86_64.tsv"
)
tree_receipt_script <- file.path(
  root, "scripts", "environment", "tree-receipt.R"
)
resource_jobs_script <- file.path(
  root, "scripts", "environment", "resource-jobs"
)
for (path in c(
  fingerprint_script, harness_script, evidence_helper_script,
  evidence_verifier_script, reverse_evidence_verifier_script,
  compat_system_evidence_script,
  tinytex_manifest_path, tree_receipt_script, resource_jobs_script
)) {
  reverse_require_regular_provenance_file(path, "reverse-dependency harness input")
}
sys.source(fingerprint_script, envir = environment())
sys.source(evidence_helper_script, envir = environment())
sys.source(compat_system_evidence_script, envir = environment())

tinytex_manifest <- reverse_read_tsv(
  tinytex_manifest_path,
  c(
    "version", "platform", "archive", "archive_size", "archive_sha256",
    "tree_manifest_sha256", "url"
  )
)
if (nrow(tinytex_manifest) != 1L) {
  stop("TinyTeX manifest must contain exactly one row", call. = FALSE)
}
tinytex_version <- tinytex_manifest$version[[1L]]
tinytex_archive_name <- tinytex_manifest$archive[[1L]]
tinytex_archive_size <- tinytex_manifest$archive_size[[1L]]
tinytex_archive_sha256 <- tinytex_manifest$archive_sha256[[1L]]
tinytex_tree_sha256 <- tinytex_manifest$tree_manifest_sha256[[1L]]
tinytex_url <- tinytex_manifest$url[[1L]]
if (!identical(tinytex_version, "2026.07") ||
    !identical(tinytex_manifest$platform[[1L]], "linux-x86_64") ||
    !identical(
      tinytex_archive_name,
      paste0("TinyTeX-linux-x86_64-v", tinytex_version, ".tar.xz")
    ) ||
    !grepl("^(0|[1-9][0-9]*)$", tinytex_archive_size) ||
    !grepl("^[0-9a-f]{64}$", tinytex_archive_sha256) ||
    !grepl("^[0-9a-f]{64}$", tinytex_tree_sha256) ||
    !identical(
      tinytex_url,
      paste0(
        "https://github.com/rstudio/tinytex-releases/releases/download/v",
        tinytex_version, "/", tinytex_archive_name
      )
    )) {
  stop("TinyTeX manifest identity or checksums are malformed", call. = FALSE)
}

tinytex_root <- reverse_require_plain_local_directory(
  file.path(root, ".local", "tinytex"), "TinyTeX distribution"
)
tinytex_bin <- file.path(tinytex_root, "bin", "x86_64-linux")
tinytex_tool_names <- c("pdflatex", "kpsewhich", "makeindex")
tinytex_tool_paths <- setNames(
  file.path(tinytex_bin, tinytex_tool_names), tinytex_tool_names
)
tinytex_archive <- file.path(
  root, ".cache", "downloads", tinytex_archive_name
)
reverse_require_regular_provenance_file(tinytex_archive, "TinyTeX archive")
tinytex_archive_parent <- normalizePath(
  dirname(tinytex_archive), winslash = "/", mustWork = TRUE
)
if (!identical(
    tinytex_archive_parent,
    file.path(root, ".cache", "downloads")
  )) {
  stop("TinyTeX archive directory escaped the repository cache", call. = FALSE)
}

texi2dvi_path <- file.path(root, ".local", "toolchain", "bin", "texi2dvi")
reverse_require_regular_provenance_file(actual_r, "local R")
reverse_require_regular_provenance_file(actual_rscript, "local Rscript")
reverse_require_regular_provenance_file(texi2dvi_path, "local texi2dvi")
if (!identical(unname(Sys.which("R")), actual_r) ||
    !identical(unname(Sys.which("Rscript")), actual_rscript) ||
    !identical(unname(Sys.which("texi2dvi")), texi2dvi_path)) {
  stop("R, Rscript, or texi2dvi does not resolve to the exact local toolchain",
    call. = FALSE)
}
tinytex_tool_sha256 <- unname(tools::sha256sum(tinytex_tool_paths))
names(tinytex_tool_sha256) <- tinytex_tool_names
texi2dvi_sha256 <- unname(tools::sha256sum(texi2dvi_path))

reverse_validate_tinytex_live <- function(context) {
  reverse_require_regular_provenance_file(tinytex_archive, "TinyTeX archive")
  observed_size <- format(
    file.info(tinytex_archive, extra_cols = FALSE)$size,
    scientific = FALSE,
    trim = TRUE
  )
  if (!identical(observed_size, tinytex_archive_size)) {
    stop("TinyTeX archive size changed ", context, call. = FALSE)
  }
  observed_tools <- unname(Sys.which(tinytex_tool_names))
  if (!identical(observed_tools, unname(tinytex_tool_paths)) ||
      any(!file.exists(tinytex_tool_paths)) ||
      any(file.access(tinytex_tool_paths, mode = 1L) != 0L) ||
      !identical(
        unname(tools::sha256sum(tinytex_tool_paths)),
        unname(tinytex_tool_sha256)
      ) ||
      !identical(unname(Sys.which("texi2dvi")), texi2dvi_path) ||
      file.access(texi2dvi_path, mode = 1L) != 0L ||
      !identical(unname(tools::sha256sum(texi2dvi_path)), texi2dvi_sha256)) {
    stop(
      "pdflatex, kpsewhich, makeindex, or texi2dvi changed ", context,
      call. = FALSE
    )
  }
  invisible(TRUE)
}
reverse_validate_tinytex_live("before reserving evidence")

candidate_package <- normalizePath(
  find.package("paradox", lib.loc = candidate_library),
  winslash = "/",
  mustWork = TRUE
)
if (!identical(dirname(candidate_package), candidate_library)) {
  stop("paradox did not resolve directly from the candidate library", call. = FALSE)
}
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
candidate_version <- as.character(utils::packageVersion("paradox", lib.loc = candidate_library))
preliminary_provenance_path <- file.path(
  candidate_library, ".paradox-candidate-provenance.tsv"
)
reverse_require_regular_provenance_file(
  preliminary_provenance_path, "candidate provenance receipt"
)
preliminary_provenance <- utils::read.delim(
  preliminary_provenance_path, header = TRUE, sep = "\t", quote = "",
  comment.char = "", colClasses = "character", check.names = FALSE,
  stringsAsFactors = FALSE
)
dependency_rows <- which(
  preliminary_provenance$key == "dependency_library_content_sha256"
)
if (!identical(names(preliminary_provenance), c("key", "value")) ||
    length(dependency_rows) != 1L ||
    !grepl("^[0-9a-f]{64}$", preliminary_provenance$value[[dependency_rows]])) {
  stop("candidate provenance lacks one dependency-library content hash",
    call. = FALSE)
}
dependency_library_declared <- preliminary_provenance$value[[dependency_rows]]
candidate_provenance <- reverse_validate_candidate_provenance(
  root = root,
  candidate_run_id = candidate_run_id,
  candidate_library = candidate_library,
  dependency_library = dependency_library,
  dependency_library_content_sha256 = dependency_library_declared,
  candidate_ref = candidate_ref,
  candidate_commit = candidate_commit,
  candidate_tree = candidate_tree,
  candidate_version = candidate_version,
  candidate_content = candidate_content
)

reverse_marker_temporary <- function(path, expected) {
  paste0(path, ".new-", rr_object_sha256(expected))
}
reverse_validate_exact_marker <- function(path, expected, label) {
  path <- rr_require_file(path, label)
  observed <- rr_read_tsv(path, c("key", "value"))
  if (!identical(observed, expected)) {
    stop(label, " disagrees with this reserved run", call. = FALSE)
  }
  invisible(path)
}
reverse_remove_marker_temporary <- function(path, expected, label) {
  temporary <- reverse_marker_temporary(path, expected)
  if (!file.exists(temporary) && !dir.exists(temporary) &&
      !reverse_is_symbolic(temporary)) {
    return(invisible(TRUE))
  }
  if (!file.exists(temporary) || dir.exists(temporary) ||
      reverse_is_symbolic(temporary) ||
      !identical(rr_path_type(temporary), "file")) {
    stop("interrupted ", label, " publication is unsafe", call. = FALSE)
  }
  if (unlink(temporary, force = TRUE) != 0L || file.exists(temporary) ||
      reverse_is_symbolic(temporary)) {
    stop("could not remove interrupted ", label, " publication",
      call. = FALSE)
  }
  invisible(TRUE)
}
reverse_publish_exact_marker <- function(path, expected, label) {
  if (file.exists(path) || dir.exists(path) || reverse_is_symbolic(path)) {
    reverse_validate_exact_marker(path, expected, label)
    reverse_remove_marker_temporary(path, expected, label)
    return(invisible(path))
  }
  reverse_remove_marker_temporary(path, expected, label)
  temporary <- reverse_marker_temporary(path, expected)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.table(
    expected, temporary, quote = FALSE, sep = "\t", row.names = FALSE,
    na = "-", fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) {
    stop("could not atomically publish ", label, call. = FALSE)
  }
  reverse_validate_exact_marker(path, expected, label)
  invisible(path)
}
reverse_reset_initialization_children <- function(run_directory, children) {
  for (child in children) {
    path <- file.path(run_directory, child)
    if (file.exists(path) || dir.exists(path) || reverse_is_symbolic(path)) {
      if (!dir.exists(path) || reverse_is_symbolic(path) ||
          !identical(rr_path_type(path), "directory")) {
        stop("pre-initialization evidence child is unsafe: ", path,
          call. = FALSE)
      }
      if (unlink(path, recursive = TRUE, force = TRUE) != 0L ||
          file.exists(path) || dir.exists(path) ||
          reverse_is_symbolic(path)) {
        stop("could not reset interrupted initialization child: ", path,
          call. = FALSE)
      }
    }
    if (!dir.create(path, recursive = FALSE, showWarnings = FALSE)) {
      stop("could not create reverse-dependency evidence child: ", path,
        call. = FALSE)
    }
  }
  invisible(TRUE)
}
reverse_reset_initialization_files <- function(run_directory, files) {
  for (file in c(files, paste0(files, ".new"))) {
    path <- file.path(run_directory, file)
    if (!file.exists(path) && !dir.exists(path) &&
        !reverse_is_symbolic(path)) {
      next
    }
    if (!file.exists(path) || dir.exists(path) ||
        reverse_is_symbolic(path) ||
        !identical(rr_path_type(path), "file")) {
      stop("pre-initialization evidence file is unsafe: ", path,
        call. = FALSE)
    }
    if (unlink(path, force = TRUE) != 0L || file.exists(path) ||
        reverse_is_symbolic(path)) {
      stop("could not reset interrupted initialization file: ", path,
        call. = FALSE)
    }
  }
  invisible(TRUE)
}
reverse_prepare_reserved_layout <- function(
  run_directory,
  reservation_marker_path,
  reservation_marker,
  initialized_marker_path,
  initialized_marker,
  children,
  files,
  resume_requested
) {
  reservation_temporary <- reverse_marker_temporary(
    reservation_marker_path, reservation_marker
  )
  initial_entries <- list.files(
    run_directory, all.files = TRUE, no.. = TRUE, full.names = TRUE
  )
  if (!resume_requested && length(initial_entries)) {
    stop("fresh reserved run directory is no longer empty", call. = FALSE)
  }
  if (resume_requested && !file.exists(reservation_marker_path)) {
    if (length(setdiff(initial_entries, reservation_temporary))) {
      stop(
        "resumed reserved output is neither empty nor an authenticated ",
        "interrupted initialization",
        call. = FALSE
      )
    }
  }
  reverse_publish_exact_marker(
    reservation_marker_path,
    reservation_marker,
    "reverse reservation marker"
  )
  initialized_temporary <- reverse_marker_temporary(
    initialized_marker_path, initialized_marker
  )
  reverse_remove_marker_temporary(
    initialized_marker_path,
    initialized_marker,
    "reverse initialized marker"
  )
  current_entries <- list.files(
    run_directory, all.files = TRUE, no.. = TRUE, full.names = TRUE
  )
  allowed_entries <- file.path(
    run_directory,
    c(
      basename(reservation_marker_path),
      basename(initialized_marker_path),
      basename(initialized_temporary),
      children,
      files,
      paste0(files, ".new")
    )
  )
  if (length(setdiff(current_entries, allowed_entries))) {
    stop("reserved run directory contains unrelated output", call. = FALSE)
  }
  initialized_exists <- file.exists(initialized_marker_path) ||
    dir.exists(initialized_marker_path) ||
    reverse_is_symbolic(initialized_marker_path)
  if (initialized_exists) {
    reverse_validate_exact_marker(
      initialized_marker_path,
      initialized_marker,
      "reverse initialized marker"
    )
    if (!resume_requested) {
      stop("fresh reserved run unexpectedly has initialized evidence",
        call. = FALSE)
    }
    for (child in children) {
      reverse_require_plain_directory(
        file.path(run_directory, child),
        "initialized reverse-dependency evidence child"
      )
    }
    for (file in files) {
      rr_require_file(
        file.path(run_directory, file),
        "initialized reverse-dependency evidence file"
      )
      if (file.exists(file.path(run_directory, paste0(file, ".new"))) ||
          reverse_is_symbolic(file.path(
            run_directory, paste0(file, ".new")
          ))) {
        stop("initialized reverse-dependency evidence has a stale temporary",
          call. = FALSE)
      }
    }
    return(TRUE)
  }

  for (child in intersect(children, c("packages", "interrupted"))) {
    path <- file.path(run_directory, child)
    if (dir.exists(path) && length(list.files(
        path, all.files = TRUE, no.. = TRUE
      ))) {
      stop(
        "uninitialized reservation contains post-boundary child evidence: ",
        path,
        call. = FALSE
      )
    }
  }
  if (file.exists(file.path(
      run_directory, "metadata", "completion.seal"
    ))) {
    stop("uninitialized reservation contains a completion seal",
      call. = FALSE)
  }
  reverse_reset_initialization_files(run_directory, files)
  reverse_reset_initialization_children(run_directory, children)
  # A retry before the initialized marker is a safe authenticated
  # reinitialization, not a semantic resume.
  FALSE
}

# Candidate provenance must be fully authenticated before the harness creates
# retained artifacts or inspects any consumer source archive.
if (!dir.exists(reverse_runs_root)) {
  created <- dir.create(
    reverse_runs_root, recursive = FALSE, showWarnings = FALSE
  )
  if (!created && !dir.exists(reverse_runs_root)) {
    stop("could not reserve reverse-dependency evidence root", call. = FALSE)
  }
}
reverse_require_plain_directory(reverse_runs_root, "reverse-dependency evidence root")
reverse_runs_root <- normalizePath(reverse_runs_root, winslash = "/", mustWork = TRUE)
if (!identical(dirname(reverse_runs_root), file.path(root, ".local", "compat"))) {
  stop("reverse-dependency evidence root escaped the repository", call. = FALSE)
}
reverse_locks_root <- file.path(reverse_runs_root, ".locks")
if (!dir.exists(reverse_locks_root)) {
  created <- dir.create(
    reverse_locks_root, recursive = FALSE, showWarnings = FALSE
  )
  if (!created && !dir.exists(reverse_locks_root)) {
    stop("could not reserve reverse-dependency mutation-lock root", call. = FALSE)
  }
}
reverse_mutation_lock <- rr_acquire_mutation_lock(reverse_locks_root, run_id)
reverse_release_mutation_lock <- function() {
  if (!is.null(reverse_mutation_lock)) {
    rr_release_mutation_lock(reverse_mutation_lock)
    reverse_mutation_lock <<- NULL
  }
  invisible(TRUE)
}
run_directory <- file.path(reverse_runs_root, run_id)
reserved_initialization_marker_path <- NULL
reserved_initialization_marker <- NULL
reserved_initialized_marker_path <- NULL
reserved_initialized_marker <- NULL
reserved_initialization_children <- c("metadata", "packages", "interrupted")
reserved_initialization_files <- "plan.tsv"
if (!is.null(reserved_run_identity)) {
  if (!identical(run_directory, reserved_run_directory)) {
    stop("derived reserved run directory changed during admission", call. = FALSE)
  }
  reverse_assert_reserved_run_directory(
    run_directory,
    reserved_run_identity,
    require_empty = !arguments$resume,
    context = "immediately before population"
  )
  marker_values <- c(
    schema = "1",
    state = "reserved",
    run_id = run_id,
    root = root,
    run_directory = run_directory,
    reservation_device = unname(reserved_run_identity[["device"]]),
    reservation_inode = unname(reserved_run_identity[["inode"]]),
    max_priority = as.character(max_priority),
    plan_only = as.character(arguments$plan_only),
    packages = if (length(arguments$packages)) {
      paste(arguments$packages, collapse = ",")
    } else "-",
    install_timeout_seconds = as.character(install_timeout),
    check_timeout_seconds = as.character(check_timeout),
    reverse_jobs = Sys.getenv("PARADOX_REVERSE_JOBS", unset = "-"),
    candidate_run_id = candidate_run_id,
    candidate_ref = candidate_ref,
    candidate_commit = candidate_commit,
    candidate_tree = candidate_tree,
    candidate_source = candidate_source,
    candidate_library = candidate_library,
    dependency_library = dependency_library,
    candidate_content_sha256 = candidate_content,
    candidate_version = candidate_version,
    candidate_provenance_sha256 = candidate_provenance$receipt_sha256,
    r_version = as.character(getRversion()),
    harness_sha256 = unname(tools::sha256sum(harness_script))
  )
  if (any(!nzchar(marker_values)) ||
      any(grepl("[\t\r\n]", marker_values))) {
    stop("reserved-run initialization identity is malformed", call. = FALSE)
  }
  reserved_initialization_marker <- data.frame(
    key = names(marker_values),
    value = unname(marker_values),
    stringsAsFactors = FALSE
  )
  reserved_initialization_marker_path <- file.path(
    run_directory, ".paradox-reverse-reservation.tsv"
  )
  reservation_sha256 <- rr_object_sha256(reserved_initialization_marker)
  initialized_values <- c(
    schema = "1",
    state = "initialized",
    reservation_marker_identity_sha256 = reservation_sha256,
    children = paste(reserved_initialization_children, collapse = ","),
    files = paste(reserved_initialization_files, collapse = ",")
  )
  reserved_initialized_marker <- data.frame(
    key = names(initialized_values),
    value = unname(initialized_values),
    stringsAsFactors = FALSE
  )
  reserved_initialized_marker_path <- file.path(
    run_directory, ".paradox-reverse-initialized.tsv"
  )
  arguments$resume <- reverse_prepare_reserved_layout(
    run_directory,
    reserved_initialization_marker_path,
    reserved_initialization_marker,
    reserved_initialized_marker_path,
    reserved_initialized_marker,
    reserved_initialization_children,
    reserved_initialization_files,
    arguments$resume
  )
} else if (!arguments$resume) {
  if (file.exists(run_directory) || dir.exists(run_directory) ||
      reverse_is_symbolic(run_directory)) {
    stop("run directory already exists: ", run_directory, call. = FALSE)
  }
  if (!dir.create(run_directory, recursive = FALSE, showWarnings = FALSE)) {
    stop("could not reserve reverse-dependency run directory", call. = FALSE)
  }
} else if (!dir.exists(run_directory) || reverse_is_symbolic(run_directory)) {
  stop("--resume run is absent or symbolic: ", run_directory, call. = FALSE)
}
run_directory <- normalizePath(run_directory, winslash = "/", mustWork = TRUE)
if (!identical(dirname(run_directory), reverse_runs_root)) {
  stop("reverse-dependency run directory escaped its evidence root", call. = FALSE)
}
for (child in reserved_initialization_children) {
  child_path <- file.path(run_directory, child)
  if (is.null(reserved_run_identity) && !arguments$resume &&
      !dir.create(child_path, recursive = FALSE, showWarnings = FALSE)) {
    stop("could not create reverse-dependency evidence child: ", child_path,
      call. = FALSE)
  }
  if ((arguments$resume || !is.null(reserved_run_identity)) &&
      (!dir.exists(child_path) || reverse_is_symbolic(child_path))) {
    stop("resume evidence child is absent or symbolic: ", child_path,
      call. = FALSE)
  }
}

if (arguments$resume && file.exists(file.path(
    run_directory, "metadata", "completion.seal"
  ))) {
  if (!is.null(reserved_run_identity)) {
    reverse_validate_exact_marker(
      reserved_initialized_marker_path,
      reserved_initialized_marker,
      "reverse initialized marker"
    )
    reverse_assert_reserved_run_directory(
      run_directory,
      reserved_run_identity,
      require_empty = FALSE,
      context = "before completed-run verification"
    )
  }
  trusted_verifier <- file.path(
    root, "compat", "verify-reverse-dependency-evidence.R"
  )
  if (!file.exists(trusted_verifier) || dir.exists(trusted_verifier) ||
      nzchar(Sys.readlink(trusted_verifier))) {
    stop("completed resume lacks the trusted reverse evidence verifier",
      call. = FALSE)
  }
  verification <- suppressWarnings(system2(
    actual_rscript,
    c("--vanilla", shQuote(trusted_verifier), shQuote(run_directory), "--quiet"),
    stdout = TRUE, stderr = TRUE
  ))
  verification_status <- as.integer(attr(verification, "status") %||% 0L)
  if (!identical(verification_status, 0L)) {
    stop("completed reverse evidence failed verification: ",
      paste(tail(verification, 30L), collapse = "\n"), call. = FALSE)
  }
  completion <- utils::read.delim(
    file.path(run_directory, "metadata", "completion.tsv"),
    header = TRUE, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE,
    stringsAsFactors = FALSE
  )
  completion <- setNames(completion$value, completion$field)
  reverse_release_mutation_lock()
  cat("Verified completed reverse-dependency evidence: ", run_directory,
    "\n", sep = "")
  if (identical(completion[["status"]], "completed_with_failures")) {
    quit(save = "no", status = 1L)
  }
  quit(save = "no", status = 0L)
}

inventory_path <- file.path(root, "compat", "reverse-dependencies.tsv")
cran_snapshot_path <- file.path(root, "compat", "cran-snapshot.tsv")
bioc_snapshot_path <- file.path(root, "compat", "bioconductor-snapshot.tsv")
reverse_metadata_inputs <- c(
  inventory_path, cran_snapshot_path, bioc_snapshot_path, fingerprint_script,
  harness_script, runner_library_script, install_worker_script,
  wave_worker_script, worker_group_script,
  evidence_helper_script, evidence_verifier_script,
  reverse_evidence_verifier_script,
  compat_system_evidence_script,
  tinytex_manifest_path, tree_receipt_script, resource_jobs_script,
  candidate_provenance$path, candidate_provenance$seal_path,
  file.path(root, "compat", "install-candidate"), git_authenticator_script
)
if (anyDuplicated(basename(reverse_metadata_inputs))) {
  stop("reverse-dependency metadata inputs have colliding retained names",
    call. = FALSE)
}
for (path in reverse_metadata_inputs) {
  reverse_require_regular_provenance_file(path, "reverse-dependency metadata input")
}
reverse_metadata_sha256 <- setNames(
  vapply(
    reverse_metadata_inputs,
    tools::sha256sum,
    character(1L),
    USE.NAMES = FALSE
  ),
  reverse_metadata_inputs
)
inventory <- reverse_read_tsv(
  inventory_path,
  c("relation", "package", "source", "priority", "notes")
)
cran_snapshot <- reverse_read_tsv(
  cran_snapshot_path,
  c(
    "package", "relation", "source", "priority", "notes", "version",
    "repository", "archive", "md5", "sha256"
  )
)
bioc_snapshot <- reverse_read_tsv(
  bioc_snapshot_path,
  c("package", "version", "repository", "archive", "sha256")
)

if (anyDuplicated(inventory$package) || anyDuplicated(cran_snapshot$package) ||
    anyDuplicated(bioc_snapshot$package)) {
  stop("reverse-dependency manifests contain duplicate packages", call. = FALSE)
}
package_name_pattern <- "^[A-Za-z][A-Za-z0-9.]*$"
if (any(!grepl(package_name_pattern, inventory$package)) ||
    any(!grepl(package_name_pattern, cran_snapshot$package)) ||
    any(!grepl(package_name_pattern, bioc_snapshot$package))) {
  stop("reverse-dependency manifests contain an unsafe package name", call. = FALSE)
}
inventory_priority <- suppressWarnings(as.integer(inventory$priority))
if (anyNA(inventory_priority) ||
    !identical(as.character(inventory_priority), inventory$priority) ||
    any(inventory_priority < 0L)) {
  stop("reverse-dependency inventory has invalid priorities", call. = FALSE)
}
inventory$priority <- inventory_priority
if (!all(inventory$source %in% c("CRAN", "Bioconductor"))) {
  stop("reverse-dependency inventory contains an unsupported source", call. = FALSE)
}

source_directories <- c(
  CRAN = file.path(root, ".local", "compat", "cran-sources"),
  Bioconductor = file.path(root, ".local", "compat", "bioconductor-sources")
)
source_directories[] <- vapply(
  names(source_directories),
  function(source) reverse_require_plain_local_directory(
    source_directories[[source]], paste(source, "source directory")
  ),
  character(1L)
)

selected_inventory <- inventory[inventory$priority <= max_priority, , drop = FALSE]
if (length(arguments$packages)) {
  if (anyDuplicated(arguments$packages)) stop("--package values must be unique", call. = FALSE)
  unknown_packages <- setdiff(arguments$packages, selected_inventory$package)
  if (length(unknown_packages)) {
    stop(
      "selected package is absent at this priority: ",
      paste(unknown_packages, collapse = ", "),
      call. = FALSE
    )
  }
  selected_inventory <- selected_inventory[
    match(arguments$packages, selected_inventory$package),
    ,
    drop = FALSE
  ]
} else {
  # Heaviest-first launch order (a scheduling hint, not evidence): the
  # longest checks start immediately and the continuous-refill wave packs
  # the tail behind them.  An explicit --package order stays the
  # operator's; an unhinted package keeps its reviewed inventory position
  # after the hinted rows.
  selected_inventory <- selected_inventory[
    rr_reverse_heaviest_first_order(selected_inventory$package),
    ,
    drop = FALSE
  ]
}
if (!nrow(selected_inventory)) stop("priority selection is empty", call. = FALSE)

reverse_plan_columns <- c(
  "package", "source", "relation", "priority", "version", "repository",
  "archive", "archive_name", "checksum_type", "declared_checksum",
  "archive_sha256", "archive_metadata_sha256", "notes"
)
retained_plan_for_resume <- if (arguments$resume) {
  reverse_read_tsv(file.path(run_directory, "plan.tsv"), reverse_plan_columns)
} else NULL

reverse_snapshot_row <- function(inventory_row) {
  package <- inventory_row$package[[1L]]
  source <- inventory_row$source[[1L]]
  snapshot <- if (identical(source, "CRAN")) cran_snapshot else bioc_snapshot
  index <- which(snapshot$package == package)
  if (length(index) != 1L) {
    stop("snapshot does not contain exactly one row for ", package, call. = FALSE)
  }
  row <- snapshot[index, , drop = FALSE]
  if (identical(source, "CRAN")) {
    for (field in c("relation", "source", "priority", "notes")) {
      expected <- as.character(inventory_row[[field]][[1L]])
      if (!identical(row[[field]][[1L]], expected)) {
        stop("CRAN snapshot disagrees with inventory for ", package, ": ", field,
          call. = FALSE)
      }
    }
  }
  row
}

reverse_archive_metadata_sha256 <- function(path) {
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("fs is required for source-archive metadata boundaries", call. = FALSE)
  }
  path <- reverse_require_regular_provenance_file(path, "pinned source archive")
  info <- fs::file_info(path, fail = TRUE, follow = FALSE)
  if (!identical(as.character(info$type), "file")) {
    stop("pinned source archive is not one regular file", call. = FALSE)
  }
  number <- function(value) format(
    as.numeric(value), scientific = FALSE, trim = TRUE, digits = 17L
  )
  rr_payload_sha256(data.frame(
    path = path, type = as.character(info$type),
    mode = sprintf("%04o", as.integer(info$permissions)),
    size = number(info$size), mtime = number(info$modification_time),
    ctime = number(info$change_time), device = number(info$device_id),
    inode = number(info$inode), hard_links = number(info$hard_links),
    stringsAsFactors = FALSE
  ))
}

plan_rows <- vector("list", nrow(selected_inventory))
for (index in seq_len(nrow(selected_inventory))) {
  inventory_row <- selected_inventory[index, , drop = FALSE]
  snapshot_row <- reverse_snapshot_row(inventory_row)
  package <- inventory_row$package[[1L]]
  source <- inventory_row$source[[1L]]
  archive_name <- snapshot_row$archive[[1L]]
  version <- snapshot_row$version[[1L]]
  expected_repository <- if (identical(source, "CRAN")) {
    "https://cloud.r-project.org/src/contrib"
  } else {
    "https://bioconductor.org/packages/3.23/bioc/src/contrib"
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9.+-]*$", version) ||
      !identical(snapshot_row$repository[[1L]], expected_repository) ||
      !identical(archive_name, paste0(package, "_", version, ".tar.gz")) ||
      !identical(basename(archive_name), archive_name) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._+-]*[.]tar[.]gz$", archive_name)) {
    stop("snapshot contains unsafe or inconsistent metadata for ", package,
      call. = FALSE)
  }
  archive <- file.path(source_directories[[source]], archive_name)
  reverse_require_regular_provenance_file(archive, "pinned source archive")
  checksum_type <- "sha256"
  declared_checksum <- tolower(snapshot_row$sha256[[1L]])
  if (!grepl("^[0-9a-f]{64}$", declared_checksum)) {
    stop("snapshot SHA-256 is malformed for ", package, call. = FALSE)
  }
  archive_metadata_sha256 <- reverse_archive_metadata_sha256(archive)
  if (arguments$resume) {
    retained_row <- retained_plan_for_resume[index, , drop = FALSE]
    if (!identical(retained_row$package[[1L]], package) ||
        !identical(retained_row$archive_sha256[[1L]], declared_checksum) ||
        !identical(retained_row$archive_metadata_sha256[[1L]],
          archive_metadata_sha256)) {
      stop("resume source archive identity or metadata changed for ", package,
        call. = FALSE)
    }
    observed_checksum <- declared_checksum
  } else {
    observed_checksum <- unname(tools::sha256sum(archive))
    if (!identical(observed_checksum, declared_checksum)) {
      stop("pinned source archive checksum mismatch for ", package,
        call. = FALSE)
    }
  }
  if (!arguments$resume && identical(source, "CRAN")) {
    declared_md5 <- tolower(snapshot_row$md5[[1L]])
    if (!grepl("^[0-9a-f]{32}$", declared_md5) ||
        !identical(unname(tools::md5sum(archive)), declared_md5)) {
      stop("pinned CRAN source archive MD5 mismatch for ", package, call. = FALSE)
    }
  }
  reverse_require_regular_provenance_file(archive, "pinned source archive")
  plan_rows[[index]] <- data.frame(
    package = package,
    source = source,
    relation = inventory_row$relation[[1L]],
    priority = inventory_row$priority[[1L]],
    version = version,
    repository = snapshot_row$repository[[1L]],
    archive = archive,
    archive_name = archive_name,
    checksum_type = checksum_type,
    declared_checksum = declared_checksum,
    archive_sha256 = observed_checksum,
    archive_metadata_sha256 = archive_metadata_sha256,
    notes = inventory_row$notes[[1L]],
    stringsAsFactors = FALSE
  )
}
plan <- do.call(rbind, plan_rows)

reverse_verify_plan_archive <- function(index, context, content = FALSE) {
  archive <- plan$archive[[index]]
  reverse_require_plain_local_directory(
    dirname(archive), paste(plan$source[[index]], "source directory")
  )
  reverse_require_regular_provenance_file(archive, "pinned source archive")
  observed_metadata <- reverse_archive_metadata_sha256(archive)
  if (!identical(observed_metadata, plan$archive_metadata_sha256[[index]])) {
    stop("pinned source archive metadata changed ", context, ": ", archive,
      call. = FALSE)
  }
  if (isTRUE(content)) {
    observed <- unname(tools::sha256sum(archive))
    if (!identical(observed, plan$archive_sha256[[index]]) ||
        !identical(reverse_archive_metadata_sha256(archive), observed_metadata)) {
      stop("pinned source archive content changed ", context, ": ", archive,
        call. = FALSE)
    }
  }
  invisible(TRUE)
}

reverse_verify_plan_archives <- function(context, content = FALSE) {
  for (index in seq_len(nrow(plan))) {
    reverse_verify_plan_archive(index, context, content = content)
  }
  invisible(TRUE)
}

reverse_verify_plan_archives("while deriving the reverse-dependency plan")

reverse_write_tsv <- function(value, path) {
  temporary <- paste0(path, ".new")
  utils::write.table(
    value,
    temporary,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    na = "-",
    fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) stop("could not atomically write ", path, call. = FALSE)
}

metadata_directory <- file.path(run_directory, "metadata")
reverse_metadata_current_sha256 <- setNames(
  vapply(
    reverse_metadata_inputs,
    tools::sha256sum,
    character(1L),
    USE.NAMES = FALSE
  ),
  reverse_metadata_inputs
)
if (!identical(reverse_metadata_current_sha256, reverse_metadata_sha256)) {
  stop("reverse-dependency metadata inputs changed while deriving the plan",
    call. = FALSE)
}
if (!arguments$resume) {
  copied_metadata <- file.copy(
    reverse_metadata_inputs,
    metadata_directory,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!all(copied_metadata)) {
    stop("could not copy run metadata inputs", call. = FALSE)
  }
}
retained_reverse_inputs <- file.path(
  metadata_directory,
  basename(reverse_metadata_inputs)
)
retained_reverse_sha256 <- vapply(
  retained_reverse_inputs,
  tools::sha256sum,
  character(1L),
  USE.NAMES = FALSE
)
if (!identical(unname(reverse_metadata_sha256), retained_reverse_sha256)) {
  stop("retained reverse-dependency inputs differ from plan inputs", call. = FALSE)
}
reverse_restore_compat_system_evidence <- function(root, metadata_directory) {
  status_path <- file.path(metadata_directory, "compat-system.tsv")
  status_table <- reverse_read_tsv(status_path, c("field", "value"))
  status <- setNames(status_table$value, status_table$field)
  active <- identical(status[["active"]], "true")
  if (!active && !identical(status[["active"]], "false")) {
    stop("retained compatibility-system status is malformed", call. = FALSE)
  }
  if (!active) {
    source_paths <- file.path(root, "compat", "compat-system-evidence.R")
    retained_paths <- c(
      file.path(metadata_directory, "compat-system-evidence.R"), status_path
    )
    return(list(
      active = FALSE, active_root = "", receipt_environment = "",
      child_environment = character(), source_paths = source_paths,
      source_sha256 = unname(tools::sha256sum(source_paths)),
      retained_paths = retained_paths,
      retained_sha256 = unname(tools::sha256sum(retained_paths))
    ))
  }
  source_paths <- c(
    receipt = file.path(root, ".local", "receipts", "compat-system", "receipt.tsv"),
    receipt_seal = file.path(root, ".local", "receipts", "compat-system", "receipt.sha256"),
    geo_lock = file.path(root, "environment", "compat-system-geo-linux-64.lock"),
    p1_lock = file.path(root, "environment", "compat-system-p1-linux-64.lock"),
    bootstrap = file.path(root, "scripts", "bootstrap-compat-system"),
    activation = file.path(root, "scripts", "activate-compat-system"),
    makevars = file.path(root, ".local", "compat", "system", "Makevars"),
    evidence_helper = file.path(root, "compat", "compat-system-evidence.R")
  )
  retained_paths <- c(
    file.path(metadata_directory, c(
      "compat-system-receipt.tsv", "compat-system-receipt.sha256",
      "compat-system-geo-linux-64.lock", "compat-system-p1-linux-64.lock",
      "bootstrap-compat-system", "activate-compat-system",
      "compat-system-Makevars", "compat-system-evidence.R"
    )),
    status_path
  )
  source_paths <- vapply(source_paths, reverse_require_regular_provenance_file,
    character(1L), label = "compatibility-system resume input")
  retained_paths <- vapply(retained_paths, reverse_require_regular_provenance_file,
    character(1L), label = "retained compatibility-system input")
  list(
    active = TRUE,
    active_root = Sys.getenv("PARADOX_COMPAT_SYSTEM_ACTIVE_ROOT", unset = ""),
    receipt_environment = Sys.getenv("PARADOX_COMPAT_SYSTEM_RECEIPT", unset = ""),
    child_environment = compat_system_child_environment(),
    source_paths = unname(source_paths),
    source_sha256 = unname(vapply(source_paths, tools::sha256sum, character(1L))),
    retained_paths = unname(retained_paths),
    retained_sha256 = unname(vapply(retained_paths, tools::sha256sum, character(1L)))
  )
}
reverse_capture_compat_system_plan <- function(root, metadata_directory) {
  active_root <- Sys.getenv("PARADOX_COMPAT_SYSTEM_ACTIVE_ROOT", unset = "")
  receipt_environment <- Sys.getenv("PARADOX_COMPAT_SYSTEM_RECEIPT", unset = "")
  if (!nzchar(active_root)) {
    return(compat_system_capture_evidence(root, metadata_directory))
  }
  if (!dir.exists(active_root) || reverse_is_symbolic(active_root)) {
    stop("active compatibility-system root is absent or symbolic",
      call. = FALSE)
  }
  active_root <- normalizePath(active_root, winslash = "/", mustWork = TRUE)
  if (!identical(active_root, root) || !nzchar(receipt_environment)) {
    stop("plan-only compatibility-system activation is inconsistent",
      call. = FALSE)
  }
  source_paths <- c(
    receipt = file.path(root, ".local", "receipts", "compat-system", "receipt.tsv"),
    receipt_seal = file.path(root, ".local", "receipts", "compat-system", "receipt.sha256"),
    geo_lock = file.path(root, "environment", "compat-system-geo-linux-64.lock"),
    p1_lock = file.path(root, "environment", "compat-system-p1-linux-64.lock"),
    bootstrap = file.path(root, "scripts", "bootstrap-compat-system"),
    activation = file.path(root, "scripts", "activate-compat-system"),
    makevars = file.path(root, ".local", "compat", "system", "Makevars"),
    evidence_helper = file.path(root, "compat", "compat-system-evidence.R")
  )
  source_paths <- vapply(source_paths, reverse_require_regular_provenance_file,
    character(1L), label = "plan compatibility-system input")
  if (!identical(normalizePath(receipt_environment, winslash = "/", mustWork = TRUE),
      unname(source_paths[["receipt"]]))) {
    stop("plan compatibility receipt does not belong to this checkout",
      call. = FALSE)
  }
  retained_names <- c(
    "compat-system-receipt.tsv", "compat-system-receipt.sha256",
    "compat-system-geo-linux-64.lock", "compat-system-p1-linux-64.lock",
    "bootstrap-compat-system", "activate-compat-system",
    "compat-system-Makevars", "compat-system-evidence.R"
  )
  retained_paths <- file.path(metadata_directory, retained_names)
  for (index in seq_along(source_paths)) {
    retained_paths[[index]] <- reverse_retain_plan_compat_input(
      source_paths[[index]], retained_paths[[index]]
    )
  }
  source_sha256 <- unname(vapply(source_paths, tools::sha256sum, character(1L)))
  status_path <- file.path(metadata_directory, "compat-system.tsv")
  reverse_write_tsv(data.frame(
    field = c(
      "schema", "active", "platform", "receipt_sha256",
      "receipt_seal_sha256", "geo_lock_sha256", "p1_lock_sha256",
      "bootstrap_sha256", "activation_sha256", "makevars_sha256",
      "evidence_helper_sha256", "verification_scope"
    ),
    value = c(
      "2", "true", "linux-64", source_sha256,
      "metadata-only-plan; full-overlay-verification-owned-by-actual-stage"
    ), stringsAsFactors = FALSE
  ), status_path)
  retained_paths <- c(retained_paths, status_path)
  list(
    active = TRUE, active_root = active_root,
    receipt_environment = receipt_environment,
    child_environment = compat_system_child_environment(),
    source_paths = unname(source_paths), source_sha256 = source_sha256,
    retained_paths = unname(retained_paths),
    retained_sha256 = unname(vapply(retained_paths, tools::sha256sum, character(1L)))
  )
}
compat_system_evidence <- if (arguments$resume) {
  reverse_restore_compat_system_evidence(root, metadata_directory)
} else if (arguments$plan_only) {
  reverse_capture_compat_system_plan(root, metadata_directory)
} else {
  compat_system_capture_evidence(root, metadata_directory)
}
reverse_validate_compat_light <- function(state, context) {
  if (!identical(
      Sys.getenv("PARADOX_COMPAT_SYSTEM_ACTIVE_ROOT", unset = ""),
      state$active_root
    ) || !identical(
      Sys.getenv("PARADOX_COMPAT_SYSTEM_RECEIPT", unset = ""),
      state$receipt_environment
    ) || !identical(compat_system_child_environment(), state$child_environment) ||
      !identical(
        unname(vapply(state$source_paths, tools::sha256sum, character(1L))),
        state$source_sha256
      ) || !identical(
        unname(vapply(state$retained_paths, tools::sha256sum, character(1L))),
        state$retained_sha256
      )) {
    stop("compatibility-system evidence changed ", context, call. = FALSE)
  }
  invisible(TRUE)
}

reverse_verify_tinytex <- function(context, log_name) {
  reverse_validate_tinytex_live(context)
  log_path <- file.path(metadata_directory, log_name)
  value <- data.frame(
    field = c("scope", "tree_manifest_sha256", "archive_sha256", "context"),
    value = c(
      "exact-tools; archive-and-full-tree-owned-by-bootstrap",
      tinytex_tree_sha256, tinytex_archive_sha256, context
    ), stringsAsFactors = FALSE
  )
  if (file.exists(log_path)) {
    retained <- reverse_read_tsv(log_path, c("field", "value"))
    if (!identical(retained, value)) {
      stop("retained TinyTeX verification marker changed", call. = FALSE)
    }
  } else {
    reverse_write_tsv(value, log_path)
  }
  invisible(TRUE)
}
reverse_verify_tinytex(
  "before reverse-dependency checks", "tinytex-verify-pre.log"
)
plan_path <- file.path(run_directory, "plan.tsv")
if (arguments$resume) {
  retained_plan <- reverse_read_tsv(plan_path, names(plan))
  if (!identical(retained_plan, transform(plan, priority = as.character(priority)))) {
    stop("--resume selection or authenticated plan differs from retained plan",
      call. = FALSE)
  }
} else {
  reverse_write_tsv(plan, plan_path)
}

reverse_resource_report <- function() {
  output <- suppressWarnings(system2(
    resource_jobs_script, c("consumer", "--report"),
    stdout = TRUE, stderr = TRUE
  ))
  status <- as.integer(attr(output, "status") %||% 0L)
  if (!identical(status, 0L) || !length(output)) {
    stop("resource-aware consumer scheduler failed: ",
      paste(output, collapse = "\n"), call. = FALSE)
  }
  connection <- textConnection(output)
  on.exit(close(connection), add = TRUE)
  report <- tryCatch(
    utils::read.delim(
      connection, header = TRUE, sep = "\t", quote = "", comment.char = "",
      colClasses = "character", check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(condition) stop(
      "could not parse resource-aware consumer scheduler report: ",
      conditionMessage(condition), call. = FALSE
    )
  )
  validated <- rr_validate_resource_report(report, "consumer")
  list(report = report, jobs = validated$jobs)
}

current_resource <- reverse_resource_report()
resource_report_path <- file.path(metadata_directory, "resource-jobs-initial.tsv")
operator_jobs_text <- Sys.getenv("PARADOX_REVERSE_JOBS", unset = "")
operator_jobs <- NULL
if (nzchar(operator_jobs_text)) {
  operator_jobs <- suppressWarnings(as.integer(operator_jobs_text))
  if (length(operator_jobs) != 1L || is.na(operator_jobs) || operator_jobs < 1L ||
      !identical(as.character(operator_jobs), operator_jobs_text) ||
      operator_jobs > current_resource$jobs) {
    stop(
      "PARADOX_REVERSE_JOBS must be a positive integer no greater than the ",
      "current automatic consumer ceiling (", current_resource$jobs, ")",
      call. = FALSE
    )
  }
}
if (!arguments$resume) {
  reverse_write_tsv(current_resource$report, resource_report_path)
  scheduler_initial_ceiling <- current_resource$jobs
  scheduler_initial_jobs <- operator_jobs %||% current_resource$jobs
  scheduler_initial_selection <- if (is.null(operator_jobs)) {
    "automatic_resource_ceiling"
  } else {
    "operator_conservative_override"
  }
} else {
  retained_scheduler <- reverse_read_tsv(
    file.path(metadata_directory, "run.tsv"), c("field", "value")
  )
  retained_scheduler <- setNames(retained_scheduler$value, retained_scheduler$field)
  required_scheduler <- c(
    "scheduler_initial_ceiling", "scheduler_initial_jobs",
    "scheduler_initial_selection"
  )
  if (any(!required_scheduler %in% names(retained_scheduler)) ||
      !grepl("^[1-9][0-9]*$", retained_scheduler[["scheduler_initial_ceiling"]]) ||
      !grepl("^[1-9][0-9]*$", retained_scheduler[["scheduler_initial_jobs"]]) ||
      !retained_scheduler[["scheduler_initial_selection"]] %in% c(
        "automatic_resource_ceiling", "operator_conservative_override"
      )) {
    stop("retained reverse scheduler identity is malformed", call. = FALSE)
  }
  scheduler_initial_ceiling <- as.integer(
    retained_scheduler[["scheduler_initial_ceiling"]]
  )
  scheduler_initial_jobs <- as.integer(retained_scheduler[["scheduler_initial_jobs"]])
  scheduler_initial_selection <- retained_scheduler[["scheduler_initial_selection"]]
  if (!file.exists(resource_report_path)) {
    stop("retained initial resource scheduler report is absent", call. = FALSE)
  }
}
if (!is.null(operator_jobs) && operator_jobs > scheduler_initial_jobs) {
  stop("PARADOX_REVERSE_JOBS may not raise the retained scheduler limit (",
    scheduler_initial_jobs, ")", call. = FALSE)
}
resource_report_sha256 <- unname(tools::sha256sum(resource_report_path))

acceptance_path <- file.path(metadata_directory, "accepted.tsv")
waves_path <- file.path(metadata_directory, "waves.tsv")
scheduler_reports_directory <- file.path(
  metadata_directory, "resource-jobs-waves"
)
if (!arguments$resume) {
  if (!dir.create(scheduler_reports_directory, recursive = FALSE,
      showWarnings = FALSE)) {
    stop("could not create per-wave resource report directory", call. = FALSE)
  }
  rr_write_tsv(rr_empty_reverse_acceptance(), acceptance_path)
  rr_write_tsv(rr_empty_reverse_waves(), waves_path)
} else {
  if (!dir.exists(scheduler_reports_directory) ||
      rr_is_symbolic(scheduler_reports_directory)) {
    stop("retained per-wave resource report directory is absent or symbolic",
      call. = FALSE)
  }
  invisible(rr_validate_reverse_acceptance(
    acceptance_path, plan$package, file.path(run_directory, "packages")
  ))
  invisible(rr_validate_reverse_waves(waves_path, plan$package))
}

full_hash_ledger_path <- file.path(
  metadata_directory, "protected-library-full-hash-passes.tsv"
)
full_hash_contexts <- if (arguments$plan_only) character() else {
  c("stage_start", "stage_completion_postflight")
}
protected_state_path <- file.path(metadata_directory, "protected-state.tsv")
candidate_subtree_observed <- NULL
protected_content_hasher <- function(path) {
  if (identical(path, candidate_library)) {
    receipt <- rr_tree_content_receipt(path, "paradox")
    candidate_subtree_observed <<- receipt$subtree_hash
    return(receipt$hash)
  }
  rr_tree_content_sha256(path)
}
if (!arguments$resume) {
  rr_write_tsv(rr_empty_full_hash_ledger(), full_hash_ledger_path)
}
candidate_library_initial <- "not-computed"
dependency_library_initial <- dependency_library_declared
protected_metadata_initial <- NULL
if (!arguments$plan_only) {
  if (!arguments$resume) {
    message("Hashing protected libraries once at actual-stage start ...")
    initial_full_hashes <- rr_record_full_hash_pass(
      full_hash_ledger_path, full_hash_contexts[[1L]], full_hash_contexts,
      candidate_library, dependency_library, expected = NULL,
      hasher = protected_content_hasher
    )
    candidate_library_initial <- initial_full_hashes[[1L]]
    dependency_library_initial <- initial_full_hashes[[2L]]
    if (!identical(dependency_library_initial, dependency_library_declared) ||
        !identical(candidate_subtree_observed, candidate_content)) {
      stop("candidate or dependency library differs from its sealed provenance",
        call. = FALSE)
    }
    protected_metadata_initial <- rr_protected_metadata(
      candidate_library, dependency_library
    )
    reverse_write_tsv(data.frame(
      field = c(
        "candidate_library_metadata_sha256",
        "dependency_library_metadata_sha256",
        "protected_library_metadata_sha256", "candidate_entries",
        "dependency_entries"
      ),
      value = c(
        protected_metadata_initial$candidate,
        protected_metadata_initial$dependency,
        protected_metadata_initial$combined,
        protected_metadata_initial$candidate_entries,
        protected_metadata_initial$dependency_entries
      ), stringsAsFactors = FALSE
    ), protected_state_path)
  } else {
    retained_hashes <- rr_read_full_hash_ledger(full_hash_ledger_path)
    if (!nrow(retained_hashes) || nrow(retained_hashes) > 2L ||
        !identical(retained_hashes$context,
          head(full_hash_contexts, nrow(retained_hashes)))) {
      stop("resume full-hash ledger has an invalid prefix", call. = FALSE)
    }
    candidate_library_initial <-
      retained_hashes$candidate_library_content_sha256[[1L]]
    dependency_library_initial <-
      retained_hashes$dependency_library_content_sha256[[1L]]
    if (!identical(dependency_library_initial, dependency_library_declared)) {
      stop("resume dependency-library identity differs from candidate receipt",
        call. = FALSE)
    }
    state_table <- reverse_read_tsv(protected_state_path, c("field", "value"))
    state <- setNames(state_table$value, state_table$field)
    protected_metadata_initial <- list(
      candidate = state[["candidate_library_metadata_sha256"]],
      dependency = state[["dependency_library_metadata_sha256"]],
      combined = state[["protected_library_metadata_sha256"]],
      candidate_entries = as.integer(state[["candidate_entries"]]),
      dependency_entries = as.integer(state[["dependency_entries"]])
    )
    rr_validate_protected_metadata(
      rr_protected_metadata(candidate_library, dependency_library),
      protected_metadata_initial, "while resuming"
    )
  }
} else {
  rr_validate_full_hash_ledger(full_hash_ledger_path, character())
}

source_date_epoch <- reverse_single_git_value(
  c("show", "-s", "--format=%ct", shQuote(candidate_commit)),
  "reading the candidate commit time"
)
run_metadata <- data.frame(
  field = rr_reverse_run_fields(),
  value = c(
    "5", run_id, format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), root,
    as.character(max_priority), as.character(arguments$plan_only),
    "true", as.character(install_timeout), as.character(check_timeout),
    as.character(worker_timeout),
    as.character(scheduler_initial_ceiling), as.character(scheduler_initial_jobs),
    scheduler_initial_selection,
    unname(reverse_metadata_sha256[[resource_jobs_script]]),
    resource_report_sha256,
    candidate_run_id, candidate_ref,
    candidate_commit, candidate_tree, candidate_source, candidate_version,
    candidate_library,
    candidate_package, candidate_content, candidate_library_initial,
    dependency_library, dependency_library_initial, actual_r,
    as.character(getRversion()), source_date_epoch,
    candidate_provenance$receipt_sha256,
    candidate_provenance$source_archive_sha256,
    candidate_provenance$installer_sha256,
    candidate_provenance$git_authenticator_sha256,
    unname(reverse_metadata_sha256[[inventory_path]]),
    unname(reverse_metadata_sha256[[cran_snapshot_path]]),
    unname(reverse_metadata_sha256[[bioc_snapshot_path]]),
    unname(reverse_metadata_sha256[[runner_library_script]]),
    unname(reverse_metadata_sha256[[install_worker_script]]),
    unname(reverse_metadata_sha256[[wave_worker_script]]),
    unname(reverse_metadata_sha256[[worker_group_script]]),
    unname(reverse_metadata_sha256[[harness_script]]),
    unname(reverse_metadata_sha256[[evidence_helper_script]]),
    unname(reverse_metadata_sha256[[evidence_verifier_script]]),
    unname(reverse_metadata_sha256[[reverse_evidence_verifier_script]]),
    unname(reverse_metadata_sha256[[tinytex_manifest_path]]),
    unname(reverse_metadata_sha256[[tree_receipt_script]]),
    tinytex_root, tinytex_archive_name, tinytex_archive_sha256,
    tinytex_tree_sha256, tinytex_tool_sha256[["pdflatex"]],
    tinytex_tool_sha256[["kpsewhich"]], tinytex_tool_sha256[["makeindex"]],
    texi2dvi_path, texi2dvi_sha256
  ),
  stringsAsFactors = FALSE
)
run_metadata_path <- file.path(metadata_directory, "run.tsv")
if (arguments$resume) {
  retained_run_metadata <- reverse_read_tsv(run_metadata_path, c("field", "value"))
  stable_fields <- setdiff(run_metadata$field, "started_utc")
  expected_stable <- setNames(run_metadata$value, run_metadata$field)[stable_fields]
  observed_stable <- setNames(
    retained_run_metadata$value, retained_run_metadata$field
  )[stable_fields]
  if (!identical(unname(observed_stable), unname(expected_stable))) {
    stop("--resume options, candidate, toolchain, or harness identity changed",
      call. = FALSE)
  }
} else {
  reverse_write_tsv(run_metadata, run_metadata_path)
}

if (!is.null(reserved_run_identity)) {
  reverse_assert_reserved_run_directory(
    run_directory,
    reserved_run_identity,
    require_empty = FALSE,
    context = "at the durable initialization boundary"
  )
  reverse_validate_exact_marker(
    reserved_initialization_marker_path,
    reserved_initialization_marker,
    "reverse reservation marker"
  )
  if (arguments$resume) {
    reverse_validate_exact_marker(
      reserved_initialized_marker_path,
      reserved_initialized_marker,
      "reverse initialized marker"
    )
  } else {
    reverse_publish_exact_marker(
      reserved_initialized_marker_path,
      reserved_initialized_marker,
      "reverse initialized marker"
    )
  }
}

if (arguments$plan_only) {
  reverse_verify_plan_archives("during final plan validation")
  reverse_verify_tinytex(
    "after reverse-dependency plan validation", "tinytex-verify-post.log"
  )
  plan_source_final <- reverse_candidate_source_state()
  plan_metadata_final <- setNames(
    vapply(
      reverse_metadata_inputs,
      tools::sha256sum,
      character(1L),
      USE.NAMES = FALSE
    ),
    reverse_metadata_inputs
  )
  plan_retained_final <- vapply(
    retained_reverse_inputs,
    tools::sha256sum,
    character(1L),
    USE.NAMES = FALSE
  )
  reverse_validate_compat_light(
    compat_system_evidence, "during final reverse-dependency plan validation"
  )
  if (!reverse_candidate_source_matches(plan_source_final) ||
      !identical(plan_metadata_final, reverse_metadata_sha256) ||
      !identical(unname(reverse_metadata_sha256), plan_retained_final)) {
    stop("a protected source or retained input changed during plan validation",
      call. = FALSE)
  }
  rr_validate_full_hash_ledger(full_hash_ledger_path, character())
  reverse_write_tsv(
    data.frame(
      field = rr_reverse_completion_fields(TRUE),
      value = c(
        "planned", nrow(plan),
        format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ),
      stringsAsFactors = FALSE
    ),
    file.path(metadata_directory, "completion.tsv")
  )
  invisible(rr_seal_stage(run_directory))
  reverse_release_mutation_lock()
  cat("Validated reverse-dependency plan: ", run_directory, "\n", sep = "")
  quit(save = "no", status = 0L)
}

reverse_quote <- function(value) shQuote(value, type = "sh")

reverse_write_command <- function(path, environment, command, command_arguments) {
  environment_command <- Sys.which("env")
  if (!nzchar(environment_command)) stop("could not locate env", call. = FALSE)
  assignments <- paste0(names(environment), "=", unname(environment))
  quoted <- c(
    reverse_quote(environment_command),
    "-i",
    vapply(assignments, reverse_quote, character(1L)),
    reverse_quote(command),
    vapply(command_arguments, reverse_quote, character(1L))
  )
  lines <- c("#!/usr/bin/env sh", "set -eu", paste("exec", paste(quoted, collapse = " ")))
  writeLines(lines, path, useBytes = TRUE)
  Sys.chmod(path, mode = "0700")
}

reverse_environment <- function(package_directory, consumer_library = NULL) {
  path_value <- Sys.getenv("PATH", unset = "")
  if (!nzchar(path_value)) stop("activated PATH is empty", call. = FALSE)
  user <- Sys.info()[["user"]]
  if (is.na(user) || !nzchar(user)) user <- "paradox-check"
  texmf_root <- file.path(package_directory, "texmf")
  row_cache_environment <- rr_row_cache_environment(package_directory)
  libraries <- c(consumer_library, candidate_library, dependency_library)
  libraries <- libraries[!is.na(libraries) & nzchar(libraries)]
  library_environment <- paste(libraries,
    collapse = .Platform$path.sep)
  environment <- c(
    HOME = file.path(package_directory, "home"),
    USER = user,
    LOGNAME = user,
    PATH = path_value,
    CONDA_PREFIX = Sys.getenv("CONDA_PREFIX", unset = ""),
    LD_LIBRARY_PATH = Sys.getenv("LD_LIBRARY_PATH", unset = ""),
    PKG_CONFIG_PATH = Sys.getenv("PKG_CONFIG_PATH", unset = ""),
    CMAKE_PREFIX_PATH = Sys.getenv("CMAKE_PREFIX_PATH", unset = ""),
    R_LIBS_USER = library_environment,
    R_LIBS = library_environment,
    R_LIBS_SITE = "",
    R_ENVIRON_USER = "/dev/null",
    R_PROFILE_USER = "/dev/null",
    R_MAKEVARS_USER = file.path(root, "environment", "Makevars"),
    R_BUILD_ENVIRON = "/dev/null",
    R_INSTALL_ENVIRON = "/dev/null",
    R_CHECK_ENVIRON = "/dev/null",
    R_HISTFILE = "/dev/null",
    R_TESTS = "",
    R_KEEP_PKG_SOURCE = "yes",
    NOT_CRAN = "true",
    PARADOX_REVERSE_COORDINATOR_PID = Sys.getenv(
      "PARADOX_REVERSE_COORDINATOR_PID", unset = "0"
    ),
    PARADOX_REVERSE_WORKER_TOKEN = Sys.getenv(
      "PARADOX_REVERSE_WORKER_TOKEN", unset = "reverse.cache.key.placeholder"
    ),
    `_R_CHECK_FORCE_SUGGESTS_` = "false",
    TMPDIR = file.path(package_directory, "tmp"),
    XDG_RUNTIME_DIR = file.path(package_directory, "runtime"),
    XDG_CACHE_HOME = file.path(package_directory, "cache"),
    R_USER_CACHE_DIR = file.path(package_directory, "cache", "R"),
    PYTHONDONTWRITEBYTECODE = "1",
    PYTHONPYCACHEPREFIX = file.path(package_directory, "cache", "python-bytecode"),
    RETICULATE_MINICONDA_PATH = file.path(package_directory, "cache", "reticulate", "miniconda"),
    RETICULATE_VIRTUALENV_ROOT = file.path(package_directory, "cache", "reticulate", "virtualenvs"),
    CCACHE_DIR = file.path(package_directory, "cache", "ccache"),
    CCACHE_TEMPDIR = file.path(package_directory, "tmp", "ccache"),
    PIP_CACHE_DIR = file.path(package_directory, "cache", "pip"),
    UV_CACHE_DIR = file.path(package_directory, "cache", "uv"),
    TEXMFVAR = file.path(texmf_root, "var"),
    TEXMFCONFIG = file.path(texmf_root, "config"),
    TEXMFHOME = file.path(texmf_root, "home"),
    TEXMFCACHE = file.path(texmf_root, "cache"),
    VARTEXFONTS = file.path(texmf_root, "fonts"),
    R_TEXI2DVICMD = texi2dvi_path,
    TEXI2DVI = texi2dvi_path,
    rr_consumer_locale_environment(),
    SOURCE_DATE_EPOCH = source_date_epoch,
    LD_PRELOAD = "",
    ASAN_OPTIONS = "",
    UBSAN_OPTIONS = ""
  )
  compiler_variables <- c(
    "CONDA_BUILD_SYSROOT", "HOST", "BUILD", "CONDA_TOOLCHAIN_HOST",
    "CONDA_TOOLCHAIN_BUILD", "AR", "RANLIB", "AS", "LD", "NM", "STRIP",
    "OBJCOPY", "OBJDUMP", "READELF", "CPP"
  )
  compiler_values <- Sys.getenv(compiler_variables, unset = "")
  names(compiler_values) <- compiler_variables
  nested_controls <- rr_reverse_nested_controls()
  environment[names(nested_controls)] <- unname(nested_controls)
  overlay_environment <- compat_system_child_environment()
  environment[names(overlay_environment)] <- overlay_environment
  environment[names(row_cache_environment)] <- row_cache_environment
  c(environment, compiler_values[nzchar(compiler_values)])
}

reverse_classify_failure <- function(log_text, command_status, check_status) {
  dependency_patterns <- paste(c(
    "ERROR: dependenc(y|ies) .* (is|are) not available",
    "dependencies? .* (is|are) not available for package",
    "package required but not available",
    "there is no package called",
    "dependency .* is not available",
    "package .* required by .* could not be found",
    "namespace .* is being loaded, but .* is required"
  ), collapse = "|")
  system_patterns <- paste(c(
    "configuration failed for package",
    "gdal-config",
    "cannot find -l",
    "library not found for -l",
    "fatal error: .*: No such file or directory",
    "system requirements .* not available"
  ), collapse = "|")
  if (grepl(dependency_patterns, log_text, ignore.case = TRUE, perl = TRUE)) {
    return("environmental_dependency_failure")
  }
  if (grepl(system_patterns, log_text, ignore.case = TRUE, perl = TRUE)) {
    return("environmental_system_dependency_failure")
  }
  if (!identical(command_status, 0L) && !nzchar(check_status)) {
    return("check_harness_or_early_install_failure")
  }
  "candidate_or_consumer_check_failure"
}

reverse_result_columns <- rr_reverse_result_columns()

reverse_empty_results <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(reverse_result_columns)),
      reverse_result_columns),
    stringsAsFactors = FALSE
  )
}

reverse_validate_result <- function(row, plan_row) {
  if (!is.data.frame(row) || !identical(names(row), reverse_result_columns) ||
      nrow(row) != 1L || anyNA(row) ||
      !identical(row$package[[1L]], plan_row$package[[1L]]) ||
      !identical(row$source[[1L]], plan_row$source[[1L]]) ||
      !identical(row$relation[[1L]], plan_row$relation[[1L]]) ||
      !identical(row$priority[[1L]], as.character(plan_row$priority[[1L]])) ||
      !identical(row$version[[1L]], plan_row$version[[1L]]) ||
      !identical(row$archive_sha256[[1L]], plan_row$archive_sha256[[1L]]) ||
      !identical(row$candidate_ref[[1L]], candidate_ref) ||
      !identical(row$candidate_commit[[1L]], candidate_commit) ||
      !identical(row$candidate_tree[[1L]], candidate_tree) ||
      !identical(row$candidate_content_sha256[[1L]], candidate_content) ||
      !identical(row$candidate_library_content_sha256[[1L]],
        candidate_library_initial) ||
      !identical(row$dependency_library_content_sha256[[1L]],
        dependency_library_initial) ||
      !row$status[[1L]] %in% c("passed", "failed") ||
      !row$count_coverage[[1L]] %in% c("exact", "partial", "unavailable")) {
    stop("package result does not match its authenticated plan: ",
      plan_row$package[[1L]], call. = FALSE)
  }
  count_header <- unlist(row[c("count_files_total", "count_files_parsed")],
    use.names = FALSE)
  if (any(!grepl("^(0|[1-9][0-9]*)$", count_header))) {
    stop("count file coverage contains malformed counts", call. = FALSE)
  }
  files_total <- as.integer(count_header[[1L]])
  files_parsed <- as.integer(count_header[[2L]])
  if (identical(row$count_coverage[[1L]], "unavailable")) {
    count_values <- unlist(row[c(
      "test_fail", "test_warn", "test_skip", "test_pass"
    )], use.names = FALSE)
    if (files_parsed != 0L || any(count_values != "-")) {
      stop("unavailable count coverage contains invented counts", call. = FALSE)
    }
  } else if (any(!grepl("^(0|[1-9][0-9]*)$", unlist(row[c(
      "count_files_total", "count_files_parsed", "test_fail", "test_warn",
      "test_skip", "test_pass"
    )], use.names = FALSE)))) {
    stop("available count coverage contains malformed counts", call. = FALSE)
  } else if ((identical(row$count_coverage[[1L]], "exact") &&
      (files_total == 0L || files_parsed != files_total)) ||
      (identical(row$count_coverage[[1L]], "partial") &&
        (files_parsed == 0L || files_parsed >= files_total))) {
    stop("test-count coverage label disagrees with parsed file counts",
      call. = FALSE)
  }
  invisible(row)
}

results <- vector("list", nrow(plan))
results_path <- file.path(run_directory, "results.tsv")
reverse_write_results <- function(values) {
  values <- Filter(Negate(is.null), values)
  combined <- if (length(values)) do.call(rbind, values) else reverse_empty_results()
  output <- combined
  if (nrow(output)) {
    output$error <- vapply(
      output$error, rr_compact_external_text, character(1L)
    )
  }
  rr_write_tsv(output, results_path, replace = file.exists(results_path))
  combined
}

reverse_validate_overlay_boundary <- function(context) {
  reverse_validate_compat_light(compat_system_evidence, context)
}

reverse_validate_row_boundary <- function(context) {
  state <- reverse_candidate_source_state()
  if (!reverse_candidate_source_matches(state)) {
    stop("candidate source changed ", context, call. = FALSE)
  }
  metadata <- rr_protected_metadata(candidate_library, dependency_library)
  rr_validate_protected_metadata(
    metadata,
    protected_metadata_initial, context
  )
  reverse_validate_overlay_boundary(context)
  metadata
}

reverse_install_inputs <- local({
  invariant <- NULL
  function(plan_row) {
    if (is.null(invariant)) {
      config_names <- c(
        "CC", "CFLAGS", "CPICFLAGS", "CPPFLAGS", "CXX", "CXXFLAGS",
        "CXXPICFLAGS", "LDFLAGS", "SHLIB_LD", "SHLIB_LDFLAGS"
      )
      config_values <- vapply(config_names, function(name) {
        output <- suppressWarnings(system2(
          actual_r, c("CMD", "config", name), stdout = TRUE, stderr = TRUE
        ))
        status <- as.integer(attr(output, "status") %||% 0L)
        if (!identical(status, 0L) || length(output) != 1L) {
          stop("could not authenticate R CMD config ", name, call. = FALSE)
        }
        output[[1L]]
      }, character(1L))
      config <- data.frame(
        field = paste0("R_CMD_config.", config_names),
        value = unname(config_values), stringsAsFactors = FALSE
      )
      makevars <- reverse_require_regular_provenance_file(
        Sys.getenv("R_MAKEVARS_USER"), "install Makevars"
      )
      files <- c(
        R = actual_r,
        Rscript = actual_rscript,
        R_exec = file.path(expected_r_home, "bin", "exec", "R"),
        R_Makeconf = file.path(R.home("etc"), "Makeconf"),
        R_Renviron = file.path(R.home("etc"), "Renviron"),
        Makevars = makevars,
        env_program = normalizePath("/usr/bin/env", winslash = "/", mustWork = TRUE),
        shell = normalizePath("/bin/sh", winslash = "/", mustWork = TRUE),
        toolchain_lock = file.path(root, "environment", "toolchain-linux-64.lock"),
        install_worker = install_worker_script
      )
      if (isTRUE(compat_system_evidence$active)) {
        overlay_paths <- compat_system_evidence$source_paths
        names(overlay_paths) <- paste0("compat_", seq_along(overlay_paths), "_",
          gsub("[^A-Za-z0-9]", "_", basename(overlay_paths)))
        files <- c(files, overlay_paths)
      }
      compiler_commands <- unique(c(
        vapply(config_values[c("CC", "CXX", "SHLIB_LD")], function(value) {
          strsplit(trimws(value), "[[:space:]]+", perl = TRUE)[[1L]][[1L]]
        }, character(1L)),
        Sys.getenv(c(
          "AR", "RANLIB", "LD", "AS", "NM", "STRIP", "OBJCOPY",
          "OBJDUMP", "READELF"
        ), unset = ""),
        c("make", "cmake", "pkg-config")
      ))
      compiler_commands <- compiler_commands[nzchar(compiler_commands)]
      compiler_paths <- unname(Sys.which(compiler_commands))
      if (length(compiler_paths) != length(compiler_commands) ||
          any(!nzchar(compiler_paths))) {
        stop("could not resolve every selected install tool", call. = FALSE)
      }
      compiler_paths <- vapply(compiler_paths, function(path) {
        resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
        reverse_require_regular_provenance_file(
          resolved, "resolved install compiler/build tool"
        )
      }, character(1L))
      names(compiler_paths) <- paste0("tool_", seq_along(compiler_paths), "_",
        gsub("[^A-Za-z0-9]", "_", basename(compiler_commands)))
      files <- c(files, compiler_paths)
      files <- vapply(files, reverse_require_regular_provenance_file,
        character(1L), label = "install-cache file input")
      file_rows <- do.call(rbind, lapply(seq_along(files), function(index) {
        data.frame(
          field = c(
            paste0("file.", names(files)[[index]], ".path"),
            paste0("file.", names(files)[[index]], ".sha256")
          ),
          value = c(files[[index]], unname(tools::sha256sum(files[[index]]))),
          stringsAsFactors = FALSE
        )
      }))
      install_environment <- reverse_environment(file.path(root, ".local", "tmp"))
      environment_names <- sort(names(install_environment), method = "radix")
      environment_values <- unname(install_environment[environment_names])
      row_local_names <- c(
        "HOME", "TMPDIR", "XDG_RUNTIME_DIR", "XDG_CACHE_HOME",
        "R_LIBS", "R_LIBS_USER",
        "R_USER_CACHE_DIR", "PYTHONPYCACHEPREFIX",
        "PARADOX_REVERSE_COORDINATOR_PID", "PARADOX_REVERSE_WORKER_TOKEN",
        "RETICULATE_MINICONDA_PATH", "RETICULATE_VIRTUALENV_ROOT",
        "CCACHE_DIR", "CCACHE_TEMPDIR", "PIP_CACHE_DIR", "UV_CACHE_DIR",
        "TEXMFVAR", "TEXMFCONFIG", "TEXMFHOME", "TEXMFCACHE", "VARTEXFONTS"
      )
      environment_values[environment_names %in% row_local_names] <- "<ROW_LOCAL>"
      environment_rows <- data.frame(
        field = paste0("environment.", environment_names),
        value = environment_values,
        stringsAsFactors = FALSE
      )
      processx_path <- normalizePath(
        find.package("processx"), winslash = "/", mustWork = TRUE
      )
      invariant <<- list(
        config = config, files = file_rows, environment = environment_rows,
        processx_version = as.character(utils::packageVersion("processx")),
        processx_content = rr_tree_content_sha256(processx_path)
      )
    }
    base <- data.frame(
      field = c(
        "schema", "package", "version", "archive_sha256",
        "candidate_content_sha256", "dependency_library_content_sha256",
        "processx_version", "processx_content_sha256", "R_version", "R_platform",
        "sysname", "release", "machine", "install_executable",
        "install_arguments"
      ),
      value = c(
        "1", plan_row$package[[1L]], plan_row$version[[1L]],
        plan_row$archive_sha256[[1L]], candidate_content,
        dependency_library_initial, invariant$processx_version,
        invariant$processx_content,
        as.character(getRversion()), R.version$platform,
        Sys.info()[["sysname"]], Sys.info()[["release"]],
        Sys.info()[["machine"]], actual_r,
        "CMD INSTALL --preclean --clean --no-multiarch --library=TARGET ARCHIVE"
      ), stringsAsFactors = FALSE
    )
    value <- rbind(base, invariant$config, invariant$environment, invariant$files)
    rownames(value) <- NULL
    value
  }
})

install_cache_parent <- file.path(root, ".local", "compat", "reverse-cache")
if (!dir.exists(install_cache_parent) &&
    !dir.create(install_cache_parent, recursive = FALSE, showWarnings = FALSE)) {
  stop("could not create reverse install-cache parent", call. = FALSE)
}
install_cache_root <- file.path(install_cache_parent, "install")
install_cache_quarantine <- file.path(install_cache_parent, "quarantine")
for (directory in c(install_cache_root, install_cache_quarantine)) {
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = FALSE, showWarnings = FALSE)) {
    stop("could not create reverse install-cache directory", call. = FALSE)
  }
  reverse_require_plain_local_directory(directory, "reverse install-cache directory")
}

reverse_install_or_reuse <- function(plan_row, package_directory) {
  inputs <- reverse_install_inputs(plan_row)
  key <- rr_install_cache_key(inputs)
  target <- file.path(install_cache_root, key)
  validate <- function() rr_validate_install_cache(
    target, key, inputs, plan_row$package[[1L]], plan_row$version[[1L]],
    rr_tree_content_sha256
  )
  if (file.exists(target) || dir.exists(target) || rr_is_symbolic(target)) {
    verified <- if (dir.exists(target) && !rr_is_symbolic(target)) {
      tryCatch(validate(), error = identity)
    } else {
      simpleError("install-cache target is not a plain directory")
    }
    if (!inherits(verified, "error")) {
      verified$reused <- TRUE
      verified$key <- key
      verified$elapsed <- 0
      verified$preflight_verified <- TRUE
      return(verified)
    }
    quarantine <- file.path(
      install_cache_quarantine,
      paste0(key, "-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
        "-", Sys.getpid())
    )
    if (!suppressWarnings(file.rename(target, quarantine))) {
      competing <- if (dir.exists(target) && !rr_is_symbolic(target)) {
        tryCatch(validate(), error = identity)
      } else NULL
      if (!is.null(competing) && !inherits(competing, "error")) {
        competing$reused <- TRUE
        competing$key <- key
        competing$elapsed <- 0
        competing$preflight_verified <- TRUE
        return(competing)
      }
      if (file.exists(target) || dir.exists(target) || rr_is_symbolic(target)) {
        stop("could not quarantine invalid install cache: ",
          conditionMessage(verified), call. = FALSE)
      }
    }
  }
  staging <- paste0(target, ".new-", Sys.getpid())
  if (file.exists(staging) || dir.exists(staging) || rr_is_symbolic(staging)) {
    stop("install-cache staging path exists: ", staging, call. = FALSE)
  }
  on.exit({
    if (file.exists(staging) || dir.exists(staging) || rr_is_symbolic(staging)) {
      unlink(staging, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  dir.create(file.path(staging, "metadata"), recursive = TRUE)
  library <- file.path(staging, "library")
  dir.create(library)
  environment <- reverse_environment(package_directory)
  install_arguments <- c(
    "CMD", "INSTALL", "--preclean", "--clean", "--no-multiarch",
    paste0("--library=", library), plan_row$archive[[1L]]
  )
  install_log <- file.path(staging, "install.log")
  rr_record_command(
    file.path(staging, "metadata", "command.tsv"), actual_r,
    install_arguments, environment, install_timeout
  )
  started <- Sys.time()
  result <- reverse_install_worker(
    actual_r, install_arguments, environment, package_directory,
    install_timeout, install_log
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  installed <- file.path(library, plan_row$package[[1L]])
  log_excerpt <- if (file.exists(install_log)) {
    rr_log_excerpt(install_log)
  } else list(oversized = FALSE, lines = character())
  log_lines <- log_excerpt$lines
  ok <- identical(result$status, 0L) && !isTRUE(result$timeout) &&
    !isTRUE(log_excerpt$oversized) &&
    dir.exists(installed) && any(grepl(
      paste0("^\\* DONE \\(", plan_row$package[[1L]], "\\)$"), log_lines
    ))
  if (!ok) {
    if (file.exists(install_log)) file.copy(
      install_log, file.path(package_directory, "install.log"),
      copy.mode = TRUE, copy.date = TRUE
    )
    command_source <- file.path(staging, "metadata", "command.tsv")
    command_destination <- file.path(package_directory, "install-command.tsv")
    if (!file.copy(
        command_source, command_destination,
        copy.mode = TRUE, copy.date = TRUE
      ) || !identical(rr_sha256(command_source), rr_sha256(command_destination))) {
      stop("could not retain failed install command evidence", call. = FALSE)
    }
    unlink(staging, recursive = TRUE, force = TRUE)
    return(list(
      ok = FALSE, key = key, reused = FALSE, elapsed = elapsed,
      result = result, log = file.path(package_directory, "install.log"),
      pathological_output = isTRUE(log_excerpt$oversized)
    ))
  }
  installed_content <- rr_tree_content_sha256(installed)
  rr_write_tsv(inputs, file.path(staging, "metadata", "cache-inputs.tsv"))
  rr_write_tsv(data.frame(
    field = c(
      "status", "cache_key", "package", "version",
      "installed_content_sha256", "finished_utc"
    ),
    value = c(
      "installed", key, plan_row$package[[1L]], plan_row$version[[1L]],
      installed_content,
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ), stringsAsFactors = FALSE
  ), file.path(staging, "metadata", "completion.tsv"))
  invisible(rr_seal_stage(staging))
  promotion <- rr_promote_install_cache(staging, target, validate)
  verified <- promotion$validation
  verified$reused <- !isTRUE(promotion$promoted)
  verified$key <- key
  verified$elapsed <- elapsed
  verified$ok <- TRUE
  verified$preflight_verified <- TRUE
  verified
}

reverse_row <- function(plan_row, package_directory, protected_before) {
  package <- plan_row$package[[1L]]
  cache <- reverse_install_or_reuse(plan_row, package_directory)
  empty_counts <- list(
    coverage = "unavailable", files_total = 0L, files_parsed = 0L,
    fail = NA_integer_, warn = NA_integer_, skip = NA_integer_, pass = NA_integer_,
    files = data.frame()
  )
  counts <- empty_counts
  check_status <- ""
  classification <- "install_failure"
  status <- "failed"
  check_elapsed <- 0
  check_result <- cache$result %||% list(status = 1L, timeout = FALSE)
  check_log <- cache$log %||% file.path(package_directory, "install.log")
  cache_manifest <- "-"
  cache_seal <- "-"
  installed_content <- "-"
  if (identical(cache$ok, FALSE)) {
    install_excerpt <- if (file.exists(check_log)) {
      rr_log_excerpt(check_log)
    } else list(oversized = FALSE, text = "")
    classification <- if (isTRUE(cache$pathological_output) ||
        isTRUE(install_excerpt$oversized)) {
      "pathological_output_size"
    } else if (isTRUE(check_result$timeout)) {
      "install_timeout"
    } else {
      reverse_classify_failure(
        install_excerpt$text, check_result$status, ""
      )
    }
  }
  if (isTRUE(cache$ok %||% TRUE)) {
    if (!isTRUE(cache$preflight_verified)) {
      stop("install cache was not authenticated immediately before its child",
        call. = FALSE)
    }
    cache_manifest <- cache$manifest_sha256
    cache_seal <- cache$seal_sha256
    installed_content <- cache$installed_content_sha256
    cache_receipts <- c(
      cache_manifest = file.path(cache$target, rr_stage_manifest_relative),
      cache_seal = file.path(cache$target, rr_stage_seal_relative),
      cache_inputs = file.path(cache$target, "metadata", "cache-inputs.tsv"),
      cache_completion = file.path(cache$target, "metadata", "completion.tsv")
    )
    for (receipt_name in names(cache_receipts)) {
      source <- reverse_require_regular_provenance_file(
        cache_receipts[[receipt_name]], "authenticated install-cache receipt"
      )
      destination <- file.path(
        package_directory, "metadata", paste0(receipt_name, ".tsv")
      )
      if (!file.copy(source, destination, copy.mode = TRUE, copy.date = TRUE) ||
          !identical(unname(tools::sha256sum(source)),
            unname(tools::sha256sum(destination)))) {
        stop("could not retain install-cache receipt for ", package,
          call. = FALSE)
      }
    }
    retained_install_log <- file.path(package_directory, "install.log")
    if (!file.copy(cache$install_log, retained_install_log,
        copy.mode = TRUE, copy.date = TRUE) ||
        !identical(unname(tools::sha256sum(cache$install_log)),
          unname(tools::sha256sum(retained_install_log)))) {
      stop("could not retain authenticated install log for ", package,
        call. = FALSE)
    }
    # Keep installation and the serialized outer worker uniformly bounded.
    # Only the authenticated mlr3 check child receives its scheduler-backed
    # two-CPU worker-contract projection.
    child_environment <- rr_reverse_check_environment(
      reverse_environment(package_directory, cache$library), package
    )
    preflight_script <- file.path(package_directory, "preflight.R")
    writeLines(c(
      "args <- commandArgs(TRUE)",
      "candidate <- normalizePath(args[[1L]], winslash = '/', mustWork = TRUE)",
      "expected_python_cache <- normalizePath(args[[2L]], winslash = '/', mustWork = FALSE)",
      "resolved <- normalizePath(find.package('paradox'), winslash = '/', mustWork = TRUE)",
      "if (!identical(resolved, candidate)) stop('wrong paradox package', call. = FALSE)",
      "if (!identical(Sys.getenv('NOT_CRAN'), 'true')) stop('NOT_CRAN is not true', call. = FALSE)",
      "expected_locale <- c(LC_ALL = 'C.UTF-8', LANG = 'C.UTF-8', LANGUAGE = 'C', TZ = 'UTC')",
      "if (!identical(Sys.getenv(names(expected_locale)), expected_locale)) stop('consumer locale is not deterministic', call. = FALSE)",
      "if (!identical(Sys.getenv('PYTHONDONTWRITEBYTECODE'), '1')) stop('Python bytecode is enabled', call. = FALSE)",
      "if (!identical(Sys.getenv('PYTHONPYCACHEPREFIX'), expected_python_cache)) stop('Python cache escaped row', call. = FALSE)"
    ), preflight_script, useBytes = TRUE)
    preflight_log <- file.path(package_directory, "preflight.log")
    preflight <- rr_run(
    actual_rscript,
      c("--vanilla", preflight_script, candidate_package,
        file.path(package_directory, "cache", "python-bytecode")),
      child_environment, package_directory, 120L, preflight_log
    )
    check_log <- file.path(package_directory, "check.log")
    work_directory <- file.path(package_directory, "work")
    dir.create(work_directory)
    check_arguments <- c(
      "CMD", "check", paste0("--output=", work_directory),
      paste0("--library=", cache$library),
      paste0("--install=check:", cache$install_log),
      "--no-manual", "--no-multiarch", "--no-stop-on-test-error",
      plan_row$archive[[1L]]
    )
    rr_record_command(
      file.path(package_directory, "command.tsv"), actual_r,
      check_arguments, child_environment, check_timeout
    )
    if (identical(preflight$status, 0L) && !isTRUE(preflight$timeout)) {
      started <- Sys.time()
      check_result <- rr_run(
    actual_r,
        check_arguments, child_environment, package_directory,
        check_timeout, check_log
      )
      check_elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    } else {
      file.copy(preflight_log, check_log, copy.mode = TRUE, copy.date = TRUE)
      check_result <- preflight
      classification <- "preflight_failure"
    }
    check_directory <- file.path(work_directory, paste0(package, ".Rcheck"))
    check_status <- rr_check_status(check_directory)
    counts <- rr_parse_test_counts(check_directory)
    pathological_output <- !is.null(attr(
      check_status, "pathological_output", exact = TRUE
    )) || length(counts$pathological_outputs) > 0L
    if (nrow(counts$files)) {
      counts$files$file <- substring(
        counts$files$file, nchar(package_directory) + 2L
      )
    }
    rr_write_tsv(
      counts$files,
      file.path(package_directory, "test-counts.tsv")
    )
    check_excerpt <- if (file.exists(check_log)) {
      rr_log_excerpt(check_log)
    } else list(oversized = FALSE, text = "", lines = character())
    pathological_output <- pathological_output || isTRUE(check_excerpt$oversized)
    counted_failures <- !is.na(counts$fail) && counts$fail > 0L
    check_failed <- !identical(check_result$status, 0L) ||
      isTRUE(check_result$timeout) || !nzchar(check_status) ||
      grepl("ERROR|WARNING", check_status) || counted_failures ||
      pathological_output
    if (!identical(classification, "preflight_failure")) {
      classification <- if (pathological_output) {
        "pathological_output_size"
      } else if (isTRUE(check_result$timeout)) {
        "check_timeout"
      } else if (check_failed) {
        reverse_classify_failure(
          check_excerpt$text, check_result$status, check_status
        )
      } else if (grepl("NOTE", check_status, fixed = TRUE)) {
        "passed_with_notes"
      } else if (!is.na(counts$warn) && counts$warn > 0L) {
        "passed_with_test_warnings"
      } else {
        "passed"
      }
    }
    status <- if (check_failed) "failed" else "passed"
    # A consumer that wrote into its cached installation invalidates the row.
    rr_validate_install_cache(
      cache$target, cache$key, reverse_install_inputs(plan_row), package,
      plan_row$version[[1L]], rr_tree_content_sha256
    )
  }
  log_lines <- if (file.exists(check_log)) {
    excerpt <- rr_log_excerpt(check_log)
    if (excerpt$oversized) character() else excerpt$lines
  } else character()
  # The parent replaces this provisional value with the single authenticated
  # post-wave boundary.  Workers never inspect or mutate shared provenance.
  protected_metadata_after <- protected_before
  count_value <- function(value) {
    if (is.na(value)) "-" else as.character(value)
  }
  row <- as.data.frame(as.list(setNames(c(
    package, plan_row$source[[1L]], plan_row$relation[[1L]],
    as.character(plan_row$priority[[1L]]), plan_row$version[[1L]],
    plan_row$archive_name[[1L]], plan_row$archive_sha256[[1L]],
    candidate_ref, candidate_commit, candidate_tree, candidate_version,
    candidate_content, candidate_library_initial, dependency_library_initial,
    protected_before, protected_metadata_after, cache$key,
    if (isTRUE(cache$reused)) "true" else "false", cache_manifest, cache_seal,
    installed_content, as.character(cache$elapsed), as.character(check_elapsed),
    as.character(install_timeout), as.character(check_timeout),
    as.character(check_result$status),
    if (isTRUE(check_result$timeout)) "true" else "false",
    if (nzchar(check_status)) check_status else "-", status, classification,
    counts$coverage, as.character(counts$files_total),
    as.character(counts$files_parsed), count_value(counts$fail),
    count_value(counts$warn), count_value(counts$skip), count_value(counts$pass),
    "true", substring(check_log, nchar(run_directory) + 2L),
    if (identical(status, "failed")) {
      rr_compact_external_text(paste(tail(log_lines, 100L), collapse = "\n"))
    } else "-"
  ), reverse_result_columns)), stringsAsFactors = FALSE)
  row[] <- lapply(row, as.character)
  reverse_validate_result(row, plan_row)
  row
}

reverse_create_row_directory <- function(index) {
  package <- plan$package[[index]]
  package_directory <- file.path(run_directory, "packages", package)
  if (file.exists(package_directory) || dir.exists(package_directory) ||
      rr_is_symbolic(package_directory)) {
    stop("package evidence path was already reserved: ", package,
      call. = FALSE)
  }
  if (!dir.create(file.path(package_directory, "metadata"), recursive = TRUE,
      showWarnings = FALSE)) {
    stop("could not create package evidence directory: ", package,
      call. = FALSE)
  }
  for (relative in c(
      "home", "tmp", "tmp/ccache", "runtime", "cache", "cache/R",
      "cache/python-bytecode", "cache/reticulate", "cache/ccache",
      "cache/pip", "cache/uv", "texmf/var", "texmf/config", "texmf/home",
      "texmf/cache", "texmf/fonts", "worker-state"
    )) {
    dir.create(file.path(package_directory, relative), recursive = TRUE,
      showWarnings = FALSE)
  }
  Sys.chmod(file.path(package_directory, "runtime"), mode = "0700")
  package_directory
}

reverse_read_worker_task <- function(index, package_directory) {
  receipt_path <- file.path(package_directory, "metadata", "worker-task.tsv")
  receipt <- rr_read_reverse_worker_task(receipt_path)
  package <- plan$package[[index]]
  report_relative <- receipt$resource_report[[1L]]
  expected_report_prefix <- file.path("metadata", "resource-jobs-waves")
  report_path <- file.path(run_directory, report_relative)
  report <- rr_read_tsv(report_path, c("field", "value"))
  report_validation <- rr_validate_resource_report(report, "consumer")
  expected_task <- list(
    index = index, plan_row = plan[index, , drop = FALSE],
    package_directory = package_directory,
    protected_before = protected_metadata_initial$combined
  )
  operator_limit <- receipt$operator_limit[[1L]]
  worker_limit <- as.integer(receipt$worker_limit[[1L]])
  if (!identical(receipt$plan_index[[1L]], as.character(index)) ||
      !identical(receipt$package[[1L]], package) ||
      !identical(dirname(report_relative), expected_report_prefix) ||
      !grepl("^attempt-[0-9]{6}\\.tsv$", basename(report_relative)) ||
      !identical(rr_sha256(report_path),
        receipt$resource_report_sha256[[1L]]) ||
      !identical(report_validation$jobs,
        as.integer(receipt$automatic_ceiling[[1L]])) ||
      worker_limit > report_validation$jobs ||
      worker_limit > scheduler_initial_jobs ||
      (!identical(operator_limit, "-") &&
        worker_limit > as.integer(operator_limit)) ||
      as.integer(receipt$position[[1L]]) > worker_limit ||
      !identical(receipt$protected_metadata_before[[1L]],
        protected_metadata_initial$combined) ||
      !identical(receipt$task_sha256[[1L]],
        rr_object_sha256(expected_task)) ||
      !identical(receipt$worker_script_sha256[[1L]],
        unname(reverse_metadata_sha256[[wave_worker_script]])) ||
      !identical(receipt$worker_group_sha256[[1L]],
        unname(reverse_metadata_sha256[[worker_group_script]]))) {
    stop("external worker task receipt disagrees with its row: ", package,
      call. = FALSE)
  }
  receipt
}

reverse_read_worker_result <- function(index, package_directory, receipt,
                                       required = TRUE) {
  launch <- file.path(package_directory, "metadata", "worker-launch.tsv")
  path <- file.path(package_directory, "metadata", "worker-result.rds")
  completion <- file.path(
    package_directory, "metadata", "worker-completion.tsv"
  )
  result_exists <- file.exists(path) || dir.exists(path) || rr_is_symbolic(path)
  completion_exists <- file.exists(completion) || dir.exists(completion) ||
    rr_is_symbolic(completion)
  launch_exists <- file.exists(launch) || dir.exists(launch) ||
    rr_is_symbolic(launch)
  if ((launch_exists && (dir.exists(launch) || rr_is_symbolic(launch))) ||
      (result_exists && (dir.exists(path) || rr_is_symbolic(path))) ||
      (completion_exists &&
        (dir.exists(completion) || rr_is_symbolic(completion)))) {
    stop("external worker transport output is not plain for ",
      plan$package[[index]], call. = FALSE)
  }
  if (!completion_exists) {
    if (required) {
      stop("durable external worker clean-exit marker is absent for ",
        plan$package[[index]], call. = FALSE)
    }
    return(NULL)
  }
  if (!launch_exists) {
    stop("clean external worker result lacks its launch marker for ",
      plan$package[[index]], call. = FALSE)
  }
  if (!result_exists) {
    stop("clean external worker marker lacks its result for ",
      plan$package[[index]], call. = FALSE)
  }
  rr_read_worker_result(
    path, as.integer(receipt$position[[1L]]), receipt$task_sha256[[1L]],
    completion
  )
}

reverse_read_worker_launch <- function(index, package_directory, receipt,
                                       required = TRUE) {
  path <- file.path(package_directory, "metadata", "worker-launch.tsv")
  exists <- file.exists(path) || dir.exists(path) || rr_is_symbolic(path)
  if (!exists) {
    if (required) {
      stop("external worker launch marker is absent for ",
        plan$package[[index]], call. = FALSE)
    }
    return(NULL)
  }
  rr_read_worker_launch(
    path, as.integer(receipt$position[[1L]]), receipt$task_sha256[[1L]]
  )
}

reverse_finalize_durable_worker <- function(index, wave_number, worker_result,
                                            protected_after) {
  finalization_started <- proc.time()[["elapsed"]]
  finalization_deadline <- min(600, max(120, worker_timeout / 10))
  check_finalization_deadline <- function(context) {
    if (proc.time()[["elapsed"]] - finalization_started > finalization_deadline) {
      stop("bounded row finalization deadline exceeded while ", context,
        call. = FALSE)
    }
  }
  package <- plan$package[[index]]
  package_directory <- file.path(run_directory, "packages", package)
  receipt <- reverse_read_worker_task(index, package_directory)
  invisible(reverse_read_worker_launch(index, package_directory, receipt))
  durable <- reverse_read_worker_result(index, package_directory, receipt)
  check_finalization_deadline("authenticating durable worker output")
  if (!identical(durable, worker_result) || !isTRUE(durable$ok)) {
    stop("successful worker result changed before promotion: ", package,
      call. = FALSE)
  }
  rr_require_file(
    file.path(package_directory, "worker-transport.log"),
    "external worker transport log"
  )
  row <- durable$value
  if (!is.data.frame(row) || nrow(row) != 1L) {
    stop("external worker returned a malformed reverse result: ", package,
      call. = FALSE)
  }
  row$protected_metadata_after[[1L]] <- protected_after
  reverse_validate_result(row, plan[index, , drop = FALSE])
  check_finalization_deadline("validating the result")
  result_path <- file.path(package_directory, "result.tsv")
  if (file.exists(result_path)) {
    retained <- rr_read_tsv(result_path, reverse_result_columns)
    if (!identical(retained, row)) {
      stop("partially promoted row result changed on resume: ", package,
        call. = FALSE)
    }
  } else {
    reverse_write_tsv(row, result_path)
  }
  completion_path <- file.path(package_directory, "metadata", "completion.tsv")
  stable_completion <- c(
    status = row$status[[1L]], classification = row$classification[[1L]],
    plan_index = as.character(index), wave = as.character(wave_number),
    worker_task_sha256 = rr_sha256(file.path(
      package_directory, "metadata", "worker-task.tsv"
    )),
    worker_launch_sha256 = rr_sha256(file.path(
      package_directory, "metadata", "worker-launch.tsv"
    )),
    worker_result_sha256 = rr_sha256(file.path(
      package_directory, "metadata", "worker-result.rds"
    )),
    worker_completion_sha256 = rr_sha256(file.path(
      package_directory, "metadata", "worker-completion.tsv"
    ))
  )
  if (file.exists(completion_path)) {
    completion <- rr_read_tsv(completion_path, c("field", "value"))
    if (!identical(completion$field, c(
        names(stable_completion), "finished_utc"
      )) || !identical(
        setNames(completion$value, completion$field)[names(stable_completion)],
        stable_completion
      )) {
      stop("partially promoted row completion changed on resume: ", package,
        call. = FALSE)
    }
  } else {
    reverse_write_tsv(data.frame(
      field = c(names(stable_completion), "finished_utc"),
      value = c(stable_completion,
        format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
      stringsAsFactors = FALSE
    ), completion_path)
  }
  check_finalization_deadline("publishing row metadata")
  for (disposable in c(
      "home", "tmp", "runtime", "cache", "texmf", "worker-state"
    )) {
    path <- file.path(package_directory, disposable)
    if (dir.exists(path) && unlink(path, recursive = TRUE, force = TRUE) != 0L) {
      stop("could not prune disposable resumed row state for ", package,
        call. = FALSE)
    }
    check_finalization_deadline("pruning disposable row state")
  }
  if (!file.exists(file.path(package_directory, rr_stage_seal_relative))) {
    invisible(rr_seal_stage(package_directory))
  } else {
    invisible(rr_verify_stage(package_directory))
  }
  check_finalization_deadline("sealing the row")
  invisible(row)
}

reverse_wave_members <- function(wave_row, field) {
  value <- wave_row[[field]][[1L]]
  if (identical(value, "-")) integer() else {
    as.integer(strsplit(value, ",", fixed = TRUE)[[1L]])
  }
}

reverse_recover_external_workers <- function(protected_after) {
  candidates <- list()
  for (index in seq_len(nrow(plan))) {
    package_directory <- file.path(
      run_directory, "packages", plan$package[[index]]
    )
    task_path <- file.path(package_directory, "metadata", "worker-task.tsv")
    if (dir.exists(package_directory) &&
        !file.exists(file.path(package_directory, rr_stage_seal_relative)) &&
        file.exists(task_path)) {
      candidates[[as.character(index)]] <- list(
        index = index, directory = package_directory,
        receipt = reverse_read_worker_task(index, package_directory)
      )
    }
  }
  if (!length(candidates)) return(invisible(FALSE))
  wave_ids <- vapply(candidates, function(value) {
    as.integer(value$receipt$wave[[1L]])
  }, integer(1L))
  waves <- rr_validate_reverse_waves(waves_path, plan$package)
  for (wave_number in sort(unique(wave_ids))) {
    members <- candidates[wave_ids == wave_number]
    positions <- vapply(members, function(value) {
      as.integer(value$receipt$position[[1L]])
    }, integer(1L))
    members <- members[order(positions)]
    positions <- sort(positions)
    indices <- vapply(members, `[[`, integer(1L), "index")
    receipts <- lapply(members, `[[`, "receipt")
    if (wave_number <= nrow(waves)) {
      wave <- waves[wave_number, , drop = FALSE]
      planned <- reverse_wave_members(wave, "plan_indices")
      errors <- reverse_wave_members(wave, "worker_error_indices")
      missing <- reverse_wave_members(wave, "worker_missing_indices")
      unstarted <- reverse_wave_members(wave, "worker_unstarted_indices")
      for (position in seq_along(members)) {
        index <- indices[[position]]
        receipt <- receipts[[position]]
        planned_position <- match(index, planned)
        if (is.na(planned_position) ||
            !identical(as.integer(receipt$position[[1L]]), planned_position) ||
            !identical(receipt$worker_limit[[1L]], wave$worker_limit[[1L]]) ||
            !identical(receipt$automatic_ceiling[[1L]],
              wave$automatic_ceiling[[1L]]) ||
            !identical(receipt$operator_limit[[1L]],
              wave$operator_limit[[1L]]) ||
            !identical(receipt$resource_report[[1L]],
              wave$resource_report[[1L]]) ||
            !identical(receipt$resource_report_sha256[[1L]],
              wave$resource_report_sha256[[1L]]) ||
            !identical(receipt$protected_metadata_before[[1L]],
              wave$protected_metadata_before[[1L]]) ||
            !identical(protected_after, wave$protected_metadata_after[[1L]])) {
          stop("interrupted worker receipt disagrees with retained wave",
            call. = FALSE)
        }
        if (index %in% c(errors, missing, unstarted)) {
          rr_quarantine_unsealed_row(
            members[[position]]$directory,
            file.path(run_directory, "interrupted"), plan$package[[index]]
          )
        } else {
          # Wave classification is authoritative for failed/unstarted members.
          # Parse only a row that the authenticated wave claims succeeded, so
          # malformed attacker-controlled failure payloads cannot block safe
          # quarantine and resume.
          result <- reverse_read_worker_result(
            index, members[[position]]$directory, receipt, required = TRUE
          )
          if (!isTRUE(result$ok)) {
            stop("retained successful wave lost its durable worker result: ",
              plan$package[[index]], call. = FALSE)
          }
          reverse_finalize_durable_worker(
            index, wave_number, result, protected_after
          )
        }
      }
      next
    }
    if (wave_number != nrow(waves) + 1L) {
      stop("interrupted worker refers to a noncontiguous wave", call. = FALSE)
    }
    results <- lapply(seq_along(members), function(position) {
      tryCatch(
        reverse_read_worker_result(
          indices[[position]], members[[position]]$directory,
          receipts[[position]], required = FALSE
        ),
        error = function(condition) list(
          ok = FALSE, value = NULL,
          error = paste0("recovery rejected worker result: ",
            conditionMessage(condition))
        )
      )
    })
    launches <- lapply(seq_along(members), function(position) {
      reverse_read_worker_launch(
        indices[[position]], members[[position]]$directory,
        receipts[[position]], required = FALSE
      )
    })
    if (all(vapply(launches, is.null, logical(1L)))) {
      for (member in members) rr_quarantine_unsealed_row(
        member$directory, file.path(run_directory, "interrupted"),
        plan$package[[member$index]]
      )
      next
    }
    if (any(!vapply(results, is.null, logical(1L)) &
        vapply(launches, is.null, logical(1L)))) {
      stop("clean external result lacks its durable launch marker",
        call. = FALSE)
    }
    first <- receipts[[1L]]
    worker_limit <- as.integer(first$worker_limit[[1L]])
    same <- function(field) all(vapply(receipts, function(receipt) {
      identical(receipt[[field]][[1L]], first[[field]][[1L]])
    }, logical(1L)))
    if (length(members) != worker_limit ||
        !identical(positions, seq_len(worker_limit)) ||
        anyDuplicated(indices) || any(!vapply(c(
          "wave", "worker_limit", "automatic_ceiling", "operator_limit",
          "resource_report", "resource_report_sha256",
          "protected_metadata_before", "started_utc", "worker_script_sha256",
          "worker_group_sha256"
        ), same, logical(1L)))) {
      stop("interrupted external wave has an incomplete task inventory",
        call. = FALSE)
    }
    errors <- indices[!vapply(results, is.null, logical(1L)) &
      !vapply(results, function(value) isTRUE(value$ok), logical(1L))]
    unstarted <- indices[vapply(launches, is.null, logical(1L))]
    missing <- indices[vapply(results, is.null, logical(1L)) &
      !vapply(launches, is.null, logical(1L))]
    rr_append_reverse_wave(
      waves_path, indices, plan$package, worker_limit, "external", errors,
      missing, unstarted, as.integer(first$automatic_ceiling[[1L]]),
      if (identical(first$operator_limit[[1L]], "-")) NULL else {
        as.integer(first$operator_limit[[1L]])
      }, first$resource_report[[1L]], first$resource_report_sha256[[1L]],
      first$protected_metadata_before[[1L]], protected_after,
      first$started_utc[[1L]],
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    waves <- rr_validate_reverse_waves(waves_path, plan$package)
    for (position in seq_along(members)) {
      index <- indices[[position]]
      if (index %in% c(errors, missing, unstarted)) {
        rr_quarantine_unsealed_row(
          members[[position]]$directory,
          file.path(run_directory, "interrupted"), plan$package[[index]]
        )
      } else {
        reverse_finalize_durable_worker(
          index, wave_number, results[[position]], protected_after
        )
      }
    }
  }
  invisible(TRUE)
}

accepted <- rr_validate_reverse_acceptance(
  acceptance_path, plan$package, file.path(run_directory, "packages")
)
accepted_packages <- accepted$package
resume_boundary <- if (arguments$resume) {
  reverse_validate_row_boundary("before accepting resumed rows")
} else NULL
if (arguments$resume) {
  invisible(reverse_recover_external_workers(resume_boundary$combined))
}
for (index in seq_len(nrow(plan))) {
  plan_row <- plan[index, , drop = FALSE]
  package <- plan_row$package[[1L]]
  package_directory <- file.path(run_directory, "packages", package)
  exists <- file.exists(package_directory) || dir.exists(package_directory) ||
    rr_is_symbolic(package_directory)
  if (!exists) next
  if (!dir.exists(package_directory) || rr_is_symbolic(package_directory)) {
    stop("package evidence path is not a plain directory: ", package,
      call. = FALSE)
  }
  if (file.exists(file.path(package_directory, rr_stage_seal_relative))) {
    row_evidence <- rr_verify_stage(package_directory)
    row <- rr_read_tsv(
      file.path(package_directory, "result.tsv"), reverse_result_columns
    )
    reverse_validate_result(row, plan_row)
    if (!package %in% accepted_packages) {
      if (!arguments$resume) {
        stop("sealed row is absent from the acceptance ledger: ", package,
          call. = FALSE)
      }
      completion <- reverse_read_tsv(
        file.path(package_directory, "metadata", "completion.tsv"),
        c("field", "value")
      )
      completion <- setNames(completion$value, completion$field)
      if (!identical(completion[["plan_index"]], as.character(index)) ||
          is.null(completion[["wave"]]) ||
          !grepl("^[1-9][0-9]*$", completion[["wave"]])) {
        stop("sealed orphan row lacks its deterministic wave identity: ", package,
          call. = FALSE)
      }
      rr_append_reverse_acceptance(
        acceptance_path, index, package, as.integer(completion[["wave"]]),
        row, row_evidence
      )
      accepted_packages <- c(accepted_packages, package)
    }
    results[[index]] <- row
    message(package, ": resumed sealed row (", row$classification[[1L]], ")")
    next
  }
  if (!arguments$resume) {
    stop("unsealed package row exists outside --resume: ", package,
      call. = FALSE)
  }
  rr_quarantine_unsealed_row(
    package_directory, file.path(run_directory, "interrupted"), package
  )
}
if (arguments$resume) {
  invisible(reverse_validate_row_boundary("after accepting resumed rows"))
  accepted <- rr_validate_reverse_acceptance(
    acceptance_path, plan$package, file.path(run_directory, "packages")
  )
  invisible(rr_validate_reverse_acceptance_waves(
    accepted, rr_validate_reverse_waves(waves_path, plan$package)
  ))
  reverse_write_results(results)
}

pending_indices <- which(vapply(results, is.null, logical(1L)))
if (length(pending_indices)) {
  pending_keys <- vapply(pending_indices, function(index) {
    rr_install_cache_key(reverse_install_inputs(plan[index, , drop = FALSE]))
  }, character(1L))
  if (anyDuplicated(pending_keys)) {
    stop("independent reverse rows unexpectedly share an install-cache key",
      call. = FALSE)
  }
}
pending_queue <- pending_indices
while (length(pending_queue)) {
  wave_number <- nrow(rr_read_reverse_waves(waves_path)) + 1L
  wave_started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  protected_before <- reverse_validate_row_boundary(
    paste0("before reverse wave ", wave_number)
  )
  provisional_indices <- head(
    pending_queue, rr_reverse_wave_capacity(scheduler_initial_jobs)
  )
  for (index in provisional_indices) {
    reverse_verify_plan_archive(
      index, paste("before wave checking", plan$package[[index]])
    )
  }
  # Recompute the cgroup/affinity/memory ceiling immediately before launching
  # every wave.  A lowering operator override is revalidated against this live
  # ceiling rather than trusting the stage-start observation.
  wave_resource <- reverse_resource_report()
  if (!is.null(operator_jobs) && operator_jobs > wave_resource$jobs) {
    stop(
      "PARADOX_REVERSE_JOBS exceeds the automatic ceiling immediately before ",
      "wave ", wave_number, " (", wave_resource$jobs, ")", call. = FALSE
    )
  }
  wave_jobs <- min(
    scheduler_initial_jobs, wave_resource$jobs,
    operator_jobs %||% scheduler_initial_jobs,
    length(pending_queue)
  )
  if (!identical(.Platform$OS.type, "unix")) wave_jobs <- min(wave_jobs, 1L)
  if (wave_jobs < 1L) {
    stop("resource-aware scheduler selected no workers for wave ", wave_number,
      call. = FALSE)
  }
  # The wave holds up to three rows per admitted worker; rr_parallel_wave
  # keeps only wave_jobs running concurrently and refills a freed slot from
  # the same wave, so a slow row no longer idles its siblings' slots.
  wave_indices <- head(pending_queue, rr_reverse_wave_capacity(wave_jobs))
  pending_queue <- tail(pending_queue, -length(wave_indices))
  report_entries <- list.files(
    scheduler_reports_directory, all.files = TRUE, full.names = TRUE,
    no.. = TRUE
  )
  if (length(report_entries) &&
      (any(file.info(report_entries, extra_cols = FALSE)$isdir) ||
        any(vapply(report_entries, rr_is_symbolic, logical(1L))) ||
        any(!grepl("^attempt-[0-9]{6}\\.tsv$", basename(report_entries))))) {
    stop("per-wave resource report inventory is unsafe", call. = FALSE)
  }
  report_path <- file.path(
    scheduler_reports_directory,
    sprintf("attempt-%06d.tsv", length(report_entries) + 1L)
  )
  reverse_write_tsv(wave_resource$report, report_path)
  report_relative <- substring(report_path, nchar(run_directory) + 2L)
  report_sha256 <- unname(tools::sha256sum(report_path))
  packages <- plan$package[wave_indices]
  package_directories <- vapply(
    wave_indices, reverse_create_row_directory, character(1L)
  )
  task_sha256 <- character(length(wave_indices))
  tasks <- lapply(seq_along(wave_indices), function(position) {
    index <- wave_indices[[position]]
    package_directory <- package_directories[[position]]
    task <- list(
      index = index,
      plan_row = plan[index, , drop = FALSE],
      package_directory = package_directory,
      protected_before = protected_before$combined
    )
    task_sha256[[position]] <<- rr_object_sha256(task)
    reverse_write_tsv(data.frame(
      schema = "1", plan_index = as.character(index),
      package = plan$package[[index]], wave = as.character(wave_number),
      position = as.character(position), worker_limit = as.character(wave_jobs),
      automatic_ceiling = as.character(wave_resource$jobs),
      operator_limit = if (is.null(operator_jobs)) "-" else {
        as.character(operator_jobs)
      },
      resource_report = report_relative,
      resource_report_sha256 = report_sha256,
      protected_metadata_before = protected_before$combined,
      started_utc = wave_started, task_sha256 = task_sha256[[position]],
      worker_script_sha256 = unname(
        reverse_metadata_sha256[[wave_worker_script]]
      ),
      worker_group_sha256 = unname(
        reverse_metadata_sha256[[worker_group_script]]
      ), stringsAsFactors = FALSE
    ), file.path(package_directory, "metadata", "worker-task.tsv"))
    task$rr_transport <- list(
      root = package_directory,
      state = file.path(package_directory, "worker-state"),
      launch = file.path(package_directory, "metadata", "worker-launch.tsv"),
      result = file.path(package_directory, "metadata", "worker-result.rds"),
      completion = file.path(
        package_directory, "metadata", "worker-completion.tsv"
      ),
      log = file.path(package_directory, "worker-transport.log")
    )
    task
  })
  wave <- rr_parallel_wave(tasks, function(task) {
    reverse_row(
      task$plan_row, task$package_directory, task$protected_before
    )
  }, wave_jobs, actual_rscript, wave_worker_script, worker_group_script,
  worker_timeout)
  if (!identical(wave$task_sha256, task_sha256)) {
    stop("external worker task identity changed during launch", call. = FALSE)
  }
  worker_error_indices <- integer()
  worker_missing_indices <- integer()
  worker_unstarted_indices <- integer()
  for (position in seq_along(wave$results)) {
    worker_result <- wave$results[[position]]
    if (!isTRUE(worker_result$ok)) {
      index <- wave_indices[[position]]
      durable_result <- file.path(
        package_directories[[position]], "metadata", "worker-result.rds"
      )
      durable_launch <- file.path(
        package_directories[[position]], "metadata", "worker-launch.tsv"
      )
      durable_completion <- file.path(
        package_directories[[position]], "metadata", "worker-completion.tsv"
      )
      launch_exists <- file.exists(durable_launch) &&
        !dir.exists(durable_launch) && !rr_is_symbolic(durable_launch)
      if (launch_exists &&
          file.exists(durable_result) && !dir.exists(durable_result) &&
          !rr_is_symbolic(durable_result) && file.exists(durable_completion) &&
          !dir.exists(durable_completion) &&
          !rr_is_symbolic(durable_completion)) {
        worker_error_indices <- c(worker_error_indices, index)
      } else if (!launch_exists) {
        worker_unstarted_indices <- c(worker_unstarted_indices, index)
      } else {
        worker_missing_indices <- c(worker_missing_indices, index)
      }
      rr_write_tsv(data.frame(
        field = c("plan_index", "package", "wave", "error", "finished_utc"),
        value = c(
          as.character(index), plan$package[[index]], as.character(wave_number),
          rr_compact_external_text(worker_result$error),
          format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        ), stringsAsFactors = FALSE
      ), file.path(package_directories[[position]], "metadata", "worker-error.tsv"))
    }
  }
  protected_after <- reverse_validate_row_boundary(
    paste0("after reverse wave ", wave_number)
  )
  for (index in wave_indices) {
    reverse_verify_plan_archive(
      index, paste("after wave checking", plan$package[[index]])
    )
  }
  wave_finished <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  rr_append_reverse_wave(
    waves_path, wave_indices, plan$package, wave_jobs, wave$backend,
    worker_error_indices, worker_missing_indices, worker_unstarted_indices,
    wave_resource$jobs, operator_jobs,
    report_relative, report_sha256,
    protected_before$combined, protected_after$combined,
    wave_started, wave_finished
  )
  invisible(rr_validate_reverse_waves(waves_path, plan$package))

  for (position in seq_along(wave_indices)) {
    index <- wave_indices[[position]]
    package <- plan$package[[index]]
    package_directory <- package_directories[[position]]
    worker_result <- wave$results[[position]]
    if (!isTRUE(worker_result$ok)) {
      message(package, ": worker error retained (", worker_result$error, ")")
      next
    }
    row <- reverse_finalize_durable_worker(
      index, wave_number, worker_result, protected_after$combined
    )
    row_evidence <- rr_verify_stage(package_directory)
    rr_append_reverse_acceptance(
      acceptance_path, index, package, wave_number, row, row_evidence
    )
    results[[index]] <- row
    message(package, ": ", row$status[[1L]], " (", row$classification[[1L]], ")")
  }
  reverse_write_results(results)
  if (length(c(
      worker_error_indices, worker_missing_indices, worker_unstarted_indices
    ))) {
    stop(
      "reverse wave ", wave_number, " retained worker errors for: ",
      paste(plan$package[c(
        worker_error_indices, worker_missing_indices, worker_unstarted_indices
      )], collapse = ", "),
      call. = FALSE
    )
  }
}

final_acceptance <- rr_validate_reverse_acceptance(
  acceptance_path, plan$package, file.path(run_directory, "packages"),
  require_complete = TRUE
)
final_waves <- rr_validate_reverse_waves(waves_path, plan$package)
invisible(rr_validate_reverse_acceptance_waves(final_acceptance, final_waves))

reverse_verify_tinytex(
  "after reverse-dependency checks", "tinytex-verify-post.log"
)
archive_postflight_path <- file.path(
  metadata_directory, "source-archive-postflight.tsv"
)
if (arguments$resume && file.exists(archive_postflight_path)) {
  archive_postflight <- reverse_read_tsv(
    archive_postflight_path, c("field", "value")
  )
  archive_postflight <- setNames(archive_postflight$value, archive_postflight$field)
  if (!identical(archive_postflight[["status"]], "passed") ||
      !identical(archive_postflight[["plan_sha256"]],
        unname(tools::sha256sum(plan_path))) ||
      !identical(archive_postflight[["packages"]], as.character(nrow(plan)))) {
    stop("retained source-archive postflight is malformed", call. = FALSE)
  }
  reverse_verify_plan_archives(
    "while resuming completed source-archive postflight", content = FALSE
  )
} else {
  reverse_verify_plan_archives(
    "during final reverse-dependency validation", content = TRUE
  )
  reverse_write_tsv(data.frame(
    field = c("status", "plan_sha256", "packages", "finished_utc"),
    value = c(
      "passed", unname(tools::sha256sum(plan_path)), as.character(nrow(plan)),
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ), stringsAsFactors = FALSE
  ), archive_postflight_path)
}
combined_results <- reverse_write_results(results)
reverse_validate_row_boundary("before final protected-library postflight")
full_hash_ledger <- rr_read_full_hash_ledger(full_hash_ledger_path)
expected_full_hashes <- c(candidate_library_initial, dependency_library_initial)
if (nrow(full_hash_ledger) == 1L) {
  message("Hashing protected libraries once for the final actual-stage postflight ...")
  rr_record_full_hash_pass(
    full_hash_ledger_path, full_hash_contexts[[2L]], full_hash_contexts,
    candidate_library, dependency_library, expected_full_hashes,
    protected_content_hasher
  )
}
rr_validate_full_hash_ledger(
  full_hash_ledger_path, full_hash_contexts, expected_full_hashes
)
rr_validate_protected_metadata(
  rr_protected_metadata(candidate_library, dependency_library),
  protected_metadata_initial, "after final protected-library postflight"
)
candidate_final <- candidate_content
candidate_library_final <- candidate_library_initial
dependency_final <- dependency_library_initial
source_final <- reverse_candidate_source_state()
metadata_final <- setNames(
  vapply(
    reverse_metadata_inputs,
    tools::sha256sum,
    character(1L),
    USE.NAMES = FALSE
  ),
  reverse_metadata_inputs
)
retained_metadata_final <- vapply(
  retained_reverse_inputs,
  tools::sha256sum,
  character(1L),
  USE.NAMES = FALSE
)
protected_inputs_unchanged <- reverse_candidate_source_matches(source_final) &&
  identical(metadata_final, reverse_metadata_sha256) &&
  identical(unname(reverse_metadata_sha256), retained_metadata_final)
if (!protected_inputs_unchanged) {
  stop("a protected source, library, or retained input changed during the gate",
    call. = FALSE)
}
compat_system_verify_evidence(
  compat_system_evidence,
  "during reverse-dependency gate completion"
)
rr_validate_protected_metadata(
  rr_protected_metadata(candidate_library, dependency_library),
  protected_metadata_initial, "after compatibility-system final verification"
)

passed <- nrow(combined_results) == nrow(plan) &&
  all(combined_results$status == "passed")
rr_write_tsv(
  data.frame(
    field = rr_reverse_completion_fields(FALSE),
    value = c(
      if (passed) "passed" else "completed_with_failures",
      nrow(plan), nrow(combined_results),
      candidate_final, candidate_library_final, dependency_final,
      unname(tools::sha256sum(results_path)),
      unname(tools::sha256sum(acceptance_path)),
      unname(tools::sha256sum(waves_path)),
      as.character(nrow(rr_read_reverse_waves(waves_path))), "2",
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    stringsAsFactors = FALSE
  ),
  file.path(metadata_directory, "completion.tsv"),
  replace = file.exists(file.path(metadata_directory, "completion.tsv"))
)
invisible(rr_seal_composite_stage(run_directory))
reverse_release_mutation_lock()

cat("Reverse-dependency artifacts: ", run_directory, "\n", sep = "")
if (!passed) quit(save = "no", status = 1L)
