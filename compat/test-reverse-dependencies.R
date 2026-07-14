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
    "  --candidate-content HASH    installed package content SHA-256\n",
    "                              (PARADOX_CANDIDATE_CONTENT_SHA256)\n\n",
    "  The candidate library must contain the sealed receipt written by\n",
    "  compat/install-candidate with the same ref, commit, and tree.\n\n",
    "Other options:\n",
    "  --run-id ID                 output below .local/compat/reverse-runs/ID\n",
    "  --package NAME              select one package; repeatable\n",
    "  --plan-only                 validate and retain a plan without checking\n",
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
    candidate_content = Sys.getenv("PARADOX_CANDIDATE_CONTENT_SHA256", unset = ""),
    run_id = NULL,
    packages = character(),
    plan_only = FALSE,
    help = FALSE
  )
  value_options <- c(
    "--root", "--max-priority", "--candidate-library",
    "--dependency-library", "--candidate-ref", "--candidate-commit",
    "--candidate-tree", "--candidate-content", "--run-id", "--package"
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
      if (identical(option, "--candidate-content")) values$candidate_content <- value
      if (identical(option, "--run-id")) values$run_id <- value
      if (identical(option, "--package")) values$packages <- c(values$packages, value)
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

root <- normalizePath(arguments$root, winslash = "/", mustWork = TRUE)
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
actual_r <- normalizePath(file.path(actual_r_home, "bin", "R"),
  winslash = "/", mustWork = TRUE)
if (!identical(actual_r_home, expected_r_home)) {
  stop("the reverse-dependency gate is not running under repository-local R", call. = FALSE)
}

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
run_directory <- file.path(reverse_runs_root, run_id)
if (file.exists(run_directory) || dir.exists(run_directory) ||
    reverse_is_symbolic(run_directory)) {
  stop("run directory already exists: ", run_directory, call. = FALSE)
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
git_authenticator_script <- file.path(
  root, "compat", "authenticate-candidate-git"
)
reverse_require_regular_provenance_file(
  git_authenticator_script, "candidate Git authenticator"
)
if (file.access(git_authenticator_script, mode = 1L) != 0L) {
  stop("candidate Git authenticator is not executable", call. = FALSE)
}
object_hash_pattern <- "^([0-9a-f]{40}|[0-9a-f]{64})$"
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
      c(root, candidate_ref, candidate_commit, candidate_tree),
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
compat_system_evidence_script <- file.path(
  root, "compat", "compat-system-evidence.R"
)
tinytex_manifest_path <- file.path(
  root, "environment", "tinytex-linux-x86_64.tsv"
)
tree_receipt_script <- file.path(
  root, "scripts", "environment", "tree-receipt.R"
)
for (path in c(
  fingerprint_script, harness_script, evidence_helper_script,
  evidence_verifier_script, compat_system_evidence_script,
  tinytex_manifest_path, tree_receipt_script
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

actual_rscript <- file.path(actual_r_home, "bin", "Rscript")
texi2dvi_path <- file.path(root, ".local", "toolchain", "bin", "texi2dvi")
reverse_require_regular_provenance_file(texi2dvi_path, "local texi2dvi")
if (!identical(unname(Sys.which("Rscript")), actual_rscript) ||
    !identical(unname(Sys.which("texi2dvi")), texi2dvi_path)) {
  stop("Rscript or texi2dvi does not resolve to the exact local toolchain",
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
  if (!identical(observed_size, tinytex_archive_size) ||
      !identical(
        unname(tools::sha256sum(tinytex_archive)),
        tinytex_archive_sha256
      )) {
    stop("TinyTeX archive changed ", context, call. = FALSE)
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
candidate_content_initial <- compat_tree_content_sha256(candidate_package)
if (!identical(candidate_content_initial, candidate_content)) {
  stop("candidate package does not match its declared content fingerprint", call. = FALSE)
}
candidate_library_initial <- compat_tree_content_sha256(candidate_library)
dependency_library_initial <- compat_tree_content_sha256(dependency_library)
candidate_version <- as.character(utils::packageVersion("paradox", lib.loc = candidate_library))
candidate_provenance <- reverse_validate_candidate_provenance(
  root = root,
  candidate_run_id = candidate_run_id,
  candidate_library = candidate_library,
  dependency_library = dependency_library,
  dependency_library_content_sha256 = dependency_library_initial,
  candidate_ref = candidate_ref,
  candidate_commit = candidate_commit,
  candidate_tree = candidate_tree,
  candidate_version = candidate_version,
  candidate_content = candidate_content
)

# Candidate provenance must be fully authenticated before the harness creates
# retained artifacts or inspects any consumer source archive.
if (!dir.exists(reverse_runs_root)) {
  if (!dir.create(reverse_runs_root, recursive = FALSE, showWarnings = FALSE)) {
    stop("could not reserve reverse-dependency evidence root", call. = FALSE)
  }
}
reverse_require_plain_directory(reverse_runs_root, "reverse-dependency evidence root")
reverse_runs_root <- normalizePath(reverse_runs_root, winslash = "/", mustWork = TRUE)
if (!identical(dirname(reverse_runs_root), file.path(root, ".local", "compat"))) {
  stop("reverse-dependency evidence root escaped the repository", call. = FALSE)
}
run_directory <- file.path(reverse_runs_root, run_id)
if (file.exists(run_directory) || dir.exists(run_directory) ||
    reverse_is_symbolic(run_directory)) {
  stop("run directory already exists: ", run_directory, call. = FALSE)
}
if (!dir.create(run_directory, recursive = FALSE, showWarnings = FALSE)) {
  stop("could not reserve reverse-dependency run directory", call. = FALSE)
}
run_directory <- normalizePath(run_directory, winslash = "/", mustWork = TRUE)
if (!identical(dirname(run_directory), reverse_runs_root)) {
  stop("reverse-dependency run directory escaped its evidence root", call. = FALSE)
}
for (child in c("metadata", "packages")) {
  child_path <- file.path(run_directory, child)
  if (!dir.create(child_path, recursive = FALSE, showWarnings = FALSE)) {
    stop("could not create reverse-dependency evidence child: ", child_path,
      call. = FALSE)
  }
}

inventory_path <- file.path(root, "compat", "reverse-dependencies.tsv")
cran_snapshot_path <- file.path(root, "compat", "cran-snapshot.tsv")
bioc_snapshot_path <- file.path(root, "compat", "bioconductor-snapshot.tsv")
reverse_metadata_inputs <- c(
  inventory_path, cran_snapshot_path, bioc_snapshot_path, fingerprint_script,
  harness_script, evidence_helper_script, evidence_verifier_script,
  compat_system_evidence_script,
  tinytex_manifest_path, tree_receipt_script,
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
}
if (!nrow(selected_inventory)) stop("priority selection is empty", call. = FALSE)

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
  observed_checksum <- unname(tools::sha256sum(archive))
  if (!identical(observed_checksum, declared_checksum)) {
    stop("pinned source archive checksum mismatch for ", package, call. = FALSE)
  }
  if (identical(source, "CRAN")) {
    declared_md5 <- tolower(snapshot_row$md5[[1L]])
    if (!grepl("^[0-9a-f]{32}$", declared_md5) ||
        !identical(unname(tools::md5sum(archive)), declared_md5)) {
      stop("pinned CRAN source archive MD5 mismatch for ", package, call. = FALSE)
    }
  }
  reverse_require_regular_provenance_file(archive, "pinned source archive")
  if (!identical(unname(tools::sha256sum(archive)), observed_checksum)) {
    stop("pinned source archive changed while authenticating ", package,
      call. = FALSE)
  }
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
    notes = inventory_row$notes[[1L]],
    stringsAsFactors = FALSE
  )
}
plan <- do.call(rbind, plan_rows)

reverse_verify_plan_archive <- function(index, context) {
  archive <- plan$archive[[index]]
  reverse_require_plain_local_directory(
    dirname(archive), paste(plan$source[[index]], "source directory")
  )
  reverse_require_regular_provenance_file(archive, "pinned source archive")
  observed <- unname(tools::sha256sum(archive))
  reverse_require_regular_provenance_file(archive, "pinned source archive")
  if (!identical(observed, plan$archive_sha256[[index]]) ||
      !identical(unname(tools::sha256sum(archive)), observed)) {
    stop("pinned source archive changed ", context, ": ", archive,
      call. = FALSE)
  }
  invisible(TRUE)
}

reverse_verify_plan_archives <- function(context) {
  for (index in seq_len(nrow(plan))) {
    reverse_verify_plan_archive(index, context)
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
copied_metadata <- file.copy(
  reverse_metadata_inputs,
  metadata_directory,
  copy.mode = TRUE,
  copy.date = TRUE
)
if (!all(copied_metadata)) stop("could not copy run metadata inputs", call. = FALSE)
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
compat_system_evidence <- compat_system_capture_evidence(root, metadata_directory)

retained_tree_receipt_script <- file.path(
  metadata_directory, basename(tree_receipt_script)
)
tinytex_tree_receipt <- file.path(metadata_directory, "tinytex-tree.tsv")
reverse_run_tinytex_receipt <- function(operation, log_path) {
  status <- suppressWarnings(system2(
    actual_rscript,
    c(
      "--vanilla", shQuote(retained_tree_receipt_script), operation,
      shQuote(tinytex_root), shQuote(tinytex_tree_receipt)
    ),
    stdout = log_path,
    stderr = log_path
  ))
  status <- as.integer(status %||% 0L)
  if (!identical(status, 0L)) {
    detail <- if (file.exists(log_path)) {
      paste(tail(readLines(log_path, warn = FALSE), 40L), collapse = "\n")
    } else {
      ""
    }
    stop(
      "TinyTeX tree receipt ", operation, " failed",
      if (nzchar(detail)) paste0(": ", detail) else "",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
reverse_run_tinytex_receipt(
  "create", file.path(metadata_directory, "tinytex-receipt-create.log")
)
reverse_require_regular_provenance_file(
  tinytex_tree_receipt, "retained TinyTeX tree receipt"
)
if (!identical(
    unname(tools::sha256sum(tinytex_tree_receipt)),
    tinytex_tree_sha256
  )) {
  stop("installed TinyTeX tree differs from the pinned archive", call. = FALSE)
}

reverse_verify_tinytex <- function(context, log_name) {
  reverse_validate_tinytex_live(context)
  if (!identical(
      unname(tools::sha256sum(tinytex_tree_receipt)),
      tinytex_tree_sha256
    )) {
    stop("retained TinyTeX tree receipt changed ", context, call. = FALSE)
  }
  reverse_run_tinytex_receipt(
    "verify", file.path(metadata_directory, log_name)
  )
  invisible(TRUE)
}
reverse_verify_tinytex(
  "before reverse-dependency checks", "tinytex-verify-pre.log"
)
reverse_write_tsv(plan, file.path(run_directory, "plan.tsv"))

source_date_epoch <- reverse_single_git_value(
  c("show", "-s", "--format=%ct", shQuote(candidate_commit)),
  "reading the candidate commit time"
)
run_metadata <- data.frame(
  field = c(
    "schema", "run_id", "started_utc", "root", "max_priority", "plan_only",
    "candidate_run_id", "candidate_ref", "candidate_commit", "candidate_tree",
    "candidate_version",
    "candidate_library", "candidate_package", "candidate_content_sha256",
    "candidate_library_content_sha256", "dependency_library",
    "dependency_library_content_sha256", "r", "r_version", "source_date_epoch",
    "candidate_provenance_sha256", "candidate_source_archive_sha256",
    "candidate_installer_sha256", "candidate_git_authenticator_sha256",
    "inventory_sha256", "cran_snapshot_sha256", "bioconductor_snapshot_sha256",
    "harness_sha256", "evidence_helper_sha256", "evidence_verifier_sha256",
    "tinytex_manifest_sha256", "tree_receipt_helper_sha256",
    "tinytex_root", "tinytex_archive", "tinytex_archive_sha256",
    "tinytex_tree_receipt_sha256", "pdflatex_sha256", "kpsewhich_sha256",
    "makeindex_sha256", "texi2dvi", "texi2dvi_sha256"
  ),
  value = c(
    "2", run_id, format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), root,
    as.character(max_priority), as.character(arguments$plan_only),
    candidate_run_id, candidate_ref,
    candidate_commit, candidate_tree, candidate_version, candidate_library,
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
    unname(reverse_metadata_sha256[[harness_script]]),
    unname(reverse_metadata_sha256[[evidence_helper_script]]),
    unname(reverse_metadata_sha256[[evidence_verifier_script]]),
    unname(reverse_metadata_sha256[[tinytex_manifest_path]]),
    unname(reverse_metadata_sha256[[tree_receipt_script]]),
    tinytex_root, tinytex_archive_name, tinytex_archive_sha256,
    tinytex_tree_sha256, tinytex_tool_sha256[["pdflatex"]],
    tinytex_tool_sha256[["kpsewhich"]], tinytex_tool_sha256[["makeindex"]],
    texi2dvi_path, texi2dvi_sha256
  ),
  stringsAsFactors = FALSE
)
reverse_write_tsv(run_metadata, file.path(metadata_directory, "run.tsv"))

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
  compat_system_verify_evidence(
    compat_system_evidence,
    "during final reverse-dependency plan validation"
  )
  if (!reverse_candidate_source_matches(plan_source_final) ||
      !identical(compat_tree_content_sha256(candidate_package), candidate_content) ||
      !identical(
        compat_tree_content_sha256(candidate_library),
        candidate_library_initial
      ) ||
      !identical(
        compat_tree_content_sha256(dependency_library),
        dependency_library_initial
      ) ||
      !identical(plan_metadata_final, reverse_metadata_sha256) ||
      !identical(unname(reverse_metadata_sha256), plan_retained_final)) {
    stop("a protected source or library changed during plan validation",
      call. = FALSE)
  }
  reverse_write_tsv(
    data.frame(
      field = c("status", "packages", "finished_utc"),
      value = c(
        "planned", nrow(plan),
        format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ),
      stringsAsFactors = FALSE
    ),
    file.path(metadata_directory, "completion.tsv")
  )
  repository_seal_evidence(run_directory)
  cat("Validated reverse-dependency plan: ", run_directory, "\n", sep = "")
  quit(save = "no", status = 0L)
}

reverse_compact <- function(value, limit = 8000L) {
  value <- gsub("[\r\n\t]+", " ", value)
  if (nchar(value, type = "chars") <= limit) return(value)
  side <- as.integer((limit - 80L) / 2L)
  paste0(
    substr(value, 1L, side),
    " [... complete output retained in package log ...] ",
    substr(value, nchar(value, type = "chars") - side + 1L, nchar(value, type = "chars"))
  )
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

reverse_environment <- function(package_directory) {
  path_value <- Sys.getenv("PATH", unset = "")
  if (!nzchar(path_value)) stop("activated PATH is empty", call. = FALSE)
  user <- Sys.info()[["user"]]
  if (is.na(user) || !nzchar(user)) user <- "paradox-check"
  texmf_root <- file.path(package_directory, "texmf")
  library_environment <- paste(c(candidate_library, dependency_library),
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
    TESTTHAT_PARALLEL = "false",
    `_R_CHECK_FORCE_SUGGESTS_` = "false",
    TMPDIR = file.path(package_directory, "tmp"),
    XDG_RUNTIME_DIR = file.path(package_directory, "runtime"),
    XDG_CACHE_HOME = file.path(package_directory, "cache"),
    R_USER_CACHE_DIR = file.path(package_directory, "cache", "R"),
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
    MAKEFLAGS = "-j1",
    LC_ALL = "C.UTF-8",
    TZ = "UTC",
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
  overlay_environment <- compat_system_child_environment()
  environment[names(overlay_environment)] <- overlay_environment
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

reverse_empty_results <- function() {
  data.frame(
    package = character(), source = character(), relation = character(),
    priority = integer(), version = character(), archive = character(),
    archive_sha256 = character(), candidate_ref = character(),
    candidate_commit = character(), candidate_tree = character(),
    observed_ref_commit_before = character(),
    observed_ref_commit_after = character(),
    observed_ref_tree_before = character(), observed_ref_tree_after = character(),
    candidate_version = character(), candidate_content_sha256 = character(),
    observed_candidate_content_before = character(),
    observed_candidate_content_after = character(),
    candidate_library_content_before = character(),
    candidate_library_content_after = character(),
    dependency_library_content_before = character(),
    dependency_library_content_after = character(), not_cran = character(),
    exit_code = integer(), check_status = character(), status = character(),
    classification = character(), elapsed_seconds = numeric(), log = character(),
    error = character(), stringsAsFactors = FALSE
  )
}

results <- vector("list", nrow(plan))
results_path <- file.path(run_directory, "results.tsv")
reverse_write_results <- function(values) {
  values <- Filter(Negate(is.null), values)
  combined <- if (length(values)) do.call(rbind, values) else reverse_empty_results()
  output <- combined
  output$error <- vapply(output$error, reverse_compact, character(1L))
  for (field in names(output)) {
    if (is.character(output[[field]])) {
      output[[field]][is.na(output[[field]]) | !nzchar(output[[field]])] <- "-"
    }
  }
  reverse_write_tsv(output, results_path)
  combined
}

for (index in seq_len(nrow(plan))) {
  package <- plan$package[[index]]
  package_directory <- file.path(run_directory, "packages", package)
  check_library <- file.path(package_directory, "library")
  work_directory <- file.path(package_directory, "work")
  dir.create(check_library, recursive = TRUE, showWarnings = FALSE)
  dir.create(work_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(package_directory, "home"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(package_directory, "tmp"), recursive = TRUE, showWarnings = FALSE)
  ccache_temporary <- file.path(package_directory, "tmp", "ccache")
  runtime_directory <- file.path(package_directory, "runtime")
  if (!dir.create(ccache_temporary, recursive = TRUE, showWarnings = FALSE) ||
      !dir.create(
        runtime_directory,
        recursive = TRUE,
        showWarnings = FALSE,
        mode = "0700"
      ) ||
      !isTRUE(Sys.chmod(runtime_directory, mode = "0700"))) {
    stop("could not create package-local ccache/runtime state for ", package,
      call. = FALSE)
  }
  dir.create(file.path(package_directory, "cache", "R"), recursive = TRUE, showWarnings = FALSE)
  for (texmf_child in c("var", "config", "home", "cache", "fonts")) {
    if (!dir.create(
        file.path(package_directory, "texmf", texmf_child),
        recursive = TRUE,
        showWarnings = FALSE
      ) && !dir.exists(file.path(package_directory, "texmf", texmf_child))) {
      stop("could not create package-local TeX state for ", package,
        call. = FALSE)
    }
  }

  source_before <- reverse_candidate_source_state()
  if (!reverse_candidate_source_matches(source_before)) {
    stop("candidate source ref changed before checking ", package, call. = FALSE)
  }
  candidate_before <- compat_tree_content_sha256(candidate_package)
  candidate_library_before <- compat_tree_content_sha256(candidate_library)
  dependency_before <- compat_tree_content_sha256(dependency_library)
  if (!identical(candidate_before, candidate_content) ||
      !identical(candidate_library_before, candidate_library_initial) ||
      !identical(dependency_before, dependency_library_initial)) {
    stop("protected library changed before checking ", package, call. = FALSE)
  }

  child_environment <- reverse_environment(package_directory)
  preflight_script <- file.path(package_directory, "preflight.R")
  writeLines(c(
    "args <- commandArgs(TRUE)",
    "candidate <- normalizePath(args[[1L]], winslash = '/', mustWork = TRUE)",
    "tool_names <- c('pdflatex', 'kpsewhich', 'makeindex')",
    "expected_tools <- args[2:4]",
    "expected_texi2dvi <- args[[5L]]",
    "observed_tools <- unname(Sys.which(tool_names))",
    "if (!identical(observed_tools, expected_tools)) stop('child resolved an unexpected TeX tool', call. = FALSE)",
    "if (!identical(Sys.getenv('R_TEXI2DVICMD'), expected_texi2dvi) || !identical(Sys.getenv('TEXI2DVI'), expected_texi2dvi) || !identical(getOption('texi2dvi'), expected_texi2dvi)) stop('child resolved an unexpected texi2dvi', call. = FALSE)",
    "texmf_variables <- c('TEXMFVAR', 'TEXMFCONFIG', 'TEXMFHOME', 'TEXMFCACHE', 'VARTEXFONTS')",
    "texmf_paths <- Sys.getenv(texmf_variables, unset = '')",
    "if (any(!nzchar(texmf_paths)) || any(!dir.exists(texmf_paths)) || any(file.access(texmf_paths, mode = 2L) != 0L)) stop('child TeX state is not package-local and writable', call. = FALSE)",
    "resolved <- normalizePath(find.package('paradox'), winslash = '/', mustWork = TRUE)",
    "if (!identical(resolved, candidate)) stop('child resolved the wrong paradox package', call. = FALSE)",
    "cat('candidate_path=', resolved, '\\n', sep = '')",
    "cat('candidate_version=', as.character(packageVersion('paradox')), '\\n', sep = '')",
    "cat('NOT_CRAN=', Sys.getenv('NOT_CRAN'), '\\n', sep = '')",
    "cat('TeX_tools=', paste(observed_tools, collapse = ':'), '\\n', sep = '')",
    "cat('TeX_state=', paste(texmf_paths, collapse = ':'), '\\n', sep = '')"
  ), preflight_script, useBytes = TRUE)

  preflight_command <- file.path(package_directory, "preflight-command.sh")
  reverse_write_command(
    preflight_command,
    child_environment,
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla", preflight_script, candidate_package,
      unname(tinytex_tool_paths), texi2dvi_path
    )
  )
  preflight_log <- file.path(package_directory, "preflight.log")
  reverse_verify_plan_archive(index, paste("before checking", package))
  preflight_status <- suppressWarnings(system2(
    "/bin/sh",
    shQuote(preflight_command),
    stdout = preflight_log,
    stderr = preflight_log
  ))

  check_command <- file.path(package_directory, "check-command.sh")
  reverse_write_command(
    check_command,
    child_environment,
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "check", "--no-manual", "--no-multiarch",
      paste0("--library=", check_library), plan$archive[[index]]
    )
  )
  check_log <- file.path(package_directory, "check.log")
  started <- Sys.time()
  command_status <- if (identical(preflight_status, 0L)) {
    local({
      old_directory <- setwd(work_directory)
      on.exit(setwd(old_directory), add = TRUE)
      suppressWarnings(system2(
        "/bin/sh",
        shQuote(check_command),
        stdout = check_log,
        stderr = check_log
      ))
    })
  } else {
    file.copy(preflight_log, check_log)
    preflight_status
  }
  command_status <- as.integer(command_status %||% 0L)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  reverse_verify_plan_archive(index, paste("while checking", package))

  check_directories <- list.dirs(work_directory, recursive = FALSE, full.names = TRUE)
  check_directories <- check_directories[endsWith(check_directories, ".Rcheck")]
  check_status <- ""
  if (length(check_directories) == 1L) {
    check_status_path <- file.path(check_directories[[1L]], "00check.log")
    if (file.exists(check_status_path)) {
      status_lines <- grep("^Status:", readLines(check_status_path, warn = FALSE), value = TRUE)
      if (length(status_lines) == 1L) check_status <- status_lines[[1L]]
    }
  }
  log_lines <- if (file.exists(check_log)) readLines(check_log, warn = FALSE) else character()
  log_text <- paste(log_lines, collapse = "\n")
  check_failed <- !identical(command_status, 0L) || !nzchar(check_status) ||
    grepl("(ERROR|WARNING)", check_status)

  candidate_after <- compat_tree_content_sha256(candidate_package)
  candidate_library_after <- compat_tree_content_sha256(candidate_library)
  dependency_after <- compat_tree_content_sha256(dependency_library)
  source_after_error <- ""
  source_after <- tryCatch(
    reverse_candidate_source_state(),
    error = function(condition) {
      source_after_error <<- conditionMessage(condition)
      list(ref_commit = "", ref_tree = "", commit_tree = "")
    }
  )
  source_provenance_failed <- nzchar(source_after_error) ||
    !reverse_candidate_source_matches(source_after)
  provenance_failed <- source_provenance_failed ||
    !identical(candidate_after, candidate_content) ||
    !identical(candidate_library_after, candidate_library_initial) ||
    !identical(dependency_after, dependency_library_initial)
  classification <- if (provenance_failed) {
    "protected_library_provenance_violation"
  } else if (check_failed) {
    reverse_classify_failure(log_text, command_status, check_status)
  } else if (grepl("NOTE", check_status, fixed = TRUE)) {
    "passed_with_notes"
  } else {
    "passed"
  }
  status <- if (provenance_failed || check_failed) "failed" else "passed"
  error <- if (identical(status, "failed")) {
    paste(
      c(
        if (nzchar(source_after_error)) source_after_error else character(),
        tail(log_lines, 100L)
      ),
      collapse = "\n"
    )
  } else {
    ""
  }

  results[[index]] <- data.frame(
    package = package,
    source = plan$source[[index]],
    relation = plan$relation[[index]],
    priority = plan$priority[[index]],
    version = plan$version[[index]],
    archive = plan$archive_name[[index]],
    archive_sha256 = plan$archive_sha256[[index]],
    candidate_ref = candidate_ref,
    candidate_commit = candidate_commit,
    candidate_tree = candidate_tree,
    observed_ref_commit_before = source_before$ref_commit,
    observed_ref_commit_after = source_after$ref_commit,
    observed_ref_tree_before = source_before$ref_tree,
    observed_ref_tree_after = source_after$ref_tree,
    candidate_version = candidate_version,
    candidate_content_sha256 = candidate_content,
    observed_candidate_content_before = candidate_before,
    observed_candidate_content_after = candidate_after,
    candidate_library_content_before = candidate_library_before,
    candidate_library_content_after = candidate_library_after,
    dependency_library_content_before = dependency_before,
    dependency_library_content_after = dependency_after,
    not_cran = "true",
    exit_code = command_status,
    check_status = check_status,
    status = status,
    classification = classification,
    elapsed_seconds = elapsed,
    log = substring(check_log, nchar(run_directory) + 2L),
    error = error,
    stringsAsFactors = FALSE
  )
  reverse_write_results(results)
  message(package, ": ", status, " (", classification, ")")
  if (provenance_failed) break
}

reverse_verify_tinytex(
  "after reverse-dependency checks", "tinytex-verify-post.log"
)
reverse_verify_plan_archives("during final reverse-dependency validation")
combined_results <- reverse_write_results(results)
candidate_final <- compat_tree_content_sha256(candidate_package)
candidate_library_final <- compat_tree_content_sha256(candidate_library)
dependency_final <- compat_tree_content_sha256(dependency_library)
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
  identical(candidate_final, candidate_content) &&
  identical(candidate_library_final, candidate_library_initial) &&
  identical(dependency_final, dependency_library_initial) &&
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

passed <- nrow(combined_results) == nrow(plan) &&
  all(combined_results$status == "passed")
reverse_write_tsv(
  data.frame(
    field = c(
      "status", "packages_planned", "packages_completed", "candidate_content_sha256",
      "candidate_library_content_sha256", "dependency_library_content_sha256",
      "results_sha256", "finished_utc"
    ),
    value = c(
      if (passed) "passed" else "failed", nrow(plan), nrow(combined_results),
      candidate_final, candidate_library_final, dependency_final,
      unname(tools::sha256sum(results_path)),
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    stringsAsFactors = FALSE
  ),
  file.path(metadata_directory, "completion.tsv")
)
repository_seal_evidence(run_directory)

cat("Reverse-dependency artifacts: ", run_directory, "\n", sep = "")
if (!passed) quit(save = "no", status = 1L)
