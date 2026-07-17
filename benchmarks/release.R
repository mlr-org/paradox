`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

release_fail <- function(...) {
  stop(paste0(...), call. = FALSE)
}

release_usage <- function() {
  cat(paste0(
    "Sealed paradox release benchmark gate\n\n",
    "Usage:\n",
    "  benchmarks/release [options]\n\n",
    "Required options:\n",
    "  --baseline-evidence PATH  Passed, sealed full differential run\n",
    "  --candidate-library PATH  Run-specific compat/install-candidate library\n",
    "  --dependency-library PATH Read-only dependency library; repeatable\n",
    "                            (the first must provide mlr3pipelines)\n",
    "  --mies-library PATH       Read-only library providing miesmuschel\n",
    "  --output PATH             Absent output below .local/benchmarks\n\n",
    "Additional options:\n",
    "  --protected-library PATH  Additional read-only library reachable by workers;\n",
    "                            repeatable and passed to the paired runner\n",
    "  --params N                Mixed parameter count [default: 64]\n",
    "  --rows N                  check_dt/qunif row count [default: 128]\n",
    "  --iterations N            Timed samples per paired workload [default: 100;\n",
    "                            reviewed policy minimum: 50]\n",
    "  --warmups N               Paired workload warmups [default: 5]\n",
    "  --seed N                  Deterministic paired seed [default: 20260713]\n",
    "  --plan-only               Authenticate inputs and print commands; reserve no output\n",
    "  -h, --help                Show this help\n\n",
    "PARADOX_CANDIDATE_RUN_ID, PARADOX_CANDIDATE_REF,\n",
    "PARADOX_CANDIDATE_COMMIT, PARADOX_CANDIDATE_TREE, and\n",
    "PARADOX_CANDIDATE_CONTENT_SHA256 must describe the installed candidate.\n",
    "The workload inventory is deliberately not selectable: a successful release\n",
    "run always measures every workload plus both focused consumer processes,\n",
    "then applies the authenticated distribution-aware regression policy.\n"
  ))
}

release_script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(release_script_argument) != 1L) {
  release_fail("could not identify the release driver location")
}
release_script <- normalizePath(
  substring(release_script_argument, nchar("--file=") + 1L),
  winslash = "/",
  mustWork = TRUE
)
release_root <- normalizePath(
  file.path(dirname(release_script), ".."),
  winslash = "/",
  mustWork = TRUE
)
release_expected_r <- file.path(release_root, ".local", "toolchain", "bin", "R")
release_expected_rscript <- file.path(
  release_root, ".local", "toolchain", "bin", "Rscript"
)
if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT", unset = ""), release_root) ||
    !identical(unname(Sys.which("R")), release_expected_r) ||
    !identical(unname(Sys.which("Rscript")), release_expected_rscript) ||
    !identical(
      normalizePath(R.home(), winslash = "/", mustWork = TRUE),
      file.path(release_root, ".local", "toolchain", "lib", "R")
    )) {
  release_fail(
    "the exact repository-local R runtime is not active; run '. scripts/activate'"
  )
}
if (!identical(release_script, file.path(release_root, "benchmarks", "release.R")) ||
    nzchar(Sys.readlink(release_script))) {
  release_fail("release.R must be the regular repository benchmark driver")
}
setwd(release_root)
release_expected_libraries <- c(
  file.path(release_root, ".local", "R", "library"),
  file.path(release_root, ".local", "toolchain", "lib", "R", "library")
)
release_observed_libraries <- normalizePath(
  .libPaths(), winslash = "/", mustWork = TRUE
)
if (!identical(release_observed_libraries, release_expected_libraries) ||
    !identical(
      Sys.getenv("R_LIBS_USER", unset = ""), release_expected_libraries[[1L]]
    ) ||
    !identical(
      Sys.getenv("R_ENVIRON_USER", unset = ""),
      file.path(release_root, "environment", "Renviron")
    ) ||
    !identical(
      Sys.getenv("R_MAKEVARS_USER", unset = ""),
      file.path(release_root, "environment", "Makevars")
    ) ||
    !identical(
      Sys.getenv("TMPDIR", unset = ""), file.path(release_root, ".local", "tmp")
    )) {
  release_fail("the activated R startup, library, or temporary-file state is not exact")
}

release_take_value <- function(arguments, index, option) {
  argument <- arguments[[index]]
  prefix <- paste0(option, "=")
  if (startsWith(argument, prefix)) {
    value <- substring(argument, nchar(prefix) + 1L)
    if (!nzchar(value)) release_fail(option, " requires a value")
    return(list(value = value, next_index = index + 1L))
  }
  if (!identical(argument, option)) return(NULL)
  if (index == length(arguments)) release_fail(option, " requires a value")
  list(value = arguments[[index + 1L]], next_index = index + 2L)
}

release_integer <- function(value, option, minimum) {
  parsed <- suppressWarnings(as.integer(value))
  if (length(parsed) != 1L || is.na(parsed) ||
      !identical(as.character(parsed), value) || parsed < minimum) {
    release_fail(option, " requires an integer >= ", minimum, "; got '", value, "'")
  }
  parsed
}

release_parse_arguments <- function(arguments) {
  result <- list(
    baseline_evidence = NULL,
    candidate_library = NULL,
    dependency_libraries = character(),
    mies_library = NULL,
    protected_libraries = character(),
    output = NULL,
    n_params = 64L,
    n_rows = 128L,
    iterations = 100L,
    warmups = 5L,
    seed = 20260713L,
    plan_only = FALSE,
    help = FALSE
  )
  value_options <- c(
    "--baseline-evidence", "--candidate-library", "--dependency-library",
    "--mies-library", "--protected-library", "--output", "--params",
    "--rows", "--iterations", "--warmups", "--seed"
  )
  singleton_seen <- character()
  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (argument %in% c("-h", "--help")) {
      if (result$help) release_fail("--help may be supplied only once")
      result$help <- TRUE
      index <- index + 1L
      next
    }
    if (identical(argument, "--plan-only")) {
      if (result$plan_only) release_fail("--plan-only may be supplied only once")
      result$plan_only <- TRUE
      index <- index + 1L
      next
    }
    matched <- FALSE
    for (option in value_options) {
      taken <- release_take_value(arguments, index, option)
      if (is.null(taken)) next
      matched <- TRUE
      index <- taken$next_index
      value <- taken$value
      if (option %in% c(
          "--baseline-evidence", "--candidate-library", "--mies-library",
          "--output", "--params", "--rows", "--iterations", "--warmups",
          "--seed"
        )) {
        if (option %in% singleton_seen) {
          release_fail(option, " may be supplied only once")
        }
        singleton_seen <- c(singleton_seen, option)
      }
      if (identical(option, "--baseline-evidence")) result$baseline_evidence <- value
      if (identical(option, "--candidate-library")) result$candidate_library <- value
      if (identical(option, "--dependency-library")) {
        result$dependency_libraries <- c(result$dependency_libraries, value)
      }
      if (identical(option, "--mies-library")) result$mies_library <- value
      if (identical(option, "--protected-library")) {
        result$protected_libraries <- c(result$protected_libraries, value)
      }
      if (identical(option, "--output")) result$output <- value
      if (identical(option, "--params")) {
        result$n_params <- release_integer(value, option, 4L)
      }
      if (identical(option, "--rows")) {
        result$n_rows <- release_integer(value, option, 1L)
      }
      if (identical(option, "--iterations")) {
        result$iterations <- release_integer(value, option, 1L)
      }
      if (identical(option, "--warmups")) {
        result$warmups <- release_integer(value, option, 0L)
      }
      if (identical(option, "--seed")) {
        result$seed <- release_integer(value, option, 0L)
      }
      break
    }
    if (!matched) release_fail("unknown option '", argument, "'; use --help")
  }
  result
}

release_arguments <- release_parse_arguments(commandArgs(trailingOnly = TRUE))
if (release_arguments$help) {
  release_usage()
  quit(save = "no", status = 0L)
}
for (field in c(
  "baseline_evidence", "candidate_library", "mies_library", "output"
)) {
  if (is.null(release_arguments[[field]])) {
    release_fail("missing required option --", gsub("_", "-", field))
  }
}
if (!length(release_arguments$dependency_libraries)) {
  release_fail("at least one --dependency-library is required")
}

release_is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

release_require_regular_file <- function(path, label) {
  if (!file.exists(path) || dir.exists(path) || release_is_symbolic(path)) {
    release_fail(label, " is absent, not a regular file, or symbolic: ", path)
  }
  invisible(path)
}

release_contains_control <- function(value) {
  grepl("[[:cntrl:]]", value)
}

release_absolute <- function(path) {
  if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) path else file.path(release_root, path)
}

release_require_local_directory <- function(path, label) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      release_contains_control(path)) {
    release_fail(label, " path is empty or contains a control character")
  }
  path <- release_absolute(path)
  if (!dir.exists(path) || release_is_symbolic(path)) {
    release_fail(label, " is absent, not a directory, or symbolic: ", path)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  local_root <- file.path(release_root, ".local")
  if (!startsWith(path, paste0(local_root, "/"))) {
    release_fail(label, " must remain below ", local_root, ": ", path)
  }
  path
}

release_output_path <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      release_contains_control(path)) {
    release_fail("--output is empty or contains a control character")
  }
  path <- release_absolute(path)
  if (file.exists(path) || dir.exists(path) || release_is_symbolic(path)) {
    release_fail("--output must be a new path: ", path)
  }
  parent <- dirname(path)
  if (!dir.exists(parent) || release_is_symbolic(parent)) {
    release_fail("--output parent must be an existing plain directory: ", parent)
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  benchmark_root <- file.path(release_root, ".local", "benchmarks")
  if (!identical(parent, benchmark_root) &&
      !startsWith(parent, paste0(benchmark_root, "/"))) {
    release_fail("--output must remain below ", benchmark_root)
  }
  name <- basename(path)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", name) ||
      name %in% c(".", "..")) {
    release_fail("--output must end in a safe run name of at most 128 characters")
  }
  file.path(parent, name)
}

release_git <- unname(Sys.which("git"))
release_expected_git <- file.path(release_root, ".local", "toolchain", "bin", "git")
if (!identical(release_git, release_expected_git)) {
  release_fail("git is not the exact activated repository-local executable")
}
release_git_environment <- Sys.getenv()
release_git_environment_names <- names(release_git_environment)
release_unsafe_git_environment <- startsWith(
  release_git_environment_names, "GIT_"
) & !release_git_environment_names %in% "GIT_PAGER" &
  nzchar(unname(release_git_environment))
if (any(release_unsafe_git_environment)) {
  release_fail(
    "release benchmarking rejects repository-altering Git environment: ",
    paste(
      release_git_environment_names[release_unsafe_git_environment],
      collapse = ", "
    )
  )
}

release_run_capture <- function(command, arguments, label, environment = character()) {
  stderr_file <- tempfile("paradox-release-stderr-")
  on.exit(unlink(stderr_file), add = TRUE)
  output <- suppressWarnings(system2(
    command,
    args = vapply(arguments, shQuote, character(1L)),
    env = environment,
    stdout = TRUE,
    stderr = stderr_file
  ))
  status <- attr(output, "status") %||% 0L
  stderr <- if (file.exists(stderr_file)) readLines(stderr_file, warn = FALSE) else character()
  if (!identical(as.integer(status), 0L)) {
    release_fail(
      label, " failed with status ", status,
      if (length(stderr)) paste0(": ", paste(stderr, collapse = "\n")) else ""
    )
  }
  output
}

release_git_one <- function(arguments, label) {
  output <- release_run_capture(release_git, arguments, label)
  if (length(output) != 1L || !nzchar(output[[1L]])) {
    release_fail(label, " returned an unexpected result")
  }
  output[[1L]]
}

release_require_committed_files <- function(paths, label) {
  for (path in unname(paths)) {
    release_require_regular_file(path, label)
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!startsWith(path, paste0(release_root, "/"))) {
      release_fail(label, " escaped the repository: ", path)
    }
    relative <- substring(path, nchar(release_root, type = "chars") + 2L)
    blob <- tempfile("paradox-release-git-blob-")
    error <- tempfile("paradox-release-git-error-")
    on.exit(unlink(c(blob, error)), add = TRUE)
    status <- suppressWarnings(system2(
      release_git,
      args = vapply(c(
        "--no-replace-objects", "-C", release_root, "cat-file", "blob",
        paste0(release_candidate_commit, ":", relative)
      ), shQuote, character(1L)),
      env = c(
        "GIT_CONFIG_GLOBAL=/dev/null", "GIT_CONFIG_SYSTEM=/dev/null",
        "GIT_CONFIG_NOSYSTEM=1", "GIT_NO_REPLACE_OBJECTS=1"
      ),
      stdout = blob,
      stderr = error
    ))
    status <- as.integer(status %||% 0L)
    if (!identical(status, 0L) || !file.exists(blob) ||
        !identical(
          unname(tools::sha256sum(path)), unname(tools::sha256sum(blob))
        )) {
      detail <- if (file.exists(error)) {
        paste(readLines(error, warn = FALSE), collapse = "\n")
      } else {
        ""
      }
      release_fail(
        label, " differs from the exported candidate commit: ", relative,
        if (nzchar(detail)) paste0(": ", detail) else ""
      )
    }
    unlink(c(blob, error))
  }
  invisible(paths)
}

release_candidate_run_id <- Sys.getenv("PARADOX_CANDIDATE_RUN_ID", unset = "")
release_candidate_ref <- Sys.getenv("PARADOX_CANDIDATE_REF", unset = "")
release_candidate_commit <- Sys.getenv("PARADOX_CANDIDATE_COMMIT", unset = "")
release_candidate_tree <- Sys.getenv("PARADOX_CANDIDATE_TREE", unset = "")
release_candidate_content <- Sys.getenv(
  "PARADOX_CANDIDATE_CONTENT_SHA256", unset = ""
)
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", release_candidate_run_id) ||
    release_candidate_run_id %in% c(".", "..")) {
  release_fail("PARADOX_CANDIDATE_RUN_ID is missing or malformed")
}
if (!startsWith(release_candidate_ref, "refs/") ||
    !identical(release_candidate_commit, tolower(release_candidate_commit)) ||
    !identical(release_candidate_tree, tolower(release_candidate_tree)) ||
    !identical(release_candidate_content, tolower(release_candidate_content)) ||
    !grepl("^[0-9a-f]{40}$", release_candidate_commit) ||
    !grepl("^[0-9a-f]{40}$", release_candidate_tree) ||
    !grepl("^[0-9a-f]{64}$", release_candidate_content)) {
  release_fail("candidate ref, commit, tree, or content environment is malformed")
}
invisible(release_run_capture(
  release_git, c("check-ref-format", release_candidate_ref),
  "validating the candidate ref"
))

release_authenticate_root <- function() {
  top_level <- release_git_one(
    c("-C", release_root, "rev-parse", "--show-toplevel"),
    "resolving repository top level"
  )
  git_directory <- release_git_one(
    c("-C", release_root, "rev-parse", "--absolute-git-dir"),
    "resolving repository Git directory"
  )
  expected_git_directory <- file.path(release_root, ".git")
  if (!identical(
      normalizePath(top_level, winslash = "/", mustWork = TRUE), release_root
    ) || !dir.exists(expected_git_directory) ||
      release_is_symbolic(expected_git_directory) ||
      !identical(
        normalizePath(git_directory, winslash = "/", mustWork = TRUE),
        expected_git_directory
      )) {
    release_fail("release benchmarking requires the repository's plain primary Git directory")
  }
  replacements <- release_run_capture(
    release_git,
    c(
      "-C", release_root, "for-each-ref", "--format=%(refname)",
      "refs/replace"
    ),
    "checking Git replacement refs"
  )
  grafts <- file.path(expected_git_directory, "info", "grafts")
  if (length(replacements) ||
      (file.exists(grafts) && isTRUE(file.info(grafts)$size > 0))) {
    release_fail("Git replacement refs and grafts are forbidden for release evidence")
  }
  head <- release_git_one(
    c("-C", release_root, "rev-parse", "HEAD^{commit}"),
    "resolving repository HEAD"
  )
  tree <- release_git_one(
    c("-C", release_root, "rev-parse", "HEAD^{tree}"),
    "resolving repository tree"
  )
  ref_commit <- release_git_one(
    c("-C", release_root, "rev-parse", paste0(release_candidate_ref, "^{commit}")),
    "resolving candidate ref"
  )
  commit <- release_git_one(
    c("-C", release_root, "rev-parse", paste0(release_candidate_commit, "^{commit}")),
    "resolving candidate commit"
  )
  status <- release_run_capture(
    release_git,
    c("-C", release_root, "status", "--porcelain=v1", "--untracked-files=all"),
    "checking repository cleanliness"
  )
  if (!identical(head, release_candidate_commit) ||
      !identical(commit, release_candidate_commit) ||
      !identical(ref_commit, release_candidate_commit) ||
      !identical(tree, release_candidate_tree) || length(status)) {
    release_fail(
      "release benchmarking requires a clean repository whose HEAD, ref, commit, ",
      "and tree exactly match the exported candidate"
    )
  }
  list(head = head, tree = tree)
}

release_root_state <- release_authenticate_root()
release_baseline_evidence <- release_require_local_directory(
  release_arguments$baseline_evidence, "differential baseline evidence"
)
release_candidate_library <- release_require_local_directory(
  release_arguments$candidate_library, "candidate library"
)
release_dependency_libraries <- vapply(
  release_arguments$dependency_libraries,
  release_require_local_directory,
  character(1L),
  label = "dependency library"
)
release_mies_library <- release_require_local_directory(
  release_arguments$mies_library, "miesmuschel library"
)
release_extra_libraries <- vapply(
  release_arguments$protected_libraries,
  release_require_local_directory,
  character(1L),
  label = "additional protected library"
)
release_ordinary_library <- release_require_local_directory(
  file.path(release_root, ".local", "R", "library"),
  "activated ordinary project library"
)
release_base_library <- release_require_local_directory(
  file.path(release_root, ".local", "toolchain", "lib", "R", "library"),
  "repository-local R base library"
)
release_output <- release_output_path(release_arguments$output)

release_expected_candidate_library <- file.path(
  release_root, ".local", "compat", "runs", release_candidate_run_id,
  "library-candidate"
)
if (!identical(release_candidate_library, release_expected_candidate_library)) {
  release_fail(
    "candidate library must be the install-candidate path for ",
    "PARADOX_CANDIDATE_RUN_ID: ", release_expected_candidate_library
  )
}

release_baseline_library <- file.path(release_baseline_evidence, "library-baseline")
if (!dir.exists(release_baseline_library) ||
    release_is_symbolic(release_baseline_library)) {
  release_fail("differential evidence has no plain library-baseline directory")
}
release_baseline_library <- normalizePath(
  release_baseline_library, winslash = "/", mustWork = TRUE
)
if (!identical(
    release_baseline_library,
    file.path(release_baseline_evidence, "library-baseline")
  )) {
  release_fail("differential library-baseline escaped its sealed evidence")
}

release_library_paths <- c(
  release_baseline_library,
  release_candidate_library,
  release_dependency_libraries,
  release_mies_library,
  release_extra_libraries,
  release_ordinary_library,
  release_base_library
)
if (anyDuplicated(release_library_paths)) {
  release_fail("baseline, candidate, dependency, mies, and protected libraries must be distinct")
}
if (length(release_library_paths) > 1L) {
  nested <- vapply(seq_along(release_library_paths), function(index) {
    any(startsWith(
      release_library_paths[-index], paste0(release_library_paths[[index]], "/")
    ))
  }, logical(1L))
  if (any(nested)) release_fail("all protected library roots must be disjoint")
}

release_fingerprint_script <- file.path(release_root, "compat", "fingerprint.R")
release_evidence_script <- file.path(release_root, "compat", "repository-evidence.R")
release_evidence_verifier <- file.path(
  release_root, "compat", "verify-repository-evidence.R"
)
release_installer <- file.path(release_root, "compat", "install-candidate")
release_git_authenticator <- file.path(
  release_root, "compat", "authenticate-candidate-git"
)
for (path in c(
  release_fingerprint_script, release_evidence_script,
  release_evidence_verifier, release_installer, release_git_authenticator
)) {
  release_require_regular_file(path, "release helper")
}
release_require_committed_files(c(
  release_fingerprint_script, release_evidence_script,
  release_evidence_verifier, release_installer, release_git_authenticator
), "release helper")

release_authenticate_candidate_git <- function() {
  output <- release_run_capture(
    release_git_authenticator,
    c(
      release_root, release_candidate_ref, release_candidate_commit,
      release_candidate_tree
    ),
    "authenticating candidate Git state"
  )
  if (!identical(output, "candidate_git_authentication=passed")) {
    release_fail("candidate Git authenticator returned an unexpected result")
  }
  invisible(TRUE)
}
release_authenticate_candidate_git()

release_fingerprint_environment <- new.env(parent = baseenv())
sys.source(release_fingerprint_script, envir = release_fingerprint_environment)
release_tree_fingerprint <- release_fingerprint_environment$compat_tree_content_sha256

release_evidence_environment <- new.env(parent = baseenv())
sys.source(release_evidence_script, envir = release_evidence_environment)

release_read_key_values <- function(path, expected_keys, label) {
  release_require_regular_file(path, label)
  lines <- readLines(path, warn = FALSE)
  separators <- regexpr("=", lines, fixed = TRUE)
  if (length(lines) != length(expected_keys) || any(separators <= 1L)) {
    release_fail(label, " has an unexpected shape")
  }
  keys <- substring(lines, 1L, separators - 1L)
  values <- substring(lines, separators + 1L)
  if (!identical(keys, expected_keys) || anyNA(values) ||
      any(release_contains_control(values))) {
    release_fail(label, " has unexpected keys, order, or values")
  }
  setNames(values, keys)
}

release_differential_files <- list(
  completion = file.path(release_baseline_evidence, "metadata", "completion.tsv"),
  manifest = file.path(
    release_baseline_evidence, "metadata", "evidence-manifest.tsv"
  ),
  seal = file.path(release_baseline_evidence, "metadata", "completion.seal"),
  report = file.path(release_baseline_evidence, "results", "report.rds"),
  report_text = file.path(release_baseline_evidence, "results", "report.txt"),
  harness_manifest = file.path(
    release_baseline_evidence, "harness", "harness-sha256.tsv"
  ),
  fingerprint_helper = file.path(
    release_baseline_evidence, "metadata", "fingerprint.R"
  )
)

release_authenticate_differential <- function() {
  verified <- release_evidence_environment$repository_verify_evidence(
    release_baseline_evidence
  )
  lapply(release_differential_files, release_require_regular_file,
    label = "differential evidence input")
  completion <- release_read_key_values(
    release_differential_files$completion,
    c(
      "harness", "schema", "run_id", "baseline_url", "baseline_ref",
      "baseline_commit", "candidate_commit", "candidate_tree",
      "candidate_revision", "candidate_clean", "shared_library",
      "shared_library_sha256", "fingerprint_helper_sha256",
      "harness_manifest_sha256", "compare_status", "completed_utc"
    ),
    "differential completion metadata"
  )
  shared_library_sha256 <- release_tree_fingerprint(release_ordinary_library)
  if (!identical(completion[["harness"]], "differential") ||
      !identical(completion[["schema"]], "2") ||
      !identical(completion[["compare_status"]], "0") ||
      !identical(completion[["candidate_clean"]], "true") ||
      !identical(completion[["candidate_commit"]], release_candidate_commit) ||
      !identical(completion[["candidate_tree"]], release_candidate_tree) ||
      !identical(completion[["candidate_revision"]], release_candidate_commit) ||
      !identical(completion[["shared_library"]], release_ordinary_library) ||
      !identical(
        completion[["shared_library_sha256"]], shared_library_sha256
      ) ||
      !identical(
        completion[["fingerprint_helper_sha256"]],
        unname(tools::sha256sum(release_fingerprint_script))
      ) ||
      !grepl("^[0-9a-f]{40}$", completion[["baseline_commit"]]) ||
      !identical(
        completion[["harness_manifest_sha256"]],
        unname(tools::sha256sum(release_differential_files$harness_manifest))
      )) {
    release_fail(
      "differential evidence did not pass cleanly for the exported frozen candidate"
    )
  }

  harness <- utils::read.delim(
    release_differential_files$harness_manifest,
    sep = "\t", quote = "", comment.char = "", colClasses = "character",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expected_roles <- c(
    "runner", "cases", "normalizer", "test-normalizer", "capture", "compare",
    "expected-differences"
  )
  expected_files <- c(
    "run", "cases.R", "normalize.R", "test-normalize.R", "capture.R",
    "compare.R", "expected-differences.tsv"
  )
  if (!identical(names(harness), c("role", "file", "sha256", "origin")) ||
      !identical(harness$role, expected_roles) ||
      !identical(harness$file, expected_files) ||
      any(harness$origin != "candidate-snapshot") ||
      any(!grepl("^[0-9a-f]{64}$", harness$sha256))) {
    release_fail("differential evidence does not use the complete default frozen harness")
  }
  current_harness <- file.path(release_root, "compat", "differential", expected_files)
  invisible(lapply(
    current_harness, release_require_regular_file,
    label = "current differential harness"
  ))
  release_require_committed_files(
    current_harness, "current differential harness"
  )
  current_harness_sha <- unname(tools::sha256sum(current_harness))
  if (!identical(current_harness_sha, harness$sha256)) {
    release_fail("differential evidence harness differs from the frozen candidate harness")
  }
  retained_helpers <- file.path(
    release_baseline_evidence, "metadata",
    c("repository-evidence.R", "verify-repository-evidence.R", "fingerprint.R")
  )
  current_helpers <- c(
    release_evidence_script, release_evidence_verifier, release_fingerprint_script
  )
  invisible(lapply(
    retained_helpers, release_require_regular_file,
    label = "retained differential evidence helper"
  ))
  if (!identical(
      unname(tools::sha256sum(retained_helpers)),
      unname(tools::sha256sum(current_helpers))
    ) || !identical(
      unname(tools::sha256sum(release_differential_files$fingerprint_helper)),
      completion[["fingerprint_helper_sha256"]]
    )) {
    release_fail("differential evidence was sealed with different evidence helpers")
  }

  report <- readRDS(release_differential_files$report)
  if (!is.list(report) || !identical(report$format_version, 2L) ||
      length(report$unexpected_differences) ||
      length(report$missing_expected_differences) ||
      length(report$expected_difference_fingerprint_mismatches) ||
      !length(report$equal) ||
      !identical(
        report$baseline_metadata$selected_cases,
        report$baseline_metadata$available_cases
      ) ||
      !identical(
        report$candidate_metadata$selected_cases,
        report$candidate_metadata$available_cases
      ) ||
      !identical(
        report$baseline_metadata$revision, completion[["baseline_commit"]]
      ) ||
      !identical(
        report$candidate_metadata$revision, release_candidate_commit
      )) {
    release_fail("differential report is not a passed full-inventory comparison")
  }
  report_lines <- readLines(release_differential_files$report_text, warn = FALSE)
  if (!length(report_lines) ||
      !identical(tail(report_lines, 1L), "compatibility gate: PASS")) {
    release_fail("differential text report does not record a passing comparison")
  }
  list(verified = verified, completion = completion, report = report)
}

release_differential <- release_authenticate_differential()
release_direct_package <- function(package, library, label) {
  path <- normalizePath(
    find.package(package, lib.loc = library), winslash = "/", mustWork = TRUE
  )
  if (!identical(dirname(path), library)) {
    release_fail(package, " did not resolve directly from ", label)
  }
  path
}
release_baseline_package <- release_direct_package(
  "paradox", release_baseline_library, "differential library-baseline"
)
release_baseline_version <- as.character(utils::packageVersion(
  "paradox", lib.loc = release_baseline_library
))
release_baseline_content <- release_tree_fingerprint(release_baseline_package)
if (!identical(
    release_baseline_version,
    release_differential$report$baseline_metadata$package_version
  )) {
  release_fail("library-baseline version differs from its sealed differential capture")
}
release_mies_package <- release_direct_package(
  "miesmuschel", release_mies_library, "the required miesmuschel library"
)
release_pipeline_package <- release_direct_package(
  "mlr3pipelines", release_dependency_libraries[[1L]],
  "the first dependency library"
)

release_candidate_provenance <- file.path(
  release_candidate_library, ".paradox-candidate-provenance.tsv"
)
release_candidate_provenance_seal <- file.path(
  release_candidate_library, ".paradox-candidate-provenance.sha256"
)
release_candidate_sentinel <- file.path(
  release_candidate_library, ".paradox-candidate-content-sha256"
)

release_authenticate_candidate <- function() {
  release_authenticate_candidate_git()
  for (path in c(
    release_candidate_provenance, release_candidate_provenance_seal,
    release_candidate_sentinel
  )) {
    release_require_regular_file(path, "candidate provenance input")
  }
  sentinel <- readLines(release_candidate_sentinel, warn = FALSE)
  if (!identical(sentinel, release_candidate_content)) {
    release_fail("candidate content sentinel differs from the exported fingerprint")
  }
  provenance <- utils::read.delim(
    release_candidate_provenance,
    sep = "\t", quote = "", comment.char = "", colClasses = "character",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expected_keys <- c(
    "schema", "candidate_run_id", "candidate_ref", "candidate_commit",
    "candidate_tree", "candidate_version", "candidate_library",
    "dependency_library", "dependency_library_content_sha256",
    "source_archive_sha256", "candidate_content_sha256", "installer_sha256",
    "git_authenticator_sha256"
  )
  if (!identical(names(provenance), c("key", "value")) ||
      !identical(provenance$key, expected_keys) || anyNA(provenance$value) ||
      any(!nzchar(provenance$value))) {
    release_fail("candidate provenance receipt has an unexpected schema")
  }
  values <- setNames(provenance$value, provenance$key)
  seal <- readLines(release_candidate_provenance_seal, warn = FALSE)
  expected_seal <- paste0(
    unname(tools::sha256sum(release_candidate_provenance)),
    "  .paradox-candidate-provenance.tsv"
  )
  if (!identical(seal, expected_seal) ||
      !identical(values[["schema"]], "2") ||
      !identical(values[["candidate_run_id"]], release_candidate_run_id) ||
      !identical(values[["candidate_ref"]], release_candidate_ref) ||
      !identical(values[["candidate_commit"]], release_candidate_commit) ||
      !identical(values[["candidate_tree"]], release_candidate_tree) ||
      !identical(values[["candidate_library"]], release_candidate_library) ||
      !identical(
        values[["dependency_library"]], release_dependency_libraries[[1L]]
      ) ||
      !identical(
        values[["dependency_library_content_sha256"]],
        release_tree_fingerprint(release_dependency_libraries[[1L]])
      ) ||
      !identical(
        values[["candidate_content_sha256"]], release_candidate_content
      ) ||
      !identical(
        values[["installer_sha256"]], unname(tools::sha256sum(release_installer))
      ) ||
      !identical(
        values[["git_authenticator_sha256"]],
        unname(tools::sha256sum(release_git_authenticator))
      ) ||
      !grepl("^[0-9a-f]{64}$", values[["source_archive_sha256"]])) {
    release_fail("candidate provenance is unsealed or differs from current inputs")
  }

  candidate_package <- normalizePath(
    find.package("paradox", lib.loc = release_candidate_library),
    winslash = "/", mustWork = TRUE
  )
  if (!identical(dirname(candidate_package), release_candidate_library)) {
    release_fail("paradox did not resolve directly from the candidate library")
  }
  expected_entries <- sort(c(
    ".paradox-candidate-content-sha256",
    ".paradox-candidate-provenance.sha256",
    ".paradox-candidate-provenance.tsv",
    "paradox"
  ))
  observed_entries <- sort(list.files(
    release_candidate_library, all.files = TRUE, no.. = TRUE
  ))
  if (!identical(observed_entries, expected_entries)) {
    release_fail(
      "candidate library has an unexpected top-level inventory: ",
      paste(observed_entries, collapse = ", ")
    )
  }
  package_content <- release_tree_fingerprint(candidate_package)
  package_version <- as.character(utils::packageVersion(
    "paradox", lib.loc = release_candidate_library
  ))
  if (!identical(package_content, release_candidate_content) ||
      !identical(package_version, values[["candidate_version"]])) {
    release_fail("installed candidate package content or version differs from provenance")
  }

  resolved_commit <- release_git_one(
    c("-C", release_root, "rev-parse", paste0(release_candidate_commit, "^{commit}")),
    "reproducing candidate commit"
  )
  resolved_tree <- release_git_one(
    c("-C", release_root, "rev-parse", paste0(release_candidate_commit, "^{tree}")),
    "reproducing candidate tree"
  )
  resolved_ref <- release_git_one(
    c("-C", release_root, "rev-parse", paste0(release_candidate_ref, "^{commit}")),
    "reproducing candidate ref"
  )
  if (!identical(resolved_commit, release_candidate_commit) ||
      !identical(resolved_tree, release_candidate_tree) ||
      !identical(resolved_ref, release_candidate_commit)) {
    release_fail("candidate ref, commit, and tree no longer resolve to provenance")
  }
  archive <- tempfile(
    "paradox-benchmark-candidate-", tmpdir = file.path(release_root, ".local", "tmp"),
    fileext = ".tar"
  )
  on.exit(unlink(archive), add = TRUE)
  invisible(release_run_capture(
    release_git,
    c(
      "--no-replace-objects", "-c", "core.attributesFile=/dev/null",
      "-c", "tar.umask=0002",
      "-C", release_root, "archive",
      "--format=tar", "-o", archive, release_candidate_commit
    ),
    "reproducing the candidate source archive",
    environment = c(
      "GIT_ATTR_NOSYSTEM=1", "GIT_CONFIG_GLOBAL=/dev/null",
      "GIT_CONFIG_SYSTEM=/dev/null", "GIT_CONFIG_NOSYSTEM=1",
      "GIT_NO_REPLACE_OBJECTS=1"
    )
  ))
  if (!file.exists(archive) ||
      !identical(
        unname(tools::sha256sum(archive)), values[["source_archive_sha256"]]
      )) {
    release_fail("candidate source archive is not reproducible from the local commit")
  }
  list(
    package = candidate_package,
    package_version = package_version,
    package_content = package_content,
    provenance = values
  )
}

release_candidate <- release_authenticate_candidate()

release_library_roles <- c(
  "baseline", "candidate",
  sprintf("dependency-%d", seq_along(release_dependency_libraries)),
  "miesmuschel",
  sprintf("protected-%d", seq_along(release_extra_libraries)),
  "ordinary-project", "r-base-library"
)
release_fingerprint_libraries <- function() {
  data.frame(
    role = release_library_roles,
    path = release_library_paths,
    content_sha256 = unname(vapply(
      release_library_paths, release_tree_fingerprint, character(1L)
    )),
    stringsAsFactors = FALSE
  )
}
release_library_before <- release_fingerprint_libraries()

release_helper_inputs <- c(
  release = file.path(release_root, "benchmarks", "release"),
  release_R = release_script,
  paired_runner = file.path(release_root, "benchmarks", "run"),
  paired_driver = file.path(release_root, "benchmarks", "run.R"),
  paired_worker = file.path(release_root, "benchmarks", "worker.R"),
  workloads = file.path(release_root, "benchmarks", "workloads.R"),
  regression_policy = file.path(
    release_root, "benchmarks", "regression-policy.R"
  ),
  regression_policy_table = file.path(
    release_root, "benchmarks", "regression-policy.tsv"
  ),
  focused_consumer = file.path(
    release_root, "benchmarks", "paramsetcollection-consumers.R"
  ),
  fingerprint = release_fingerprint_script,
  evidence = release_evidence_script,
  evidence_verifier = release_evidence_verifier,
  candidate_installer = release_installer,
  candidate_git_authenticator = release_git_authenticator
)
release_helper_target_names <- c(
  "release", "release.R", "run", "run.R", "worker.R", "workloads.R",
  "regression-policy.R", "regression-policy.tsv",
  "paramsetcollection-consumers.R", "fingerprint.R", "repository-evidence.R",
  "verify-repository-evidence.R", "install-candidate",
  "authenticate-candidate-git"
)
if (length(release_helper_target_names) != length(release_helper_inputs) ||
    anyDuplicated(release_helper_target_names)) {
  release_fail(
    "benchmark helper input and retained-target inventories are inconsistent"
  )
}
invisible(lapply(
  release_helper_inputs, release_require_regular_file,
  label = "benchmark release helper"
))
release_require_committed_files(
  release_helper_inputs, "benchmark release helper"
)
release_helper_sha256 <- unname(tools::sha256sum(release_helper_inputs))

release_workload_environment <- new.env(parent = baseenv())
sys.source(
  release_helper_inputs[["workloads"]], envir = release_workload_environment
)
release_all_workloads <- release_workload_environment$benchmark_workload_names()
release_policy_environment <- new.env(parent = baseenv())
sys.source(
  release_helper_inputs[["regression_policy"]],
  envir = release_policy_environment
)
release_policy_spec <- release_policy_environment$benchmark_regression_policy_spec()
release_policy_environment$benchmark_regression_validate_spec(release_policy_spec)
if (release_arguments$iterations < release_policy_spec$minimum_samples) {
  release_fail(
    "--iterations must be >= ", release_policy_spec$minimum_samples,
    " for distribution-aware release decisions"
  )
}
release_regression_policy <-
  release_policy_environment$benchmark_regression_read_policy(
    release_helper_inputs[["regression_policy_table"]], release_all_workloads
  )
release_policy_inputs <- release_helper_inputs[c(
  "regression_policy", "regression_policy_table"
)]
release_policy_sha256 <- setNames(
  unname(tools::sha256sum(release_policy_inputs)),
  names(release_policy_inputs)
)
release_policy_manifest_lines <- c(
  "role\tfile\tsha256",
  paste(
    names(release_policy_inputs), basename(release_policy_inputs),
    release_policy_sha256, sep = "\t"
  )
)
release_policy_manifest_file <- tempfile(
  "paradox-benchmark-policy-", tmpdir = file.path(release_root, ".local", "tmp")
)
writeLines(release_policy_manifest_lines, release_policy_manifest_file, useBytes = TRUE)
release_policy_manifest_sha256 <- unname(
  tools::sha256sum(release_policy_manifest_file)
)
unlink(release_policy_manifest_file)

release_support_libraries <- c(
  release_dependency_libraries, release_mies_library,
  release_extra_libraries, release_ordinary_library
)
release_paired_output <- file.path(release_output, "paired")
release_consumer_directory <- file.path(release_output, "consumers")
release_consumer_baseline <- file.path(release_consumer_directory, "baseline.csv")
release_consumer_candidate <- file.path(release_consumer_directory, "candidate.csv")
release_consumer_baseline_samples <- file.path(
  release_consumer_directory, "baseline-samples.csv"
)
release_consumer_candidate_samples <- file.path(
  release_consumer_directory, "candidate-samples.csv"
)
release_paired_arguments <- c(
  "--no-save", "--no-restore", "--no-site-file", "--no-init-file",
  release_helper_inputs[["paired_driver"]],
  "--baseline-library", release_baseline_library,
  "--candidate-library", release_candidate_library,
  unlist(lapply(release_support_libraries, function(path) {
    c("--dependency-library", path)
  }), use.names = FALSE),
  "--baseline-ref", release_differential$completion[["baseline_commit"]],
  "--candidate-ref", release_candidate_commit,
  "--output", release_paired_output,
  "--params", as.character(release_arguments$n_params),
  "--rows", as.character(release_arguments$n_rows),
  "--iterations", as.character(release_arguments$iterations),
  "--warmups", as.character(release_arguments$warmups),
  "--seed", as.character(release_arguments$seed)
)
release_focus_arguments <- function(label, library, output, samples_output) {
  c(
    "--vanilla", release_helper_inputs[["focused_consumer"]], label, library,
    output, release_mies_library, release_dependency_libraries[[1L]],
    samples_output
  )
}
release_child_environment <- c(
  paste0("R_LIBS=", paste(release_support_libraries, collapse = .Platform$path.sep)),
  paste0(
    "R_LIBS_USER=", paste(release_support_libraries, collapse = .Platform$path.sep)
  ),
  "R_LIBS_SITE=",
  "R_ENVIRON=", "R_ENVIRON_USER=", "R_PROFILE=", "R_PROFILE_USER=",
  "R_TESTS=", "NOT_CRAN=true", "R_PARALLEL_PORT=random"
)

release_quote_command <- function(command, arguments) {
  paste(c(shQuote(command), vapply(arguments, shQuote, character(1L))), collapse = " ")
}
release_planned_commands <- list(
  paired = list(
    command = release_expected_rscript,
    arguments = release_paired_arguments,
    environment = release_child_environment
  ),
  consumer_baseline = list(
    command = release_expected_rscript,
    arguments = release_focus_arguments(
      "baseline", release_baseline_library, release_consumer_baseline,
      release_consumer_baseline_samples
    ),
    environment = release_child_environment
  ),
  consumer_candidate = list(
    command = release_expected_rscript,
    arguments = release_focus_arguments(
      "candidate", release_candidate_library, release_consumer_candidate,
      release_consumer_candidate_samples
    ),
    environment = release_child_environment
  )
)

if (release_arguments$plan_only) {
  cat("Release benchmark inputs authenticated; no output was written.\n")
  cat("output=", release_output, "\n", sep = "")
  cat("baseline_evidence_manifest_sha256=",
      release_differential$verified$manifest_sha256, "\n", sep = "")
  cat("candidate_content_sha256=", release_candidate_content, "\n", sep = "")
  cat(
    "regression_policy_manifest_sha256=", release_policy_manifest_sha256,
    "\n", sep = ""
  )
  cat("regression_policy_rows=", nrow(release_regression_policy), "\n", sep = "")
  for (name in names(release_planned_commands)) {
    cat(name, "=", release_quote_command(
      release_planned_commands[[name]]$command,
      release_planned_commands[[name]]$arguments
    ), "\n", sep = "")
  }
  quit(save = "no", status = 0L)
}

if (!dir.create(release_output, recursive = FALSE, showWarnings = FALSE)) {
  release_fail("could not reserve new benchmark output: ", release_output)
}
release_output <- normalizePath(release_output, winslash = "/", mustWork = TRUE)
for (child in c("metadata", "helpers", "provenance", "logs", "consumers")) {
  path <- file.path(release_output, child)
  if (!dir.create(path, recursive = FALSE, showWarnings = FALSE)) {
    release_fail("could not create benchmark evidence child: ", path)
  }
}

release_write_tsv <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(path) || file.exists(temporary)) {
    release_fail("refusing to overwrite benchmark evidence file: ", path)
  }
  on.exit(unlink(temporary), add = TRUE)
  utils::write.table(
    value, temporary, quote = FALSE, sep = "\t", row.names = FALSE,
    na = "-", fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) release_fail("could not write ", path)
  invisible(path)
}

release_write_lines <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(path) || file.exists(temporary)) {
    release_fail("refusing to overwrite benchmark evidence file: ", path)
  }
  on.exit(unlink(temporary), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) release_fail("could not write ", path)
  invisible(path)
}

release_log <- file.path(release_output, "logs", "release.log")
release_log_line <- function(text) {
  line <- paste0(format(Sys.time(), tz = "UTC", usetz = TRUE), " ", text)
  cat(line, "\n", sep = "")
  cat(line, "\n", sep = "", file = release_log, append = TRUE)
}
release_started <- format(Sys.time(), tz = "UTC", usetz = TRUE)
release_log_line("authenticated frozen release inputs")

release_helper_targets <- file.path(
  release_output, "helpers", release_helper_target_names
)
if (!all(file.copy(
    release_helper_inputs, release_helper_targets,
    overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE
  ))) {
  release_fail("could not retain benchmark release helper copies")
}
if (!identical(
    unname(tools::sha256sum(release_helper_targets)), release_helper_sha256
  )) {
  release_fail("retained benchmark release helpers differ from their inputs")
}
release_write_tsv(data.frame(
  role = names(release_helper_inputs),
  source = unname(release_helper_inputs),
  retained = substring(
    release_helper_targets, nchar(release_output, type = "chars") + 2L
  ),
  sha256 = release_helper_sha256,
  stringsAsFactors = FALSE
), file.path(release_output, "metadata", "helpers.tsv"))
release_write_tsv(data.frame(
  role = names(release_policy_inputs),
  source = unname(release_policy_inputs),
  retained = substring(
    release_helper_targets[match(
      names(release_policy_inputs), names(release_helper_inputs)
    )],
    nchar(release_output, type = "chars") + 2L
  ),
  sha256 = release_policy_sha256,
  policy_manifest_sha256 = rep(
    release_policy_manifest_sha256, length(release_policy_inputs)
  ),
  stringsAsFactors = FALSE
), file.path(release_output, "metadata", "regression-policy-inputs.tsv"))
release_write_lines(
  release_policy_manifest_lines,
  file.path(release_output, "metadata", "regression-policy-manifest.tsv")
)
if (!identical(
    unname(tools::sha256sum(file.path(
      release_output, "metadata", "regression-policy-manifest.tsv"
    ))),
    release_policy_manifest_sha256
  )) {
  release_fail("retained regression policy manifest differs from its input hash")
}

release_provenance_sources <- c(
  differential_completion = release_differential_files$completion,
  differential_report_rds = release_differential_files$report,
  differential_report = release_differential_files$report_text,
  differential_harness_manifest = release_differential_files$harness_manifest,
  differential_fingerprint_helper = release_differential_files$fingerprint_helper,
  differential_manifest = release_differential_files$manifest,
  differential_seal = release_differential_files$seal,
  candidate_provenance = release_candidate_provenance,
  candidate_provenance_seal = release_candidate_provenance_seal,
  candidate_content = release_candidate_sentinel
)
release_provenance_targets <- file.path(
  release_output, "provenance",
  c(
    "differential-completion.tsv", "differential-report.rds",
    "differential-report.txt", "differential-harness-sha256.tsv",
    "differential-fingerprint.R",
    "differential-evidence-manifest.tsv", "differential-completion.seal",
    ".paradox-candidate-provenance.tsv",
    ".paradox-candidate-provenance.sha256",
    ".paradox-candidate-content-sha256"
  )
)
if (!all(file.copy(
    release_provenance_sources, release_provenance_targets,
    overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE
  ))) {
  release_fail("could not retain benchmark provenance inputs")
}
release_provenance_sha <- unname(tools::sha256sum(release_provenance_sources))
if (!identical(
    unname(tools::sha256sum(release_provenance_targets)), release_provenance_sha
  )) {
  release_fail("retained benchmark provenance differs from its inputs")
}
release_write_tsv(data.frame(
  role = names(release_provenance_sources),
  source = unname(release_provenance_sources),
  retained = substring(
    release_provenance_targets, nchar(release_output, type = "chars") + 2L
  ),
  sha256 = release_provenance_sha,
  stringsAsFactors = FALSE
), file.path(release_output, "metadata", "provenance-inputs.tsv"))
release_write_tsv(
  release_library_before,
  file.path(release_output, "metadata", "libraries-before.tsv")
)
release_write_tsv(data.frame(
  role = c("baseline", "candidate", "miesmuschel", "mlr3pipelines"),
  version = c(
    release_baseline_version,
    release_candidate$package_version,
    as.character(utils::packageVersion("miesmuschel", lib.loc = release_mies_library)),
    as.character(utils::packageVersion(
      "mlr3pipelines", lib.loc = release_dependency_libraries[[1L]]
    ))
  ),
  package_path = c(
    release_baseline_package, release_candidate$package,
    release_mies_package, release_pipeline_package
  ),
  package_content_sha256 = c(
    release_baseline_content, release_candidate$package_content,
    release_tree_fingerprint(release_mies_package),
    release_tree_fingerprint(release_pipeline_package)
  ),
  stringsAsFactors = FALSE
), file.path(release_output, "metadata", "package-provenance.tsv"))

release_command_rows <- do.call(rbind, lapply(names(release_planned_commands), function(id) {
  command <- release_planned_commands[[id]]
  rbind(
    data.frame(
      command_id = id, record = "command", position = "0", key = "-",
      value = command$command, stringsAsFactors = FALSE
    ),
    data.frame(
      command_id = id, record = "argument",
      position = as.character(seq_along(command$arguments)), key = "-",
      value = command$arguments, stringsAsFactors = FALSE
    ),
    data.frame(
      command_id = id, record = "environment",
      position = as.character(seq_along(command$environment)),
      key = sub("=.*$", "", command$environment),
      value = sub("^[^=]*=", "", command$environment),
      stringsAsFactors = FALSE
    )
  )
}))
release_write_tsv(
  release_command_rows,
  file.path(release_output, "metadata", "commands.tsv")
)

release_run_logged <- function(id, specification) {
  stdout_path <- file.path(release_output, "logs", paste0(id, ".stdout.log"))
  stderr_path <- file.path(release_output, "logs", paste0(id, ".stderr.log"))
  release_log_line(paste0("starting ", id))
  status <- suppressWarnings(system2(
    specification$command,
    args = vapply(specification$arguments, shQuote, character(1L)),
    stdout = stdout_path,
    stderr = stderr_path,
    env = specification$environment
  ))
  status <- as.integer(status %||% 0L)
  if (file.exists(stdout_path)) cat(readLines(stdout_path, warn = FALSE), sep = "\n")
  if (file.exists(stderr_path)) {
    cat(readLines(stderr_path, warn = FALSE), sep = "\n", file = stderr())
  }
  release_log_line(paste0("completed ", id, " with status ", status))
  status
}

release_status <- setNames(rep(NA_integer_, length(release_planned_commands)),
  names(release_planned_commands))
release_status[["paired"]] <- release_run_logged(
  "paired", release_planned_commands[["paired"]]
)
if (identical(release_status[["paired"]], 0L)) {
  release_status[["consumer_baseline"]] <- release_run_logged(
    "consumer_baseline", release_planned_commands[["consumer_baseline"]]
  )
}
if (identical(
    unname(release_status[c("paired", "consumer_baseline")]), c(0L, 0L)
  )) {
  release_status[["consumer_candidate"]] <- release_run_logged(
    "consumer_candidate", release_planned_commands[["consumer_candidate"]]
  )
}
release_write_tsv(data.frame(
  command_id = names(release_status),
  status = ifelse(is.na(release_status), "not-run", as.character(release_status)),
  stringsAsFactors = FALSE
), file.path(release_output, "metadata", "command-status.tsv"))

release_validation_error <- NULL
release_workload_count <- NA_integer_
release_consumer_rows <- NA_integer_
release_regression_summary <- list(
  row_count = NA_integer_, pass_count = NA_integer_,
  marginal_count = NA_integer_, fail_count = NA_integer_,
  worst_median_case = "-", worst_median_ratio = NA_real_,
  worst_median_budget_fraction = NA_real_,
  worst_q75_case = "-", worst_q75_ratio = NA_real_,
  worst_q75_budget_fraction = NA_real_,
  worst_allocation_case = "-", worst_allocation_ratio = NA_real_,
  worst_allocation_delta_bytes = NA_real_,
  worst_allocation_budget_fraction = NA_real_
)
if (all(!is.na(release_status) & release_status == 0L)) {
  release_validation_error <- tryCatch({
    all_workloads <- release_all_workloads
    summary_baseline <- utils::read.csv(
      file.path(release_paired_output, "summary-baseline.csv"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    summary_candidate <- utils::read.csv(
      file.path(release_paired_output, "summary-candidate.csv"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    comparison <- utils::read.csv(
      file.path(release_paired_output, "comparison.csv"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    if (!identical(summary_baseline$workload, all_workloads) ||
        !identical(summary_candidate$workload, all_workloads) ||
        !identical(comparison$workload, all_workloads)) {
      release_fail("paired runner did not retain the complete ordered workload inventory")
    }
    paired_samples_baseline <- utils::read.csv(
      file.path(release_paired_output, "samples-baseline.csv"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    paired_samples_candidate <- utils::read.csv(
      file.path(release_paired_output, "samples-candidate.csv"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    paired_sample_columns <- c(
      "workload", "iteration", "elapsed_seconds", "elapsed_ns",
      "gc_level0", "gc_level1", "gc_level2"
    )
    paired_counts <- function(value) {
      unname(table(factor(value$workload, levels = all_workloads)))
    }
    summaries_match_samples <- function(
      summary, summary_keys, samples, sample_keys
    ) {
      if (!all(c(
          "iterations", "min_ns", "q25_ns", "median_ns", "q75_ns", "max_ns"
        ) %in% names(summary))) {
        return(FALSE)
      }
      for (index in seq_len(nrow(summary))) {
        elapsed <- samples$elapsed_ns[sample_keys == summary_keys[[index]]]
        expected <- unname(stats::quantile(
          elapsed, c(0, 0.25, 0.5, 0.75, 1), type = 8
        ))
        observed <- as.numeric(unlist(summary[index, c(
          "min_ns", "q25_ns", "median_ns", "q75_ns", "max_ns"
        )], use.names = FALSE))
        if (length(elapsed) != summary$iterations[[index]] ||
            !isTRUE(all.equal(
              observed, expected, tolerance = 1e-10,
              check.attributes = FALSE
            ))) {
          return(FALSE)
        }
      }
      TRUE
    }
    if (!identical(names(paired_samples_baseline), paired_sample_columns) ||
        !identical(names(paired_samples_candidate), paired_sample_columns) ||
        !identical(unique(paired_samples_baseline$workload), all_workloads) ||
        !identical(unique(paired_samples_candidate$workload), all_workloads) ||
        any(paired_counts(paired_samples_baseline) != release_arguments$iterations) ||
        any(paired_counts(paired_samples_candidate) != release_arguments$iterations) ||
        !summaries_match_samples(
          summary_baseline, summary_baseline$workload,
          paired_samples_baseline, paired_samples_baseline$workload
        ) || !summaries_match_samples(
          summary_candidate, summary_candidate$workload,
          paired_samples_candidate, paired_samples_candidate$workload
        )) {
      release_fail("paired samples do not match the complete requested inventory")
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      release_fail("jsonlite is unavailable for paired metadata validation")
    }
    paired_metadata <- jsonlite::read_json(
      file.path(release_paired_output, "metadata.json"), simplifyVector = TRUE
    )
    if (!identical(unname(paired_metadata$arguments$workloads), all_workloads) ||
        !identical(
          paired_metadata$arguments$baseline_library, release_baseline_library
        ) ||
        !identical(
          paired_metadata$arguments$candidate_library, release_candidate_library
        ) ||
        !identical(
          unname(paired_metadata$arguments$dependency_libraries),
          release_support_libraries
        ) ||
        !identical(
          paired_metadata$arguments$baseline_ref,
          release_differential$completion[["baseline_commit"]]
        ) ||
        !identical(
          paired_metadata$arguments$candidate_ref, release_candidate_commit
        ) ||
        !identical(paired_metadata$repository$head, release_candidate_commit) ||
        !identical(paired_metadata$repository$dirty, FALSE) ||
        !identical(
          as.integer(paired_metadata$arguments$n_params),
          release_arguments$n_params
        ) ||
        !identical(
          as.integer(paired_metadata$arguments$n_rows), release_arguments$n_rows
        ) ||
        !identical(
          as.integer(paired_metadata$arguments$iterations),
          release_arguments$iterations
        ) ||
        !identical(
          as.integer(paired_metadata$arguments$warmups), release_arguments$warmups
        ) ||
        !identical(
          as.integer(paired_metadata$arguments$seed), release_arguments$seed
        )) {
      release_fail("paired metadata differs from the authenticated release inputs")
    }
    release_workload_count <- length(all_workloads)

    consumer_baseline <- utils::read.csv(
      release_consumer_baseline, stringsAsFactors = FALSE, check.names = FALSE
    )
    consumer_candidate <- utils::read.csv(
      release_consumer_candidate, stringsAsFactors = FALSE, check.names = FALSE
    )
    consumer_samples_baseline <- utils::read.csv(
      release_consumer_baseline_samples,
      stringsAsFactors = FALSE, check.names = FALSE
    )
    consumer_samples_candidate <- utils::read.csv(
      release_consumer_candidate_samples,
      stringsAsFactors = FALSE, check.names = FALSE
    )
    key_columns <- c(
      "consumer_case", "operation", "n_sets", "n_params", "iterations"
    )
    expected_cases <- rep(c(
      "mies_mutator_maybe", "mies_optimizer", "mlr3pipelines_graph"
    ), each = 3L)
    expected_operations <- rep(c(
      "params", "values", "get_values_unchecked"
    ), times = 3L)
    expected_consumer_summary_columns <- c(
      "label", "consumer_case", "operation", "n_sets", "n_params",
      "iterations", "min_ns", "q25_ns", "median_ns", "q75_ns",
      "max_ns", "mem_alloc_bytes"
    )
    expected_consumer_sample_columns <- c(
      "label", "consumer_case", "operation", "iteration", "elapsed_ns"
    )
    expected_sample_cases <- rep(expected_cases, each = 100L)
    expected_sample_operations <- rep(expected_operations, each = 100L)
    if (!identical(names(consumer_baseline), names(consumer_candidate)) ||
        !identical(names(consumer_baseline), expected_consumer_summary_columns) ||
        !identical(consumer_baseline$label, rep("baseline", 9L)) ||
        !identical(consumer_candidate$label, rep("candidate", 9L)) ||
        !identical(consumer_baseline$consumer_case, expected_cases) ||
        !identical(consumer_candidate$consumer_case, expected_cases) ||
        !identical(consumer_baseline$operation, expected_operations) ||
        !identical(consumer_candidate$operation, expected_operations) ||
        !identical(consumer_baseline[key_columns], consumer_candidate[key_columns]) ||
        any(!is.finite(consumer_baseline$median_ns)) ||
        any(!is.finite(consumer_candidate$median_ns)) ||
        any(consumer_baseline$median_ns <= 0) ||
        any(consumer_candidate$median_ns <= 0) ||
        any(!is.finite(consumer_baseline$mem_alloc_bytes)) ||
        any(!is.finite(consumer_candidate$mem_alloc_bytes)) ||
        any(consumer_baseline$mem_alloc_bytes < 0) ||
        any(consumer_candidate$mem_alloc_bytes < 0) ||
        any(consumer_baseline$iterations != 100L) ||
        !identical(names(consumer_samples_baseline),
          expected_consumer_sample_columns) ||
        !identical(names(consumer_samples_candidate),
          expected_consumer_sample_columns) ||
        !identical(consumer_samples_baseline$label, rep("baseline", 900L)) ||
        !identical(consumer_samples_candidate$label, rep("candidate", 900L)) ||
        !identical(consumer_samples_baseline$consumer_case,
          expected_sample_cases) ||
        !identical(consumer_samples_candidate$consumer_case,
          expected_sample_cases) ||
        !identical(consumer_samples_baseline$operation,
          expected_sample_operations) ||
        !identical(consumer_samples_candidate$operation,
          expected_sample_operations) ||
        !summaries_match_samples(
          consumer_baseline,
          paste(consumer_baseline$consumer_case, consumer_baseline$operation),
          consumer_samples_baseline,
          paste(
            consumer_samples_baseline$consumer_case,
            consumer_samples_baseline$operation
          )
        ) || !summaries_match_samples(
          consumer_candidate,
          paste(consumer_candidate$consumer_case, consumer_candidate$operation),
          consumer_samples_candidate,
          paste(
            consumer_samples_candidate$consumer_case,
            consumer_samples_candidate$operation
          )
        )) {
      release_fail("focused consumer outputs have unexpected shape or inputs")
    }
    safe_ratio <- function(numerator, denominator) {
      result <- numerator / denominator
      result[numerator == 0 & denominator == 0] <- 1
      result
    }
    consumer_comparison <- data.frame(
      consumer_baseline[key_columns],
      baseline_median_ns = consumer_baseline$median_ns,
      candidate_median_ns = consumer_candidate$median_ns,
      speedup = safe_ratio(
        consumer_baseline$median_ns, consumer_candidate$median_ns
      ),
      baseline_mem_alloc_bytes = consumer_baseline$mem_alloc_bytes,
      candidate_mem_alloc_bytes = consumer_candidate$mem_alloc_bytes,
      allocation_ratio = safe_ratio(
        consumer_baseline$mem_alloc_bytes,
        consumer_candidate$mem_alloc_bytes
      ),
      stringsAsFactors = FALSE
    )
    utils::write.csv(
      consumer_comparison,
      file.path(release_consumer_directory, "comparison.csv"),
      row.names = FALSE, na = ""
    )
    release_consumer_rows <- nrow(consumer_comparison)

    policy_samples <- function(paired, consumer) {
      rbind(
        data.frame(
          scope = rep("paired", nrow(paired)),
          case = paired$workload,
          operation = rep("-", nrow(paired)),
          iteration = as.integer(paired$iteration),
          elapsed_ns = paired$elapsed_ns,
          stringsAsFactors = FALSE
        ),
        data.frame(
          scope = rep("consumer", nrow(consumer)),
          case = consumer$consumer_case,
          operation = consumer$operation,
          iteration = as.integer(consumer$iteration),
          elapsed_ns = consumer$elapsed_ns,
          stringsAsFactors = FALSE
        )
      )
    }
    policy_allocations <- function(paired, consumer) {
      rbind(
        data.frame(
          scope = rep("paired", nrow(paired)),
          case = paired$workload,
          operation = rep("-", nrow(paired)),
          mem_alloc_bytes = paired$mem_alloc_bytes,
          stringsAsFactors = FALSE
        ),
        data.frame(
          scope = rep("consumer", nrow(consumer)),
          case = consumer$consumer_case,
          operation = consumer$operation,
          mem_alloc_bytes = consumer$mem_alloc_bytes,
          stringsAsFactors = FALSE
        )
      )
    }
    regression_ledger <- release_policy_environment$benchmark_regression_evaluate(
      release_regression_policy,
      policy_samples(paired_samples_baseline, consumer_samples_baseline),
      policy_samples(paired_samples_candidate, consumer_samples_candidate),
      policy_allocations(summary_baseline, consumer_baseline),
      policy_allocations(summary_candidate, consumer_candidate)
    )
    release_write_tsv(
      regression_ledger,
      file.path(release_output, "metadata", "regression-decisions.tsv")
    )
    release_regression_summary <-
      release_policy_environment$benchmark_regression_summarize(
        regression_ledger
      )
    release_log_line(paste0(
      "regression decisions: pass=", release_regression_summary$pass_count,
      ", marginal=", release_regression_summary$marginal_count,
      ", fail=", release_regression_summary$fail_count
    ))
    review_rows <- regression_ledger$decision != "pass"
    if (any(review_rows)) {
      diagnostics <- regression_ledger[review_rows, c(
        "scope", "case", "operation", "tier", "decision", "reasons",
        "median_ratio", "median_ratio_ci_low", "median_ratio_limit",
        "median_budget_fraction",
        "q75_ratio", "q75_ratio_ci_low", "q75_ratio_limit",
        "q75_budget_fraction", "allocation_ratio", "allocation_delta_bytes",
        "allocation_budget_fraction"
      )]
      cat("\nRegression-policy review rows:\n")
      print(diagnostics, row.names = FALSE, digits = 4)
    }
    if (release_regression_summary$fail_count > 0L) {
      failed <- regression_ledger[regression_ledger$decision == "fail", ]
      failed_keys <- ifelse(
        failed$scope == "paired", failed$case,
        paste(failed$case, failed$operation, sep = "/")
      )
      release_fail(
        "material benchmark regression in ",
        paste(failed_keys, collapse = ", "),
        "; see metadata/regression-decisions.tsv"
      )
    }
    NULL
  }, error = function(condition) conditionMessage(condition))
}

release_post_error <- tryCatch({
  release_library_after <- release_fingerprint_libraries()
  release_write_tsv(
    release_library_after,
    file.path(release_output, "metadata", "libraries-after.tsv")
  )
  if (!identical(release_library_after, release_library_before)) {
    release_fail("a protected benchmark library changed during the release gate")
  }
  invisible(release_authenticate_root())
  invisible(release_authenticate_differential())
  invisible(release_authenticate_candidate())
  if (!identical(
      unname(tools::sha256sum(release_helper_inputs)), release_helper_sha256
    ) || !identical(
      unname(tools::sha256sum(release_helper_targets)), release_helper_sha256
    ) || !identical(
      unname(tools::sha256sum(release_provenance_sources)), release_provenance_sha
    ) || !identical(
      unname(tools::sha256sum(release_provenance_targets)), release_provenance_sha
    ) || !identical(
      unname(tools::sha256sum(file.path(
        release_output, "metadata", "regression-policy-manifest.tsv"
      ))),
      release_policy_manifest_sha256
    )) {
    release_fail(
      "a helper, provenance input, or policy manifest changed during the release gate"
    )
  }
  NULL
}, error = function(condition) conditionMessage(condition))

release_pass <- all(!is.na(release_status) & release_status == 0L) &&
  is.null(release_validation_error) && is.null(release_post_error) &&
  identical(
    release_regression_summary$row_count, nrow(release_regression_policy)
  ) && identical(release_regression_summary$fail_count, 0L) &&
  file.exists(file.path(
    release_output, "metadata", "regression-decisions.tsv"
  ))
release_completion_value <- function(value) {
  if (length(value) != 1L || is.na(value)) "-" else as.character(value)
}
release_regression_status <- if (is.na(release_regression_summary$fail_count)) {
  "not-run"
} else if (release_regression_summary$fail_count > 0L) {
  "fail"
} else if (release_regression_summary$marginal_count > 0L) {
  "marginal"
} else {
  "pass"
}
release_completion <- c(
  "harness=benchmark-release",
  "schema=2",
  paste0("status=", if (release_pass) "pass" else "fail"),
  paste0("candidate_run_id=", release_candidate_run_id),
  paste0("candidate_ref=", release_candidate_ref),
  paste0("candidate_commit=", release_candidate_commit),
  paste0("candidate_tree=", release_candidate_tree),
  paste0("candidate_content_sha256=", release_candidate_content),
  paste0(
    "baseline_evidence_manifest_sha256=",
    release_differential$verified$manifest_sha256
  ),
  paste0("baseline_commit=", release_differential$completion[["baseline_commit"]]),
  paste0("baseline_version=", release_baseline_version),
  paste0("baseline_content_sha256=", release_baseline_content),
  paste0("candidate_version=", release_candidate$package_version),
  paste0("regression_policy_schema=", release_policy_spec$schema),
  paste0("regression_decision_status=", release_regression_status),
  paste0("regression_minimum_samples=", release_policy_spec$minimum_samples),
  paste0(
    "regression_bootstrap_replicates=",
    release_policy_spec$bootstrap_replicates
  ),
  paste0("regression_confidence=", release_policy_spec$confidence),
  paste0(
    "regression_policy_R_sha256=",
    release_policy_sha256[["regression_policy"]]
  ),
  paste0(
    "regression_policy_table_sha256=",
    release_policy_sha256[["regression_policy_table"]]
  ),
  paste0(
    "regression_policy_manifest_sha256=", release_policy_manifest_sha256
  ),
  paste0("regression_policy_rows=", nrow(release_regression_policy)),
  paste0(
    "regression_decision_rows=",
    release_completion_value(release_regression_summary$row_count)
  ),
  paste0(
    "regression_pass_count=",
    release_completion_value(release_regression_summary$pass_count)
  ),
  paste0(
    "regression_marginal_count=",
    release_completion_value(release_regression_summary$marginal_count)
  ),
  paste0(
    "regression_fail_count=",
    release_completion_value(release_regression_summary$fail_count)
  ),
  paste0(
    "worst_median_case=",
    release_completion_value(release_regression_summary$worst_median_case)
  ),
  paste0(
    "worst_median_ratio=",
    release_completion_value(release_regression_summary$worst_median_ratio)
  ),
  paste0(
    "worst_median_budget_fraction=",
    release_completion_value(
      release_regression_summary$worst_median_budget_fraction
    )
  ),
  paste0(
    "worst_q75_case=",
    release_completion_value(release_regression_summary$worst_q75_case)
  ),
  paste0(
    "worst_q75_ratio=",
    release_completion_value(release_regression_summary$worst_q75_ratio)
  ),
  paste0(
    "worst_q75_budget_fraction=",
    release_completion_value(
      release_regression_summary$worst_q75_budget_fraction
    )
  ),
  paste0(
    "worst_allocation_case=",
    release_completion_value(release_regression_summary$worst_allocation_case)
  ),
  paste0(
    "worst_allocation_ratio=",
    release_completion_value(release_regression_summary$worst_allocation_ratio)
  ),
  paste0(
    "worst_allocation_delta_bytes=",
    release_completion_value(
      release_regression_summary$worst_allocation_delta_bytes
    )
  ),
  paste0(
    "worst_allocation_budget_fraction=",
    release_completion_value(
      release_regression_summary$worst_allocation_budget_fraction
    )
  ),
  paste0("workload_count=", if (is.na(release_workload_count)) "-" else release_workload_count),
  paste0("consumer_rows=", if (is.na(release_consumer_rows)) "-" else release_consumer_rows),
  paste0("protected_library_count=", nrow(release_library_before)),
  paste0("started_utc=", release_started),
  paste0("completed_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0(
    "validation_error=",
    if (is.null(release_validation_error)) {
      "-"
    } else {
      gsub("[\\r\\n\\t]", " ", release_validation_error)
    }
  ),
  paste0(
    "postcondition_error=",
    if (is.null(release_post_error)) "-" else gsub("[\\r\\n\\t]", " ", release_post_error)
  )
)
release_write_lines(
  release_completion,
  file.path(release_output, "metadata", "completion.tsv")
)

if (!release_pass) {
  release_log_line("release benchmark gate failed; evidence remains deliberately unsealed")
  release_fail(
    "release benchmark gate failed; inspect the unsealed stage at ", release_output,
    if (!is.null(release_validation_error)) paste0("; ", release_validation_error) else "",
    if (!is.null(release_post_error)) paste0("; ", release_post_error) else ""
  )
}

release_log_line(paste0(
  "all workloads, focused consumers, regression policy, and postconditions ",
  "passed; marginal decisions=", release_regression_summary$marginal_count
))
retained_evidence_environment <- new.env(parent = baseenv())
sys.source(
  file.path(release_output, "helpers", "repository-evidence.R"),
  envir = retained_evidence_environment
)
sealed <- retained_evidence_environment$repository_seal_evidence(release_output)
verified <- retained_evidence_environment$repository_verify_evidence(release_output)
if (!identical(sealed$manifest_sha256, verified$manifest_sha256)) {
  release_fail("sealed benchmark evidence did not verify in-process")
}
external_verification <- release_run_capture(
  release_expected_rscript,
  c(
    "--vanilla",
    file.path(release_output, "helpers", "verify-repository-evidence.R"),
    release_output, "--quiet"
  ),
  "verifying sealed benchmark evidence with the retained verifier"
)
cat(external_verification, sep = "\n")
cat("benchmark_evidence=", release_output, "\n", sep = "")
cat("evidence_manifest_sha256=", verified$manifest_sha256, "\n", sep = "")
