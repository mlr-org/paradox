args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 9L) {
  stop(
    paste(
      "usage: repository-test-child.R CHECKOUT FRAMEWORK CANDIDATE_LIB",
      "DEPENDENCY_LIB EXTRA_LIBS VERSION CONTENT_SHA COUNTS ROW_STATE"
    ),
    call. = FALSE
  )
}

is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

require_directory <- function(path, label) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      !dir.exists(path) || is_symbolic(path)) {
    stop(label, " is missing, not a directory, or symbolic: ", path,
      call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

require_file <- function(path, label) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path) || dir.exists(path) || is_symbolic(path)) {
    stop(label, " is missing, not a regular file, or symbolic: ", path,
      call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

is_sha256 <- function(value) {
  length(value) == 1L && !is.na(value) &&
    grepl("^[0-9a-f]{64}$", value)
}

is_within <- function(path, parent) {
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}

prepare_counts_path <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      grepl("[\r\n\t]", path)) {
    stop("structured-count output path is invalid", call. = FALSE)
  }
  name <- basename(path)
  if (!nzchar(name) || name %in% c(".", "..")) {
    stop("structured-count output has an unsafe basename", call. = FALSE)
  }
  parent <- require_directory(dirname(path), "structured-count output parent")
  path <- file.path(parent, name)
  if (file.exists(path) || dir.exists(path) || is_symbolic(path)) {
    stop("structured-count output already exists or is symbolic", call. = FALSE)
  }
  path
}

count_fields <- c(
  "schema", "availability", "test_cases", "expectations", "passed",
  "failed", "skipped", "errors", "warnings"
)

unavailable_counts <- function() {
  c(
    schema = "1", availability = "unavailable_before_results",
    test_cases = "-", expectations = "-", passed = "-", failed = "-",
    skipped = "-", errors = "-", warnings = "-"
  )
}

validate_counts <- function(value) {
  value_names <- names(value)
  value <- stats::setNames(as.character(unname(value)), value_names)
  if (!identical(names(value), count_fields) || anyNA(value) ||
      any(!nzchar(value)) || any(grepl("[\r\n\t]", value))) {
    stop("internal structured-count schema is invalid", call. = FALSE)
  }
  numeric_fields <- setdiff(count_fields, c("schema", "availability"))
  numeric_values <- value[numeric_fields]
  if (!identical(value[["schema"]], "1") ||
      !value[["availability"]] %in% c(
        "unavailable_before_results", "complete_testthat",
        "partial_tinytest", "partial_base_files"
      ) || any(numeric_values != "-" &
      !grepl("^(0|[1-9][0-9]*)$", numeric_values))) {
    stop("internal structured counts contain an invalid value", call. = FALSE)
  }
  value
}

write_counts <- function(value, path) {
  value <- validate_counts(value)
  temporary <- paste0(path, ".new")
  if (file.exists(path) || dir.exists(path) || is_symbolic(path) ||
      file.exists(temporary) || dir.exists(temporary) || is_symbolic(temporary)) {
    stop("refusing to overwrite structured-count evidence", call. = FALSE)
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  frame <- data.frame(
    field = names(value), value = unname(value), stringsAsFactors = FALSE
  )
  utils::write.table(
    frame, temporary, quote = FALSE, sep = "\t", row.names = FALSE,
    na = "-", fileEncoding = "UTF-8"
  )
  require_file(temporary, "structured-count temporary output")
  if (!file.rename(temporary, path)) {
    stop("could not atomically retain structured test counts", call. = FALSE)
  }
  invisible(path)
}

parse_extra_libraries <- function(value) {
  if (identical(value, "-")) return(character())
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("extra-library argument is invalid", call. = FALSE)
  }
  paths <- strsplit(value, .Platform$path.sep, fixed = TRUE)[[1L]]
  if (!length(paths) || any(!nzchar(paths)) ||
      !identical(paste(paths, collapse = .Platform$path.sep), value)) {
    stop("extra-library argument contains an empty path", call. = FALSE)
  }
  unname(vapply(
    seq_along(paths),
    function(index) require_directory(
      paths[[index]], sprintf("extra library %d", index)
    ),
    character(1L)
  ))
}

validate_row_environment <- function(row_state, libraries) {
  state_paths <- c(
    HOME = "home", TMPDIR = "tmp", XDG_CACHE_HOME = "xdg-cache",
    XDG_CONFIG_HOME = "xdg-config", XDG_DATA_HOME = "xdg-data",
    XDG_STATE_HOME = "xdg-state", XDG_RUNTIME_DIR = "xdg-runtime",
    R_USER_CACHE_DIR = "r-cache", R_USER_CONFIG_DIR = "r-config",
    R_USER_DATA_DIR = "r-data", PYTHONPYCACHEPREFIX = "pycache",
    PYTHONUSERBASE = "python-user", PIP_CACHE_DIR = "pip-cache",
    UV_CACHE_DIR = "uv-cache", UV_PYTHON_INSTALL_DIR = "uv-python",
    RETICULATE_VIRTUALENV_ROOT = "virtualenvs", WORKON_HOME = "virtualenvs",
    RETICULATE_MINICONDA_PATH = "miniconda", MPLCONFIGDIR = "matplotlib",
    TORCH_HOME = "torch", HF_HOME = "huggingface",
    HUGGINGFACE_HUB_CACHE = "huggingface/hub",
    TRANSFORMERS_CACHE = "huggingface/transformers", KERAS_HOME = "keras",
    NUMBA_CACHE_DIR = "numba", CUDA_CACHE_PATH = "cuda", WEKA_HOME = "weka",
    CCACHE_DIR = "ccache", CCACHE_TEMPDIR = "ccache-tmp"
  )
  expected_paths <- file.path(row_state, unname(state_paths))
  names(expected_paths) <- names(state_paths)
  for (index in seq_along(expected_paths)) {
    observed <- require_directory(
      expected_paths[[index]], paste0(names(expected_paths)[[index]], " directory")
    )
    if (!identical(observed, expected_paths[[index]])) {
      stop(names(expected_paths)[[index]], " directory is not canonical",
        call. = FALSE)
    }
  }
  observed_paths <- Sys.getenv(names(expected_paths), unset = NA_character_)
  invalid_paths <- names(expected_paths)[
    is.na(observed_paths) | observed_paths != expected_paths
  ]
  if (length(invalid_paths)) {
    stop(
      "child cache paths are not exactly row-local: ",
      paste(invalid_paths, collapse = ", "), call. = FALSE
    )
  }
  runtime_mode <- as.integer(file.info(expected_paths[["XDG_RUNTIME_DIR"]])$mode)
  if (length(runtime_mode) != 1L || is.na(runtime_mode) ||
      bitwAnd(runtime_mode, 511L) != 448L) {
    stop("row-local XDG runtime directory does not have mode 0700",
      call. = FALSE)
  }

  library_environment <- paste(libraries, collapse = .Platform$path.sep)
  constants <- c(
    PARADOX_ROW_STATE_ROOT = row_state,
    R_LIBS = library_environment,
    R_LIBS_USER = library_environment,
    NOT_CRAN = "true",
    TESTTHAT_PARALLEL = "false",
    PYTHONDONTWRITEBYTECODE = "1",
    PYTHONNOUSERSITE = "1",
    PIP_CONFIG_FILE = "/dev/null",
    PIP_DISABLE_PIP_VERSION_CHECK = "1",
    RETICULATE_AUTOCONFIGURE = "FALSE",
    RETICULATE_AUTOCREATE_PACKAGE_VENV = "FALSE",
    RETICULATE_USE_MANAGED_VENV = "yes"
  )
  observed_constants <- Sys.getenv(names(constants), unset = NA_character_)
  invalid_constants <- names(constants)[
    is.na(observed_constants) | observed_constants != constants
  ]
  if (length(invalid_constants)) {
    stop(
      "child library, Python, or test environment is not isolated: ",
      paste(invalid_constants, collapse = ", "), call. = FALSE
    )
  }
  empty_constants <- c(
    "PYTHONPATH", "PYTHONHOME", "VIRTUAL_ENV", "RETICULATE_PYTHON",
    "RETICULATE_PYTHON_ENV"
  )
  nonempty_constants <- empty_constants[
    nzchar(Sys.getenv(empty_constants, unset = ""))
  ]
  if (length(nonempty_constants)) {
    stop(
      "child inherited a Python interpreter or module override: ",
      paste(nonempty_constants, collapse = ", "), call. = FALSE
    )
  }
  if (nzchar(Sys.getenv("R_TESTS", unset = ""))) {
    stop("R_TESTS must be empty; the child installs its null PDF device itself",
      call. = FALSE)
  }
  invisible(TRUE)
}

testthat_count <- function(frame, field) {
  if (!field %in% names(frame)) {
    stop("testthat results lack the ", field, " count", call. = FALSE)
  }
  value <- frame[[field]]
  if (is.logical(value)) value <- as.integer(value)
  if (!is.numeric(value) || anyNA(value) || any(!is.finite(value)) ||
      any(value < 0) || any(value != floor(value))) {
    stop("testthat returned an invalid ", field, " count", call. = FALSE)
  }
  sum(value)
}

counts_path <- prepare_counts_path(args[[8L]])
counts <- unavailable_counts()

status <- tryCatch({
  if (!"--vanilla" %in% commandArgs(trailingOnly = FALSE)) {
    stop("repository-test child must be invoked by Rscript --vanilla",
      call. = FALSE)
  }
  checkout <- require_directory(args[[1L]], "repository checkout")
  framework <- args[[2L]]
  if (!framework %in% c("testthat", "tinytest", "base")) {
    stop("unsupported test framework: ", framework, call. = FALSE)
  }
  require_file(file.path(checkout, "DESCRIPTION"), "repository DESCRIPTION")

  candidate_library <- require_directory(args[[3L]], "candidate library")
  dependency_library <- require_directory(args[[4L]], "dependency library")
  extra_libraries <- parse_extra_libraries(args[[5L]])
  libraries <- c(candidate_library, extra_libraries, dependency_library)
  if (anyDuplicated(libraries)) {
    stop("candidate, dependency, and extra libraries must be distinct",
      call. = FALSE)
  }

  candidate_version <- args[[6L]]
  candidate_content_sha256 <- args[[7L]]
  if (length(candidate_version) != 1L || is.na(candidate_version) ||
      !nzchar(candidate_version) || grepl("[\r\n\t]", candidate_version)) {
    stop("candidate version is invalid", call. = FALSE)
  }
  if (!is_sha256(candidate_content_sha256)) {
    stop("candidate content sentinel is not a lowercase SHA-256", call. = FALSE)
  }
  row_state <- require_directory(args[[9L]], "row-local state root")
  protected_paths <- c(checkout, libraries)
  if (any(vapply(protected_paths, function(path) {
    is_within(row_state, path) || is_within(path, row_state)
  }, logical(1L)))) {
    stop("row-local state overlaps a checkout or protected library",
      call. = FALSE)
  }
  counts_parent <- dirname(counts_path)
  if (any(vapply(c(checkout, libraries), function(path) {
    is_within(counts_parent, path)
  }, logical(1L)))) {
    stop("structured-count output overlaps a protected library", call. = FALSE)
  }

  validate_row_environment(row_state, libraries)
  base_library <- require_directory(.Library, "R base library")
  .libPaths(c(libraries, base_library))
  observed_libraries <- unname(vapply(
    .libPaths(), require_directory, character(1L), label = "active R library"
  ))
  expected_libraries <- unique(c(libraries, base_library))
  if (!identical(observed_libraries, expected_libraries)) {
    stop("child library order differs from the isolated release plan",
      call. = FALSE)
  }

  sentinel <- require_file(
    file.path(candidate_library, ".paradox-candidate-content-sha256"),
    "candidate content sentinel"
  )
  declared_content <- readLines(sentinel, warn = FALSE)
  if (length(declared_content) != 1L ||
      !identical(declared_content, candidate_content_sha256)) {
    stop("candidate content sentinel differs from the expected fingerprint",
      call. = FALSE)
  }
  resolved_candidate <- normalizePath(
    find.package("paradox"), winslash = "/", mustWork = TRUE
  )
  if (!identical(dirname(resolved_candidate), candidate_library)) {
    stop("paradox did not resolve from the dedicated candidate library",
      call. = FALSE)
  }
  if (!identical(as.character(utils::packageVersion("paradox")),
      candidate_version)) {
    stop("child resolved an unexpected paradox version", call. = FALSE)
  }
  null_pdf_device <- function(...) {
    arguments <- list(...)
    arguments["file"] <- list(NULL)
    do.call(grDevices::pdf, arguments)
  }
  options(device = null_pdf_device)
  Sys.setenv(NOT_CRAN = "true", TESTTHAT_PARALLEL = "false")

  message("candidate_resolution=", resolved_candidate)
  message("candidate_version=", candidate_version)
  message("candidate_content_sha256=", candidate_content_sha256)
  message("test_framework=", framework)

  if (identical(framework, "testthat")) {
    result <- testthat::test_local(
      checkout, reporter = "summary", stop_on_failure = FALSE
    )
    frame <- as.data.frame(result)
    failed <- testthat_count(frame, "failed")
    skipped <- testthat_count(frame, "skipped")
    errors <- testthat_count(frame, "error")
    warnings <- testthat_count(frame, "warning")
    passed <- testthat_count(frame, "passed")
    expectations <- testthat_count(frame, "nb")
    if (expectations != passed + failed + skipped + warnings) {
      stop("testthat result counts do not form a complete expectation total",
        call. = FALSE)
    }
    counts <- c(
      schema = "1", availability = "complete_testthat",
      test_cases = as.character(nrow(frame)),
      expectations = as.character(expectations),
      passed = as.character(passed), failed = as.character(failed),
      skipped = as.character(skipped), errors = as.character(errors),
      warnings = as.character(warnings)
    )
    if (failed > 0 || errors > 0) {
      stop(failed, " failed expectations and ", errors, " errors",
        call. = FALSE)
    }
  } else if (identical(framework, "tinytest")) {
    pkgload::load_all(checkout, helpers = FALSE, export_all = FALSE, quiet = TRUE)
    result <- tinytest::run_test_dir(
      file.path(checkout, "inst", "tinytest"), at_home = TRUE,
      verbose = 1L, lc_collate = "C"
    )
    failed <- vapply(result, isFALSE, logical(1L))
    counts <- c(
      schema = "1", availability = "partial_tinytest",
      test_cases = as.character(length(result)),
      expectations = as.character(length(result)),
      passed = as.character(sum(!failed)), failed = as.character(sum(failed)),
      skipped = "-", errors = "-", warnings = "-"
    )
    if (any(failed)) {
      detail <- vapply(result[failed], format, character(1L), type = "long")
      stop(
        sum(failed), " out of ", length(result), " tinytests failed:\n",
        paste(detail, collapse = "\n"), call. = FALSE
      )
    }
  } else {
    pkgload::load_all(checkout, helpers = FALSE, export_all = FALSE, quiet = TRUE)
    test_files <- list.files(
      file.path(checkout, "tests"), pattern = "[.][Rr]$", full.names = TRUE
    )
    counts <- c(
      schema = "1", availability = "partial_base_files",
      test_cases = as.character(length(test_files)), expectations = "-",
      passed = "-", failed = "-", skipped = "-", errors = "-", warnings = "-"
    )
    for (test_file in test_files) {
      sys.source(
        test_file, envir = new.env(parent = globalenv()), keep.source = TRUE
      )
    }
  }
  message("ordinary_upstream_tests=passed")
  0L
}, error = function(condition) {
  message("ordinary_upstream_tests=failed")
  message("error=", conditionMessage(condition))
  1L
})

write_counts(counts, counts_path)
quit(save = "no", status = status, runLast = FALSE)
