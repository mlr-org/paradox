`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

benchmark_usage <- function() {
  cat(paste0(
    "Paired paradox benchmark harness\n\n",
    "Usage:\n",
    "  benchmarks/run [options]\n\n",
    "Options:\n",
    "  --baseline-library PATH   Library containing pinned upstream paradox\n",
    "                            [default: .local/baseline-library]\n",
    "  --candidate-library PATH  Library containing candidate paradox\n",
    "                            [default: .local/R/library-dev]\n",
    "  --dependency-library PATH Additional dependency library; repeatable\n",
    "                            [defaults: .local/compat/R/library-dependencies\n",
    "                             and .local/R/library]\n",
    "  --baseline-ref TEXT       Recorded upstream revision [default: unrecorded]\n",
    "  --candidate-ref TEXT      Recorded candidate revision [default: HEAD+dirty state]\n",
    "  --output PATH             New result directory\n",
    "                            [default: .local/benchmarks/<UTC>-<pid>]\n",
    "  --params N                Number of mixed parameters [default: 64]\n",
    "  --rows N                  Rows for check_dt/qunif [default: 128]\n",
    "  --iterations N            Timed samples per workload [default: 100]\n",
    "  --warmups N               Untimed evaluations per workload [default: 5]\n",
    "  --seed N                  Deterministic R seed [default: 20260713]\n",
    "  --workloads A,B,...       Run only the named workloads\n",
    "  --list-workloads          Print workload names and exit\n",
    "  -h, --help                Show this help\n\n",
    "Both packages run in separate fresh R processes. The harness reads CPU\n",
    "governor and load metadata but never changes system settings.\n"
  ))
}

benchmark_parse_integer <- function(value, option, minimum) {
  parsed <- suppressWarnings(as.integer(value))
  if (length(parsed) != 1L || is.na(parsed) || as.character(parsed) != value || parsed < minimum) {
    stop(sprintf("%s requires an integer >= %d; got '%s'.", option, minimum, value), call. = FALSE)
  }
  parsed
}

benchmark_take_value <- function(arguments, index, option) {
  argument <- arguments[[index]]
  prefix <- paste0(option, "=")
  if (startsWith(argument, prefix)) {
    value <- substring(argument, nchar(prefix) + 1L)
    if (!nzchar(value)) stop(sprintf("%s requires a value.", option), call. = FALSE)
    return(list(value = value, next_index = index + 1L))
  }
  if (!identical(argument, option)) return(NULL)
  if (index == length(arguments)) stop(sprintf("%s requires a value.", option), call. = FALSE)
  list(value = arguments[[index + 1L]], next_index = index + 2L)
}

benchmark_parse_arguments <- function(arguments) {
  values <- list(
    baseline_library = ".local/baseline-library",
    candidate_library = ".local/R/library-dev",
    dependency_libraries = character(),
    baseline_ref = NULL,
    candidate_ref = NULL,
    output = NULL,
    n_params = 64L,
    n_rows = 128L,
    iterations = 100L,
    warmups = 5L,
    seed = 20260713L,
    workloads = NULL,
    help = FALSE,
    list_workloads = FALSE
  )

  options <- c(
    "--baseline-library", "--candidate-library", "--dependency-library",
    "--baseline-ref", "--candidate-ref", "--output", "--params", "--rows",
    "--iterations", "--warmups", "--seed", "--workloads"
  )

  i <- 1L
  while (i <= length(arguments)) {
    argument <- arguments[[i]]
    if (argument %in% c("-h", "--help")) {
      values$help <- TRUE
      i <- i + 1L
      next
    }
    if (identical(argument, "--list-workloads")) {
      values$list_workloads <- TRUE
      i <- i + 1L
      next
    }

    matched <- FALSE
    for (option in options) {
      taken <- benchmark_take_value(arguments, i, option)
      if (is.null(taken)) next
      value <- taken$value
      i <- taken$next_index
      matched <- TRUE
      if (identical(option, "--baseline-library")) values$baseline_library <- value
      if (identical(option, "--candidate-library")) values$candidate_library <- value
      if (identical(option, "--dependency-library")) {
        values$dependency_libraries <- c(values$dependency_libraries, value)
      }
      if (identical(option, "--baseline-ref")) values$baseline_ref <- value
      if (identical(option, "--candidate-ref")) values$candidate_ref <- value
      if (identical(option, "--output")) values$output <- value
      if (identical(option, "--params")) {
        values$n_params <- benchmark_parse_integer(value, option, 4L)
      }
      if (identical(option, "--rows")) {
        values$n_rows <- benchmark_parse_integer(value, option, 1L)
      }
      if (identical(option, "--iterations")) {
        values$iterations <- benchmark_parse_integer(value, option, 1L)
      }
      if (identical(option, "--warmups")) {
        values$warmups <- benchmark_parse_integer(value, option, 0L)
      }
      if (identical(option, "--seed")) {
        values$seed <- benchmark_parse_integer(value, option, 0L)
      }
      if (identical(option, "--workloads")) {
        values$workloads <- strsplit(value, ",", fixed = TRUE)[[1L]]
        if (!length(values$workloads) || any(!nzchar(values$workloads))) {
          stop("--workloads requires a comma-separated list of names.", call. = FALSE)
        }
      }
      break
    }
    if (!matched) stop(sprintf("Unknown option '%s'. Use --help for usage.", argument), call. = FALSE)
  }

  if (!length(values$dependency_libraries)) {
    values$dependency_libraries <- c(
      ".local/compat/R/library-dependencies",
      ".local/R/library"
    )
  }
  values
}

benchmark_git <- function(arguments, default = NA_character_) {
  result <- tryCatch(
    system2("git", arguments, stdout = TRUE, stderr = FALSE),
    warning = function(...) character(),
    error = function(...) character()
  )
  if (length(result)) paste(result, collapse = "\n") else default
}

benchmark_normalize_library <- function(path, option) {
  if (!dir.exists(path)) stop(sprintf("%s does not exist: %s", option, path), call. = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

benchmark_compare <- function(output_directory) {
  baseline <- utils::read.csv(file.path(output_directory, "summary-baseline.csv"), check.names = FALSE)
  candidate <- utils::read.csv(file.path(output_directory, "summary-candidate.csv"), check.names = FALSE)
  if (!identical(baseline$workload, candidate$workload)) {
    stop("Baseline and candidate summaries contain different workloads.", call. = FALSE)
  }

  safe_ratio <- function(numerator, denominator) {
    result <- numerator / denominator
    result[numerator == 0 & denominator == 0] <- 1
    result
  }

  comparison <- data.frame(
    workload = baseline$workload,
    baseline_median_ns = baseline$median_ns,
    candidate_median_ns = candidate$median_ns,
    speedup = safe_ratio(baseline$median_ns, candidate$median_ns),
    baseline_mem_alloc_bytes = baseline$mem_alloc_bytes,
    candidate_mem_alloc_bytes = candidate$mem_alloc_bytes,
    allocation_ratio = safe_ratio(baseline$mem_alloc_bytes, candidate$mem_alloc_bytes),
    baseline_gc = baseline$gc_level0 + baseline$gc_level1 + baseline$gc_level2,
    candidate_gc = candidate$gc_level0 + candidate$gc_level1 + candidate$gc_level2,
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    comparison,
    file.path(output_directory, "comparison.csv"),
    row.names = FALSE,
    na = ""
  )
  comparison
}

arguments <- benchmark_parse_arguments(commandArgs(trailingOnly = TRUE))
root <- normalizePath(Sys.getenv("PARADOX_ROOT", unset = getwd()), winslash = "/", mustWork = TRUE)
workload_file <- file.path(root, "benchmarks", "workloads.R")
worker_file <- file.path(root, "benchmarks", "worker.R")

if (arguments$help) {
  benchmark_usage()
  quit(save = "no", status = 0L)
}

source(workload_file, local = TRUE)
available_workloads <- benchmark_workload_names()
if (arguments$list_workloads) {
  cat(paste(available_workloads, collapse = "\n"), "\n", sep = "")
  quit(save = "no", status = 0L)
}

selected_workloads <- arguments$workloads %||% available_workloads
unknown <- setdiff(selected_workloads, available_workloads)
if (length(unknown)) {
  stop(sprintf("Unknown workload(s): %s", paste(unknown, collapse = ", ")), call. = FALSE)
}
if (anyDuplicated(selected_workloads)) stop("--workloads must not contain duplicates.", call. = FALSE)

setwd(root)
baseline_library <- benchmark_normalize_library(arguments$baseline_library, "--baseline-library")
candidate_library <- benchmark_normalize_library(arguments$candidate_library, "--candidate-library")
dependency_libraries <- unique(vapply(
  arguments$dependency_libraries,
  benchmark_normalize_library,
  character(1L),
  option = "--dependency-library"
))

head_revision <- benchmark_git(c("rev-parse", "HEAD"))
status <- benchmark_git(c("status", "--porcelain=v1", "--untracked-files=all"), default = "")
baseline_ref <- arguments$baseline_ref %||% "unrecorded"
if (is.null(arguments$baseline_ref)) {
  message(
    "No --baseline-ref was supplied; recording 'unrecorded'. ",
    "The installed-package MD5 remains authoritative."
  )
}
candidate_ref <- arguments$candidate_ref %||% paste0(
  head_revision,
  if (nzchar(status)) "+dirty" else "+clean"
)

if (is.null(arguments$output)) {
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  output_directory <- file.path(root, ".local", "benchmarks", sprintf("%s-%d", stamp, Sys.getpid()))
} else {
  output_directory <- if (grepl("^(/|[A-Za-z]:[/\\\\])", arguments$output)) {
    arguments$output
  } else {
    file.path(root, arguments$output)
  }
}
if (dir.exists(output_directory) && length(list.files(output_directory, all.files = TRUE, no.. = TRUE))) {
  stop(sprintf("Refusing to overwrite non-empty output directory: %s", output_directory), call. = FALSE)
}
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
output_directory <- normalizePath(output_directory, winslash = "/", mustWork = TRUE)

if (!requireNamespace("callr", quietly = TRUE)) {
  stop("The driver library does not provide the 'callr' package.", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The driver library does not provide the 'jsonlite' package.", call. = FALSE)
}

child_libraries <- function(target) {
  unique(c(target, dependency_libraries, .Library.site, .Library))
}
run_child <- function(label, target, revision) {
  cat(sprintf("Running %s in a fresh R process (%s) ...\n", label, target))
  callr::r(
    function(worker_file, worker_arguments) {
      source(worker_file, local = TRUE)
      do.call(benchmark_worker, worker_arguments)
    },
    args = list(
      worker_file = worker_file,
      worker_arguments = list(
        label = label,
        target_library = target,
        output_directory = output_directory,
        workload_file = workload_file,
        selected_workloads = selected_workloads,
        n_params = arguments$n_params,
        n_rows = arguments$n_rows,
        iterations = arguments$iterations,
        warmups = arguments$warmups,
        seed = arguments$seed,
        revision = revision
      )
    ),
    libpath = child_libraries(target),
    env = c(
      NOT_CRAN = "true",
      R_TESTS = "",
      R_PARALLEL_PORT = "random"
    ),
    stdout = "|",
    stderr = "2>&1",
    spinner = FALSE,
    show = TRUE
  )
}

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
baseline <- run_child("baseline", baseline_library, baseline_ref)
candidate <- run_child("candidate", candidate_library, candidate_ref)

if (!identical(baseline$validation_keys, candidate$validation_keys)) {
  mismatched <- names(baseline$validation_keys)[
    !vapply(
      names(baseline$validation_keys),
      function(name) identical(baseline$validation_keys[[name]], candidate$validation_keys[[name]]),
      logical(1L)
    )
  ]
  stop(
    sprintf("Validated results differ between installations: %s", paste(mismatched, collapse = ", ")),
    call. = FALSE
  )
}

comparison <- benchmark_compare(output_directory)
driver_metadata <- list(
  schema_version = 1L,
  started_at_utc = started_at,
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  repository = list(
    root = root,
    head = head_revision,
    dirty = nzchar(status),
    status = if (nzchar(status)) strsplit(status, "\n", fixed = TRUE)[[1L]] else character()
  ),
  arguments = list(
    baseline_library = baseline_library,
    candidate_library = candidate_library,
    dependency_libraries = dependency_libraries,
    baseline_ref = baseline_ref,
    candidate_ref = candidate_ref,
    output_directory = output_directory,
    n_params = arguments$n_params,
    n_rows = arguments$n_rows,
    iterations = arguments$iterations,
    warmups = arguments$warmups,
    seed = arguments$seed,
    workloads = selected_workloads
  ),
  baseline = baseline$metadata,
  candidate = candidate$metadata,
  validation_keys = baseline$validation_keys,
  files = c(
    "metadata.json",
    "comparison.csv",
    "summary-baseline.csv",
    "summary-candidate.csv",
    "samples-baseline.csv",
    "samples-candidate.csv",
    "allocations-baseline.csv",
    "allocations-candidate.csv"
  )
)
jsonlite::write_json(
  driver_metadata,
  file.path(output_directory, "metadata.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null",
  na = "null",
  digits = NA
)

cat("\nComparison (ratios greater than 1 favor the candidate):\n")
print(
  transform(
    comparison,
    baseline_median_us = baseline_median_ns / 1e3,
    candidate_median_us = candidate_median_ns / 1e3
  )[, c(
    "workload", "baseline_median_us", "candidate_median_us", "speedup",
    "baseline_mem_alloc_bytes", "candidate_mem_alloc_bytes", "allocation_ratio"
  )],
  row.names = FALSE,
  digits = 4
)
cat(sprintf("\nRaw results and metadata: %s\n", output_directory))
