#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(3L, 4L)) {
  stop(
    paste(
      "usage: run-native-tests.R SNAPSHOT INSTALLED_LIBRARY",
      "none|analyzer|focused|full [LEDGER_TSV]"
    ),
    call. = FALSE
  )
}

snapshot <- normalizePath(args[[1L]], mustWork = TRUE)
library_path <- normalizePath(args[[2L]], mustWork = TRUE)
selection <- match.arg(args[[3L]], c("none", "analyzer", "focused", "full"))
ledger_path <- if (length(args) == 4L) {
  normalizePath(args[[4L]], winslash = "/", mustWork = FALSE)
} else {
  NULL
}

sanitize_field <- function(value) {
  value <- paste(value, collapse = " | ")
  value <- gsub("[\t\r\n]+", " ", value)
  if (nzchar(value)) value else "-"
}

write_ledger <- function(
  results,
  selected_files,
  worker_evidence_sha256 = NULL,
  requested_jobs = 1L,
  effective_jobs = 1L,
  scheduler_backend = "serial"
) {
  if (is.null(ledger_path)) return(invisible(NULL))
  if (!dir.exists(dirname(ledger_path))) {
    dir.create(dirname(ledger_path), recursive = TRUE, mode = "0700")
  }
  if (is.null(results)) {
    summarized <- data.frame()
  } else {
    summarized <- testthat:::as.data.frame.testthat_results(results)
  }
  if (nrow(summarized)) {
    skip_reasons <- vapply(summarized$result, function(expectations) {
      skipped <- Filter(
        function(expectation) inherits(expectation, "expectation_skip"),
        expectations
      )
      if (!length(skipped)) return("-")
      sanitize_field(vapply(skipped, conditionMessage, character(1L)))
    }, character(1L))
    rows <- data.frame(
      kind = "test",
      schema = "1",
      selection = selection,
      not_cran = identical(Sys.getenv("NOT_CRAN", unset = ""), "true"),
      dso_sha256 = dso_sha256,
      file = vapply(summarized$file, sanitize_field, character(1L)),
      context = vapply(summarized$context, sanitize_field, character(1L)),
      test = vapply(summarized$test, sanitize_field, character(1L)),
      expectations = summarized$nb,
      passed = summarized$passed,
      failed = summarized$failed,
      skipped = as.integer(summarized$skipped),
      errors = as.integer(summarized$error),
      warnings = summarized$warning,
      skip_reason = skip_reasons,
      detail = "-",
      stringsAsFactors = FALSE
    )
  } else {
    rows <- data.frame(
      kind = character(), schema = character(), selection = character(),
      not_cran = logical(), dso_sha256 = character(), file = character(),
      context = character(),
      test = character(), expectations = integer(), passed = integer(),
      failed = integer(), skipped = integer(), errors = integer(),
      warnings = integer(), skip_reason = character(), detail = character(),
      stringsAsFactors = FALSE
    )
  }
  summary_row <- data.frame(
    kind = "summary",
    schema = "1",
    selection = selection,
    not_cran = identical(Sys.getenv("NOT_CRAN", unset = ""), "true"),
    dso_sha256 = dso_sha256,
    file = "-",
    context = "-",
    test = "complete",
    expectations = sum(rows$expectations),
    passed = sum(rows$passed),
    failed = sum(rows$failed),
    skipped = sum(rows$skipped),
    errors = sum(rows$errors),
    warnings = sum(rows$warnings),
    skip_reason = "-",
    detail = paste0(
      "files=", length(unique(selected_files)),
      ";test_blocks=", nrow(rows),
      ";requested_jobs=", requested_jobs,
      ";effective_jobs=", effective_jobs,
      ";scheduler_backend=", scheduler_backend,
      if (is.null(worker_evidence_sha256)) "" else paste0(
        ";worker_evidence_sha256=", worker_evidence_sha256
      )
    ),
    stringsAsFactors = FALSE
  )
  output <- rbind(rows, summary_row)
  temporary <- paste0(ledger_path, ".new.", Sys.getpid())
  on.exit(unlink(temporary), add = TRUE)
  write.table(
    output,
    temporary,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    na = ""
  )
  if (!file.rename(temporary, ledger_path)) {
    stop("could not publish native functional-test ledger", call. = FALSE)
  }
  invisible(NULL)
}

.libPaths(unique(c(library_path, .libPaths())))
installed <- normalizePath(find.package("paradox", lib.loc = library_path),
  mustWork = TRUE)
if (!startsWith(installed, paste0(library_path, .Platform$file.sep))) {
  stop("paradox did not resolve from the isolated run library", call. = FALSE)
}

description <- read.dcf(file.path(installed, "DESCRIPTION"),
  fields = c("Package", "Version", "Built"))
cat("installed_package=", description[1L, "Package"], "\n", sep = "")
cat("installed_version=", description[1L, "Version"], "\n", sep = "")
cat("installed_path=", installed, "\n", sep = "")
cat("tests=", selection, "\n", sep = "")

load_candidate <- function() {
  library("paradox", character.only = TRUE, lib.loc = library_path)
  loaded <- getLoadedDLLs()[["paradox"]]
  if (is.null(loaded) || is.null(loaded[["path"]])) {
    stop("paradox did not retain one loaded native library", call. = FALSE)
  }
  path <- normalizePath(loaded[["path"]], mustWork = TRUE)
  installed_prefix <- paste0(installed, .Platform$file.sep)
  link_target <- Sys.readlink(path)
  if (!startsWith(path, installed_prefix) || dir.exists(path) ||
      (!is.na(link_target) && nzchar(link_target))) {
    stop("loaded paradox DSO escaped the isolated installation", call. = FALSE)
  }
  list(path = path, sha256 = unname(tools::sha256sum(path)))
}

assert_dso_unchanged <- function(loaded) {
  current_loaded <- getLoadedDLLs()[["paradox"]]
  if (is.null(current_loaded) || is.null(current_loaded[["path"]])) {
    stop("paradox DSO was unloaded during native tests", call. = FALSE)
  }
  current_path <- normalizePath(current_loaded[["path"]], mustWork = TRUE)
  current <- unname(tools::sha256sum(current_path))
  if (!identical(current_path, loaded$path) ||
      !identical(current, loaded$sha256)) {
    stop("loaded paradox DSO changed during native tests", call. = FALSE)
  }
  invisible(current)
}

if (selection == "none") {
  loaded_dso <- load_candidate()
  dso_sha256 <- loaded_dso$sha256
  assert_dso_unchanged(loaded_dso)
  write_ledger(NULL, character())
  quit(save = "no", status = 0L)
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat is not installed in a repository-local library", call. = FALSE)
}

Sys.setenv(NOT_CRAN = if (selection == "analyzer") "false" else "true")
Sys.setenv(
  MC_CORES = "1",
  R_FUTURE_PLAN = "sequential",
  R_FUTURE_FORK_ENABLE = "false",
  R_PARALLELLY_FORK_ENABLE = "false"
)
options(
  warn = 2L,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  mc.cores = 1L
)

loaded_dso <- load_candidate()
dso_sha256 <- loaded_dso$sha256
analyzer_files <- c(
  "test-native-adversarial-storage.R",
  "test-native-altrep-lifetimes.R",
  "test-native-domain-kernels.R",
  "test-native-gctorture.R",
  "test-native-paramset-qunif.R",
  "test-native-paramset-trafo-gctorture.R",
  "test-native-paramset-value-mutation.R",
  "test-native-paramsetcollection-construction.R"
)
filter <- switch(selection,
  analyzer = paste0(
    "^(",
    paste(
      sub("[.][Rr]$", "", sub("^test-", "", analyzer_files)),
      collapse = "|"
    ),
    ")$"
  ),
  # The focused wave is architectural, not merely filename-conventional.
  # Keep the explicit Paradox-2 contract/token suites here even when their
  # public class/API names intentionally do not contain "native".
  focused = paste0(
    "(characterization|native|regression|",
    "ParamSetShadow|core-state-contract|paramset-equality|to_tune|",
    "upgrade-paradox-object|upgrade-registry)"
  ),
  NULL
)
test_directory <- file.path(snapshot, "tests", "testthat")
sort_bytes <- function(x) sort(x, method = "radix")
available_files <- dir(
  test_directory,
  "^test.*\\.[rR]$",
  full.names = FALSE
)
if (selection == "analyzer") {
  if (!identical(sort_bytes(intersect(available_files, analyzer_files)),
      analyzer_files)) {
    stop("analyzer test inventory is absent or incomplete", call. = FALSE)
  }
  selected_files <- analyzer_files
} else {
  selected_files <- sort_bytes(available_files)
  if (!is.null(filter)) selected_files <- selected_files[grepl(filter, selected_files)]
}

combine_results <- function(values) {
  output <- list()
  for (value in values) output <- c(output, unclass(value))
  structure(output, class = "testthat_results")
}

worker_evidence_path <- function(path) {
  if (!grepl("[.]tsv$", path)) {
    stop("parallel native-test ledger path must end in .tsv", call. = FALSE)
  }
  sub("[.]tsv$", "-workers.tsv", path)
}

run_parallel_tests <- function(files, jobs) {
  if (is.null(ledger_path)) {
    stop("parallel native tests require a retained ledger path", call. = FALSE)
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("processx is required for isolated parallel native tests", call. = FALSE)
  }

  command <- commandArgs(trailingOnly = FALSE)
  script_argument <- grep("^--file=", command, value = TRUE)
  if (length(script_argument) != 1L) {
    stop("could not identify the native-test coordinator script", call. = FALSE)
  }
  coordinator <- normalizePath(
    sub("^--file=", "", script_argument),
    mustWork = TRUE
  )
  worker_script <- file.path(dirname(coordinator), "run-native-test-worker.R")
  if (!file.exists(worker_script) || dir.exists(worker_script) ||
      (!is.na(Sys.readlink(worker_script)) && nzchar(Sys.readlink(worker_script)))) {
    stop("isolated native-test worker is absent or symbolic", call. = FALSE)
  }
  worker_script <- normalizePath(worker_script, mustWork = TRUE)
  worker_group <- file.path(dirname(coordinator), "run-native-test-worker-group")
  if (!file.exists(worker_group) || dir.exists(worker_group) ||
      file.access(worker_group, mode = 1L) != 0L ||
      (!is.na(Sys.readlink(worker_group)) && nzchar(Sys.readlink(worker_group)))) {
    stop("native-test worker process-group supervisor is absent or unsafe", call. = FALSE)
  }
  worker_group <- normalizePath(worker_group, mustWork = TRUE)

  # These two full-only files share reticulate's managed Python state.  They
  # execute together, after all other tasks, and never overlap another worker.
  configspace_lane <- c(
    "test_paramset_to_configspace.R",
    "test_paramset_to_configspace_old.R"
  )
  lane_files <- intersect(configspace_lane, files)
  parallel_files <- setdiff(files, lane_files)
  tasks <- lapply(parallel_files, function(file) {
    list(files = file, lane = "parallel")
  })
  if (length(lane_files)) {
    if (!identical(lane_files, configspace_lane)) {
      stop("ConfigSpace serialized lane inventory is incomplete", call. = FALSE)
    }
    tasks[[length(tasks) + 1L]] <- list(
      files = lane_files,
      lane = "exclusive"
    )
  }
  for (index in seq_along(tasks)) tasks[[index]]$index <- index
  worker_timeout_seconds <- 1800

  evidence_path <- worker_evidence_path(ledger_path)
  worker_root <- sub("[.]tsv$", "-workers", ledger_path)
  if (file.exists(evidence_path) ||
      (!is.na(Sys.readlink(evidence_path)) && nzchar(Sys.readlink(evidence_path))) ||
      file.exists(worker_root) ||
      (!is.na(Sys.readlink(worker_root)) && nzchar(Sys.readlink(worker_root)))) {
    stop("parallel native-test evidence path already exists", call. = FALSE)
  }
  dir.create(worker_root, recursive = FALSE, mode = "0700")
  logs_directory <- file.path(worker_root, "logs")
  results_directory <- file.path(worker_root, "results")
  work_directory <- file.path(worker_root, "work")
  dir.create(logs_directory, mode = "0700")
  dir.create(results_directory, mode = "0700")
  dir.create(work_directory, mode = "0700")

  jobs <- min(jobs, max(1L, length(parallel_files)))
  cat("native_parallel_jobs=", jobs, "\n", sep = "")
  cat("native_parallel_tasks=", length(tasks), "\n", sep = "")
  cat("native_worker_timeout_seconds=", worker_timeout_seconds, "\n", sep = "")

  active <- list()
  records <- vector("list", length(tasks))
  terminate_active <- function() {
    for (item in active) {
      if (isTRUE(tryCatch(
        item$process$is_alive(),
        error = function(...) FALSE
      ))) {
        try(item$process$kill_tree(), silent = TRUE)
      }
    }
    for (item in active) {
      try(item$process$wait(5000L), silent = TRUE)
      if (isTRUE(tryCatch(
        item$process$is_alive(),
        error = function(...) FALSE
      ))) {
        try(item$process$kill(), silent = TRUE)
        try(item$process$wait(5000L), silent = TRUE)
      }
    }
  }
  on.exit(terminate_active(), add = TRUE)

  launch <- function(task) {
    label <- sprintf("task-%03d", task$index)
    task_root <- file.path(work_directory, label)
    task_tmp <- file.path(task_root, "tmp")
    task_home <- file.path(task_root, "home")
    task_cache <- file.path(task_root, "cache")
    task_tex <- file.path(task_root, "texmf")
    dir.create(task_root, mode = "0700")
    for (directory in c(task_tmp, task_home, task_cache, task_tex)) {
      dir.create(directory, mode = "0700")
    }
    result_path <- file.path(results_directory, paste0(label, ".rds"))
    log_path <- file.path(logs_directory, paste0(label, ".log"))
    worker_token <- paste(
      "paradox-native", Sys.getpid(), task$index,
      substr(dso_sha256, 1L, 16L),
      sep = "-"
    )
    worker_args <- c(
      "--vanilla", worker_script, snapshot, library_path, selection,
      dso_sha256, result_path, task$files
    )
    worker_environment <- c(
      "current",
      HOME = task_home,
      TMPDIR = task_tmp,
      TMP = task_tmp,
      TEMP = task_tmp,
      XDG_CACHE_HOME = task_cache,
      R_USER_CACHE_DIR = file.path(task_cache, "R"),
      PYTHONDONTWRITEBYTECODE = "1",
      PYTHONPYCACHEPREFIX = file.path(task_cache, "python-bytecode"),
      TEXMFVAR = file.path(task_tex, "var"),
      TEXMFCONFIG = file.path(task_tex, "config"),
      TEXMFHOME = file.path(task_tex, "home"),
      TEXMFCACHE = file.path(task_tex, "cache"),
      VARTEXFONTS = file.path(task_tex, "fonts"),
      R_LIBS_USER = library_path,
      R_ENVIRON_USER = "/dev/null",
      R_PROFILE_USER = "/dev/null",
      R_HISTFILE = "/dev/null",
      PARADOX_NATIVE_COORDINATOR_PID = as.character(Sys.getpid()),
      PARADOX_NATIVE_WORKER_TOKEN = worker_token,
      TESTTHAT_PARALLEL = "false",
      TESTTHAT_IS_PARALLEL = "false",
      TESTTHAT_CPUS = "1",
      MAKEFLAGS = "-j1",
      CMAKE_BUILD_PARALLEL_LEVEL = "1",
      MC_CORES = "1",
      R_FUTURE_PLAN = "sequential",
      R_FUTURE_FORK_ENABLE = "false",
      R_PARALLELLY_FORK_ENABLE = "false",
      OMP_NUM_THREADS = "1",
      OMP_THREAD_LIMIT = "1",
      OPENBLAS_NUM_THREADS = "1",
      GOTO_NUM_THREADS = "1",
      MKL_NUM_THREADS = "1",
      BLIS_NUM_THREADS = "1",
      VECLIB_MAXIMUM_THREADS = "1",
      NUMEXPR_NUM_THREADS = "1",
      RCPP_PARALLEL_NUM_THREADS = "1",
      R_PARALLELLY_AVAILABLECORES_FALLBACK = "1",
      R_FUTURE_AVAILABLECORES_FALLBACK = "1",
      `_R_CHECK_LIMIT_CORES_` = "true"
    )
    process <- tryCatch(
      processx::process$new(
        worker_group,
        c(file.path(R.home("bin"), "Rscript"), worker_args),
        stdout = log_path,
        stderr = "2>&1",
        env = worker_environment,
        cleanup = TRUE,
        cleanup_tree = TRUE,
        supervise = TRUE,
        linux_pdeathsig = TRUE
      ),
      error = identity
    )
    if (inherits(process, "error")) {
      writeLines(
        paste0("worker launch failed: ", conditionMessage(process)),
        log_path
      )
      records[[task$index]] <<- list(
        task = task, status = 127L, result_path = result_path,
        log_path = log_path
      )
      return(invisible(FALSE))
    }
    active[[label]] <<- list(
      process = process, task = task, result_path = result_path,
      log_path = log_path,
      started = unname(proc.time()[["elapsed"]])
    )
    invisible(TRUE)
  }

  run_wave <- function(wave, limit) {
    next_task <- 1L
    while (next_task <= length(wave) || length(active)) {
      while (next_task <= length(wave) && length(active) < limit) {
        launch(wave[[next_task]])
        next_task <- next_task + 1L
      }
      if (!length(active)) next
      finished_labels <- character()
      repeat {
        finished <- vapply(active, function(item) {
          !isTRUE(tryCatch(
            item$process$is_alive(),
            error = function(...) FALSE
          ))
        }, logical(1L))
        if (any(finished)) {
          finished_labels <- names(active)[finished]
          break
        }
        now <- unname(proc.time()[["elapsed"]])
        timed_out <- vapply(active, function(item) {
          now - item$started >= worker_timeout_seconds
        }, logical(1L))
        if (any(timed_out)) {
          for (label in names(active)[timed_out]) {
            item <- active[[label]]
            cat(
              "native worker timed out after ", worker_timeout_seconds,
              " seconds\n", sep = "", file = item$log_path, append = TRUE
            )
            try(item$process$kill_tree(), silent = TRUE)
            try(item$process$wait(5000L), silent = TRUE)
            if (isTRUE(tryCatch(
              item$process$is_alive(), error = function(...) FALSE
            ))) {
              try(item$process$kill(), silent = TRUE)
              try(item$process$wait(5000L), silent = TRUE)
            }
            records[[item$task$index]] <<- list(
              task = item$task, status = 124L,
              result_path = item$result_path, log_path = item$log_path
            )
            active[[label]] <<- NULL
          }
          break
        }
        Sys.sleep(0.05)
      }
      for (label in finished_labels) {
        item <- active[[label]]
        item$process$wait()
        records[[item$task$index]] <<- list(
          task = item$task,
          status = item$process$get_exit_status(),
          result_path = item$result_path,
          log_path = item$log_path
        )
        active[[label]] <<- NULL
      }
    }
  }

  ordinary_tasks <- Filter(function(task) task$lane == "parallel", tasks)
  exclusive_tasks <- Filter(function(task) task$lane == "exclusive", tasks)
  run_wave(ordinary_tasks, jobs)
  run_wave(exclusive_tasks, 1L)

  for (record in records) {
    cat("\n=== native worker ", sprintf("%03d", record$task$index),
        " (", paste(record$task$files, collapse = ","), ") ===\n", sep = "")
    if (file.exists(record$log_path)) {
      cat(readLines(record$log_path, warn = FALSE), sep = "\n")
      cat("\n")
    }
  }
  bad_status <- vapply(records, function(record) {
    is.null(record) || !identical(record$status, 0L) ||
      !file.exists(record$result_path)
  }, logical(1L))
  if (any(bad_status)) {
    stop(
      sum(bad_status), " isolated native-test worker(s) failed; all task logs retained",
      call. = FALSE
    )
  }

  results_by_file <- setNames(vector("list", length(files)), files)
  evidence <- vector("list", length(records))
  for (index in seq_along(records)) {
    record <- records[[index]]
    if (dir.exists(record$result_path) ||
        (!is.na(Sys.readlink(record$result_path)) &&
          nzchar(Sys.readlink(record$result_path)))) {
      stop("native-test worker result is non-regular or symbolic", call. = FALSE)
    }
    payload <- readRDS(record$result_path)
    if (!is.list(payload) || !identical(payload$schema, 1L) ||
        !identical(payload$selection, selection) ||
        !identical(payload$files, record$task$files) ||
        !identical(payload$dso_path, loaded_dso$path) ||
        !identical(payload$dso_sha256_before, dso_sha256) ||
        !identical(payload$dso_sha256_after, dso_sha256) ||
        !is.list(payload$results) ||
        !identical(names(payload$results), record$task$files) ||
        any(!vapply(payload$results, inherits, logical(1L), "testthat_results"))) {
      stop("native-test worker payload identity differs", call. = FALSE)
    }
    for (file in record$task$files) {
      if (!is.null(results_by_file[[file]])) {
        stop("native-test worker returned one file twice", call. = FALSE)
      }
      results_by_file[[file]] <- payload$results[[file]]
    }
    task_results <- combine_results(payload$results)
    summarized <- testthat:::as.data.frame.testthat_results(task_results)
    evidence[[index]] <- data.frame(
      schema = "1",
      selection = selection,
      jobs = jobs,
      task = sprintf("%03d", record$task$index),
      lane = record$task$lane,
      files = paste(record$task$files, collapse = ","),
      dso_sha256_before = payload$dso_sha256_before,
      dso_sha256_after = payload$dso_sha256_after,
      test_blocks = nrow(summarized),
      expectations = sum(summarized$nb),
      passed = sum(summarized$passed),
      failed = sum(summarized$failed),
      skipped = sum(as.integer(summarized$skipped)),
      errors = sum(as.integer(summarized$error)),
      warnings = sum(summarized$warning),
      status = "complete",
      stringsAsFactors = FALSE
    )
  }
  if (any(vapply(results_by_file, is.null, logical(1L)))) {
    stop("parallel native-test result inventory is incomplete", call. = FALSE)
  }

  evidence <- do.call(rbind, evidence)
  evidence_temporary <- paste0(evidence_path, ".new.", Sys.getpid())
  on.exit(unlink(evidence_temporary), add = TRUE)
  write.table(
    evidence,
    evidence_temporary,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    na = ""
  )
  if (!file.rename(evidence_temporary, evidence_path)) {
    stop("could not publish native-test worker evidence", call. = FALSE)
  }
  evidence_sha256 <- unname(tools::sha256sum(evidence_path))

  # Result payloads and mutable worker homes are transport, not evidence.  The
  # deterministic task ledger and per-task logs remain under artifacts.
  unlink(results_directory, recursive = TRUE, force = TRUE)
  unlink(work_directory, recursive = TRUE, force = TRUE)
  list(
    results = combine_results(results_by_file),
    evidence_sha256 = evidence_sha256,
    effective_jobs = jobs
  )
}

requested_jobs <- Sys.getenv("PARADOX_NATIVE_TEST_JOBS", unset = "")
requested_jobs_value <- 1L
effective_jobs <- 1L
scheduler_backend <- "serial"
parallel_result <- NULL
if (nzchar(requested_jobs)) {
  parsed_jobs <- suppressWarnings(as.integer(requested_jobs))
  if (is.na(parsed_jobs) || parsed_jobs < 1L || parsed_jobs > 16L ||
      !identical(as.character(parsed_jobs), requested_jobs)) {
    stop("PARADOX_NATIVE_TEST_JOBS is not a canonical reviewed job count", call. = FALSE)
  }
  requested_jobs_value <- parsed_jobs
  if (!selection %in% c("focused", "full")) {
    stop("parallel native-test jobs are limited to focused/full tests", call. = FALSE)
  }
  parallel_supported <- identical(Sys.info()[["sysname"]], "Linux") &&
    file.exists("/usr/bin/setsid") && file.access("/usr/bin/setsid", 1L) == 0L
  if (parallel_supported && parsed_jobs > 1L) {
    parallel_result <- run_parallel_tests(selected_files, parsed_jobs)
    effective_jobs <- parallel_result$effective_jobs
    scheduler_backend <- "linux-isolated"
  } else {
    if (parsed_jobs > 1L) scheduler_backend <- "serial-fallback"
    cat("native_parallel_fallback=serial\n")
  }
}

if (is.null(parallel_result)) {
  results <- testthat::test_dir(
    test_directory,
    filter = filter,
    reporter = "summary",
    load_helpers = TRUE,
    # Finish the selected inventory before applying the stop policy, then write
    # the complete result batch.  This prevents an expensive run from exposing
    # only one ordinary expectation failure at a time.
    stop_on_failure = FALSE,
    stop_on_warning = FALSE,
    package = "paradox",
    load_package = "none"
  )
  worker_evidence_sha256 <- NULL
} else {
  results <- parallel_result$results
  worker_evidence_sha256 <- parallel_result$evidence_sha256
}
assert_dso_unchanged(loaded_dso)
write_ledger(
  results,
  selected_files,
  worker_evidence_sha256,
  requested_jobs = requested_jobs_value,
  effective_jobs = effective_jobs,
  scheduler_backend = scheduler_backend
)
testthat:::test_files_check(
  results,
  stop_on_failure = TRUE,
  stop_on_warning = TRUE
)
