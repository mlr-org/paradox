# Shared primitives for the resumable source-package reverse-dependency gate.
#
# This file deliberately separates byte-affecting install inputs from runner,
# reporting, and verification policy.  A verifier edit must not rebuild a
# consumer package whose installed bytes would be identical.

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

rr_fail <- function(...) stop(..., call. = FALSE)

rr_is_symbolic <- function(path) {
  value <- Sys.readlink(path)
  length(value) == 1L && !is.na(value) && nzchar(value)
}

rr_path_type <- function(path) {
  if (!requireNamespace("fs", quietly = TRUE)) {
    rr_fail("fs is required for non-following path inspection")
  }
  info <- tryCatch(
    fs::file_info(path, fail = TRUE, follow = FALSE),
    error = function(condition) rr_fail("could not inspect path: ", path)
  )
  as.character(info$type[[1L]])
}

rr_require_file <- function(path, label = "file") {
  if (!file.exists(path) || rr_is_symbolic(path) ||
      !identical(rr_path_type(path), "file")) {
    rr_fail(label, " is missing, not regular, or symbolic: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

rr_require_directory <- function(path, label = "directory") {
  if (!dir.exists(path) || rr_is_symbolic(path) ||
      !identical(rr_path_type(path), "directory")) {
    rr_fail(label, " is missing, not a directory, or symbolic: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

rr_sha256 <- function(path) unname(tools::sha256sum(rr_require_file(path)))

rr_safe_relative <- function(path) {
  length(path) == 1L && !is.na(path) && nzchar(path) &&
    !startsWith(path, "/") && !grepl("(^|/)\\.\\.(/|$)", path) &&
    !grepl("[[:cntrl:]]", path)
}

rr_read_tsv <- function(path, columns = NULL, allow_empty = FALSE) {
  path <- rr_require_file(path, "TSV input")
  value <- utils::read.delim(
    path, header = TRUE, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!is.null(columns) && !identical(names(value), columns)) {
    rr_fail("unexpected TSV columns in ", path)
  }
  if ((!allow_empty && !nrow(value)) || anyNA(value)) {
    rr_fail("empty or NA-bearing TSV input: ", path)
  }
  value
}

rr_write_tsv <- function(value, path, replace = FALSE) {
  if (!dir.exists(dirname(path))) rr_fail("TSV parent is absent: ", dirname(path))
  temporary <- paste0(path, ".new-", Sys.getpid())
  if (file.exists(temporary) || dir.exists(temporary) || rr_is_symbolic(temporary)) {
    rr_fail("temporary TSV path exists: ", temporary)
  }
  if (!replace && (file.exists(path) || dir.exists(path) || rr_is_symbolic(path))) {
    rr_fail("TSV output exists: ", path)
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.table(
    value, temporary, quote = FALSE, sep = "\t", row.names = FALSE,
    na = "-", fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) rr_fail("could not atomically write ", path)
  invisible(path)
}

rr_write_lines <- function(value, path, replace = FALSE) {
  temporary <- paste0(path, ".new-", Sys.getpid())
  if (file.exists(temporary) || dir.exists(temporary) || rr_is_symbolic(temporary)) {
    rr_fail("temporary output exists: ", temporary)
  }
  if (!replace && (file.exists(path) || dir.exists(path) || rr_is_symbolic(path))) {
    rr_fail("output exists: ", path)
  }
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) rr_fail("could not atomically write ", path)
  invisible(path)
}

rr_process_start_identity <- function(pid) {
  pid <- as.character(pid)
  if (!grepl("^[1-9][0-9]*$", pid) || !dir.exists("/proc")) return("-")
  path <- file.path("/proc", pid, "stat")
  line <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(...) "")
  if (length(line) != 1L || !nzchar(line)) return("-")
  close <- max(gregexpr(")", line, fixed = TRUE)[[1L]])
  if (!is.finite(close) || close < 1L) return("-")
  fields <- strsplit(trimws(substring(line, close + 1L)), "[[:space:]]+")[[1L]]
  # Field 22 (starttime) is the twentieth field after the parenthesized comm.
  if (length(fields) < 20L || !grepl("^[0-9]+$", fields[[20L]])) "-" else {
    fields[[20L]]
  }
}

rr_mutation_lock_columns <- function() {
  c("schema", "pid", "process_start", "token", "created_utc")
}

rr_validate_mutation_lock_owner <- function(lock, expected = NULL) {
  lock <- rr_require_directory(lock, "reverse mutation lock")
  entries <- list.files(lock, all.files = TRUE, no.. = TRUE)
  if (!identical(entries, "owner.tsv")) {
    rr_fail("reverse mutation lock inventory is not exact")
  }
  retained <- rr_read_tsv(
    file.path(lock, "owner.tsv"), rr_mutation_lock_columns()
  )
  valid <- nrow(retained) == 1L && identical(retained$schema, "1") &&
    grepl("^[1-9][0-9]*$", retained$pid) &&
    grepl("^(-|0|[1-9][0-9]*)$", retained$process_start) &&
    grepl("^[0-9a-f]{64}$", retained$token) &&
    grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      retained$created_utc
    )
  if (!valid) rr_fail("reverse mutation lock owner is malformed")
  if (!is.null(expected) && !identical(retained, expected)) {
    rr_fail("reverse mutation lock ownership changed")
  }
  retained
}

rr_mutation_lock_retirement_path <- function(value) {
  if (!is.list(value) || is.null(value$path) || is.null(value$owner) ||
      !is.data.frame(value$owner) || nrow(value$owner) != 1L ||
      !identical(names(value$owner), rr_mutation_lock_columns())) {
    rr_fail("mutation-lock retirement input is malformed")
  }
  parent <- rr_require_directory(dirname(value$path),
    "reverse mutation-lock root")
  name <- basename(value$path)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*[.]lock$", name)) {
    rr_fail("mutation-lock retirement path is unsafe")
  }
  run_id <- sub("[.]lock$", "", name)
  owner <- value$owner
  if (!grepl("^[1-9][0-9]*$", owner$pid) ||
      !grepl("^(-|0|[1-9][0-9]*)$", owner$process_start) ||
      !grepl("^[0-9a-f]{64}$", owner$token)) {
    rr_fail("mutation-lock retirement owner is malformed")
  }
  file.path(parent, paste0(
    ".retired-", run_id, "-", owner$pid, "-", owner$process_start,
    "-", owner$token
  ))
}

rr_acquire_mutation_lock <- function(locks_root, run_id) {
  if (length(run_id) != 1L || !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", run_id)) {
    rr_fail("mutation-lock run id is unsafe")
  }
  if (!dir.exists(locks_root)) {
    created <- dir.create(locks_root, recursive = FALSE, showWarnings = FALSE)
    if (!created && !dir.exists(locks_root)) {
      rr_fail("could not create reverse mutation-lock root")
    }
  }
  locks_root <- rr_require_directory(locks_root, "reverse mutation-lock root")
  lock <- file.path(locks_root, paste0(run_id, ".lock"))
  stale_root <- file.path(locks_root, "stale")
  pid <- as.character(Sys.getpid())
  process_start <- rr_process_start_identity(pid)
  token <- rr_object_sha256(list(
    pid = pid, process_start = process_start, time = Sys.time(),
    nonce = tempfile("reverse-lock-")
  ))
  owner <- data.frame(
    schema = "1", pid = pid, process_start = process_start, token = token,
    created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  for (attempt in 1:2) {
    staging <- file.path(locks_root, paste0(
      ".", run_id, ".new-", pid, "-", token
    ))
    if (!dir.create(staging, recursive = FALSE, showWarnings = FALSE)) {
      rr_fail("could not reserve private mutation-lock publication")
    }
    rr_write_tsv(owner, file.path(staging, "owner.tsv"))
    if (suppressWarnings(file.rename(staging, lock))) {
      return(list(path = lock, owner = owner))
    }
    if (unlink(staging, recursive = TRUE, force = TRUE) != 0L ||
        dir.exists(staging)) {
      rr_fail("could not remove losing mutation-lock publication")
    }
    # The retained owner may have atomically retired the canonical name after
    # our publication lost but before we inspected it.  Retry publication;
    # absence here is a normal release boundary, not malformed lock state.
    if (!file.exists(lock) && !dir.exists(lock) && !rr_is_symbolic(lock)) {
      next
    }
    if (!dir.exists(lock) || rr_is_symbolic(lock) ||
        !identical(rr_path_type(lock), "directory")) {
      rr_fail("reverse mutation lock is unsafe: ", lock)
    }
    retained <- rr_validate_mutation_lock_owner(lock)
    # Signal zero is observation only.  Never signal or terminate a prior
    # coordinator.  A live PID is the retained owner only when its start
    # identity also matches; unavailable identities fail conservatively.
    alive <- isTRUE(rr_pid_is_alive(retained$pid[[1L]]))
    live_start <- if (alive) {
      rr_process_start_identity(retained$pid[[1L]])
    } else "-"
    if (alive && (identical(retained$process_start[[1L]], "-") ||
        identical(live_start, "-") ||
        identical(retained$process_start[[1L]], live_start))) {
      rr_fail("reverse run is already owned by active coordinator pid ",
        retained$pid[[1L]])
    }
    if (!dir.exists(stale_root)) {
      created <- dir.create(stale_root, recursive = FALSE, showWarnings = FALSE)
      if (!created && !dir.exists(stale_root)) {
        rr_fail("could not create stale mutation-lock archive")
      }
    }
    rr_require_directory(stale_root, "stale mutation-lock archive")
    destination <- file.path(stale_root, paste0(
      run_id, "-", retained$pid[[1L]], "-", retained$token[[1L]]
    ))
    if (file.exists(destination) || dir.exists(destination) ||
        rr_is_symbolic(destination) || !file.rename(lock, destination)) {
      rr_fail("could not quarantine stale reverse mutation lock")
    }
  }
  rr_fail("could not acquire reverse mutation lock")
}

rr_release_mutation_lock <- function(value) {
  if (!is.list(value) || is.null(value$path) || is.null(value$owner)) {
    rr_fail("mutation-lock release input is malformed")
  }
  lock <- rr_require_directory(value$path, "reverse mutation lock")
  invisible(rr_validate_mutation_lock_owner(lock, value$owner))
  retirement <- rr_mutation_lock_retirement_path(value)
  if (file.exists(retirement) || dir.exists(retirement) ||
      rr_is_symbolic(retirement)) {
    rr_fail("mutation-lock retirement path already exists")
  }
  if (!suppressWarnings(file.rename(lock, retirement))) {
    rr_fail("could not atomically retire reverse mutation lock")
  }
  # From this point onward the canonical lock name is free.  A crash leaves a
  # complete, token-bound private retirement directory, never an ownerless
  # canonical lock.  Acquirers deliberately ignore such retired directories.
  invisible(rr_validate_mutation_lock_owner(retirement, value$owner))
  if (unlink(retirement, recursive = TRUE, force = TRUE) != 0L ||
      dir.exists(retirement)) {
    rr_fail("could not release reverse mutation lock")
  }
  invisible(TRUE)
}

rr_canonical_payload <- function(value) {
  if (!is.data.frame(value) || is.null(names(value)) ||
      anyNA(names(value)) || any(!nzchar(names(value))) ||
      anyDuplicated(names(value)) ||
      any(!vapply(value, is.character, logical(1L)))) {
    rr_fail("payload must be a named all-character data frame")
  }
  lengths <- vapply(value, length, integer(1L))
  if (length(unique(lengths)) > 1L || any(vapply(value, anyNA, logical(1L)))) {
    rr_fail("payload columns must be equally long and contain no NA")
  }
  list(
    format = "paradox-reverse-character-table-v1",
    names = enc2utf8(as.vector(names(value), mode = "character")),
    columns = lapply(value, function(column) {
      column <- as.vector(column, mode = "character")
      attributes(column) <- NULL
      enc2utf8(column)
    })
  )
}

rr_payload_sha256 <- function(value) {
  temporary <- tempfile("paradox-reverse-payload-")
  on.exit(unlink(temporary), add = TRUE)
  writeBin(
    serialize(rr_canonical_payload(value), NULL, version = 3L, xdr = TRUE),
    temporary
  )
  rr_sha256(temporary)
}

rr_object_sha256 <- function(value) {
  temporary <- tempfile("paradox-reverse-object-")
  on.exit(unlink(temporary), add = TRUE)
  writeBin(serialize(value, NULL, version = 3L, xdr = TRUE), temporary)
  rr_sha256(temporary)
}

rr_reverse_nested_controls <- function() c(
  TESTTHAT_PARALLEL = "false",
  TESTTHAT_IS_PARALLEL = "false",
  TESTTHAT_CPUS = "1",
  MAKEFLAGS = "-j1",
  CMAKE_BUILD_PARALLEL_LEVEL = "1",
  `_R_CHECK_LIMIT_CORES_` = "true",
  MC_CORES = "1",
  R_FUTURE_PLAN = "sequential",
  R_FUTURE_FORK_ENABLE = "false",
  R_FUTURE_AVAILABLECORES_FALLBACK = "1",
  R_PARALLELLY_FORK_ENABLE = "false",
  R_PARALLELLY_AVAILABLECORES_FALLBACK = "1",
  OMP_NUM_THREADS = "1",
  OMP_THREAD_LIMIT = "1",
  OPENBLAS_NUM_THREADS = "1",
  GOTO_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  BLIS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1",
  RCPP_PARALLEL_NUM_THREADS = "1"
)

rr_validate_resource_report <- function(report, profile = "consumer",
                                        require_no_operator = TRUE) {
  fields <- c(
    "schema", "profile", "platform", "online_cpus", "affinity_cpus",
    "cgroup_cpu_limit", "cpu_limit", "cpu_reserve", "cpu_per_job",
    "cpu_jobs", "memory_source", "memory_available_mib",
    "cgroup_memory_available_mib", "memory_reserve_mib",
    "memory_mib_per_job", "memory_jobs", "profile_max_jobs",
    "operator_max_jobs", "jobs"
  )
  if (!is.data.frame(report) || !identical(names(report), c("field", "value")) ||
      !identical(report$field, fields) || anyNA(report) ||
      anyDuplicated(report$field) || !identical(profile, "consumer")) {
    rr_fail("resource scheduler report has an unsupported shape")
  }
  values <- setNames(report$value, report$field)
  positive <- function(name) {
    value <- values[[name]]
    if (!grepl("^[1-9][0-9]{0,8}$", value)) {
      rr_fail("resource scheduler field is not a canonical positive integer: ",
        name)
    }
    as.integer(value)
  }
  nonnegative <- function(name) {
    value <- values[[name]]
    if (!grepl("^(0|[1-9][0-9]{0,8})$", value)) {
      rr_fail("resource scheduler field is not a canonical integer: ", name)
    }
    as.integer(value)
  }
  optional_positive <- function(name, absent) {
    if (identical(values[[name]], absent)) return(Inf)
    positive(name)
  }
  if (!identical(values[["schema"]], "1") ||
      !identical(values[["profile"]], profile) ||
      !grepl("^[A-Za-z][A-Za-z0-9._-]*$", values[["platform"]]) ||
      !grepl("^[a-z_]+(\\+cgroup)?$", values[["memory_source"]])) {
    rr_fail("resource scheduler report has an invalid identity")
  }
  online <- positive("online_cpus")
  affinity <- positive("affinity_cpus")
  cgroup_cpu <- optional_positive("cgroup_cpu_limit", "unlimited")
  cpu_limit <- positive("cpu_limit")
  cpu_reserve <- nonnegative("cpu_reserve")
  cpu_per_job <- positive("cpu_per_job")
  cpu_jobs <- positive("cpu_jobs")
  memory_available <- positive("memory_available_mib")
  cgroup_memory <- optional_positive(
    "cgroup_memory_available_mib", "unlimited"
  )
  memory_reserve <- positive("memory_reserve_mib")
  memory_per_job <- positive("memory_mib_per_job")
  memory_jobs <- positive("memory_jobs")
  profile_max <- optional_positive("profile_max_jobs", "unlimited")
  operator_max <- optional_positive("operator_max_jobs", "none")
  jobs <- positive("jobs")
  expected_cpu_limit <- min(online, affinity, cgroup_cpu)
  expected_cpu_reserve <- if (expected_cpu_limit >= 16L) {
    2L
  } else if (expected_cpu_limit >= 4L) {
    1L
  } else 0L
  expected_cpu_jobs <- max(
    1L, as.integer((expected_cpu_limit - expected_cpu_reserve) %/% cpu_per_job)
  )
  expected_memory_reserve <- max(16384L, memory_available %/% 4L)
  expected_memory_jobs <- as.integer(
    (memory_available - expected_memory_reserve) %/% memory_per_job
  )
  if (is.finite(cgroup_memory) && memory_available > cgroup_memory) {
    rr_fail("resource scheduler memory ceiling exceeds its cgroup headroom")
  }
  if (!identical(cpu_per_job, 2L) || !identical(memory_per_job, 8192L) ||
      !identical(profile_max, 4L) ||
      (isTRUE(require_no_operator) && is.finite(operator_max)) ||
      !identical(cpu_limit, as.integer(expected_cpu_limit)) ||
      !identical(cpu_reserve, expected_cpu_reserve) ||
      !identical(cpu_jobs, expected_cpu_jobs) ||
      !identical(memory_reserve, expected_memory_reserve) ||
      expected_memory_jobs < 1L || !identical(memory_jobs, expected_memory_jobs) ||
      !identical(jobs, as.integer(min(
        cpu_jobs, memory_jobs, profile_max, operator_max
      )))) {
    rr_fail("resource scheduler report is internally inconsistent")
  }
  list(values = values, jobs = jobs)
}

rr_is_utc_timestamp <- function(value) {
  if (length(value) != 1L || is.na(value) ||
      !grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        value
      )) {
    return(FALSE)
  }
  parsed <- suppressWarnings(strptime(
    value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  ))
  !is.na(parsed) && identical(
    format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), value
  )
}

rr_reverse_run_fields <- function() c(
  "schema", "run_id", "started_utc", "root", "max_priority", "plan_only",
  "resume_capable", "install_timeout_seconds", "check_timeout_seconds",
  "worker_timeout_seconds",
  "scheduler_initial_ceiling", "scheduler_initial_jobs",
  "scheduler_initial_selection", "resource_jobs_helper_sha256",
  "resource_jobs_initial_report_sha256",
  "candidate_run_id", "candidate_ref", "candidate_commit", "candidate_tree",
  "candidate_source", "candidate_version",
  "candidate_library", "candidate_package", "candidate_content_sha256",
  "candidate_library_content_sha256", "dependency_library",
  "dependency_library_content_sha256", "r", "r_version", "source_date_epoch",
  "candidate_provenance_sha256", "candidate_source_archive_sha256",
  "candidate_installer_sha256", "candidate_git_authenticator_sha256",
  "inventory_sha256", "cran_snapshot_sha256", "bioconductor_snapshot_sha256",
  "runner_library_sha256", "install_worker_sha256", "wave_worker_sha256",
  "worker_group_sha256",
  "harness_sha256", "evidence_helper_sha256", "evidence_verifier_sha256",
  "reverse_evidence_verifier_sha256",
  "tinytex_manifest_sha256", "tree_receipt_helper_sha256",
  "tinytex_root", "tinytex_archive", "tinytex_archive_sha256",
  "tinytex_tree_receipt_sha256", "pdflatex_sha256", "kpsewhich_sha256",
  "makeindex_sha256", "texi2dvi", "texi2dvi_sha256"
)

rr_reverse_run_sha256_fields <- function() {
  grep("_sha256$", rr_reverse_run_fields(), value = TRUE)
}

rr_validate_reverse_run_metadata <- function(run_table) {
  if (!is.data.frame(run_table) ||
      !identical(names(run_table), c("field", "value")) ||
      !is.character(run_table$field) || !is.character(run_table$value) ||
      anyNA(run_table) ||
      !identical(run_table$field, rr_reverse_run_fields())) {
    rr_fail("reverse run metadata does not have the exact producer schema")
  }
  run <- setNames(run_table$value, run_table$field)
  safe_id <- function(value) {
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", value) &&
      !value %in% c(".", "..")
  }
  positive_integer <- function(value) {
    grepl("^[1-9][0-9]{0,9}$", value) &&
      as.double(value) <= .Machine$integer.max
  }
  positive_scheduler <- function(value) grepl("^[1-9][0-9]{0,8}$", value)
  nonnegative_integer <- function(value) {
    grepl("^(0|[1-9][0-9]{0,9})$", value) &&
      as.double(value) <= .Machine$integer.max
  }
  nonnegative_epoch <- function(value) {
    grepl("^(0|[1-9][0-9]{0,18})$", value)
  }
  timeout_fields <- c(
    "install_timeout_seconds", "check_timeout_seconds",
    "worker_timeout_seconds"
  )
  sha256_fields <- rr_reverse_run_sha256_fields()
  ordinary_sha256_fields <- setdiff(
    sha256_fields, "candidate_library_content_sha256"
  )
  plan_only_candidate_library <- identical(run[["plan_only"]], "TRUE") &&
    identical(run[["candidate_library_content_sha256"]], "not-computed")
  textual_fields <- setdiff(
    names(run), c(
      "schema", "run_id", "started_utc", "max_priority", "plan_only",
      "resume_capable", timeout_fields, "scheduler_initial_ceiling",
      "scheduler_initial_jobs", "scheduler_initial_selection",
      "candidate_run_id", "candidate_commit", "candidate_tree",
      "source_date_epoch", sha256_fields
    )
  )
  valid <- identical(run[["schema"]], "5") &&
    safe_id(run[["run_id"]]) && safe_id(run[["candidate_run_id"]]) &&
    rr_is_utc_timestamp(run[["started_utc"]]) &&
    nonnegative_integer(run[["max_priority"]]) &&
    run[["plan_only"]] %in% c("TRUE", "FALSE") &&
    identical(run[["resume_capable"]], "true") &&
    all(vapply(
      unname(run[timeout_fields]), positive_integer, logical(1L)
    )) &&
    as.double(run[["worker_timeout_seconds"]]) ==
      as.double(run[["install_timeout_seconds"]]) +
        as.double(run[["check_timeout_seconds"]]) + 900 &&
    positive_scheduler(run[["scheduler_initial_ceiling"]]) &&
    positive_scheduler(run[["scheduler_initial_jobs"]]) &&
    as.integer(run[["scheduler_initial_jobs"]]) <=
      as.integer(run[["scheduler_initial_ceiling"]]) &&
    run[["scheduler_initial_selection"]] %in% c(
      "automatic_resource_ceiling", "operator_conservative_override"
    ) &&
    grepl("^[0-9a-f]{40}$", run[["candidate_commit"]]) &&
    grepl("^[0-9a-f]{40}$", run[["candidate_tree"]]) &&
    nonnegative_epoch(run[["source_date_epoch"]]) &&
    all(grepl("^[0-9a-f]{64}$", unname(run[ordinary_sha256_fields]))) &&
    (plan_only_candidate_library || grepl(
      "^[0-9a-f]{64}$", run[["candidate_library_content_sha256"]]
    )) &&
    all(nzchar(unname(run[textual_fields]))) &&
    !any(grepl("[[:cntrl:]]", unname(run[textual_fields])))
  if (!isTRUE(valid)) {
    rr_fail("reverse run metadata has an unsupported identity or value")
  }
  run
}

rr_reverse_completion_fields <- function(plan_only = FALSE) {
  if (isTRUE(plan_only)) {
    c("status", "packages", "finished_utc")
  } else {
    c(
      "status", "packages_planned", "packages_completed",
      "candidate_content_sha256", "candidate_library_content_sha256",
      "dependency_library_content_sha256", "results_sha256",
      "acceptance_ledger_sha256", "wave_ledger_sha256", "waves_completed",
      "protected_library_full_hash_passes", "finished_utc"
    )
  }
}

rr_validate_reverse_completion_metadata <- function(completion_table, run) {
  if (!is.character(run) ||
      !identical(names(run), rr_reverse_run_fields())) {
    rr_fail("reverse completion lacks exact run metadata")
  }
  plan_only <- identical(run[["plan_only"]], "TRUE")
  expected_fields <- rr_reverse_completion_fields(plan_only)
  if (!is.data.frame(completion_table) ||
      !identical(names(completion_table), c("field", "value")) ||
      !is.character(completion_table$field) ||
      !is.character(completion_table$value) || anyNA(completion_table) ||
      !identical(completion_table$field, expected_fields)) {
    rr_fail("reverse completion metadata does not have the exact producer schema")
  }
  completion <- setNames(completion_table$value, completion_table$field)
  nonnegative <- function(value) grepl("^(0|[1-9][0-9]{0,18})$", value)
  if (!rr_is_utc_timestamp(completion[["finished_utc"]])) {
    rr_fail("reverse completion timestamp is malformed")
  }
  if (plan_only) {
    if (!identical(completion[["status"]], "planned") ||
        !nonnegative(completion[["packages"]])) {
      rr_fail("plan-only reverse completion metadata is malformed")
    }
    return(completion)
  }
  identity_fields <- c(
    "candidate_content_sha256", "candidate_library_content_sha256",
    "dependency_library_content_sha256"
  )
  retained_hash_fields <- c(
    identity_fields, "results_sha256", "acceptance_ledger_sha256",
    "wave_ledger_sha256"
  )
  if (!completion[["status"]] %in% c("passed", "completed_with_failures") ||
      !nonnegative(completion[["packages_planned"]]) ||
      !nonnegative(completion[["packages_completed"]]) ||
      !nonnegative(completion[["waves_completed"]]) ||
      !identical(completion[["protected_library_full_hash_passes"]], "2") ||
      any(!grepl("^[0-9a-f]{64}$", unname(completion[retained_hash_fields]))) ||
      !identical(unname(completion[identity_fields]),
        unname(run[identity_fields]))) {
    rr_fail("reverse completion metadata has a malformed or changed identity")
  }
  completion
}

# One content read yields the complete-tree identity and, when requested, a
# package-subtree identity with the same serialization contract as
# compat/fingerprint.R.  This prevents candidate-package authentication from
# becoming a hidden third pass over protected bytes.
rr_tree_content_receipt <- function(path, subtree = NULL) {
  path <- rr_require_directory(path, "content tree")
  files <- list.files(
    path, all.files = TRUE, full.names = TRUE, recursive = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  info <- file.info(files, extra_cols = FALSE)
  keep <- !is.na(info$isdir) & !info$isdir
  files <- files[keep]
  info <- info[keep, , drop = FALSE]
  relative <- substring(files, nchar(path) + 2L)
  ordering <- order(relative, method = "radix")
  inventory <- data.frame(
    path = relative[ordering],
    size = as.numeric(info$size[ordering]),
    sha256 = unname(tools::sha256sum(files[ordering])),
    stringsAsFactors = FALSE
  )
  hash_inventory <- function(value) {
    temporary <- tempfile("paradox-reverse-content-")
    on.exit(unlink(temporary), add = TRUE)
    payload <- list(
      path = value$path,
      size = unname(value$size),
      sha256 = unname(value$sha256)
    )
    writeBin(serialize(payload, NULL, version = 3L, xdr = TRUE), temporary)
    rr_sha256(temporary)
  }
  full_hash <- hash_inventory(inventory)
  subtree_hash <- NULL
  if (!is.null(subtree)) {
    if (!rr_safe_relative(subtree) || grepl("/", subtree, fixed = TRUE)) {
      rr_fail("content subtree must be one safe top-level name")
    }
    prefix <- paste0(subtree, "/")
    selected <- startsWith(inventory$path, prefix)
    if (!any(selected)) rr_fail("content subtree is absent: ", subtree)
    subset <- inventory[selected, , drop = FALSE]
    subset$path <- substring(subset$path, nchar(prefix) + 1L)
    subtree_hash <- hash_inventory(subset)
  }
  list(hash = full_hash, subtree_hash = subtree_hash, files = nrow(inventory))
}

rr_tree_content_sha256 <- function(path) rr_tree_content_receipt(path)$hash

# lstat-style inventory.  The relative path is part of every record, so
# swapping metadata between files cannot preserve the fingerprint.  Symbolic
# links are not followed; their link text is recorded instead.
rr_metadata_entries <- function(directory, tree_name) {
  directory <- rr_require_directory(directory, paste(tree_name, "tree"))
  if (!requireNamespace("fs", quietly = TRUE)) {
    rr_fail("fs is required for non-following protected-tree metadata")
  }
  paths <- as.character(fs::dir_ls(
    directory, all = TRUE, recurse = TRUE, type = "any", fail = TRUE
  ))
  paths <- c(directory, paths)
  info <- fs::file_info(paths, fail = TRUE, follow = FALSE)
  types <- as.character(info$type)
  if (anyNA(types) || any(!types %in% c("file", "directory", "symlink"))) {
    rr_fail(tree_name, " tree contains an unsupported path type")
  }
  relative <- c(".", substring(paths[-1L], nchar(directory) + 2L))
  if (any(relative[-1L] == "") || any(!vapply(
      relative[-1L], rr_safe_relative, logical(1L)
    )) || anyDuplicated(relative)) {
    rr_fail(tree_name, " tree contains an unsafe or duplicate path")
  }
  ordering <- order(relative, method = "radix")
  relative <- relative[ordering]
  paths <- paths[ordering]
  info <- info[ordering, , drop = FALSE]
  types <- types[ordering]
  number <- function(value) format(
    as.numeric(value), scientific = FALSE, trim = TRUE, digits = 17L
  )
  file_rows <- types == "file"
  sizes <- rep("-", length(paths))
  sizes[file_rows] <- number(info$size[file_rows])
  link_target <- rep("-", length(paths))
  symbolic <- types == "symlink"
  if (any(symbolic)) {
    link_target[symbolic] <- vapply(paths[symbolic], Sys.readlink, character(1L))
    if (any(!nzchar(link_target[symbolic])) ||
        any(grepl("[[:cntrl:]]", link_target[symbolic]))) {
      rr_fail(tree_name, " tree contains an unsafe symbolic-link target")
    }
  }
  hard_link_identity <- rep("-", length(paths))
  linked <- file_rows & as.numeric(info$hard_links) > 1
  hard_link_identity[linked] <- paste0(
    number(info$device_id[linked]), ":", number(info$inode[linked])
  )
  data.frame(
    tree = tree_name,
    path = relative,
    type = types,
    mode = sprintf("%04o", as.integer(info$permissions)),
    size = sizes,
    mtime = number(info$modification_time),
    ctime = number(info$change_time),
    device = number(info$device_id),
    inode = number(info$inode),
    hard_links = number(info$hard_links),
    hard_link_identity = hard_link_identity,
    link_target = link_target,
    stringsAsFactors = FALSE
  )
}

rr_protected_metadata <- function(candidate_library, dependency_library) {
  candidate <- rr_metadata_entries(candidate_library, "candidate_library")
  dependency <- rr_metadata_entries(dependency_library, "dependency_library")
  combined <- rbind(candidate, dependency)
  list(
    candidate = rr_payload_sha256(candidate),
    dependency = rr_payload_sha256(dependency),
    combined = rr_payload_sha256(combined),
    candidate_entries = nrow(candidate),
    dependency_entries = nrow(dependency)
  )
}

rr_validate_protected_metadata <- function(observed, expected, context) {
  fields <- c(
    "candidate", "dependency", "combined", "candidate_entries",
    "dependency_entries"
  )
  if (!identical(unname(as.character(observed[fields])),
      unname(as.character(expected[fields])))) {
    rr_fail("protected-library metadata changed ", context)
  }
  invisible(observed)
}

rr_row_cache_environment <- function(package_directory) {
  package_directory <- normalizePath(
    package_directory, winslash = "/", mustWork = TRUE
  )
  c(
    XDG_RUNTIME_DIR = file.path(package_directory, "runtime"),
    XDG_CACHE_HOME = file.path(package_directory, "cache"),
    R_USER_CACHE_DIR = file.path(package_directory, "cache", "R"),
    PYTHONDONTWRITEBYTECODE = "1",
    PYTHONPYCACHEPREFIX = file.path(package_directory, "cache", "python-bytecode"),
    RETICULATE_MINICONDA_PATH = file.path(
      package_directory, "cache", "reticulate", "miniconda"
    ),
    RETICULATE_VIRTUALENV_ROOT = file.path(
      package_directory, "cache", "reticulate", "virtualenvs"
    ),
    CCACHE_DIR = file.path(package_directory, "cache", "ccache"),
    CCACHE_TEMPDIR = file.path(package_directory, "tmp", "ccache"),
    PIP_CACHE_DIR = file.path(package_directory, "cache", "pip"),
    UV_CACHE_DIR = file.path(package_directory, "cache", "uv")
  )
}

rr_full_hash_columns <- function() c(
  "ordinal", "context", "candidate_library_content_sha256",
  "dependency_library_content_sha256", "started_utc", "finished_utc"
)

rr_empty_full_hash_ledger <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(rr_full_hash_columns())),
      rr_full_hash_columns()),
    stringsAsFactors = FALSE
  )
}

rr_read_full_hash_ledger <- function(path) {
  rr_read_tsv(path, rr_full_hash_columns(), allow_empty = TRUE)
}

# `hasher` is injectable solely so the deterministic self-test can prove that
# plan/self-test paths make zero expensive calls.  Production passes the
# authenticated complete-tree content hasher from compat/fingerprint.R.
rr_record_full_hash_pass <- function(path, context, expected_contexts,
                                     candidate_library, dependency_library,
                                     expected = NULL, hasher) {
  ledger <- rr_read_full_hash_ledger(path)
  ordinal <- nrow(ledger) + 1L
  if (ordinal > length(expected_contexts) ||
      !identical(context, expected_contexts[[ordinal]])) {
    rr_fail("unexpected or repeated protected-library full hash pass: ", context)
  }
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  observed <- c(
    candidate_library_content_sha256 = hasher(candidate_library),
    dependency_library_content_sha256 = hasher(dependency_library)
  )
  finished <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (!is.null(expected) && !identical(unname(observed), unname(expected))) {
    rr_fail("protected-library content mismatch during ", context)
  }
  row <- data.frame(
    ordinal = as.character(ordinal), context = context,
    candidate_library_content_sha256 = observed[[1L]],
    dependency_library_content_sha256 = observed[[2L]],
    started_utc = started, finished_utc = finished,
    stringsAsFactors = FALSE
  )
  rr_write_tsv(rbind(ledger, row), path, replace = TRUE)
  observed
}

rr_validate_full_hash_ledger <- function(path, expected_contexts,
                                         expected = NULL) {
  ledger <- rr_read_full_hash_ledger(path)
  valid <- nrow(ledger) == length(expected_contexts) &&
    identical(ledger$ordinal, as.character(seq_along(expected_contexts))) &&
    identical(ledger$context, expected_contexts) &&
    all(grepl("^[0-9a-f]{64}$", ledger$candidate_library_content_sha256)) &&
    all(grepl("^[0-9a-f]{64}$", ledger$dependency_library_content_sha256))
  if (!valid) rr_fail("protected-library full-hash ledger is incomplete or malformed")
  if (!is.null(expected) && nrow(ledger) && (
      any(ledger$candidate_library_content_sha256 != expected[[1L]]) ||
      any(ledger$dependency_library_content_sha256 != expected[[2L]])
    )) {
    rr_fail("protected-library full-hash ledger disagrees with stage identity")
  }
  invisible(ledger)
}

rr_stage_manifest_relative <- file.path("metadata", "evidence-manifest.tsv")
rr_stage_directories_relative <- file.path("metadata", "directories.tsv")
rr_stage_seal_relative <- file.path("metadata", "completion.seal")
rr_stage_file_limit_bytes <- 512 * 1024^2
rr_log_file_limit_bytes <- 256 * 1024^2
rr_log_sample_bytes <- 1024^2

rr_stage_entries <- function(stage, label = "evidence stage") {
  stage <- rr_require_directory(stage, label)
  entries <- as.character(fs::dir_ls(
    stage, all = TRUE, recurse = TRUE, type = "any", fail = TRUE
  ))
  if (length(entries) > 250000L) {
    rr_fail(label, " contains a pathological number of entries")
  }
  if (!length(entries)) {
    return(list(paths = character(), types = character(), info = NULL))
  }
  info <- fs::file_info(entries, fail = TRUE, follow = FALSE)
  types <- as.character(info$type)
  if (anyNA(types) || any(!types %in% c("file", "directory"))) {
    rr_fail(label, " contains a symbolic or non-regular path")
  }
  relative <- substring(entries, nchar(stage) + 2L)
  if (any(!vapply(relative, rr_safe_relative, logical(1L))) ||
      anyDuplicated(relative)) {
    rr_fail(label, " contains an unsafe or duplicate path")
  }
  list(paths = entries, relative = relative, types = types, info = info)
}

rr_stage_inventory <- function(stage) {
  stage <- rr_require_directory(stage, "evidence stage")
  entries <- rr_stage_entries(stage)
  selected <- entries$types == "file"
  files <- entries$paths[selected]
  relative <- entries$relative[selected]
  sizes <- as.numeric(entries$info$size[selected])
  if (length(sizes) && (anyNA(sizes) ||
      any(sizes > rr_stage_file_limit_bytes))) {
    rr_fail("evidence stage contains a pathologically large file")
  }
  keep <- !relative %in% c(rr_stage_manifest_relative, rr_stage_seal_relative)
  files <- files[keep]
  relative <- relative[keep]
  ordering <- order(relative, method = "radix")
  data.frame(
    path = relative[ordering],
    size = format(sizes[keep][ordering],
      scientific = FALSE, trim = TRUE),
    sha256 = unname(tools::sha256sum(files[ordering])),
    stringsAsFactors = FALSE
  )
}

rr_stage_directories <- function(stage) {
  stage <- rr_require_directory(stage, "evidence stage")
  entries <- rr_stage_entries(stage)
  relative <- entries$relative[entries$types == "directory"]
  data.frame(path = sort(relative, method = "radix"), stringsAsFactors = FALSE)
}

rr_pid_is_alive <- function(pid) {
  if (length(pid) != 1L || is.na(pid) || !grepl("^[1-9][0-9]*$", pid)) {
    return(NA)
  }
  status <- suppressWarnings(system2(
    "/bin/kill", c("-0", pid), stdout = FALSE, stderr = FALSE
  ))
  identical(status, 0L)
}

rr_recover_publication_temporaries <- function(paths) {
  for (path in paths) {
    parent <- rr_require_directory(dirname(path), "receipt parent")
    prefix <- paste0(basename(path), ".new-")
    candidates <- list.files(parent, all.files = TRUE, full.names = TRUE,
      no.. = TRUE)
    candidates <- candidates[startsWith(basename(candidates), prefix)]
    for (candidate in candidates) {
      owner <- substring(basename(candidate), nchar(prefix) + 1L)
      alive <- rr_pid_is_alive(owner)
      if (is.na(alive) || alive || !identical(rr_path_type(candidate), "file")) {
        rr_fail("unsafe or live interrupted receipt publication: ", candidate)
      }
      if (unlink(candidate, force = TRUE) != 0L || file.exists(candidate)) {
        rr_fail("could not remove stale receipt publication: ", candidate)
      }
    }
  }
  invisible(TRUE)
}

rr_publish_expected_tsv <- function(value, path, columns) {
  if (file.exists(path) || dir.exists(path) || rr_is_symbolic(path)) {
    if (!identical(rr_read_tsv(path, columns, allow_empty = TRUE), value)) {
      rr_fail("partial receipt publication disagrees with current evidence: ", path)
    }
  } else {
    rr_write_tsv(value, path)
  }
  invisible(path)
}

rr_seal_stage <- function(stage) {
  stage <- rr_require_directory(stage, "evidence stage")
  rr_require_directory(file.path(stage, "metadata"), "evidence metadata")
  paths <- file.path(stage, c(
    rr_stage_manifest_relative, rr_stage_directories_relative,
    rr_stage_seal_relative
  ))
  rr_recover_publication_temporaries(paths)
  if (file.exists(paths[[3L]]) || dir.exists(paths[[3L]]) ||
      rr_is_symbolic(paths[[3L]])) {
    return(rr_verify_stage(stage))
  }
  if ((file.exists(paths[[1L]]) || dir.exists(paths[[1L]]) ||
       rr_is_symbolic(paths[[1L]])) && !file.exists(paths[[2L]])) {
    rr_fail("manifest publication exists without its directory inventory")
  }
  rr_publish_expected_tsv(
    rr_stage_directories(stage), paths[[2L]], "path"
  )
  inventory <- rr_stage_inventory(stage)
  if (!nrow(inventory)) rr_fail("cannot seal an empty evidence stage")
  rr_publish_expected_tsv(inventory, paths[[1L]], c("path", "size", "sha256"))
  expected_seal <- paste0("evidence_manifest_sha256=", rr_sha256(paths[[1L]]))
  if (file.exists(paths[[3L]]) || dir.exists(paths[[3L]]) ||
      rr_is_symbolic(paths[[3L]])) {
    if (!identical(readLines(rr_require_file(paths[[3L]]), warn = FALSE),
        expected_seal)) rr_fail("partial evidence seal publication is invalid")
  } else {
    rr_write_lines(expected_seal, paths[[3L]])
  }
  list(
    manifest_sha256 = rr_sha256(paths[[1L]]),
    seal_sha256 = rr_sha256(paths[[3L]]), files = nrow(inventory)
  )
}

rr_verify_stage <- function(stage) {
  stage <- rr_require_directory(stage, "sealed evidence stage")
  manifest_path <- rr_require_file(
    file.path(stage, rr_stage_manifest_relative), "evidence manifest"
  )
  directories_path <- rr_require_file(
    file.path(stage, rr_stage_directories_relative), "directory inventory"
  )
  seal_path <- rr_require_file(
    file.path(stage, rr_stage_seal_relative), "evidence seal"
  )
  if (!identical(readLines(seal_path, warn = FALSE),
      paste0("evidence_manifest_sha256=", rr_sha256(manifest_path)))) {
    rr_fail("evidence seal mismatch: ", stage)
  }
  manifest <- rr_read_tsv(manifest_path, c("path", "size", "sha256"))
  if (anyDuplicated(manifest$path) ||
      !identical(manifest$path, sort(manifest$path, method = "radix")) ||
      any(!vapply(manifest$path, rr_safe_relative, logical(1L))) ||
      any(!grepl("^(0|[1-9][0-9]*)$", manifest$size)) ||
      any(!grepl("^[0-9a-f]{64}$", manifest$sha256)) ||
      !identical(manifest, rr_stage_inventory(stage))) {
    rr_fail("sealed evidence contents changed: ", stage)
  }
  directories <- rr_read_tsv(directories_path, "path", allow_empty = TRUE)
  if (anyDuplicated(directories$path) ||
      (nrow(directories) && any(!vapply(
        directories$path, rr_safe_relative, logical(1L)
      ))) || !identical(directories, rr_stage_directories(stage))) {
    rr_fail("sealed evidence directory set changed: ", stage)
  }
  list(
    manifest_sha256 = rr_sha256(manifest_path),
    seal_sha256 = rr_sha256(seal_path), files = nrow(manifest)
  )
}

rr_composite_children <- function(stage) {
  roots <- file.path(stage, c("packages", "interrupted"))
  children <- unlist(lapply(roots, function(root) {
    if (!dir.exists(root)) return(character())
    root <- rr_require_directory(root, "composite child root")
    entries <- as.character(fs::dir_ls(
      root, all = TRUE, recurse = FALSE, type = "any", fail = TRUE
    ))
    if (length(entries)) {
      types <- as.character(fs::file_info(
        entries, fail = TRUE, follow = FALSE
      )$type)
      if (any(types != "directory")) {
        rr_fail("composite child root contains a non-directory entry")
      }
    }
    entries
  }), use.names = FALSE)
  if (length(children)) {
    relative <- substring(children, nchar(stage) + 2L)
    if (any(!vapply(relative, rr_safe_relative, logical(1L))) ||
        any(vapply(children, rr_is_symbolic, logical(1L)))) {
      rr_fail("composite evidence has an unsafe child stage")
    }
    children <- children[order(relative, method = "radix")]
  }
  children
}

rr_validate_composite_children <- function(stage, plan_packages = NULL,
                                            accepted_packages = NULL,
                                            allow_interrupted = TRUE) {
  stage <- rr_require_directory(stage, "composite evidence stage")
  children <- rr_composite_children(stage)
  relative <- substring(children, nchar(stage) + 2L)
  package_names <- basename(relative[startsWith(relative, "packages/")])
  interrupted_names <- basename(relative[startsWith(relative, "interrupted/")])
  if (length(package_names) && any(!grepl(
      "^[A-Za-z][A-Za-z0-9.]*$", package_names))) {
    rr_fail("composite package child inventory has an invalid name")
  }
  interrupted_pattern <- "^([A-Za-z][A-Za-z0-9.]*)-attempt-([1-9][0-9]*)$"
  matches <- regexec(interrupted_pattern, interrupted_names)
  captures <- regmatches(interrupted_names, matches)
  if (length(interrupted_names) && any(lengths(captures) != 3L)) {
    rr_fail("composite interrupted child inventory has an invalid name")
  }
  interrupted_packages <- if (length(captures)) {
    vapply(captures, `[[`, character(1L), 2L)
  } else character()
  interrupted_attempts <- if (length(captures)) {
    as.integer(vapply(captures, `[[`, character(1L), 3L))
  } else integer()
  if (!isTRUE(allow_interrupted) && length(interrupted_names)) {
    rr_fail("composite evidence unexpectedly contains interrupted rows")
  }
  if (length(interrupted_packages)) {
    interrupted_children <- children[startsWith(relative, "interrupted/")]
    for (index in seq_along(interrupted_children)) {
      completion <- rr_read_tsv(file.path(
        interrupted_children[[index]], "metadata", "completion.tsv"
      ), c("field", "value"))
      values <- setNames(completion$value, completion$field)
      if (!identical(completion$field, c(
          "status", "package", "diagnostics", "recorded_utc"
        )) || !identical(values[["status"]], "interrupted") ||
          !identical(values[["package"]], interrupted_packages[[index]]) ||
          !grepl("^(0|[1-9][0-9]*)$", values[["diagnostics"]]) ||
          !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T", values[["recorded_utc"]])) {
        rr_fail("interrupted child completion has an invalid schema")
      }
    }
    for (package in unique(interrupted_packages)) {
      observed <- sort(interrupted_attempts[interrupted_packages == package])
      if (!identical(observed, seq_along(observed))) {
        rr_fail("interrupted child attempts are not contiguous: ", package)
      }
    }
  }
  if (!is.null(plan_packages)) {
    if (anyDuplicated(plan_packages) ||
        any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", plan_packages)) ||
        any(!package_names %in% plan_packages) ||
        any(!interrupted_packages %in% plan_packages)) {
      rr_fail("composite child inventory contains an unplanned package")
    }
  }
  if (!is.null(accepted_packages) &&
      (!setequal(package_names, accepted_packages) ||
       length(package_names) != length(accepted_packages))) {
    rr_fail("composite package children differ from accepted rows")
  }
  list(
    children = children, packages = package_names,
    interrupted = interrupted_names, interrupted_packages = interrupted_packages
  )
}

# The run-level seal binds each already-verified row by its manifest and seal,
# rather than hashing every retained Rcheck artifact for a second time.
rr_composite_inventory <- function(stage) {
  stage <- rr_require_directory(stage, "composite evidence stage")
  children <- rr_validate_composite_children(stage)$children
  invisible(lapply(children, rr_verify_stage))
  entries <- rr_stage_entries(stage, "composite evidence")
  selected <- entries$types == "file"
  files <- entries$paths[selected]
  relative <- entries$relative[selected]
  sizes <- as.numeric(entries$info$size[selected])
  if (length(sizes) && (anyNA(sizes) ||
      any(sizes > rr_stage_file_limit_bytes))) {
    rr_fail("composite evidence contains a pathologically large receipt")
  }
  inside_child <- startsWith(relative, "packages/") |
    startsWith(relative, "interrupted/")
  receipt_suffixes <- c(
    rr_stage_manifest_relative, rr_stage_directories_relative,
    rr_stage_seal_relative
  )
  child_receipt <- inside_child & vapply(relative, function(path) {
    any(endsWith(path, paste0("/", receipt_suffixes)))
  }, logical(1L))
  excluded_top <- relative %in% c(
    rr_stage_manifest_relative, rr_stage_seal_relative
  )
  keep <- (!inside_child | child_receipt) & !excluded_top
  files <- files[keep]
  relative <- relative[keep]
  ordering <- order(relative, method = "radix")
  data.frame(
    path = relative[ordering],
    size = format(sizes[keep][ordering],
      scientific = FALSE, trim = TRUE),
    sha256 = unname(tools::sha256sum(files[ordering])),
    stringsAsFactors = FALSE
  )
}

rr_composite_directories <- function(stage) {
  stage <- rr_require_directory(stage, "composite evidence stage")
  entries <- rr_stage_entries(stage, "composite evidence")
  relative <- entries$relative[entries$types == "directory"]
  pieces <- strsplit(relative, "/", fixed = TRUE)
  keep <- vapply(pieces, function(piece) {
    length(piece) <= 2L || !piece[[1L]] %in% c("packages", "interrupted")
  }, logical(1L))
  relative <- relative[keep]
  data.frame(path = sort(relative, method = "radix"), stringsAsFactors = FALSE)
}

rr_seal_composite_stage <- function(stage) {
  stage <- rr_require_directory(stage, "composite evidence stage")
  paths <- file.path(stage, c(
    rr_stage_manifest_relative, rr_stage_directories_relative,
    rr_stage_seal_relative
  ))
  rr_recover_publication_temporaries(paths)
  if (file.exists(paths[[3L]]) || dir.exists(paths[[3L]]) ||
      rr_is_symbolic(paths[[3L]])) {
    return(rr_verify_composite_stage(stage))
  }
  if ((file.exists(paths[[1L]]) || dir.exists(paths[[1L]]) ||
       rr_is_symbolic(paths[[1L]])) && !file.exists(paths[[2L]])) {
    rr_fail("composite manifest exists without its directory inventory")
  }
  rr_publish_expected_tsv(
    rr_composite_directories(stage), paths[[2L]], "path"
  )
  inventory <- rr_composite_inventory(stage)
  if (!nrow(inventory)) rr_fail("cannot seal empty composite evidence")
  rr_publish_expected_tsv(inventory, paths[[1L]], c("path", "size", "sha256"))
  expected_seal <- paste0("evidence_manifest_sha256=", rr_sha256(paths[[1L]]))
  if (file.exists(paths[[3L]]) || dir.exists(paths[[3L]]) ||
      rr_is_symbolic(paths[[3L]])) {
    if (!identical(readLines(rr_require_file(paths[[3L]]), warn = FALSE),
        expected_seal)) rr_fail("partial composite seal publication is invalid")
  } else {
    rr_write_lines(expected_seal, paths[[3L]])
  }
  list(
    manifest_sha256 = rr_sha256(paths[[1L]]),
    seal_sha256 = rr_sha256(paths[[3L]]), rows = length(rr_composite_children(stage))
  )
}

rr_verify_composite_stage <- function(stage) {
  stage <- rr_require_directory(stage, "sealed composite evidence")
  manifest_path <- rr_require_file(
    file.path(stage, rr_stage_manifest_relative), "composite manifest"
  )
  directories_path <- rr_require_file(
    file.path(stage, rr_stage_directories_relative), "composite directories"
  )
  seal_path <- rr_require_file(
    file.path(stage, rr_stage_seal_relative), "composite seal"
  )
  if (!identical(readLines(seal_path, warn = FALSE),
      paste0("evidence_manifest_sha256=", rr_sha256(manifest_path)))) {
    rr_fail("composite evidence seal mismatch")
  }
  manifest <- rr_read_tsv(manifest_path, c("path", "size", "sha256"))
  if (!identical(manifest, rr_composite_inventory(stage))) {
    rr_fail("composite evidence contents changed")
  }
  directories <- rr_read_tsv(directories_path, "path", allow_empty = TRUE)
  if (!identical(directories, rr_composite_directories(stage))) {
    rr_fail("composite evidence directory set changed")
  }
  list(
    manifest_sha256 = rr_sha256(manifest_path),
    seal_sha256 = rr_sha256(seal_path), rows = length(rr_composite_children(stage))
  )
}

rr_install_cache_key <- function(inputs) {
  if (!is.data.frame(inputs) ||
      !identical(names(inputs), c("field", "value")) ||
      any(!vapply(inputs, is.character, logical(1L))) || anyNA(inputs) ||
      anyDuplicated(inputs$field) || any(!nzchar(inputs$field)) ||
      !identical(inputs$field[[1L]], "schema") ||
      !identical(inputs$value[[1L]], "1")) {
    rr_fail("install-cache inputs have an invalid schema")
  }
  forbidden <- c("runner", "verifier", "report", "plan", "row_order")
  if (any(vapply(forbidden, function(word) {
      any(grepl(paste0("(^|[.])", word, "([.]|$)"), inputs$field))
    }, logical(1L)))) {
    rr_fail("install-cache key contains verifier/report-only inputs")
  }
  rr_payload_sha256(inputs)
}

rr_installed_hash_from_manifest <- function(manifest, package) {
  prefix <- paste0("library/", package, "/")
  selected <- startsWith(manifest$path, prefix)
  if (!any(selected)) rr_fail("install-cache manifest lacks installed package")
  relative <- substring(manifest$path[selected], nchar(prefix) + 1L)
  if (any(!vapply(relative, rr_safe_relative, logical(1L))) ||
      anyDuplicated(relative)) {
    rr_fail("install-cache package manifest is unsafe")
  }
  ordering <- order(relative, method = "radix")
  payload <- list(
    path = relative[ordering],
    size = as.numeric(manifest$size[selected][ordering]),
    sha256 = manifest$sha256[selected][ordering]
  )
  temporary <- tempfile("paradox-reverse-installed-hash-")
  on.exit(unlink(temporary), add = TRUE)
  writeBin(serialize(payload, NULL, version = 3L, xdr = TRUE), temporary)
  rr_sha256(temporary)
}

rr_validate_install_cache <- function(target, expected_key, expected_inputs,
                                      package, version, content_hasher) {
  target <- rr_require_directory(target, "install cache")
  if (!identical(basename(target), expected_key) ||
      !grepl("^[0-9a-f]{64}$", expected_key)) {
    rr_fail("install-cache path does not match its key")
  }
  evidence <- rr_verify_stage(target)
  retained_inputs <- rr_read_tsv(
    file.path(target, "metadata", "cache-inputs.tsv"), c("field", "value")
  )
  if (!identical(retained_inputs, expected_inputs) ||
      !identical(rr_install_cache_key(retained_inputs), expected_key)) {
    rr_fail("install-cache input projection or key changed")
  }
  completion_table <- rr_read_tsv(
    file.path(target, "metadata", "completion.tsv"), c("field", "value")
  )
  expected_fields <- c(
    "status", "cache_key", "package", "version",
    "installed_content_sha256", "finished_utc"
  )
  if (anyDuplicated(completion_table$field) ||
      !identical(completion_table$field, expected_fields)) {
    rr_fail("install-cache completion schema changed")
  }
  completion <- setNames(completion_table$value, completion_table$field)
  installed <- rr_require_directory(
    file.path(target, "library", package), "cached installed package"
  )
  installed_version <- as.character(utils::packageVersion(
    package, lib.loc = file.path(target, "library")
  ))
  manifest <- rr_read_tsv(
    file.path(target, rr_stage_manifest_relative), c("path", "size", "sha256")
  )
  installed_content <- rr_installed_hash_from_manifest(manifest, package)
  if (!identical(completion[["status"]], "installed") ||
      !identical(completion[["cache_key"]], expected_key) ||
      !identical(completion[["package"]], package) ||
      !identical(completion[["version"]], version) ||
      !identical(installed_version, version) ||
      !identical(completion[["installed_content_sha256"]], installed_content)) {
    rr_fail("install-cache semantic completion disagrees with installed bytes")
  }
  list(
    target = target, library = file.path(target, "library"),
    install_log = rr_require_file(file.path(target, "install.log"),
      "cached install log"),
    installed_content_sha256 = installed_content,
    manifest_sha256 = evidence$manifest_sha256,
    seal_sha256 = evidence$seal_sha256
  )
}

rr_quarantine_unsealed_row <- function(package_directory, interrupted_root,
                                       package) {
  if (!dir.exists(package_directory) || rr_is_symbolic(package_directory)) {
    rr_fail("interrupted package row is absent or symbolic: ", package)
  }
  if (!dir.exists(interrupted_root) &&
      !dir.create(interrupted_root, recursive = FALSE, showWarnings = FALSE)) {
    rr_fail("could not create interrupted-row evidence root")
  }
  attempts <- list.files(
    interrupted_root,
    pattern = paste0("^", package, "-attempt-[0-9]+$"),
    full.names = FALSE
  )
  destination <- file.path(
    interrupted_root, paste0(package, "-attempt-", length(attempts) + 1L)
  )
  if (!dir.create(file.path(destination, "metadata"), recursive = TRUE,
      showWarnings = FALSE)) {
    rr_fail("could not reserve interrupted-row evidence")
  }
  diagnostic_names <- c(
    "check.log", "install.log", "preflight.log", "command.tsv", "result.tsv",
    "worker-transport.log"
  )
  candidates <- c(
    file.path(package_directory, diagnostic_names),
    file.path(package_directory, "metadata", diagnostic_names),
    file.path(package_directory, "metadata", c(
      "worker-error.tsv", "worker-task.tsv", "worker-launch.tsv"
    ))
  )
  candidates <- candidates[file.exists(candidates) &
    !vapply(candidates, rr_is_symbolic, logical(1L)) &
    vapply(candidates, function(path) {
      identical(rr_path_type(path), "file")
    }, logical(1L))]
  # Retain bounded diagnostics, never a potentially enormous check tree.
  candidates <- unique(candidates)
  retained <- character()
  for (source in utils::head(candidates, 20L)) {
    source_size <- as.numeric(fs::file_info(
      source, fail = TRUE, follow = FALSE
    )$size[[1L]])
    bounded <- 8 * 1024^2
    ordinal <- length(retained) + 1L
    name <- paste0(ordinal, "-", basename(source))
    if (source_size > bounded) name <- paste0(name, ".tail")
    destination_file <- file.path(destination, name)
    if (source_size <= bounded) {
      if (!file.copy(source, destination_file, copy.mode = TRUE,
          copy.date = TRUE) ||
          !identical(rr_sha256(source), rr_sha256(destination_file))) {
        rr_fail("could not retain interrupted-row diagnostic")
      }
    } else {
      tail_bytes <- min(rr_log_sample_bytes, source_size)
      input <- file(source, open = "rb")
      seek(input, where = source_size - tail_bytes, origin = "start")
      bytes <- readBin(input, "raw", n = as.integer(tail_bytes))
      close(input)
      output <- file(destination_file, open = "wb")
      writeBin(bytes, output)
      close(output)
      rr_write_tsv(data.frame(
        field = c("schema", "source_basename", "source_size",
          "retained_tail_bytes"),
        value = c("1", basename(source), as.character(source_size),
          as.character(length(bytes))), stringsAsFactors = FALSE
      ), file.path(destination, paste0(ordinal, "-truncation.tsv")))
    }
    retained <- c(retained, name)
  }
  rr_write_tsv(data.frame(
    field = c("status", "package", "diagnostics", "recorded_utc"),
    value = c(
      "interrupted", package, as.character(length(retained)),
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ), stringsAsFactors = FALSE
  ), file.path(destination, "metadata", "completion.tsv"))
  rr_seal_stage(destination)
  if (unlink(package_directory, recursive = TRUE, force = TRUE) != 0L ||
      dir.exists(package_directory)) {
    rr_fail("could not remove incomplete package row: ", package)
  }
  destination
}

rr_log_excerpt <- function(path, max_bytes = rr_log_file_limit_bytes,
                           sample_bytes = rr_log_sample_bytes) {
  path <- rr_require_file(path, "consumer log")
  info <- fs::file_info(path, fail = TRUE, follow = FALSE)
  size <- as.numeric(info$size[[1L]])
  if (is.na(size) || size < 0 || !is.finite(size)) {
    rr_fail("consumer log has an invalid size: ", path)
  }
  if (size > max_bytes) {
    return(list(path = path, size = size, oversized = TRUE, text = "",
      lines = character()))
  }
  sample_bytes <- max(1L, min(as.integer(sample_bytes), 16L * 1024L^2))
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  if (size <= 2 * sample_bytes) {
    bytes <- readBin(connection, "raw", n = as.integer(size))
  } else {
    head <- readBin(connection, "raw", n = sample_bytes)
    seek(connection, where = size - sample_bytes, origin = "start")
    tail <- readBin(connection, "raw", n = sample_bytes)
    bytes <- c(head, charToRaw("\n[... bounded log middle omitted ...]\n"), tail)
  }
  bytes[bytes == as.raw(0L)] <- charToRaw(" ")
  text <- rawToChar(bytes)
  Encoding(text) <- "bytes"
  list(
    path = path, size = size, oversized = FALSE, text = text,
    lines = strsplit(text, "\n", fixed = TRUE)[[1L]]
  )
}

rr_scan_test_output <- function(path, max_bytes = rr_log_file_limit_bytes,
                                chunk_bytes = rr_log_sample_bytes,
                                line_limit_bytes = rr_log_sample_bytes) {
  path <- rr_require_file(path, "consumer test output")
  size <- as.numeric(fs::file_info(path, fail = TRUE, follow = FALSE)$size[[1L]])
  if (is.na(size) || size > max_bytes) {
    return(list(pathological = TRUE, lines = character()))
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  carry <- ""
  retained <- character()
  repeat {
    bytes <- readBin(connection, "raw", n = as.integer(chunk_bytes))
    if (!length(bytes)) break
    bytes[bytes == as.raw(0L)] <- charToRaw(" ")
    text <- paste0(carry, rawToChar(bytes))
    Encoding(text) <- "bytes"
    pieces <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    carry <- pieces[[length(pieces)]]
    if (nchar(carry, type = "bytes") > line_limit_bytes) {
      return(list(pathological = TRUE, lines = character()))
    }
    complete <- utils::head(pieces, -1L)
    if (length(complete)) {
      selected <- grepl(
        "\\[ FAIL [0-9]+ \\| WARN [0-9]+ \\| SKIP [0-9]+ \\| PASS [0-9]+ \\]|[0-9]+ tests OK|(^|[[:space:]])(FAIL|ERROR)([[:space:]]|$)",
        complete, ignore.case = TRUE, perl = TRUE
      )
      retained <- c(retained, complete[selected])
      if (length(retained) > 100000L) {
        return(list(pathological = TRUE, lines = character()))
      }
    }
  }
  if (nzchar(carry)) {
    selected <- grepl(
      "\\[ FAIL [0-9]+ \\| WARN [0-9]+ \\| SKIP [0-9]+ \\| PASS [0-9]+ \\]|[0-9]+ tests OK|(^|[[:space:]])(FAIL|ERROR)([[:space:]]|$)",
      carry, ignore.case = TRUE, perl = TRUE
    )
    if (selected) retained <- c(retained, carry)
  }
  list(pathological = FALSE, lines = retained)
}

rr_parse_test_counts <- function(check_directory,
                                 max_output_bytes = rr_log_file_limit_bytes) {
  empty <- function(coverage = "unavailable", total = 0L, parsed = 0L) {
    list(
      files = data.frame(
        file = character(), fail = integer(), warn = integer(),
        skip = integer(), pass = integer(), summary = character(),
        stringsAsFactors = FALSE
      ),
      coverage = coverage, files_total = total, files_parsed = parsed,
      fail = NA_integer_, warn = NA_integer_, skip = NA_integer_,
      pass = NA_integer_, pathological_outputs = character()
    )
  }
  if (!dir.exists(check_directory)) return(empty())
  outputs <- sort(list.files(
    check_directory, pattern = "[.]Rout([.]fail)?$", recursive = TRUE,
    full.names = TRUE
  ), method = "radix")
  if (length(outputs) > 10000L) {
    rr_fail("consumer check produced a pathological number of Rout files")
  }
  if (!length(outputs)) return(empty())
  rows <- list()
  pathological <- character()
  pattern <- "\\[ FAIL ([0-9]+) \\| WARN ([0-9]+) \\| SKIP ([0-9]+) \\| PASS ([0-9]+) \\]"
  for (output in outputs) {
    scan <- rr_scan_test_output(output, max_bytes = max_output_bytes)
    if (scan$pathological) {
      pathological <- c(pathological, normalizePath(
        output, winslash = "/", mustWork = TRUE
      ))
      next
    }
    lines <- scan$lines
    lines <- gsub("\\033\\[[0-9;]*[[:alpha:]]", "", lines, perl = TRUE)
    matches <- regexec(pattern, lines, perl = TRUE)
    captures <- regmatches(lines, matches)
    captures <- captures[lengths(captures) == 5L]
    if (length(captures)) {
      final <- captures[[length(captures)]]
      rows[[length(rows) + 1L]] <- data.frame(
        file = normalizePath(output, winslash = "/", mustWork = TRUE),
        fail = as.integer(final[[2L]]), warn = as.integer(final[[3L]]),
        skip = as.integer(final[[4L]]), pass = as.integer(final[[5L]]),
        summary = final[[1L]], stringsAsFactors = FALSE
      )
      next
    }
    tiny <- regexec("([0-9]+) tests OK([[:space:]]|$)", lines, perl = TRUE)
    tiny <- regmatches(lines, tiny)
    tiny <- tiny[lengths(tiny) == 3L]
    if (length(tiny) && !any(grepl(
        "(^|[[:space:]])(FAIL|ERROR)([[:space:]]|$)", lines,
        ignore.case = TRUE, perl = TRUE
      ))) {
      passes <- sum(vapply(tiny, function(value) {
        as.integer(value[[2L]])
      }, integer(1L)))
      rows[[length(rows) + 1L]] <- data.frame(
        file = normalizePath(output, winslash = "/", mustWork = TRUE),
        fail = 0L, warn = 0L, skip = 0L, pass = passes,
        summary = paste0("tinytest clean records: PASS ", passes),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    value <- empty("unavailable", length(outputs), 0L)
    value$pathological_outputs <- pathological
    return(value)
  }
  files <- do.call(rbind, rows)
  coverage <- if (nrow(files) == length(outputs)) "exact" else "partial"
  list(
    files = files, coverage = coverage, files_total = length(outputs),
    files_parsed = nrow(files), fail = sum(files$fail),
    warn = sum(files$warn), skip = sum(files$skip), pass = sum(files$pass),
    pathological_outputs = pathological
  )
}

rr_reverse_result_columns <- function() c(
  "package", "source", "relation", "priority", "version", "archive",
  "archive_sha256", "candidate_ref", "candidate_commit", "candidate_tree",
  "candidate_version", "candidate_content_sha256",
  "candidate_library_content_sha256", "dependency_library_content_sha256",
  "protected_metadata_before", "protected_metadata_after", "cache_key",
  "cache_reused", "cache_manifest_sha256", "cache_seal_sha256",
  "installed_content_sha256", "install_elapsed_seconds",
  "check_elapsed_seconds", "install_timeout_seconds", "check_timeout_seconds",
  "exit_code", "timed_out", "check_status", "status", "classification",
  "count_coverage", "count_files_total", "count_files_parsed", "test_fail",
  "test_warn", "test_skip", "test_pass", "not_cran", "log", "error"
)

rr_check_status <- function(check_directory) {
  path <- file.path(check_directory, "00check.log")
  if (!file.exists(path)) return("")
  excerpt <- rr_log_excerpt(path)
  if (excerpt$oversized) {
    value <- ""
    attr(value, "pathological_output") <- normalizePath(
      path, winslash = "/", mustWork = TRUE
    )
    return(value)
  }
  status <- grep("^Status:", excerpt$lines, value = TRUE)
  if (length(status) == 1L) status[[1L]] else ""
}

rr_run <- function(command, arguments, environment, directory,
                   timeout_seconds, log) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    rr_fail("processx is required for bounded reverse-dependency children")
  }
  result <- processx::run(
    "/usr/bin/env", c("-i", paste0(names(environment), "=", environment),
      command, arguments),
    error_on_status = FALSE, wd = directory, timeout = timeout_seconds,
    stdout = log, stderr = "2>&1", cleanup_tree = TRUE,
    supervise = TRUE, linux_pdeathsig = TRUE, windows_hide_window = TRUE
  )
  list(status = as.integer(result$status), timeout = isTRUE(result$timeout))
}

rr_record_command <- function(path, command, arguments, environment,
                              timeout_seconds) {
  rr_write_tsv(data.frame(
    kind = c("command", rep("argument", length(arguments)),
      rep("environment", length(environment)), "timeout_seconds"),
    name = c("executable", as.character(seq_along(arguments)),
      names(environment), "timeout"),
    value = c(command, arguments, unname(environment),
      as.character(timeout_seconds)),
    stringsAsFactors = FALSE
  ), path)
}

# Construct the deliberately bounded global environment needed by a serialized
# worker closure.  R's serializer does not copy the process global environment;
# recording these exact reachable bindings lets a fresh Rscript reconstruct it
# without sourcing the monolithic reverse harness or repeating parent checks.
rr_worker_bundle <- function(worker) {
  if (!is.function(worker) || !requireNamespace("codetools", quietly = TRUE)) {
    rr_fail("external worker bundling requires one function and codetools")
  }
  global <- environment(worker)
  if (!is.environment(global)) rr_fail("external worker has no environment")
  find_binding <- function(name, environment) {
    current <- environment
    while (!identical(current, emptyenv())) {
      if (exists(name, envir = current, inherits = FALSE)) return(current)
      current <- parent.env(current)
    }
    NULL
  }
  globals <- list()
  pending <- list(.worker = worker)
  inspected <- character()
  while (length(pending)) {
    function_name <- names(pending)[[1L]]
    function_value <- pending[[1L]]
    pending <- pending[-1L]
    if (function_name %in% inspected) next
    inspected <- c(inspected, function_name)
    global_names <- sort(unique(codetools::findGlobals(
      function_value, merge = TRUE
    )), method = "radix")
    for (name in global_names) {
      binding <- find_binding(name, environment(function_value))
      if (is.null(binding) || !identical(binding, global) ||
          name %in% names(globals)) next
      value <- get(name, envir = binding, inherits = FALSE)
      globals[[name]] <- value
      if (is.function(value)) pending[[name]] <- value
    }
  }
  list(worker = worker, globals = globals)
}

rr_worker_result_columns <- function() c(
  "schema", "position", "task_sha256", "ok", "value", "error"
)

rr_read_worker_launch <- function(path, position, task_sha256) {
  value <- rr_read_tsv(
    path, c("schema", "position", "task_sha256", "wrapper_pid")
  )
  if (nrow(value) != 1L || !identical(value$schema[[1L]], "1") ||
      !identical(value$position[[1L]], as.character(position)) ||
      !identical(value$task_sha256[[1L]], task_sha256) ||
      !grepl("^[1-9][0-9]*$", value$wrapper_pid[[1L]])) {
    rr_fail("external worker launch marker is malformed")
  }
  value
}

rr_read_worker_completion <- function(path, position, task_sha256) {
  value <- rr_read_tsv(
    path, c("schema", "position", "task_sha256", "worker_exit_status")
  )
  if (nrow(value) != 1L || !identical(value$schema[[1L]], "1") ||
      !identical(value$position[[1L]], as.character(position)) ||
      !identical(value$task_sha256[[1L]], task_sha256) ||
      !identical(value$worker_exit_status[[1L]], "0")) {
    rr_fail("external worker clean-exit marker is malformed")
  }
  value
}

rr_read_worker_result <- function(path, position, task_sha256,
                                  completion = NULL) {
  if (!is.null(completion)) {
    invisible(rr_read_worker_completion(completion, position, task_sha256))
  }
  path <- rr_require_file(path, "durable external-worker result")
  result_size <- as.numeric(fs::file_info(
    path, fail = TRUE, follow = FALSE
  )$size[[1L]])
  if (is.na(result_size) || result_size > 64 * 1024^2) {
    rr_fail("durable external-worker result is pathologically large")
  }
  value <- tryCatch(readRDS(path), error = function(condition) {
    rr_fail("could not read durable external-worker result: ",
      conditionMessage(condition))
  })
  if (!is.list(value) || !identical(names(value), rr_worker_result_columns()) ||
      !identical(value$schema, 1L) ||
      !identical(value$position, as.integer(position)) ||
      !identical(value$task_sha256, task_sha256) ||
      length(value$ok) != 1L || is.na(value$ok) || !is.logical(value$ok) ||
      length(value$error) != 1L || !is.character(value$error) ||
      is.na(value$error) || !nzchar(value$error) ||
      (isTRUE(value$ok) && !identical(value$error, "-")) ||
      (!isTRUE(value$ok) && !is.null(value$value))) {
    rr_fail("durable external-worker result is malformed")
  }
  value
}

rr_worker_environment <- function(state, inherited = Sys.getenv()) {
  state <- rr_require_directory(state, "external reverse-worker state")
  names <- c(
    "home", "tmp", "xdg-cache", "xdg-config", "xdg-data", "xdg-state",
    "xdg-runtime", "r-cache", "python-cache", "reticulate", "pip", "uv"
  )
  paths <- file.path(state, names)
  if (!all(vapply(paths, dir.create, logical(1L), recursive = FALSE,
      showWarnings = FALSE))) {
    rr_fail("could not reserve isolated external reverse-worker startup state")
  }
  Sys.chmod(paths[[7L]], mode = "0700")
  environment <- inherited
  overrides <- c(
    HOME = paths[[1L]], TMPDIR = paths[[2L]],
    XDG_CACHE_HOME = paths[[3L]], XDG_CONFIG_HOME = paths[[4L]],
    XDG_DATA_HOME = paths[[5L]], XDG_STATE_HOME = paths[[6L]],
    XDG_RUNTIME_DIR = paths[[7L]], R_USER_CACHE_DIR = paths[[8L]],
    PYTHONPYCACHEPREFIX = paths[[9L]],
    RETICULATE_MINICONDA_PATH = paths[[10L]],
    RETICULATE_VIRTUALENV_ROOT = paths[[10L]], PIP_CACHE_DIR = paths[[11L]],
    UV_CACHE_DIR = paths[[12L]], R_PROFILE_USER = "/dev/null",
    R_ENVIRON_USER = "/dev/null", R_HISTFILE = "/dev/null",
    PYTHONDONTWRITEBYTECODE = "1", PYTHONNOUSERSITE = "1"
  )
  controls <- rr_reverse_nested_controls()
  environment[names(overrides)] <- unname(overrides)
  environment[names(controls)] <- unname(controls)
  environment
}

rr_validate_worker_transport <- function(transport) {
  if (!is.list(transport) || !identical(names(transport), c(
      "root", "state", "launch", "result", "completion", "log"))) {
    rr_fail("external worker transport is malformed")
  }
  root <- rr_require_directory(transport$root, "external worker transport root")
  state <- rr_require_directory(transport$state, "external worker state")
  contained <- function(path) {
    parent <- rr_require_directory(dirname(path), "external worker output parent")
    identical(parent, root) || startsWith(parent, paste0(root, "/"))
  }
  if (!startsWith(state, paste0(root, "/")) ||
      !contained(transport$launch) || !contained(transport$result) ||
      !contained(transport$completion) ||
      !contained(transport$log) ||
      any(vapply(c(
        transport$launch, transport$result, transport$completion, transport$log
      ),
        rr_is_symbolic,
        logical(1L))) || file.exists(transport$launch) ||
      dir.exists(transport$launch) || file.exists(transport$result) ||
      dir.exists(transport$result) || file.exists(transport$completion) ||
      dir.exists(transport$completion) || file.exists(transport$log) ||
      dir.exists(transport$log)) {
    rr_fail("external worker transport escaped its reserved row or exists")
  }
  list(
    root = root, state = state, launch = transport$launch,
    result = transport$result,
    completion = transport$completion, log = transport$log
  )
}

# Execute one bounded wave using independently started R interpreters.  A
# processx supervisor and Linux parent-death signal own each complete descendant
# tree.  Results are atomically published outside disposable startup state, so
# a sibling that finished before parent interruption remains resumable.
rr_parallel_wave <- function(tasks, worker, jobs, rscript, worker_script,
                             worker_group, timeout_seconds,
                             inherited_environment = Sys.getenv()) {
  if (!is.list(tasks) || !length(tasks) || !is.function(worker) ||
      length(jobs) != 1L || is.na(jobs) || jobs < 1L ||
      jobs != as.integer(jobs) || length(tasks) > as.integer(jobs) ||
      length(timeout_seconds) != 1L || is.na(timeout_seconds) ||
      !is.finite(timeout_seconds) || timeout_seconds <= 0 ||
      !requireNamespace("processx", quietly = TRUE) ||
      !identical(.Platform$OS.type, "unix")) {
    rr_fail("external parallel-wave inputs are malformed or unsupported")
  }
  rscript <- rr_require_file(rscript, "external worker Rscript")
  worker_script <- rr_require_file(worker_script, "external worker script")
  worker_group <- rr_require_file(worker_group, "external worker group supervisor")
  if (file.access(worker_group, mode = 1L) != 0L) {
    rr_fail("external worker group supervisor is not executable")
  }
  bundle <- rr_worker_bundle(worker)
  positions <- seq_along(tasks)
  transports <- lapply(tasks, function(task) {
    if (!is.list(task) || is.null(task$rr_transport)) {
      rr_fail("external worker task omits its transport")
    }
    rr_validate_worker_transport(task$rr_transport)
  })
  processes <- vector("list", length(tasks))
  task_hashes <- character(length(tasks))
  launched_at <- rep(NA_real_, length(tasks))
  timed_out <- rep(FALSE, length(tasks))
  termination_attempted <- rep(FALSE, length(tasks))
  cleanup_deadline <- rep(Inf, length(tasks))
  abandoned <- rep(FALSE, length(tasks))
  active <- TRUE
  on.exit({
    if (active) {
      for (process in processes) {
        if (!is.null(process) && isTRUE(tryCatch(
            process$is_alive(), error = function(...) FALSE))) {
          suppressWarnings(try(process$kill_tree(), silent = TRUE))
        }
      }
      for (process in processes) {
        if (!is.null(process)) suppressWarnings(try(
          process$wait(5000), silent = TRUE
        ))
      }
    }
  }, add = TRUE)
  for (position in positions) {
    task <- tasks[[position]]
    task$rr_transport <- NULL
    task_hashes[[position]] <- rr_object_sha256(task)
    spec <- list(
      schema = 1L, position = as.integer(position), task = task,
      task_sha256 = task_hashes[[position]], bundle = bundle,
      transport_root = transports[[position]]$root,
      result = transports[[position]]$result
    )
    spec_path <- file.path(transports[[position]]$state, "spec.rds")
    saveRDS(spec, spec_path, version = 3L)
    environment <- rr_worker_environment(
      transports[[position]]$state, inherited_environment
    )
    group_environment <- c(
      PARADOX_REVERSE_COORDINATOR_PID = as.character(Sys.getpid()),
      PARADOX_REVERSE_WORKER_TOKEN = paste0(
        "reverse.", Sys.getpid(), ".", position, ".", task_hashes[[position]]
      ),
      PARADOX_REVERSE_WORKER_LAUNCH = transports[[position]]$launch,
      PARADOX_REVERSE_WORKER_RESULT = transports[[position]]$result,
      PARADOX_REVERSE_WORKER_COMPLETION = transports[[position]]$completion,
      PARADOX_REVERSE_TASK_POSITION = as.character(position),
      PARADOX_REVERSE_TASK_SHA256 = task_hashes[[position]]
    )
    environment[names(group_environment)] <- unname(group_environment)
    processes[[position]] <- processx::process$new(
      worker_group, c(rscript, "--vanilla", worker_script, spec_path),
      env = environment,
      stdout = transports[[position]]$log, stderr = "2>&1", cleanup = TRUE,
      cleanup_tree = TRUE, supervise = TRUE, windows_verbatim_args = TRUE,
      linux_pdeathsig = TRUE
    )
    launched_at[[position]] <- proc.time()[["elapsed"]]
  }
  unfinished <- rep(TRUE, length(processes))
  while (any(unfinished)) {
    now <- proc.time()[["elapsed"]]
    for (position in positions[unfinished]) {
      alive <- isTRUE(tryCatch(
        processes[[position]]$is_alive(), error = function(...) FALSE
      ))
      if (alive && now - launched_at[[position]] >= timeout_seconds &&
          !termination_attempted[[position]]) {
        timed_out[[position]] <- TRUE
        termination_attempted[[position]] <- TRUE
        cleanup_deadline[[position]] <- now + 10
        suppressWarnings(try(processes[[position]]$kill_tree(), silent = TRUE))
        suppressWarnings(try(processes[[position]]$kill(), silent = TRUE))
        suppressWarnings(try(processes[[position]]$wait(5000), silent = TRUE))
        alive <- isTRUE(tryCatch(
          processes[[position]]$is_alive(), error = function(...) FALSE
        ))
      }
      if (alive && termination_attempted[[position]] &&
          proc.time()[["elapsed"]] >= cleanup_deadline[[position]]) {
        abandoned[[position]] <- TRUE
        alive <- FALSE
      }
      if (!alive) unfinished[[position]] <- FALSE
    }
    if (any(unfinished)) Sys.sleep(0.02)
  }
  # processx marks the complete spawned tree in its environment and can still
  # find reparented descendants on platforms without /proc or setsid.  Sweep
  # every task, including nominal successes, before accepting its clean marker.
  for (position in positions) {
    suppressWarnings(try(processes[[position]]$kill_tree(), silent = TRUE))
    suppressWarnings(try(processes[[position]]$wait(1000), silent = TRUE))
  }
  values <- lapply(positions, function(position) {
    status <- tryCatch(
      processes[[position]]$get_exit_status(), error = function(...) NA_integer_
    )
    if (timed_out[[position]] || abandoned[[position]] ||
        !identical(status, 0L) ||
        !file.exists(transports[[position]]$launch) ||
        !file.exists(transports[[position]]$result) ||
        !file.exists(transports[[position]]$completion)) {
      return(list(
        schema = 1L, position = as.integer(position),
        task_sha256 = task_hashes[[position]], ok = FALSE, value = NULL,
        error = paste0(
          if (timed_out[[position]]) "external worker deadline exceeded; " else "",
          "external worker exited without a result (status=",
          if (is.na(status)) "unknown" else status, ")")
      ))
    }
    tryCatch(
      {
        invisible(rr_read_worker_launch(
          transports[[position]]$launch, position, task_hashes[[position]]
        ))
        rr_read_worker_result(
        transports[[position]]$result, position, task_hashes[[position]],
        transports[[position]]$completion
        )
      },
      error = function(condition) list(
        schema = 1L, position = as.integer(position),
        task_sha256 = task_hashes[[position]], ok = FALSE, value = NULL,
        error = conditionMessage(condition)
      )
    )
  })
  for (transport in transports) {
    if (dir.exists(transport$state) &&
        unlink(transport$state, recursive = TRUE, force = TRUE) != 0L) {
      rr_fail("could not prune external worker startup state")
    }
  }
  # A process in an uninterruptible kernel state must not keep the coordinator
  # forever.  One last bounded best-effort reap is made before relinquishing
  # the processx cleanup guard.
  for (position in positions[abandoned]) {
    suppressWarnings(try(processes[[position]]$kill_tree(), silent = TRUE))
    suppressWarnings(try(processes[[position]]$kill(), silent = TRUE))
    suppressWarnings(try(processes[[position]]$wait(1000), silent = TRUE))
  }
  active <- FALSE
  list(backend = "external", results = values, task_sha256 = task_hashes)
}

# A cache build is staged and sealed privately.  Promotion is atomic.  If a
# concurrent run won the same-key race, authenticate its completed target and
# discard our duplicate staging tree instead of corrupting or replacing it.
rr_promote_install_cache <- function(staging, target, validate_target) {
  staging <- rr_require_directory(staging, "staged install cache")
  if (!is.function(validate_target)) rr_fail("cache validator is not a function")
  if (rr_is_symbolic(target)) rr_fail("install-cache target is symbolic")
  if (suppressWarnings(file.rename(staging, target))) {
    return(list(promoted = TRUE, validation = validate_target()))
  }
  validation <- tryCatch(validate_target(), error = identity)
  if (inherits(validation, "error")) {
    rr_fail(
      "install-cache promotion lost a race to an invalid target: ",
      conditionMessage(validation)
    )
  }
  if (unlink(staging, recursive = TRUE, force = TRUE) != 0L ||
      file.exists(staging) || dir.exists(staging) || rr_is_symbolic(staging)) {
    rr_fail("could not prune duplicate staged install cache")
  }
  list(promoted = FALSE, validation = validation)
}

rr_reverse_acceptance_columns <- function() c(
  "ordinal", "plan_index", "package", "status", "classification", "wave",
  "result_sha256", "row_manifest_sha256", "row_seal_sha256", "accepted_utc"
)

rr_empty_reverse_acceptance <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(rr_reverse_acceptance_columns())),
      rr_reverse_acceptance_columns()),
    stringsAsFactors = FALSE
  )
}

rr_read_reverse_acceptance <- function(path) {
  rr_read_tsv(path, rr_reverse_acceptance_columns(), allow_empty = TRUE)
}

rr_validate_reverse_acceptance <- function(path, plan_packages, package_root,
                                            require_complete = FALSE) {
  accepted <- rr_read_reverse_acceptance(path)
  if (!length(plan_packages) || anyDuplicated(plan_packages) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", plan_packages))) {
    rr_fail("acceptance plan package inventory is malformed")
  }
  if (!nrow(accepted)) {
    if (require_complete) rr_fail("acceptance ledger is incomplete")
    return(accepted)
  }
  canonical <- function(value) grepl("^[1-9][0-9]*$", value)
  indices <- suppressWarnings(as.integer(accepted$plan_index))
  if (!identical(accepted$ordinal, as.character(seq_len(nrow(accepted)))) ||
      any(!canonical(accepted$plan_index)) || anyNA(indices) ||
      any(indices > length(plan_packages)) || anyDuplicated(indices) ||
      anyDuplicated(accepted$package) ||
      !identical(accepted$package, unname(plan_packages[indices])) ||
      any(!accepted$status %in% c("passed", "failed")) ||
      any(!canonical(accepted$wave)) ||
      any(!grepl("^[0-9a-f]{64}$", accepted$result_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", accepted$row_manifest_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", accepted$row_seal_sha256))) {
    rr_fail("acceptance ledger is malformed or disagrees with the plan")
  }
  for (index in seq_len(nrow(accepted))) {
    row_root <- file.path(package_root, accepted$package[[index]])
    paths <- file.path(row_root, c(
      "result.tsv", rr_stage_manifest_relative, rr_stage_seal_relative
    ))
    observed <- unname(tools::sha256sum(vapply(
      paths, rr_require_file, character(1L), label = "accepted row receipt"
    )))
    expected <- unname(unlist(accepted[index, c(
      "result_sha256", "row_manifest_sha256", "row_seal_sha256"
    )], use.names = FALSE))
    if (!identical(observed, expected)) {
      rr_fail("accepted row receipt changed: ", accepted$package[[index]])
    }
  }
  if (require_complete &&
      (!identical(sort(indices), seq_along(plan_packages)) ||
        nrow(accepted) != length(plan_packages))) {
    rr_fail("acceptance ledger does not contain every planned row")
  }
  accepted
}

rr_append_reverse_acceptance <- function(path, plan_index, package, wave,
                                         result, row_evidence) {
  accepted <- rr_read_reverse_acceptance(path)
  if (package %in% accepted$package || as.character(plan_index) %in%
      accepted$plan_index) {
    rr_fail("row was already accepted: ", package)
  }
  if (!is.data.frame(result) || nrow(result) != 1L ||
      !all(c("status", "classification") %in% names(result)) ||
      length(row_evidence$manifest_sha256) != 1L ||
      length(row_evidence$seal_sha256) != 1L) {
    rr_fail("accepted row inputs are malformed")
  }
  row_path <- file.path(dirname(path), "..", "packages", package, "result.tsv")
  row <- data.frame(
    ordinal = as.character(nrow(accepted) + 1L),
    plan_index = as.character(plan_index), package = package,
    status = result$status[[1L]], classification = result$classification[[1L]],
    wave = as.character(wave), result_sha256 = rr_sha256(row_path),
    row_manifest_sha256 = row_evidence$manifest_sha256,
    row_seal_sha256 = row_evidence$seal_sha256,
    accepted_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  rr_write_tsv(rbind(accepted, row), path, replace = TRUE)
  invisible(row)
}

rr_reverse_wave_columns <- function() c(
  "ordinal", "plan_indices", "packages", "worker_limit", "backend",
  "workers_started", "workers_completed", "worker_error_indices",
  "worker_missing_indices", "worker_unstarted_indices",
  "automatic_ceiling", "operator_limit", "resource_report",
  "resource_report_sha256",
  "protected_metadata_before", "protected_metadata_after", "started_utc",
  "finished_utc"
)

rr_reverse_worker_task_columns <- function() c(
  "schema", "plan_index", "package", "wave", "position", "worker_limit",
  "automatic_ceiling", "operator_limit", "resource_report",
  "resource_report_sha256", "protected_metadata_before", "started_utc",
  "task_sha256", "worker_script_sha256", "worker_group_sha256"
)

rr_read_reverse_worker_task <- function(path) {
  value <- rr_read_tsv(path, rr_reverse_worker_task_columns())
  positive <- function(field) grepl("^[1-9][0-9]*$", value[[field]][[1L]])
  if (nrow(value) != 1L || !identical(value$schema[[1L]], "1") ||
      any(!vapply(c(
        "plan_index", "wave", "position", "worker_limit", "automatic_ceiling"
      ), positive, logical(1L))) ||
      !grepl("^(-|[1-9][0-9]*)$", value$operator_limit[[1L]]) ||
      !grepl("^[A-Za-z][A-Za-z0-9.]*$", value$package[[1L]]) ||
      !rr_safe_relative(value$resource_report[[1L]]) ||
      any(!grepl("^[0-9a-f]{64}$", c(
        value$resource_report_sha256[[1L]],
        value$protected_metadata_before[[1L]], value$task_sha256[[1L]],
        value$worker_script_sha256[[1L]], value$worker_group_sha256[[1L]]
      ))) ||
      !grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        value$started_utc[[1L]]
      )) {
    rr_fail("external reverse-worker task receipt is malformed")
  }
  value
}

rr_empty_reverse_waves <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(rr_reverse_wave_columns())),
      rr_reverse_wave_columns()),
    stringsAsFactors = FALSE
  )
}

rr_read_reverse_waves <- function(path) {
  rr_read_tsv(path, rr_reverse_wave_columns(), allow_empty = TRUE)
}

rr_validate_reverse_waves <- function(path, plan_packages) {
  waves <- rr_read_reverse_waves(path)
  if (!nrow(waves)) return(waves)
  positive <- function(value) grepl("^[1-9][0-9]*$", value)
  nonnegative <- function(value) grepl("^(0|[1-9][0-9]*)$", value)
  if (!identical(waves$ordinal, as.character(seq_len(nrow(waves)))) ||
      any(!positive(waves$worker_limit)) ||
      any(!positive(waves$workers_started)) ||
      any(!nonnegative(waves$workers_completed)) ||
      any(!positive(waves$automatic_ceiling)) ||
      any(!grepl("^(-|[1-9][0-9]*)$", waves$operator_limit)) ||
      any(!vapply(waves$resource_report, rr_safe_relative, logical(1L))) ||
      any(!grepl("^[0-9a-f]{64}$", waves$resource_report_sha256)) ||
      any(!identical(waves$backend, rep("external", nrow(waves)))) ||
      any(!grepl("^[0-9a-f]{64}$", waves$protected_metadata_before)) ||
      any(!grepl("^[0-9a-f]{64}$", waves$protected_metadata_after))) {
    rr_fail("reverse wave ledger is malformed")
  }
  for (ordinal in seq_len(nrow(waves))) {
    indices <- strsplit(waves$plan_indices[[ordinal]], ",", fixed = TRUE)[[1L]]
    packages <- strsplit(waves$packages[[ordinal]], ",", fixed = TRUE)[[1L]]
    parsed <- suppressWarnings(as.integer(indices))
    errors <- if (identical(waves$worker_error_indices[[ordinal]], "-")) {
      character()
    } else {
      strsplit(waves$worker_error_indices[[ordinal]], ",", fixed = TRUE)[[1L]]
    }
    missing <- if (identical(waves$worker_missing_indices[[ordinal]], "-")) {
      character()
    } else {
      strsplit(waves$worker_missing_indices[[ordinal]], ",", fixed = TRUE)[[1L]]
    }
    unstarted <- if (identical(
        waves$worker_unstarted_indices[[ordinal]], "-")) {
      character()
    } else {
      strsplit(
        waves$worker_unstarted_indices[[ordinal]], ",", fixed = TRUE
      )[[1L]]
    }
    if (any(!positive(indices)) || anyNA(parsed) ||
        any(parsed > length(plan_packages)) || anyDuplicated(parsed) ||
        !identical(packages, unname(plan_packages[parsed])) ||
        !identical(waves$workers_started[[ordinal]],
          as.character(length(parsed) - length(unstarted))) ||
        !identical(waves$workers_completed[[ordinal]],
          as.character(length(parsed) - length(unstarted) - length(missing))) ||
        length(parsed) != as.integer(waves$worker_limit[[ordinal]]) ||
        as.integer(waves$workers_started[[ordinal]]) >
          as.integer(waves$worker_limit[[ordinal]]) ||
        as.integer(waves$workers_completed[[ordinal]]) >
          as.integer(waves$workers_started[[ordinal]]) ||
        any(!errors %in% indices) || anyDuplicated(errors) ||
        any(!missing %in% indices) || anyDuplicated(missing) ||
        any(!unstarted %in% indices) || anyDuplicated(unstarted) ||
        any(errors %in% c(missing, unstarted)) ||
        any(missing %in% unstarted) ||
        as.integer(waves$worker_limit[[ordinal]]) >
          as.integer(waves$automatic_ceiling[[ordinal]]) ||
        (!identical(waves$operator_limit[[ordinal]], "-") &&
          as.integer(waves$worker_limit[[ordinal]]) >
            as.integer(waves$operator_limit[[ordinal]]))) {
      rr_fail("reverse wave disagrees with its planned rows")
    }
  }
  waves
}

rr_validate_reverse_wave_report_inventory <- function(report_files, waves) {
  if (!is.character(report_files) || anyNA(report_files) ||
      !is.data.frame(waves) ||
      !identical(names(waves), rr_reverse_wave_columns())) {
    rr_fail("per-wave resource scheduler inventory inputs are malformed")
  }
  retained <- basename(report_files)
  referenced <- basename(waves$resource_report)
  if (anyDuplicated(retained) || anyDuplicated(referenced) ||
      length(retained) != length(referenced) ||
      !identical(
        sort(retained, method = "radix"),
        sort(referenced, method = "radix")
      )) {
    rr_fail(
      "per-wave resource scheduler report inventory is not exact"
    )
  }
  invisible(TRUE)
}

rr_append_reverse_wave <- function(path, plan_indices, plan_packages,
                                   worker_limit, backend, worker_errors,
                                   worker_missing, worker_unstarted,
                                   automatic_ceiling, operator_limit,
                                   resource_report, resource_report_sha256,
                                   protected_before, protected_after,
                                   started_utc, finished_utc) {
  waves <- rr_read_reverse_waves(path)
  row <- data.frame(
    ordinal = as.character(nrow(waves) + 1L),
    plan_indices = paste(plan_indices, collapse = ","),
    packages = paste(plan_packages[plan_indices], collapse = ","),
    worker_limit = as.character(worker_limit), backend = backend,
    workers_started = as.character(
      length(plan_indices) - length(worker_unstarted)
    ),
    workers_completed = as.character(
      length(plan_indices) - length(worker_unstarted) - length(worker_missing)
    ),
    worker_error_indices = if (length(worker_errors)) {
      paste(worker_errors, collapse = ",")
    } else "-",
    worker_missing_indices = if (length(worker_missing)) {
      paste(worker_missing, collapse = ",")
    } else "-",
    worker_unstarted_indices = if (length(worker_unstarted)) {
      paste(worker_unstarted, collapse = ",")
    } else "-",
    automatic_ceiling = as.character(automatic_ceiling),
    operator_limit = if (is.null(operator_limit)) "-" else {
      as.character(operator_limit)
    },
    resource_report = resource_report,
    resource_report_sha256 = resource_report_sha256,
    protected_metadata_before = protected_before,
    protected_metadata_after = protected_after,
    started_utc = started_utc, finished_utc = finished_utc,
    stringsAsFactors = FALSE
  )
  rr_write_tsv(rbind(waves, row), path, replace = TRUE)
  invisible(row)
}

rr_validate_reverse_wave_admission <- function(wave, pending, initial_jobs,
                                                live_jobs) {
  if (!is.data.frame(wave) || nrow(wave) != 1L ||
      !identical(names(wave), rr_reverse_wave_columns()) ||
      length(initial_jobs) != 1L || length(live_jobs) != 1L ||
      is.na(initial_jobs) || is.na(live_jobs) || initial_jobs < 1L ||
      live_jobs < 1L || initial_jobs != as.integer(initial_jobs) ||
      live_jobs != as.integer(live_jobs) || !is.integer(pending) ||
      anyNA(pending) || any(pending < 1L) || anyDuplicated(pending)) {
    rr_fail("reverse wave admission inputs are malformed")
  }
  operator_limit <- if (identical(wave$operator_limit[[1L]], "-")) {
    Inf
  } else as.integer(wave$operator_limit[[1L]])
  expected_width <- as.integer(min(
    as.integer(initial_jobs), as.integer(live_jobs), operator_limit,
    length(pending)
  ))
  members <- as.integer(strsplit(
    wave$plan_indices[[1L]], ",", fixed = TRUE
  )[[1L]])
  if (expected_width < 1L ||
      !identical(as.integer(wave$worker_limit[[1L]]), expected_width) ||
      !identical(members, utils::head(pending, expected_width))) {
    rr_fail("reverse wave does not use its exact retained admission width")
  }
  unsuccessful <- unlist(lapply(c(
    "worker_error_indices", "worker_missing_indices",
    "worker_unstarted_indices"
  ), function(field) {
    value <- wave[[field]][[1L]]
    if (identical(value, "-")) integer() else {
      as.integer(strsplit(value, ",", fixed = TRUE)[[1L]])
    }
  }), use.names = FALSE)
  pending[!pending %in% setdiff(members, unsuccessful)]
}

rr_validate_reverse_acceptance_waves <- function(accepted, waves) {
  if (!is.data.frame(accepted) || !is.data.frame(waves)) {
    rr_fail("acceptance/wave ledgers are malformed")
  }
  accepted_wave <- setNames(
    suppressWarnings(as.integer(accepted$wave)), accepted$plan_index
  )
  if (length(accepted_wave) && (anyNA(accepted_wave) ||
      any(accepted_wave < 1L) || any(accepted_wave > nrow(waves)) ||
      anyDuplicated(names(accepted_wave)))) {
    rr_fail("acceptance ledger refers to an absent or repeated wave member")
  }
  seen <- character()
  eventual_wave <- function(member) {
    if (member %in% names(accepted_wave)) unname(accepted_wave[[member]]) else NULL
  }
  for (wave_ordinal in seq_len(nrow(waves))) {
    members <- strsplit(
      waves$plan_indices[[wave_ordinal]], ",", fixed = TRUE
    )[[1L]]
    unsuccessful <- unlist(lapply(c(
      "worker_error_indices", "worker_missing_indices",
      "worker_unstarted_indices"
    ), function(field) {
      value <- waves[[field]][[wave_ordinal]]
      if (identical(value, "-")) character() else {
        strsplit(value, ",", fixed = TRUE)[[1L]]
      }
    }), use.names = FALSE)
    successful <- setdiff(members, unsuccessful)
    for (member in members) {
      eventual <- eventual_wave(member)
      if (!is.null(eventual) && wave_ordinal > eventual) {
        rr_fail("wave reran a row after its successful acceptance: ", member)
      }
      if (!is.null(eventual) && wave_ordinal < eventual &&
          !member %in% unsuccessful) {
        rr_fail("an earlier successful wave member was silently discarded: ",
          member)
      }
    }
    for (member in successful) {
      eventual <- eventual_wave(member)
      if (is.null(eventual) || eventual != wave_ordinal) {
        rr_fail("successful wave member lacks acceptance in its exact wave: ",
          member)
      }
    }
    accepted_here <- names(accepted_wave)[accepted_wave == wave_ordinal]
    if (!setequal(accepted_here, successful) ||
        length(accepted_here) != length(successful)) {
      rr_fail("wave success and acceptance inventories are not bijective")
    }
    seen <- c(seen, members)
  }
  if (length(accepted_wave) && any(!names(accepted_wave) %in% seen)) {
    rr_fail("accepted row is absent from every retained wave")
  }
  invisible(TRUE)
}
