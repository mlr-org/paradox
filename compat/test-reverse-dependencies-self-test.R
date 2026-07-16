#!/usr/bin/env Rscript

started <- proc.time()[["elapsed"]]
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("could not identify reverse self-test location", call. = FALSE)
}
script <- normalizePath(
  sub("^--file=", "", script_argument), winslash = "/", mustWork = TRUE
)
sys.source(file.path(dirname(script), "reverse-runner.R"), envir = environment())
wave_worker_script <- file.path(dirname(script), "reverse-wave-worker.R")
worker_group_script <- file.path(dirname(script), "reverse-worker-group")
external_rscript <- normalizePath(
  file.path(R.home("bin"), "Rscript"), winslash = "/", mustWork = TRUE
)
self_arguments <- commandArgs(trailingOnly = TRUE)
if (length(self_arguments) &&
    identical(self_arguments[[1L]], "--interrupt-coordinator")) {
  if (length(self_arguments) != 2L) {
    rr_fail("interrupt coordinator requires one state directory")
  }
  state <- normalizePath(
    self_arguments[[2L]], winslash = "/", mustWork = TRUE
  )
  tasks <- lapply(c("quick", "blocked"), function(name) {
    row <- file.path(state, name)
    dir.create(file.path(row, "metadata"), recursive = TRUE)
    dir.create(file.path(row, "worker-state"))
    list(
      name = name, peer_pid = file.path(state, "grandchild.pid"),
      rr_transport = list(
        root = row, state = file.path(row, "worker-state"),
        launch = file.path(row, "metadata", "worker-launch.tsv"),
        result = file.path(row, "metadata", "worker-result.rds"),
        completion = file.path(row, "metadata", "worker-completion.tsv"),
        log = file.path(row, "worker-transport.log")
      )
    )
  })
  invisible(rr_parallel_wave(tasks, function(task) {
    if (identical(task$name, "quick")) {
      deadline <- Sys.time() + 10
      while (!file.exists(task$peer_pid) && Sys.time() < deadline) {
        Sys.sleep(0.01)
      }
      if (!file.exists(task$peer_pid)) {
        stop("blocked sibling did not publish its grandchild", call. = FALSE)
      }
      return("durable-completed-sibling")
    }
    command <- paste0(
      "sleep 300 & child=$!; printf '%s\\n' \"$child\" > ",
      shQuote(task$peer_pid), "; wait \"$child\""
    )
    child <- processx::process$new(
      "/bin/sh", c("-c", command), cleanup = TRUE, cleanup_tree = TRUE,
      supervise = TRUE, linux_pdeathsig = TRUE
    )
    child$wait(-1)
    "blocked-worker-unexpectedly-returned"
  }, 2L, external_rscript, wave_worker_script, worker_group_script, 60L))
  quit(save = "no", status = 0L)
}
tracked_runner <- file.path(dirname(script), "test-reverse-dependencies.R")
tracked_lines <- readLines(tracked_runner, warn = FALSE)
if (sum(grepl("rr_record_full_hash_pass\\(", tracked_lines)) != 2L ||
    any(grepl("compat_tree_content_sha256", tracked_lines, fixed = TRUE)) ||
    !any(grepl('"--no-stop-on-test-error"', tracked_lines, fixed = TRUE)) ||
    !any(grepl("rr_parallel_wave(tasks", tracked_lines, fixed = TRUE)) ||
    sum(grepl("wave_resource <- reverse_resource_report()", tracked_lines,
      fixed = TRUE)) != 1L ||
    !any(grepl("resource-jobs", tracked_lines, fixed = TRUE)) ||
    !any(grepl("reverse_recover_external_workers", tracked_lines,
      fixed = TRUE)) ||
    !any(grepl("wave_worker_script", tracked_lines, fixed = TRUE)) ||
    !any(grepl('if (!arguments$plan_only)', tracked_lines, fixed = TRUE)) ||
    !any(grepl('if (arguments$resume)', tracked_lines, fixed = TRUE))) {
  rr_fail("tracked reverse runner lost its verification-economy call graph")
}
controls <- rr_reverse_nested_controls()
if (anyDuplicated(names(controls)) ||
    !identical(controls[["TESTTHAT_CPUS"]], "1") ||
    !identical(controls[["CMAKE_BUILD_PARALLEL_LEVEL"]], "1") ||
    !identical(controls[["GOTO_NUM_THREADS"]], "1") ||
    !identical(controls[["R_FUTURE_PLAN"]], "sequential") ||
    !identical(controls[["R_FUTURE_AVAILABLECORES_FALLBACK"]], "1") ||
    !identical(controls[["R_PARALLELLY_AVAILABLECORES_FALLBACK"]], "1")) {
  rr_fail("uniform nested-parallel controls are incomplete")
}
mlr3_controls <- rr_reverse_check_nested_controls("mlr3")
ordinary_check_controls <- rr_reverse_check_nested_controls("bbotk")
expected_mlr3_changes <- c(
  "MC_CORES", "R_FUTURE_AVAILABLECORES_FALLBACK",
  "R_PARALLELLY_AVAILABLECORES_FALLBACK", "OMP_NUM_THREADS",
  "OMP_THREAD_LIMIT"
)
observed_mlr3_changes <- names(controls)[controls != mlr3_controls]
if (!identical(ordinary_check_controls, controls) ||
    !identical(observed_mlr3_changes, expected_mlr3_changes) ||
    !identical(unname(controls[expected_mlr3_changes]), rep("1", 5L)) ||
    !identical(unname(mlr3_controls[expected_mlr3_changes]), rep("2", 5L)) ||
    !identical(
      mlr3_controls[setdiff(names(controls), expected_mlr3_changes)],
      controls[setdiff(names(controls), expected_mlr3_changes)]
    )) {
  rr_fail("mlr3's check-only two-CPU projection is not exact")
}
synthetic_environment <- c(SYNTHETIC_SENTINEL = "retained", controls)
ordinary_check_environment <- rr_reverse_check_environment(
  synthetic_environment, "miesmuschel"
)
mlr3_check_environment <- rr_reverse_check_environment(
  synthetic_environment, "mlr3"
)
if (!identical(ordinary_check_environment, synthetic_environment) ||
    !identical(mlr3_check_environment[["SYNTHETIC_SENTINEL"]], "retained") ||
    !identical(
      mlr3_check_environment[names(mlr3_controls)], mlr3_controls
    ) || !identical(synthetic_environment[names(controls)], controls)) {
  rr_fail("reverse check environment projection escaped its exact scope")
}
locale_environment <- rr_consumer_locale_environment()
expected_locale_environment <- c(
  LC_ALL = "C.UTF-8", LANG = "C.UTF-8", LANGUAGE = "C", TZ = "UTC"
)
if (!identical(locale_environment, expected_locale_environment)) {
  rr_fail("deterministic consumer locale controls are incomplete")
}

expect_error <- function(expression, pattern = NULL) {
  message <- tryCatch({ force(expression); NULL }, error = conditionMessage)
  if (is.null(message)) rr_fail("expected an error")
  if (!is.null(pattern) && !grepl(pattern, message, fixed = TRUE)) {
    rr_fail("unexpected error: ", message)
  }
  invisible(message)
}

temporary <- tempfile("paradox-reverse-economy-self-test-")
if (!dir.create(temporary)) rr_fail("could not create self-test root")
on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)

# A worker must replace hostile inherited locale and timezone values before it
# deserializes or executes any consumer-row closure.
worker_locale_state <- file.path(temporary, "worker-locale-state")
dir.create(worker_locale_state)
hostile_worker_environment <- Sys.getenv()
hostile_worker_environment[names(locale_environment)] <- c(
  "C", "POSIX", "de_DE", "Pacific/Honolulu"
)
worker_environment <- rr_worker_environment(
  worker_locale_state, hostile_worker_environment
)
if (!identical(
    unname(as.character(worker_environment[names(locale_environment)])),
    unname(locale_environment)
  )) {
  rr_fail("external reverse worker inherited a hostile locale")
}
if (!identical(
    unname(as.character(worker_environment[names(controls)])),
    unname(controls)
  )) {
  rr_fail("external reverse worker inherited a check-only CPU exception")
}

# Run mutation is exclusive.  A live owner is observed but never signalled;
# an authenticated dead owner is archived before a new lock is published.
locks <- file.path(temporary, "mutation-locks")
active_lock <- rr_acquire_mutation_lock(locks, "active")
expect_error(
  rr_acquire_mutation_lock(locks, "active"), "already owned by active"
)
rr_release_mutation_lock(active_lock)
# A crash after retirement rename leaves the complete private owner directory
# while making the canonical name immediately available to one new owner.
retiring_lock <- rr_acquire_mutation_lock(locks, "release-crash")
retirement <- rr_mutation_lock_retirement_path(retiring_lock)
if (!file.rename(retiring_lock$path, retirement)) {
  rr_fail("could not simulate post-rename release interruption")
}
invisible(rr_validate_mutation_lock_owner(retirement, retiring_lock$owner))
replacement_lock <- rr_acquire_mutation_lock(locks, "release-crash")
expect_error(
  rr_acquire_mutation_lock(locks, "release-crash"),
  "already owned by active"
)
if (!dir.exists(retirement)) {
  rr_fail("new lock acquisition consumed a private retirement directory")
}
rr_release_mutation_lock(replacement_lock)
invisible(rr_validate_mutation_lock_owner(retirement, retiring_lock$owner))
unlink(retirement, recursive = TRUE, force = TRUE)
# If publication first loses to an active canonical lock and that owner retires
# before inspection, acquisition retries instead of diagnosing an absent lock
# as malformed.  Masking file.rename makes this narrow boundary deterministic.
boundary_lock <- rr_acquire_mutation_lock(locks, "release-boundary")
boundary_retirement <- rr_mutation_lock_retirement_path(boundary_lock)
base_file_rename <- base::file.rename
publication_intercepted <- FALSE
file.rename <- function(from, to) {
  if (!publication_intercepted && identical(to, boundary_lock$path) &&
      startsWith(basename(from), ".release-boundary.new-")) {
    if (!base_file_rename(boundary_lock$path, boundary_retirement)) {
      rr_fail("could not simulate release during losing publication")
    }
    publication_intercepted <<- TRUE
    return(FALSE)
  }
  base_file_rename(from, to)
}
boundary_replacement <- tryCatch(
  rr_acquire_mutation_lock(locks, "release-boundary"),
  finally = rm(file.rename, envir = environment())
)
if (!publication_intercepted || !dir.exists(boundary_retirement)) {
  rr_fail("release-boundary acquisition retry was not exercised")
}
rr_release_mutation_lock(boundary_replacement)
invisible(rr_validate_mutation_lock_owner(
  boundary_retirement, boundary_lock$owner
))
unlink(boundary_retirement, recursive = TRUE, force = TRUE)
# A crash before the rename leaves a valid canonical dead-owner lock.  The
# existing stale-owner path below must archive it before replacement.
dir.create(file.path(locks, "stale-run.lock"))
rr_write_tsv(data.frame(
  schema = "1", pid = "2147483647", process_start = "-",
  token = strrep("a", 64L), created_utc = "2026-07-15T00:00:00Z",
  stringsAsFactors = FALSE
), file.path(locks, "stale-run.lock", "owner.tsv"))
stale_lock <- rr_acquire_mutation_lock(locks, "stale-run")
if (!dir.exists(file.path(
    locks, "stale", paste0("stale-run-2147483647-", strrep("a", 64L))
  ))) {
  rr_fail("stale mutation lock was not conservatively archived")
}
rr_release_mutation_lock(stale_lock)

# Malformed watchdog identities must fail before reaching any kill command.
invalid_identities <- list(
  c("--watchdog", "abc", "token", "1", "1", "0"),
  c("--watchdog", "01", "token", "1", "1", "0"),
  c("--watchdog", "1", "", "1", "1", "0"),
  c("--watchdog", "1", "token", "-1", "1", "0"),
  c("--watchdog", "1", "token", "1", "-1", "0")
)
for (arguments in invalid_identities) {
  status <- suppressWarnings(system2(
    worker_group_script, arguments, stdout = FALSE, stderr = FALSE
  ))
  if (identical(status, 0L)) rr_fail("unsafe watchdog identity was admitted")
}

external_task <- function(task, directory) {
  dir.create(file.path(directory, "metadata"), recursive = TRUE)
  dir.create(file.path(directory, "worker-state"))
  task$rr_transport <- list(
    root = directory, state = file.path(directory, "worker-state"),
    launch = file.path(directory, "metadata", "worker-launch.tsv"),
    result = file.path(directory, "metadata", "worker-result.rds"),
    completion = file.path(directory, "metadata", "worker-completion.tsv"),
    log = file.path(directory, "worker-transport.log")
  )
  task
}

# Row-local caches include Python's two independent bytecode controls and do
# not escape the disposable row.
row_directory <- file.path(temporary, "row")
dir.create(row_directory)
row_environment <- rr_row_cache_environment(row_directory)
if (anyDuplicated(names(row_environment)) ||
    !identical(row_environment[["PYTHONDONTWRITEBYTECODE"]], "1") ||
    !identical(row_environment[["PYTHONPYCACHEPREFIX"]],
      file.path(row_directory, "cache", "python-bytecode")) ||
    any(!startsWith(
      row_environment[names(row_environment) != "PYTHONDONTWRITEBYTECODE"],
      paste0(row_directory, "/")
    ))) {
  rr_fail("row-local cache environment escaped its row")
}

# The cheap boundary fingerprint is path-bound, non-following, and catches an
# in-place write through ctime/size without reading payload bytes.
metadata_root <- file.path(temporary, "metadata")
dir.create(metadata_root)
writeLines("alpha", file.path(metadata_root, "a"), useBytes = TRUE)
writeLines("beta-long", file.path(metadata_root, "b"), useBytes = TRUE)
before_entries <- rr_metadata_entries(metadata_root, "fixture")
before_hash <- rr_payload_sha256(before_entries)
Sys.sleep(0.05)
writeLines("alpha-now-longer", file.path(metadata_root, "a"), useBytes = TRUE)
after_entries <- rr_metadata_entries(metadata_root, "fixture")
after_hash <- rr_payload_sha256(after_entries)
if (identical(before_hash, after_hash) ||
    !all(c("path", "ctime", "device", "inode", "hard_link_identity") %in%
      names(after_entries))) {
  rr_fail("metadata boundary did not detect an in-place write")
}
path_swapped <- after_entries
path_swapped$path[path_swapped$path %in% c("a", "b")] <- c("b", "a")
path_swapped <- path_swapped[order(path_swapped$path), , drop = FALSE]
rownames(path_swapped) <- NULL
if (identical(rr_payload_sha256(path_swapped), after_hash)) {
  rr_fail("metadata fingerprint is not bound to paths")
}
link <- file.path(metadata_root, "link")
if (file.symlink(file.path(metadata_root, "a"), link)) {
  link_row <- rr_metadata_entries(metadata_root, "fixture")
  link_row <- link_row[link_row$path == "link", , drop = FALSE]
  if (nrow(link_row) != 1L || link_row$type != "symlink" ||
      !nzchar(link_row$link_target)) {
    rr_fail("metadata inventory followed or lost a symbolic link")
  }
  unlink(link)
}

# Instrument the expensive boundary.  An actual stage has exactly two passes
# (two small fixture libraries per pass); plan and self-test ledgers have zero.
candidate <- file.path(temporary, "candidate")
dependency <- file.path(temporary, "dependency")
dir.create(candidate)
dir.create(dependency)
writeLines("candidate", file.path(candidate, "payload"), useBytes = TRUE)
writeLines("dependency", file.path(dependency, "payload"), useBytes = TRUE)
full_hash_calls <- 0L
fixture_hasher <- function(path) {
  full_hash_calls <<- full_hash_calls + 1L
  unname(tools::sha256sum(file.path(path, "payload")))
}
actual_ledger <- file.path(temporary, "actual-full-hashes.tsv")
rr_write_tsv(rr_empty_full_hash_ledger(), actual_ledger)
contexts <- c("stage_start", "stage_completion_postflight")
identity <- rr_record_full_hash_pass(
  actual_ledger, contexts[[1L]], contexts, candidate, dependency,
  expected = NULL, hasher = fixture_hasher
)
invisible(rr_record_full_hash_pass(
  actual_ledger, contexts[[2L]], contexts, candidate, dependency,
  expected = identity, hasher = fixture_hasher
))
invisible(rr_validate_full_hash_ledger(actual_ledger, contexts, identity))
if (full_hash_calls != 4L) {
  rr_fail("actual fixture did not perform exactly two two-library passes")
}
expect_error(rr_record_full_hash_pass(
  actual_ledger, "third", contexts, candidate, dependency,
  expected = identity, hasher = fixture_hasher
), "unexpected or repeated")
plan_ledger <- file.path(temporary, "plan-full-hashes.tsv")
rr_write_tsv(rr_empty_full_hash_ledger(), plan_ledger)
calls_before_plan <- full_hash_calls
invisible(rr_validate_full_hash_ledger(plan_ledger, character()))
if (full_hash_calls != calls_before_plan) {
  rr_fail("plan/self-test validation invoked a full-tree hasher")
}

# Ordinary evidence seals bind files and empty directories.  Composite seals
# verify each accepted row (including a failed result) but hash only row
# receipts at the run level.
stage <- file.path(temporary, "stage")
dir.create(file.path(stage, "metadata"), recursive = TRUE)
dir.create(file.path(stage, "empty"))
writeLines("payload", file.path(stage, "payload"), useBytes = TRUE)
rr_write_tsv(data.frame(field = "status", value = "fixture",
  stringsAsFactors = FALSE), file.path(stage, "metadata", "completion.tsv"))
invisible(rr_seal_stage(stage))
invisible(rr_verify_stage(stage))
dir.create(file.path(stage, "unrecorded"))
expect_error(rr_verify_stage(stage), "directory set changed")
unlink(file.path(stage, "unrecorded"), recursive = TRUE)
writeLines("tampered", file.path(stage, "payload"), useBytes = TRUE)
expect_error(rr_verify_stage(stage), "contents changed")

# Receipt publication is restartable at every final-path boundary.  The seal
# remains the last atomic publication and stale private temporaries are pruned
# only when their recorded publisher PID is no longer alive.
partial_stage <- file.path(temporary, "partial-stage-publication")
dir.create(file.path(partial_stage, "metadata"), recursive = TRUE)
writeLines("payload", file.path(partial_stage, "payload"), useBytes = TRUE)
rr_write_tsv(
  rr_stage_directories(partial_stage),
  file.path(partial_stage, rr_stage_directories_relative)
)
stale_temporary <- paste0(
  file.path(partial_stage, rr_stage_manifest_relative), ".new-2147483647"
)
writeLines("interrupted", stale_temporary, useBytes = TRUE)
invisible(rr_seal_stage(partial_stage))
invisible(rr_verify_stage(partial_stage))
if (file.exists(stale_temporary)) {
  rr_fail("stale interrupted seal publication survived recovery")
}

fifo_stage <- file.path(temporary, "fifo-stage")
dir.create(file.path(fifo_stage, "metadata"), recursive = TRUE)
fifo <- file.path(fifo_stage, "blocking-fifo")
mkfifo <- unname(Sys.which("mkfifo"))
if (!nzchar(mkfifo) || system2(mkfifo, fifo) != 0L) {
  rr_fail("could not create FIFO adversarial fixture")
}
expect_error(rr_stage_inventory(fifo_stage), "non-regular path")
unlink(fifo)

composite <- file.path(temporary, "composite")
dir.create(file.path(composite, "metadata"), recursive = TRUE)
dir.create(file.path(composite, "packages"))
dir.create(file.path(composite, "interrupted"))
for (package in c("passed", "failed")) {
  child <- file.path(composite, "packages", package)
  dir.create(file.path(child, "metadata"), recursive = TRUE)
  writeLines(package, file.path(child, "result.tsv"), useBytes = TRUE)
  rr_write_tsv(data.frame(field = "status", value = package,
    stringsAsFactors = FALSE), file.path(child, "metadata", "completion.tsv"))
  invisible(rr_seal_stage(child))
}
rr_write_tsv(data.frame(field = "status", value = "completed_with_failures",
  stringsAsFactors = FALSE), file.path(composite, "metadata", "completion.tsv"))
invisible(rr_seal_composite_stage(composite))
composite_evidence <- rr_verify_composite_stage(composite)
if (composite_evidence$rows != 2L) {
  rr_fail("composite stage did not accept both sealed bounded results")
}
extra_child_file <- file.path(composite, "packages", "extra-file")
writeLines("unexpected", extra_child_file, useBytes = TRUE)
expect_error(rr_composite_children(composite), "non-directory entry")
unlink(extra_child_file)
unplanned <- file.path(composite, "packages", "unplanned")
dir.create(unplanned)
expect_error(rr_validate_composite_children(
  composite, c("passed", "failed"), c("passed", "failed")
), "unplanned package")
unlink(unplanned, recursive = TRUE)
writeLines("tampered", file.path(composite, "packages", "passed", "result.tsv"))
expect_error(rr_verify_composite_stage(composite), "contents changed")

partial_composite <- file.path(temporary, "partial-composite-publication")
dir.create(file.path(partial_composite, "metadata"), recursive = TRUE)
dir.create(file.path(partial_composite, "packages"))
dir.create(file.path(partial_composite, "interrupted"))
partial_child <- file.path(partial_composite, "packages", "fixture")
dir.create(file.path(partial_child, "metadata"), recursive = TRUE)
writeLines("row", file.path(partial_child, "result.tsv"), useBytes = TRUE)
rr_write_tsv(data.frame(field = "status", value = "passed",
  stringsAsFactors = FALSE), file.path(partial_child, "metadata", "completion.tsv"))
invisible(rr_seal_stage(partial_child))
rr_write_tsv(
  rr_composite_directories(partial_composite),
  file.path(partial_composite, rr_stage_directories_relative)
)
invisible(rr_seal_composite_stage(partial_composite))
invisible(rr_verify_composite_stage(partial_composite))

plan_inventory <- file.path(temporary, "plan-child-inventory")
dir.create(file.path(plan_inventory, "metadata"), recursive = TRUE)
dir.create(file.path(plan_inventory, "packages"))
dir.create(file.path(plan_inventory, "interrupted"))
dir.create(file.path(plan_inventory, "packages", "other"))
expect_error(rr_validate_composite_children(
  plan_inventory, c("planned", "other"), character(),
  allow_interrupted = FALSE
), "differ from accepted")

# Install keys contain install-affecting bytes, not plan/report/verifier noise.
install_inputs <- data.frame(
  field = c(
    "schema", "package", "version", "archive_sha256",
    "candidate_content_sha256", "dependency_library_content_sha256",
    "install_arguments", "file.install_worker.sha256"
  ),
  value = c(
    "1", "fixture", "1.0.0", strrep("a", 64L), strrep("b", 64L),
    strrep("c", 64L), "CMD INSTALL TARGET ARCHIVE", strrep("d", 64L)
  ), stringsAsFactors = FALSE
)
install_key <- rr_install_cache_key(install_inputs)
roundtrip_path <- file.path(temporary, "install-inputs-roundtrip.tsv")
attributed_inputs <- install_inputs
rownames(attributed_inputs) <- paste0("generated.", seq_len(nrow(attributed_inputs)))
attr(attributed_inputs, "report-only") <- "ignored"
rr_write_tsv(attributed_inputs, roundtrip_path)
if (!identical(install_key, rr_install_cache_key(rr_read_tsv(
    roundtrip_path, c("field", "value")
  )))) {
  rr_fail("install cache key is not stable across its retained TSV boundary")
}
changed <- install_inputs
changed$value[changed$field == "archive_sha256"] <- strrep("e", 64L)
if (identical(install_key, rr_install_cache_key(changed))) {
  rr_fail("archive byte change did not invalidate install cache")
}
forbidden <- rbind(install_inputs, data.frame(
  field = "file.verifier.sha256", value = strrep("f", 64L),
  stringsAsFactors = FALSE
))
expect_error(rr_install_cache_key(forbidden), "verifier/report-only")

fixture_content_hash <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- list.files(path, all.files = TRUE, recursive = TRUE,
    full.names = TRUE, no.. = TRUE)
  info <- file.info(files, extra_cols = FALSE)
  files <- files[!info$isdir]
  relative <- substring(files, nchar(path) + 2L)
  ordering <- order(relative, method = "radix")
  payload <- list(
    path = relative[ordering], size = unname(info$size[!info$isdir][ordering]),
    sha256 = unname(tools::sha256sum(files[ordering]))
  )
  path_out <- tempfile("fixture-content-")
  on.exit(unlink(path_out), add = TRUE)
  writeBin(serialize(payload, NULL, version = 3L, xdr = TRUE), path_out)
  rr_sha256(path_out)
}

make_cache <- function(path, completion_hash) {
  dir.create(file.path(path, "metadata"), recursive = TRUE)
  package <- file.path(path, "library", "fixture")
  dir.create(package, recursive = TRUE)
  writeLines(c("Package: fixture", "Version: 1.0.0"),
    file.path(package, "DESCRIPTION"), useBytes = TRUE)
  writeLines("* DONE (fixture)", file.path(path, "install.log"), useBytes = TRUE)
  rr_write_tsv(install_inputs, file.path(path, "metadata", "cache-inputs.tsv"))
  actual_hash <- fixture_content_hash(package)
  rr_write_tsv(data.frame(
    field = c("status", "cache_key", "package", "version",
      "installed_content_sha256", "finished_utc"),
    value = c("installed", basename(path), "fixture", "1.0.0",
      if (identical(completion_hash, "actual")) actual_hash else completion_hash,
      "2026-07-15T00:00:00Z"), stringsAsFactors = FALSE
  ), file.path(path, "metadata", "completion.tsv"))
  rr_seal_stage(path)
  actual_hash
}

valid_cache <- file.path(temporary, install_key)
valid_hash <- make_cache(valid_cache, "actual")
if (!identical(
    rr_tree_content_sha256(file.path(valid_cache, "library", "fixture")),
    valid_hash
  )) {
  rr_fail("one-pass tree content contract differs from compat fingerprint")
}
library_receipt <- rr_tree_content_receipt(
  file.path(valid_cache, "library"), "fixture"
)
if (!identical(library_receipt$subtree_hash, valid_hash)) {
  rr_fail("one-pass protected-library receipt lost its package subtree hash")
}
valid <- rr_validate_install_cache(
  valid_cache, install_key, install_inputs, "fixture", "1.0.0",
  fixture_content_hash
)
if (!identical(valid$installed_content_sha256, valid_hash)) {
  rr_fail("valid install cache semantic hash changed")
}
corrupt_key_inputs <- install_inputs
corrupt_key_inputs$value[corrupt_key_inputs$field == "archive_sha256"] <-
  strrep("9", 64L)
corrupt_key <- rr_install_cache_key(corrupt_key_inputs)
corrupt_cache <- file.path(temporary, corrupt_key)
old_inputs <- install_inputs
install_inputs <- corrupt_key_inputs
invisible(make_cache(corrupt_cache, strrep("0", 64L)))
child_started <- FALSE
expect_error({
  rr_validate_install_cache(
    corrupt_cache, corrupt_key, corrupt_key_inputs, "fixture", "1.0.0",
    fixture_content_hash
  )
  child_started <- TRUE
}, "semantic completion")
install_inputs <- old_inputs
if (child_started) rr_fail("semantic cache corruption reached a test child")

# A bounded wave really overlaps independent workers, waits for every sibling,
# and returns results in task order even when one worker fails first.
parallel_root <- file.path(temporary, "parallel-wave")
dir.create(parallel_root)
parallel_tasks <- lapply(1:2, function(index) {
  directory <- file.path(parallel_root, paste0("row-", index))
  external_task(list(index = index, directory = directory), directory)
})
parallel_wave <- rr_parallel_wave(parallel_tasks, function(task) {
  writeLines("ready", file.path(task$directory, "ready"), useBytes = TRUE)
  peer <- file.path(
    dirname(task$directory), paste0("row-", if (task$index == 1L) 2L else 1L),
    "ready"
  )
  deadline <- Sys.time() + 5
  while (!file.exists(peer) && Sys.time() < deadline) Sys.sleep(0.01)
  if (!file.exists(peer)) stop("parallel peer never started", call. = FALSE)
  writeLines("finished", file.path(task$directory, "finished"), useBytes = TRUE)
  if (task$index == 2L) stop("synthetic worker failure", call. = FALSE)
  task$index
}, 2L, external_rscript, wave_worker_script, worker_group_script, 30L)
if (!identical(parallel_wave$backend, "external") ||
    length(parallel_wave$results) != 2L ||
    !isTRUE(parallel_wave$results[[1L]]$ok) ||
    isTRUE(parallel_wave$results[[2L]]$ok) ||
    !identical(parallel_wave$results[[1L]]$value, 1L) ||
    !grepl("synthetic worker failure", parallel_wave$results[[2L]]$error,
      fixed = TRUE) ||
    any(!file.exists(file.path(
      parallel_root, paste0("row-", 1:2), "finished"
    )))) {
  rr_fail("bounded wave lost concurrency, ordering, or a sibling failure")
}

# The coordinator imposes an outer deadline even when the worker never reaches
# its own bounded child calls.
hanging_directory <- file.path(temporary, "hanging-worker")
hanging_task <- external_task(list(), hanging_directory)
hanging_started <- proc.time()[["elapsed"]]
hanging <- rr_parallel_wave(
  list(hanging_task), function(task) {
    Sys.sleep(300)
    TRUE
  }, 1L, external_rscript, wave_worker_script, worker_group_script, 0.2
)
hanging_elapsed <- proc.time()[["elapsed"]] - hanging_started
if (isTRUE(hanging$results[[1L]]$ok) ||
    !grepl("deadline exceeded", hanging$results[[1L]]$error, fixed = TRUE) ||
    hanging_elapsed > 15) {
  rr_fail("outer worker deadline did not terminate a hanging interpreter")
}

# With the Linux token sweep deliberately disabled, setsid/processx ownership
# still removes a TERM-ignoring background descendant before accepting the row.
descendant_directory <- file.path(temporary, "term-ignoring-descendant")
descendant_pid_path <- file.path(descendant_directory, "descendant.pid")
descendant_task <- external_task(list(pid_path = descendant_pid_path),
  descendant_directory)
disabled_proc_environment <- Sys.getenv()
disabled_proc_environment[["PARADOX_REVERSE_DISABLE_PROC_SWEEP"]] <- "1"
descendant <- rr_parallel_wave(
  list(descendant_task), function(task) {
    process <- processx::process$new(
      "/bin/sh", c("-c", "trap '' TERM; while :; do sleep 30; done"),
      cleanup = FALSE, cleanup_tree = FALSE, supervise = FALSE
    )
    writeLines(as.character(process$get_pid()), task$pid_path, useBytes = TRUE)
    process$get_pid()
  }, 1L, external_rscript, wave_worker_script, worker_group_script, 15L,
  inherited_environment = disabled_proc_environment
)
if (!isTRUE(descendant$results[[1L]]$ok)) {
  rr_fail("TERM-ignoring descendant fixture did not complete")
}
descendant_pid <- as.integer(readLines(descendant_pid_path, warn = FALSE))
if (isTRUE(tryCatch(
    ps::ps_is_running(ps::ps_handle(descendant_pid)),
    error = function(...) FALSE
  ))) {
  suppressWarnings(try(tools::pskill(descendant_pid, 9L), silent = TRUE))
  rr_fail("normal worker completion left a TERM-ignoring descendant alive")
}

# Parent termination cannot orphan a worker's grandchildren.  A sibling that
# atomically published its result first remains byte-identical and only the
# missing sibling is selected by the synthetic resume.
interrupt_case <- function(signal, label) {
  state <- file.path(temporary, paste0("interrupt-", label))
  dir.create(state)
  coordinator_log <- file.path(state, "coordinator.log")
  coordinator <- processx::process$new(
    external_rscript,
    c("--vanilla", script, "--interrupt-coordinator", state),
    stdout = coordinator_log, stderr = "2>&1", cleanup = TRUE,
    cleanup_tree = TRUE, supervise = TRUE, linux_pdeathsig = TRUE
  )
  grandchild <- NA_integer_
  on.exit({
    if (isTRUE(tryCatch(coordinator$is_alive(), error = function(...) FALSE))) {
      suppressWarnings(try(coordinator$kill_tree(), silent = TRUE))
    }
    if (!is.na(grandchild) && isTRUE(tryCatch(
        ps::ps_is_running(ps::ps_handle(grandchild)),
        error = function(...) FALSE))) {
      suppressWarnings(try(tools::pskill(grandchild, 9L), silent = TRUE))
    }
  }, add = TRUE)
  quick_result <- file.path(state, "quick", "metadata", "worker-result.rds")
  quick_completion <- file.path(
    state, "quick", "metadata", "worker-completion.tsv"
  )
  grandchild_path <- file.path(state, "grandchild.pid")
  deadline <- Sys.time() + 15
  while ((!file.exists(quick_result) || !file.exists(quick_completion) ||
      !file.exists(grandchild_path)) &&
      coordinator$is_alive() && Sys.time() < deadline) {
    Sys.sleep(0.02)
  }
  if (!file.exists(quick_result) || !file.exists(quick_completion) ||
      !file.exists(grandchild_path) || !coordinator$is_alive()) {
    rr_fail("interrupt fixture did not reach its supervised overlap: ",
      paste(readLines(coordinator_log, warn = FALSE), collapse = "\n"))
  }
  grandchild_text <- readLines(grandchild_path, warn = FALSE)
  if (length(grandchild_text) != 1L ||
      !grepl("^[1-9][0-9]*$", grandchild_text)) {
    rr_fail("interrupt fixture published a malformed grandchild PID")
  }
  grandchild <- as.integer(grandchild_text)
  completed <- readRDS(quick_result)
  completed_hash <- rr_sha256(quick_result)
  if (!isTRUE(completed$ok) ||
      !identical(completed$value, "durable-completed-sibling") ||
      !isTRUE(ps::ps_is_running(ps::ps_handle(grandchild)))) {
    rr_fail("interrupt fixture lost overlap or its completed sibling")
  }
  coordinator$signal(signal)
  coordinator$wait(10000)
  if (coordinator$is_alive()) {
    rr_fail("coordinator survived synthetic ", label)
  }
  deadline <- Sys.time() + 10
  while (isTRUE(tryCatch(
      ps::ps_is_running(ps::ps_handle(grandchild)),
      error = function(...) FALSE)) && Sys.time() < deadline) {
    Sys.sleep(0.02)
  }
  if (isTRUE(tryCatch(
      ps::ps_is_running(ps::ps_handle(grandchild)),
      error = function(...) FALSE))) {
    rr_fail("supervised ", label, " left a worker grandchild alive")
  }
  blocked_result <- file.path(
    state, "blocked", "metadata", "worker-result.rds"
  )
  blocked_completion <- file.path(
    state, "blocked", "metadata", "worker-completion.tsv"
  )
  blocked_launch <- file.path(
    state, "blocked", "metadata", "worker-launch.tsv"
  )
  if (!file.exists(quick_result) || !file.exists(quick_completion) ||
      !file.exists(blocked_launch) || file.exists(blocked_completion)) {
    rr_fail("interruption did not retain exactly the completed sibling")
  }
  resume_directory <- file.path(state, "resume-blocked")
  resume_task <- external_task(list(name = "blocked"), resume_directory)
  resumed <- rr_parallel_wave(
    list(resume_task), function(task) paste0(task$name, "-resumed"), 1L,
    external_rscript, wave_worker_script, worker_group_script, 30L
  )
  if (!isTRUE(resumed$results[[1L]]$ok) ||
      !identical(resumed$results[[1L]]$value, "blocked-resumed") ||
      !identical(rr_sha256(quick_result), completed_hash)) {
    rr_fail("resume reran or changed the completed sibling after ", label)
  }
  invisible(TRUE)
}
interrupt_case(15L, "sigterm")
interrupt_case(9L, "sigkill")

# The parent records the complete wave first, then seals and accepts successful
# rows in plan order.  A failed worker remains unsealed for quarantine/retry;
# the previously accepted sibling is never rewritten.
promotion_root <- file.path(temporary, "parent-promotion")
promotion_metadata <- file.path(promotion_root, "metadata")
promotion_packages <- file.path(promotion_root, "packages")
promotion_interrupted <- file.path(promotion_root, "interrupted")
dir.create(promotion_metadata, recursive = TRUE)
dir.create(promotion_packages)
dir.create(promotion_interrupted)
promotion_acceptance <- file.path(promotion_metadata, "accepted.tsv")
promotion_waves <- file.path(promotion_metadata, "waves.tsv")
rr_write_tsv(rr_empty_reverse_acceptance(), promotion_acceptance)
rr_write_tsv(rr_empty_reverse_waves(), promotion_waves)
promotion_plan <- c("first", "second")
promotion_protected <- strrep("9", 64L)
rr_append_reverse_wave(
  promotion_waves, 1:2, promotion_plan,
  2L, parallel_wave$backend, 2L, integer(), integer(), 2L, NULL,
  "metadata/resource-wave-1.tsv",
  strrep("8", 64L), promotion_protected, promotion_protected,
  "2026-07-15T00:00:00Z", "2026-07-15T00:00:01Z"
)
first_row <- file.path(promotion_packages, "first")
second_row <- file.path(promotion_packages, "second")
dir.create(file.path(first_row, "metadata"), recursive = TRUE)
dir.create(file.path(second_row, "metadata"), recursive = TRUE)
first_result <- data.frame(
  status = "passed", classification = "passed", stringsAsFactors = FALSE
)
rr_write_tsv(first_result, file.path(first_row, "result.tsv"))
rr_write_tsv(data.frame(field = "wave", value = "1", stringsAsFactors = FALSE),
  file.path(first_row, "metadata", "completion.tsv"))
first_evidence <- rr_seal_stage(first_row)
rr_append_reverse_acceptance(
  promotion_acceptance, 1L, "first", 1L, first_result, first_evidence
)
rr_write_tsv(data.frame(
  field = "error", value = parallel_wave$results[[2L]]$error,
  stringsAsFactors = FALSE
), file.path(second_row, "metadata", "worker-error.tsv"))
accepted_before_retry <- rr_validate_reverse_acceptance(
  promotion_acceptance, promotion_plan, promotion_packages
)
if (!identical(accepted_before_retry$package, "first") ||
    !identical(rr_validate_reverse_waves(
      promotion_waves, promotion_plan
    )$worker_error_indices, "2")) {
  rr_fail("parent did not retain the complete failed wave deterministically")
}
first_seal_before_retry <- rr_sha256(
  file.path(first_row, rr_stage_seal_relative)
)
invisible(rr_quarantine_unsealed_row(
  second_row, promotion_interrupted, "second"
))
dir.create(file.path(second_row, "metadata"), recursive = TRUE)
second_result <- data.frame(
  status = "failed", classification = "consumer_failure",
  stringsAsFactors = FALSE
)
rr_write_tsv(second_result, file.path(second_row, "result.tsv"))
rr_write_tsv(data.frame(field = "wave", value = "2", stringsAsFactors = FALSE),
  file.path(second_row, "metadata", "completion.tsv"))
rr_append_reverse_wave(
  promotion_waves, 2L, promotion_plan, 1L, "external", integer(), integer(),
  integer(), 2L, NULL,
  "metadata/resource-wave-2.tsv", strrep("8", 64L),
  promotion_protected, promotion_protected, "2026-07-15T00:00:02Z",
  "2026-07-15T00:00:03Z"
)
second_evidence <- rr_seal_stage(second_row)
rr_append_reverse_acceptance(
  promotion_acceptance, 2L, "second", 2L, second_result, second_evidence
)
accepted_after_retry <- rr_validate_reverse_acceptance(
  promotion_acceptance, promotion_plan, promotion_packages,
  require_complete = TRUE
)
if (!identical(accepted_after_retry$ordinal, c("1", "2")) ||
    !identical(accepted_after_retry$package, promotion_plan) ||
    !identical(first_seal_before_retry,
      rr_sha256(file.path(first_row, rr_stage_seal_relative)))) {
  rr_fail("retry changed or reordered a previously accepted sibling")
}
invisible(rr_validate_reverse_acceptance_waves(
  accepted_after_retry, rr_validate_reverse_waves(
    promotion_waves, promotion_plan
  )
))

# A successful member may not be silently discarded and rerun in a later wave.
discarded_waves_path <- file.path(temporary, "discarded-success-waves.tsv")
rr_write_tsv(rr_empty_reverse_waves(), discarded_waves_path)
for (ordinal in 1:2) {
  rr_append_reverse_wave(
    discarded_waves_path, 1L, "first", 1L, "external", integer(),
    integer(), integer(), 1L, NULL,
    paste0("metadata/discarded-", ordinal, ".tsv"), strrep("8", 64L),
    promotion_protected, promotion_protected,
    paste0("2026-07-15T00:00:0", ordinal - 1L, "Z"),
    paste0("2026-07-15T00:00:0", ordinal, "Z")
  )
}
discarded_acceptance <- data.frame(
  plan_index = "1", package = "first", wave = "2",
  stringsAsFactors = FALSE
)
expect_error(rr_validate_reverse_acceptance_waves(
  discarded_acceptance,
  rr_validate_reverse_waves(discarded_waves_path, "first")
), "silently discarded")

# A parent killed during process creation does not overstate the number of
# workers that crossed the wrapper-owned launch boundary.
partial_launch_waves <- file.path(temporary, "partial-launch-waves.tsv")
rr_write_tsv(rr_empty_reverse_waves(), partial_launch_waves)
rr_append_reverse_wave(
  partial_launch_waves, 1:2, c("launched", "unstarted"), 2L, "external",
  integer(), 1L, 2L, 2L, NULL, "metadata/resource-partial.tsv",
  strrep("8", 64L), promotion_protected, promotion_protected,
  "2026-07-15T00:00:00Z", "2026-07-15T00:00:01Z"
)
partial_launch <- rr_validate_reverse_waves(
  partial_launch_waves, c("launched", "unstarted")
)
if (!identical(partial_launch$workers_started[[1L]], "1") ||
    !identical(partial_launch$workers_completed[[1L]], "0") ||
    !identical(partial_launch$worker_missing_indices[[1L]], "1") ||
    !identical(partial_launch$worker_unstarted_indices[[1L]], "2")) {
  rr_fail("partial launch wave overstated started or completed workers")
}
overstated_waves <- file.path(temporary, "overstated-workers.tsv")
overstated <- rr_read_reverse_waves(partial_launch_waves)
overstated$workers_started[[1L]] <- "3"
rr_write_tsv(overstated, overstated_waves)
expect_error(
  rr_validate_reverse_waves(overstated_waves, c("launched", "unstarted")),
  "disagrees with its planned rows"
)

# Two concurrent builders may race for the same key, but exactly one sealed
# tree is promoted and the loser authenticates/reuses it without replacement.
race_root <- file.path(temporary, "cache-race")
dir.create(race_root)
race_target <- file.path(race_root, strrep("a", 64L))
race_staging <- file.path(race_root, c("first.new", "second.new"))
for (staging in race_staging) {
  dir.create(file.path(staging, "metadata"), recursive = TRUE)
  writeLines("identical-cache-bytes", file.path(staging, "payload"), useBytes = TRUE)
  writeLines("complete", file.path(staging, "metadata", "state"), useBytes = TRUE)
  invisible(rr_seal_stage(staging))
}
race_tasks <- lapply(seq_along(race_staging), function(index) list(
  index = index, staging = race_staging[[index]]
))
race_transport <- file.path(race_root, paste0("transport-", seq_along(race_tasks)))
race_tasks <- Map(external_task, race_tasks, race_transport)
race <- rr_parallel_wave(race_tasks, function(task) {
  ready <- file.path(race_root, paste0("ready-", task$index))
  writeLines("ready", ready, useBytes = TRUE)
  peer <- file.path(race_root, paste0("ready-", if (task$index == 1L) 2L else 1L))
  deadline <- Sys.time() + 5
  while (!file.exists(peer) && Sys.time() < deadline) Sys.sleep(0.01)
  if (!file.exists(peer)) stop("cache-race peer never started", call. = FALSE)
  rr_promote_install_cache(
    task$staging, race_target, function() rr_verify_stage(race_target)
  )$promoted
}, 2L, external_rscript, wave_worker_script, worker_group_script, 30L)
if (any(!vapply(race$results, function(value) isTRUE(value$ok), logical(1L))) ||
    sum(vapply(race$results, function(value) isTRUE(value$value), logical(1L))) != 1L ||
    any(file.exists(race_staging) | dir.exists(race_staging)) ||
    !is.list(rr_verify_stage(race_target))) {
  rr_fail("same-key install-cache promotion race was not atomic")
}

# Count coverage never invents zeros: every parsed Rout is exact, a subset is
# partial, and no recognized summary is explicitly unavailable.
counts <- file.path(temporary, "counts")
dir.create(counts)
writeLines("[ FAIL 0 | WARN 1 | SKIP 2 | PASS 30 ]",
  file.path(counts, "a.Rout"), useBytes = TRUE)
exact <- rr_parse_test_counts(counts)
if (!identical(exact$coverage, "exact") || exact$warn != 1L ||
    exact$skip != 2L || exact$pass != 30L) {
  rr_fail("exact test-count fixture was not parsed")
}
writeLines("unstructured output", file.path(counts, "b.Rout"), useBytes = TRUE)
partial <- rr_parse_test_counts(counts)
if (!identical(partial$coverage, "partial") || partial$files_parsed != 1L ||
    partial$files_total != 2L) {
  rr_fail("partial test-count fixture was mislabeled")
}
unlink(file.path(counts, "a.Rout"))
unavailable <- rr_parse_test_counts(counts)
if (!identical(unavailable$coverage, "unavailable") ||
    !is.na(unavailable$pass)) {
  rr_fail("unavailable test counts were invented")
}
oversized_output <- file.path(counts, "oversized.Rout")
writeBin(as.raw(rep(65L, 4096L)), oversized_output)
oversized <- rr_parse_test_counts(counts, max_output_bytes = 1024)
if (!identical(oversized$coverage, "unavailable") ||
    !identical(oversized$pathological_outputs,
      normalizePath(oversized_output, winslash = "/", mustWork = TRUE))) {
  rr_fail("pathological Rout size was not rejected without loading it")
}

# Exercise the tracked evidence verifier on a complete synthetic failed row.
# This is a schema/integration fixture only: it installs and checks nothing.
verification_stage <- file.path(temporary, "verification-stage")
verification_metadata <- file.path(verification_stage, "metadata")
verification_row <- file.path(verification_stage, "packages", "fixture")
dir.create(verification_metadata, recursive = TRUE)
dir.create(file.path(verification_stage, "interrupted"))
dir.create(file.path(verification_row, "metadata"), recursive = TRUE)
runner_source <- file.path(dirname(script), "reverse-runner.R")
worker_source <- file.path(dirname(script), "reverse-install-worker.R")
wave_worker_source <- file.path(dirname(script), "reverse-wave-worker.R")
worker_group_source <- file.path(dirname(script), "reverse-worker-group")
verifier_source <- file.path(dirname(script),
  "verify-reverse-dependency-evidence.R")
resource_jobs_source <- file.path(
  dirname(dirname(script)), "scripts", "environment", "resource-jobs"
)
invisible(file.copy(
  runner_source, file.path(verification_metadata, basename(runner_source))
))
invisible(file.copy(
  worker_source, file.path(verification_metadata, basename(worker_source))
))
invisible(file.copy(
  wave_worker_source,
  file.path(verification_metadata, basename(wave_worker_source))
))
invisible(file.copy(
  worker_group_source,
  file.path(verification_metadata, basename(worker_group_source))
))
invisible(file.copy(
  verifier_source, file.path(verification_metadata, basename(verifier_source))
))
invisible(file.copy(
  resource_jobs_source,
  file.path(verification_metadata, basename(resource_jobs_source))
))
resource_report_fixture <- data.frame(
  field = c(
    "schema", "profile", "platform", "online_cpus", "affinity_cpus",
    "cgroup_cpu_limit", "cpu_limit", "cpu_reserve", "cpu_per_job",
    "cpu_jobs", "memory_source", "memory_available_mib",
    "cgroup_memory_available_mib", "memory_reserve_mib",
    "memory_mib_per_job", "memory_jobs", "profile_max_jobs",
    "operator_max_jobs", "jobs"
  ),
  value = c(
    "1", "consumer", "Linux", "4", "4", "1", "1", "0", "2", "1",
    "proc_memavailable", "24576", "unlimited", "16384", "8192", "1",
    "4", "none", "1"
  ),
  stringsAsFactors = FALSE
)
invisible(rr_validate_resource_report(resource_report_fixture, "consumer"))
raised_resource_report <- resource_report_fixture
raised_resource_report$value[
  raised_resource_report$field == "jobs"
] <- "2"
expect_error(
  rr_validate_resource_report(raised_resource_report, "consumer"),
  "internally inconsistent"
)
rr_write_tsv(
  resource_report_fixture,
  file.path(verification_metadata, "resource-jobs-initial.tsv")
)
candidate_hash <- strrep("1", 64L)
candidate_library_hash <- strrep("2", 64L)
dependency_hash <- strrep("3", 64L)
protected_hash <- strrep("4", 64L)
archive_hash <- strrep("5", 64L)
run_values <- setNames(
  rep("synthetic", length(rr_reverse_run_fields())), rr_reverse_run_fields()
)
run_values[rr_reverse_run_sha256_fields()] <- strrep("9", 64L)
run_values[c(
  "schema", "run_id", "started_utc", "max_priority", "plan_only",
  "resume_capable", "install_timeout_seconds", "check_timeout_seconds",
  "worker_timeout_seconds", "scheduler_initial_ceiling",
  "scheduler_initial_jobs", "scheduler_initial_selection",
  "candidate_run_id", "candidate_ref", "candidate_commit", "candidate_tree",
  "candidate_version", "candidate_content_sha256",
  "candidate_library_content_sha256", "dependency_library_content_sha256",
  "r_version", "source_date_epoch", "resource_jobs_helper_sha256",
  "resource_jobs_initial_report_sha256", "runner_library_sha256",
  "install_worker_sha256", "wave_worker_sha256", "worker_group_sha256",
  "reverse_evidence_verifier_sha256"
)] <- c(
  "5", "synthetic", "2026-07-15T00:00:00Z", "0", "FALSE", "true",
  "60", "60", "1020", "1", "1", "automatic_resource_ceiling",
  "synthetic-candidate", "refs/synthetic", strrep("a", 40L),
  strrep("b", 40L), "0.0.0", candidate_hash, candidate_library_hash,
  dependency_hash, as.character(getRversion()), "0",
  rr_sha256(resource_jobs_source),
  rr_sha256(file.path(verification_metadata, "resource-jobs-initial.tsv")),
  rr_sha256(runner_source), rr_sha256(worker_source),
  rr_sha256(wave_worker_source), rr_sha256(worker_group_source),
  rr_sha256(verifier_source)
)
run_fixture <- data.frame(
  field = names(run_values), value = unname(run_values),
  stringsAsFactors = FALSE
)
run_fixture_values <- rr_validate_reverse_run_metadata(run_fixture)
expect_error(
  rr_validate_reverse_run_metadata(run_fixture[-1L, , drop = FALSE]),
  "exact producer schema"
)
expect_error(
  rr_validate_reverse_run_metadata(rbind(
    run_fixture,
    data.frame(field = "unexpected", value = "value",
      stringsAsFactors = FALSE)
  )),
  "exact producer schema"
)
plan_run_values <- run_fixture_values
plan_run_values[["plan_only"]] <- "TRUE"
plan_run_values[["candidate_library_content_sha256"]] <- "not-computed"
plan_completion_fixture <- data.frame(
  field = rr_reverse_completion_fields(TRUE),
  value = c("planned", "1", "2026-07-15T00:00:01Z"),
  stringsAsFactors = FALSE
)
invisible(rr_validate_reverse_completion_metadata(
  plan_completion_fixture, plan_run_values
))
expect_error(
  rr_validate_reverse_completion_metadata(rbind(
    plan_completion_fixture,
    data.frame(field = "unexpected", value = "value",
      stringsAsFactors = FALSE)
  ), plan_run_values),
  "exact producer schema"
)
malformed_plan_completion <- plan_completion_fixture
malformed_plan_completion$value[[3L]] <- "2026-99-99T00:00:01Z"
expect_error(
  rr_validate_reverse_completion_metadata(
    malformed_plan_completion, plan_run_values
  ),
  "timestamp is malformed"
)
rr_write_tsv(run_fixture, file.path(verification_metadata, "run.tsv"))
plan_fixture <- data.frame(
  package = "fixture", source = "CRAN", relation = "imports", priority = "0",
  version = "1.0.0", repository = "https://example.invalid",
  archive = "/synthetic/fixture_1.0.0.tar.gz",
  archive_name = "fixture_1.0.0.tar.gz", checksum_type = "sha256",
  declared_checksum = archive_hash, archive_sha256 = archive_hash,
  archive_metadata_sha256 = strrep("7", 64L),
  notes = "synthetic", stringsAsFactors = FALSE
)
rr_write_tsv(plan_fixture, file.path(verification_stage, "plan.tsv"))
acceptance_fixture_path <- file.path(verification_metadata, "accepted.tsv")
waves_fixture_path <- file.path(verification_metadata, "waves.tsv")
wave_resource_fixture_directory <- file.path(
  verification_metadata, "resource-jobs-waves"
)
dir.create(wave_resource_fixture_directory)
rr_write_tsv(
  resource_report_fixture,
  file.path(wave_resource_fixture_directory, "attempt-000001.tsv")
)
rr_write_tsv(rr_empty_reverse_acceptance(), acceptance_fixture_path)
rr_write_tsv(rr_empty_reverse_waves(), waves_fixture_path)
rr_write_tsv(data.frame(
  field = c("status", "plan_sha256", "packages", "finished_utc"),
  value = c(
    "passed", rr_sha256(file.path(verification_stage, "plan.tsv")), "1",
    "2026-07-15T00:00:01Z"
  ), stringsAsFactors = FALSE
), file.path(verification_metadata, "source-archive-postflight.tsv"))
rr_write_tsv(data.frame(
  field = c("protected_library_metadata_sha256"), value = protected_hash,
  stringsAsFactors = FALSE
), file.path(verification_metadata, "protected-state.tsv"))
full_fixture <- data.frame(
  ordinal = c("1", "2"),
  context = c("stage_start", "stage_completion_postflight"),
  candidate_library_content_sha256 = rep(candidate_library_hash, 2L),
  dependency_library_content_sha256 = rep(dependency_hash, 2L),
  started_utc = rep("2026-07-15T00:00:00Z", 2L),
  finished_utc = rep("2026-07-15T00:00:01Z", 2L),
  stringsAsFactors = FALSE
)
rr_write_tsv(full_fixture,
  file.path(verification_metadata, "protected-library-full-hash-passes.tsv"))
result_fixture <- as.data.frame(as.list(setNames(
  rep("-", length(rr_reverse_result_columns())), rr_reverse_result_columns()
)), stringsAsFactors = FALSE)
result_fixture[1L, c(
  "package", "source", "relation", "priority", "version", "archive",
  "archive_sha256", "candidate_ref", "candidate_commit", "candidate_tree",
  "candidate_version", "candidate_content_sha256",
  "candidate_library_content_sha256", "dependency_library_content_sha256",
  "protected_metadata_before", "protected_metadata_after", "cache_key",
  "cache_reused", "install_elapsed_seconds", "check_elapsed_seconds",
  "install_timeout_seconds", "check_timeout_seconds", "exit_code", "timed_out",
  "status", "classification", "count_coverage", "count_files_total",
  "count_files_parsed", "not_cran", "log", "error"
)] <- list(
  "fixture", "CRAN", "imports", "0", "1.0.0", "fixture_1.0.0.tar.gz",
  archive_hash, "refs/synthetic", strrep("a", 40L), strrep("b", 40L),
  "0.0.0", candidate_hash, candidate_library_hash, dependency_hash,
  protected_hash, protected_hash, strrep("6", 64L), "false", "0.1", "0",
  "60", "60", "1", "false", "failed", "install_failure", "unavailable",
  "0", "0", "true", "packages/fixture/install.log", "synthetic failure"
)
result_fixture[] <- lapply(result_fixture, as.character)
rr_write_tsv(result_fixture, file.path(verification_row, "result.tsv"))
nested_fixture <- rr_reverse_nested_controls()
locale_fixture <- rr_consumer_locale_environment()
rr_write_tsv(data.frame(
  kind = c(
    rep("environment", length(locale_fixture) + length(nested_fixture)),
    "timeout_seconds"
  ),
  name = c(names(locale_fixture), names(nested_fixture), "timeout"),
  value = c(unname(locale_fixture), unname(nested_fixture), "60"),
  stringsAsFactors = FALSE
), file.path(verification_row, "install-command.tsv"))
worker_task_hash <- strrep("8", 64L)
wave_report_relative <- "metadata/resource-jobs-waves/attempt-000001.tsv"
wave_report_hash <- rr_sha256(file.path(
  verification_metadata, "resource-jobs-waves", "attempt-000001.tsv"
))
worker_task_path <- file.path(
  verification_row, "metadata", "worker-task.tsv"
)
rr_write_tsv(data.frame(
  schema = "1", plan_index = "1", package = "fixture", wave = "1",
  position = "1", worker_limit = "1", automatic_ceiling = "1",
  operator_limit = "-", resource_report = wave_report_relative,
  resource_report_sha256 = wave_report_hash,
  protected_metadata_before = protected_hash,
  started_utc = "2026-07-15T00:00:00Z", task_sha256 = worker_task_hash,
  worker_script_sha256 = rr_sha256(wave_worker_source),
  worker_group_sha256 = rr_sha256(worker_group_source),
  stringsAsFactors = FALSE
), worker_task_path)
worker_result_path <- file.path(
  verification_row, "metadata", "worker-result.rds"
)
worker_launch_path <- file.path(
  verification_row, "metadata", "worker-launch.tsv"
)
rr_write_tsv(data.frame(
  schema = "1", position = "1", task_sha256 = worker_task_hash,
  wrapper_pid = "12345", stringsAsFactors = FALSE
), worker_launch_path)
saveRDS(list(
  schema = 1L, position = 1L, task_sha256 = worker_task_hash,
  ok = TRUE, value = result_fixture, error = "-"
), worker_result_path, version = 3L)
worker_completion_path <- file.path(
  verification_row, "metadata", "worker-completion.tsv"
)
rr_write_tsv(data.frame(
  schema = "1", position = "1", task_sha256 = worker_task_hash,
  worker_exit_status = "0", stringsAsFactors = FALSE
), worker_completion_path)
writeLines(character(), file.path(verification_row, "worker-transport.log"))
rr_write_tsv(data.frame(
  field = c(
    "status", "classification", "plan_index", "wave",
    "worker_task_sha256", "worker_launch_sha256", "worker_result_sha256",
    "worker_completion_sha256", "finished_utc"
  ),
  value = c(
    "failed", "install_failure", "1", "1", rr_sha256(worker_task_path),
    rr_sha256(worker_launch_path), rr_sha256(worker_result_path),
    rr_sha256(worker_completion_path),
    "2026-07-15T00:00:02Z"
  ), stringsAsFactors = FALSE
), file.path(verification_row, "metadata", "completion.tsv"))
rr_append_reverse_wave(
  waves_fixture_path, 1L, plan_fixture$package, 1L, "external", integer(),
  integer(), integer(), 1L, NULL, wave_report_relative, wave_report_hash,
  protected_hash, protected_hash, "2026-07-15T00:00:00Z",
  "2026-07-15T00:00:01Z"
)
validated_waves_fixture <- rr_validate_reverse_waves(
  waves_fixture_path, plan_fixture$package
)
wave_report_files_fixture <- list.files(
  wave_resource_fixture_directory, all.files = TRUE, full.names = TRUE,
  no.. = TRUE
)
invisible(rr_validate_reverse_wave_report_inventory(
  wave_report_files_fixture, validated_waves_fixture
))
expect_error(
  rr_validate_reverse_wave_report_inventory(
    c(
      wave_report_files_fixture,
      file.path(wave_resource_fixture_directory, "attempt-000002.tsv")
    ),
    validated_waves_fixture
  ),
  "inventory is not exact"
)
verification_row_evidence <- rr_seal_stage(verification_row)
rr_append_reverse_acceptance(
  acceptance_fixture_path, 1L, "fixture", 1L, result_fixture,
  verification_row_evidence
)
rr_write_tsv(result_fixture, file.path(verification_stage, "results.tsv"))
results_hash <- rr_sha256(file.path(verification_stage, "results.tsv"))
completion_fixture <- data.frame(
  field = c(
    "status", "packages_planned", "packages_completed",
    "candidate_content_sha256", "candidate_library_content_sha256",
    "dependency_library_content_sha256", "results_sha256",
    "acceptance_ledger_sha256", "wave_ledger_sha256", "waves_completed",
    "protected_library_full_hash_passes", "finished_utc"
  ),
  value = c(
    "completed_with_failures", "1", "1", candidate_hash,
    candidate_library_hash, dependency_hash, results_hash,
    rr_sha256(acceptance_fixture_path), rr_sha256(waves_fixture_path), "1", "2",
    "2026-07-15T00:00:03Z"
  ), stringsAsFactors = FALSE
)
invisible(rr_validate_reverse_completion_metadata(
  completion_fixture, run_fixture_values
))
changed_completion_identity <- completion_fixture
changed_completion_identity$value[
  changed_completion_identity$field == "candidate_content_sha256"
] <- strrep("0", 64L)
expect_error(
  rr_validate_reverse_completion_metadata(
    changed_completion_identity, run_fixture_values
  ),
  "changed identity"
)
malformed_completion_time <- completion_fixture
malformed_completion_time$value[
  malformed_completion_time$field == "finished_utc"
] <- "2026-07-15T99:00:03Z"
expect_error(
  rr_validate_reverse_completion_metadata(
    malformed_completion_time, run_fixture_values
  ),
  "timestamp is malformed"
)
rr_write_tsv(completion_fixture,
  file.path(verification_metadata, "completion.tsv"))
invisible(rr_seal_composite_stage(verification_stage))
verification <- suppressWarnings(system2(
  file.path(R.home("bin"), "Rscript"),
  c("--vanilla", verifier_source, verification_stage, "--quiet"),
  stdout = TRUE, stderr = TRUE
))
verification_status <- as.integer(attr(verification, "status") %||% 0L)
if (!identical(verification_status, 0L)) {
  rr_fail("synthetic reverse evidence did not verify: ",
    paste(verification, collapse = "\n"))
}

# Only an incomplete current row is quarantined; a prior sealed row remains
# byte-identical and is therefore resumable without rerunning.
resume_root <- file.path(temporary, "resume")
sealed_row <- file.path(resume_root, "packages", "accepted")
current_row <- file.path(resume_root, "packages", "current")
dir.create(file.path(sealed_row, "metadata"), recursive = TRUE)
writeLines("accepted", file.path(sealed_row, "result.tsv"), useBytes = TRUE)
rr_write_tsv(data.frame(field = "status", value = "failed",
  stringsAsFactors = FALSE), file.path(sealed_row, "metadata", "completion.tsv"))
invisible(rr_seal_stage(sealed_row))
sealed_hash <- rr_sha256(file.path(sealed_row, rr_stage_manifest_relative))
dir.create(file.path(current_row, "metadata"), recursive = TRUE)
oversized_interrupted_log <- file.path(current_row, "check.log")
connection <- file(oversized_interrupted_log, open = "wb")
seek(connection, where = 9 * 1024^2 - 1L, origin = "start")
writeBin(as.raw(10L), connection)
close(connection)
interrupted <- file.path(resume_root, "interrupted")
dir.create(interrupted)
invisible(rr_quarantine_unsealed_row(current_row, interrupted, "current"))
if (dir.exists(current_row) || !dir.exists(file.path(interrupted,
    "current-attempt-1")) ||
    !file.exists(file.path(interrupted,
      "current-attempt-1", "1-check.log.tail")) ||
    !file.exists(file.path(interrupted,
      "current-attempt-1", "1-truncation.tsv")) ||
    !identical(sealed_hash,
      rr_sha256(file.path(sealed_row, rr_stage_manifest_relative)))) {
  rr_fail("resume quarantine changed an accepted prior row")
}

elapsed <- proc.time()[["elapsed"]] - started
cat("reverse_self_test=passed\n")
cat("synthetic_real_consumer_rows=0\n")
cat("protected_multigigabyte_hash_passes=0\n")
cat("fixture_full_hash_passes=2\n")
cat("fixture_full_hash_calls=", full_hash_calls, "\n", sep = "")
cat("tracked_full_hash_call_sites=2\n")
cat("synthetic_parallel_workers=2\n")
cat("synthetic_worker_failures_retained=1\n")
cat("synthetic_cache_race_promotions=1\n")
cat("elapsed_seconds=", sprintf("%.3f", elapsed), "\n", sep = "")
