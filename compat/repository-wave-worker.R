#!/usr/bin/env Rscript

# One independently started repository-row transport.  The scheduler owns all
# shared protection checks, result interpretation, attempt sealing, and
# promotion; this child can only populate its reserved attempt directory.

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop("repository wave worker requires SPEC and RESULT", call. = FALSE)
}
spec_path <- normalizePath(arguments[[1L]], winslash = "/", mustWork = TRUE)
result_path <- arguments[[2L]]
spec <- readRDS(spec_path)
if (!is.list(spec) || !identical(names(spec), c(
    "runner", "context", "row", "prepared", "fixture"))) {
  stop("repository wave worker specification is malformed", call. = FALSE)
}
sys.source(spec$runner, envir = environment(), keep.source = FALSE)

fixture <- spec$fixture
if (!is.null(fixture)) {
  if (!startsWith(spec$context$config$run_id, "synthetic-") ||
      !is.list(fixture) || !identical(names(fixture),
        c("delays", "fail", "timing_directory"))) {
    stop("repository worker fixture is forbidden outside synthetic evidence",
      call. = FALSE)
  }
  repository <- spec$row$repository[[1L]]
  delay <- fixture$delays[[repository]]
  if (is.null(delay)) delay <- 0
  if (length(delay) != 1L || !is.numeric(delay) || is.na(delay) ||
      delay < 0 || delay > 10) {
    stop("synthetic repository worker delay is malformed", call. = FALSE)
  }
  timing <- repository_runner_require_directory(fixture$timing_directory,
    "synthetic worker timing directory")
  writeLines(format(as.numeric(Sys.time()), digits = 17L), file.path(timing,
    paste0(repository, "-start")), useBytes = TRUE)
  if (delay > 0) Sys.sleep(delay)
  if (repository %in% fixture$fail) {
    result <- list(ok = FALSE, value = NULL, error = "injected worker failure")
  } else {
    result <- repository_runner_execute_worker(spec$context, spec$row,
      spec$prepared, repository_runner_execute_attempt)
  }
} else {
  result <- repository_runner_execute_worker(spec$context, spec$row,
    spec$prepared, repository_runner_execute_attempt)
}
if (!is.null(fixture)) {
  writeLines(format(as.numeric(Sys.time()), digits = 17L), file.path(timing,
    paste0(repository, "-end")), useBytes = TRUE)
}

temporary <- paste0(result_path, ".new")
if (file.exists(result_path) || file.exists(temporary)) {
  stop("repository wave worker refuses to overwrite its result", call. = FALSE)
}
saveRDS(result, temporary, version = 3L)
if (!file.rename(temporary, result_path)) {
  stop("repository wave worker could not publish its result", call. = FALSE)
}
