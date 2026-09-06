#!/usr/bin/env Rscript

# One independently supervised reverse-dependency row.  The parent owns every
# protected-input boundary, resource decision, row seal, and acceptance ledger.
# This child only reconstructs the bounded serialized worker closure, executes
# its reserved task, and atomically publishes a durable transport result.

arguments <- commandArgs(trailingOnly = TRUE)
is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}
if (length(arguments) != 1L) {
  stop("reverse wave worker requires one SPEC path", call. = FALSE)
}
spec_path <- arguments[[1L]]
if (!file.exists(spec_path) || dir.exists(spec_path) ||
    is_symbolic(spec_path)) {
  stop("reverse wave worker specification is absent or symbolic", call. = FALSE)
}
spec_path <- normalizePath(spec_path, winslash = "/", mustWork = TRUE)
spec <- readRDS(spec_path)
if (!is.list(spec) || !identical(names(spec), c(
    "schema", "position", "task", "task_sha256", "bundle",
    "transport_root", "result")) || !identical(spec$schema, 1L) ||
    length(spec$position) != 1L || is.na(spec$position) ||
    spec$position < 1L || spec$position != as.integer(spec$position) ||
    !grepl("^[0-9a-f]{64}$", spec$task_sha256) ||
    !is.list(spec$bundle) ||
    !identical(names(spec$bundle), c("worker", "globals")) ||
    !is.function(spec$bundle$worker) || !is.list(spec$bundle$globals) ||
    (length(spec$bundle$globals) &&
      (is.null(names(spec$bundle$globals)) ||
        anyNA(names(spec$bundle$globals)) ||
        any(!nzchar(names(spec$bundle$globals))) ||
        anyDuplicated(names(spec$bundle$globals))))) {
  stop("reverse wave worker specification is malformed", call. = FALSE)
}
root <- normalizePath(spec$transport_root, winslash = "/", mustWork = TRUE)
result_parent <- normalizePath(
  dirname(spec$result), winslash = "/", mustWork = TRUE
)
if (is_symbolic(root) || is_symbolic(result_parent) ||
    !(identical(result_parent, root) ||
      startsWith(result_parent, paste0(root, "/"))) ||
    file.exists(spec$result) || dir.exists(spec$result) ||
    is_symbolic(spec$result)) {
  stop("reverse wave worker result escaped or already exists", call. = FALSE)
}

temporary_hash <- tempfile("paradox-reverse-task-")
on.exit(unlink(temporary_hash), add = TRUE)
writeBin(
  serialize(spec$task, NULL, version = 3L, xdr = TRUE), temporary_hash
)
observed_task_sha256 <- unname(tools::sha256sum(temporary_hash))
if (!identical(observed_task_sha256, spec$task_sha256)) {
  stop("reverse wave worker task identity changed in transport", call. = FALSE)
}

invisible(list2env(spec$bundle$globals, envir = .GlobalEnv))
worker_result <- tryCatch(
  list(ok = TRUE, value = spec$bundle$worker(spec$task), error = "-"),
  error = function(condition) list(
    ok = FALSE, value = NULL,
    error = substr(gsub("[\r\n\t]+", " ", conditionMessage(condition)),
      1L, 8000L)
  )
)
result <- list(
  schema = 1L,
  position = as.integer(spec$position),
  task_sha256 = spec$task_sha256,
  ok = isTRUE(worker_result$ok),
  value = worker_result$value,
  error = worker_result$error
)
temporary <- paste0(spec$result, ".new-", Sys.getpid())
if (file.exists(temporary) || dir.exists(temporary) ||
    is_symbolic(temporary)) {
  stop("reverse wave worker temporary result exists", call. = FALSE)
}
on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
saveRDS(result, temporary, version = 3L)
if (!file.rename(temporary, spec$result)) {
  stop("reverse wave worker could not publish its result", call. = FALSE)
}
