arguments <- commandArgs(FALSE)
script_argument <- grep("^--file=", arguments, value = TRUE)
if (length(script_argument) != 1L) {
  stop("could not identify worker-diagnostics test location", call. = FALSE)
}
script <- normalizePath(
  substring(script_argument, nchar("--file=") + 1L),
  winslash = "/", mustWork = TRUE
)
worker <- file.path(dirname(dirname(script)), "worker.R")
environment <- new.env(parent = baseenv())
sys.source(worker, envir = environment)

validation_error <- list(validate = function(result) {
  stop("fixture failure", call. = FALSE)
})
observed <- tryCatch(
  environment$benchmark_validate_workload_result(
    validation_error, TRUE, "check_dt", "initial"
  ),
  error = conditionMessage
)
expected <- paste0(
  "Workload 'check_dt' semantic validation failed in phase 'initial': ",
  "fixture failure"
)
if (!identical(observed, expected)) {
  stop("worker validation error lost its workload or phase", call. = FALSE)
}

identity_validator <- list(validate = identity)
observed <- tryCatch(
  environment$benchmark_validate_workload_result(
    identity_validator, 2L, "qunif", "post-warmup", 1L
  ),
  error = conditionMessage
)
expected <- "Workload 'qunif' changed its validated result in phase 'post-warmup'."
if (!identical(observed, expected)) {
  stop("worker key mismatch lost its workload or phase", call. = FALSE)
}

key <- environment$benchmark_validate_workload_result(
  identity_validator, 1L, "qunif", "post-timing", 1L
)
if (!identical(key, 1L)) {
  stop("worker validation changed a successful key", call. = FALSE)
}

cat("PASS: worker validation diagnostics retain workload and phase\n")
