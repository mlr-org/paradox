#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
quiet <- FALSE
positionals <- character()
for (argument in args) {
  if (identical(argument, "--quiet")) {
    if (quiet) stop("--quiet may be supplied only once", call. = FALSE)
    quiet <- TRUE
  } else if (startsWith(argument, "--")) {
    stop("unknown option: ", argument, call. = FALSE)
  } else {
    positionals <- c(positionals, argument)
  }
}
if (length(positionals) != 1L) {
  stop("usage: verify-repository-evidence.R STAGE [--quiet]", call. = FALSE)
}

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("could not identify verifier location", call. = FALSE)
}
script <- normalizePath(
  substring(script_argument, nchar("--file=") + 1L),
  winslash = "/",
  mustWork = TRUE
)
sys.source(file.path(dirname(script), "repository-evidence.R"), envir = environment())

verified <- repository_verify_evidence(positionals[[1L]])
if (!quiet) {
  cat("evidence_stage=", verified$stage, "\n", sep = "")
  cat("evidence_manifest_sha256=", verified$manifest_sha256, "\n", sep = "")
  cat("evidence_files=", verified$files, "\n", sep = "")
}
