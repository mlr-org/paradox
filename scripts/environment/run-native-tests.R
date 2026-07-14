#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "usage: run-native-tests.R SNAPSHOT INSTALLED_LIBRARY none|focused|full",
    call. = FALSE
  )
}

snapshot <- normalizePath(args[[1L]], mustWork = TRUE)
library_path <- normalizePath(args[[2L]], mustWork = TRUE)
selection <- match.arg(args[[3L]], c("none", "focused", "full"))

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

if (selection == "none") {
  library("paradox", character.only = TRUE, lib.loc = library_path)
  quit(save = "no", status = 0L)
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat is not installed in a repository-local library", call. = FALSE)
}

Sys.setenv(NOT_CRAN = "true")
options(
  warn = 2L,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

library("paradox", character.only = TRUE, lib.loc = library_path)
filter <- if (selection == "focused") {
  "(characterization|native|regression)"
} else {
  NULL
}

testthat::test_dir(
  file.path(snapshot, "tests", "testthat"),
  filter = filter,
  reporter = "summary",
  load_helpers = TRUE,
  stop_on_failure = TRUE,
  stop_on_warning = TRUE,
  package = "paradox",
  load_package = "none"
)
