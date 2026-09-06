#!/usr/bin/env Rscript

# Bind a maintained documentation consumer's exact source evidence to the
# nested `$values` assignment contract that it demonstrates.

fail <- function(...) stop(..., call. = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  fail("usage: mlr3benchmark-paramset.R SOURCE_DIRECTORY")
}
source_directory <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source_path <- file.path(source_directory, "R", "autoplot.BenchmarkAggr.R")
if (!file.exists(source_path) || dir.exists(source_path) ||
    nzchar(Sys.readlink(source_path))) {
  fail("mlr3benchmark source contract is missing or symbolic")
}
source_lines <- readLines(source_path, warn = FALSE)
contract <- "learns$classif.xgboost$param_set$values$nrounds = 50"
if (!any(grepl(contract, source_lines, fixed = TRUE))) {
  fail("the pinned mlr3benchmark nested ParamSet assignment is absent")
}

suppressPackageStartupMessages({
  library(paradox)
  library(mlr3)
})
if (!identical(as.character(utils::packageVersion("paradox")), "2.0.0")) {
  fail("the workload did not load the paradox 2.0.0 candidate")
}

learner <- lrn("classif.rpart")
learner$param_set$values$cp <- 0.05
if (!identical(learner$param_set$values$cp, 0.05)) {
  fail("nested ParamSet value assignment did not persist")
}
learner$param_set$set_values(.values = list(xval = 0L))
observed <- learner$param_set$get_values()
if (!identical(observed[c("cp", "xval")], list(cp = 0.05, xval = 0L))) {
  fail("ParamSet$set_values(.values=) did not preserve the documented state")
}

cat(
  "source_sha256\t", unname(tools::sha256sum(source_path)), "\n",
  "cp\t", observed$cp, "\n",
  "xval\t", observed$xval, "\n",
  sep = ""
)
