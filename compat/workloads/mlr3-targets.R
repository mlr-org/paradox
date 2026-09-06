#!/usr/bin/env Rscript

# Authenticate the legacy mlr3-targets surface and exercise its supported
# modern equivalent. The 2022 prototype predates p_*()/ps() and is retained as
# explicit migration evidence rather than pretending its removed Param* names
# are still an executable interface.

fail <- function(...) stop(..., call. = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  fail("usage: mlr3-targets.R SOURCE_DIRECTORY")
}
source_directory <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source_path <- file.path(source_directory, "plans", "01-param-sets.R")
if (!file.exists(source_path) || dir.exists(source_path) ||
    nzchar(Sys.readlink(source_path))) {
  fail("mlr3-targets legacy parameter-space source is missing or symbolic")
}
source_text <- paste(readLines(source_path, warn = FALSE), collapse = "\n")
legacy_tokens <- c(
  "ParamSet$new(params = list(", "ParamFct$new(", "ParamDbl$new(",
  "ParamInt$new(", "foo$trafo <- function(x, param_set)"
)
if (any(!vapply(legacy_tokens, grepl, logical(1L), x = source_text, fixed = TRUE))) {
  fail("the pinned mlr3-targets legacy constructor surface changed")
}

suppressPackageStartupMessages(library(paradox))
if (!identical(as.character(utils::packageVersion("paradox")), "2.0.0")) {
  fail("the workload did not load the paradox 2.0.0 candidate")
}

spaces <- list(
  svm_linear = ps(
    type = p_fct("C-classification"),
    kernel = p_fct("radial"),
    cost = p_dbl(0, 100),
    gamma = p_dbl(0, 100)
  ),
  svm_log = ps(
    type = p_fct("C-classification"),
    kernel = p_fct("radial"),
    cost = p_dbl(-5, 5),
    gamma = p_dbl(-5, 3)
  ),
  knn = ps(k = p_int(1, 50), distance = p_dbl(1, 50)),
  knn_small = ps(k = p_int(1, 30))
)
spaces$svm_log$extra_trafo <- function(x, param_set) {
  x$cost <- 2^x$cost
  x$gamma <- 2^x$gamma
  x
}

set.seed(20260714L)
for (name in names(spaces)) {
  space <- spaces[[name]]
  design <- generate_design_random(space, 32L)
  if (!inherits(space, "ParamSet") || !inherits(design, "Design") ||
      nrow(design$data) != 32L || !identical(names(design$data), space$ids())) {
    fail("modernized mlr3-targets space is malformed: ", name)
  }
  values <- design$transpose()
  transformed <- lapply(values, space$trafo)
  if (length(transformed) != 32L ||
      any(!vapply(transformed, is.list, logical(1L)))) {
    fail("modernized mlr3-targets transformation failed: ", name)
  }
}
if (any(vapply(spaces$svm_log$trafo(list(cost = 0, gamma = -1))[
    c("cost", "gamma")
  ], function(value) !is.numeric(value) || value <= 0, logical(1L)))) {
  fail("modernized global transformation did not preserve legacy semantics")
}

cat(
  "source_sha256\t", unname(tools::sha256sum(source_path)), "\n",
  "legacy_constructor_names\t", length(legacy_tokens), "\n",
  "modernized_spaces\t", length(spaces), "\n",
  sep = ""
)
