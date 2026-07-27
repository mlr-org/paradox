#!/usr/bin/env Rscript

arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) {
  stop("usage: core-graph-roots.R CANDIDATE_LIBRARY", call. = FALSE)
}
candidate_library = normalizePath(arguments[[1L]], mustWork = TRUE)
.libPaths(unique(c(candidate_library, .libPaths())))
library("paradox", character.only = TRUE, lib.loc = candidate_library)

namespace = asNamespace("paradox")
core_graph_counts_symbol = get0(
  "C_test_core_graph_root_barrier_counts",
  envir = namespace,
  inherits = FALSE
)
if (is.null(core_graph_counts_symbol)) {
  stop(
    "candidate was not built with PARADOX_TEST_CORE_GRAPH_ROOTS",
    call. = FALSE
  )
}
check = function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

deep_graph = ps(x = p_int())
for (index in seq_len(20L)) {
  deep_graph = ParamSetCollection$new(list(node = deep_graph))
}
invisible(.Call(core_graph_counts_symbol, TRUE))
deep_shadow = ParamSetShadow$new(deep_graph, character())
core_graph_count = .Call(core_graph_counts_symbol, FALSE)
check(
  identical(core_graph_count, 21L),
  sprintf(
    "unexpected core-graph barrier count: %s",
    core_graph_count
  )
)
check(
  identical(deep_shadow$origin, deep_graph),
  "instrumented deep Shadow lost its origin"
)

cat("core_graph_barriers=", core_graph_count, "\n", sep = "")
cat("core_graph_root_regression=passed\n")
