#!/usr/bin/env Rscript

arguments = commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) {
  stop("usage: gc-row-names-roots.R CANDIDATE_LIBRARY", call. = FALSE)
}
candidate_library = normalizePath(arguments[[1L]], mustWork = TRUE)
.libPaths(unique(c(candidate_library, .libPaths())))
library("paradox", character.only = TRUE, lib.loc = candidate_library)

namespace = asNamespace("paradox")
counts_symbol = get0(
  "C_test_gc_row_names_barrier_counts",
  envir = namespace,
  inherits = FALSE
)
if (is.null(counts_symbol)) {
  stop(
    "candidate was not built with PARADOX_TEST_GC_ROW_NAMES_ROOTS",
    call. = FALSE
  )
}
store_symbol = get(
  "C_param_set_store_values",
  envir = namespace,
  inherits = FALSE
)
collection_symbol = get(
  "C_param_set_collection_values",
  envir = namespace,
  inherits = FALSE
)

expected_zero = c(
  store_local = 0L,
  collection_local = 0L,
  collection_carrier = 0L
)
read_counts = function(reset = FALSE) {
  observed = .Call(counts_symbol, reset)
  if (!identical(names(observed), names(expected_zero)) ||
      typeof(observed) != "integer") {
    stop("invalid instrumented barrier counter result", call. = FALSE)
  }
  observed
}
check = function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

param_set = ps(a = p_int(), b = p_dbl())
param_set$values = list(a = 1L, b = 0.25)
collection = ParamSetCollection$new(list(left = param_set))
private = param_set$.__enclos_env__$private
collection_private = collection$.__enclos_env__$private
check(
  identical(
    .row_names_info(private$.params, type = 0L),
    c(NA_integer_, -2L)
  ),
  "ParamSet fixture does not have compact row names"
)
check(
  identical(
    .row_names_info(collection_private$.params, type = 0L),
    c(NA_integer_, -2L)
  ),
  "ParamSetCollection fixture does not have compact row names"
)

invisible(read_counts(reset = TRUE))
stored = .Call(
  store_symbol,
  private,
  param_set,
  list(a = 2L, b = 0.5)
)
check(
  identical(stored, list(a = 2L, b = 0.5)),
  "instrumented ParamSet value store returned the wrong value"
)
store_counts = read_counts(reset = TRUE)
expected_store = c(
  store_local = 2L,
  collection_local = 0L,
  collection_carrier = 0L
)
check(
  identical(store_counts, expected_store),
  sprintf(
    "unexpected store barrier counts: %s",
    paste(store_counts, collapse = ",")
  )
)

observed = .Call(collection_symbol, collection_private, collection)
check(
  identical(observed, list(left.a = 2L, left.b = 0.5)),
  "instrumented ParamSetCollection value aggregation returned the wrong value"
)
collection_counts = read_counts(reset = FALSE)
expected_collection = c(
  store_local = 0L,
  collection_local = 2L,
  collection_carrier = 2L
)
check(
  identical(collection_counts, expected_collection),
  sprintf(
    "unexpected collection barrier counts: %s",
    paste(collection_counts, collapse = ",")
  )
)

cat("store_local=", store_counts[["store_local"]], "\n", sep = "")
cat(
  "collection_local=",
  collection_counts[["collection_local"]],
  "\n",
  sep = ""
)
cat(
  "collection_carrier=",
  collection_counts[["collection_carrier"]],
  "\n",
  sep = ""
)
cat("gc_row_names_root_regression=passed\n")
