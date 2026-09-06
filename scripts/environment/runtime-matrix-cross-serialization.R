#!/usr/bin/env Rscript

# One semantic fixture shared by the R 4.0 producer and R 3.6 consumer.  The
# shell coordinator supplies and authenticates the exact runtime and installed
# candidate library; this script owns only construction and behavioral checks.

fail <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

assert_identical <- function(observed, expected, label) {
  if (!identical(observed, expected)) {
    fail(
      "%s differs:\nobserved: %s\nexpected: %s",
      label,
      paste(capture.output(str(observed)), collapse = "\n"),
      paste(capture.output(str(expected)), collapse = "\n")
    )
  }
  invisible(observed)
}

assert_true <- function(value, label) {
  if (!isTRUE(value)) {
    fail("%s is not TRUE", label)
  }
  invisible(value)
}

write_result <- function(path, fields) {
  if (!is.list(fields) || is.null(names(fields)) ||
      any(!nzchar(names(fields))) || anyDuplicated(names(fields))) {
    fail("internal result fields are malformed")
  }
  values <- vapply(fields, as.character, character(1L))
  if (anyNA(values) || any(grepl("[\t\r\n]", values))) {
    fail("internal result values are not one-line TSV fields")
  }
  writeLines(
    c(
      "field\tvalue",
      paste(names(values), values, sep = "\t")
    ),
    path,
    useBytes = TRUE
  )
}

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 6L) {
  fail(
    paste(
      "usage: runtime-matrix-cross-serialization.R",
      "MODE EXPECTED_RUNTIME EXPECTED_LIBRARY FIXTURE ROUNDTRIP RESULT"
    )
  )
}

mode <- arguments[[1L]]
expected_runtime <- arguments[[2L]]
expected_library <- arguments[[3L]]
fixture <- arguments[[4L]]
roundtrip <- arguments[[5L]]
result <- arguments[[6L]]
if (!mode %in% c("produce", "consume")) {
  fail("mode must be produce or consume")
}
if (!grepl("^[0-9]+[.][0-9]+[.][0-9]+$", expected_runtime)) {
  fail("expected runtime has an invalid shape")
}
for (path in c(expected_library, fixture, roundtrip, result)) {
  if (!grepl("^/", path) || grepl("[\t\r\n]", path)) {
    fail("library and artifact paths must be plain absolute paths")
  }
}

options(warn = 2L)
observed_runtime <- as.character(getRversion())
assert_identical(observed_runtime, expected_runtime, "runtime version")
expected_library <- normalizePath(
  expected_library, winslash = "/", mustWork = TRUE
)
suppressPackageStartupMessages(library(
  paradox, lib.loc = expected_library,
  character.only = FALSE
))
suppressPackageStartupMessages(library(R6))
package_path <- normalizePath(
  find.package("paradox", lib.loc = expected_library),
  winslash = "/", mustWork = TRUE
)
expected_package_path <- file.path(expected_library, "paradox")
assert_identical(package_path, expected_package_path, "loaded package path")
dlls <- getLoadedDLLs()
if (!"paradox" %in% names(dlls)) {
  fail("the paradox DLL is not loaded")
}
dll_path <- normalizePath(
  dlls[["paradox"]][["path"]], winslash = "/", mustWork = TRUE
)
expected_dll_path <- normalizePath(
  file.path(package_path, "libs", "paradox.so"),
  winslash = "/", mustWork = TRUE
)
assert_identical(dll_path, expected_dll_path, "loaded candidate DLL path")
package_version <- as.character(utils::packageVersion(
  "paradox", lib.loc = expected_library
))

expected_ids <- c(
  "nested.primary.enabled", "nested.primary.amount",
  "nested.primary.hidden", "nested.primary.payload",
  "nested.alias.enabled", "nested.alias.amount",
  "nested.alias.hidden", "nested.alias.payload",
  "direct.enabled", "direct.amount", "direct.hidden", "direct.payload"
)
raw_values <- list(
  enabled = FALSE, amount = 4L, hidden = 9L, payload = NULL
)
active_values <- list(enabled = FALSE, hidden = 9L, payload = NULL)

exercise_carrier <- function(carrier) {
  assert_true(
    inherits(carrier, "ParadoxCrossRuntimeCarrier"),
    "carrier class"
  )
  assert_identical(
    class(carrier$graph),
    c("ParamSetCollection", "ParamSet", "R6"),
    "outer collection class"
  )
  assert_identical(carrier$graph$ids(), expected_ids, "translated IDs")

  inner <- carrier$graph$sets[[1L]]
  direct <- carrier$graph$sets[[2L]]
  primary <- inner$sets[[1L]]
  alias <- inner$sets[[2L]]
  assert_true(identical(primary, alias), "shared inner child identity")
  assert_true(identical(primary, direct), "shared outer child identity")
  assert_true(identical(primary, carrier$alias), "carrier alias identity")
  assert_true(
    identical(carrier$view$origin, primary),
    "shadow origin identity"
  )
  core_types <- vapply(
    list(primary, inner, carrier$graph, carrier$view),
    function(node) typeof(node$.__enclos_env__$private$.core),
    character(1L)
  )
  assert_identical(
    core_types,
    rep("externalptr", 4L),
    "current-v2 native capsules"
  )
  hidden_reference <- attr(carrier, "paradox_cross_runtime_reference")
  assert_true(
    is.function(hidden_reference),
    "attribute-hidden reference closure"
  )
  assert_true(
    identical(hidden_reference(), primary),
    "attribute-hidden closure identity"
  )

  assert_identical(primary$values, raw_values, "raw dormant values")
  assert_identical(
    primary$get_values(remove_dependencies = TRUE),
    active_values,
    "dependency-filtered dormant values"
  )
  assert_identical(
    carrier$view$values,
    list(enabled = FALSE, amount = 4L, payload = NULL),
    "shadow raw values"
  )
  assert_identical(
    carrier$view$get_values(remove_dependencies = TRUE),
    list(enabled = FALSE, payload = NULL),
    "shadow dependency-filtered values"
  )
  assert_true(
    primary$test(list(
      enabled = TRUE, amount = 5L, hidden = 9L, payload = NULL
    )),
    "point validation"
  )
  assert_identical(
    primary$trafo(list(
      enabled = TRUE, amount = 5L, hidden = 9L, payload = NULL
    )),
    list(enabled = TRUE, amount = 6L, hidden = 9L, payload = NULL),
    "transformation"
  )

  summary <- carrier$summarize()
  assert_identical(summary$ids, expected_ids, "R6 method IDs")
  assert_true(summary$shared, "R6 method shared identity")
  assert_true(summary$closure_shared, "R6 method closure identity")
  invisible(primary)
}

if (identical(mode, "produce")) {
  if (file.exists(fixture) || file.exists(roundtrip) || file.exists(result)) {
    fail("producer outputs must be absent")
  }

  child <- ps(
    enabled = p_lgl(init = FALSE),
    amount = p_int(
      0L, 10L, depends = enabled == TRUE,
      trafo = function(value) value + 1L
    ),
    hidden = p_int(0L, 20L),
    payload = p_uty()
  )
  child$values <- raw_values
  inner <- ParamSetCollection$new(list(primary = child, alias = child))
  outer <- ParamSetCollection$new(list(nested = inner, direct = child))
  view <- ParamSetShadow$new(child, "hidden")

  Carrier <- R6::R6Class(
    "ParadoxCrossRuntimeCarrier",
    cloneable = FALSE,
    public = list(
      graph = NULL,
      alias = NULL,
      view = NULL,
      initialize = function(graph, alias, view) {
        self$graph <- graph
        self$alias <- alias
        self$view <- view
      },
      summarize = function() {
        inner <- self$graph$sets[[1L]]
        leaf <- inner$sets[[1L]]
        reference <- attr(self, "paradox_cross_runtime_reference")
        list(
          ids = self$graph$ids(),
          shared = identical(leaf, inner$sets[[2L]]) &&
            identical(leaf, self$graph$sets[[2L]]) &&
            identical(leaf, self$alias) &&
            identical(leaf, self$view$origin),
          closure_shared = is.function(reference) &&
            identical(reference(), leaf)
        )
      }
    )
  )
  carrier <- Carrier$new(outer, child, view)
  attr(carrier, "paradox_cross_runtime_reference") <- local({
    reference <- child
    function() reference
  })
  exercise_carrier(carrier)
  saveRDS(carrier, fixture, version = 3L)
  write_result(result, list(
    schema = 1L,
    mode = "producer",
    runtime = observed_runtime,
    package = "paradox",
    package_version = package_version,
    package_library = expected_library,
    package_path = package_path,
    dll_path = dll_path,
    serialization_version = 3L,
    fixture_status = "written",
    shared_leaf_edges = 3L,
    attribute_closure = "present",
    semantic_status = "passed"
  ))
  cat(
    "cross_runtime_serialization_producer=passed\n",
    sprintf("runtime=%s\n", observed_runtime),
    sep = ""
  )
} else {
  if (!file.exists(fixture) || file.exists(roundtrip) || file.exists(result)) {
    fail("consumer inputs or outputs have the wrong existence state")
  }
  carrier <- readRDS(fixture)
  primary <- exercise_carrier(carrier)

  primary$values <- list(
    enabled = TRUE, amount = 6L, hidden = 9L, payload = NULL
  )
  assert_identical(
    carrier$view$values,
    list(enabled = TRUE, amount = 6L, payload = NULL),
    "post-load shadow mutation"
  )
  assert_identical(
    carrier$graph$sets[[1L]]$sets[[2L]]$values,
    list(enabled = TRUE, amount = 6L, hidden = 9L, payload = NULL),
    "post-load shared collection mutation"
  )

  saveRDS(carrier, roundtrip, version = 3L)
  reloaded <- readRDS(roundtrip)
  reloaded_primary <- reloaded$graph$sets[[1L]]$sets[[1L]]
  assert_true(
    identical(reloaded_primary, reloaded$graph$sets[[1L]]$sets[[2L]]) &&
      identical(reloaded_primary, reloaded$graph$sets[[2L]]) &&
      identical(reloaded_primary, reloaded$alias) &&
      identical(reloaded_primary, reloaded$view$origin) &&
      identical(
        attr(reloaded, "paradox_cross_runtime_reference")(),
        reloaded_primary
      ),
    "R 3.6 round-trip graph identity"
  )
  assert_identical(
    reloaded_primary$values,
    list(enabled = TRUE, amount = 6L, hidden = 9L, payload = NULL),
    "R 3.6 round-trip mutation"
  )
  assert_identical(
    reloaded$summarize()$ids,
    expected_ids,
    "R 3.6 round-trip R6 method"
  )

  write_result(result, list(
    schema = 1L,
    mode = "consumer",
    runtime = observed_runtime,
    package = "paradox",
    package_version = package_version,
    package_library = expected_library,
    package_path = package_path,
    dll_path = dll_path,
    serialization_version = 3L,
    fixture_status = "loaded",
    mutation_status = "passed",
    roundtrip_status = "passed",
    shared_leaf_edges = 3L,
    attribute_closure = "present",
    semantic_status = "passed"
  ))
  cat(
    "cross_runtime_serialization_consumer=passed\n",
    sprintf("runtime=%s\n", observed_runtime),
    sep = ""
  )
}
