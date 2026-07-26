runtime_matrix_audit_testthat_results <- function(results, summary) {
  if (!is.data.frame(summary) ||
      !all(c("file", "test") %in% names(summary))) {
    stop("pinned testthat returned a malformed tabular result structure",
      call. = FALSE)
  }
  raw_blocks <- unclass(results)
  if (!is.list(raw_blocks) || length(raw_blocks) != nrow(summary) ||
      any(!vapply(raw_blocks, is.list, logical(1L))) ||
      any(!vapply(
        raw_blocks,
        function(block) {
          identical(sort(names(block)), sort(c(
            "file", "context", "test", "user", "system", "real", "results"
          ))) && is.list(block$results)
        },
        logical(1L)
      ))) {
    stop("pinned testthat returned a malformed raw result structure",
      call. = FALSE)
  }
  raw_files <- vapply(raw_blocks, function(block) {
    if (length(block$file) != 1L) NA_character_ else as.character(block$file)
  }, character(1L))
  raw_tests <- vapply(raw_blocks, function(block) {
    if (length(block$test) != 1L) NA_character_ else as.character(block$test)
  }, character(1L))
  if (!identical(raw_files, as.character(summary$file)) ||
      !identical(raw_tests, as.character(summary$test))) {
    stop("pinned testthat raw and tabular result identities disagree",
      call. = FALSE)
  }
  raw_results <- lapply(raw_blocks, `[[`, "results")
  expectations <- unlist(raw_results, recursive = FALSE, use.names = FALSE)
  expectation_classes <- c(
    success = "expectation_success",
    failure = "expectation_failure",
    error = "expectation_error",
    skip = "expectation_skip",
    warning = "expectation_warning"
  )
  expectation_type <- function(expectation) {
    if (!inherits(expectation, "expectation") ||
        !inherits(expectation, "condition")) {
      stop("pinned testthat returned a non-expectation result", call. = FALSE)
    }
    matches <- names(expectation_classes)[vapply(
      expectation_classes,
      function(class) inherits(expectation, class),
      logical(1L)
    )]
    if (length(matches) != 1L) {
      stop("pinned testthat returned an ambiguous expectation class",
        call. = FALSE)
    }
    matches[[1L]]
  }
  list(
    raw_blocks = raw_blocks,
    raw_results = raw_results,
    expectation_types = vapply(
      expectations, expectation_type, character(1L)
    )
  )
}
