#!/usr/bin/env Rscript

runtime_matrix_skip_policy_is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) & nzchar(target)
}

runtime_matrix_skip_policy_children <- function(value) {
  output <- list()
  for (index in seq_along(value)) {
    if (!rlang::is_missing(value[[index]])) {
      output[[length(output) + 1L]] <- value[[index]]
    }
  }
  output
}

runtime_matrix_contains_named_call <- function(value, target) {
  if (!is.call(value) && !is.expression(value)) return(FALSE)
  if (is.call(value) && is.name(value[[1L]]) &&
      identical(as.character(value[[1L]]), target)) {
    return(TRUE)
  }
  any(vapply(
    runtime_matrix_skip_policy_children(value),
    runtime_matrix_contains_named_call,
    logical(1L),
    target = target
  ))
}

runtime_matrix_not_cran_test_titles <- function(path) {
  titles <- character()
  walk <- function(value) {
    if (is.call(value) && is.name(value[[1L]]) &&
        identical(as.character(value[[1L]]), "test_that") &&
        runtime_matrix_contains_named_call(value, "skip_on_cran")) {
      title <- value[[2L]]
      if (!is.character(title) || length(title) != 1L || is.na(title) ||
          !nzchar(title)) {
        stop("skip_on_cran test has a non-literal title", call. = FALSE)
      }
      titles <<- c(titles, title)
      return(invisible(NULL))
    }
    if (is.call(value) || is.expression(value)) {
      for (child in runtime_matrix_skip_policy_children(value)) walk(child)
    }
    invisible(NULL)
  }
  walk(parse(path, keep.source = FALSE))
  titles
}

runtime_matrix_validate_pre46_exclusion_policy <- function(
    snapshot, test_files = NULL, test_contexts = NULL) {
  snapshot <- normalizePath(snapshot, mustWork = TRUE)
  test_directory <- file.path(snapshot, "tests", "testthat")
  if (is.null(test_files)) {
    test_files <- dir(
      test_directory,
      pattern = "^test.*\\.[rR]$",
      full.names = FALSE
    )
  }
  if (is.null(test_contexts)) {
    test_contexts <- sub(
      "[.][Rr]$", "", sub("^test[-_]", "", test_files)
    )
  }
  test_paths <- file.path(test_directory, test_files)
  valid_tests <- length(test_files) > 0L &&
    length(test_contexts) == length(test_files) &&
    all(file.exists(test_paths)) && !any(dir.exists(test_paths)) &&
    !any(runtime_matrix_skip_policy_is_symbolic(test_paths)) &&
    !anyDuplicated(test_files) && !anyDuplicated(test_contexts)
  if (!valid_tests) {
    stop("testthat source discovery is empty, symbolic, or ambiguous",
      call. = FALSE)
  }

  manifest_path <- file.path(
    snapshot, "environment", "runtime-matrix-pre46-exclusions.tsv"
  )
  if (!file.exists(manifest_path) || dir.exists(manifest_path) ||
      runtime_matrix_skip_policy_is_symbolic(manifest_path)) {
    stop("pre-R-4.6 exclusion manifest is absent or symbolic", call. = FALSE)
  }
  exclusions <- read.delim(
    manifest_path,
    header = TRUE,
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE
  )
  if (!identical(names(exclusions), c("context", "reason")) ||
      nrow(exclusions) != 0L || anyNA(exclusions) ||
      any(!nzchar(exclusions$reason)) ||
      any(grepl("[\t\r\n]", exclusions$reason)) ||
      any(!grepl("^native-[a-z0-9][a-z0-9-]*$", exclusions$context)) ||
      anyDuplicated(exclusions$context)) {
    stop("pre-R-4.6 exclusion manifest is malformed", call. = FALSE)
  }

  # The reviewed Paradox-2 policy is deliberately header-only. Do not derive
  # candidate file names with paste0() here: on supported R releases,
  # paste0("test-", character(), ".R") is "test-.R" and makes the valid empty
  # policy look stale.
  exclusions
}

runtime_matrix_validate_result_skip_policy <- function(
    snapshot, test_files = NULL, test_paths = NULL) {
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("the exact runtime lock does not provide rlang", call. = FALSE)
  }
  snapshot <- normalizePath(snapshot, mustWork = TRUE)
  test_directory <- file.path(snapshot, "tests", "testthat")
  if (is.null(test_files)) {
    test_files <- dir(
      test_directory,
      pattern = "^test.*\\.[rR]$",
      full.names = FALSE
    )
  }
  if (is.null(test_paths)) {
    test_paths <- file.path(test_directory, test_files)
  }
  valid_tests <- length(test_files) > 0L &&
    length(test_paths) == length(test_files) &&
    all(file.exists(test_paths)) && !any(dir.exists(test_paths)) &&
    !any(runtime_matrix_skip_policy_is_symbolic(test_paths)) &&
    !anyDuplicated(test_files)
  if (!valid_tests) {
    stop("testthat source discovery is empty, symbolic, or ambiguous",
      call. = FALSE)
  }

  manifest_path <- file.path(
    snapshot, "environment", "runtime-matrix-result-skips.tsv"
  )
  if (!file.exists(manifest_path) || dir.exists(manifest_path) ||
      runtime_matrix_skip_policy_is_symbolic(manifest_path)) {
    stop("result-skip manifest is absent or symbolic", call. = FALSE)
  }
  policy <- read.delim(
    manifest_path,
    header = TRUE,
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE
  )
  policy_order <- if (nrow(policy) > 0L) {
    do.call(order, c(
      policy[c("runtime", "file", "test", "reason")],
      list(method = "radix")
    ))
  } else {
    integer()
  }
  reviewed_runtimes <- c("4.3.3", "4.5.2")
  if (!identical(names(policy), c("runtime", "file", "test", "reason")) ||
      anyNA(policy) || any(!nzchar(policy$test)) ||
      any(!nzchar(policy$reason)) ||
      any(!policy$runtime %in% reviewed_runtimes) ||
      any(!grepl("^test[-_][A-Za-z0-9_-]+[.]R$", policy$file)) ||
      any(!policy$file %in% test_files) ||
      any(grepl("[\t\r\n]", policy$test)) ||
      any(grepl("[\t\r\n]", policy$reason)) ||
      anyDuplicated(policy[c("runtime", "file", "test")]) ||
      !identical(policy_order, seq_len(nrow(policy)))) {
    stop("result-skip manifest is malformed", call. = FALSE)
  }

  source_rows <- lapply(seq_along(test_paths), function(index) {
    titles <- runtime_matrix_not_cran_test_titles(test_paths[[index]])
    if (!length(titles)) return(NULL)
    data.frame(
      file = rep(test_files[[index]], length(titles)),
      test = titles,
      stringsAsFactors = FALSE
    )
  })
  source_rows <- do.call(rbind, source_rows)
  if (is.null(source_rows)) {
    source_rows <- data.frame(
      file = character(), test = character(), stringsAsFactors = FALSE
    )
  }
  row.names(source_rows) <- NULL
  if (anyDuplicated(source_rows)) {
    stop("source contains duplicate skip_on_cran test titles", call. = FALSE)
  }
  expected <- do.call(rbind, lapply(reviewed_runtimes, function(runtime) {
    data.frame(
      runtime = rep(runtime, nrow(source_rows)),
      file = source_rows$file,
      test = source_rows$test,
      reason = rep("Reason: On CRAN", nrow(source_rows)),
      stringsAsFactors = FALSE
    )
  }))
  expected <- expected[do.call(order, c(
    expected[c("runtime", "file", "test", "reason")],
    list(method = "radix")
  )), , drop = FALSE]
  row.names(expected) <- NULL
  if (!identical(policy, expected)) {
    stop(
      "result-skip manifest differs from current skip_on_cran test titles",
      call. = FALSE
    )
  }
  policy
}

runtime_matrix_skip_policy_main <- function(args = commandArgs(TRUE)) {
  if (length(args) != 1L) {
    stop(
      "usage: runtime-matrix-skip-policy.R SNAPSHOT",
      call. = FALSE
    )
  }
  exclusions <- runtime_matrix_validate_pre46_exclusion_policy(args[[1L]])
  policy <- runtime_matrix_validate_result_skip_policy(args[[1L]])
  cat("runtime_matrix_skip_policy_preflight=passed\n")
  cat("reviewed_pre46_exclusion_row_count=", nrow(exclusions), "\n", sep = "")
  cat("reviewed_result_skip_row_count=", nrow(policy), "\n", sep = "")
}

if (sys.nframe() == 0L) {
  runtime_matrix_skip_policy_main()
}
