#!/usr/bin/env Rscript

runtime_matrix_old_r_stress_is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

runtime_matrix_old_r_stress_children <- function(value) {
  output <- list()
  for (index in seq_along(value)) {
    if (!identical(value[[index]], quote(expr = ))) {
      output[[length(output) + 1L]] <- value[[index]]
    }
  }
  output
}

runtime_matrix_old_r_stress_contains_call <- function(value, target) {
  if (!is.call(value) && !is.expression(value)) return(FALSE)
  if (is.call(value) && is.name(value[[1L]]) &&
      identical(as.character(value[[1L]]), target)) {
    return(TRUE)
  }
  any(vapply(
    runtime_matrix_old_r_stress_children(value),
    runtime_matrix_old_r_stress_contains_call,
    logical(1L),
    target = target
  ))
}

runtime_matrix_old_r_stress_test_title <- function(expression) {
  if (!is.call(expression) || !is.name(expression[[1L]]) ||
      !identical(as.character(expression[[1L]]), "test_that") ||
      length(expression) < 3L || !is.character(expression[[2L]]) ||
      length(expression[[2L]]) != 1L || is.na(expression[[2L]]) ||
      !nzchar(expression[[2L]])) {
    return(NA_character_)
  }
  expression[[2L]]
}

runtime_matrix_old_r_stress_collect_test_titles <- function(value) {
  if (!is.call(value) && !is.expression(value)) return(character())
  output <- character()
  if (is.call(value)) {
    head <- value[[1L]]
    if (is.name(head) && identical(as.character(head), "test_that")) {
      output <- runtime_matrix_old_r_stress_test_title(value)
    } else if (is.call(head) && length(head) == 3L &&
        is.name(head[[1L]]) &&
        as.character(head[[1L]]) %in% c("::", ":::") &&
        is.name(head[[2L]]) &&
        identical(as.character(head[[2L]]), "testthat") &&
        is.name(head[[3L]]) &&
        identical(as.character(head[[3L]]), "test_that")) {
      # A qualified call would bypass the injected unqualified selector.
      output <- NA_character_
    }
  }
  c(
    output,
    unlist(lapply(
      runtime_matrix_old_r_stress_children(value),
      runtime_matrix_old_r_stress_collect_test_titles
    ), use.names = FALSE)
  )
}

runtime_matrix_old_r_stress_has_safe_selector_references <- function(
    value, direct_call_head = FALSE) {
  if (is.name(value)) {
    return(
      !identical(as.character(value), "test_that") ||
        isTRUE(direct_call_head)
    )
  }
  # Literal lookup/mutation such as get("test_that") or
  # assign("test_that", ...) could bypass the injected selector just as surely
  # as a direct assignment. Test titles and ordinary selected test bodies have
  # no reason to carry this exact implementation name as data.
  if (is.character(value)) {
    return(!any(!is.na(value) & value == "test_that"))
  }
  if (!is.call(value) && !is.expression(value)) return(TRUE)
  for (index in seq_along(value)) {
    if (identical(value[[index]], quote(expr = ))) next
    child <- value[[index]]
    allowed_head <- is.call(value) && index == 1L &&
      is.name(child) && identical(as.character(child), "test_that")
    if (!runtime_matrix_old_r_stress_has_safe_selector_references(
        child, allowed_head)) {
      return(FALSE)
    }
  }
  TRUE
}

runtime_matrix_old_r_stress_starts_on_cran_skip <- function(expression) {
  if (!is.call(expression) || length(expression) < 3L) return(FALSE)
  body <- expression[[3L]]
  body_expressions <- if (is.call(body) && is.name(body[[1L]]) &&
      identical(as.character(body[[1L]]), "{")) {
    as.list(body)[-1L]
  } else {
    list(body)
  }
  if (!length(body_expressions)) return(FALSE)
  first <- body_expressions[[1L]]
  is.call(first) && is.name(first[[1L]]) &&
    identical(as.character(first[[1L]]), "skip_on_cran") &&
    length(first) == 1L
}

runtime_matrix_old_r_stress_quote <- function(value) {
  value <- gsub("\\", "\\\\", value, fixed = TRUE)
  value <- gsub("\"", "\\\"", value, fixed = TRUE)
  paste0("\"", value, "\"")
}

runtime_matrix_old_r_stress_filter_lines <- function(policy) {
  quoted <- runtime_matrix_old_r_stress_quote(policy$test)
  c(
    "# Generated from runtime-matrix-old-r-stress.tsv; do not edit.",
    ".paradox_old_r_stress_titles <- c(",
    paste0("  ", quoted, ifelse(
      seq_along(quoted) == length(quoted), "", ","
    )),
    ")",
    "test_that <- function(desc, code) {",
    "  code_expr <- substitute(code)",
    "  if (!is.character(desc) || length(desc) != 1L || is.na(desc) ||",
    "      !desc %in% .paradox_old_r_stress_titles) {",
    "    return(invisible(NULL))",
    "  }",
    "  call <- substitute(",
    "    testthat::test_that(DESC, CODE),",
    "    list(DESC = desc, CODE = code_expr)",
    "  )",
    "  eval(call, envir = parent.frame())",
    "}"
  )
}

runtime_matrix_old_r_stress_filter_self_test <- function(
    work_directory = tempdir()) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("old-runtime stress filter self-test requires testthat",
      call. = FALSE)
  }
  work_directory <- normalizePath(
    work_directory, winslash = "/", mustWork = TRUE
  )
  test_path <- tempfile(
    ".old-r-stress-filter-", tmpdir = work_directory, fileext = ".R"
  )
  probe_name <- paste0(
    ".paradox_old_r_stress_filter_probe_", Sys.getpid()
  )
  if (exists(probe_name, envir = .GlobalEnv, inherits = FALSE)) {
    stop("old-runtime stress filter self-test probe already exists",
      call. = FALSE)
  }
  assign(probe_name, new.env(parent = emptyenv()), envir = .GlobalEnv)
  probe <- get(probe_name, envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    unlink(test_path, force = TRUE)
    if (exists(probe_name, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = probe_name, envir = .GlobalEnv)
    }
  }, add = TRUE)
  writeLines(c(
    runtime_matrix_old_r_stress_filter_lines(data.frame(
      test = "selected block", stringsAsFactors = FALSE
    )),
    paste0(
      ".probe <- get(\"", probe_name,
      "\", envir = .GlobalEnv, inherits = FALSE)"
    ),
    ".probe$caller <- environment()",
    paste0(
      "test_that(\"unselected block\", {",
      " .probe$unselected <- TRUE; stop(\"unselected code was forced\") })"
    ),
    "test_that(\"selected block\", {",
    "  .probe$selected <- environment()",
    "  testthat::expect_false(identical(environment(), .probe$caller))",
    "  testthat::expect_true(TRUE)",
    "})"
  ), test_path, useBytes = TRUE)

  captured <- character()
  warning_messages <- character()
  output_sink <- sink.number()
  message_sink <- sink.number(type = "message")
  capture_connection <- textConnection(
    "captured", open = "w", local = TRUE
  )
  on.exit({
    while (sink.number(type = "message") > message_sink) {
      sink(type = "message")
    }
    while (sink.number() > output_sink) sink()
    try(close(capture_connection), silent = TRUE)
  }, add = TRUE)
  sink(capture_connection)
  sink(capture_connection, type = "message")
  results <- withCallingHandlers(
    testthat::test_file(test_path, reporter = "summary"),
    warning = function(condition) {
      warning_messages <<- c(
        warning_messages, conditionMessage(condition)
      )
    }
  )
  sink(type = "message")
  sink()
  close(capture_connection)
  summary <- as.data.frame(results)
  raw <- unlist(
    lapply(unclass(results), `[[`, "results"),
    recursive = FALSE,
    use.names = FALSE
  )
  bad <- vapply(
    raw,
    function(result) {
      inherits(result, "expectation_failure") ||
        inherits(result, "expectation_error") ||
        inherits(result, "expectation_warning") ||
        inherits(result, "expectation_skip")
    },
    logical(1L)
  )
  if (nrow(summary) != 1L ||
      !identical(as.character(summary$test), "selected block") ||
      !is.null(probe$unselected) || !is.environment(probe$selected) ||
      identical(probe$selected, probe$caller) || any(bad) ||
      any(grepl("braced expression", warning_messages, fixed = TRUE)) ||
      any(grepl("braced expression", captured, fixed = TRUE))) {
    stop(
      paste(
        "old-runtime stress filter forced an unselected block, lost",
        "test isolation, or changed the selected expression"
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

runtime_matrix_validate_old_r_stress_policy <- function(snapshot) {
  snapshot <- normalizePath(snapshot, winslash = "/", mustWork = TRUE)
  manifest <- file.path(
    snapshot, "environment", "runtime-matrix-old-r-stress.tsv"
  )
  if (!file.exists(manifest) || dir.exists(manifest) ||
      runtime_matrix_old_r_stress_is_symbolic(manifest)) {
    stop("old-runtime stress manifest is absent or symbolic", call. = FALSE)
  }
  policy <- read.delim(
    manifest,
    header = TRUE,
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE
  )
  test_directory <- file.path(snapshot, "tests", "testthat")
  test_files <- dir(
    test_directory,
    pattern = "^test.*\\.[Rr]$",
    full.names = FALSE
  )
  expected_names <- c("file", "test", "mode", "coverage")
  if (!identical(names(policy), expected_names) || !nrow(policy) ||
      anyNA(policy) || any(!nzchar(as.matrix(policy))) ||
      anyDuplicated(paste(policy$file, policy$test, sep = "\t")) ||
      anyDuplicated(policy$test) ||
      any(!grepl("^test[^/]*\\.[Rr]$", policy$file)) ||
      any(!policy$file %in% test_files) ||
      any(!grepl("^[ -~]+$", policy$test)) ||
      any(!policy$mode %in% c("not-cran", "companion")) ||
      any(!grepl(
        "^[a-z][a-z0-9-]*(,[a-z][a-z0-9-]*)*$", policy$coverage
      ))) {
    stop("old-runtime stress manifest is malformed", call. = FALSE)
  }
  order_index <- do.call(order, c(
    policy[c("file", "test")], list(method = "radix")
  ))
  if (!identical(order_index, seq_len(nrow(policy)))) {
    stop("old-runtime stress manifest is not byte-sorted", call. = FALSE)
  }
  allowed_coverage <- c(
    "callback-reentry", "collection-lifetime", "finalizer", "gc-rooting",
    "optional-binding", "trafo-lifetime"
  )
  coverage <- strsplit(policy$coverage, ",", fixed = TRUE)
  if (any(vapply(
      coverage,
      function(value) {
        anyDuplicated(value) ||
          !identical(value, sort(value, method = "radix")) ||
          any(!value %in% allowed_coverage)
      },
      logical(1L)
    )) || !identical(
      sort(unique(unlist(coverage, use.names = FALSE)), method = "radix"),
      allowed_coverage
    )) {
    stop("old-runtime stress coverage is incomplete or malformed",
      call. = FALSE)
  }
  if (!all(c("not-cran", "companion") %in% policy$mode)) {
    stop("old-runtime stress policy lost a required selection mode",
      call. = FALSE)
  }
  parsed <- list()
  for (file in unique(policy$file)) {
    path <- file.path(test_directory, file)
    if (!file.exists(path) || dir.exists(path) ||
        runtime_matrix_old_r_stress_is_symbolic(path)) {
      stop("old-runtime stress policy names an absent or symbolic test file",
        call. = FALSE)
    }
    parsed[[file]] <- parse(path, keep.source = FALSE)
    if (any(!vapply(
        parsed[[file]],
        runtime_matrix_old_r_stress_has_safe_selector_references,
        logical(1L)
      ))) {
      stop(
        paste(
          "old-runtime stress source rebinds, aliases, or dynamically",
          "references the injected test_that selector:"
        ),
        file,
        call. = FALSE
      )
    }
  }
  for (index in seq_len(nrow(policy))) {
    expressions <- parsed[[policy$file[[index]]]]
    titles <- vapply(
      expressions, runtime_matrix_old_r_stress_test_title, character(1L)
    )
    matches <- which(!is.na(titles) & titles == policy$test[[index]])
    if (length(matches) != 1L) {
      stop(
        "old-runtime stress target is absent, nonliteral, or ambiguous: ",
        policy$file[[index]], " / ", policy$test[[index]],
        call. = FALSE
      )
    }
    expression <- expressions[[matches[[1L]]]]
    has_skip <- runtime_matrix_old_r_stress_contains_call(
      expression, "skip_on_cran"
    )
    if (identical(policy$mode[[index]], "not-cran")) {
      if (!has_skip ||
          !runtime_matrix_old_r_stress_starts_on_cran_skip(expression)) {
        stop(
          "not-cran stress target lost its leading skip_on_cran boundary: ",
          policy$file[[index]], " / ", policy$test[[index]],
          call. = FALSE
        )
      }
    } else if (has_skip) {
      stop(
        "companion stress target unexpectedly contains skip_on_cran: ",
        policy$file[[index]], " / ", policy$test[[index]],
        call. = FALSE
      )
    }
  }
  all_titles <- unlist(lapply(
    parsed,
    runtime_matrix_old_r_stress_collect_test_titles
  ), use.names = FALSE)
  if (anyNA(all_titles) || any(vapply(
      policy$test,
      function(title) sum(!is.na(all_titles) & all_titles == title) != 1L,
      logical(1L)
    ))) {
    stop(
      paste(
        "old-runtime stress calls are not all unqualified/literal, or target",
        "titles are not globally unique in selected files"
      ),
      call. = FALSE
    )
  }
  policy
}

runtime_matrix_old_r_stress_policy_main <- function(
    args = commandArgs(TRUE)) {
  filter_mode <- length(args) == 2L &&
    identical(args[[1L]], "--filter")
  if (length(args) != 1L && !filter_mode) {
    stop(
      paste(
        "usage: runtime-matrix-old-r-stress-policy.R",
        "[--filter] SNAPSHOT"
      ),
      call. = FALSE
    )
  }
  snapshot <- if (filter_mode) args[[2L]] else args[[1L]]
  policy <- runtime_matrix_validate_old_r_stress_policy(snapshot)
  if (filter_mode) {
    writeLines(
      runtime_matrix_old_r_stress_filter_lines(policy),
      con = stdout(),
      useBytes = TRUE
    )
    return(invisible(policy))
  }
  cat("runtime_matrix_old_r_stress_policy=passed\n")
  cat("selected_file_count=", length(unique(policy$file)), "\n", sep = "")
  cat("selected_target_count=", nrow(policy), "\n", sep = "")
}

if (sys.nframe() == 0L) {
  runtime_matrix_old_r_stress_policy_main()
}
