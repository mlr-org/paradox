#!/usr/bin/env Rscript

runtime_matrix_skip_policy_is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) & nzchar(target)
}

runtime_matrix_skip_policy_children <- function(value) {
  output <- list()
  for (index in seq_along(value)) {
    if (!identical(value[[index]], quote(expr = ))) {
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

runtime_matrix_named_call_sequence <- function(value) {
  sequence <- character()
  walk <- function(node) {
    if (!is.call(node) && !is.expression(node)) return(invisible(NULL))
    if (is.call(node) && is.name(node[[1L]])) {
      sequence <<- c(sequence, as.character(node[[1L]]))
    }
    for (child in runtime_matrix_skip_policy_children(node)) walk(child)
    invisible(NULL)
  }
  walk(value)
  sequence
}

runtime_matrix_direct_test_expressions <- function(test_call) {
  if (!is.call(test_call) || length(test_call) < 3L) return(list())
  body <- test_call[[3L]]
  if (is.call(body) && is.name(body[[1L]]) &&
      identical(as.character(body[[1L]]), "{")) {
    return(runtime_matrix_skip_policy_children(body)[-1L])
  }
  list(body)
}

runtime_matrix_leading_guard_sequence <- function(test_call, guard_names) {
  sequence <- character()
  for (expression in runtime_matrix_direct_test_expressions(test_call)) {
    if (!is.call(expression) || !is.name(expression[[1L]])) break
    name <- as.character(expression[[1L]])
    if (!name %in% guard_names) break
    sequence <- c(sequence, name)
  }
  sequence
}

runtime_matrix_literal_test_blocks <- function(path) {
  blocks <- list()
  walk <- function(value) {
    if (is.call(value) && is.name(value[[1L]]) &&
        identical(as.character(value[[1L]]), "test_that")) {
      title <- value[[2L]]
      if (!is.character(title) || length(title) != 1L || is.na(title) ||
          !nzchar(title)) {
        stop("test_that block has a non-literal title", call. = FALSE)
      }
      blocks[[length(blocks) + 1L]] <<- list(title = title, call = value)
      return(invisible(NULL))
    }
    if (is.call(value) || is.expression(value)) {
      for (child in runtime_matrix_skip_policy_children(value)) walk(child)
    }
    invisible(NULL)
  }
  walk(parse(path, keep.source = FALSE))
  blocks
}

runtime_matrix_not_cran_test_titles <- function(path) {
  blocks <- runtime_matrix_literal_test_blocks(path)
  if (!length(blocks)) return(character())
  vapply(
    blocks[vapply(
      blocks,
      function(block) {
        runtime_matrix_contains_named_call(block$call, "skip_on_cran")
      },
      logical(1L)
    )],
    `[[`,
    character(1L),
    "title"
  )
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
  registry_path <- file.path(snapshot, "environment", "runtime-matrix.tsv")
  if (!file.exists(registry_path) || dir.exists(registry_path) ||
      runtime_matrix_skip_policy_is_symbolic(registry_path)) {
    stop("runtime registry is absent or symbolic", call. = FALSE)
  }
  registry <- read.delim(
    registry_path,
    header = TRUE,
    quote = "",
    comment.char = "",
    colClasses = "character",
    check.names = FALSE
  )
  expected_registry_columns <- c(
    "ordinal", "runtime", "platform", "runtime_lock", "dependency_mode",
    "dependency_lock", "api_branch", "prefix_repair"
  )
  if (!identical(names(registry), expected_registry_columns) ||
      !nrow(registry) || anyNA(registry) ||
      anyDuplicated(registry$runtime)) {
    stop("runtime registry is malformed", call. = FALSE)
  }
  reviewed_runtimes <- registry$runtime
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
  # The supported-runtime suite deliberately runs with NOT_CRAN=true. Keep
  # discovering skip_on_cran() blocks so duplicate literal identities and
  # guard ordering remain authenticated, but do not admit "On CRAN" as an
  # expected result: those blocks must execute. Only genuine runtime
  # capabilities may remain in the result-skip ledger.
  expected_baseline <- policy[FALSE, , drop = FALSE]
  extra <- policy
  allowed_extra_reasons <- c(
    active_binding =
      "Reason: R < 4.0 cannot safely inspect active-binding functions",
    list_altrep =
      "Reason: R < 4.3 cannot construct list ALTREP test fixtures",
    old_binding_existence =
      paste0(
        "Reason: R >= 4.2 has a public non-evaluating ",
        "binding-existence operation"
      )
  )
  active_binding_runtimes <- reviewed_runtimes[
    vapply(
      reviewed_runtimes,
      function(runtime) utils::compareVersion(runtime, "4.0.0") < 0L,
      logical(1L)
    )
  ]
  list_altrep_runtimes <- reviewed_runtimes[
    vapply(
      reviewed_runtimes,
      function(runtime) utils::compareVersion(runtime, "4.3.0") < 0L,
      logical(1L)
    )
  ]
  old_binding_existence_runtimes <- reviewed_runtimes[
    vapply(
      reviewed_runtimes,
      function(runtime) utils::compareVersion(runtime, "4.2.0") >= 0L,
      logical(1L)
    )
  ]
  valid_extra_runtime <- (
    extra$reason == allowed_extra_reasons[["active_binding"]] &
      extra$runtime %in% active_binding_runtimes
  ) | (
    extra$reason == allowed_extra_reasons[["list_altrep"]] &
      extra$runtime %in% list_altrep_runtimes
  ) | (
    extra$reason == allowed_extra_reasons[["old_binding_existence"]] &
      extra$runtime %in% old_binding_existence_runtimes
  )
  if (any(!extra$reason %in% allowed_extra_reasons) ||
      any(!valid_extra_runtime)) {
    stop("result-skip manifest contains an unreviewed version-specific skip",
      call. = FALSE)
  }
  source_blocks <- setNames(lapply(test_paths, runtime_matrix_literal_test_blocks),
    test_files)
  reviewed_triggers <- c(
    "skip_on_cran", "skip_if_no_active_binding_inspection",
    "skip_if_no_list_altrep", "skip_if_no_old_r_binding_existence_path"
  )
  for (blocks in source_blocks) {
    for (block in blocks) {
      recursive_sequence <- runtime_matrix_named_call_sequence(block$call)
      leading_sequence <- runtime_matrix_leading_guard_sequence(
        block$call, reviewed_triggers
      )
      occurrences <- vapply(
        reviewed_triggers,
        function(trigger) sum(recursive_sequence == trigger),
        integer(1L)
      )
      if (any(occurrences > 0L & (
          occurrences != 1L |
            !reviewed_triggers %in% leading_sequence
        ))) {
        stop(
          "version-specific and CRAN skip helpers must be unique leading guards",
          call. = FALSE
        )
      }
      positions <- match(reviewed_triggers, leading_sequence)
      if (!is.na(positions[[1L]]) &&
          any(positions[[1L]] > positions[-1L], na.rm = TRUE)) {
        stop("skip_on_cran must precede version-specific skip guards",
          call. = FALSE)
      }
      if (!is.na(positions[[2L]]) && !is.na(positions[[3L]]) &&
          positions[[2L]] > positions[[3L]]) {
        stop("active-binding skip must precede the list-ALTREP skip",
          call. = FALSE)
      }
      if (!is.na(positions[[2L]]) && !is.na(positions[[4L]]) &&
          positions[[2L]] > positions[[4L]]) {
        stop("active-binding skip must precede the old-binding-existence skip",
          call. = FALSE)
      }
      if (!is.na(positions[[3L]]) && !is.na(positions[[4L]]) &&
          positions[[3L]] > positions[[4L]]) {
        stop("list-ALTREP skip must precede the old-binding-existence skip",
          call. = FALSE)
      }
    }
  }
  extra_calls <- lapply(seq_len(nrow(extra)), function(index) {
    blocks <- source_blocks[[extra$file[[index]]]]
    positions <- which(vapply(
      blocks,
      function(block) identical(block$title, extra$test[[index]]),
      logical(1L)
    ))
    if (length(positions) != 1L) {
      stop("version-specific result skip has no unique literal test block",
        call. = FALSE)
    }
    blocks[[positions[[1L]]]]$call
  })
  for (index in seq_len(nrow(extra))) {
    expected_call <- switch(
      match(extra$reason[[index]], allowed_extra_reasons),
      "skip_if_no_active_binding_inspection",
      "skip_if_no_list_altrep",
      "skip_if_no_old_r_binding_existence_path"
    )
    if (!expected_call %in% runtime_matrix_leading_guard_sequence(
        extra_calls[[index]], reviewed_triggers
      )) {
      stop("version-specific result skip does not contain its reviewed trigger",
        call. = FALSE)
    }
  }
  active_rows <- lapply(names(source_blocks), function(file) {
    blocks <- source_blocks[[file]]
    titles <- vapply(
      blocks[vapply(
        blocks,
        function(block) "skip_if_no_active_binding_inspection" %in%
          runtime_matrix_leading_guard_sequence(
            block$call, reviewed_triggers
          ),
        logical(1L)
      )],
      `[[`,
      character(1L),
      "title"
    )
    if (!length(titles)) return(NULL)
    do.call(rbind, lapply(active_binding_runtimes, function(runtime) {
      data.frame(
        runtime = rep(runtime, length(titles)),
        file = rep(file, length(titles)),
        test = titles,
        reason = rep(allowed_extra_reasons[["active_binding"]], length(titles)),
        stringsAsFactors = FALSE
      )
    }))
  })
  active_rows <- do.call(rbind, active_rows)
  if (is.null(active_rows)) active_rows <- extra[FALSE, , drop = FALSE]
  recorded_active <- extra[
    extra$reason == allowed_extra_reasons[["active_binding"]],
    ,
    drop = FALSE
  ]
  active_order <- function(value) {
    value[do.call(order, c(
      value[c("runtime", "file", "test", "reason")],
      list(method = "radix")
    )), , drop = FALSE]
  }
  row.names(active_rows) <- NULL
  row.names(recorded_active) <- NULL
  active_rows <- active_order(active_rows)
  baseline_keys <- paste(
    expected_baseline$runtime, expected_baseline$file,
    expected_baseline$test, sep = "\t"
  )
  active_rows <- active_rows[
    !paste(
      active_rows$runtime, active_rows$file, active_rows$test,
      sep = "\t"
    ) %in% baseline_keys,
    ,
    drop = FALSE
  ]
  recorded_active <- active_order(recorded_active)
  row.names(active_rows) <- NULL
  row.names(recorded_active) <- NULL
  if (!identical(recorded_active, active_rows)) {
    stop("active-binding result skips differ from current guarded tests",
      call. = FALSE)
  }
  list_altrep_rows <- lapply(names(source_blocks), function(file) {
    blocks <- source_blocks[[file]]
    titles <- vapply(
      blocks[vapply(
        blocks,
        function(block) "skip_if_no_list_altrep" %in%
          runtime_matrix_leading_guard_sequence(
            block$call, reviewed_triggers
          ),
        logical(1L)
      )],
      `[[`,
      character(1L),
      "title"
    )
    if (!length(titles)) return(NULL)
    do.call(rbind, lapply(list_altrep_runtimes, function(runtime) {
      data.frame(
        runtime = rep(runtime, length(titles)),
        file = rep(file, length(titles)),
        test = titles,
        reason = rep(allowed_extra_reasons[["list_altrep"]], length(titles)),
        stringsAsFactors = FALSE
      )
    }))
  })
  list_altrep_rows <- do.call(rbind, list_altrep_rows)
  if (is.null(list_altrep_rows)) {
    list_altrep_rows <- extra[FALSE, , drop = FALSE]
  }
  earlier_keys <- c(
    baseline_keys,
    paste(active_rows$runtime, active_rows$file, active_rows$test, sep = "\t")
  )
  list_altrep_rows <- list_altrep_rows[
    !paste(
      list_altrep_rows$runtime, list_altrep_rows$file,
      list_altrep_rows$test, sep = "\t"
    ) %in% earlier_keys,
    ,
    drop = FALSE
  ]
  recorded_list_altrep <- extra[
    extra$reason == allowed_extra_reasons[["list_altrep"]],
    ,
    drop = FALSE
  ]
  row.names(list_altrep_rows) <- NULL
  row.names(recorded_list_altrep) <- NULL
  list_altrep_rows <- active_order(list_altrep_rows)
  recorded_list_altrep <- active_order(recorded_list_altrep)
  row.names(list_altrep_rows) <- NULL
  row.names(recorded_list_altrep) <- NULL
  if (!identical(recorded_list_altrep, list_altrep_rows)) {
    stop("list-ALTREP result skips differ from current guarded tests",
      call. = FALSE)
  }
  old_binding_existence_rows <- lapply(names(source_blocks), function(file) {
    blocks <- source_blocks[[file]]
    titles <- vapply(
      blocks[vapply(
        blocks,
        function(block) "skip_if_no_old_r_binding_existence_path" %in%
          runtime_matrix_leading_guard_sequence(
            block$call, reviewed_triggers
          ),
        logical(1L)
      )],
      `[[`,
      character(1L),
      "title"
    )
    if (!length(titles)) return(NULL)
    do.call(rbind, lapply(
      old_binding_existence_runtimes,
      function(runtime) {
        data.frame(
          runtime = rep(runtime, length(titles)),
          file = rep(file, length(titles)),
          test = titles,
          reason = rep(
            allowed_extra_reasons[["old_binding_existence"]],
            length(titles)
          ),
          stringsAsFactors = FALSE
        )
      }
    ))
  })
  old_binding_existence_rows <- do.call(rbind, old_binding_existence_rows)
  if (is.null(old_binding_existence_rows)) {
    old_binding_existence_rows <- extra[FALSE, , drop = FALSE]
  }
  old_binding_existence_rows <- old_binding_existence_rows[
    !paste(
      old_binding_existence_rows$runtime,
      old_binding_existence_rows$file,
      old_binding_existence_rows$test,
      sep = "\t"
    ) %in% c(
      baseline_keys,
      paste(active_rows$runtime, active_rows$file, active_rows$test, sep = "\t"),
      paste(
        list_altrep_rows$runtime,
        list_altrep_rows$file,
        list_altrep_rows$test,
        sep = "\t"
      )
    ),
    ,
    drop = FALSE
  ]
  recorded_old_binding_existence <- extra[
    extra$reason == allowed_extra_reasons[["old_binding_existence"]],
    ,
    drop = FALSE
  ]
  old_binding_existence_rows <- active_order(old_binding_existence_rows)
  recorded_old_binding_existence <-
    active_order(recorded_old_binding_existence)
  row.names(old_binding_existence_rows) <- NULL
  row.names(recorded_old_binding_existence) <- NULL
  if (!identical(
      recorded_old_binding_existence,
      old_binding_existence_rows
    )) {
    stop(
      "old binding-existence result skips differ from current guarded tests",
      call. = FALSE
    )
  }
  expected <- rbind(
    expected_baseline,
    active_rows,
    list_altrep_rows,
    old_binding_existence_rows
  )
  expected <- expected[do.call(order, c(
    expected[c("runtime", "file", "test", "reason")],
    list(method = "radix")
  )), , drop = FALSE]
  row.names(expected) <- NULL
  if (!identical(policy, expected)) {
    stop(
      "result-skip manifest differs from current runtime-capability guards",
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
