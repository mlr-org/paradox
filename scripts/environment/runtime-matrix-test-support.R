runtime_matrix_test_tree_is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  !is.na(target) & nzchar(target)
}

runtime_matrix_test_tree_byte_sort <- function(value) {
  value[order(value, method = "radix")]
}

runtime_matrix_test_tree_assert_safe_relative <- function(relative) {
  if (!length(relative)) {
    return(invisible(TRUE))
  }
  components <- strsplit(relative, "/", fixed = TRUE)
  unsafe <- !nzchar(relative) | startsWith(relative, "/") |
    endsWith(relative, "/") | grepl("//", relative, fixed = TRUE) |
    grepl("[[:cntrl:]]", relative) |
    vapply(
      components,
      function(value) {
        any(!nzchar(value) | value == "." | value == "..")
      },
      logical(1L)
    )
  if (any(unsafe)) {
    stop(
      "test-support tree contains an ambiguous or control-character path",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

runtime_matrix_test_tree_kind <- function(path) {
  stat <- Sys.which("stat")
  if (length(stat) != 1L || is.na(stat) || !nzchar(stat)) {
    stop("GNU stat is required for plain test-support admission",
      call. = FALSE)
  }
  vapply(
    path,
    function(value) {
      output <- system2(
        stat,
        args = c("--format=%F", "--", shQuote(value)),
        stdout = TRUE,
        stderr = TRUE,
        env = "LC_ALL=C"
      )
      status <- attr(output, "status")
      if ((!is.null(status) && status != 0L) || length(output) != 1L ||
          !output %in% c(
            "directory", "regular file", "regular empty file"
          )) {
        stop(
          "test-support tree contains a missing or non-regular member",
          call. = FALSE
        )
      }
      if (identical(output, "regular empty file")) {
        "regular file"
      } else {
        output
      }
    },
    character(1L),
    USE.NAMES = FALSE
  )
}

runtime_matrix_test_tree_discover <- function(directory) {
  if (!file.exists(directory) || !dir.exists(directory) ||
      runtime_matrix_test_tree_is_symbolic(directory)) {
    stop("test-support root is absent or symbolic", call. = FALSE)
  }
  root <- normalizePath(directory, mustWork = TRUE)
  pending_absolute <- root
  pending_relative <- ""
  directories <- character()
  files <- character()

  while (length(pending_absolute)) {
    parent_absolute <- pending_absolute[[1L]]
    parent_relative <- pending_relative[[1L]]
    pending_absolute <- pending_absolute[-1L]
    pending_relative <- pending_relative[-1L]
    entries <- dir(
      parent_absolute,
      all.files = TRUE,
      no.. = TRUE,
      full.names = FALSE
    )
    if (!length(entries)) {
      next
    }
    entries <- runtime_matrix_test_tree_byte_sort(entries)
    absolute <- file.path(parent_absolute, entries)
    relative <- if (nzchar(parent_relative)) {
      file.path(parent_relative, entries)
    } else {
      entries
    }
    relative <- gsub(.Platform$file.sep, "/", relative, fixed = TRUE)
    runtime_matrix_test_tree_assert_safe_relative(relative)
    if (any(runtime_matrix_test_tree_is_symbolic(absolute))) {
      stop("test-support tree contains a symbolic member", call. = FALSE)
    }
    kind <- runtime_matrix_test_tree_kind(absolute)
    is_directory <- kind == "directory"
    is_file <- kind == "regular file"
    if (any(is_directory)) {
      new_directories <- relative[is_directory]
      directories <- c(directories, new_directories)
      pending_absolute <- c(pending_absolute, absolute[is_directory])
      pending_relative <- c(pending_relative, new_directories)
    }
    if (any(is_file)) {
      files <- c(files, relative[is_file])
    }
  }

  directories <- runtime_matrix_test_tree_byte_sort(directories)
  files <- runtime_matrix_test_tree_byte_sort(files)
  if (anyDuplicated(directories) || anyDuplicated(files) ||
      any(directories %in% files)) {
    stop("test-support tree inventory is ambiguous", call. = FALSE)
  }
  top_level <- !grepl("/", files, fixed = TRUE)
  tests <- files[
    top_level & grepl("^test.*\\.[rR]$", files)
  ]
  support <- files[!files %in% tests]
  list(
    directories = directories,
    files = files,
    tests = runtime_matrix_test_tree_byte_sort(tests),
    support = runtime_matrix_test_tree_byte_sort(support)
  )
}

runtime_matrix_test_tree_same_file <- function(first, second) {
  first_info <- file.info(first)
  second_info <- file.info(second)
  if (anyNA(first_info$size) || anyNA(second_info$size) ||
      first_info$size != second_info$size) {
    return(FALSE)
  }
  first_connection <- file(first, open = "rb")
  on.exit(close(first_connection))
  second_connection <- file(second, open = "rb")
  on.exit(close(second_connection), add = TRUE)
  repeat {
    first_chunk <- readBin(first_connection, what = "raw", n = 65536L)
    second_chunk <- readBin(second_connection, what = "raw", n = 65536L)
    if (!identical(first_chunk, second_chunk)) {
      return(FALSE)
    }
    if (!length(first_chunk)) {
      return(TRUE)
    }
  }
}

runtime_matrix_test_tree_mode <- function(path) {
  info <- file.info(path)
  if (nrow(info) != 1L || is.na(info$mode[[1L]])) {
    stop("could not inspect a test-support member mode", call. = FALSE)
  }
  as.integer(info$mode[[1L]])
}

runtime_matrix_test_tree_chmod_exact <- function(path, mode) {
  previous_umask <- Sys.umask("0000")
  on.exit(Sys.umask(previous_umask))
  Sys.chmod(path, mode = mode)
  invisible(NULL)
}

runtime_matrix_test_tree_create_private_directory <- function(path) {
  if (file.exists(path) || runtime_matrix_test_tree_is_symbolic(path)) {
    stop("private test-support directory already exists or is symbolic",
      call. = FALSE)
  }
  previous_umask <- Sys.umask("0000")
  on.exit(Sys.umask(previous_umask))
  if (!dir.create(path, mode = "0700") ||
      runtime_matrix_test_tree_is_symbolic(path)) {
    stop("could not create a private test-support directory",
      call. = FALSE)
  }
  runtime_matrix_test_tree_chmod_exact(path, "0700")
  if (runtime_matrix_test_tree_mode(path) != strtoi("700", base = 8L)) {
    stop("private test-support directory has an inexact mode",
      call. = FALSE)
  }
  invisible(NULL)
}

runtime_matrix_test_tree_stage <- function(
  source_directory,
  staged_directory,
  selected_tests
) {
  source <- runtime_matrix_test_tree_discover(source_directory)
  if (!is.character(selected_tests) || anyNA(selected_tests) ||
      any(!nzchar(selected_tests)) || anyDuplicated(selected_tests) ||
      any(!selected_tests %in% source$tests)) {
    stop("selected test inventory is not an exact source subset",
      call. = FALSE)
  }
  selected_tests <- runtime_matrix_test_tree_byte_sort(selected_tests)
  if (!file.exists(staged_directory) || !dir.exists(staged_directory) ||
      runtime_matrix_test_tree_is_symbolic(staged_directory) ||
      length(dir(
        staged_directory,
        all.files = TRUE,
        no.. = TRUE,
        full.names = FALSE
      ))) {
    stop("staged test-support root is absent, symbolic, or non-empty",
      call. = FALSE)
  }
  source_root <- normalizePath(source_directory, mustWork = TRUE)
  staged_root <- normalizePath(staged_directory, mustWork = TRUE)
  source_prefix <- paste0(source_root, .Platform$file.sep)
  staged_prefix <- paste0(staged_root, .Platform$file.sep)
  if (identical(source_root, staged_root) ||
      startsWith(source_root, staged_prefix) ||
      startsWith(staged_root, source_prefix)) {
    stop("source and staged test-support trees overlap", call. = FALSE)
  }
  previous_umask <- Sys.umask("0000")
  on.exit(Sys.umask(previous_umask))

  if (length(source$directories)) {
    depth <- lengths(strsplit(source$directories, "/", fixed = TRUE))
    directory_order <- order(
      depth, source$directories, method = "radix"
    )
    for (relative in source$directories[directory_order]) {
      staged_path <- file.path(staged_root, relative)
      runtime_matrix_test_tree_create_private_directory(staged_path)
    }
  }

  staged_files <- runtime_matrix_test_tree_byte_sort(c(
    source$support,
    selected_tests
  ))
  for (relative in staged_files) {
    source_path <- file.path(source_root, relative)
    staged_path <- file.path(staged_root, relative)
    if (file.exists(staged_path) ||
        runtime_matrix_test_tree_is_symbolic(staged_path) ||
        !file.copy(
          source_path,
          staged_path,
          copy.mode = TRUE,
          copy.date = FALSE
        )) {
      stop("could not copy an exact staged test-support file",
        call. = FALSE)
    }
    runtime_matrix_test_tree_chmod_exact(
      staged_path,
      file.info(source_path)$mode[[1L]]
    )
    if (!runtime_matrix_test_tree_same_file(source_path, staged_path) ||
        runtime_matrix_test_tree_mode(staged_path) !=
          runtime_matrix_test_tree_mode(source_path)) {
      stop("staged test-support file differs from source",
        call. = FALSE)
    }
  }

  if (length(source$directories)) {
    depth <- lengths(strsplit(source$directories, "/", fixed = TRUE))
    directory_order <- order(
      -depth, source$directories, method = "radix"
    )
    for (relative in source$directories[directory_order]) {
      source_path <- file.path(source_root, relative)
      staged_path <- file.path(staged_root, relative)
      runtime_matrix_test_tree_chmod_exact(
        staged_path,
        file.info(source_path)$mode[[1L]]
      )
      if (runtime_matrix_test_tree_mode(staged_path) !=
          runtime_matrix_test_tree_mode(source_path)) {
        stop("staged support directory mode differs from source",
          call. = FALSE)
      }
    }
  }

  staged <- runtime_matrix_test_tree_discover(staged_root)
  if (!identical(staged$directories, source$directories) ||
      !identical(staged$files, staged_files) ||
      !identical(staged$tests, selected_tests) ||
      !identical(staged$support, source$support)) {
    stop("staged test-support tree differs from the selected projection",
      call. = FALSE)
  }
  list(
    support_files = source$support,
    support_directories = source$directories,
    staged_files = staged_files
  )
}

runtime_matrix_test_tree_receipt <- function(
  receipt_helper,
  operation,
  staged_directory,
  receipt
) {
  if (!identical(operation, "create") && !identical(operation, "verify")) {
    stop("unknown test-support receipt operation", call. = FALSE)
  }
  if (!file.exists(receipt_helper) || dir.exists(receipt_helper) ||
      runtime_matrix_test_tree_is_symbolic(receipt_helper) ||
      file.access(receipt_helper, mode = 1L) != 0L) {
    stop("test-support receipt helper is absent or symbolic",
      call. = FALSE)
  }
  arguments <- vapply(
    c(operation, staged_directory, receipt),
    shQuote,
    character(1L)
  )
  output <- system2(
    receipt_helper,
    args = arguments,
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      paste(
        "test-support tree receipt failed:",
        paste(output, collapse = "\n")
      ),
      call. = FALSE
    )
  }
  if (!file.exists(receipt) || dir.exists(receipt) ||
      runtime_matrix_test_tree_is_symbolic(receipt)) {
    stop("test-support tree receipt is absent or symbolic",
      call. = FALSE)
  }
  invisible(output)
}
