#!/usr/bin/env Rscript

# Create or verify a deterministic, content-based identity for the compiler
# closure used by the offline public-R-API gate.  Hashing the whole local
# toolchain would read several gigabytes; this receipt instead covers the two
# drivers, target configuration, GCC cc1, their resolved dynamic libraries,
# and every default/explicit toolchain header tree that can affect syntax
# admission.

args <- commandArgs(trailingOnly = TRUE)
usage <- paste(
  "usage: compiler-identity.R create|verify GCC CLANG TOOLCHAIN_ROOT RECEIPT",
  "       compiler-identity.R self-test"
)
if (!length(args) || !args[[1L]] %in% c("create", "verify", "self-test")) {
  stop(usage, call. = FALSE)
}

fail <- function(...) stop(..., call. = FALSE)

sha256_text <- function(value) {
  temporary <- tempfile("compiler-identity-text-")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeBin(charToRaw(enc2utf8(value)), temporary)
  unname(tools::sha256sum(temporary))
}

path_link <- function(path) {
  value <- Sys.readlink(path)
  if (length(value) != 1L || is.na(value)) "" else value
}

plain_directory <- function(path, label) {
  if (!dir.exists(path) || nzchar(path_link(path))) {
    fail(label, " is absent, non-directory, or symbolic: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

plain_file_or_link <- function(path, label) {
  if (!file.exists(path) || dir.exists(path)) {
    fail(label, " is absent or not a file: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

safe_value <- function(value, label) {
  value <- paste(value, collapse = "\n")
  if (!nzchar(value)) fail(label, " is empty")
  value
}

run_tool <- function(tool, arguments, label) {
  old <- Sys.getenv(
    c("LD_PRELOAD", "DYLD_INSERT_LIBRARIES", "COMPILER_PATH", "GCC_EXEC_PREFIX"),
    unset = NA_character_
  )
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv,
        setNames(list(old[[name]]), name))
    }
  }, add = TRUE)
  Sys.unsetenv(names(old))
  result <- suppressWarnings(system2(
    tool,
    arguments,
    stdout = TRUE,
    stderr = TRUE,
    env = c("LC_ALL=C", "LANG=C", "TZ=UTC")
  ))
  status <- attr(result, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    fail(label, " failed with status ", status, ": ",
      paste(result, collapse = " | "))
  }
  safe_value(result, label)
}

parse_include_search <- function(output, label) {
  lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  start <- which(lines == "#include <...> search starts here:")
  end <- which(lines == "End of search list.")
  if (length(start) != 1L || length(end) != 1L || end[[1L]] <= start[[1L]]) {
    fail(label, " did not report one unambiguous include search list")
  }
  active_raw <- trimws(lines[seq.int(start[[1L]] + 1L, end[[1L]] - 1L)])
  active_raw <- sub("[[:space:]]+\\(framework directory\\)$", "", active_raw)
  if (!length(active_raw) || any(!nzchar(active_raw)) ||
      any(!startsWith(active_raw, "/")) || any(grepl("[\t\r\n]", active_raw))) {
    fail(label, " reported an empty, relative, or malformed include path")
  }
  active <- unique(vapply(active_raw, plain_directory, character(1L),
    label = paste0(label, " active include path")))

  absent_lines <- lines[grepl(
    "^ignoring nonexistent directory \\\"[^\\\"]+\\\"$", lines
  )]
  absent_raw <- unique(sub(
    "^ignoring nonexistent directory \\\"([^\\\"]+)\\\"$", "\\1",
    absent_lines
  ))
  if (length(absent_raw) &&
      (any(!startsWith(absent_raw, "/")) ||
       any(grepl("[\t\r\n]", absent_raw)))) {
    fail(label, " reported a relative or malformed absent include path")
  }
  list(active_raw = active_raw, active = active, absent_raw = absent_raw)
}

include_path_inventory <- function(paths) {
  if (!length(paths)) return("-")
  paste(paths, collapse = "\n")
}

absent_include_inventory <- function(paths, label) {
  if (!length(paths)) return("-")
  states <- vapply(paths, function(path) {
    link <- path_link(path)
    if (file.exists(path) || dir.exists(path)) {
      fail(label, " was reported absent but now exists: ", path)
    }
    state <- if (nzchar(link)) paste0("dangling_symlink=", link) else "absent"
    paste(path, state, sep = "\t")
  }, character(1L))
  paste(states, collapse = "\n")
}

empty_manifest <- function() {
  data.frame(
    schema = character(), kind = character(), role = character(),
    path = character(), target = character(), size = character(),
    mode = character(), sha256 = character(), stringsAsFactors = FALSE
  )
}

row_value <- function(role, value) {
  value <- safe_value(value, role)
  data.frame(
    schema = "1", kind = "value", role = role, path = "-", target = "-",
    size = as.character(length(charToRaw(enc2utf8(value)))), mode = "-",
    sha256 = sha256_text(value), stringsAsFactors = FALSE
  )
}

row_file <- function(role, path) {
  canonical <- plain_file_or_link(path, role)
  info <- file.info(canonical)
  if (anyNA(info[c("size", "mode")])) fail("could not stat ", canonical)
  data.frame(
    schema = "1", kind = "file", role = role, path = canonical,
    target = "-", size = format(info$size, scientific = FALSE, trim = TRUE),
    mode = sprintf("%04o", as.integer(info$mode)),
    sha256 = unname(tools::sha256sum(canonical)), stringsAsFactors = FALSE
  )
}

row_invocation <- function(role, path) {
  if (!file.exists(path) || dir.exists(path)) {
    fail(role, " invocation is absent or not a file: ", path)
  }
  absolute <- if (startsWith(path, "/")) path else normalizePath(path,
    winslash = "/", mustWork = TRUE)
  link <- path_link(absolute)
  canonical <- normalizePath(absolute, winslash = "/", mustWork = TRUE)
  rbind(
    row_value(paste0(role, "_invoked_path"), absolute),
    row_value(paste0(role, "_link_target"), if (nzchar(link)) link else "-"),
    row_file(paste0(role, "_executable"), canonical)
  )
}

collect_tree <- function(root, role) {
  root <- plain_directory(root, role)
  rows <- list()
  add <- function(row) rows[[length(rows) + 1L]] <<- row
  visit <- function(directory, relative = "") {
    entries <- list.files(
      directory, all.files = TRUE, full.names = FALSE, no.. = TRUE,
      recursive = FALSE
    )
    entries <- sort(entries, method = "radix")
    for (entry in entries) {
      if (!nzchar(entry) || grepl("[\t\r\n]", entry)) {
        fail(role, " tree contains an unsupported member name")
      }
      child_relative <- if (nzchar(relative)) {
        paste0(relative, "/", entry)
      } else entry
      path <- file.path(directory, entry)
      link <- path_link(path)
      if (nzchar(link)) {
        canonical <- normalizePath(path, winslash = "/", mustWork = TRUE)
        if (dir.exists(canonical)) {
          fail(role, " tree contains a symbolic directory: ", child_relative)
        }
        add(data.frame(
          schema = "1", kind = "symlink", role = role,
          path = paste0(root, "/", child_relative), target = link,
          size = "-", mode = "-", sha256 = "-", stringsAsFactors = FALSE
        ))
        add(row_file(paste0(role, "_symlink_target"), canonical))
      } else if (dir.exists(path)) {
        info <- file.info(path)
        if (is.na(info$mode)) fail("could not stat tree directory: ", path)
        add(data.frame(
          schema = "1", kind = "directory", role = role,
          path = paste0(root, "/", child_relative), target = "-", size = "-",
          mode = sprintf("%04o", as.integer(info$mode)), sha256 = "-",
          stringsAsFactors = FALSE
        ))
        visit(path, child_relative)
      } else {
        add(row_file(role, path))
      }
    }
  }
  root_info <- file.info(root)
  add(data.frame(
    schema = "1", kind = "directory", role = role, path = root,
    target = "-", size = "-",
    mode = sprintf("%04o", as.integer(root_info$mode)), sha256 = "-",
    stringsAsFactors = FALSE
  ))
  visit(root)
  do.call(rbind, rows)
}

parse_ldd <- function(output, label) {
  lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  if (any(grepl("=>[[:space:]]+not found", lines))) {
    fail(label, " has an unresolved dynamic dependency")
  }
  paths <- character()
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "linux-vdso")) next
    if (grepl("=>", line, fixed = TRUE)) {
      candidate <- sub("^.*=>[[:space:]]+", "", line)
      candidate <- sub("[[:space:]]+\\(0x[0-9A-Fa-f]+\\).*$", "", candidate)
    } else {
      candidate <- sub("[[:space:]]+\\(0x[0-9A-Fa-f]+\\).*$", "", line)
    }
    if (startsWith(candidate, "/")) paths <- c(paths, candidate)
  }
  paths <- unique(vapply(paths, normalizePath, character(1L), winslash = "/",
    mustWork = TRUE))
  sort(paths, method = "radix")
}

validate_manifest <- function(value) {
  expected <- names(empty_manifest())
  if (!identical(names(value), expected) || !nrow(value) || anyNA(value) ||
      any(value$schema != "1") || any(!value$kind %in%
        c("value", "file", "directory", "symlink")) ||
      any(!grepl("^[a-z][a-z0-9_]*$", value$role)) ||
      any(grepl("[\t\r\n]", value$path)) ||
      anyDuplicated(paste(value$kind, value$role, value$path, sep = "\t"))) {
    fail("compiler identity receipt has an invalid schema or identity")
  }
  content <- value$kind %in% c("value", "file")
  if (any(!grepl("^[0-9a-f]{64}$", value$sha256[content])) ||
      any(!grepl("^(0|[1-9][0-9]*)$", value$size[content])) ||
      any(value$sha256[!content] != "-") ||
      any(value$kind == "file" & !grepl("^[0-7]{4}$", value$mode)) ||
      any(value$kind == "directory" & !grepl("^[0-7]{4}$", value$mode)) ||
      any(value$kind %in% c("value", "symlink") & value$mode != "-") ||
      any(value$kind != "symlink" & value$target != "-") ||
      any(value$kind == "symlink" & (value$target == "-" |
        grepl("[\t\r\n]", value$target)))) {
    fail("compiler identity receipt has invalid kind-specific fields")
  }
  invisible(value)
}

discover_identity <- function(gcc, clang, toolchain) {
  if (!identical(Sys.info()[["sysname"]], "Linux")) {
    fail("compiler identity discovery currently supports Linux only")
  }
  toolchain <- plain_directory(toolchain, "toolchain root")
  gcc_canonical <- plain_file_or_link(gcc, "GCC")
  clang_canonical <- plain_file_or_link(clang, "Clang")
  prefix <- paste0(toolchain, "/")
  if (!startsWith(gcc_canonical, prefix) || !startsWith(clang_canonical, prefix)) {
    fail("compiler drivers must resolve inside the repository-local toolchain")
  }
  ldd <- normalizePath("/usr/bin/ldd", winslash = "/", mustWork = TRUE)
  if (dir.exists(ldd) || nzchar(path_link(ldd))) fail("ldd must be a plain file")

  gcc_version <- run_tool(gcc, "--version", "GCC version")
  gcc_target <- run_tool(gcc, "-dumpmachine", "GCC target")
  gcc_dumpversion <- run_tool(gcc, "-dumpversion", "GCC dumpversion")
  gcc_search <- run_tool(gcc, "-print-search-dirs", "GCC search directories")
  gcc_specs <- run_tool(gcc, "-dumpspecs", "GCC specs")
  gcc_driver_plan <- run_tool(gcc, c("-###", "-E", "-x", "c", "/dev/null"),
    "GCC effective preprocessing plan")
  gcc_verbose_plan <- run_tool(gcc, c("-v", "-E", "-x", "c", "/dev/null"),
    "GCC verbose preprocessing plan")
  gcc_include_search <- parse_include_search(gcc_verbose_plan,
    "GCC verbose preprocessing plan")
  gcc_cc1_raw <- trimws(run_tool(gcc, "-print-prog-name=cc1", "GCC cc1 path"))
  gcc_include_raw <- trimws(run_tool(gcc, "-print-file-name=include",
    "GCC builtin include path"))
  gcc_include_fixed_raw <- trimws(run_tool(gcc,
    "-print-file-name=include-fixed", "GCC fixed include path"))
  gcc_sysroot_raw <- trimws(run_tool(gcc, "-print-sysroot", "GCC sysroot"))
  gcc_cc1 <- plain_file_or_link(gcc_cc1_raw, "GCC cc1")
  gcc_include <- plain_directory(gcc_include_raw, "GCC builtin include tree")
  gcc_include_fixed <- plain_directory(gcc_include_fixed_raw,
    "GCC fixed include tree")
  gcc_sysroot <- plain_directory(gcc_sysroot_raw, "compiler sysroot")
  sysroot_include <- plain_directory(file.path(gcc_sysroot, "usr", "include"),
    "compiler sysroot include tree")
  toolchain_include <- plain_directory(file.path(toolchain, "include"),
    "explicit toolchain include tree")
  if (!all(startsWith(c(gcc_cc1, gcc_include, gcc_include_fixed, gcc_sysroot,
      sysroot_include, toolchain_include), prefix))) {
    fail("GCC closure escaped the repository-local toolchain")
  }

  clang_version <- run_tool(clang, "--version", "Clang version")
  clang_target <- run_tool(clang, "-dumpmachine", "Clang target")
  clang_search <- run_tool(clang, "-print-search-dirs", "Clang search directories")
  clang_driver_plan <- run_tool(clang,
    c("-###", "-E", "-x", "c", "/dev/null"),
    "Clang effective preprocessing plan")
  clang_verbose_plan <- run_tool(clang,
    c("-v", "-E", "-x", "c", "/dev/null"),
    "Clang verbose preprocessing plan")
  clang_include_search <- parse_include_search(clang_verbose_plan,
    "Clang verbose preprocessing plan")
  clang_resource_raw <- trimws(run_tool(clang, "-print-resource-dir",
    "Clang resource directory"))
  clang_resource <- plain_directory(clang_resource_raw, "Clang resource tree")
  clang_config_path <- paste0(clang, ".cfg")
  if (!file.exists(clang_config_path) || dir.exists(clang_config_path) ||
      nzchar(path_link(clang_config_path))) {
    fail("Clang target configuration is absent, non-file, or symbolic: ",
      clang_config_path)
  }
  clang_config <- normalizePath(clang_config_path, winslash = "/",
    mustWork = TRUE)
  if (!all(startsWith(c(clang_resource, clang_config), prefix))) {
    fail("Clang resource tree escaped the repository-local toolchain")
  }

  effective_include_roots <- unique(c(
    gcc_include_search$active, clang_include_search$active,
    toolchain_include
  ))
  if (any(!startsWith(effective_include_roots, prefix))) {
    fail("an effective compiler include tree escaped the local toolchain")
  }
  reviewed_include_roots <- unique(c(
    gcc_include, gcc_include_fixed, sysroot_include, toolchain_include,
    clang_resource
  ))
  additional_include_roots <- setdiff(effective_include_roots,
    reviewed_include_roots)

  value_rows <- rbind(
    row_value("platform", paste(Sys.info()[c("sysname", "machine")],
      collapse = "/")),
    row_value("gcc_version", gcc_version),
    row_value("gcc_target", gcc_target),
    row_value("gcc_dumpversion", gcc_dumpversion),
    row_value("gcc_search_directories", gcc_search),
    row_value("gcc_specs", gcc_specs),
    row_value("gcc_effective_preprocessing_plan", gcc_driver_plan),
    row_value("gcc_verbose_preprocessing_plan", gcc_verbose_plan),
    row_value("gcc_effective_include_paths",
      include_path_inventory(gcc_include_search$active_raw)),
    row_value("gcc_absent_include_candidates", absent_include_inventory(
      gcc_include_search$absent_raw, "GCC include candidate")),
    row_value("gcc_cc1_path", gcc_cc1),
    row_value("gcc_builtin_include_path", gcc_include),
    row_value("gcc_fixed_include_path", gcc_include_fixed),
    row_value("compiler_sysroot_path", gcc_sysroot),
    row_value("compiler_sysroot_include_path", sysroot_include),
    row_value("explicit_toolchain_include_path", toolchain_include),
    row_value("clang_version", clang_version),
    row_value("clang_target", clang_target),
    row_value("clang_search_directories", clang_search),
    row_value("clang_effective_preprocessing_plan", clang_driver_plan),
    row_value("clang_verbose_preprocessing_plan", clang_verbose_plan),
    row_value("clang_effective_include_paths",
      include_path_inventory(clang_include_search$active_raw)),
    row_value("clang_absent_include_candidates", absent_include_inventory(
      clang_include_search$absent_raw, "Clang include candidate")),
    row_value("clang_target_config_path", clang_config),
    row_value("clang_resource_path", clang_resource)
  )

  ldd_targets <- c(gcc_canonical, gcc_cc1, clang_canonical)
  dependency_rows <- list()
  ldd_rows <- list(row_file("ldd_tool", ldd))
  for (index in seq_along(ldd_targets)) {
    label <- c("gcc", "gcc_cc1", "clang")[[index]]
    output <- run_tool(ldd, ldd_targets[[index]], paste0("ldd ", label))
    dependencies <- parse_ldd(output, paste0("ldd ", label))
    ldd_rows[[length(ldd_rows) + 1L]] <- row_value(
      paste0("ldd_", label, "_dependencies"),
      paste(dependencies, collapse = "\n")
    )
    for (dependency in dependencies) {
      dependency_rows[[length(dependency_rows) + 1L]] <-
        row_file("dynamic_dependency", dependency)
    }
  }

  result <- rbind(
    row_invocation("gcc", gcc),
    row_invocation("clang", clang),
    row_file("gcc_cc1", gcc_cc1),
    row_file("clang_target_config", clang_config),
    value_rows,
    do.call(rbind, ldd_rows),
    if (length(dependency_rows)) do.call(rbind, dependency_rows) else empty_manifest(),
    collect_tree(gcc_include, "gcc_builtin_tree"),
    collect_tree(gcc_include_fixed, "gcc_fixed_tree"),
    collect_tree(sysroot_include, "compiler_sysroot_include_tree"),
    collect_tree(toolchain_include, "explicit_toolchain_include_tree"),
    collect_tree(clang_resource, "clang_resource_tree"),
    if (length(additional_include_roots)) {
      do.call(rbind, lapply(additional_include_roots, collect_tree,
        role = "additional_effective_include_tree"))
    } else empty_manifest()
  )
  # The same library can appear in several ldd closures.  Its content identity
  # is independent of which driver first discovered it.
  identity_keys <- paste(result$kind, result$role, result$path, sep = "\t")
  duplicate_keys <- unique(identity_keys[duplicated(identity_keys)])
  for (key in duplicate_keys) {
    rows <- which(identity_keys == key)
    if (any(vapply(result, function(column) {
      any(column[rows] != column[rows[[1L]]])
    }, logical(1L)))) {
      fail("compiler closure changed while duplicate identity rows were read")
    }
  }
  result <- result[!duplicated(identity_keys), , drop = FALSE]
  ordering <- do.call(order, c(result[c("kind", "role", "path", "target")],
    list(method = "radix")))
  result <- result[ordering, , drop = FALSE]
  row.names(result) <- NULL
  validate_manifest(result)
  result
}

write_manifest <- function(value, path) {
  if (file.exists(path) || dir.exists(path) || nzchar(path_link(path))) {
    fail("compiler identity output already exists: ", path)
  }
  parent <- dirname(path)
  if (!dir.exists(parent) || nzchar(path_link(parent))) {
    fail("compiler identity output parent is absent or symbolic")
  }
  temporary <- tempfile("compiler-identity-", tmpdir = parent)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  write.table(value, temporary, sep = "\t", quote = TRUE, row.names = FALSE,
    col.names = TRUE, na = "", fileEncoding = "UTF-8")
  if (!file.rename(temporary, path)) fail("could not publish compiler identity")
  invisible(path)
}

read_manifest <- function(path) {
  if (!file.exists(path) || dir.exists(path) || nzchar(path_link(path))) {
    fail("compiler identity receipt is absent or symbolic")
  }
  value <- read.delim(path, header = TRUE, sep = "\t", quote = "\"",
    colClasses = "character", check.names = FALSE, na.strings = character(),
    stringsAsFactors = FALSE)
  validate_manifest(value)
  value
}

self_test <- function() {
  root <- tempfile("compiler-identity-self-test-")
  dir.create(root, mode = "0700")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  tree <- file.path(root, "tree")
  dir.create(tree)
  writeBin(charToRaw("alpha\n"), file.path(tree, "alpha.h"))
  dir.create(file.path(tree, "nested"))
  writeBin(charToRaw("beta\n"), file.path(tree, "nested", "beta.h"))
  value <- rbind(row_value("synthetic_value", "one\ntwo"),
    collect_tree(tree, "synthetic_tree"))
  ordering <- do.call(order, c(value[c("kind", "role", "path", "target")],
    list(method = "radix")))
  value <- value[ordering, , drop = FALSE]
  row.names(value) <- NULL
  receipt <- file.path(root, "receipt.tsv")
  write_manifest(value, receipt)
  retained <- read_manifest(receipt)
  if (!identical(value, retained)) fail("synthetic receipt did not round trip")
  writeBin(charToRaw("changed\n"), file.path(tree, "alpha.h"))
  changed <- rbind(row_value("synthetic_value", "one\ntwo"),
    collect_tree(tree, "synthetic_tree"))
  changed <- changed[do.call(order, c(
    changed[c("kind", "role", "path", "target")], list(method = "radix")
  )), , drop = FALSE]
  row.names(changed) <- NULL
  if (identical(changed, retained)) fail("synthetic content mutation was accepted")
  missing <- file.path(tree, "future-default")
  absent_verbose <- paste(
    paste0("ignoring nonexistent directory \\\"", missing, "\\\""),
    "#include <...> search starts here:", paste0(" ", tree),
    "End of search list.", sep = "\n"
  )
  absent_search <- parse_include_search(absent_verbose,
    "synthetic absent search")
  absent_state <- rbind(
    row_value("synthetic_active_paths",
      include_path_inventory(absent_search$active_raw)),
    row_value("synthetic_absent_paths", absent_include_inventory(
      absent_search$absent_raw, "synthetic include candidate"))
  )
  dir.create(missing)
  writeBin(charToRaw("future\n"), file.path(missing, "future.h"))
  present_verbose <- paste(
    "#include <...> search starts here:", paste0(" ", tree),
    paste0(" ", missing), "End of search list.", sep = "\n"
  )
  present_search <- parse_include_search(present_verbose,
    "synthetic present search")
  present_state <- rbind(
    row_value("synthetic_active_paths",
      include_path_inventory(present_search$active_raw)),
    row_value("synthetic_absent_paths", absent_include_inventory(
      present_search$absent_raw, "synthetic include candidate")),
    collect_tree(missing, "synthetic_new_default_tree")
  )
  if (identical(absent_state, present_state)) {
    fail("synthetic absent-to-present include transition was accepted")
  }
  cat("compiler_identity_self_test=passed\n")
  cat("synthetic_mutation_rejected=true\n")
  cat("synthetic_absence_transition_rejected=true\n")
}

operation <- args[[1L]]
if (identical(operation, "self-test")) {
  if (length(args) != 1L) fail(usage)
  self_test()
  quit(save = "no", status = 0L)
}
if (length(args) != 5L) fail(usage)
gcc <- args[[2L]]
clang <- args[[3L]]
toolchain <- args[[4L]]
receipt <- normalizePath(args[[5L]], winslash = "/", mustWork = FALSE)

if (identical(operation, "create")) {
  write_manifest(discover_identity(gcc, clang, toolchain), receipt)
  cat("compiler_identity_created=", receipt, "\n", sep = "")
  quit(save = "no", status = 0L)
}

expected <- read_manifest(receipt)
observed <- discover_identity(gcc, clang, toolchain)
if (!identical(observed, expected)) {
  expected_keys <- paste(expected$kind, expected$role, expected$path, sep = "\t")
  observed_keys <- paste(observed$kind, observed$role, observed$path, sep = "\t")
  missing <- setdiff(expected_keys, observed_keys)
  added <- setdiff(observed_keys, expected_keys)
  common <- intersect(expected_keys, observed_keys)
  changed <- common[vapply(common, function(key) {
    !identical(
      expected[match(key, expected_keys), , drop = FALSE],
      observed[match(key, observed_keys), , drop = FALSE]
    )
  }, logical(1L))]
  fail(
    "compiler closure does not match its receipt; missing=", length(missing),
    "; added=", length(added), "; changed=", length(changed)
  )
}
cat("compiler_identity_verified=", receipt, "\n", sep = "")
cat("compiler_identity_rows=", nrow(expected), "\n", sep = "")
