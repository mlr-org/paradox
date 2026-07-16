repository_runner_fail <- function(...) stop(..., call. = FALSE)

repository_runner_is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

repository_runner_require_directory <- function(path, label = "directory") {
  if (!dir.exists(path) || repository_runner_is_symbolic(path)) {
    repository_runner_fail(label, " is missing, not a directory, or symbolic: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

repository_runner_require_file <- function(path, label = "file") {
  if (!file.exists(path) || dir.exists(path) || repository_runner_is_symbolic(path)) {
    repository_runner_fail(label, " is missing, not a regular file, or symbolic: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

repository_runner_safe_name <- function(value, label = "name") {
  if (length(value) != 1L || is.na(value) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", value) ||
      value %in% c(".", "..")) {
    repository_runner_fail(label, " must be one safe name of at most 128 characters")
  }
  value
}

repository_runner_safe_relative <- function(path) {
  length(path) == 1L && !is.na(path) && nzchar(path) &&
    !startsWith(path, "/") && !startsWith(path, "\\") &&
    !grepl("(^|/)[.][.]?(/|$)", path, perl = TRUE) &&
    !grepl("\r", path, fixed = TRUE) && !grepl("\n", path, fixed = TRUE) &&
    !grepl("\t", path, fixed = TRUE) && !grepl("\\", path, fixed = TRUE)
}

repository_runner_sha256 <- function(path) unname(tools::sha256sum(path))

repository_runner_tempfile <- function(pattern, fileext = "") {
  directory <- getOption("paradox.repository_runner_tempdir", tempdir())
  directory <- repository_runner_require_directory(directory,
    "repository runner temporary directory")
  tempfile(pattern, tmpdir = directory, fileext = fileext)
}

repository_runner_object_sha256 <- function(value, prefix = "repository-runner-") {
  path <- repository_runner_tempfile(prefix)
  connection <- file(path, open = "wb")
  closed <- FALSE
  on.exit({
    if (!closed) close(connection)
    unlink(path)
  }, add = TRUE)
  emit <- function(text) {
    if (length(text) != 1L || is.na(text)) {
      repository_runner_fail("canonical hash encountered an invalid token")
    }
    text <- as.character(text)
    bytes <- charToRaw(enc2utf8(text))
    writeBin(as.integer(length(bytes)), connection, size = 4L,
      endian = "big")
    if (length(bytes)) writeBin(bytes, connection)
  }
  emit_names <- function(names_value, length_value) {
    if (is.null(names_value)) {
      emit("names:null")
    } else {
      if (length(names_value) != length_value || anyNA(names_value)) {
        repository_runner_fail("canonical hash encountered invalid names")
      }
      emit("names")
      for (name in names_value) emit(name)
    }
  }
  walk <- function(object) {
    if (is.data.frame(object)) {
      emit("data.frame")
      emit(as.character(nrow(object)))
      emit(as.character(ncol(object)))
      emit_names(names(object), ncol(object))
      emit("row.names")
      for (name in row.names(object)) emit(name)
      for (column in object) walk(column)
      return(invisible(NULL))
    }
    if (is.list(object)) {
      emit("list")
      emit(as.character(length(object)))
      emit_names(names(object), length(object))
      for (element in object) walk(element)
      return(invisible(NULL))
    }
    type <- typeof(object)
    if (!type %in% c("character", "integer", "double", "logical", "raw")) {
      repository_runner_fail("canonical hash does not support type ", type)
    }
    emit(type)
    emit(as.character(length(object)))
    emit_names(names(object), length(object))
    for (index in seq_along(object)) {
      if (identical(type, "double") && is.nan(object[[index]])) {
        emit("NaN")
      } else if (is.na(object[[index]])) {
        emit("NA")
      } else if (identical(type, "double")) {
        emit(sprintf("%a", object[[index]]))
      } else if (identical(type, "raw")) {
        emit(sprintf("%02x", as.integer(object[[index]])))
      } else {
        emit(as.character(object[[index]]))
      }
    }
    invisible(NULL)
  }
  walk(value)
  close(connection)
  closed <- TRUE
  repository_runner_sha256(path)
}

repository_runner_map_frame <- function(value) data.frame(
  field = names(value), value = unname(value), stringsAsFactors = FALSE
)

repository_runner_read_tsv <- function(path, columns, allow_empty = FALSE,
                                       label = "TSV") {
  repository_runner_require_file(path, label)
  value <- utils::read.delim(
    path, header = TRUE, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!identical(names(value), columns) || anyNA(value) ||
      (!allow_empty && !nrow(value)) ||
      (nrow(value) && any(vapply(value, function(column) any(!nzchar(column)),
        logical(1L))))) {
    repository_runner_fail(label, " has an unexpected schema or empty value: ", path)
  }
  value
}

repository_runner_read_map <- function(path, fields = NULL, label = "metadata") {
  value <- repository_runner_read_tsv(path, c("field", "value"), label = label)
  if (anyDuplicated(value$field) ||
      (!is.null(fields) && !identical(value$field, fields))) {
    repository_runner_fail(label, " has duplicate, missing, or reordered fields")
  }
  stats::setNames(value$value, value$field)
}

repository_runner_write_tsv <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(path) || dir.exists(path) || repository_runner_is_symbolic(path) ||
      file.exists(temporary) || dir.exists(temporary) ||
      repository_runner_is_symbolic(temporary)) {
    repository_runner_fail("refusing to overwrite append-only evidence: ", path)
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.table(value, temporary, quote = FALSE, sep = "\t",
    row.names = FALSE, na = "-", fileEncoding = "UTF-8")
  if (!file.rename(temporary, path)) {
    repository_runner_fail("could not atomically write evidence: ", path)
  }
  invisible(path)
}

repository_runner_write_lines <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(path) || dir.exists(path) || repository_runner_is_symbolic(path) ||
      file.exists(temporary) || dir.exists(temporary) ||
      repository_runner_is_symbolic(temporary)) {
    repository_runner_fail("refusing to overwrite append-only evidence: ", path)
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(value, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) {
    repository_runner_fail("could not atomically write evidence: ", path)
  }
  invisible(path)
}

repository_runner_reserve_directory <- function(parent, name, label = "directory") {
  parent <- repository_runner_require_directory(parent, paste0(label, " parent"))
  repository_runner_safe_name(name, label)
  path <- file.path(parent, name)
  if (file.exists(path) || dir.exists(path) || repository_runner_is_symbolic(path) ||
      !dir.create(path, recursive = FALSE, showWarnings = FALSE)) {
    repository_runner_fail(label, " already exists or cannot be reserved: ", path)
  }
  path <- repository_runner_require_directory(path, label)
  if (!identical(dirname(path), parent)) repository_runner_fail(label, " escaped parent")
  path
}

# Git archives preserve symbolic links.  Admit only the narrow form used by
# reviewed consumer sources: one relative link directly to a regular file in
# the same authenticated tree.  Directory links, chains, broken links, dot
# components, and escaping targets remain forbidden.
repository_runner_symlink_target <- function(path, root, label = "source tree") {
  root <- repository_runner_require_directory(root, paste0(label, " root"))
  target <- Sys.readlink(path)
  if (length(target) != 1L || is.na(target) ||
      !repository_runner_safe_relative(target)) {
    repository_runner_fail(label, " contains an unsafe symbolic-link target")
  }
  target_path <- file.path(dirname(path), target)
  if (!file.exists(target_path) || dir.exists(target_path) ||
      repository_runner_is_symbolic(target_path)) {
    repository_runner_fail(
      label, " symbolic links must directly target one regular file"
    )
  }
  resolved <- normalizePath(target_path, winslash = "/", mustWork = TRUE)
  if (!startsWith(resolved, paste0(root, "/"))) {
    repository_runner_fail(label, " contains an escaping symbolic link")
  }
  target
}

repository_runner_entries <- function(path, label = "tree",
                                      allow_symlinks = FALSE) {
  path <- repository_runner_require_directory(path, paste0(label, " root"))
  if (!requireNamespace("fs", quietly = TRUE)) {
    repository_runner_fail("fs is required for non-following tree inspection")
  }
  paths <- as.character(fs::dir_ls(path, all = TRUE, recurse = TRUE,
    type = "any", fail = TRUE))
  if (!length(paths)) {
    return(data.frame(
      absolute = character(), path = character(), type = character(),
      mode = character(), size = character(), mtime = numeric(), ctime = numeric(),
      device = numeric(), inode = numeric(), hard_links = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  info <- fs::file_info(paths)
  type <- as.character(info$type)
  allowed_types <- c("file", "directory", if (allow_symlinks) "symlink")
  if (anyNA(type) || any(!type %in% allowed_types)) {
    repository_runner_fail(label, " contains a symbolic or special path")
  }
  relative <- substring(paths, nchar(path, type = "chars") + 2L)
  if (any(!vapply(relative, repository_runner_safe_relative, logical(1L))) ||
      anyDuplicated(relative)) {
    repository_runner_fail(label, " contains an unsafe or duplicated path")
  }
  order <- order(relative, method = "radix")
  paths <- paths[order]
  relative <- relative[order]
  info <- info[order, , drop = FALSE]
  type <- type[order]
  leaves <- type %in% c("file", "symlink")
  size <- rep("-", length(paths))
  size[leaves] <- format(
    as.numeric(info$size[leaves]), scientific = FALSE, trim = TRUE
  )
  data.frame(
    absolute = paths, path = relative, type = type,
    mode = sprintf("%04o", as.integer(info$permissions)), size = size,
    mtime = as.numeric(info$modification_time),
    ctime = as.numeric(info$change_time), device = as.numeric(info$device_id),
    inode = as.numeric(info$inode), hard_links = as.numeric(info$hard_links),
    stringsAsFactors = FALSE
  )
}

repository_runner_tree_manifest <- function(path) {
  path <- repository_runner_require_directory(path, "source tree root")
  entries <- repository_runner_entries(path, "source tree", allow_symlinks = TRUE)
  files <- entries$type == "file"
  links <- entries$type == "symlink"
  sha256 <- rep("-", nrow(entries))
  if (any(files)) sha256[files] <- repository_runner_sha256(entries$absolute[files])
  if (any(links)) {
    targets <- vapply(
      entries$absolute[links], repository_runner_symlink_target,
      character(1L), root = path, label = "source tree"
    )
    sha256[links] <- vapply(
      targets, repository_runner_object_sha256, character(1L),
      prefix = "repository-runner-symlink-"
    )
  }
  data.frame(path = entries$path, type = entries$type, mode = entries$mode,
    size = entries$size, sha256 = sha256, stringsAsFactors = FALSE)
}

repository_runner_file_manifest <- function(path, exclude_git = FALSE) {
  path <- repository_runner_require_directory(path, "source manifest root")
  entries <- repository_runner_entries(
    path, "source manifest", allow_symlinks = TRUE
  )
  if (exclude_git) {
    entries <- entries[entries$path != ".git" & !startsWith(entries$path, ".git/"),
      , drop = FALSE]
  }
  entries <- entries[entries$type %in% c("file", "symlink"), , drop = FALSE]
  modes <- strtoi(entries$mode, base = 8L)
  if (anyNA(modes)) repository_runner_fail("file manifest contains an invalid mode")
  files <- entries$type == "file"
  links <- entries$type == "symlink"
  sha256 <- character(nrow(entries))
  if (any(files)) sha256[files] <- repository_runner_sha256(entries$absolute[files])
  if (any(links)) {
    targets <- vapply(
      entries$absolute[links], repository_runner_symlink_target,
      character(1L), root = path, label = "source manifest"
    )
    sha256[links] <- vapply(
      targets, repository_runner_object_sha256, character(1L),
      prefix = "repository-runner-symlink-"
    )
  }
  data.frame(
    path = entries$path, size = entries$size,
    executable = ifelse(files & bitwAnd(modes, 64L) != 0L, "true", "false"),
    sha256 = sha256,
    stringsAsFactors = FALSE
  )
}

repository_runner_metadata_sha256 <- function(path) {
  entries <- repository_runner_entries(path, "protected metadata tree")
  repository_runner_object_sha256(entries[, c(
    "path", "type", "mode", "size", "mtime", "ctime", "device", "inode",
    "hard_links"
  ), drop = FALSE], "repository-runner-metadata-")
}

repository_runner_inventory <- function(path) {
  entries <- repository_runner_entries(path, "scoped evidence")
  entries <- entries[!entries$path %in% c(
    ".repository-runner-manifest.tsv", ".repository-runner-seal"
  ), , drop = FALSE]
  files <- entries$type == "file"
  sha256 <- rep("-", nrow(entries))
  if (any(files)) sha256[files] <- repository_runner_sha256(entries$absolute[files])
  data.frame(path = entries$path, type = entries$type, size = entries$size,
    sha256 = sha256, stringsAsFactors = FALSE)
}

repository_runner_seal_directory <- function(path) {
  path <- repository_runner_require_directory(path, "directory to seal")
  inventory <- repository_runner_inventory(path)
  if (!nrow(inventory)) repository_runner_fail("cannot seal empty evidence")
  manifest <- file.path(path, ".repository-runner-manifest.tsv")
  seal <- file.path(path, ".repository-runner-seal")
  repository_runner_write_tsv(inventory, manifest)
  repository_runner_write_lines(
    paste0("manifest_sha256=", repository_runner_sha256(manifest)), seal)
  repository_runner_verify_sealed_directory(path)
}

repository_runner_verify_sealed_directory <- function(path) {
  path <- repository_runner_require_directory(path, "sealed evidence")
  manifest <- repository_runner_require_file(file.path(path,
    ".repository-runner-manifest.tsv"), "scoped evidence manifest")
  seal <- repository_runner_require_file(file.path(path,
    ".repository-runner-seal"), "scoped evidence seal")
  if (!identical(readLines(seal, warn = FALSE),
      paste0("manifest_sha256=", repository_runner_sha256(manifest)))) {
    repository_runner_fail("scoped evidence seal is malformed")
  }
  expected <- repository_runner_read_tsv(manifest,
    c("path", "type", "size", "sha256"), label = "scoped evidence manifest")
  if (anyDuplicated(expected$path) ||
      !identical(expected$path, sort(expected$path, method = "radix")) ||
      any(!vapply(expected$path, repository_runner_safe_relative, logical(1L))) ||
      any(!expected$type %in% c("file", "directory")) ||
      any(expected$type == "directory" & (expected$size != "-" |
        expected$sha256 != "-")) ||
      any(expected$type == "file" & (!grepl("^(0|[1-9][0-9]*)$", expected$size) |
        !grepl("^[0-9a-f]{64}$", expected$sha256)))) {
    repository_runner_fail("scoped evidence manifest is malformed")
  }
  observed <- repository_runner_inventory(path)
  if (!identical(expected, observed)) {
    repository_runner_fail("scoped evidence differs from its manifest")
  }
  list(path = path, manifest_sha256 = repository_runner_sha256(manifest),
    seal_sha256 = repository_runner_sha256(seal))
}

repository_runner_assert_git_environment <- function() {
  names <- grep("^GIT_", names(Sys.getenv()), value = TRUE)
  forbidden <- names[names != "GIT_PAGER" & nzchar(Sys.getenv(names))]
  if (length(forbidden)) {
    repository_runner_fail("repository-altering Git environment is forbidden: ",
      paste(forbidden, collapse = ", "))
  }
  invisible(TRUE)
}

repository_runner_git_environment <- function() c(
  GIT_CONFIG_GLOBAL = "/dev/null", GIT_CONFIG_SYSTEM = "/dev/null",
  GIT_CONFIG_NOSYSTEM = "1", GIT_ATTR_NOSYSTEM = "1",
  GIT_NO_REPLACE_OBJECTS = "1", GIT_OPTIONAL_LOCKS = "0",
  GIT_PAGER = "cat", LC_ALL = "C", TZ = "UTC"
)

repository_runner_git_prefix <- function() c(
  "--no-replace-objects", "-c", "core.attributesFile=/dev/null",
  "-c", "core.fsmonitor=false", "-c", "core.untrackedCache=false",
  "-c", "core.quotePath=false", "-c", "tar.umask=0022"
)

repository_runner_git_run <- function(git, repository, arguments, label = "Git",
                                      stdout = "|", stdin = NULL,
                                      allow_status = 0L) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    repository_runner_fail("processx is required for repository validation")
  }
  result <- processx::run(git, c(repository_runner_git_prefix(), arguments),
    wd = repository, env = repository_runner_git_environment(), stdout = stdout,
    stderr = "|", stdin = stdin, error_on_status = FALSE, cleanup_tree = TRUE,
    windows_verbatim_args = TRUE)
  if (!result$status %in% allow_status) {
    detail <- paste(c(result$stdout, result$stderr), collapse = "\n")
    repository_runner_fail(label, " failed", if (nzchar(detail)) paste0(": ", detail) else "")
  }
  result
}

repository_runner_git_value <- function(git, repository, arguments, label) {
  value <- trimws(repository_runner_git_run(git, repository, arguments, label)$stdout)
  if (length(value) != 1L || !nzchar(value) || grepl("[\r\n\t]", value)) {
    repository_runner_fail(label, " returned an unexpected value")
  }
  value
}

repository_runner_forbid_git_path <- function(path, label) {
  if (file.exists(path) || dir.exists(path) || repository_runner_is_symbolic(path)) {
    repository_runner_fail(label, " is forbidden: ", path)
  }
  invisible(TRUE)
}

repository_runner_audit_git_common <- function(git, repository, common, worktree_git) {
  replacements <- trimws(repository_runner_git_run(git, repository,
    c("for-each-ref", "--format=%(refname)", "refs/replace"),
    "auditing replacement refs")$stdout)
  if (nzchar(replacements)) repository_runner_fail("Git replacement refs are forbidden")
  for (path in unique(c(
      file.path(common, "info", c("grafts", "attributes")),
      file.path(common, "objects", "info", c("alternates", "http-alternates")),
      file.path(worktree_git, "info", c("grafts", "attributes")),
      file.path(worktree_git, "objects", "info", c("alternates", "http-alternates"))))) {
    repository_runner_forbid_git_path(path, "Git graft, attribute, or alternate input")
  }
  dangerous <- repository_runner_git_run(git, repository,
    c("config", "--local", "--get-regexp",
      "^(include[.]|includeif[.]|core[.]attributesfile$|core[.]hookspath$|core[.]worktree$|extensions[.]worktreeconfig$|extensions[.]partialclone$|remote[.].*[.]promisor$)"),
    "auditing repository configuration", allow_status = c(0L, 1L))
  if (identical(dangerous$status, 0L) && nzchar(trimws(dangerous$stdout))) {
    repository_runner_fail("repository configuration redirects authenticated Git inputs")
  }
  flags <- repository_runner_git_run(git, repository, c("ls-files", "-v"),
    "auditing index flags")$stdout
  flags <- strsplit(flags, "\n", fixed = TRUE)[[1L]]
  flags <- substring(flags[nzchar(flags)], 1L, 1L)
  if (any(grepl("^[a-zS]$", flags))) {
    repository_runner_fail("Git index hides tracked state with flag ",
      paste(unique(flags[grepl("^[a-zS]$", flags)]), collapse = ","))
  }
  invisible(TRUE)
}

repository_runner_authenticate_candidate <- function(config) {
  repository_runner_assert_git_environment()
  git <- repository_runner_require_file(config$git, "repository-local Git")
  root <- repository_runner_require_directory(config$root, "repository root")
  source <- repository_runner_require_directory(config$candidate_source,
    "detached candidate source")
  common <- repository_runner_require_directory(file.path(root, ".git"),
    "candidate common Git directory")
  repository_runner_require_file(file.path(source, ".git"),
    "detached candidate Git marker")
  top <- normalizePath(repository_runner_git_value(git, source,
    c("rev-parse", "--show-toplevel"), "resolving candidate source root"),
    winslash = "/", mustWork = TRUE)
  git_dir <- normalizePath(repository_runner_git_value(git, source,
    c("rev-parse", "--absolute-git-dir"), "resolving candidate Git directory"),
    winslash = "/", mustWork = TRUE)
  observed_common <- normalizePath(repository_runner_git_value(git, source,
    c("rev-parse", "--path-format=absolute", "--git-common-dir"),
    "resolving candidate common directory"), winslash = "/", mustWork = TRUE)
  repository_runner_require_directory(git_dir, "candidate worktree Git directory")
  if (!identical(top, source) || !identical(observed_common, common) ||
      identical(git_dir, common) || !startsWith(git_dir, paste0(common, "/worktrees/"))) {
    repository_runner_fail("candidate source is not the expected linked worktree")
  }
  repository_runner_audit_git_common(git, source, common, git_dir)
  symbolic <- repository_runner_git_run(git, source,
    c("symbolic-ref", "-q", "HEAD"), "checking detached candidate HEAD",
    allow_status = c(0L, 1L))
  if (!identical(symbolic$status, 1L) || nzchar(trimws(symbolic$stdout))) {
    repository_runner_fail("candidate source HEAD is not detached")
  }
  origin <- repository_runner_git_value(git, source,
    c("remote", "get-url", "origin"), "reading candidate origin")
  head <- repository_runner_git_value(git, source,
    c("rev-parse", "--verify", "HEAD^{commit}"), "resolving candidate HEAD")
  head_tree <- repository_runner_git_value(git, source,
    c("rev-parse", "--verify", "HEAD^{tree}"), "resolving candidate HEAD tree")
  ref_commit <- repository_runner_git_value(git, source,
    c("rev-parse", "--verify", paste0(config$candidate_ref, "^{commit}")),
    "resolving candidate ref")
  ref_tree <- repository_runner_git_value(git, source,
    c("rev-parse", "--verify", paste0(config$candidate_ref, "^{tree}")),
    "resolving candidate ref tree")
  commit_tree <- repository_runner_git_value(git, source,
    c("rev-parse", "--verify", paste0(config$candidate_commit, "^{tree}")),
    "resolving candidate commit tree")
  status <- repository_runner_git_run(git, source,
    c("status", "--porcelain=v1", "--untracked-files=all", "--ignored=matching"),
    "auditing candidate source cleanliness")$stdout
  tree_modes <- repository_runner_git_run(git, source,
    c("ls-tree", "-r", "--full-tree", config$candidate_commit),
    "auditing candidate tree modes")$stdout
  if (!identical(origin, config$candidate_origin) ||
      !identical(head, config$candidate_commit) ||
      !identical(head_tree, config$candidate_tree) ||
      !identical(ref_commit, config$candidate_commit) ||
      !identical(ref_tree, config$candidate_tree) ||
      !identical(commit_tree, config$candidate_tree) || nzchar(trimws(status)) ||
      grepl("(^|\n)(120000|160000) ", tree_modes, perl = TRUE)) {
    repository_runner_fail("candidate origin, ref, commit, tree, mode, or cleanliness differs")
  }
  output <- c(
    source = source, origin = origin, commit = head, tree = head_tree,
    ref = config$candidate_ref, git_directory = git_dir,
    common_directory = observed_common
  )
  list(git = git, checkout = source, commit = head, tree = head_tree,
    origin = origin, output = output,
    output_sha256 = repository_runner_object_sha256(output,
      "repository-runner-candidate-auth-"))
}

repository_runner_authenticate_consumer <- function(config, row) {
  git <- repository_runner_require_file(config$git, "repository-local Git")
  checkout <- repository_runner_require_directory(file.path(config$consumer_root,
    row$repository[[1L]]), paste0(row$repository[[1L]], " checkout"))
  git_dir <- repository_runner_require_directory(file.path(checkout, ".git"),
    "consumer Git directory")
  top <- normalizePath(repository_runner_git_value(git, checkout,
    c("rev-parse", "--show-toplevel"), "resolving consumer checkout"),
    winslash = "/", mustWork = TRUE)
  absolute_git <- normalizePath(repository_runner_git_value(git, checkout,
    c("rev-parse", "--absolute-git-dir"), "resolving consumer Git directory"),
    winslash = "/", mustWork = TRUE)
  if (!identical(top, checkout) || !identical(absolute_git, git_dir)) {
    repository_runner_fail("consumer checkout is not a plain primary worktree")
  }
  repository_runner_audit_git_common(git, checkout, git_dir, git_dir)
  origin <- repository_runner_git_value(git, checkout,
    c("remote", "get-url", "origin"), "reading consumer origin")
  head <- repository_runner_git_value(git, checkout,
    c("rev-parse", "--verify", "HEAD^{commit}"), "resolving consumer HEAD")
  commit <- repository_runner_git_value(git, checkout,
    c("rev-parse", "--verify", paste0(row$commit[[1L]], "^{commit}")),
    "resolving consumer commit")
  tree <- repository_runner_git_value(git, checkout,
    c("rev-parse", "--verify", paste0(row$commit[[1L]], "^{tree}")),
    "resolving consumer tree")
  if (!identical(origin, row$origin[[1L]]) || !identical(head, row$commit[[1L]]) ||
      !identical(commit, row$commit[[1L]]) || !identical(tree, row$tree[[1L]])) {
    repository_runner_fail("consumer origin, HEAD, commit, or tree differs: ",
      row$repository[[1L]])
  }
  list(git = git, checkout = checkout, origin = origin, commit = commit, tree = tree)
}

repository_runner_git_tree <- function(authentication) {
  output <- repository_runner_git_run(authentication$git, authentication$checkout,
    c("ls-tree", "-r", "--full-tree", authentication$commit),
    "reading pinned Git tree")$stdout
  lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  lines <- lines[nzchar(lines)]
  parsed <- lapply(lines, function(line) {
    pieces <- strsplit(line, "\t", fixed = TRUE)[[1L]]
    if (length(pieces) != 2L || !repository_runner_safe_relative(pieces[[2L]])) {
      repository_runner_fail("pinned Git tree contains an unsafe path")
    }
    header <- strsplit(pieces[[1L]], " ", fixed = TRUE)[[1L]]
    if (length(header) != 3L ||
        !header[[1L]] %in% c("100644", "100755", "120000") ||
        !identical(header[[2L]], "blob") ||
        !grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", header[[3L]])) {
      repository_runner_fail("pinned Git tree contains a non-regular entry")
    }
    c(mode = header[[1L]], object = header[[3L]], path = pieces[[2L]])
  })
  if (!length(parsed)) repository_runner_fail("pinned Git tree is empty")
  value <- as.data.frame(do.call(rbind, parsed), stringsAsFactors = FALSE)
  value <- value[order(value$path, method = "radix"), , drop = FALSE]
  row.names(value) <- NULL
  if (anyDuplicated(value$path)) repository_runner_fail("pinned Git tree duplicates a path")
  value
}

repository_runner_validate_extraction <- function(authentication, source) {
  source <- repository_runner_require_directory(source, "archive extraction")
  tree <- repository_runner_git_tree(authentication)
  manifest <- repository_runner_file_manifest(source)
  if (!identical(manifest$path, tree$path)) {
    repository_runner_fail("archive extraction paths differ from the pinned Git tree")
  }
  regular <- tree$mode != "120000"
  links <- !regular
  hashes <- character(nrow(tree))
  if (any(regular)) {
    input <- repository_runner_tempfile("repository-runner-hash-object-")
    on.exit(unlink(input), add = TRUE)
    # Absolute paths prevent an enclosing checkout (the retained evidence lives
    # below the project) from changing Git's stdin-paths prefix semantics.
    writeLines(file.path(source, manifest$path[regular]), input, useBytes = TRUE)
    regular_hashes <- repository_runner_git_run(authentication$git, source,
      c("hash-object", "--no-filters", "--stdin-paths"),
      "hashing archive files as Git blobs", stdin = input)$stdout
    hashes[regular] <- strsplit(
      trimws(regular_hashes), "\n", fixed = TRUE
    )[[1L]]
  }
  if (any(links)) {
    targets <- vapply(
      file.path(source, manifest$path[links]),
      repository_runner_symlink_target, character(1L), root = source,
      label = "archive extraction"
    )
    blobs <- vapply(which(links), function(index) {
      repository_runner_git_run(
        authentication$git, authentication$checkout,
        c("cat-file", "blob", tree$object[[index]]),
        "reading symbolic-link Git blob"
      )$stdout
    }, character(1L))
    if (!identical(unname(blobs), unname(targets))) {
      repository_runner_fail("archive symbolic links differ from the Git tree")
    }
    hashes[links] <- tree$object[links]
  }
  executable <- ifelse(tree$mode == "100755", "true", "false")
  if (!identical(hashes, tree$object) || !identical(manifest$executable, executable)) {
    repository_runner_fail("archive bytes or executable modes differ from the Git tree")
  }
  cbind(tree, size = manifest$size, sha256 = manifest$sha256,
    stringsAsFactors = FALSE)
}

repository_runner_archive_entries <- function(archive) {
  archive <- repository_runner_require_file(archive, "source archive")
  entries <- suppressWarnings(utils::untar(archive, list = TRUE, tar = "internal"))
  safe <- vapply(entries, function(entry) {
    normalized <- sub("/$", "", entry)
    identical(normalized, "source") || (startsWith(entry, "source/") &&
      repository_runner_safe_relative(substring(entry, 8L)))
  }, logical(1L))
  if (!length(entries) || anyNA(entries) || any(!safe) || anyDuplicated(entries)) {
    repository_runner_fail("source archive contains an unsafe or duplicate entry")
  }
  entries
}

repository_runner_create_archive <- function(authentication, archive) {
  if (file.exists(archive) || dir.exists(archive) || repository_runner_is_symbolic(archive)) {
    repository_runner_fail("archive output already exists: ", archive)
  }
  repository_runner_git_run(authentication$git, authentication$checkout,
    c("archive", "--format=tar", "--prefix=source/", authentication$commit),
    "creating deterministic source archive", stdout = archive)
  repository_runner_require_file(archive, "source archive")
  entries <- repository_runner_archive_entries(archive)
  list(path = normalizePath(archive, winslash = "/", mustWork = TRUE),
    sha256 = repository_runner_sha256(archive), entries = entries)
}

repository_runner_extract_archive <- function(archive, parent, name) {
  output <- repository_runner_reserve_directory(parent, name, "archive extraction root")
  suppressWarnings(utils::untar(archive, exdir = output, tar = "internal"))
  source <- repository_runner_require_directory(file.path(output, "source"),
    "archive source extraction")
  list(root = output, source = source)
}

repository_runner_candidate_proof <- function(authentication, output) {
  archive <- repository_runner_create_archive(authentication,
    file.path(output, "candidate-source.tar"))
  temporary <- tempfile("candidate-proof-", tmpdir = dirname(output))
  if (!dir.create(temporary, recursive = FALSE, showWarnings = FALSE)) {
    repository_runner_fail("could not reserve candidate proof extraction")
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  suppressWarnings(utils::untar(archive$path, exdir = temporary, tar = "internal"))
  extracted <- repository_runner_require_directory(file.path(temporary, "source"),
    "candidate proof extraction")
  tree <- repository_runner_validate_extraction(authentication, extracted)
  worktree <- repository_runner_file_manifest(authentication$checkout,
    exclude_git = TRUE)
  extracted_manifest <- repository_runner_file_manifest(extracted)
  if (!identical(worktree, extracted_manifest)) {
    repository_runner_fail("detached candidate bytes differ from its Git archive")
  }
  repository_runner_write_tsv(data.frame(path = archive$entries,
    stringsAsFactors = FALSE), file.path(output, "candidate-archive-entries.tsv"))
  repository_runner_write_tsv(tree, file.path(output, "candidate-tree.tsv"))
  repository_runner_write_tsv(worktree,
    file.path(output, "candidate-source-manifest.tsv"))
  list(archive_sha256 = archive$sha256,
    tree_sha256 = repository_runner_sha256(file.path(output, "candidate-tree.tsv")),
    manifest_sha256 = repository_runner_sha256(file.path(output,
      "candidate-source-manifest.tsv")))
}

repository_runner_selection <- function(value) {
  columns <- c("position", "repository", "priority", "origin", "commit", "tree")
  if (!identical(names(value), columns) || !nrow(value) || anyNA(value)) {
    repository_runner_fail("repository selection has an unexpected schema")
  }
  position <- suppressWarnings(as.integer(value$position))
  priority <- suppressWarnings(as.integer(value$priority))
  if (anyNA(position) || anyNA(priority) ||
      !identical(position, seq_len(nrow(value))) ||
      !identical(as.character(position), as.character(value$position)) ||
      !identical(as.character(priority), as.character(value$priority)) ||
      any(priority < 0L) ||
      anyDuplicated(value$repository) ||
      any(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", value$repository)) ||
      any(!grepl("^https://[^[:space:]]+[.]git$", value$origin)) ||
      any(!grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", value$commit)) ||
      any(!grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", value$tree))) {
    repository_runner_fail("repository selection is malformed or unordered")
  }
  value$position <- position
  value$priority <- priority
  row.names(value) <- NULL
  value
}

repository_runner_protected_paths <- function(config) {
  paths <- c(
    candidate_package = repository_runner_require_directory(
      config$candidate_package, "candidate package"),
    candidate_library = repository_runner_require_directory(
      config$candidate_library, "candidate library"),
    dependency_library = repository_runner_require_directory(
      config$dependency_library, "dependency library")
  )
  if (length(config$extra_libraries)) {
    extras <- vapply(seq_along(config$extra_libraries), function(index) {
      repository_runner_require_directory(config$extra_libraries[[index]],
        paste0("extra library ", index))
    }, character(1L))
    names(extras) <- sprintf("extra_library_%03d", seq_along(extras))
    paths <- c(paths, extras)
  }
  if (anyDuplicated(unname(paths))) {
    repository_runner_fail("protected library paths must be distinct")
  }
  paths
}

repository_runner_full_content_boundary <- function(config, fingerprint,
                                                       boundary) {
  if (!is.function(fingerprint) || !boundary %in% c("start", "final")) {
    repository_runner_fail("full-content boundary requires a fingerprint function")
  }
  paths <- repository_runner_protected_paths(config)
  hashes <- vapply(paths, fingerprint, character(1L))
  if (any(!grepl("^[0-9a-f]{64}$", hashes))) {
    repository_runner_fail("protected content fingerprint is malformed")
  }
  data.frame(boundary = boundary, kind = names(paths),
    position = as.character(seq_along(paths)),
    path = unname(paths), sha256 = unname(hashes), stringsAsFactors = FALSE)
}

repository_runner_protected_metadata <- function(config) {
  paths <- repository_runner_protected_paths(config)
  data.frame(kind = names(paths), position = seq_along(paths), path = unname(paths),
    sha256 = unname(vapply(paths, repository_runner_metadata_sha256,
      character(1L))),
    stringsAsFactors = FALSE)
}

repository_runner_tool_receipt <- function(config, output = NULL) {
  paths <- config$tool_files
  if (!length(paths) || is.null(names(paths)) || any(!nzchar(names(paths))) ||
      anyDuplicated(names(paths)) ||
      any(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", names(paths)))) {
    repository_runner_fail("runner tool inputs must have unique safe labels")
  }
  paths <- vapply(seq_along(paths), function(index) {
    repository_runner_require_file(paths[[index]], paste0("tool input ", names(paths)[[index]]))
  }, character(1L))
  names(paths) <- names(config$tool_files)
  receipt <- data.frame(name = names(paths), path = unname(paths),
    sha256 = repository_runner_sha256(paths), stringsAsFactors = FALSE)
  if (!is.null(output)) {
    tools <- repository_runner_reserve_directory(output, "tools", "retained tools")
    destinations <- file.path(tools, names(paths))
    if (!all(file.copy(paths, destinations, copy.mode = TRUE, copy.date = TRUE)) ||
        !identical(repository_runner_sha256(destinations), receipt$sha256)) {
      repository_runner_fail("could not retain exact runner tool inputs")
    }
    receipt$retained_sha256 <- repository_runner_sha256(destinations)
  }
  receipt
}

repository_runner_resource_fields <- function() c(
  "schema", "profile", "platform", "online_cpus", "affinity_cpus",
  "cgroup_cpu_limit", "cpu_limit", "cpu_reserve", "cpu_per_job",
  "cpu_jobs", "memory_source", "memory_available_mib",
  "cgroup_memory_available_mib", "memory_reserve_mib",
  "memory_mib_per_job", "memory_jobs", "profile_max_jobs",
  "operator_max_jobs", "jobs"
)

repository_runner_parse_resource_report <- function(lines) {
  if (!length(lines) || anyNA(lines) || any(grepl("[\r]", lines))) {
    repository_runner_fail("resource scheduler returned malformed text")
  }
  value <- tryCatch(utils::read.delim(text = paste(lines, collapse = "\n"),
    header = TRUE, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE),
    error = function(condition) condition)
  if (inherits(value, "condition") || !identical(names(value), c("field", "value")) ||
      anyNA(value) || any(!nzchar(value$field)) || any(!nzchar(value$value)) ||
      anyDuplicated(value$field) ||
      !identical(value$field, repository_runner_resource_fields())) {
    repository_runner_fail("resource scheduler report has an unexpected schema")
  }
  report <- stats::setNames(value$value, value$field)
  positive <- function(name) {
    text <- report[[name]]
    if (!grepl("^[1-9][0-9]{0,8}$", text)) {
      repository_runner_fail(
        "resource scheduler field is not a canonical positive integer: ", name
      )
    }
    as.integer(text)
  }
  nonnegative <- function(name) {
    text <- report[[name]]
    if (!grepl("^(0|[1-9][0-9]{0,8})$", text)) {
      repository_runner_fail(
        "resource scheduler field is not a canonical nonnegative integer: ",
        name
      )
    }
    as.integer(text)
  }
  optional_positive <- function(name, absent) {
    if (identical(report[[name]], absent)) return(Inf)
    positive(name)
  }
  if (!identical(report[["schema"]], "1") ||
      !identical(report[["profile"]], "consumer") ||
      !identical(report[["profile_max_jobs"]], "4") ||
      !grepl("^[A-Za-z0-9_.+-]+$", report[["platform"]]) ||
      !grepl("^[A-Za-z0-9_.+-]+$", report[["memory_source"]])) {
    repository_runner_fail("resource scheduler report violates consumer policy")
  }
  online <- positive("online_cpus")
  affinity <- positive("affinity_cpus")
  cgroup_cpu <- optional_positive("cgroup_cpu_limit", "unlimited")
  cpu_limit <- positive("cpu_limit")
  cpu_reserve <- nonnegative("cpu_reserve")
  cpu_per_job <- positive("cpu_per_job")
  cpu_jobs <- positive("cpu_jobs")
  memory_available <- positive("memory_available_mib")
  cgroup_memory <- optional_positive(
    "cgroup_memory_available_mib", "unlimited"
  )
  memory_reserve <- positive("memory_reserve_mib")
  memory_per_job <- positive("memory_mib_per_job")
  memory_jobs <- positive("memory_jobs")
  operator_max <- optional_positive("operator_max_jobs", "none")
  jobs <- positive("jobs")
  expected_cpu_limit <- min(online, affinity, cgroup_cpu)
  expected_cpu_reserve <- if (expected_cpu_limit >= 16L) {
    2L
  } else if (expected_cpu_limit >= 4L) {
    1L
  } else 0L
  expected_cpu_jobs <- max(
    1L, as.integer((expected_cpu_limit - expected_cpu_reserve) %/% 2L)
  )
  expected_memory_reserve <- max(16384L, memory_available %/% 4L)
  expected_memory_jobs <- as.integer(
    (memory_available - expected_memory_reserve) %/% 8192L
  )
  unconstrained_jobs <- min(expected_cpu_jobs, expected_memory_jobs, 4L)
  if (!identical(cpu_limit, as.integer(expected_cpu_limit)) ||
      !identical(cpu_reserve, expected_cpu_reserve) ||
      !identical(cpu_per_job, 2L) ||
      !identical(cpu_jobs, expected_cpu_jobs) ||
      is.finite(cgroup_memory) && memory_available > cgroup_memory ||
      !identical(memory_reserve, expected_memory_reserve) ||
      !identical(memory_per_job, 8192L) || expected_memory_jobs < 1L ||
      !identical(memory_jobs, expected_memory_jobs) ||
      is.finite(operator_max) && operator_max > unconstrained_jobs ||
      !identical(jobs, as.integer(min(unconstrained_jobs, operator_max)))) {
    repository_runner_fail("resource scheduler report violates consumer policy")
  }
  list(table = value, values = report, jobs = jobs, lines = lines)
}

repository_runner_resource_decision <- function(config, operator_max = NULL,
                                                retained_max = NULL) {
  helper <- config$tool_files[["resource-jobs"]]
  if (is.null(helper)) {
    repository_runner_fail("runner tool inputs omit the resource scheduler")
  }
  helper <- repository_runner_require_file(helper, "resource scheduler")
  if (file.access(helper, mode = 1L) != 0L) {
    repository_runner_fail("resource scheduler is not executable")
  }
  run_report <- function(arguments) {
    result <- processx::run(helper, arguments, stdout = "|", stderr = "|",
      error_on_status = FALSE, cleanup_tree = TRUE, timeout = 30000)
    if (!identical(result$status, 0L)) {
      repository_runner_fail("resource scheduler failed: ",
        repository_runner_compact_error(paste(c(result$stdout, result$stderr),
          collapse = "\n")))
    }
    lines <- strsplit(result$stdout, "\n", fixed = TRUE)[[1L]]
    lines <- lines[nzchar(lines)]
    repository_runner_parse_resource_report(lines)
  }
  automatic <- run_report(c("consumer", "--report"))
  selection <- "automatic"
  decision <- automatic
  normalize_limit <- function(value, label) {
    if (length(value) != 1L || is.na(value)) {
      repository_runner_fail(label, " must be one canonical positive integer")
    }
    text <- as.character(value)
    if (!grepl("^[1-9][0-9]*$", text) ||
        !identical(as.character(as.integer(text)), text)) {
      repository_runner_fail(label, " must be one canonical positive integer")
    }
    as.integer(text)
  }
  retained_limit <- if (is.null(retained_max)) automatic$jobs else
    normalize_limit(retained_max, "retained consumer ceiling")
  if (retained_limit > automatic$jobs && is.null(retained_max)) {
    repository_runner_fail("automatic consumer ceiling is internally inconsistent")
  }
  operator_limit <- if (is.null(operator_max)) NULL else
    normalize_limit(operator_max, "consumer job override")
  if (!is.null(operator_max)) {
    if (operator_limit > retained_limit ||
        (is.null(retained_max) && operator_limit > automatic$jobs)) {
      repository_runner_fail("consumer job override may lower but not exceed the current safe ceiling ",
        min(automatic$jobs, retained_limit))
    }
    selection <- "operator_conservative_override"
  } else if (retained_limit < automatic$jobs) {
    selection <- "retained_initial_ceiling"
  }
  effective_limit <- min(c(automatic$jobs, retained_limit,
    if (is.null(operator_limit)) Inf else operator_limit))
  if (effective_limit < automatic$jobs) {
    decision <- run_report(c("consumer", "--max-jobs",
      as.character(effective_limit), "--report"))
    if (decision$jobs > effective_limit ||
        !identical(decision$values[["operator_max_jobs"]],
          as.character(effective_limit))) {
      repository_runner_fail("resource scheduler did not honour the conservative ceiling")
    }
  }
  decision$selection <- selection
  decision$helper <- helper
  decision$helper_sha256 <- repository_runner_sha256(helper)
  decision$automatic_table <- automatic$table
  decision$automatic_jobs <- automatic$jobs
  decision$retained_jobs <- retained_limit
  decision$operator_jobs <- if (is.null(operator_limit)) NULL else operator_limit
  decision$effective_limit <- effective_limit
  decision
}

repository_runner_config_fields <- function() c(
  "schema", "stage_kind", "run_id", "created_utc", "root", "stage",
  "consumer_root", "selected_rows", "timeout_seconds", "candidate_origin",
  "candidate_ref", "candidate_commit", "candidate_tree", "candidate_source",
  "candidate_version", "candidate_library", "candidate_package",
  "candidate_content_sha256", "candidate_provenance_sha256",
  "candidate_installer_archive_sha256", "dependency_library",
  "dependency_content_sha256", "extra_libraries", "git", "git_sha256",
  "rscript", "r_version", "selection_sha256", "tool_receipt_sha256",
  "base_environment_sha256", "resource_initial_sha256",
  "resource_initial_metadata_sha256",
  "candidate_proof_archive_sha256", "candidate_proof_tree_sha256",
  "candidate_proof_manifest_sha256", "protected_content_start_sha256",
  "protected_metadata_start_sha256", "not_cran",
  "protected_library_full_hash_boundaries"
)

repository_runner_config_values <- function(config, selection, created_utc,
                                             proof, start_content,
                                             start_metadata, tool_receipt_path,
                                             resource_initial_path,
                                             resource_initial_metadata_path) {
  c(
    schema = "1", stage_kind = "repository_tests_resumable", run_id = config$run_id,
    created_utc = created_utc, root = config$root, stage = config$stage,
    consumer_root = config$consumer_root,
    selected_rows = as.character(nrow(selection)),
    timeout_seconds = format(config$timeout_seconds, scientific = FALSE, trim = TRUE),
    candidate_origin = config$candidate_origin, candidate_ref = config$candidate_ref,
    candidate_commit = config$candidate_commit, candidate_tree = config$candidate_tree,
    candidate_source = config$candidate_source,
    candidate_version = config$candidate_version,
    candidate_library = config$candidate_library,
    candidate_package = config$candidate_package,
    candidate_content_sha256 = config$candidate_content_sha256,
    candidate_provenance_sha256 = config$candidate_provenance_sha256,
    candidate_installer_archive_sha256 = config$candidate_installer_archive_sha256,
    dependency_library = config$dependency_library,
    dependency_content_sha256 = config$dependency_content_sha256,
    extra_libraries = if (length(config$extra_libraries))
      paste(config$extra_libraries, collapse = .Platform$path.sep) else "-",
    git = config$git, git_sha256 = repository_runner_sha256(config$git),
    rscript = config$rscript, r_version = as.character(getRversion()),
    selection_sha256 = repository_runner_sha256(file.path(config$stage, "inputs",
      "selection.tsv")),
    tool_receipt_sha256 = repository_runner_sha256(tool_receipt_path),
    base_environment_sha256 = repository_runner_sha256(file.path(config$stage,
      "inputs", "base-environment.tsv")),
    resource_initial_sha256 = repository_runner_sha256(resource_initial_path),
    resource_initial_metadata_sha256 = repository_runner_sha256(
      resource_initial_metadata_path),
    candidate_proof_archive_sha256 = proof$archive_sha256,
    candidate_proof_tree_sha256 = proof$tree_sha256,
    candidate_proof_manifest_sha256 = proof$manifest_sha256,
    protected_content_start_sha256 = repository_runner_sha256(start_content),
    protected_metadata_start_sha256 = repository_runner_sha256(start_metadata),
    not_cran = "true", protected_library_full_hash_boundaries = "1"
  )
}

repository_runner_assert_config <- function(config) {
  required <- c(
    "root", "stage", "run_id", "consumer_root", "timeout_seconds",
    "candidate_origin", "candidate_ref", "candidate_commit", "candidate_tree",
    "candidate_source", "candidate_version", "candidate_library",
    "candidate_package", "candidate_content_sha256", "candidate_provenance_sha256",
    "candidate_installer_archive_sha256", "dependency_library",
    "dependency_content_sha256", "extra_libraries", "git", "rscript", "tool_files",
    "base_environment"
  )
  if (any(!required %in% names(config))) {
    repository_runner_fail("runner configuration is incomplete")
  }
  repository_runner_safe_name(config$run_id, "repository run ID")
  config$root <- repository_runner_require_directory(config$root, "repository root")
  config$consumer_root <- repository_runner_require_directory(config$consumer_root,
    "consumer checkout root")
  config$candidate_source <- repository_runner_require_directory(
    config$candidate_source, "candidate source")
  config$candidate_library <- repository_runner_require_directory(
    config$candidate_library, "candidate library")
  config$candidate_package <- repository_runner_require_directory(
    config$candidate_package, "candidate package")
  config$dependency_library <- repository_runner_require_directory(
    config$dependency_library, "dependency library")
  config$git <- repository_runner_require_file(config$git, "repository-local Git")
  config$rscript <- repository_runner_require_file(config$rscript,
    "repository-local Rscript")
  if (!identical(dirname(config$candidate_package), config$candidate_library)) {
    repository_runner_fail("candidate package must be a direct candidate-library child")
  }
  if (length(config$extra_libraries)) {
    config$extra_libraries <- unname(vapply(seq_along(config$extra_libraries),
      function(index) repository_runner_require_directory(
        config$extra_libraries[[index]], paste0("extra library ", index)),
      character(1L)))
  }
  if (!grepl("^https://[^[:space:]]+[.]git$", config$candidate_origin) ||
      !grepl("^refs/[A-Za-z0-9][A-Za-z0-9._/-]*$", config$candidate_ref) ||
      !grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", config$candidate_commit) ||
      !grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", config$candidate_tree) ||
      any(!grepl("^[0-9a-f]{64}$", c(config$candidate_content_sha256,
        config$candidate_provenance_sha256,
        config$candidate_installer_archive_sha256,
        config$dependency_content_sha256))) ||
      length(config$timeout_seconds) != 1L || !is.finite(config$timeout_seconds) ||
      config$timeout_seconds < 1 || config$timeout_seconds > 21600 ||
      length(config$candidate_version) != 1L || is.na(config$candidate_version) ||
      !nzchar(config$candidate_version) ||
      grepl("[\r\n\t]", config$candidate_version)) {
    repository_runner_fail("runner candidate pins, hashes, or timeout are malformed")
  }
  if (length(config$base_environment) &&
      (is.null(names(config$base_environment)) ||
       anyDuplicated(names(config$base_environment)) ||
       any(!nzchar(names(config$base_environment))) || anyNA(config$base_environment) ||
       any(!nzchar(config$base_environment)))) {
    repository_runner_fail("base child environment must be one named vector")
  }
  stage_parent <- normalizePath(dirname(config$stage), winslash = "/",
    mustWork = TRUE)
  repository_runner_safe_name(basename(config$stage), "repository stage name")
  config$stage <- file.path(stage_parent, basename(config$stage))
  config
}

repository_runner_initialize <- function(config, selection, fingerprint) {
  config <- repository_runner_assert_config(config)
  selection <- repository_runner_selection(selection)
  if (dir.exists(config$stage)) {
    return(repository_runner_load(config, selection, require_current = TRUE))
  }
  parent <- repository_runner_require_directory(dirname(config$stage),
    "repository stage parent")
  stage <- repository_runner_reserve_directory(parent, basename(config$stage),
    "repository stage")
  config$stage <- stage
  dir.create(file.path(stage, "metadata"), recursive = FALSE)
  inputs <- repository_runner_reserve_directory(stage, "inputs", "immutable inputs")
  repository_runner_reserve_directory(stage, "rows", "row evidence root")
  repository_runner_reserve_directory(stage, "completions", "completion root")
  scheduler <- repository_runner_reserve_directory(stage, "scheduler",
    "resource scheduler evidence root")
  repository_runner_reserve_directory(scheduler, "waves", "scheduler wave root")
  repository_runner_reserve_directory(scheduler, "quarantine",
    "scheduler quarantine root")
  authentication <- repository_runner_authenticate_candidate(config)
  proof <- repository_runner_candidate_proof(authentication, inputs)
  repository_runner_write_tsv(selection, file.path(inputs, "selection.tsv"))
  environment_names <- if (length(config$base_environment)) {
    names(config$base_environment)
  } else character()
  repository_runner_write_tsv(data.frame(name = environment_names,
    value = unname(config$base_environment), stringsAsFactors = FALSE),
    file.path(inputs, "base-environment.tsv"))
  initial_resource <- repository_runner_resource_decision(config)
  resource_initial_path <- file.path(inputs, "resource-initial.tsv")
  repository_runner_write_tsv(initial_resource$table, resource_initial_path)
  resource_initial_metadata_path <- file.path(inputs,
    "resource-initial-metadata.tsv")
  repository_runner_write_tsv(repository_runner_map_frame(c(schema = "1",
    selection = initial_resource$selection, helper = initial_resource$helper,
    helper_sha256 = initial_resource$helper_sha256,
    report_sha256 = repository_runner_sha256(resource_initial_path))),
    resource_initial_metadata_path)
  start_content_path <- file.path(inputs, "protected-content-start.tsv")
  start_content <- repository_runner_full_content_boundary(config, fingerprint, "start")
  repository_runner_write_tsv(start_content, start_content_path)
  expected <- c(candidate_package = config$candidate_content_sha256,
    dependency_library = config$dependency_content_sha256)
  observed <- stats::setNames(start_content$sha256, start_content$kind)
  if (!identical(unname(observed[names(expected)]), unname(expected))) {
    repository_runner_fail("candidate or dependency content differs at stage start")
  }
  start_metadata_path <- file.path(inputs, "protected-metadata-start.tsv")
  repository_runner_write_tsv(repository_runner_protected_metadata(config),
    start_metadata_path)
  tool_receipt <- repository_runner_tool_receipt(config, inputs)
  tool_receipt_path <- file.path(inputs, "tool-receipt.tsv")
  repository_runner_write_tsv(tool_receipt, tool_receipt_path)
  repository_runner_write_tsv(repository_runner_map_frame(authentication$output),
    file.path(inputs, "candidate-authentication.tsv"))
  run <- repository_runner_config_values(config, selection,
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), proof,
    start_content_path, start_metadata_path, tool_receipt_path,
    resource_initial_path, resource_initial_metadata_path)
  repository_runner_write_tsv(repository_runner_map_frame(run),
    file.path(inputs, "run.tsv"))
  evidence <- repository_runner_seal_directory(inputs)
  repository_runner_load(config, selection, require_current = TRUE,
    expected_input_seal = evidence$manifest_sha256)
}

repository_runner_verify_current_tools <- function(context) {
  receipt <- context$tool_receipt
  current <- vapply(receipt$path, repository_runner_require_file, character(1L),
    label = "current runner tool input")
  if (!identical(repository_runner_sha256(current), receipt$sha256)) {
    repository_runner_fail("runner tool input changed during the stage")
  }
  repository_runner_object_sha256(stats::setNames(receipt$sha256, receipt$name),
    "repository-runner-tools-")
}

repository_runner_verify_candidate_current <- function(context,
                                                        recreate_archive = FALSE,
                                                        verify_worktree_bytes = FALSE) {
  authentication <- repository_runner_authenticate_candidate(context$config)
  expected_authentication <- repository_runner_read_map(file.path(context$inputs,
    "candidate-authentication.tsv"), label = "candidate authentication")
  if (!identical(unname(authentication$output[names(expected_authentication)]),
      unname(expected_authentication))) {
    repository_runner_fail("candidate authentication differs from stage start")
  }
  if (isTRUE(verify_worktree_bytes)) {
    current <- repository_runner_file_manifest(authentication$checkout,
      exclude_git = TRUE)
    expected <- repository_runner_read_tsv(file.path(context$inputs,
      "candidate-source-manifest.tsv"),
      c("path", "size", "executable", "sha256"),
      label = "candidate source manifest")
    if (!identical(current, expected)) {
      repository_runner_fail("candidate source bytes changed during the stage")
    }
  }
  if (recreate_archive) {
    archive <- tempfile("repository-runner-final-candidate-", fileext = ".tar")
    on.exit(unlink(archive), add = TRUE)
    observed <- repository_runner_create_archive(authentication, archive)
    if (!identical(observed$sha256,
        context$run[["candidate_proof_archive_sha256"]])) {
      repository_runner_fail("final candidate archive differs from stage start")
    }
  }
  authentication
}

repository_runner_assert_metadata <- function(context) {
  observed <- repository_runner_protected_metadata(context$config)
  expected <- repository_runner_read_tsv(file.path(context$inputs,
    "protected-metadata-start.tsv"), c("kind", "position", "path", "sha256"),
    label = "protected metadata start")
  observed$position <- as.character(observed$position)
  if (!identical(observed, expected)) {
    changed_rows <- Reduce(`|`, Map(function(lhs, rhs) lhs != rhs,
      observed, expected))
    changed <- expected$kind[changed_rows]
    repository_runner_fail("protected library metadata changed during the stage",
      if (length(changed)) paste0(": ", paste(changed, collapse = ", ")) else
        " (data-frame attributes differ)")
  }
  observed
}

repository_runner_load <- function(config, selection = NULL, require_current = TRUE,
                                   expected_input_seal = NULL) {
  config <- repository_runner_assert_config(config)
  stage <- repository_runner_require_directory(config$stage, "repository stage")
  scheduler <- repository_runner_require_directory(file.path(stage, "scheduler"),
    "resource scheduler evidence root")
  repository_runner_require_directory(file.path(scheduler, "waves"),
    "scheduler wave root")
  repository_runner_require_directory(file.path(scheduler, "quarantine"),
    "scheduler quarantine root")
  inputs <- repository_runner_require_directory(file.path(stage, "inputs"),
    "immutable stage inputs")
  evidence <- repository_runner_verify_sealed_directory(inputs)
  if (!is.null(expected_input_seal) &&
      !identical(evidence$manifest_sha256, expected_input_seal)) {
    repository_runner_fail("new stage input seal changed during initialization")
  }
  run <- repository_runner_read_map(file.path(inputs, "run.tsv"),
    repository_runner_config_fields(), "repository stage run")
  if (!identical(run[["schema"]], "1") ||
      !identical(run[["stage_kind"]], "repository_tests_resumable") ||
      !identical(run[["run_id"]], config$run_id) ||
      !identical(run[["root"]], config$root) || !identical(run[["stage"]], stage) ||
      !identical(run[["protected_library_full_hash_boundaries"]], "1")) {
    repository_runner_fail("repository stage is not the expected resumable schema")
  }
  retained <- repository_runner_read_tsv(file.path(inputs, "selection.tsv"),
    c("position", "repository", "priority", "origin", "commit", "tree"),
    label = "retained repository selection")
  retained <- repository_runner_selection(retained)
  if (!is.null(selection)) {
    selection <- repository_runner_selection(selection)
    if (!identical(retained, selection)) {
      repository_runner_fail("resume selection differs from sealed stage inputs")
    }
  }
  expected_config <- c(
    consumer_root = config$consumer_root,
    selected_rows = as.character(nrow(retained)),
    timeout_seconds = format(config$timeout_seconds, scientific = FALSE, trim = TRUE),
    candidate_origin = config$candidate_origin, candidate_ref = config$candidate_ref,
    candidate_commit = config$candidate_commit, candidate_tree = config$candidate_tree,
    candidate_source = config$candidate_source,
    candidate_version = config$candidate_version,
    candidate_library = config$candidate_library,
    candidate_package = config$candidate_package,
    candidate_content_sha256 = config$candidate_content_sha256,
    candidate_provenance_sha256 = config$candidate_provenance_sha256,
    candidate_installer_archive_sha256 = config$candidate_installer_archive_sha256,
    dependency_library = config$dependency_library,
    dependency_content_sha256 = config$dependency_content_sha256,
    extra_libraries = if (length(config$extra_libraries))
      paste(config$extra_libraries, collapse = .Platform$path.sep) else "-",
    git = config$git, git_sha256 = repository_runner_sha256(config$git),
    rscript = config$rscript, r_version = as.character(getRversion()),
    selection_sha256 = repository_runner_sha256(file.path(
      inputs, "selection.tsv")), base_environment_sha256 = repository_runner_sha256(
        file.path(inputs, "base-environment.tsv")),
    resource_initial_sha256 = repository_runner_sha256(file.path(
      inputs, "resource-initial.tsv")),
    resource_initial_metadata_sha256 = repository_runner_sha256(file.path(
      inputs, "resource-initial-metadata.tsv")), not_cran = "true"
  )
  if (!identical(unname(run[names(expected_config)]), unname(expected_config))) {
    repository_runner_fail("current runner configuration differs from sealed stage")
  }
  tool_receipt <- repository_runner_read_tsv(file.path(inputs, "tool-receipt.tsv"),
    c("name", "path", "sha256", "retained_sha256"), label = "tool receipt")
  expected_tool_receipt <- repository_runner_tool_receipt(config)
  retained_tools <- file.path(inputs, "tools", tool_receipt$name)
  retained_tools <- vapply(seq_along(retained_tools), function(index) {
    repository_runner_require_file(retained_tools[[index]],
      paste0("retained tool ", tool_receipt$name[[index]]))
  }, character(1L))
  if (!identical(tool_receipt[, c("name", "path", "sha256"), drop = FALSE],
      expected_tool_receipt) ||
      !identical(tool_receipt$retained_sha256, tool_receipt$sha256) ||
      !identical(repository_runner_sha256(retained_tools), tool_receipt$sha256)) {
    repository_runner_fail("retained runner tools differ from sealed inputs")
  }
  retained_environment <- repository_runner_read_tsv(file.path(inputs,
    "base-environment.tsv"), c("name", "value"), allow_empty = TRUE,
    label = "base child environment")
  current_environment_names <- if (length(config$base_environment)) {
    names(config$base_environment)
  } else character()
  current_environment <- data.frame(name = current_environment_names,
    value = unname(config$base_environment), stringsAsFactors = FALSE)
  if (!identical(retained_environment, current_environment)) {
    repository_runner_fail("current child environment differs from sealed stage")
  }
  initial_resource <- repository_runner_read_tsv(file.path(inputs,
    "resource-initial.tsv"), c("field", "value"),
    label = "initial resource decision")
  initial_resource_value <- repository_runner_parse_resource_report(c("field\tvalue", paste(
    initial_resource$field, initial_resource$value, sep = "\t")))
  initial_resource_metadata <- repository_runner_read_map(file.path(inputs,
    "resource-initial-metadata.tsv"), c("schema", "selection", "helper",
      "helper_sha256", "report_sha256"), "initial resource metadata")
  helper_index <- match("resource-jobs", tool_receipt$name)
  if (is.na(helper_index) || !identical(initial_resource_metadata[["schema"]], "1") ||
      !identical(initial_resource_metadata[["selection"]], "automatic") ||
      !identical(initial_resource_metadata[["helper"]],
        tool_receipt$path[[helper_index]]) ||
      !identical(initial_resource_metadata[["helper_sha256"]],
        tool_receipt$sha256[[helper_index]]) ||
      !identical(initial_resource_metadata[["report_sha256"]],
        repository_runner_sha256(file.path(inputs, "resource-initial.tsv")))) {
    repository_runner_fail("initial resource decision lacks authenticated scheduler identity")
  }
  start_content <- repository_runner_read_tsv(file.path(inputs,
    "protected-content-start.tsv"),
    c("boundary", "kind", "position", "path", "sha256"),
    label = "protected content start")
  start_metadata <- repository_runner_read_tsv(file.path(inputs,
    "protected-metadata-start.tsv"), c("kind", "position", "path", "sha256"),
    label = "protected metadata start")
  protected_paths <- repository_runner_protected_paths(config)
  expected_positions <- as.character(seq_along(protected_paths))
  expected_pins <- c(candidate_package = config$candidate_content_sha256,
    dependency_library = config$dependency_content_sha256)
  observed_pins <- stats::setNames(start_content$sha256, start_content$kind)
  artifacts <- c(
    tool_receipt_sha256 = repository_runner_sha256(file.path(inputs,
      "tool-receipt.tsv")),
    candidate_proof_archive_sha256 = repository_runner_sha256(file.path(inputs,
      "candidate-source.tar")),
    candidate_proof_tree_sha256 = repository_runner_sha256(file.path(inputs,
      "candidate-tree.tsv")),
    candidate_proof_manifest_sha256 = repository_runner_sha256(file.path(inputs,
      "candidate-source-manifest.tsv")),
    protected_content_start_sha256 = repository_runner_sha256(file.path(inputs,
      "protected-content-start.tsv")),
    protected_metadata_start_sha256 = repository_runner_sha256(file.path(inputs,
      "protected-metadata-start.tsv"))
  )
  if (!identical(unname(run[names(artifacts)]), unname(artifacts)) ||
      any(start_content$boundary != "start") ||
      !identical(start_content$kind, names(protected_paths)) ||
      !identical(start_content$position, expected_positions) ||
      !identical(start_content$path, unname(protected_paths)) ||
      any(!grepl("^[0-9a-f]{64}$", start_content$sha256)) ||
      !identical(unname(observed_pins[names(expected_pins)]),
        unname(expected_pins)) ||
      !identical(start_metadata$kind, names(protected_paths)) ||
      !identical(start_metadata$position, expected_positions) ||
      !identical(start_metadata$path, unname(protected_paths)) ||
      any(!grepl("^[0-9a-f]{64}$", start_metadata$sha256))) {
    repository_runner_fail("sealed stage inputs have inconsistent semantics")
  }
  context <- list(config = config, stage = stage, inputs = inputs, run = run,
    selection = retained, input_evidence = evidence, tool_receipt = tool_receipt,
    initial_resource = initial_resource_value)
  if (require_current) {
    repository_runner_verify_current_tools(context)
    repository_runner_verify_candidate_current(context)
    repository_runner_assert_metadata(context)
  }
  context
}

repository_runner_count_fields <- function() c(
  "schema", "availability", "test_cases", "expectations", "passed", "failed",
  "skipped", "errors", "warnings"
)

repository_runner_unavailable_counts <- function(availability) c(
  schema = "1", availability = availability, test_cases = "-", expectations = "-",
  passed = "-", failed = "-", skipped = "-", errors = "-", warnings = "-"
)

repository_runner_validate_counts <- function(path, framework, process_status,
                                              timed_out) {
  counts <- repository_runner_read_map(path, repository_runner_count_fields(),
    "structured test counts")
  allowed <- c("complete_testthat", "partial_tinytest", "partial_base_files",
    "not_applicable", "unavailable_before_results", "unavailable_interrupted",
    "unavailable_timed_out")
  if (!identical(counts[["schema"]], "1") ||
      !counts[["availability"]] %in% allowed) {
    repository_runner_fail("structured counts have an unsupported availability")
  }
  numeric_fields <- setdiff(repository_runner_count_fields(), c("schema", "availability"))
  valid <- vapply(counts[numeric_fields], function(value)
    identical(value, "-") || grepl("^(0|[1-9][0-9]*)$", value), logical(1L))
  if (any(!valid)) repository_runner_fail("structured counts contain an invalid number")
  if (identical(counts[["availability"]], "complete_testthat")) {
    values <- as.integer(counts[numeric_fields])
    names(values) <- numeric_fields
    if (!identical(framework, "testthat") || anyNA(values) ||
        values[["expectations"]] != sum(values[c(
          "passed", "failed", "skipped", "warnings")]) ||
        (identical(process_status, 0L) &&
          (values[["failed"]] != 0L || values[["errors"]] != 0L))) {
      repository_runner_fail("complete testthat counts are inconsistent")
    }
  }
  if (identical(counts[["availability"]], "partial_tinytest")) {
    fields <- c("test_cases", "expectations", "passed", "failed")
    values <- as.integer(counts[fields])
    names(values) <- fields
    if (!identical(framework, "tinytest") || anyNA(values) ||
        values[["test_cases"]] != values[["expectations"]] ||
        values[["expectations"]] != values[["passed"]] + values[["failed"]] ||
        any(counts[c("skipped", "errors", "warnings")] != "-")) {
      repository_runner_fail("partial tinytest counts are inconsistent")
    }
  }
  if (identical(counts[["availability"]], "partial_base_files") &&
      (!identical(framework, "base") || counts[["test_cases"]] == "-" ||
       any(counts[setdiff(numeric_fields, "test_cases")] != "-"))) {
    repository_runner_fail("partial base-file counts are inconsistent")
  }
  if (identical(counts[["availability"]], "not_applicable") &&
      (!identical(framework, "none") || any(counts[numeric_fields] != "0"))) {
    repository_runner_fail("not-applicable counts are inconsistent")
  }
  unavailable <- startsWith(counts[["availability"]], "unavailable_")
  if (unavailable && (identical(framework, "none") ||
      any(counts[numeric_fields] != "-") || identical(process_status, 0L) ||
      (identical(counts[["availability"]], "unavailable_timed_out") !=
        isTRUE(timed_out)))) {
    repository_runner_fail("unavailable counts are inconsistent")
  }
  counts
}

repository_runner_nested_parallel_environment <- function() c(
  CMAKE_BUILD_PARALLEL_LEVEL = "1", `_R_CHECK_LIMIT_CORES_` = "true",
  MC_CORES = "1", R_FUTURE_PLAN = "sequential",
  R_FUTURE_FORK_ENABLE = "false", R_PARALLELLY_FORK_ENABLE = "false",
  R_PARALLELLY_AVAILABLECORES_FALLBACK = "1",
  R_FUTURE_AVAILABLECORES_FALLBACK = "1"
)

repository_runner_environment_receipt_names <- function() c(
  "NOT_CRAN", "TESTTHAT_PARALLEL", "TESTTHAT_CPUS", "MAKEFLAGS",
  names(repository_runner_nested_parallel_environment()),
  "OMP_NUM_THREADS", "OMP_THREAD_LIMIT", "OPENBLAS_NUM_THREADS",
  "GOTO_NUM_THREADS", "MKL_NUM_THREADS", "BLIS_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS",
  "RCPP_PARALLEL_NUM_THREADS", "R_LIBS", "R_LIBS_USER",
  "PARADOX_ROW_STATE_ROOT", "HOME", "TMPDIR", "XDG_CACHE_HOME",
  "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_STATE_HOME", "XDG_RUNTIME_DIR",
  "R_USER_CACHE_DIR", "R_USER_CONFIG_DIR", "R_USER_DATA_DIR",
  "PYTHONDONTWRITEBYTECODE", "PYTHONNOUSERSITE", "PYTHONPYCACHEPREFIX",
  "PYTHONUSERBASE", "PIP_CACHE_DIR", "PIP_CONFIG_FILE",
  "PIP_DISABLE_PIP_VERSION_CHECK", "UV_CACHE_DIR", "UV_PYTHON_INSTALL_DIR",
  "RETICULATE_VIRTUALENV_ROOT", "WORKON_HOME", "RETICULATE_MINICONDA_PATH",
  "RETICULATE_AUTOCONFIGURE", "RETICULATE_AUTOCREATE_PACKAGE_VENV",
  "RETICULATE_USE_MANAGED_VENV", "MPLCONFIGDIR", "TORCH_HOME", "HF_HOME",
  "HUGGINGFACE_HUB_CACHE", "TRANSFORMERS_CACHE", "KERAS_HOME",
  "NUMBA_CACHE_DIR", "CUDA_CACHE_PATH", "WEKA_HOME", "CCACHE_DIR",
  "CCACHE_TEMPDIR"
)

repository_runner_process_environment <- function(context, row_state) {
  row_state <- repository_runner_require_directory(row_state, "row-local state")
  state_names <- c(
    home = "home", tmp = "tmp", xdg_cache = "xdg-cache",
    xdg_config = "xdg-config", xdg_data = "xdg-data", xdg_state = "xdg-state",
    xdg_runtime = "xdg-runtime", r_cache = "r-cache", r_config = "r-config",
    r_data = "r-data", pip_cache = "pip-cache", uv_cache = "uv-cache",
    uv_python = "uv-python", virtualenvs = "virtualenvs", miniconda = "miniconda",
    python_user = "python-user", pycache = "pycache", matplotlib = "matplotlib",
    torch = "torch", huggingface = "huggingface", keras = "keras",
    numba = "numba", cuda = "cuda", weka = "weka", ccache = "ccache",
    ccache_tmp = "ccache-tmp"
  )
  paths <- file.path(row_state, unname(state_names))
  created <- vapply(paths, dir.create, logical(1L), recursive = FALSE,
    showWarnings = FALSE)
  if (!all(created) || any(vapply(paths, repository_runner_is_symbolic, logical(1L)))) {
    repository_runner_fail("could not reserve row-local cache directories")
  }
  state <- stats::setNames(paths, names(state_names))
  Sys.chmod(state[["xdg_runtime"]], "0700")
  nested <- c(hub = file.path(state[["huggingface"]], "hub"),
    transformers = file.path(state[["huggingface"]], "transformers"))
  if (!all(vapply(nested, dir.create, logical(1L), recursive = FALSE,
      showWarnings = FALSE))) {
    repository_runner_fail("could not reserve nested row-local caches")
  }
  libraries <- c(context$config$candidate_library,
    context$config$extra_libraries, context$config$dependency_library)
  library_environment <- paste(libraries, collapse = .Platform$path.sep)
  environment <- context$config$base_environment
  nested_parallel <- repository_runner_nested_parallel_environment()
  override_names <- c(
    "NOT_CRAN", "TESTTHAT_PARALLEL", "TESTTHAT_CPUS", "MAKEFLAGS",
    names(nested_parallel),
    "OMP_NUM_THREADS", "OMP_THREAD_LIMIT", "OPENBLAS_NUM_THREADS",
    "GOTO_NUM_THREADS", "MKL_NUM_THREADS", "BLIS_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS",
    "RCPP_PARALLEL_NUM_THREADS", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE",
    "R_PROFILE_USER", "R_ENVIRON_USER", "R_DEFAULT_PACKAGES", "R_TESTS",
    "PARADOX_ROW_STATE_ROOT", "HOME", "TMPDIR", "XDG_CACHE_HOME",
    "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_STATE_HOME", "XDG_RUNTIME_DIR",
    "R_USER_CACHE_DIR", "R_USER_CONFIG_DIR", "R_USER_DATA_DIR",
    "PYTHONDONTWRITEBYTECODE", "PYTHONNOUSERSITE", "PYTHONPYCACHEPREFIX",
    "PYTHONUSERBASE", "PYTHONPATH", "PYTHONHOME", "VIRTUAL_ENV",
    "PIP_CACHE_DIR", "PIP_CONFIG_FILE", "PIP_DISABLE_PIP_VERSION_CHECK",
    "UV_CACHE_DIR", "UV_PYTHON_INSTALL_DIR", "RETICULATE_PYTHON",
    "RETICULATE_PYTHON_ENV", "RETICULATE_VIRTUALENV_ROOT", "WORKON_HOME",
    "RETICULATE_MINICONDA_PATH", "RETICULATE_AUTOCONFIGURE",
    "RETICULATE_AUTOCREATE_PACKAGE_VENV", "RETICULATE_USE_MANAGED_VENV",
    "MPLCONFIGDIR", "TORCH_HOME", "HF_HOME", "HUGGINGFACE_HUB_CACHE",
    "TRANSFORMERS_CACHE", "KERAS_HOME", "NUMBA_CACHE_DIR", "CUDA_CACHE_PATH",
    "WEKA_HOME", "CCACHE_DIR", "CCACHE_TEMPDIR"
  )
  overrides <- c(
    NOT_CRAN = "true", TESTTHAT_PARALLEL = "false", TESTTHAT_CPUS = "1",
    MAKEFLAGS = "-j1", nested_parallel,
    OMP_NUM_THREADS = "1", OMP_THREAD_LIMIT = "1",
    OPENBLAS_NUM_THREADS = "1", GOTO_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    BLIS_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1", RCPP_PARALLEL_NUM_THREADS = "1",
    R_LIBS = library_environment,
    R_LIBS_USER = library_environment, R_LIBS_SITE = "", R_PROFILE_USER = "",
    R_ENVIRON_USER = "",
    R_DEFAULT_PACKAGES = "datasets,utils,grDevices,graphics,stats,methods",
    R_TESTS = "",
    PARADOX_ROW_STATE_ROOT = row_state, HOME = state[["home"]],
    TMPDIR = state[["tmp"]], XDG_CACHE_HOME = state[["xdg_cache"]],
    XDG_CONFIG_HOME = state[["xdg_config"]], XDG_DATA_HOME = state[["xdg_data"]],
    XDG_STATE_HOME = state[["xdg_state"]], XDG_RUNTIME_DIR = state[["xdg_runtime"]],
    R_USER_CACHE_DIR = state[["r_cache"]], R_USER_CONFIG_DIR = state[["r_config"]],
    R_USER_DATA_DIR = state[["r_data"]], PYTHONDONTWRITEBYTECODE = "1",
    PYTHONNOUSERSITE = "1", PYTHONPYCACHEPREFIX = state[["pycache"]],
    PYTHONUSERBASE = state[["python_user"]], PYTHONPATH = "", PYTHONHOME = "",
    VIRTUAL_ENV = "", PIP_CACHE_DIR = state[["pip_cache"]],
    PIP_CONFIG_FILE = "/dev/null", PIP_DISABLE_PIP_VERSION_CHECK = "1",
    UV_CACHE_DIR = state[["uv_cache"]], UV_PYTHON_INSTALL_DIR = state[["uv_python"]],
    RETICULATE_PYTHON = "", RETICULATE_PYTHON_ENV = "",
    RETICULATE_VIRTUALENV_ROOT = state[["virtualenvs"]],
    WORKON_HOME = state[["virtualenvs"]],
    RETICULATE_MINICONDA_PATH = state[["miniconda"]],
    RETICULATE_AUTOCONFIGURE = "FALSE",
    RETICULATE_AUTOCREATE_PACKAGE_VENV = "FALSE",
    RETICULATE_USE_MANAGED_VENV = "yes", MPLCONFIGDIR = state[["matplotlib"]],
    TORCH_HOME = state[["torch"]], HF_HOME = state[["huggingface"]],
    HUGGINGFACE_HUB_CACHE = nested[["hub"]],
    TRANSFORMERS_CACHE = nested[["transformers"]], KERAS_HOME = state[["keras"]],
    NUMBA_CACHE_DIR = state[["numba"]], CUDA_CACHE_PATH = state[["cuda"]],
    WEKA_HOME = state[["weka"]], CCACHE_DIR = state[["ccache"]],
    CCACHE_TEMPDIR = state[["ccache_tmp"]]
  )
  if (!identical(override_names, names(overrides))) {
    repository_runner_fail("row-local environment override order is inconsistent")
  }
  environment[override_names] <- unname(overrides)
  environment
}

repository_runner_environment_receipt <- function(environment) {
  names <- repository_runner_environment_receipt_names()
  if (any(!names %in% names(environment)) || anyNA(environment[names])) {
    repository_runner_fail("child environment lacks required isolation values")
  }
  data.frame(name = names, value = unname(environment[names]),
    stringsAsFactors = FALSE)
}

repository_runner_validate_environment_receipt <- function(receipt, attempt,
                                                           context) {
  names <- repository_runner_environment_receipt_names()
  if (!identical(receipt$name, names) || anyDuplicated(receipt$name)) {
    repository_runner_fail("child environment receipt has unexpected scope")
  }
  values <- stats::setNames(receipt$value, receipt$name)
  state <- file.path(attempt, "work", "row-state")
  paths <- c(
    PARADOX_ROW_STATE_ROOT = state, HOME = file.path(state, "home"),
    TMPDIR = file.path(state, "tmp"), XDG_CACHE_HOME = file.path(state, "xdg-cache"),
    XDG_CONFIG_HOME = file.path(state, "xdg-config"),
    XDG_DATA_HOME = file.path(state, "xdg-data"),
    XDG_STATE_HOME = file.path(state, "xdg-state"),
    XDG_RUNTIME_DIR = file.path(state, "xdg-runtime"),
    R_USER_CACHE_DIR = file.path(state, "r-cache"),
    R_USER_CONFIG_DIR = file.path(state, "r-config"),
    R_USER_DATA_DIR = file.path(state, "r-data"),
    PYTHONPYCACHEPREFIX = file.path(state, "pycache"),
    PYTHONUSERBASE = file.path(state, "python-user"),
    PIP_CACHE_DIR = file.path(state, "pip-cache"),
    UV_CACHE_DIR = file.path(state, "uv-cache"),
    UV_PYTHON_INSTALL_DIR = file.path(state, "uv-python"),
    RETICULATE_VIRTUALENV_ROOT = file.path(state, "virtualenvs"),
    WORKON_HOME = file.path(state, "virtualenvs"),
    RETICULATE_MINICONDA_PATH = file.path(state, "miniconda"),
    MPLCONFIGDIR = file.path(state, "matplotlib"), TORCH_HOME = file.path(state, "torch"),
    HF_HOME = file.path(state, "huggingface"),
    HUGGINGFACE_HUB_CACHE = file.path(state, "huggingface", "hub"),
    TRANSFORMERS_CACHE = file.path(state, "huggingface", "transformers"),
    KERAS_HOME = file.path(state, "keras"), NUMBA_CACHE_DIR = file.path(state, "numba"),
    CUDA_CACHE_PATH = file.path(state, "cuda"), WEKA_HOME = file.path(state, "weka"),
    CCACHE_DIR = file.path(state, "ccache"),
    CCACHE_TEMPDIR = file.path(state, "ccache-tmp")
  )
  libraries <- paste(c(context$config$candidate_library,
    context$config$extra_libraries, context$config$dependency_library),
    collapse = .Platform$path.sep)
  constants <- c(NOT_CRAN = "true", TESTTHAT_PARALLEL = "false",
    TESTTHAT_CPUS = "1", MAKEFLAGS = "-j1",
    repository_runner_nested_parallel_environment(), OMP_NUM_THREADS = "1",
    OMP_THREAD_LIMIT = "1", OPENBLAS_NUM_THREADS = "1",
    GOTO_NUM_THREADS = "1", MKL_NUM_THREADS = "1", BLIS_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1",
    RCPP_PARALLEL_NUM_THREADS = "1", R_LIBS = libraries,
    R_LIBS_USER = libraries, PYTHONDONTWRITEBYTECODE = "1", PYTHONNOUSERSITE = "1",
    PIP_CONFIG_FILE = "/dev/null", PIP_DISABLE_PIP_VERSION_CHECK = "1",
    RETICULATE_AUTOCONFIGURE = "FALSE",
    RETICULATE_AUTOCREATE_PACKAGE_VENV = "FALSE",
    RETICULATE_USE_MANAGED_VENV = "yes")
  expected <- c(constants, paths)
  if (!identical(unname(values[names(expected)]), unname(expected))) {
    repository_runner_fail("child environment receipt does not prove row isolation")
  }
  invisible(TRUE)
}

repository_runner_manifest_delta <- function(before, after) {
  columns <- c("path", "type", "mode", "size", "sha256")
  if (!identical(names(before), columns) || !identical(names(after), columns)) {
    repository_runner_fail("tree manifests have unexpected columns")
  }
  paths <- sort(unique(c(before$path, after$path)), method = "radix")
  bi <- match(paths, before$path)
  ai <- match(paths, after$path)
  added <- is.na(bi)
  deleted <- is.na(ai)
  modified <- !added & !deleted & vapply(seq_along(paths), function(index) {
    !identical(unname(unlist(before[bi[[index]], setdiff(columns, "path"),
      drop = FALSE])), unname(unlist(after[ai[[index]], setdiff(columns, "path"),
      drop = FALSE])))
  }, logical(1L))
  keep <- added | deleted | modified
  paths <- paths[keep]
  bi <- bi[keep]
  ai <- ai[keep]
  field <- function(value, index, field) {
    output <- rep("-", length(index))
    present <- !is.na(index)
    output[present] <- value[[field]][index[present]]
    output
  }
  value <- data.frame(
    change = as.character(ifelse(is.na(bi), "added",
      ifelse(is.na(ai), "deleted", "modified"))),
    path = paths, before_type = field(before, bi, "type"),
    after_type = field(after, ai, "type"), before_mode = field(before, bi, "mode"),
    after_mode = field(after, ai, "mode"), before_size = field(before, bi, "size"),
    after_size = field(after, ai, "size"), before_sha256 = field(before, bi, "sha256"),
    after_sha256 = field(after, ai, "sha256"), stringsAsFactors = FALSE
  )
  row.names(value) <- NULL
  value
}

repository_runner_candidate_sentinel_sha256 <- function(context) {
  path <- repository_runner_require_file(file.path(context$config$candidate_library,
    ".paradox-candidate-content-sha256"), "candidate content sentinel")
  value <- readLines(path, warn = FALSE)
  if (!identical(value, context$config$candidate_content_sha256)) {
    repository_runner_fail("candidate content sentinel differs from the sealed pin")
  }
  repository_runner_sha256(path)
}

repository_runner_capture_protection <- function(context) {
  errors <- character()
  capture <- function(name, expression, fallback) tryCatch(expression,
    error = function(condition) {
      errors[[name]] <<- conditionMessage(condition)
      fallback
    })
  tool <- capture("tools", repository_runner_verify_current_tools(context), "-")
  candidate <- capture("candidate_source",
    repository_runner_verify_candidate_current(context), NULL)
  metadata <- capture("protected_libraries",
    repository_runner_protected_metadata(context$config), NULL)
  expected_metadata <- repository_runner_read_tsv(file.path(context$inputs,
    "protected-metadata-start.tsv"), c("kind", "position", "path", "sha256"),
    label = "protected metadata start")
  if (!is.null(metadata)) metadata$position <- as.character(metadata$position)
  sentinel <- capture("candidate_sentinel",
    repository_runner_candidate_sentinel_sha256(context), "-")
  violations <- c(
    if (identical(tool, "-")) "tools" else character(),
    if (is.null(candidate)) "candidate_source" else character(),
    if (is.null(metadata) || !identical(metadata, expected_metadata))
      "protected_libraries" else character(),
    if (identical(sentinel, "-")) "candidate_sentinel" else character()
  )
  list(tool_sha256 = tool,
    candidate_authentication_sha256 = if (is.null(candidate)) "-" else
      candidate$output_sha256,
    protected_metadata_sha256 = if (is.null(metadata)) "-" else
      repository_runner_object_sha256(metadata, "repository-runner-row-metadata-"),
    candidate_sentinel_sha256 = sentinel, violations = unique(violations),
    errors = errors)
}

repository_runner_assert_protection <- function(value, label) {
  if (length(value$violations)) {
    detail <- if (length(value$errors)) paste(value$errors, collapse = "; ") else
      paste(value$violations, collapse = ", ")
    repository_runner_fail(label, " protected-input check failed: ", detail)
  }
  invisible(value)
}

repository_runner_framework <- function(checkout) {
  if (dir.exists(file.path(checkout, "tests", "testthat"))) return("testthat")
  if (dir.exists(file.path(checkout, "inst", "tinytest"))) return("tinytest")
  tests <- file.path(checkout, "tests")
  if (dir.exists(tests) && length(list.files(tests, pattern = "[.][Rr]$"))) {
    return("base")
  }
  "none"
}

repository_runner_run_child <- function(context, checkout, framework, log,
                                        counts, row_state, environment) {
  config <- context$config
  child <- config$tool_files[["repository-test-child.R"]]
  extra <- if (length(config$extra_libraries))
    paste(config$extra_libraries, collapse = .Platform$path.sep) else "-"
  arguments <- c(
    "--vanilla", child, checkout, framework, config$candidate_library,
    config$dependency_library, extra, config$candidate_version,
    config$candidate_content_sha256, counts, row_state
  )
  result <- processx::run(config$rscript, arguments, env = environment,
    stdout = log, stderr_to_stdout = TRUE, error_on_status = FALSE, echo = FALSE,
    cleanup_tree = TRUE, timeout = config$timeout_seconds,
    windows_verbatim_args = TRUE)
  list(status = result$status, timed_out = isTRUE(result$timeout),
    command = paste(c(config$rscript, arguments), collapse = " "))
}

repository_runner_classify_failure <- function(message, timed_out = FALSE) {
  if (timed_out) return("timeout")
  dependency <- paste(c("there is no package called",
    "package required but not available", "namespace .* is being loaded, but .* is required",
    "dependencies?.*not available"), collapse = "|")
  system <- paste(c("cannot find -l", "library not found for -l",
    "No such file or directory", "configuration failed for package", "command not found"),
    collapse = "|")
  if (grepl(dependency, message, ignore.case = TRUE, perl = TRUE)) {
    "environmental_dependency_failure"
  } else if (grepl(system, message, ignore.case = TRUE, perl = TRUE)) {
    "environmental_system_dependency_failure"
  } else {
    "candidate_or_consumer_test_failure"
  }
}

repository_runner_compact_error <- function(value, limit = 4000L) {
  value <- gsub("[\r\n\t]+", " ", value)
  if (!nzchar(value)) return("-")
  if (nchar(value, type = "chars") <= limit) return(value)
  side <- as.integer((limit - 80L) / 2L)
  paste0(substr(value, 1L, side), " [... omitted ...] ",
    substr(value, nchar(value) - side + 1L, nchar(value)))
}

repository_runner_row_fields <- function() c(
  "schema", "position", "repository", "priority", "origin", "commit", "tree",
  "framework", "candidate_version", "candidate_ref", "candidate_commit",
  "candidate_tree", "candidate_content_sha256", "candidate_library_sha256",
  "dependency_library_sha256", "extra_library_sha256", "not_cran",
  "timeout_seconds", "process_exit_status", "process_timed_out", "status",
  "classification", "elapsed_seconds", "mutation_added", "mutation_modified",
  "mutation_deleted", "archive_sha256", "source_authentication_sha256",
  "archive_entries_sha256", "source_tree_sha256",
  "pristine_manifest_sha256", "work_before_manifest_sha256",
  "work_after_manifest_sha256", "delta_sha256", "test_log_sha256",
  "test_counts_sha256", "child_environment_sha256", "test_count_availability",
  "test_cases", "test_expectations", "test_passed", "test_failed",
  "test_skipped", "test_errors", "test_warnings",
  "protected_metadata_sha256_expected", "protected_metadata_sha256_pre_child",
  "protected_metadata_sha256_post_child", "candidate_sentinel_sha256_pre_child",
  "candidate_sentinel_sha256_post_child", "tool_inputs_sha256_pre_child",
  "tool_inputs_sha256_post_child", "candidate_authentication_sha256_pre_child",
  "candidate_authentication_sha256_post_child", "test_command", "error"
)

repository_runner_row_name <- function(row) sprintf("%04d-%s",
  as.integer(row$position[[1L]]), row$repository[[1L]])

repository_runner_validate_row_scope <- function(context, require_all = FALSE) {
  root <- repository_runner_require_directory(file.path(context$stage, "rows"),
    "row evidence root")
  paths <- as.character(fs::dir_ls(root, all = TRUE, recurse = FALSE,
    type = "any", fail = TRUE))
  expected <- vapply(seq_len(nrow(context$selection)), function(index) {
    repository_runner_row_name(context$selection[index, , drop = FALSE])
  }, character(1L))
  if (length(paths)) {
    info <- fs::file_info(paths)
    observed <- basename(paths)
    if (any(as.character(info$type) != "directory") ||
        any(vapply(paths, repository_runner_is_symbolic, logical(1L))) ||
        anyDuplicated(observed) || any(!observed %in% expected)) {
      repository_runner_fail("row evidence root contains an unknown or unsafe entry")
    }
  } else {
    observed <- character()
  }
  if (isTRUE(require_all) && !setequal(observed, expected)) {
    repository_runner_fail("row evidence does not cover the complete dynamic selection")
  }
  invisible(observed)
}

repository_runner_next_attempt <- function(row_directory) {
  attempts <- list.files(row_directory, pattern = "^attempt-[0-9]{6}$")
  numbers <- if (length(attempts)) as.integer(sub("^attempt-", "", attempts)) else 0L
  sprintf("attempt-%06d", max(numbers) + 1L)
}

repository_runner_quarantine_root <- function(row_directory) {
  path <- file.path(row_directory, "quarantine")
  if (dir.exists(path)) repository_runner_require_directory(path, "row quarantine") else
    repository_runner_reserve_directory(row_directory, "quarantine", "row quarantine")
}

repository_runner_quarantine_attempt <- function(row_directory, attempt, reason, detail) {
  root <- repository_runner_quarantine_root(row_directory)
  output <- file.path(root, basename(attempt))
  if (dir.exists(output)) return(invisible(output))
  output <- repository_runner_reserve_directory(root, basename(attempt),
    "attempt quarantine")
  evidence <- tryCatch(repository_runner_verify_sealed_directory(attempt),
    error = function(...) NULL)
  value <- c(schema = "1", attempt = basename(attempt), reason = reason,
    detail = repository_runner_compact_error(detail),
    attempt_manifest_sha256 = if (is.null(evidence)) "-" else evidence$manifest_sha256,
    attempt_seal_sha256 = if (is.null(evidence)) "-" else evidence$seal_sha256,
    summary_sha256 = if (file.exists(file.path(attempt, "summary.tsv")))
      repository_runner_sha256(file.path(attempt, "summary.tsv")) else "-")
  repository_runner_write_tsv(repository_runner_map_frame(value),
    file.path(output, "quarantine.tsv"))
  repository_runner_seal_directory(output)
  invisible(output)
}

repository_runner_quarantine_partial_acceptance <- function(row_directory,
                                                            fail = TRUE) {
  paths <- file.path(row_directory, c("accepted.tsv", "accepted.seal"))
  present <- file.exists(paths) | dir.exists(paths) |
    vapply(paths, repository_runner_is_symbolic, logical(1L))
  if (sum(present) != 1L) return(invisible(FALSE))
  root <- repository_runner_quarantine_root(row_directory)
  existing <- list.files(root, pattern = "^partial-promotion-[0-9]{6}$")
  number <- if (length(existing)) max(as.integer(sub(
    "^partial-promotion-", "", existing))) + 1L else 1L
  output <- repository_runner_reserve_directory(root,
    sprintf("partial-promotion-%06d", number), "partial promotion quarantine")
  source <- paths[present][[1L]]
  destination <- file.path(output, basename(source))
  if (repository_runner_is_symbolic(source) || !file.rename(source, destination)) {
    repository_runner_fail("could not safely quarantine partial acceptance")
  }
  repository_runner_write_tsv(repository_runner_map_frame(c(
    schema = "1", reason = "partial_acceptance_promotion",
    retained_file = basename(destination),
    retained_sha256 = repository_runner_sha256(destination)
  )), file.path(output, "quarantine.tsv"))
  repository_runner_seal_directory(output)
  if (isTRUE(fail)) {
    repository_runner_fail("partial acceptance was quarantined; restart explicitly")
  }
  invisible(TRUE)
}

repository_runner_accept_attempt <- function(row_directory, attempt, validation,
                                             wave) {
  evidence <- repository_runner_verify_sealed_directory(attempt)
  if (is.null(wave) || !is.list(wave) || is.null(wave$path) ||
      is.null(wave$evidence)) {
    repository_runner_fail("attempt acceptance requires one verified sealed wave")
  }
  wave_path <- repository_runner_require_directory(wave$path,
    "accepting scheduler wave")
  wave_evidence <- repository_runner_verify_sealed_directory(wave_path)
  if (!identical(validation$attempt, attempt) ||
      !identical(validation$evidence$manifest_sha256, evidence$manifest_sha256) ||
      !identical(wave$evidence$manifest_sha256,
        wave_evidence$manifest_sha256) ||
      !identical(wave$evidence$seal_sha256, wave_evidence$seal_sha256) ||
      grepl("provenance_violation$", validation$summary[["classification"]])) {
    repository_runner_fail("attempt lacks semantic and provenance validation")
  }
  accepted <- c(schema = "2", attempt = basename(attempt),
    manifest_sha256 = evidence$manifest_sha256, seal_sha256 = evidence$seal_sha256,
    summary_sha256 = repository_runner_sha256(file.path(attempt, "summary.tsv")),
    wave = basename(wave_path),
    wave_manifest_sha256 = wave_evidence$manifest_sha256,
    wave_seal_sha256 = wave_evidence$seal_sha256)
  path <- file.path(row_directory, "accepted.tsv")
  seal <- file.path(row_directory, "accepted.seal")
  repository_runner_write_tsv(repository_runner_map_frame(accepted), path)
  repository_runner_write_lines(paste0("accepted_sha256=", repository_runner_sha256(path)),
    seal)
  invisible(accepted)
}

repository_runner_verify_attempt <- function(context, row, attempt_override = NULL,
                                             deep = TRUE) {
  row_directory <- repository_runner_require_directory(file.path(context$stage, "rows",
    repository_runner_row_name(row)), "row evidence")
  if (is.null(attempt_override)) {
    accepted_path <- repository_runner_require_file(file.path(row_directory,
      "accepted.tsv"), "row acceptance")
    accepted_seal <- repository_runner_require_file(file.path(row_directory,
      "accepted.seal"), "row acceptance seal")
    if (!identical(readLines(accepted_seal, warn = FALSE),
        paste0("accepted_sha256=", repository_runner_sha256(accepted_path)))) {
      repository_runner_fail("row acceptance seal is invalid")
    }
    accepted <- repository_runner_read_map(accepted_path,
      c("schema", "attempt", "manifest_sha256", "seal_sha256", "summary_sha256",
        "wave", "wave_manifest_sha256", "wave_seal_sha256"),
      "row acceptance")
    if (!identical(accepted[["schema"]], "2") ||
        !grepl("^wave-[0-9]{6}$", accepted[["wave"]]) ||
        any(!grepl("^[0-9a-f]{64}$", accepted[c("wave_manifest_sha256",
          "wave_seal_sha256")]))) {
      repository_runner_fail("row acceptance uses an unsupported schema")
    }
    repository_runner_safe_name(accepted[["attempt"]], "accepted attempt")
    attempt <- repository_runner_require_directory(file.path(row_directory,
      accepted[["attempt"]]), "accepted attempt")
  } else {
    accepted_path <- accepted_seal <- NULL
    accepted <- NULL
    attempt <- repository_runner_require_directory(attempt_override,
      "pre-promotion attempt")
    if (!identical(dirname(attempt), row_directory)) {
      repository_runner_fail("pre-promotion attempt escaped its row")
    }
  }
  evidence <- repository_runner_verify_sealed_directory(attempt)
  if (!is.null(accepted) &&
      (!identical(accepted[["manifest_sha256"]], evidence$manifest_sha256) ||
       !identical(accepted[["seal_sha256"]], evidence$seal_sha256) ||
       !identical(accepted[["summary_sha256"]], repository_runner_sha256(
         file.path(attempt, "summary.tsv"))))) {
    repository_runner_fail("accepted attempt hashes differ from its pointer")
  }
  summary <- repository_runner_read_map(file.path(attempt, "summary.tsv"),
    repository_runner_row_fields(), "row summary")
  start_content <- repository_runner_read_tsv(file.path(context$inputs,
    "protected-content-start.tsv"), c("boundary", "kind", "position", "path", "sha256"),
    label = "protected content start")
  content <- stats::setNames(start_content$sha256, start_content$kind)
  extras <- content[startsWith(names(content), "extra_library_")]
  expected <- c(schema = "1", position = as.character(row$position[[1L]]),
    repository = row$repository[[1L]], priority = as.character(row$priority[[1L]]),
    origin = row$origin[[1L]], commit = row$commit[[1L]], tree = row$tree[[1L]],
    candidate_version = context$config$candidate_version,
    candidate_ref = context$config$candidate_ref,
    candidate_commit = context$config$candidate_commit,
    candidate_tree = context$config$candidate_tree,
    candidate_content_sha256 = context$config$candidate_content_sha256,
    candidate_library_sha256 = content[["candidate_library"]],
    dependency_library_sha256 = content[["dependency_library"]],
    extra_library_sha256 = if (length(extras)) paste(extras, collapse = ";") else "-",
    not_cran = "true", timeout_seconds = format(context$config$timeout_seconds,
      scientific = FALSE, trim = TRUE))
  if (!identical(unname(summary[names(expected)]), unname(expected))) {
    repository_runner_fail("row summary differs from selection or stage inputs")
  }
  hashes <- c(source_tree_sha256 = repository_runner_sha256(file.path(attempt,
    "source-tree.tsv")), source_authentication_sha256 = repository_runner_sha256(
      file.path(attempt, "source-authentication.tsv")),
    archive_entries_sha256 = repository_runner_sha256(file.path(attempt,
      "archive-entries.tsv")),
    pristine_manifest_sha256 = repository_runner_sha256(file.path(
      attempt, "pristine-manifest.tsv")), work_before_manifest_sha256 =
      repository_runner_sha256(file.path(attempt, "work-before-manifest.tsv")),
    work_after_manifest_sha256 = repository_runner_sha256(file.path(attempt,
      "work-after-manifest.tsv")), delta_sha256 = repository_runner_sha256(file.path(
      attempt, "post-test-delta.tsv")), test_log_sha256 = repository_runner_sha256(
      file.path(attempt, "test.log")), test_counts_sha256 = repository_runner_sha256(
      file.path(attempt, "test-counts.tsv")), child_environment_sha256 =
      repository_runner_sha256(file.path(attempt, "child-environment.tsv")))
  if (!identical(unname(summary[names(hashes)]), unname(hashes)) ||
      !identical(repository_runner_sha256(file.path(attempt, "source.tar")),
        summary[["archive_sha256"]])) {
    repository_runner_fail("row summary does not bind retained artifacts")
  }
  source_authentication <- repository_runner_read_map(file.path(attempt,
    "source-authentication.tsv"), c("git", "git_sha256", "checkout", "origin",
      "commit", "tree", "archive_sha256"), "source authentication")
  expected_checkout <- normalizePath(file.path(context$config$consumer_root,
    row$repository[[1L]]), winslash = "/", mustWork = TRUE)
  expected_authentication <- c(git = context$config$git,
    git_sha256 = repository_runner_sha256(context$config$git),
    checkout = expected_checkout, origin = row$origin[[1L]],
    commit = row$commit[[1L]], tree = row$tree[[1L]],
    archive_sha256 = summary[["archive_sha256"]])
  archive_entries <- repository_runner_read_tsv(file.path(attempt,
    "archive-entries.tsv"), "path", label = "source archive entries")
  expected_tree <- repository_runner_read_tsv(file.path(attempt, "source-tree.tsv"),
    c("mode", "object", "path", "size", "sha256"), label = "source-tree proof")
  if (!identical(unname(source_authentication), unname(expected_authentication)) ||
      any(!vapply(archive_entries$path, function(entry) {
        normalized <- sub("/$", "", entry)
        identical(normalized, "source") || (startsWith(entry, "source/") &&
          repository_runner_safe_relative(substring(entry, 8L)))
      }, logical(1L))) || anyDuplicated(archive_entries$path) ||
      any(!expected_tree$mode %in% c("100644", "100755", "120000")) ||
      any(!grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", expected_tree$object)) ||
      any(!vapply(expected_tree$path, repository_runner_safe_relative,
        logical(1L))) || anyDuplicated(expected_tree$path) ||
      !identical(expected_tree$path,
        sort(expected_tree$path, method = "radix")) ||
      any(!grepl("^(0|[1-9][0-9]*)$", expected_tree$size)) ||
      any(!grepl("^[0-9a-f]{64}$", expected_tree$sha256))) {
    repository_runner_fail("row source authentication or tree proof is malformed")
  }
  if (isTRUE(deep)) {
    authentication <- repository_runner_authenticate_consumer(context$config, row)
    observed_entries <- repository_runner_archive_entries(file.path(attempt,
      "source.tar"))
    if (!identical(archive_entries$path, observed_entries)) {
      repository_runner_fail("row archive inventory differs from retained evidence")
    }
    temporary <- tempfile("repository-runner-verify-row-", tmpdir = dirname(attempt))
    if (!dir.create(temporary, recursive = FALSE)) {
      repository_runner_fail("could not reserve row archive verification")
    }
    on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
    suppressWarnings(utils::untar(file.path(attempt, "source.tar"), exdir = temporary,
      tar = "internal"))
    observed_tree <- repository_runner_validate_extraction(authentication,
      repository_runner_require_directory(file.path(temporary, "source"),
        "verified row extraction"))
    if (!identical(observed_tree, expected_tree)) {
      repository_runner_fail("retained row archive differs from its Git tree proof")
    }
  }
  pristine <- repository_runner_read_tsv(file.path(attempt, "pristine-manifest.tsv"),
    c("path", "type", "mode", "size", "sha256"), allow_empty = TRUE,
    label = "pristine manifest")
  before <- repository_runner_read_tsv(file.path(attempt, "work-before-manifest.tsv"),
    c("path", "type", "mode", "size", "sha256"), allow_empty = TRUE,
    label = "work-before manifest")
  after <- repository_runner_read_tsv(file.path(attempt, "work-after-manifest.tsv"),
    c("path", "type", "mode", "size", "sha256"), allow_empty = TRUE,
    label = "work-after manifest")
  delta_columns <- c("change", "path", "before_type", "after_type", "before_mode",
    "after_mode", "before_size", "after_size", "before_sha256", "after_sha256")
  delta <- repository_runner_read_tsv(file.path(attempt, "post-test-delta.tsv"),
    delta_columns, allow_empty = TRUE, label = "post-test delta")
  expected_delta <- repository_runner_manifest_delta(before, after)
  counts <- repository_runner_validate_counts(file.path(attempt, "test-counts.tsv"),
    summary[["framework"]], suppressWarnings(as.integer(summary[["process_exit_status"]])),
    identical(summary[["process_timed_out"]], "true"))
  count_summary <- c(test_count_availability = counts[["availability"]],
    test_cases = counts[["test_cases"]], test_expectations = counts[["expectations"]],
    test_passed = counts[["passed"]], test_failed = counts[["failed"]],
    test_skipped = counts[["skipped"]], test_errors = counts[["errors"]],
    test_warnings = counts[["warnings"]])
  receipt <- repository_runner_read_tsv(file.path(attempt, "child-environment.tsv"),
    c("name", "value"), label = "child environment receipt")
  repository_runner_validate_environment_receipt(receipt, attempt, context)
  mutation <- c(added = sum(delta$change == "added"),
    modified = sum(delta$change == "modified"), deleted = sum(delta$change == "deleted"))
  provenance <- c("validation_tool_provenance_violation",
    "candidate_source_provenance_violation", "protected_library_provenance_violation")
  ordinary <- c("passed", "harness_no_tests", "timeout",
    "environmental_dependency_failure", "environmental_system_dependency_failure",
    "candidate_or_consumer_test_failure")
  numeric_status <- suppressWarnings(as.integer(summary[["process_exit_status"]]))
  numeric_elapsed <- suppressWarnings(as.numeric(summary[["elapsed_seconds"]]))
  log <- readLines(file.path(attempt, "test.log"), warn = FALSE)
  log_has <- function(text) any(grepl(text, log, fixed = TRUE))
  normal <- !summary[["classification"]] %in% provenance
  status_consistent <- switch(summary[["status"]],
    passed = summary[["framework"]] != "none" && identical(numeric_status, 0L) &&
      summary[["process_timed_out"]] == "false" &&
      summary[["classification"]] == "passed" && summary[["error"]] == "-" &&
      log_has("ordinary_upstream_tests=passed"),
    skipped_no_tests = summary[["framework"]] == "none" &&
      identical(numeric_status, 0L) && summary[["process_timed_out"]] == "false" &&
      summary[["classification"]] == "harness_no_tests" && summary[["error"]] == "-",
    timed_out = summary[["process_timed_out"]] == "true" &&
      summary[["classification"]] == "timeout" && summary[["error"]] != "-",
    failed = summary[["error"]] != "-" &&
      (summary[["classification"]] %in% provenance ||
       (!identical(numeric_status, 0L) && summary[["classification"]] %in% ordinary)),
    FALSE)
  expected_metadata <- repository_runner_read_tsv(file.path(context$inputs,
    "protected-metadata-start.tsv"), c("kind", "position", "path", "sha256"),
    label = "protected metadata start")
  metadata_sha <- repository_runner_object_sha256(expected_metadata,
    "repository-runner-row-metadata-")
  sentinel_sha <- repository_runner_candidate_sentinel_sha256(context)
  checks <- c(
    pristine_before = identical(pristine, before),
    delta = identical(delta, expected_delta),
    counts = identical(unname(summary[names(count_summary)]),
      unname(count_summary)),
    mutation_added = identical(summary[["mutation_added"]],
      as.character(mutation[["added"]])),
    mutation_modified = identical(summary[["mutation_modified"]],
      as.character(mutation[["modified"]])),
    mutation_deleted = identical(summary[["mutation_deleted"]],
      as.character(mutation[["deleted"]])),
    framework = summary[["framework"]] %in% c("testthat", "tinytest", "base", "none"),
    status = summary[["status"]] %in%
      c("passed", "failed", "timed_out", "skipped_no_tests"),
    classification = summary[["classification"]] %in% c(ordinary, provenance),
    accepted_provenance = is.null(accepted) || normal,
    numeric_status = !is.na(numeric_status),
    numeric_elapsed = !is.na(numeric_elapsed) && numeric_elapsed >= 0,
    status_semantics = status_consistent,
    metadata_expected = identical(
      summary[["protected_metadata_sha256_expected"]], metadata_sha),
    metadata_pre = !normal || identical(
      summary[["protected_metadata_sha256_pre_child"]], metadata_sha),
    metadata_post = !normal || identical(
      summary[["protected_metadata_sha256_post_child"]], metadata_sha),
    sentinel_pre = !normal || identical(
      summary[["candidate_sentinel_sha256_pre_child"]], sentinel_sha),
    sentinel_post = !normal || identical(
      summary[["candidate_sentinel_sha256_post_child"]], sentinel_sha),
    tools = !normal || identical(summary[["tool_inputs_sha256_pre_child"]],
      summary[["tool_inputs_sha256_post_child"]]),
    candidate_authentication = !normal || identical(
      summary[["candidate_authentication_sha256_pre_child"]],
      summary[["candidate_authentication_sha256_post_child"]])
  )
  if (any(!checks)) {
    repository_runner_fail("row attempt has inconsistent semantic evidence: ",
      paste(names(checks)[!checks], collapse = ", "))
  }
  list(summary = summary, attempt = attempt, accepted = accepted, evidence = evidence,
    accepted_sha256 = if (is.null(accepted_path)) "-" else
      repository_runner_sha256(accepted_path))
}

repository_runner_promote_attempt <- function(context, row, row_directory, attempt,
                                              validator = NULL, wave = NULL) {
  validation <- tryCatch(if (is.null(validator))
    repository_runner_verify_attempt(context, row, attempt) else validator(attempt),
    error = function(condition) condition)
  if (inherits(validation, "condition")) {
    repository_runner_quarantine_attempt(row_directory, attempt,
      "semantic_validation_failed", conditionMessage(validation))
    repository_runner_fail("row attempt failed semantic validation before promotion: ",
      conditionMessage(validation))
  }
  if (grepl("provenance_violation$", validation$summary[["classification"]])) {
    repository_runner_quarantine_attempt(row_directory, attempt,
      "provenance_validation_failed", validation$summary[["error"]])
    repository_runner_fail("row attempt failed provenance validation before promotion")
  }
  repository_runner_assert_parent_lock(context)
  repository_runner_accept_attempt(row_directory, attempt, validation, wave)
  # The exact attempt was deeply validated immediately above.  Reopen the
  # append-only acceptance pointer and scoped seal without repeating archive
  # extraction and Git blob hashing.
  repository_runner_verify_attempt(context, row, deep = FALSE)
}

repository_runner_prepare_row <- function(context, row) {
  repository_runner_validate_row_scope(context)
  rows <- repository_runner_require_directory(file.path(context$stage, "rows"),
    "row evidence root")
  row_directory <- file.path(rows, repository_runner_row_name(row))
  if (dir.exists(row_directory)) {
    row_directory <- repository_runner_require_directory(row_directory, "row evidence")
  } else {
    row_directory <- repository_runner_reserve_directory(rows,
      repository_runner_row_name(row), "row evidence")
  }
  repository_runner_quarantine_partial_acceptance(row_directory)
  if (file.exists(file.path(row_directory, "accepted.tsv"))) {
    repository_runner_verify_attempt(context, row, deep = FALSE)
    return(list(state = "reused", row_directory = row_directory, attempt = NULL))
  }
  attempt <- repository_runner_reserve_directory(row_directory,
    repository_runner_next_attempt(row_directory), "row attempt")
  list(state = "pending", row_directory = row_directory, attempt = attempt)
}

repository_runner_execute_attempt <- function(context, row, row_directory, attempt) {
  row_directory <- repository_runner_require_directory(row_directory, "row evidence")
  attempt <- repository_runner_require_directory(attempt, "row attempt")
  expected_row <- file.path(context$stage, "rows", repository_runner_row_name(row))
  if (!identical(row_directory, expected_row) ||
      !identical(dirname(attempt), row_directory) ||
      !grepl("^attempt-[0-9]{6}$", basename(attempt))) {
    repository_runner_fail("row worker received an attempt outside its selected scope")
  }
  worker_temp <- repository_runner_reserve_directory(attempt, "worker-tmp",
    "row worker temporary directory")
  old_temp_option <- getOption("paradox.repository_runner_tempdir")
  options(paradox.repository_runner_tempdir = worker_temp)
  on.exit({
    if (is.null(old_temp_option)) {
      options(paradox.repository_runner_tempdir = NULL)
    } else {
      options(paradox.repository_runner_tempdir = old_temp_option)
    }
    unlink(worker_temp, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  started <- Sys.time()
  message("Testing row ", row$position[[1L]], "/", nrow(context$selection), ": ",
    row$repository[[1L]])
  authentication <- repository_runner_authenticate_consumer(context$config, row)
  archive <- repository_runner_create_archive(authentication,
    file.path(attempt, "source.tar"))
  repository_runner_write_tsv(repository_runner_map_frame(c(
    git = authentication$git, git_sha256 = repository_runner_sha256(authentication$git),
    checkout = authentication$checkout, origin = authentication$origin,
    commit = authentication$commit, tree = authentication$tree,
    archive_sha256 = archive$sha256
  )), file.path(attempt, "source-authentication.tsv"))
  repository_runner_write_tsv(data.frame(path = archive$entries,
    stringsAsFactors = FALSE), file.path(attempt, "archive-entries.tsv"))
  pristine <- repository_runner_extract_archive(archive$path, attempt, "pristine")
  work <- repository_runner_extract_archive(archive$path, attempt, "work")
  source_tree <- repository_runner_validate_extraction(authentication, pristine$source)
  repository_runner_validate_extraction(authentication, work$source)
  repository_runner_write_tsv(source_tree, file.path(attempt, "source-tree.tsv"))
  repository_runner_require_file(file.path(work$source, "DESCRIPTION"),
    "consumer DESCRIPTION")
  pristine_manifest <- repository_runner_tree_manifest(pristine$source)
  work_before <- repository_runner_tree_manifest(work$source)
  if (!identical(pristine_manifest, work_before)) {
    repository_runner_fail("pristine and writable sources differ before testing")
  }
  repository_runner_write_tsv(pristine_manifest,
    file.path(attempt, "pristine-manifest.tsv"))
  repository_runner_write_tsv(work_before,
    file.path(attempt, "work-before-manifest.tsv"))
  framework <- repository_runner_framework(work$source)
  row_state <- repository_runner_reserve_directory(work$root, "row-state",
    "row-local state")
  environment <- repository_runner_process_environment(context, row_state)
  receipt <- repository_runner_environment_receipt(environment)
  repository_runner_write_tsv(receipt, file.path(attempt, "child-environment.tsv"))
  log <- file.path(attempt, "test.log")
  counts_path <- file.path(attempt, "test-counts.tsv")
  if (identical(framework, "none")) {
    writeLines("ordinary_upstream_tests=skipped_no_tests", log, useBytes = TRUE)
    process <- list(status = 0L, timed_out = FALSE, command = "-")
    repository_runner_write_tsv(repository_runner_map_frame(c(
      schema = "1", availability = "not_applicable", test_cases = "0",
      expectations = "0", passed = "0", failed = "0", skipped = "0",
      errors = "0", warnings = "0"
    )), counts_path)
  } else {
    process <- repository_runner_run_child(context, work$source, framework, log,
      counts_path, row_state, environment)
  }
  if (!file.exists(log)) writeLines("child produced no log", log, useBytes = TRUE)
  if (!file.exists(counts_path)) {
    availability <- if (isTRUE(process$timed_out)) "unavailable_timed_out" else
      "unavailable_interrupted"
    repository_runner_write_tsv(repository_runner_map_frame(
      repository_runner_unavailable_counts(availability)), counts_path)
  }
  counts <- repository_runner_validate_counts(counts_path, framework,
    process$status, process$timed_out)
  work_after <- repository_runner_tree_manifest(work$source)
  delta <- repository_runner_manifest_delta(work_before, work_after)
  repository_runner_write_tsv(work_after,
    file.path(attempt, "work-after-manifest.tsv"))
  repository_runner_write_tsv(delta, file.path(attempt, "post-test-delta.tsv"))
  log_lines <- readLines(log, warn = FALSE)
  log_text <- paste(log_lines, collapse = "\n")
  tail_text <- paste(utils::tail(log_lines, 80L), collapse = "\n")
  status <- if (framework == "none") {
    "skipped_no_tests"
  } else if (isTRUE(process$timed_out)) "timed_out" else if (process$status == 0L) {
    "passed"
  } else "failed"
  classification <- if (status == "passed") "passed" else if (status == "skipped_no_tests") {
    "harness_no_tests"
  } else repository_runner_classify_failure(log_text, isTRUE(process$timed_out))
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  unlink(c(pristine$root, work$root), recursive = TRUE, force = TRUE)
  if (dir.exists(pristine$root) || dir.exists(work$root)) {
    repository_runner_fail("could not prune disposable row sources and caches")
  }
  start_content <- repository_runner_read_tsv(file.path(context$inputs,
    "protected-content-start.tsv"), c("boundary", "kind", "position", "path", "sha256"),
    label = "protected content start")
  content <- stats::setNames(start_content$sha256, start_content$kind)
  extras <- content[startsWith(names(content), "extra_library_")]
  start_metadata <- repository_runner_read_tsv(file.path(context$inputs,
    "protected-metadata-start.tsv"), c("kind", "position", "path", "sha256"),
    label = "protected metadata start")
  metadata_sha <- repository_runner_object_sha256(start_metadata,
    "repository-runner-row-metadata-")
  error <- if (status %in% c("passed", "skipped_no_tests")) "-" else {
    repository_runner_compact_error(tail_text)
  }
  summary <- c(
    schema = "1", position = as.character(row$position[[1L]]),
    repository = row$repository[[1L]], priority = as.character(row$priority[[1L]]),
    origin = row$origin[[1L]], commit = row$commit[[1L]], tree = row$tree[[1L]],
    framework = framework, candidate_version = context$config$candidate_version,
    candidate_ref = context$config$candidate_ref,
    candidate_commit = context$config$candidate_commit,
    candidate_tree = context$config$candidate_tree,
    candidate_content_sha256 = context$config$candidate_content_sha256,
    candidate_library_sha256 = content[["candidate_library"]],
    dependency_library_sha256 = content[["dependency_library"]],
    extra_library_sha256 = if (length(extras)) paste(extras, collapse = ";") else "-",
    not_cran = "true", timeout_seconds = format(context$config$timeout_seconds,
      scientific = FALSE, trim = TRUE), process_exit_status = as.character(process$status),
    process_timed_out = if (isTRUE(process$timed_out)) "true" else "false",
    status = status, classification = classification,
    elapsed_seconds = format(elapsed, digits = 15L, scientific = FALSE, trim = TRUE),
    mutation_added = as.character(sum(delta$change == "added")),
    mutation_modified = as.character(sum(delta$change == "modified")),
    mutation_deleted = as.character(sum(delta$change == "deleted")),
    archive_sha256 = archive$sha256,
    source_authentication_sha256 = repository_runner_sha256(file.path(attempt,
      "source-authentication.tsv")),
    archive_entries_sha256 = repository_runner_sha256(file.path(attempt,
      "archive-entries.tsv")),
    source_tree_sha256 = repository_runner_sha256(file.path(attempt, "source-tree.tsv")),
    pristine_manifest_sha256 = repository_runner_sha256(file.path(attempt,
      "pristine-manifest.tsv")),
    work_before_manifest_sha256 = repository_runner_sha256(file.path(attempt,
      "work-before-manifest.tsv")),
    work_after_manifest_sha256 = repository_runner_sha256(file.path(attempt,
      "work-after-manifest.tsv")), delta_sha256 = repository_runner_sha256(file.path(
      attempt, "post-test-delta.tsv")), test_log_sha256 = repository_runner_sha256(log),
    test_counts_sha256 = repository_runner_sha256(counts_path),
    child_environment_sha256 = repository_runner_sha256(file.path(attempt,
      "child-environment.tsv")), test_count_availability = counts[["availability"]],
    test_cases = counts[["test_cases"]], test_expectations = counts[["expectations"]],
    test_passed = counts[["passed"]], test_failed = counts[["failed"]],
    test_skipped = counts[["skipped"]], test_errors = counts[["errors"]],
    test_warnings = counts[["warnings"]],
    protected_metadata_sha256_expected = metadata_sha,
    protected_metadata_sha256_pre_child = "-",
    protected_metadata_sha256_post_child = "-",
    candidate_sentinel_sha256_pre_child = "-",
    candidate_sentinel_sha256_post_child = "-",
    tool_inputs_sha256_pre_child = "-",
    tool_inputs_sha256_post_child = "-",
    candidate_authentication_sha256_pre_child = "-",
    candidate_authentication_sha256_post_child = "-",
    test_command = process$command, error = error
  )
  unlink(worker_temp, recursive = TRUE, force = TRUE)
  if (dir.exists(worker_temp)) {
    repository_runner_fail("could not prune row worker temporary directory")
  }
  invisible(list(status = status, summary = summary))
}

repository_runner_scheduler_roots <- function(context) {
  scheduler <- repository_runner_require_directory(file.path(context$stage,
    "scheduler"), "resource scheduler evidence root")
  list(root = scheduler,
    waves = repository_runner_require_directory(file.path(scheduler, "waves"),
      "scheduler wave root"),
    quarantine = repository_runner_require_directory(file.path(scheduler,
      "quarantine"), "scheduler quarantine root"))
}

repository_runner_boundary_fields <- function() c(
  "schema", "status", "tool_sha256", "candidate_authentication_sha256",
  "protected_metadata_sha256", "candidate_sentinel_sha256", "error"
)

repository_runner_boundary_values <- function(value) {
  if (!is.list(value) || any(!c("tool_sha256",
      "candidate_authentication_sha256", "protected_metadata_sha256",
      "candidate_sentinel_sha256", "violations", "errors") %in% names(value))) {
    repository_runner_fail("protected-input boundary result is malformed")
  }
  detail <- c(value$violations, unname(value$errors))
  detail <- detail[nzchar(detail)]
  c(schema = "1", status = if (length(value$violations)) "failed" else "passed",
    tool_sha256 = value$tool_sha256,
    candidate_authentication_sha256 = value$candidate_authentication_sha256,
    protected_metadata_sha256 = value$protected_metadata_sha256,
    candidate_sentinel_sha256 = value$candidate_sentinel_sha256,
    error = if (length(detail)) repository_runner_compact_error(
      paste(detail, collapse = "; ")) else "-")
}

repository_runner_capture_boundary <- function(context) {
  value <- tryCatch(repository_runner_capture_protection(context),
    error = function(condition) NULL)
  if (is.null(value)) {
    return(c(schema = "1", status = "failed", tool_sha256 = "-",
      candidate_authentication_sha256 = "-", protected_metadata_sha256 = "-",
      candidate_sentinel_sha256 = "-", error =
        "protected-input boundary capture raised an error"))
  }
  repository_runner_boundary_values(value)
}

repository_runner_boundaries_match <- function(before, after) {
  fields <- c("tool_sha256", "candidate_authentication_sha256",
    "protected_metadata_sha256", "candidate_sentinel_sha256")
  identical(before[["status"]], "passed") &&
    identical(after[["status"]], "passed") &&
    identical(unname(before[fields]), unname(after[fields]))
}

repository_runner_quarantine_scheduler_path <- function(context, path, reason) {
  roots <- repository_runner_scheduler_roots(context)
  path <- repository_runner_require_directory(path, "interrupted scheduler evidence")
  original <- basename(path)
  label <- gsub("[^A-Za-z0-9._-]", "-", original)
  if (!grepl("^[A-Za-z0-9]", label)) label <- paste0("external-", label)
  existing <- list.files(roots$quarantine, pattern = paste0("^",
    label, "-interrupted-[0-9]{6}$"))
  number <- if (length(existing)) max(as.integer(sub("^.*-", "", existing))) + 1L else 1L
  wrapper <- repository_runner_reserve_directory(roots$quarantine,
    sprintf("%s-interrupted-%06d", label, number),
    "scheduler interruption quarantine")
  payload <- file.path(wrapper, "payload")
  if (!file.rename(path, payload)) {
    repository_runner_fail("could not quarantine interrupted scheduler evidence")
  }
  repository_runner_write_tsv(repository_runner_map_frame(c(
    schema = "1", original = original, reason = reason,
    quarantined_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )), file.path(wrapper, "quarantine.tsv"))
  repository_runner_seal_directory(wrapper)
  invisible(wrapper)
}

repository_runner_recover_scheduler <- function(context) {
  roots <- repository_runner_scheduler_roots(context)
  paths <- list.files(roots$waves, pattern = "^wave-[0-9]{6}$",
    full.names = TRUE)
  for (path in paths) {
    manifest <- file.path(path, ".repository-runner-manifest.tsv")
    seal <- file.path(path, ".repository-runner-seal")
    present <- file.exists(c(manifest, seal))
    if (all(present)) {
      repository_runner_verify_sealed_directory(path)
    } else {
      repository_runner_quarantine_scheduler_path(context, path,
        "interrupted_before_wave_seal")
    }
  }
  invisible(TRUE)
}

repository_runner_parent_lock_path <- function(context) file.path(
  dirname(context$stage), paste0(".", basename(context$stage),
    ".repository-parent-lock"))

repository_runner_parent_lock_acquire <- function(context) {
  if (!requireNamespace("ps", quietly = TRUE)) {
    repository_runner_fail("ps is required for repository parent locking")
  }
  repository_runner_scheduler_roots(context)
  lock <- repository_runner_parent_lock_path(context)
  if (dir.exists(lock)) {
    lock <- repository_runner_require_directory(lock, "repository parent lock")
    owner_path <- file.path(lock, "owner.tsv")
    active <- TRUE
    owner <- tryCatch(repository_runner_read_map(owner_path,
      c("schema", "pid", "create_time", "run_id", "token"),
      "repository parent lock owner"), error = function(...) NULL)
    if (is.null(owner) || !grepl("^[1-9][0-9]*$", owner[["pid"]]) ||
        !grepl("^[0-9]+([.][0-9]+)?$", owner[["create_time"]])) {
      active <- FALSE
    } else {
      pid <- as.integer(owner[["pid"]])
      handle <- tryCatch(ps::ps_handle(pid), error = function(...) NULL)
      active <- !is.null(handle) && isTRUE(tryCatch(ps::ps_is_running(handle),
        error = function(...) FALSE)) && identical(
          format(as.numeric(ps::ps_create_time(handle)), digits = 17L,
            scientific = FALSE, trim = TRUE), owner[["create_time"]])
    }
    if (active) repository_runner_fail("another repository parent owns the stage lock")
    repository_runner_quarantine_scheduler_path(context, lock,
      "stale_repository_parent_lock")
  } else if (file.exists(lock) || repository_runner_is_symbolic(lock)) {
    repository_runner_fail("repository parent lock path is unsafe")
  }
  if (!dir.create(lock, recursive = FALSE, showWarnings = FALSE)) {
    repository_runner_fail("could not atomically acquire repository parent lock")
  }
  handle <- ps::ps_handle(Sys.getpid())
  create_time <- format(as.numeric(ps::ps_create_time(handle)), digits = 17L,
    scientific = FALSE, trim = TRUE)
  token <- repository_runner_object_sha256(c(pid = as.character(Sys.getpid()),
    create_time = create_time, run_id = context$config$run_id,
    stage = context$stage), "repository-parent-lock-")
  repository_runner_write_tsv(repository_runner_map_frame(c(schema = "1",
    pid = as.character(Sys.getpid()), create_time = create_time,
    run_id = context$config$run_id, token = token)), file.path(lock, "owner.tsv"))
  list(path = lock, token = token)
}

repository_runner_parent_lock_release <- function(lock) {
  if (is.null(lock) || !is.list(lock) || !identical(names(lock), c("path", "token"))) {
    repository_runner_fail("repository parent lock handle is malformed")
  }
  path <- repository_runner_require_directory(lock$path, "repository parent lock")
  owner <- repository_runner_read_map(file.path(path, "owner.tsv"),
    c("schema", "pid", "create_time", "run_id", "token"),
    "repository parent lock owner")
  if (!identical(owner[["token"]], lock$token) ||
      unlink(path, recursive = TRUE, force = FALSE) != 0L || dir.exists(path)) {
    repository_runner_fail("could not release the exact repository parent lock")
  }
  invisible(TRUE)
}

repository_runner_assert_parent_lock <- function(context) {
  if (!requireNamespace("ps", quietly = TRUE)) {
    repository_runner_fail("ps is required for repository parent locking")
  }
  path <- repository_runner_require_directory(repository_runner_parent_lock_path(
    context), "repository parent lock")
  owner <- repository_runner_read_map(file.path(path, "owner.tsv"),
    c("schema", "pid", "create_time", "run_id", "token"),
    "repository parent lock owner")
  handle <- tryCatch(ps::ps_handle(Sys.getpid()), error = function(...) NULL)
  create_time <- if (is.null(handle)) "-" else format(as.numeric(
    ps::ps_create_time(handle)), digits = 17L, scientific = FALSE, trim = TRUE)
  if (!identical(owner[["schema"]], "1") ||
      !identical(owner[["pid"]], as.character(Sys.getpid())) ||
      !identical(owner[["create_time"]], create_time) ||
      !identical(owner[["run_id"]], context$config$run_id) ||
      !grepl("^[0-9a-f]{64}$", owner[["token"]])) {
    repository_runner_fail("current process does not own the repository parent lock")
  }
  invisible(owner)
}

repository_runner_next_wave_name <- function(context) {
  roots <- repository_runner_scheduler_roots(context)
  names <- c(list.files(roots$waves), list.files(roots$quarantine))
  names <- names[grepl("^wave-[0-9]{6}", names)]
  numbers <- if (length(names)) as.integer(substr(names, 6L, 11L)) else 0L
  sprintf("wave-%06d", max(numbers) + 1L)
}

repository_runner_begin_wave <- function(context, decision, rows, prepared,
                                         parent_before,
                                         operator_max = NULL) {
  roots <- repository_runner_scheduler_roots(context)
  wave <- repository_runner_reserve_directory(roots$waves,
    repository_runner_next_wave_name(context), "scheduler wave")
  repository_runner_write_tsv(decision$automatic_table, file.path(wave,
    "resource-automatic-report.tsv"))
  repository_runner_write_tsv(decision$table, file.path(wave,
    "resource-report.tsv"))
  plan <- data.frame(position = rows$position, repository = rows$repository,
    attempt = vapply(prepared, function(value) substring(value$attempt,
      nchar(context$stage) + 2L), character(1L)), stringsAsFactors = FALSE)
  repository_runner_write_tsv(plan, file.path(wave, "plan.tsv"))
  repository_runner_write_tsv(repository_runner_map_frame(parent_before),
    file.path(wave, "parent-pre-boundary.tsv"))
  values <- c(schema = "1", wave = basename(wave),
    started_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    jobs = as.character(decision$jobs),
    automatic_jobs = as.character(decision$automatic_jobs),
    retained_initial_jobs = as.character(decision$retained_jobs),
    selection = decision$selection,
    operator_max_jobs = if (is.null(operator_max)) "none" else
      as.character(operator_max),
    applied_max_jobs = decision$values[["operator_max_jobs"]],
    resource_helper = decision$helper,
    resource_helper_sha256 = decision$helper_sha256,
    resource_automatic_report_sha256 = repository_runner_sha256(file.path(wave,
      "resource-automatic-report.tsv")),
    resource_report_sha256 = repository_runner_sha256(file.path(wave,
      "resource-report.tsv")), plan_sha256 = repository_runner_sha256(file.path(
      wave, "plan.tsv")), parent_pre_boundary_sha256 = repository_runner_sha256(
      file.path(wave, "parent-pre-boundary.tsv")), rows = as.character(nrow(rows)))
  repository_runner_write_tsv(repository_runner_map_frame(values),
    file.path(wave, "decision.tsv"))
  list(path = wave, plan = plan, decision = values)
}

repository_runner_execute_worker <- function(context, row, prepared, worker) {
  tryCatch(list(ok = TRUE, value = worker(context, row,
    prepared$row_directory, prepared$attempt)), error = function(condition) list(
      ok = FALSE, value = NULL, error = repository_runner_compact_error(
        conditionMessage(condition))))
}

repository_runner_finalize_attempt <- function(context, row, prepared,
                                               worker_result, before, after) {
  if (!isTRUE(worker_result$ok) || !is.list(worker_result$value) ||
      !identical(names(worker_result$value), c("status", "summary")) ||
      !is.character(worker_result$value$status) ||
      length(worker_result$value$status) != 1L ||
      !identical(names(worker_result$value$summary), repository_runner_row_fields())) {
    repository_runner_fail("row worker returned a malformed completion payload")
  }
  summary <- worker_result$value$summary
  summary[c("protected_metadata_sha256_pre_child",
    "candidate_sentinel_sha256_pre_child", "tool_inputs_sha256_pre_child",
    "candidate_authentication_sha256_pre_child")] <- before[c(
      "protected_metadata_sha256", "candidate_sentinel_sha256", "tool_sha256",
      "candidate_authentication_sha256")]
  summary[c("protected_metadata_sha256_post_child",
    "candidate_sentinel_sha256_post_child", "tool_inputs_sha256_post_child",
    "candidate_authentication_sha256_post_child")] <- after[c(
      "protected_metadata_sha256", "candidate_sentinel_sha256", "tool_sha256",
      "candidate_authentication_sha256")]
  repository_runner_write_tsv(repository_runner_map_frame(summary), file.path(
    prepared$attempt, "summary.tsv"))
  evidence <- repository_runner_seal_directory(prepared$attempt)
  validation <- repository_runner_verify_attempt(context, row,
    attempt_override = prepared$attempt, deep = TRUE)
  list(status = worker_result$value$status, evidence = evidence,
    validation = validation)
}

repository_runner_worker_environment <- function(state) {
  state <- repository_runner_require_directory(state,
    "external repository worker state")
  names <- c("home", "tmp", "xdg-cache", "xdg-config", "xdg-data",
    "xdg-state", "xdg-runtime")
  paths <- file.path(state, names)
  if (!all(vapply(paths, dir.create, logical(1L), recursive = FALSE,
      showWarnings = FALSE))) {
    repository_runner_fail("could not reserve external worker isolation")
  }
  Sys.chmod(paths[[7L]], "0700")
  environment <- Sys.getenv()
  overrides <- c(HOME = paths[[1L]], TMPDIR = paths[[2L]],
    XDG_CACHE_HOME = paths[[3L]], XDG_CONFIG_HOME = paths[[4L]],
    XDG_DATA_HOME = paths[[5L]], XDG_STATE_HOME = paths[[6L]],
    XDG_RUNTIME_DIR = paths[[7L]], R_PROFILE_USER = "", R_ENVIRON_USER = "",
    TESTTHAT_PARALLEL = "false", TESTTHAT_CPUS = "1", MAKEFLAGS = "-j1",
    repository_runner_nested_parallel_environment(),
    OMP_NUM_THREADS = "1", OMP_THREAD_LIMIT = "1", OPENBLAS_NUM_THREADS = "1",
    GOTO_NUM_THREADS = "1", MKL_NUM_THREADS = "1", BLIS_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1",
    RCPP_PARALLEL_NUM_THREADS = "1", PYTHONDONTWRITEBYTECODE = "1",
    PYTHONNOUSERSITE = "1")
  environment[names(overrides)] <- unname(overrides)
  environment
}

repository_runner_run_workers <- function(context, rows, prepared,
                                          fixture = NULL) {
  worker_script <- context$config$tool_files[["repository-wave-worker.R"]]
  if (is.null(worker_script)) {
    repository_runner_fail("runner tool inputs omit the external wave worker")
  }
  worker_script <- repository_runner_require_file(worker_script,
    "external repository wave worker")
  processes <- vector("list", nrow(rows))
  states <- vector("character", nrow(rows))
  results <- vector("character", nrow(rows))
  active <- TRUE
  on.exit({
    if (active) {
      for (process in processes) {
        if (!is.null(process) && isTRUE(tryCatch(process$is_alive(),
            error = function(...) FALSE))) {
          suppressWarnings(try(process$kill_tree(), silent = TRUE))
        }
      }
      for (process in processes) {
        if (!is.null(process)) suppressWarnings(try(process$wait(5000),
          silent = TRUE))
      }
    }
  }, add = TRUE)
  for (index in seq_len(nrow(rows))) {
    states[[index]] <- repository_runner_reserve_directory(
      prepared[[index]]$attempt, "external-worker-state",
      "external repository worker state")
    environment <- repository_runner_worker_environment(states[[index]])
    spec <- list(runner = context$config$tool_files[["repository-runner.R"]],
      context = context, row = rows[index, , drop = FALSE],
      prepared = prepared[[index]], fixture = fixture)
    spec_path <- file.path(states[[index]], "spec.rds")
    results[[index]] <- file.path(states[[index]], "result.rds")
    saveRDS(spec, spec_path, version = 3L)
    log <- file.path(prepared[[index]]$attempt, "worker-transport.log")
    processes[[index]] <- processx::process$new(context$config$rscript,
      c("--vanilla", worker_script, spec_path, results[[index]]),
      env = environment, stdout = log, stderr = "2>&1", cleanup = TRUE,
      cleanup_tree = TRUE, supervise = TRUE, windows_verbatim_args = TRUE)
  }
  for (process in processes) process$wait(-1)
  active <- FALSE
  values <- lapply(seq_len(nrow(rows)), function(index) {
    status <- tryCatch(processes[[index]]$get_exit_status(),
      error = function(...) NA_integer_)
    if (!identical(status, 0L) || !file.exists(results[[index]])) {
      return(list(ok = FALSE, value = NULL,
        error = paste0("external_worker_exit_", if (is.na(status)) "unknown" else
          status)))
    }
    value <- tryCatch(readRDS(results[[index]]), error = function(...) NULL)
    if (is.null(value) || !is.list(value) || is.null(value$ok)) {
      list(ok = FALSE, value = NULL, error = "external_worker_malformed_result")
    } else value
  })
  for (state in states) unlink(state, recursive = TRUE, force = TRUE)
  if (any(dir.exists(states))) {
    repository_runner_fail("could not prune external worker transport state")
  }
  values
}

repository_runner_run_wave <- function(context, rows, prepared, decision,
                                       operator_max = NULL,
                                       worker = repository_runner_execute_attempt,
                                       worker_fixture = NULL) {
  if (.Platform$OS.type != "unix") {
    repository_runner_fail("bounded repository waves require Unix process supervision")
  }
  if (!is.function(worker) || !identical(worker,
      repository_runner_execute_attempt) ||
      nrow(rows) != length(prepared) || !nrow(rows) ||
      nrow(rows) > decision$jobs) {
    repository_runner_fail("repository wave inputs are malformed or exceed its ceiling")
  }
  repository_runner_assert_parent_lock(context)
  parent_before <- repository_runner_capture_boundary(context)
  if (!identical(parent_before[["status"]], "passed")) {
    repository_runner_fail("parent pre-wave protected-input boundary failed: ",
      parent_before[["error"]])
  }
  wave <- repository_runner_begin_wave(context, decision, rows, prepared,
    parent_before, operator_max)
  worker_results <- repository_runner_run_workers(context, rows, prepared,
    worker_fixture)
  worker_results <- lapply(worker_results, function(value) {
    if (is.null(value) || !is.list(value) || is.null(value$ok)) {
      list(ok = FALSE, value = NULL, error = "worker_terminated_without_result")
    } else value
  })

  repository_runner_assert_parent_lock(context)
  parent_after <- repository_runner_capture_boundary(context)
  boundary_passed <- repository_runner_boundaries_match(parent_before, parent_after)
  if (!boundary_passed && identical(parent_after[["status"]], "passed")) {
    parent_after[["status"]] <- "failed"
    parent_after[["error"]] <- "protected-input identities changed across the wave"
  }
  repository_runner_write_tsv(repository_runner_map_frame(parent_after),
    file.path(wave$path, "parent-post-boundary.tsv"))

  result_rows <- vector("list", nrow(rows))
  for (index in seq_len(nrow(rows))) {
    finalized <- NULL
    finalization_error <- "-"
    if (boundary_passed && isTRUE(worker_results[[index]]$ok)) {
      finalized <- tryCatch(repository_runner_finalize_attempt(context,
        rows[index, , drop = FALSE], prepared[[index]], worker_results[[index]],
        parent_before, parent_after), error = function(condition) {
          finalization_error <<- repository_runner_compact_error(
            conditionMessage(condition))
          NULL
        })
    }
    worker_error <- if (isTRUE(worker_results[[index]]$ok)) "-" else {
      value <- worker_results[[index]]$error
      if (is.null(value) || !length(value) || !nzchar(value[[1L]]))
        "worker_failed_without_error" else value[[1L]]
    }
    state <- if (!boundary_passed) "boundary_failed" else if
      (!isTRUE(worker_results[[index]]$ok)) "worker_failed" else if
      (is.null(finalized)) "finalization_failed" else "ready"
    result_rows[[index]] <- data.frame(position = rows$position[[index]],
      repository = rows$repository[[index]], state = state,
      worker_status = if (isTRUE(worker_results[[index]]$ok)) "completed" else "failed",
      attempt = wave$plan$attempt[[index]],
      attempt_manifest_sha256 = if (is.null(finalized)) "-" else
        finalized$evidence$manifest_sha256,
      attempt_seal_sha256 = if (is.null(finalized)) "-" else
        finalized$evidence$seal_sha256,
      worker_error = worker_error, finalization_error = finalization_error,
      stringsAsFactors = FALSE)
  }
  result <- do.call(rbind, result_rows)
  repository_runner_write_tsv(result, file.path(wave$path, "result.tsv"))
  repository_runner_write_tsv(repository_runner_map_frame(c(schema = "1",
    wave = basename(wave$path),
    finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    rows = as.character(nrow(result)), ready = as.character(sum(
      result$state == "ready")), failed = as.character(sum(
      result$state != "ready")), result_sha256 = repository_runner_sha256(
      file.path(wave$path, "result.tsv")), parent_pre_boundary_sha256 =
      repository_runner_sha256(file.path(wave$path, "parent-pre-boundary.tsv")),
    parent_post_boundary_sha256 =
      repository_runner_sha256(file.path(wave$path, "parent-post-boundary.tsv"))
  )), file.path(wave$path, "completion.tsv"))
  repository_runner_seal_directory(wave$path)
  verified <- repository_runner_verify_wave(context, wave$path)
  promoted <- repository_runner_promote_wave(context, verified)
  invisible(list(result = result, wave = wave$path, promoted = promoted,
    failed = any(result$state != "ready")))
}

repository_runner_run_rows_bounded <- function(context, rows,
                                               operator_max = NULL,
                                               worker = repository_runner_execute_attempt,
                                               worker_fixture = NULL) {
  rows <- repository_runner_selection(rows)
  if (!identical(rows$position, context$selection$position) ||
      !identical(rows$repository, context$selection$repository)) {
    repository_runner_fail("bounded scheduler rows differ from the retained selection")
  }
  repository_runner_assert_parent_lock(context)
  repository_runner_recover_scheduler(context)
  repository_runner_resume_ready_waves(context)
  outcomes <- rep("pending", nrow(rows))
  for (index in seq_len(nrow(rows))) {
    row_directory <- file.path(context$stage, "rows",
      repository_runner_row_name(rows[index, , drop = FALSE]))
    if (dir.exists(row_directory) && file.exists(file.path(row_directory,
        "accepted.tsv"))) {
      repository_runner_prepare_row(context, rows[index, , drop = FALSE])
      outcomes[[index]] <- "reused"
      message("Skipping accepted row ", rows$position[[index]], ": ",
        rows$repository[[index]])
    }
  }
  while (any(outcomes == "pending")) {
    repository_runner_verify_current_tools(context)
    decision <- repository_runner_resource_decision(context$config, operator_max,
      retained_max = context$initial_resource$jobs)
    helper_index <- match("resource-jobs", context$tool_receipt$name)
    if (is.na(helper_index) || !identical(decision$helper_sha256,
        context$tool_receipt$sha256[[helper_index]])) {
      repository_runner_fail("resource scheduler differs from sealed tool identity")
    }
    indices <- which(outcomes == "pending")
    indices <- utils::head(indices, decision$jobs)
    wave_rows <- rows[indices, , drop = FALSE]
    prepared <- lapply(seq_along(indices), function(offset)
      repository_runner_prepare_row(context,
        wave_rows[offset, , drop = FALSE]))
    if (any(vapply(prepared, function(value) value$state != "pending",
        logical(1L)))) {
      repository_runner_fail("row acceptance changed while reserving a wave")
    }
    wave <- repository_runner_run_wave(context, wave_rows, prepared, decision,
      operator_max, worker, worker_fixture)
    accepted <- wave$promoted
    outcomes[indices[accepted]] <- "accepted"
    if (isTRUE(wave$failed)) {
      repository_runner_fail("repository wave retained ", sum(!accepted),
        " failed row(s); resume reruns only those rows")
    }
  }
  outcomes
}

repository_runner_verify_wave <- function(context, path) {
  evidence <- repository_runner_verify_sealed_directory(path)
  inventory <- sort(list.files(path, all.files = TRUE, no.. = TRUE),
    method = "radix")
  expected_inventory <- sort(c(".repository-runner-manifest.tsv",
    ".repository-runner-seal", "completion.tsv", "decision.tsv",
    "parent-post-boundary.tsv", "parent-pre-boundary.tsv", "plan.tsv",
    "resource-automatic-report.tsv", "resource-report.tsv", "result.tsv"),
    method = "radix")
  if (!identical(inventory, expected_inventory)) {
    repository_runner_fail("scheduler wave has an unexpected inventory")
  }
  decision <- repository_runner_read_map(file.path(path, "decision.tsv"),
    c("schema", "wave", "started_utc", "jobs", "automatic_jobs",
      "retained_initial_jobs", "selection", "operator_max_jobs",
      "applied_max_jobs", "resource_helper", "resource_helper_sha256",
      "resource_automatic_report_sha256", "resource_report_sha256",
      "plan_sha256", "parent_pre_boundary_sha256", "rows"),
    "scheduler wave decision")
  automatic_table <- repository_runner_read_tsv(file.path(path,
    "resource-automatic-report.tsv"), c("field", "value"),
    label = "scheduler wave automatic resource report")
  automatic <- repository_runner_parse_resource_report(c("field\tvalue", paste(
    automatic_table$field, automatic_table$value, sep = "\t")))
  report_table <- repository_runner_read_tsv(file.path(path,
    "resource-report.tsv"), c("field", "value"),
    label = "scheduler wave resource report")
  report <- repository_runner_parse_resource_report(c("field\tvalue", paste(
    report_table$field, report_table$value, sep = "\t")))
  plan <- repository_runner_read_tsv(file.path(path, "plan.tsv"),
    c("position", "repository", "attempt"), label = "scheduler wave plan")
  result <- repository_runner_read_tsv(file.path(path, "result.tsv"),
    c("position", "repository", "state", "worker_status", "attempt",
      "attempt_manifest_sha256", "attempt_seal_sha256", "worker_error",
      "finalization_error"),
    label = "scheduler wave result")
  before <- repository_runner_read_map(file.path(path,
    "parent-pre-boundary.tsv"), repository_runner_boundary_fields(),
    "wave parent pre-boundary")
  after <- repository_runner_read_map(file.path(path,
    "parent-post-boundary.tsv"), repository_runner_boundary_fields(),
    "wave parent post-boundary")
  completion <- repository_runner_read_map(file.path(path, "completion.tsv"),
    c("schema", "wave", "finished_utc", "rows", "ready", "failed",
      "result_sha256", "parent_pre_boundary_sha256",
      "parent_post_boundary_sha256"),
    "scheduler wave completion")
  helper_index <- match("resource-jobs", context$tool_receipt$name)
  jobs <- suppressWarnings(as.integer(decision[["jobs"]]))
  automatic_jobs <- suppressWarnings(as.integer(decision[["automatic_jobs"]]))
  retained_jobs <- suppressWarnings(as.integer(decision[["retained_initial_jobs"]]))
  expected_completion <- c(schema = "1", wave = basename(path),
    rows = as.character(nrow(result)), ready = as.character(sum(
      result$state == "ready")), failed = as.character(sum(
      result$state != "ready")), result_sha256 = repository_runner_sha256(
      file.path(path, "result.tsv")), parent_pre_boundary_sha256 =
      repository_runner_sha256(file.path(path, "parent-pre-boundary.tsv")),
    parent_post_boundary_sha256 =
      repository_runner_sha256(file.path(path, "parent-post-boundary.tsv")))
  if (is.na(helper_index) || !identical(decision[["schema"]], "1") ||
      !identical(decision[["wave"]], basename(path)) || is.na(jobs) ||
      is.na(automatic_jobs) || is.na(retained_jobs) || jobs < 1L || jobs > 4L ||
      automatic_jobs < jobs || retained_jobs < jobs ||
      retained_jobs != context$initial_resource$jobs ||
      nrow(plan) < 1L || nrow(plan) > jobs ||
      !identical(decision[["rows"]], as.character(nrow(plan))) ||
      !identical(decision[["jobs"]], as.character(report$jobs)) ||
      !identical(decision[["automatic_jobs"]], as.character(automatic$jobs)) ||
      !(decision[["selection"]] %in% c("automatic",
        "retained_initial_ceiling", "operator_conservative_override")) ||
      !identical(decision[["resource_helper"]],
        context$tool_receipt$path[[helper_index]]) ||
      !identical(decision[["resource_helper_sha256"]],
        context$tool_receipt$sha256[[helper_index]]) ||
      !identical(decision[["resource_automatic_report_sha256"]],
        repository_runner_sha256(file.path(path,
          "resource-automatic-report.tsv"))) ||
      !identical(decision[["resource_report_sha256"]],
        repository_runner_sha256(file.path(path, "resource-report.tsv"))) ||
      !identical(decision[["plan_sha256"]],
        repository_runner_sha256(file.path(path, "plan.tsv"))) ||
      !identical(decision[["parent_pre_boundary_sha256"]],
        repository_runner_sha256(file.path(path, "parent-pre-boundary.tsv"))) ||
      !identical(plan[, c("position", "repository", "attempt")],
        result[, c("position", "repository", "attempt")]) ||
      any(!result$state %in% c("ready", "worker_failed",
        "finalization_failed", "boundary_failed")) ||
      any(!result$worker_status %in% c("completed", "failed")) ||
      !identical(before[["schema"]], "1") ||
      !identical(after[["schema"]], "1") ||
      !(before[["status"]] %in% c("passed", "failed")) ||
      !(after[["status"]] %in% c("passed", "failed")) ||
      !identical(before[["status"]], "passed") ||
      !identical(unname(completion[names(expected_completion)]),
        unname(expected_completion)) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        decision[["started_utc"]]) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        completion[["finished_utc"]])) {
    repository_runner_fail("scheduler wave evidence is semantically inconsistent")
  }
  selected <- match(plan$position, context$selection$position)
  expected_attempt <- if (anyNA(selected)) character() else vapply(
    seq_len(nrow(plan)), function(index) file.path("rows",
      repository_runner_row_name(context$selection[selected[[index]], , drop = FALSE]),
      basename(plan$attempt[[index]])), character(1L))
  if (anyNA(selected) || anyDuplicated(plan$position) ||
      anyDuplicated(plan$repository) || anyDuplicated(plan$attempt) ||
      !identical(plan$repository, context$selection$repository[selected]) ||
      any(!grepl("^rows/[A-Za-z0-9._-]+/attempt-[0-9]{6}$", plan$attempt)) ||
      !identical(plan$attempt, expected_attempt)) {
    repository_runner_fail("scheduler wave plan escaped or duplicated its selection")
  }
  boundary_passed <- repository_runner_boundaries_match(before, after)
  ready <- result$state == "ready"
  state_checks <- c(
    ready = all(!ready | (result$worker_status == "completed" &
      result$worker_error == "-" & result$finalization_error == "-" &
      grepl("^[0-9a-f]{64}$", result$attempt_manifest_sha256) &
      grepl("^[0-9a-f]{64}$", result$attempt_seal_sha256))),
    failed_hashes = all(ready | (result$attempt_manifest_sha256 == "-" &
      result$attempt_seal_sha256 == "-")),
    worker_failed = all(result$state != "worker_failed" |
      (result$worker_status == "failed" & result$worker_error != "-")),
    boundary_failed = all(result$state != "boundary_failed" |
      (result$worker_status == "completed" & !boundary_passed)),
    finalization_failed = all(result$state != "finalization_failed" |
      (result$worker_status == "completed" & boundary_passed &
       result$finalization_error != "-")),
    boundary_relation = identical(any(ready), FALSE) || boundary_passed,
    boundary_failure_relation = boundary_passed ||
      all(result$state == "boundary_failed" | result$state == "worker_failed")
  )
  if (any(!state_checks)) {
    repository_runner_fail("scheduler wave result states are inconsistent: ",
      paste(names(state_checks)[!state_checks], collapse = ", "))
  }
  for (index in which(ready)) {
    row_index <- match(result$position[[index]], context$selection$position)
    validation <- repository_runner_verify_attempt(context,
      context$selection[row_index, , drop = FALSE],
      attempt_override = file.path(context$stage, result$attempt[[index]]),
      deep = FALSE)
    if (!identical(result$attempt_manifest_sha256[[index]],
        validation$evidence$manifest_sha256) ||
        !identical(result$attempt_seal_sha256[[index]],
          validation$evidence$seal_sha256)) {
      repository_runner_fail("scheduler wave attempt hashes differ from the row")
    }
  }
  list(path = path, evidence = evidence, decision = decision, plan = plan,
    result = result, before = before, after = after, completion = completion)
}

repository_runner_acceptance_matches_wave <- function(validation, wave,
                                                      index) {
  accepted <- validation$accepted
  identical(accepted[["wave"]], basename(wave$path)) &&
    identical(accepted[["wave_manifest_sha256"]],
      wave$evidence$manifest_sha256) &&
    identical(accepted[["wave_seal_sha256"]], wave$evidence$seal_sha256) &&
    identical(accepted[["attempt"]], basename(file.path(
      wave$result$attempt[[index]])))
}

repository_runner_promote_wave <- function(context, wave) {
  repository_runner_assert_parent_lock(context)
  ready <- which(wave$result$state == "ready")
  promoted <- rep(FALSE, nrow(wave$result))
  for (index in ready) {
    row_index <- match(wave$result$position[[index]], context$selection$position)
    row <- context$selection[row_index, , drop = FALSE]
    row_directory <- repository_runner_require_directory(file.path(context$stage,
      "rows", repository_runner_row_name(row)), "wave row evidence")
    repository_runner_quarantine_partial_acceptance(row_directory, fail = FALSE)
    if (file.exists(file.path(row_directory, "accepted.tsv"))) {
      validation <- repository_runner_verify_attempt(context, row, deep = FALSE)
      if (!repository_runner_acceptance_matches_wave(validation, wave, index)) {
        repository_runner_fail("accepted row is not bound to its sealed scheduler wave")
      }
    } else {
      attempt <- repository_runner_require_directory(file.path(context$stage,
        wave$result$attempt[[index]]), "ready wave attempt")
      repository_runner_promote_attempt(context, row, row_directory, attempt,
        validator = function(path) repository_runner_verify_attempt(context, row,
          attempt_override = path, deep = FALSE), wave = wave)
    }
    promoted[[index]] <- TRUE
  }
  promoted
}

repository_runner_resume_ready_waves <- function(context) {
  roots <- repository_runner_scheduler_roots(context)
  waves <- sort(list.files(roots$waves, pattern = "^wave-[0-9]{6}$",
    full.names = TRUE), method = "radix")
  for (path in waves) {
    repository_runner_promote_wave(context, repository_runner_verify_wave(context,
      path))
  }
  invisible(TRUE)
}

repository_runner_scheduler_ledger <- function(context,
                                               require_all_accepted = TRUE) {
  roots <- repository_runner_scheduler_roots(context)
  waves <- sort(list.files(roots$waves, pattern = "^wave-[0-9]{6}$",
    full.names = TRUE), method = "radix")
  quarantines <- sort(list.files(roots$quarantine, full.names = TRUE),
    method = "radix")
  rows <- list()
  ready <- list()
  for (path in waves) {
    value <- repository_runner_verify_wave(context, path)
    rows[[length(rows) + 1L]] <- data.frame(kind = "wave", name = basename(path),
      manifest_sha256 = value$evidence$manifest_sha256,
      seal_sha256 = value$evidence$seal_sha256, stringsAsFactors = FALSE)
    for (index in which(value$result$state == "ready")) {
      ready[[length(ready) + 1L]] <- data.frame(
        position = value$result$position[[index]],
        repository = value$result$repository[[index]], wave = basename(path),
        wave_manifest_sha256 = value$evidence$manifest_sha256,
        wave_seal_sha256 = value$evidence$seal_sha256,
        attempt = basename(value$result$attempt[[index]]),
        stringsAsFactors = FALSE)
    }
  }
  for (path in quarantines) {
    evidence <- repository_runner_verify_sealed_directory(path)
    rows[[length(rows) + 1L]] <- data.frame(kind = "quarantine",
      name = basename(path), manifest_sha256 = evidence$manifest_sha256,
      seal_sha256 = evidence$seal_sha256, stringsAsFactors = FALSE)
  }
  if (!length(rows)) repository_runner_fail("scheduler has no sealed wave evidence")
  ready_table <- if (length(ready)) do.call(rbind, ready) else data.frame(
    position = character(), repository = character(), wave = character(),
    wave_manifest_sha256 = character(), wave_seal_sha256 = character(),
    attempt = character(), stringsAsFactors = FALSE)
  if (anyDuplicated(ready_table$position) || anyDuplicated(ready_table$repository)) {
    repository_runner_fail("scheduler has duplicate ready evidence for one row")
  }
  if (isTRUE(require_all_accepted)) {
    if (nrow(ready_table) != nrow(context$selection)) {
      repository_runner_fail("scheduler ready evidence does not cover every selected row")
    }
    order <- match(context$selection$position, ready_table$position)
    if (anyNA(order) || !identical(context$selection$repository,
        ready_table$repository[order])) {
      repository_runner_fail("scheduler ready evidence differs from the selection")
    }
    for (index in seq_len(nrow(context$selection))) {
      validation <- repository_runner_verify_attempt(context,
        context$selection[index, , drop = FALSE], deep = FALSE)
      expected <- ready_table[order[[index]], , drop = FALSE]
      accepted <- validation$accepted
      if (!identical(accepted[["wave"]], expected$wave[[1L]]) ||
          !identical(accepted[["wave_manifest_sha256"]],
            expected$wave_manifest_sha256[[1L]]) ||
          !identical(accepted[["wave_seal_sha256"]],
            expected$wave_seal_sha256[[1L]]) ||
          !identical(accepted[["attempt"]], expected$attempt[[1L]])) {
        repository_runner_fail("row acceptance and sealed scheduler wave differ")
      }
    }
  }
  value <- do.call(rbind, rows)
  row.names(value) <- NULL
  value
}

repository_runner_run_row <- function(context, row) {
  repository_runner_assert_parent_lock(context)
  prepared <- repository_runner_prepare_row(context, row)
  if (identical(prepared$state, "reused")) return(invisible("reused"))
  decision <- repository_runner_resource_decision(context$config, "1",
    retained_max = context$initial_resource$jobs)
  wave <- repository_runner_run_wave(context, row, list(prepared), decision, "1")
  if (!isTRUE(wave$promoted[[1L]]) || isTRUE(wave$failed)) {
    repository_runner_fail("single-row repository wave failed")
  }
  invisible("accepted")
}

repository_runner_completion_fields <- function() c(
  "schema", "stage_kind", "run_id", "completed_utc", "rows", "passed", "failed",
  "skipped", "rows_sha256", "protected_content_start_sha256",
  "protected_content_final_sha256", "candidate_final_archive_sha256",
  "protected_library_full_hash_boundaries", "resource_initial_sha256",
  "resource_scheduler_sha256", "scheduler_ledger_sha256", "scheduler_waves",
  "scheduler_quarantines"
)

repository_runner_complete <- function(context, fingerprint, seal_stage,
                                       verify_stage) {
  if (file.exists(file.path(context$stage, "metadata", "completion.seal"))) {
    return(repository_runner_verify_stage(context$config, verify_stage))
  }
  repository_runner_assert_parent_lock(context)
  repository_runner_validate_row_scope(context, require_all = TRUE)
  rows <- lapply(seq_len(nrow(context$selection)), function(index) {
    repository_runner_verify_attempt(context, context$selection[index, , drop = FALSE])
  })
  repository_runner_verify_current_tools(context)
  repository_runner_verify_candidate_current(context, recreate_archive = TRUE)
  repository_runner_assert_metadata(context)
  completions <- repository_runner_require_directory(file.path(context$stage,
    "completions"), "completion root")
  scheduler_ledger <- repository_runner_scheduler_ledger(context,
    require_all_accepted = TRUE)
  existing <- list.files(completions, pattern = "^completion-[0-9]{6}$")
  number <- if (length(existing)) max(as.integer(sub("^completion-", "", existing))) + 1L else 1L
  completion <- repository_runner_reserve_directory(completions,
    sprintf("completion-%06d", number), "completion attempt")
  final_content <- repository_runner_full_content_boundary(context$config,
    fingerprint, "final")
  start_content <- repository_runner_read_tsv(file.path(context$inputs,
    "protected-content-start.tsv"), c("boundary", "kind", "position", "path", "sha256"),
    label = "protected content start")
  if (!identical(start_content[, setdiff(names(start_content), "boundary"), drop = FALSE],
      final_content[, setdiff(names(final_content), "boundary"), drop = FALSE])) {
    changed <- start_content$kind[start_content$sha256 != final_content$sha256 |
      start_content$path != final_content$path]
    repository_runner_fail("protected contents changed between stage boundaries",
      if (length(changed)) paste0(": ", paste(changed, collapse = ", ")) else
        " (data-frame attributes differ)")
  }
  final_path <- file.path(completion, "protected-content-final.tsv")
  repository_runner_write_tsv(final_content, final_path)
  row_table <- do.call(rbind, lapply(seq_along(rows), function(index) data.frame(
    position = context$selection$position[[index]],
    repository = context$selection$repository[[index]],
    status = rows[[index]]$summary[["status"]],
    classification = rows[[index]]$summary[["classification"]],
    wave = rows[[index]]$accepted[["wave"]],
    accepted_sha256 = rows[[index]]$accepted_sha256,
    attempt_manifest_sha256 = rows[[index]]$evidence$manifest_sha256,
    stringsAsFactors = FALSE
  )))
  rows_path <- file.path(completion, "rows.tsv")
  repository_runner_write_tsv(row_table, rows_path)
  scheduler_ledger_path <- file.path(completion, "scheduler-ledger.tsv")
  repository_runner_write_tsv(scheduler_ledger, scheduler_ledger_path)
  candidate_archive <- repository_runner_require_file(file.path(context$inputs,
    "candidate-source.tar"), "candidate source archive")
  scheduler_index <- match("resource-jobs", context$tool_receipt$name)
  if (is.na(scheduler_index)) {
    repository_runner_fail("completion lacks the authenticated resource scheduler")
  }
  values <- c(schema = "1", stage_kind = "repository_tests_resumable_completion",
    run_id = context$config$run_id,
    completed_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    rows = as.character(nrow(row_table)),
    passed = as.character(sum(row_table$status == "passed")),
    failed = as.character(sum(row_table$status %in% c("failed", "timed_out"))),
    skipped = as.character(sum(row_table$status == "skipped_no_tests")),
    rows_sha256 = repository_runner_sha256(rows_path),
    protected_content_start_sha256 = context$run[["protected_content_start_sha256"]],
    protected_content_final_sha256 = repository_runner_sha256(final_path),
    candidate_final_archive_sha256 = repository_runner_sha256(candidate_archive),
    protected_library_full_hash_boundaries = "2",
    resource_initial_sha256 = context$run[["resource_initial_sha256"]],
    resource_scheduler_sha256 = context$tool_receipt$sha256[[scheduler_index]],
    scheduler_ledger_sha256 = repository_runner_sha256(scheduler_ledger_path),
    scheduler_waves = as.character(sum(scheduler_ledger$kind == "wave")),
    scheduler_quarantines = as.character(sum(
      scheduler_ledger$kind == "quarantine")))
  repository_runner_write_tsv(repository_runner_map_frame(values),
    file.path(completion, "completion.tsv"))
  repository_runner_seal_directory(completion)
  if (!is.function(seal_stage)) repository_runner_fail("stage sealer is unavailable")
  repository_runner_assert_parent_lock(context)
  seal_stage(context$stage)
  repository_runner_verify_stage(context$config, verify_stage)
}

repository_runner_verify_stage <- function(config, verify_stage) {
  context <- repository_runner_load(config, require_current = FALSE)
  if (!is.function(verify_stage)) repository_runner_fail("stage verifier is unavailable")
  stage_evidence <- verify_stage(context$stage)
  repository_runner_validate_row_scope(context, require_all = TRUE)
  rows <- lapply(seq_len(nrow(context$selection)), function(index) {
    repository_runner_verify_attempt(context, context$selection[index, , drop = FALSE])
  })
  completions <- list.files(file.path(context$stage, "completions"),
    pattern = "^completion-[0-9]{6}$", full.names = TRUE)
  sealed <- vapply(completions, function(path) tryCatch({
    repository_runner_verify_sealed_directory(path)
    TRUE
  }, error = function(...) FALSE), logical(1L))
  if (sum(sealed) != 1L) repository_runner_fail("stage lacks exactly one sealed completion")
  completion_directory <- completions[sealed][[1L]]
  completion_evidence <- repository_runner_verify_sealed_directory(completion_directory)
  completion <- repository_runner_read_map(file.path(completion_directory,
    "completion.tsv"), repository_runner_completion_fields(), "completion metadata")
  start_content <- repository_runner_read_tsv(file.path(context$inputs,
    "protected-content-start.tsv"),
    c("boundary", "kind", "position", "path", "sha256"),
    label = "protected content start")
  final_content <- repository_runner_read_tsv(file.path(completion_directory,
    "protected-content-final.tsv"),
    c("boundary", "kind", "position", "path", "sha256"),
    label = "protected content final")
  table <- repository_runner_read_tsv(file.path(completion_directory, "rows.tsv"),
    c("position", "repository", "status", "classification", "wave",
      "accepted_sha256", "attempt_manifest_sha256"), label = "completion rows")
  retained_scheduler_ledger <- repository_runner_read_tsv(file.path(
    completion_directory, "scheduler-ledger.tsv"),
    c("kind", "name", "manifest_sha256", "seal_sha256"),
    label = "completion scheduler ledger")
  current_scheduler_ledger <- repository_runner_scheduler_ledger(context,
    require_all_accepted = TRUE)
  scheduler_index <- match("resource-jobs", context$tool_receipt$name)
  expected <- c(schema = "1", stage_kind = "repository_tests_resumable_completion",
    run_id = config$run_id, rows = as.character(nrow(table)),
    passed = as.character(sum(table$status == "passed")),
    failed = as.character(sum(table$status %in% c("failed", "timed_out"))),
    skipped = as.character(sum(table$status == "skipped_no_tests")),
    rows_sha256 = repository_runner_sha256(file.path(completion_directory, "rows.tsv")),
    protected_content_start_sha256 = context$run[["protected_content_start_sha256"]],
    protected_content_final_sha256 = repository_runner_sha256(file.path(
      completion_directory, "protected-content-final.tsv")),
    candidate_final_archive_sha256 = context$run[["candidate_proof_archive_sha256"]],
    protected_library_full_hash_boundaries = "2",
    resource_initial_sha256 = context$run[["resource_initial_sha256"]],
    resource_scheduler_sha256 = if (is.na(scheduler_index)) "-" else
      context$tool_receipt$sha256[[scheduler_index]],
    scheduler_ledger_sha256 = repository_runner_sha256(file.path(
      completion_directory, "scheduler-ledger.tsv")),
    scheduler_waves = as.character(sum(retained_scheduler_ledger$kind == "wave")),
    scheduler_quarantines = as.character(sum(
      retained_scheduler_ledger$kind == "quarantine")))
  if (!identical(unname(completion[names(expected)]), unname(expected)) ||
      is.na(scheduler_index) ||
      !identical(retained_scheduler_ledger, current_scheduler_ledger) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        completion[["completed_utc"]]) ||
      any(start_content$boundary != "start") ||
      any(final_content$boundary != "final") ||
      !identical(start_content[, setdiff(names(start_content), "boundary"),
        drop = FALSE], final_content[, setdiff(names(final_content), "boundary"),
        drop = FALSE]) ||
      !identical(as.integer(table$position), context$selection$position) ||
      !identical(table$repository, context$selection$repository)) {
    repository_runner_fail("completion does not bind the dynamic selection")
  }
  for (index in seq_along(rows)) {
    if (!identical(table$status[[index]], rows[[index]]$summary[["status"]]) ||
        !identical(table$classification[[index]],
          rows[[index]]$summary[["classification"]]) ||
        !identical(table$wave[[index]], rows[[index]]$accepted[["wave"]]) ||
        !identical(table$accepted_sha256[[index]], rows[[index]]$accepted_sha256) ||
        !identical(table$attempt_manifest_sha256[[index]],
          rows[[index]]$evidence$manifest_sha256)) {
      repository_runner_fail("completion row differs from accepted evidence")
    }
  }
  list(context = context, rows = rows, completion = completion,
    completion_evidence = completion_evidence, stage_evidence = stage_evidence)
}
