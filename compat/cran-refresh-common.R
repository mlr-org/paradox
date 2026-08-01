cran_refresh_stop <- function(...) {
  stop(..., call. = FALSE)
}

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

cran_refresh_inventory_columns <- c(
  "relation", "package", "source", "priority", "notes"
)
cran_refresh_snapshot_columns <- c(
  "package", "relation", "source", "priority", "notes", "version",
  "repository", "archive", "md5", "sha256"
)
cran_refresh_live_columns <- c("package", "relation", "version", "md5")
cran_refresh_review_columns <- c(
  "action", "package", "relation", "priority", "notes"
)
cran_refresh_relations <- c("depends", "imports", "suggests")
cran_refresh_metadata_fields <- c(
  "Package", "Version", "MD5sum", "Priority", "Path", "Depends", "Imports",
  "LinkingTo", "Suggests", "Enhances"
)
cran_refresh_relation_fields <- c(
  depends = "Depends", imports = "Imports", suggests = "Suggests"
)
cran_refresh_unsupported_relation_fields <- c(
  linkingto = "LinkingTo", enhances = "Enhances"
)
cran_refresh_repository <- "https://cloud.r-project.org/src/contrib"
cran_refresh_package_pattern <- "^[A-Za-z][A-Za-z0-9.]*$"
cran_refresh_version_pattern <- "^[A-Za-z0-9][A-Za-z0-9.+-]*$"
cran_refresh_archive_pattern <- "^[A-Za-z0-9][A-Za-z0-9._+-]*[.]tar[.]gz$"

cran_refresh_require_names <- function(value, expected, label) {
  if (!is.data.frame(value) || !identical(names(value), expected)) {
    cran_refresh_stop(label, " has an unexpected schema")
  }
  invisible(value)
}

cran_refresh_validate_text <- function(value, label, allow_empty = FALSE) {
  if (!is.character(value) || anyNA(value) ||
      (!allow_empty && any(!nzchar(value))) ||
      any(grepl("[\t\r\n]", value))) {
    cran_refresh_stop(label, " contains missing, empty, or control-bearing text")
  }
  invisible(value)
}

cran_refresh_parse_priority <- function(value, label) {
  cran_refresh_validate_text(value, label)
  parsed <- suppressWarnings(as.integer(value))
  if (anyNA(parsed) || any(parsed < 0L) ||
      !identical(as.character(parsed), value)) {
    cran_refresh_stop(label, " must contain canonical non-negative integers")
  }
  parsed
}

cran_refresh_validate_run_id <- function(value) {
  if (length(value) != 1L || is.na(value) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", value) ||
      value %in% c(".", "..")) {
    cran_refresh_stop("--run-id must be a safe name of at most 128 characters")
  }
  value
}

cran_refresh_dependency_names <- function(value) {
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    return(character())
  }
  value <- gsub("[\r\n]", " ", value)
  entries <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  entries <- sub(
    "[[:space:]]*\\([^()]*\\)[[:space:]]*$", "", entries
  )
  entries <- trimws(entries)
  if (any(!nzchar(entries)) ||
      any(!grepl(cran_refresh_package_pattern, entries))) {
    cran_refresh_stop("CRAN dependency metadata is malformed: ", value)
  }
  entries
}

cran_refresh_canonicalize_metadata <- function(metadata) {
  cran_refresh_require_names(
    metadata, cran_refresh_metadata_fields, "CRAN PACKAGES metadata"
  )
  if (!nrow(metadata)) cran_refresh_stop("CRAN PACKAGES metadata is empty")
  cran_refresh_validate_text(metadata$Package, "CRAN package names")
  cran_refresh_validate_text(metadata$Version, "CRAN package versions")
  if (any(!grepl(cran_refresh_package_pattern, metadata$Package)) ||
      any(!grepl(cran_refresh_version_pattern, metadata$Version))) {
    cran_refresh_stop("CRAN PACKAGES metadata has unsafe identities")
  }

  for (field in c("Priority", "Path")) {
    present <- !is.na(metadata[[field]])
    values <- metadata[[field]][present]
    if (any(!nzchar(values) | grepl("[\t\r\n]", values))) {
      cran_refresh_stop("CRAN PACKAGES metadata has unsafe ", field, " values")
    }
  }

  duplicated_identity <- duplicated(metadata$Package) |
    duplicated(metadata$Package, fromLast = TRUE)
  path_row <- !is.na(metadata$Path)
  if (any(path_row & !duplicated_identity)) {
    cran_refresh_stop(
      "CRAN PACKAGES metadata has a Path row without a canonical root row"
    )
  }

  duplicate_packages <- unique(metadata$Package[duplicated_identity])
  all_relation_fields <- c(
    cran_refresh_relation_fields, cran_refresh_unsupported_relation_fields
  )
  for (package in duplicate_packages) {
    rows <- which(metadata$Package == package)
    canonical <- rows[is.na(metadata$Path[rows])]
    alternates <- rows[!is.na(metadata$Path[rows])]
    if (length(canonical) != 1L || !length(alternates)) {
      cran_refresh_stop(
        "duplicate CRAN package must have exactly one canonical root row and ",
        "at least one versioned Recommended alternate: ", package
      )
    }
    if (anyNA(metadata$Priority[rows]) ||
        any(metadata$Priority[rows] != "recommended")) {
      cran_refresh_stop(
        "duplicate CRAN package is not uniformly Priority recommended: ",
        package
      )
    }
    alternate_paths <- metadata$Path[alternates]
    if (any(!grepl(
        "^[0-9]+[.][0-9]+[.][0-9]+/Recommended$", alternate_paths
      )) || anyDuplicated(alternate_paths)) {
      cran_refresh_stop(
        "duplicate CRAN package has an unsafe or repeated Recommended Path: ",
        package
      )
    }
    duplicate_md5 <- metadata$MD5sum[rows]
    if (anyNA(duplicate_md5) ||
        any(!grepl("^[0-9A-Fa-f]{32}$", duplicate_md5))) {
      cran_refresh_stop(
        "duplicate CRAN package has malformed MD5 metadata: ", package
      )
    }
    for (index in alternates) {
      declarations <- names(all_relation_fields)[vapply(
        all_relation_fields,
        function(field) {
          "paradox" %in% cran_refresh_dependency_names(
            metadata[[field]][[index]]
          )
        },
        logical(1L)
      )]
      if (length(declarations)) {
        cran_refresh_stop(
          "versioned Recommended alternate declares paradox through ",
          paste(declarations, collapse = "/"), ": ", package
        )
      }
    }
  }

  canonical <- metadata[!path_row, , drop = FALSE]
  row.names(canonical) <- NULL
  if (anyDuplicated(canonical$Package)) {
    cran_refresh_stop("canonical CRAN PACKAGES metadata has duplicate identities")
  }
  canonical
}

cran_refresh_direct_reverses <- function(metadata) {
  metadata <- cran_refresh_canonicalize_metadata(metadata)

  hits <- lapply(seq_len(nrow(metadata)), function(index) {
    unsupported <- names(cran_refresh_unsupported_relation_fields)[vapply(
      cran_refresh_unsupported_relation_fields,
      function(field) {
        "paradox" %in% cran_refresh_dependency_names(metadata[[field]][[index]])
      },
      logical(1L)
    )]
    if (length(unsupported)) {
      cran_refresh_stop(
        "CRAN package declares paradox through an unsupported relation (",
        paste(unsupported, collapse = "/"), "): ", metadata$Package[[index]]
      )
    }
    relations <- names(cran_refresh_relation_fields)[vapply(
      cran_refresh_relation_fields,
      function(field) {
        "paradox" %in% cran_refresh_dependency_names(metadata[[field]][[index]])
      },
      logical(1L)
    )]
    if (length(relations) > 1L) {
      cran_refresh_stop(
        "CRAN package declares paradox in multiple dependency fields: ",
        metadata$Package[[index]]
      )
    }
    if (!length(relations)) return(NULL)
    data.frame(
      package = metadata$Package[[index]],
      relation = relations[[1L]],
      version = metadata$Version[[index]],
      md5 = tolower(metadata$MD5sum[[index]]),
      stringsAsFactors = FALSE
    )
  })
  hits <- Filter(Negate(is.null), hits)
  if (!length(hits)) cran_refresh_stop("CRAN metadata has no paradox reverse dependencies")
  result <- do.call(rbind, hits)
  row.names(result) <- NULL
  cran_refresh_validate_text(result$md5, "CRAN reverse-dependency MD5 values")
  if (any(!grepl("^[0-9a-f]{32}$", result$md5)) ||
      anyDuplicated(result$package)) {
    cran_refresh_stop("CRAN reverse-dependency metadata is malformed")
  }
  result
}

cran_refresh_read_packages <- function(path) {
  if (!exists("compat_fetch_require_regular", mode = "function", inherits = TRUE)) {
    cran_refresh_stop("source-fetch archive helpers are not loaded")
  }
  compat_fetch_require_regular(path, "downloaded CRAN PACKAGES.gz")
  connection <- gzfile(path, open = "rt", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  value <- tryCatch(
    read.dcf(connection, fields = cran_refresh_metadata_fields),
    error = function(error) {
      cran_refresh_stop("could not parse CRAN PACKAGES.gz: ", conditionMessage(error))
    }
  )
  value <- as.data.frame(value, stringsAsFactors = FALSE, check.names = FALSE)
  cran_refresh_require_names(
    value, cran_refresh_metadata_fields, "CRAN PACKAGES metadata"
  )
  value
}

cran_refresh_empty_review <- function() {
  stats::setNames(
    data.frame(
      matrix(character(), nrow = 0L, ncol = length(cran_refresh_review_columns)),
      stringsAsFactors = FALSE
    ),
    cran_refresh_review_columns
  )
}

cran_refresh_validate_review <- function(review) {
  cran_refresh_require_names(review, cran_refresh_review_columns,
    "reviewed structural changes")
  if (!nrow(review)) return(review)
  for (field in cran_refresh_review_columns) {
    cran_refresh_validate_text(review[[field]], paste("reviewed", field))
  }
  if (any(!review$action %in% c("add", "remove", "relation")) ||
      any(!grepl(cran_refresh_package_pattern, review$package)) ||
      any(!review$relation %in% cran_refresh_relations) ||
      anyDuplicated(review$package)) {
    cran_refresh_stop("reviewed structural changes are malformed or duplicated")
  }
  cran_refresh_parse_priority(review$priority, "reviewed priorities")
  review
}

cran_refresh_validate_inventory <- function(inventory) {
  cran_refresh_require_names(inventory, cran_refresh_inventory_columns,
    "reverse-dependency inventory")
  if (!nrow(inventory)) cran_refresh_stop("reverse-dependency inventory is empty")
  for (field in cran_refresh_inventory_columns) {
    cran_refresh_validate_text(inventory[[field]], paste("inventory", field))
  }
  if (any(!inventory$relation %in% cran_refresh_relations) ||
      any(!inventory$source %in% c("CRAN", "Bioconductor")) ||
      any(!grepl(cran_refresh_package_pattern, inventory$package)) ||
      anyDuplicated(inventory$package)) {
    cran_refresh_stop("reverse-dependency inventory is malformed or duplicated")
  }
  cran_refresh_parse_priority(inventory$priority, "inventory priorities")
  inventory
}

cran_refresh_validate_snapshot <- function(snapshot) {
  cran_refresh_require_names(snapshot, cran_refresh_snapshot_columns,
    "CRAN source snapshot")
  if (!nrow(snapshot)) cran_refresh_stop("CRAN source snapshot is empty")
  for (field in cran_refresh_snapshot_columns) {
    cran_refresh_validate_text(snapshot[[field]], paste("snapshot", field))
  }
  if (any(!snapshot$relation %in% cran_refresh_relations) ||
      any(snapshot$source != "CRAN") ||
      any(!grepl(cran_refresh_package_pattern, snapshot$package)) ||
      any(!grepl(cran_refresh_version_pattern, snapshot$version)) ||
      any(snapshot$repository != cran_refresh_repository) ||
      any(!grepl(cran_refresh_archive_pattern, snapshot$archive)) ||
      any(snapshot$archive != paste0(snapshot$package, "_", snapshot$version,
        ".tar.gz")) ||
      any(!grepl("^[0-9a-f]{32}$", tolower(snapshot$md5))) ||
      any(!grepl("^[0-9a-f]{64}$", tolower(snapshot$sha256))) ||
      anyDuplicated(snapshot$package) || anyDuplicated(snapshot$archive)) {
    cran_refresh_stop("CRAN source snapshot is malformed or duplicated")
  }
  cran_refresh_parse_priority(snapshot$priority, "snapshot priorities")
  snapshot$md5 <- tolower(snapshot$md5)
  snapshot$sha256 <- tolower(snapshot$sha256)
  snapshot
}

cran_refresh_insert_inventory_row <- function(inventory, row) {
  same_group <- which(
    inventory$source == row$source[[1L]] &
      inventory$relation == row$relation[[1L]] &
      inventory$priority == row$priority[[1L]]
  )
  if (length(same_group)) {
    after <- max(same_group)
  } else {
    relation_rank <- match(inventory$relation, cran_refresh_relations)
    target_rank <- match(row$relation[[1L]], cran_refresh_relations)
    priority <- suppressWarnings(as.integer(inventory$priority))
    target_priority <- as.integer(row$priority[[1L]])
    preceding <- which(
      relation_rank < target_rank |
        (relation_rank == target_rank & priority <= target_priority)
    )
    after <- if (length(preceding)) max(preceding) else 0L
  }
  if (after == 0L) return(rbind(row, inventory))
  if (after == nrow(inventory)) return(rbind(inventory, row))
  rbind(inventory[seq_len(after), , drop = FALSE], row,
    inventory[(after + 1L):nrow(inventory), , drop = FALSE])
}

cran_refresh_plan <- function(inventory, snapshot, live, review = NULL) {
  inventory <- cran_refresh_validate_inventory(inventory)
  snapshot <- cran_refresh_validate_snapshot(snapshot)
  cran_refresh_require_names(live, cran_refresh_live_columns,
    "live CRAN reverse dependencies")
  if (!nrow(live)) cran_refresh_stop("live CRAN reverse dependencies are empty")
  for (field in cran_refresh_live_columns) {
    cran_refresh_validate_text(live[[field]], paste("live", field))
  }
  live$md5 <- tolower(live$md5)
  if (any(!grepl(cran_refresh_package_pattern, live$package)) ||
      any(!live$relation %in% cran_refresh_relations) ||
      any(!grepl(cran_refresh_version_pattern, live$version)) ||
      any(!grepl("^[0-9a-f]{32}$", live$md5)) ||
      anyDuplicated(live$package)) {
    cran_refresh_stop("live CRAN reverse dependencies are malformed or duplicated")
  }
  review <- cran_refresh_validate_review(review %||% cran_refresh_empty_review())

  old <- inventory[inventory$source == "CRAN", , drop = FALSE]
  if (!setequal(old$package, snapshot$package)) {
    cran_refresh_stop("CRAN inventory and source snapshot package sets disagree")
  }
  aligned_snapshot <- snapshot[match(old$package, snapshot$package), , drop = FALSE]
  for (field in c("relation", "source", "priority", "notes")) {
    if (!identical(old[[field]], aligned_snapshot[[field]])) {
      cran_refresh_stop("CRAN inventory and source snapshot disagree: ", field)
    }
  }

  added <- live$package[!live$package %in% old$package]
  removed <- old$package[!old$package %in% live$package]
  common <- old$package[old$package %in% live$package]
  old_common <- old[match(common, old$package), , drop = FALSE]
  live_common <- live[match(common, live$package), , drop = FALSE]
  relation_changed <- common[old_common$relation != live_common$relation]

  expected <- rbind(
    if (length(added)) data.frame(
      action = "add", package = added,
      relation = live$relation[match(added, live$package)],
      stringsAsFactors = FALSE
    ),
    if (length(removed)) data.frame(
      action = "remove", package = removed,
      relation = old$relation[match(removed, old$package)],
      stringsAsFactors = FALSE
    ),
    if (length(relation_changed)) data.frame(
      action = "relation", package = relation_changed,
      relation = live$relation[match(relation_changed, live$package)],
      stringsAsFactors = FALSE
    )
  )
  if (is.null(expected)) {
    expected <- data.frame(
      action = character(), package = character(), relation = character(),
      stringsAsFactors = FALSE
    )
  }
  row.names(expected) <- NULL
  if (nrow(expected) != nrow(review) ||
      !setequal(expected$package, review$package)) {
    cran_refresh_stop(
      "structural CRAN reverse-dependency changes lack an exact reviewed row"
    )
  }
  if (nrow(expected)) {
    aligned_review <- review[match(expected$package, review$package), , drop = FALSE]
    if (!identical(expected$action, aligned_review$action) ||
        !identical(expected$relation, aligned_review$relation)) {
      cran_refresh_stop("reviewed actions or relations disagree with live CRAN")
    }
    removal <- expected$action == "remove"
    if (any(removal)) {
      removal_old <- old[match(expected$package[removal], old$package), , drop = FALSE]
      removal_review <- aligned_review[removal, , drop = FALSE]
      if (!identical(removal_old$priority, removal_review$priority) ||
          !identical(removal_old$notes, removal_review$notes)) {
        cran_refresh_stop("reviewed removals must repeat the exact old metadata")
      }
    }
  }

  proposed_inventory <- inventory[!inventory$package %in% removed, , drop = FALSE]
  if (length(relation_changed)) {
    changed_review <- review[match(relation_changed, review$package), , drop = FALSE]
    positions <- match(relation_changed, proposed_inventory$package)
    proposed_inventory$relation[positions] <- changed_review$relation
    proposed_inventory$priority[positions] <- changed_review$priority
    proposed_inventory$notes[positions] <- changed_review$notes
  }
  if (length(added)) {
    addition_review <- review[match(sort(added), review$package), , drop = FALSE]
    for (index in seq_len(nrow(addition_review))) {
      row <- data.frame(
        relation = addition_review$relation[[index]],
        package = addition_review$package[[index]],
        source = "CRAN",
        priority = addition_review$priority[[index]],
        notes = addition_review$notes[[index]],
        stringsAsFactors = FALSE
      )
      proposed_inventory <- cran_refresh_insert_inventory_row(
        proposed_inventory, row
      )
    }
  }
  row.names(proposed_inventory) <- NULL
  cran_refresh_validate_inventory(proposed_inventory)

  proposed_cran <- proposed_inventory[
    proposed_inventory$source == "CRAN", , drop = FALSE
  ]
  aligned_live <- live[match(proposed_cran$package, live$package), , drop = FALSE]
  if (anyNA(aligned_live$package) ||
      !identical(proposed_cran$package, aligned_live$package)) {
    cran_refresh_stop("proposed CRAN inventory does not match live metadata")
  }
  proposed_snapshot <- data.frame(
    package = proposed_cran$package,
    relation = proposed_cran$relation,
    source = proposed_cran$source,
    priority = proposed_cran$priority,
    notes = proposed_cran$notes,
    version = aligned_live$version,
    repository = rep.int(cran_refresh_repository, nrow(proposed_cran)),
    archive = paste0(proposed_cran$package, "_", aligned_live$version,
      ".tar.gz"),
    md5 = aligned_live$md5,
    sha256 = rep.int("", nrow(proposed_cran)),
    stringsAsFactors = FALSE
  )

  old_index <- match(proposed_snapshot$package, snapshot$package)
  copy <- !is.na(old_index)
  copy[copy] <- vapply(which(copy), function(index) {
    old_row <- snapshot[old_index[[index]], , drop = FALSE]
    identical(old_row$version[[1L]], proposed_snapshot$version[[index]]) &&
      identical(old_row$archive[[1L]], proposed_snapshot$archive[[index]]) &&
      identical(old_row$md5[[1L]], proposed_snapshot$md5[[index]])
  }, logical(1L))
  archive_plan <- data.frame(
    package = proposed_snapshot$package,
    action = ifelse(copy, "copy", "download"),
    old_version = ifelse(is.na(old_index), "", snapshot$version[old_index]),
    version = proposed_snapshot$version,
    archive = proposed_snapshot$archive,
    md5 = proposed_snapshot$md5,
    stringsAsFactors = FALSE
  )

  report_packages <- unique(c(old$package, live$package))
  report_old <- match(report_packages, old$package)
  report_live <- match(report_packages, live$package)
  structural <- rep.int("", length(report_packages))
  structural[report_packages %in% added] <- "add"
  structural[report_packages %in% removed] <- "remove"
  structural[report_packages %in% relation_changed] <- "relation"
  version_change <- !is.na(report_old) & !is.na(report_live) &
    old$package[report_old] == live$package[report_live] &
    aligned_snapshot$version[match(report_packages, old$package)] !=
      live$version[report_live]
  version_change[is.na(version_change)] <- FALSE
  changes <- data.frame(
    package = report_packages,
    structural_change = structural,
    version_change = ifelse(version_change, "version", ""),
    old_relation = ifelse(is.na(report_old), "", old$relation[report_old]),
    relation = ifelse(is.na(report_live), "", live$relation[report_live]),
    old_version = ifelse(is.na(report_old), "",
      aligned_snapshot$version[match(report_packages, old$package)]),
    version = ifelse(is.na(report_live), "", live$version[report_live]),
    stringsAsFactors = FALSE
  )
  changes[is.na(changes)] <- ""

  list(
    inventory = proposed_inventory,
    snapshot = proposed_snapshot,
    archive_plan = archive_plan,
    changes = changes,
    expected_review = expected
  )
}

cran_refresh_write_tsv <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(path) || dir.exists(path) ||
      file.exists(temporary) || dir.exists(temporary) ||
      compat_fetch_is_symbolic(path) ||
      compat_fetch_is_symbolic(temporary)) {
    cran_refresh_stop("refusing to overwrite refresh output: ", path)
  }
  utils::write.table(
    value, temporary, quote = FALSE, sep = "\t", row.names = FALSE,
    col.names = TRUE, na = "", fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) {
    cran_refresh_stop("could not atomically publish refresh output: ", path)
  }
  invisible(path)
}

cran_refresh_retain_input <- function(source, destination, expected_sha256,
    label) {
  compat_fetch_require_regular(source, label)
  if (length(expected_sha256) != 1L || is.na(expected_sha256) ||
      !grepl("^[0-9a-f]{64}$", expected_sha256)) {
    cran_refresh_stop(label, " has an invalid expected SHA-256")
  }
  if (file.exists(destination) || dir.exists(destination) ||
      compat_fetch_is_symbolic(destination)) {
    cran_refresh_stop("refusing to overwrite retained input: ", destination)
  }
  if (!file.copy(source, destination, copy.mode = TRUE, copy.date = TRUE)) {
    cran_refresh_stop("could not retain ", label)
  }
  compat_fetch_require_regular(destination, paste("retained", label))
  observed <- compat_fetch_file_hashes(
    destination, paste("retained", label)
  )[[1L]]
  if (!identical(observed, expected_sha256)) {
    cran_refresh_stop("retained ", label, " changed")
  }
  invisible(destination)
}

cran_refresh_cache_receipt <- function(directory, expected = NULL) {
  if (!dir.exists(directory) || compat_fetch_is_symbolic(directory)) {
    cran_refresh_stop("source cache is absent or symbolic: ", directory)
  }
  entries <- sort(list.files(directory, all.files = TRUE, no.. = TRUE))
  if (!is.null(expected) &&
      (!identical(entries, sort(expected)) || length(entries) != length(expected))) {
    cran_refresh_stop("source cache inventory is not exact: ", directory)
  }
  paths <- file.path(directory, entries)
  for (path in paths) compat_fetch_require_regular(path, "source cache archive")
  first <- unname(tools::sha256sum(paths))
  md5 <- unname(tools::md5sum(paths))
  info <- file.info(paths, extra_cols = FALSE)
  for (path in paths) compat_fetch_require_regular(path, "source cache archive")
  second <- unname(tools::sha256sum(paths))
  if (!identical(first, second) || anyNA(info$size) || anyNA(info$mode)) {
    cran_refresh_stop("source cache changed while it was authenticated")
  }
  data.frame(
    archive = entries,
    size = format(info$size, scientific = FALSE, trim = TRUE),
    mode = sprintf("%04o", as.integer(info$mode)),
    md5 = md5,
    sha256 = second,
    stringsAsFactors = FALSE
  )
}

cran_refresh_copy_archive <- function(source, destination, row) {
  compat_fetch_verify_archive(source, row)
  if (file.exists(destination) || dir.exists(destination) ||
      compat_fetch_is_symbolic(destination)) {
    cran_refresh_stop("refusing to overwrite staged archive: ", destination)
  }
  if (!file.copy(source, destination, copy.mode = TRUE, copy.date = TRUE)) {
    cran_refresh_stop("could not copy authenticated archive: ", source)
  }
  compat_fetch_verify_archive(destination, row)
  invisible(destination)
}

cran_refresh_download_archive <- function(row, destination) {
  parent <- dirname(destination)
  staging <- compat_fetch_reserve_directory(parent, ".fetch-")
  on.exit(unlink(staging, recursive = TRUE, force = FALSE), add = TRUE)
  staged <- file.path(staging, row$archive[[1L]])
  urls <- c(
    paste0(cran_refresh_repository, "/", row$archive[[1L]]),
    paste0(cran_refresh_repository, "/Archive/", row$package[[1L]], "/",
      row$archive[[1L]])
  )
  selected <- NULL
  for (url in urls) {
    status <- tryCatch(
      utils::download.file(
        url, staged, method = "libcurl", mode = "wb", quiet = FALSE
      ),
      error = identity
    )
    if (!inherits(status, "error") && identical(as.integer(status), 0L)) {
      selected <- url
      break
    }
    if (file.exists(staged) || dir.exists(staged) ||
        compat_fetch_is_symbolic(staged)) {
      compat_fetch_require_regular(staged, "partial CRAN source download")
      if (!file.remove(staged)) {
        cran_refresh_stop("could not remove a partial CRAN source download")
      }
    }
  }
  if (is.null(selected)) {
    cran_refresh_stop("could not download current CRAN source: ", row$archive[[1L]])
  }
  compat_fetch_require_regular(staged, "downloaded CRAN source archive")
  observed_md5 <- unname(tools::md5sum(staged))
  if (!identical(observed_md5, row$md5[[1L]])) {
    cran_refresh_stop("downloaded CRAN source MD5 disagrees with PACKAGES: ",
      row$archive[[1L]])
  }
  compat_fetch_archive_metadata(staged, row$package[[1L]], row$version[[1L]])
  sha256 <- unname(tools::sha256sum(staged))
  if (!identical(sha256, unname(tools::sha256sum(staged)))) {
    cran_refresh_stop("downloaded CRAN source changed while it was authenticated")
  }
  row$sha256[[1L]] <- sha256
  compat_fetch_verify_archive(staged, row)
  compat_fetch_select_archive(staged, destination)
  compat_fetch_verify_archive(destination, row)
  list(sha256 = sha256, url = selected)
}

cran_refresh_read_review <- function(path) {
  if (is.null(path)) return(cran_refresh_empty_review())
  compat_fetch_require_regular(path, "reviewed structural-change input")
  value <- utils::read.delim(
    path, header = TRUE, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE, na.strings = character(),
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )
  cran_refresh_validate_review(value)
}

cran_refresh_parse_arguments <- function(arguments) {
  values <- list(root = NULL, run_id = NULL, reviewed_changes = NULL, help = FALSE)
  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (argument %in% c("-h", "--help")) {
      values$help <- TRUE
      index <- index + 1L
      next
    }
    matched <- FALSE
    for (option in c("--root", "--run-id", "--reviewed-changes")) {
      prefix <- paste0(option, "=")
      if (startsWith(argument, prefix)) {
        value <- substring(argument, nchar(prefix) + 1L)
        next_index <- index + 1L
      } else if (identical(argument, option)) {
        if (index == length(arguments)) {
          cran_refresh_stop(option, " requires a value")
        }
        value <- arguments[[index + 1L]]
        next_index <- index + 2L
      } else {
        next
      }
      if (!nzchar(value)) cran_refresh_stop(option, " requires a value")
      name <- switch(option,
        "--root" = "root", "--run-id" = "run_id",
        "--reviewed-changes" = "reviewed_changes"
      )
      if (!is.null(values[[name]])) {
        cran_refresh_stop(option, " may be supplied only once")
      }
      values[[name]] <- value
      index <- next_index
      matched <- TRUE
      break
    }
    if (!matched) cran_refresh_stop("unknown option: ", argument)
  }
  values
}

cran_refresh_main <- function(arguments, script) {
  usage <- paste(
    "usage: refresh-cran-snapshot.R --root ROOT --run-id ID",
    "[--reviewed-changes FILE]"
  )
  values <- cran_refresh_parse_arguments(arguments)
  if (values$help) {
    cat(
      usage, "\n\n",
      "Stage current CRAN reverse metadata, archives, and proposed manifests.\n",
      "Tracked manifests and the canonical source cache are never modified.\n",
      sep = ""
    )
    return(invisible(NULL))
  }
  if (is.null(values$root) || is.null(values$run_id)) {
    cran_refresh_stop(usage)
  }
  run_id <- cran_refresh_validate_run_id(values$run_id)
  root <- normalizePath(values$root, winslash = "/", mustWork = TRUE)
  if (!identical(script, file.path(root, "compat", basename(script)))) {
    cran_refresh_stop("--root does not own this refresh script")
  }
  expected_home <- file.path(root, ".local", "toolchain", "lib", "R")
  expected_rscript <- file.path(root, ".local", "toolchain", "bin", "Rscript")
  if (!identical(Sys.getenv("PARADOX_ACTIVE_ROOT", unset = ""), root) ||
      !identical(normalizePath(R.home(), winslash = "/", mustWork = TRUE),
        expected_home) ||
      !identical(unname(Sys.which("Rscript")), expected_rscript)) {
    cran_refresh_stop("activate the exact repository-local R first: . scripts/activate")
  }
  compat_fetch_require_regular(expected_rscript, "repository-local Rscript")

  inventory_path <- file.path(root, "compat", "reverse-dependencies.tsv")
  snapshot_path <- file.path(root, "compat", "cran-snapshot.tsv")
  helper_paths <- c(
    file.path(root, "compat", "refresh-cran-snapshot.R"),
    file.path(root, "compat", "cran-refresh-common.R"),
    file.path(root, "compat", "source-fetch-common.R")
  )
  input_paths <- c(inventory_path, snapshot_path, helper_paths)
  review_path <- NULL
  if (!is.null(values$reviewed_changes)) {
    review_path <- normalizePath(
      values$reviewed_changes, winslash = "/", mustWork = TRUE
    )
    compat_fetch_require_regular(review_path, "reviewed structural-change input")
    input_paths <- c(input_paths, review_path)
  }
  input_hashes <- compat_fetch_file_hashes(input_paths, "CRAN refresh input")
  names(input_hashes) <- input_paths

  compat_root <- file.path(root, ".local", "compat")
  refresh_root <- file.path(compat_root, "cran-refresh")
  for (path in c(file.path(root, ".local"), compat_root, refresh_root)) {
    compat_fetch_make_directory(root, path, "managed CRAN refresh directory")
  }
  run <- file.path(refresh_root, run_id)
  compat_fetch_require_plain_chain(root, run, "CRAN refresh run")
  if (file.exists(run) || dir.exists(run) || compat_fetch_is_symbolic(run) ||
      !dir.create(run, recursive = FALSE, mode = "0700")) {
    cran_refresh_stop("refresh run ID already exists; choose a fresh ID: ", run_id)
  }
  run <- normalizePath(run, winslash = "/", mustWork = TRUE)
  for (name in c("cache", "metadata", "proposed")) {
    path <- file.path(run, name)
    if (!dir.create(path, recursive = FALSE, mode = "0700")) {
      cran_refresh_stop("could not create refresh run directory: ", path)
    }
  }
  cache <- file.path(run, "cache")
  metadata_directory <- file.path(run, "metadata")
  proposed_directory <- file.path(run, "proposed")

  inventory <- compat_fetch_read_tsv(
    inventory_path, cran_refresh_inventory_columns,
    "reverse-dependency inventory"
  )
  snapshot <- compat_fetch_read_tsv(
    snapshot_path, cran_refresh_snapshot_columns, "CRAN source snapshot"
  )
  review <- cran_refresh_read_review(review_path)
  reviewed_copy <- file.path(metadata_directory, "reviewed-changes.tsv")
  cran_refresh_write_tsv(review, reviewed_copy)
  if (!is.null(review_path)) {
    raw_review_copy <- file.path(
      metadata_directory, "reviewed-changes.input.tsv"
    )
    review_input_index <- match(review_path, input_paths)
    if (is.na(review_input_index)) {
      cran_refresh_stop("reviewed structural-change input was not authenticated")
    }
    cran_refresh_retain_input(
      review_path, raw_review_copy, input_hashes[[review_input_index]],
      "reviewed structural-change input"
    )
  }

  canonical <- file.path(compat_root, "cran-sources")
  compat_fetch_require_plain_chain(root, canonical, "canonical CRAN source cache")
  canonical_before <- cran_refresh_cache_receipt(canonical, snapshot$archive)
  canonical_receipt <- file.path(metadata_directory, "canonical-cache.tsv")
  cran_refresh_write_tsv(canonical_before, canonical_receipt)
  for (index in seq_len(nrow(snapshot))) {
    compat_fetch_verify_archive(
      file.path(canonical, snapshot$archive[[index]]),
      snapshot[index, , drop = FALSE]
    )
  }

  packages_path <- file.path(run, "PACKAGES.gz")
  packages_url <- paste0(cran_refresh_repository, "/PACKAGES.gz")
  started_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  status <- tryCatch(
    utils::download.file(
      packages_url, packages_path, method = "libcurl", mode = "wb",
      quiet = FALSE
    ),
    error = identity
  )
  if (inherits(status, "error") || !identical(as.integer(status), 0L)) {
    cran_refresh_stop("could not download CRAN PACKAGES.gz exactly once")
  }
  packages_hash <- compat_fetch_file_hashes(
    packages_path, "downloaded CRAN PACKAGES.gz"
  )[[1L]]
  packages_info <- file.info(packages_path, extra_cols = FALSE)
  packages_receipt <- data.frame(
    url = packages_url,
    size = format(packages_info$size, scientific = FALSE, trim = TRUE),
    md5 = unname(tools::md5sum(packages_path)),
    sha256 = packages_hash,
    stringsAsFactors = FALSE
  )
  cran_refresh_write_tsv(
    packages_receipt, file.path(metadata_directory, "packages-receipt.tsv")
  )
  live <- cran_refresh_direct_reverses(cran_refresh_read_packages(packages_path))
  cran_refresh_write_tsv(
    live, file.path(metadata_directory, "live-reverses.tsv")
  )
  plan <- cran_refresh_plan(inventory, snapshot, live, review)
  cran_refresh_write_tsv(
    plan$changes, file.path(metadata_directory, "changes.tsv")
  )

  archive_results <- vector("list", nrow(plan$archive_plan))
  for (index in seq_len(nrow(plan$archive_plan))) {
    archive_row <- plan$archive_plan[index, , drop = FALSE]
    proposed_row <- plan$snapshot[index, , drop = FALSE]
    destination <- file.path(cache, archive_row$archive[[1L]])
    if (identical(archive_row$action[[1L]], "copy")) {
      old_index <- match(archive_row$package[[1L]], snapshot$package)
      old_row <- snapshot[old_index, , drop = FALSE]
      cran_refresh_copy_archive(
        file.path(canonical, old_row$archive[[1L]]), destination, old_row
      )
      sha256 <- old_row$sha256[[1L]]
      selected_url <- paste0("canonical-cache:", old_row$archive[[1L]])
    } else {
      downloaded <- cran_refresh_download_archive(proposed_row, destination)
      sha256 <- downloaded$sha256
      selected_url <- downloaded$url
    }
    plan$snapshot$sha256[[index]] <- sha256
    proposed_row$sha256[[1L]] <- sha256
    compat_fetch_verify_archive(destination, proposed_row)
    archive_results[[index]] <- data.frame(
      package = archive_row$package[[1L]],
      action = archive_row$action[[1L]],
      archive = archive_row$archive[[1L]],
      selected_source = selected_url,
      md5 = archive_row$md5[[1L]],
      sha256 = sha256,
      stringsAsFactors = FALSE
    )
  }
  archive_results <- do.call(rbind, archive_results)
  row.names(archive_results) <- NULL
  cran_refresh_write_tsv(
    archive_results, file.path(metadata_directory, "archive-actions.tsv")
  )

  proposed_inventory_path <- file.path(
    proposed_directory, "reverse-dependencies.tsv"
  )
  proposed_snapshot_path <- file.path(proposed_directory, "cran-snapshot.tsv")
  cran_refresh_write_tsv(plan$inventory, proposed_inventory_path)
  cran_refresh_write_tsv(plan$snapshot, proposed_snapshot_path)
  final_cache <- cran_refresh_cache_receipt(cache, plan$snapshot$archive)
  cran_refresh_write_tsv(
    final_cache, file.path(metadata_directory, "staged-cache.tsv")
  )
  for (index in seq_len(nrow(plan$snapshot))) {
    compat_fetch_verify_archive(
      file.path(cache, plan$snapshot$archive[[index]]),
      plan$snapshot[index, , drop = FALSE]
    )
  }

  if (!identical(
      compat_fetch_file_hashes(packages_path, "downloaded CRAN PACKAGES.gz")[[1L]],
      packages_hash
    )) {
    cran_refresh_stop("CRAN PACKAGES.gz changed after planning")
  }
  final_input_hashes <- compat_fetch_file_hashes(input_paths, "CRAN refresh input")
  if (!identical(unname(final_input_hashes), unname(input_hashes))) {
    cran_refresh_stop("CRAN refresh inputs changed during staging")
  }
  canonical_after <- cran_refresh_cache_receipt(canonical, snapshot$archive)
  if (!identical(canonical_before, canonical_after)) {
    cran_refresh_stop("canonical CRAN source cache changed during staging")
  }

  input_receipt <- data.frame(
    path = input_paths,
    sha256 = unname(input_hashes),
    stringsAsFactors = FALSE
  )
  cran_refresh_write_tsv(
    input_receipt, file.path(metadata_directory, "inputs.tsv")
  )
  input_receipt_path <- file.path(metadata_directory, "inputs.tsv")
  completed_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  completion <- data.frame(
    field = c(
      "schema", "status", "run_id", "started_utc", "completed_utc",
      "packages_gz_sha256", "live_reverse_dependencies", "staged_archives",
      "proposed_inventory_sha256", "proposed_snapshot_sha256",
      "inputs_receipt_sha256",
      "canonical_cache_receipt_sha256", "staged_cache_receipt_sha256"
    ),
    value = c(
      "1", "completed", run_id, started_utc, completed_utc, packages_hash,
      as.character(nrow(live)), as.character(nrow(final_cache)),
      unname(tools::sha256sum(proposed_inventory_path)),
      unname(tools::sha256sum(proposed_snapshot_path)),
      unname(tools::sha256sum(input_receipt_path)),
      unname(tools::sha256sum(canonical_receipt)),
      unname(tools::sha256sum(file.path(metadata_directory, "staged-cache.tsv")))
    ),
    stringsAsFactors = FALSE
  )
  completion_path <- file.path(metadata_directory, "completion.tsv")
  cran_refresh_write_tsv(completion, completion_path)
  seal <- file.path(metadata_directory, "completion.sha256")
  writeLines(unname(tools::sha256sum(completion_path)), seal, useBytes = TRUE)
  compat_fetch_require_regular(seal, "CRAN refresh completion seal")
  if (!identical(readLines(seal, warn = FALSE),
      unname(tools::sha256sum(completion_path)))) {
    cran_refresh_stop("CRAN refresh completion seal is invalid")
  }
  message("Staged authenticated CRAN refresh proposal: ", run)
  invisible(run)
}
