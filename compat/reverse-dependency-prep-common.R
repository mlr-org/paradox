reverse_prep_stop <- function(...) {
  stop(..., call. = FALSE)
}

reverse_prep_is_symbolic <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

reverse_prep_hard_fields <- c("Depends", "Imports", "LinkingTo")

reverse_prep_parse_dependency_field <- function(value, field, target) {
  if (length(value) != 1L || is.na(value)) {
    reverse_prep_stop("invalid ", field, " field for ", target)
  }
  value <- trimws(gsub("[[:space:]]+", " ", value))
  if (!nzchar(value)) {
    return(data.frame(
      target = character(), field = character(), dependency = character(),
      constraint = character(), spec = character(), excluded = logical(),
      stringsAsFactors = FALSE
    ))
  }

  tokens <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  if (!length(tokens) || any(!nzchar(tokens))) {
    reverse_prep_stop("malformed ", field, " field for ", target)
  }
  pattern <- paste0(
    "^([A-Za-z][A-Za-z0-9.]*)[[:space:]]*",
    "(?:\\([[:space:]]*(>=|<=|==|!=|>|<)[[:space:]]*",
    "([A-Za-z0-9][A-Za-z0-9.+-]*)[[:space:]]*\\))?$"
  )
  matches <- regexec(pattern, tokens, perl = TRUE)
  captures <- regmatches(tokens, matches)
  if (any(lengths(captures) == 0L)) {
    reverse_prep_stop(
      "unsupported or malformed ", field, " dependency for ", target, ": ",
      tokens[[which(lengths(captures) == 0L)[[1L]]]]
    )
  }
  dependency <- vapply(captures, `[[`, character(1L), 2L)
  operator <- vapply(captures, function(value) {
    if (length(value) >= 3L) value[[3L]] else ""
  }, character(1L))
  version <- vapply(captures, function(value) {
    if (length(value) >= 4L) value[[4L]] else ""
  }, character(1L))
  constraint <- ifelse(nzchar(operator), paste(operator, version), "")
  spec <- ifelse(
    nzchar(constraint),
    paste0(dependency, " (", constraint, ")"),
    dependency
  )
  data.frame(
    target = rep.int(target, length(tokens)),
    field = rep.int(field, length(tokens)),
    dependency = dependency,
    constraint = constraint,
    spec = spec,
    excluded = dependency == "paradox",
    stringsAsFactors = FALSE
  )
}

reverse_prep_description_dependencies <- function(description, target) {
  if (length(description) != 1L || !file.exists(description) ||
      dir.exists(description) || reverse_prep_is_symbolic(description)) {
    reverse_prep_stop(
      "extracted DESCRIPTION is missing, non-regular, or symbolic for ", target
    )
  }
  fields <- read.dcf(
    description,
    fields = c("Package", "Version", reverse_prep_hard_fields)
  )
  if (nrow(fields) != 1L ||
      !identical(unname(fields[[1L, "Package"]]), target)) {
    reverse_prep_stop("DESCRIPTION package disagrees with target ", target)
  }
  dependencies <- lapply(reverse_prep_hard_fields, function(field) {
    value <- fields[[1L, field]]
    if (is.na(value)) {
      return(reverse_prep_parse_dependency_field("", field, target))
    }
    reverse_prep_parse_dependency_field(unname(value), field, target)
  })
  dependencies <- do.call(rbind, dependencies)
  if (any(dependencies$dependency == target)) {
    reverse_prep_stop("reverse target declares itself as a dependency: ", target)
  }
  list(
    package = target,
    version = unname(fields[[1L, "Version"]]),
    dependencies = dependencies
  )
}

reverse_prep_write_resolver_description <- function(
    parsed, synthetic_package, path) {
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", synthetic_package)) {
    reverse_prep_stop("invalid synthetic resolver package name")
  }
  if (file.exists(path) || dir.exists(path) || reverse_prep_is_symbolic(path)) {
    reverse_prep_stop("resolver DESCRIPTION already exists: ", path)
  }
  parent <- dirname(path)
  if (!dir.exists(parent) || reverse_prep_is_symbolic(parent)) {
    reverse_prep_stop("resolver DESCRIPTION parent is not a plain directory")
  }
  dependencies <- parsed$dependencies[!parsed$dependencies$excluded, , drop = FALSE]
  record <- list(
    Package = synthetic_package,
    Version = "1.0.0",
    Title = paste("Hard Dependencies of", parsed$package),
    Description = paste(
      "Authenticated synthetic resolver input for the hard dependencies of",
      parsed$package
    ),
    License = "MIT"
  )
  for (field in reverse_prep_hard_fields) {
    specs <- dependencies$spec[dependencies$field == field]
    if (length(specs)) record[[field]] <- paste(specs, collapse = ", ")
  }
  matrix <- matrix(
    unlist(record, use.names = FALSE),
    nrow = 1L,
    dimnames = list(NULL, names(record))
  )
  write.dcf(matrix, file = path, indent = 2L, width = 78L)
  if (!file.exists(path) || dir.exists(path) || reverse_prep_is_symbolic(path)) {
    reverse_prep_stop("could not write regular resolver DESCRIPTION")
  }
  invisible(path)
}

reverse_prep_read_lockfile <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    reverse_prep_stop("jsonlite is required to authenticate the pak lockfile")
  }
  if (!file.exists(path) || dir.exists(path) || reverse_prep_is_symbolic(path)) {
    reverse_prep_stop("pak lockfile is missing, non-regular, or symbolic")
  }
  initial_sha256 <- unname(tools::sha256sum(path))
  lock <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(condition) reverse_prep_stop(
      "could not parse pak lockfile: ", conditionMessage(condition)
    )
  )
  if (!identical(lock$lockfile_version, 1L) || !is.list(lock$packages) ||
      !length(lock$packages)) {
    reverse_prep_stop("pak lockfile has an unsupported schema")
  }
  if (!file.exists(path) || dir.exists(path) || reverse_prep_is_symbolic(path) ||
      !identical(unname(tools::sha256sum(path)), initial_sha256)) {
    reverse_prep_stop("pak lockfile changed while it was authenticated")
  }
  lock
}

reverse_prep_enrich_lockfile <- function(
    input, output, source_directory, download = utils::download.file) {
  if (!exists("compat_fetch_archive_metadata", mode = "function",
      inherits = TRUE)) {
    reverse_prep_stop("source archive authenticator is unavailable")
  }
  if (file.exists(output) || dir.exists(output) ||
      reverse_prep_is_symbolic(output)) {
    reverse_prep_stop("enriched pak lockfile output already exists")
  }
  if (!dir.exists(source_directory) ||
      reverse_prep_is_symbolic(source_directory)) {
    reverse_prep_stop("retained dependency source directory is not plain")
  }
  source_directory <- normalizePath(
    source_directory, winslash = "/", mustWork = TRUE
  )
  lock <- reverse_prep_read_lockfile(input)
  retained <- list()
  for (index in seq_along(lock$packages)) {
    row <- lock$packages[[index]]
    if (!identical(row$type, "standard")) next
    sha256 <- row$sha256
    if (!is.null(sha256) && length(sha256) == 1L &&
        is.character(sha256) && grepl("^[0-9a-f]{64}$", sha256)) {
      next
    }
    repository_type <- row$repotype
    if (length(repository_type) != 1L || !is.character(repository_type) ||
        is.na(repository_type) || !nzchar(repository_type) ||
        length(row$package) != 1L || length(row$version) != 1L ||
        !grepl("^[A-Za-z][A-Za-z0-9.]*$", row$package) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9.+-]*$", row$version)) {
      reverse_prep_stop(
        "pak lockfile has an invalid checksum-less standard source row"
      )
    }
    sources <- unlist(row$sources, use.names = FALSE)
    archive_name <- paste0(row$package, "_", row$version, ".tar.gz")
    eligible <- sources[
      grepl("^https://", sources) & basename(sub("[?#].*$", "", sources)) ==
        archive_name
    ]
    if (!length(eligible)) {
      reverse_prep_stop(
        "checksum-less lock row lacks an exact HTTPS source for ", row$package
      )
    }
    destination <- file.path(source_directory, archive_name)
    if (file.exists(destination) || dir.exists(destination) ||
        reverse_prep_is_symbolic(destination)) {
      reverse_prep_stop("retained dependency source already exists: ", destination)
    }
    downloaded <- FALSE
    errors <- character()
    for (url in eligible) {
      status <- tryCatch(
        download(
          url, destination, method = "libcurl", mode = "wb", quiet = TRUE
        ),
        error = identity
      )
      if (!inherits(status, "error") && identical(as.integer(status), 0L)) {
        downloaded <- TRUE
        selected_url <- url
        break
      }
      errors <- c(errors, if (inherits(status, "error")) {
        conditionMessage(status)
      } else {
        paste0("status ", status)
      })
      if (file.exists(destination) && !dir.exists(destination) &&
          !reverse_prep_is_symbolic(destination)) {
        unlink(destination, force = FALSE)
      }
    }
    if (!downloaded) {
      reverse_prep_stop(
        "could not download exact dependency source for ", row$package,
        if (length(errors)) paste0(": ", paste(errors, collapse = " | ")) else ""
      )
    }
    compat_fetch_archive_metadata(destination, row$package, row$version)
    initial_sha256 <- unname(tools::sha256sum(destination))
    initial_md5 <- unname(tools::md5sum(destination))
    initial_size <- file.info(destination, extra_cols = FALSE)$size
    compat_fetch_archive_metadata(destination, row$package, row$version)
    if (!identical(unname(tools::sha256sum(destination)), initial_sha256) ||
        !identical(unname(tools::md5sum(destination)), initial_md5) ||
        !identical(file.info(destination, extra_cols = FALSE)$size, initial_size)) {
      reverse_prep_stop(
        "dependency source changed while authenticating ",
        row$package
      )
    }
    row$sha256 <- initial_sha256
    row$filesize <- unname(initial_size)
    row$sources <- as.list(c(
      utils::URLencode(paste0("file://", destination), reserved = FALSE),
      sources
    ))
    lock$packages[[index]] <- row
    retained[[length(retained) + 1L]] <- data.frame(
      package = row$package,
      version = row$version,
      repository_type = repository_type,
      source_url = selected_url,
      retained_archive = destination,
      size = format(initial_size, scientific = FALSE, trim = TRUE),
      md5 = initial_md5,
      sha256 = initial_sha256,
      stringsAsFactors = FALSE
    )
  }

  temporary <- paste0(output, ".new")
  if (file.exists(temporary) || dir.exists(temporary) ||
      reverse_prep_is_symbolic(temporary)) {
    reverse_prep_stop("enriched pak lockfile temporary output exists")
  }
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  jsonlite::write_json(
    lock, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  reverse_prep_read_lockfile(temporary)
  if (!file.rename(temporary, output)) {
    reverse_prep_stop("could not atomically publish enriched pak lockfile")
  }
  reverse_prep_read_lockfile(output)
  if (!length(retained)) {
    return(data.frame(
      package = character(), version = character(), repository_type = character(),
      source_url = character(), retained_archive = character(),
      size = character(), md5 = character(), sha256 = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, retained)
}

reverse_prep_version_satisfies <- function(observed, constraint) {
  if (!nzchar(constraint)) return(TRUE)
  parts <- strsplit(constraint, " ", fixed = TRUE)[[1L]]
  if (length(parts) != 2L ||
      !parts[[1L]] %in% c(">=", "<=", "==", "!=", ">", "<")) {
    reverse_prep_stop("invalid retained dependency constraint: ", constraint)
  }
  comparison <- utils::compareVersion(observed, parts[[2L]])
  switch(parts[[1L]],
    ">=" = comparison >= 0L,
    "<=" = comparison <= 0L,
    "==" = comparison == 0L,
    "!=" = comparison != 0L,
    ">" = comparison > 0L,
    "<" = comparison < 0L
  )
}

reverse_prep_lock_plan <- function(
    path, synthetic_packages, selected_targets, dependency_library,
    direct_dependencies, base_packages, retained_source_root = NULL) {
  lock <- reverse_prep_read_lockfile(path)
  packages <- lock$packages
  required_scalar <- c("package", "version", "type", "direct")
  for (index in seq_along(packages)) {
    row <- packages[[index]]
    if (!all(required_scalar %in% names(row)) ||
        length(row$package) != 1L || length(row$version) != 1L ||
        length(row$type) != 1L || length(row$direct) != 1L ||
        !is.character(row$package) || !is.character(row$version) ||
        !is.character(row$type) || !is.logical(row$direct) ||
        is.na(row$package) || is.na(row$version) || is.na(row$type) ||
        is.na(row$direct) || !nzchar(row$package) || !nzchar(row$version)) {
      reverse_prep_stop("pak lockfile contains an invalid package row")
    }
    dependency_types <- unlist(row$dep_types, use.names = FALSE)
    if (!setequal(dependency_types, reverse_prep_hard_fields) ||
        length(dependency_types) != length(reverse_prep_hard_fields)) {
      reverse_prep_stop(
        "pak lockfile contains non-hard dependency policy for ", row$package
      )
    }
  }
  package_names <- vapply(packages, `[[`, character(1L), "package")
  if (anyDuplicated(package_names)) {
    reverse_prep_stop("pak lockfile contains duplicate packages")
  }
  types <- vapply(packages, `[[`, character(1L), "type")
  direct <- vapply(packages, `[[`, logical(1L), "direct")
  if (!setequal(package_names[types == "deps"], synthetic_packages) ||
      any(!direct[types == "deps"]) || any(direct[types != "deps"]) ||
      any(!types %in% c("deps", "installed", "standard"))) {
    reverse_prep_stop("pak lockfile contains an unexpected root or source type")
  }
  paradox_index <- which(package_names == "paradox")
  if (length(paradox_index) &&
      any(types[paradox_index] != "installed" | direct[paradox_index])) {
    reverse_prep_stop("pak lockfile attempts to install or update paradox")
  }

  dependency_library <- normalizePath(
    dependency_library, winslash = "/", mustWork = TRUE
  )
  for (index in which(types == "installed")) {
    ref <- packages[[index]]$ref
    prefix <- "installed::"
    if (length(ref) != 1L || !is.character(ref) ||
        !startsWith(ref, prefix)) {
      reverse_prep_stop("pak lockfile has a malformed installed reference")
    }
    installed_path <- substring(ref, nchar(prefix) + 1L)
    expected <- file.path(dependency_library, package_names[[index]])
    if (!identical(installed_path, expected)) {
      reverse_prep_stop(
        "pak lockfile resolved an installed package outside the dependency library: ",
        package_names[[index]]
      )
    }
  }
  for (index in which(types == "standard")) {
    row <- packages[[index]]
    sha256 <- row$sha256
    sources <- unlist(row$sources, use.names = FALSE)
    if (length(sha256) != 1L || !is.character(sha256) ||
        !grepl("^[0-9a-f]{64}$", sha256) || !length(sources) ||
        any(!grepl("^(https|file)://", sources)) ||
        !any(grepl("^https://", sources))) {
      reverse_prep_stop(
        "pak lockfile lacks authenticated source metadata for ", row$package
      )
    }
    local_sources <- sources[grepl("^file://", sources)]
    if (length(local_sources)) {
      if (length(local_sources) != 1L ||
          !identical(sources[[1L]], local_sources[[1L]]) ||
          is.null(retained_source_root)) {
        reverse_prep_stop(
          "pak lockfile has an invalid retained source for ", row$package
        )
      }
      retained_source_root <- normalizePath(
        retained_source_root, winslash = "/", mustWork = TRUE
      )
      local_path <- utils::URLdecode(sub("^file://", "", local_sources))
      if (!startsWith(local_path, paste0(retained_source_root, "/")) ||
          !file.exists(local_path) || dir.exists(local_path) ||
          reverse_prep_is_symbolic(local_path) ||
          !identical(
            normalizePath(local_path, winslash = "/", mustWork = TRUE),
            local_path
          ) || !identical(unname(tools::sha256sum(local_path)), sha256)) {
        reverse_prep_stop(
          "pak lockfile retained source is absent, escaped, or changed for ",
          row$package
        )
      }
      if (exists("compat_fetch_archive_metadata", mode = "function",
          inherits = TRUE)) {
        compat_fetch_archive_metadata(local_path, row$package, row$version)
      }
    }
  }

  required <- unique(direct_dependencies$dependency[
    !direct_dependencies$excluded & direct_dependencies$dependency != "R"
  ])
  unresolved <- setdiff(required, c(package_names, base_packages))
  if (length(unresolved)) {
    reverse_prep_stop(
      "pak lockfile omitted hard dependencies: ",
      paste(sort(unresolved, method = "radix"), collapse = ", ")
    )
  }
  versions <- vapply(packages, `[[`, character(1L), "version")
  constrained <- direct_dependencies[
    !direct_dependencies$excluded &
      direct_dependencies$dependency != "R" &
      nzchar(direct_dependencies$constraint) &
      !direct_dependencies$dependency %in% base_packages,
    , drop = FALSE
  ]
  for (index in seq_len(nrow(constrained))) {
    plan_index <- match(constrained$dependency[[index]], package_names)
    if (is.na(plan_index) || !reverse_prep_version_satisfies(
        versions[[plan_index]], constrained$constraint[[index]])) {
      reverse_prep_stop(
        "pak lockfile violates the hard dependency constraint for ",
        constrained$target[[index]], ": ", constrained$spec[[index]]
      )
    }
  }
  data.frame(
    package = package_names,
    version = versions,
    type = types,
    direct = direct,
    selected_reverse_target = package_names %in% selected_targets,
    source_sha256 = vapply(packages, function(row) {
      value <- row$sha256
      if (is.null(value) || length(value) != 1L || !nzchar(value)) "-" else value
    }, character(1L)),
    stringsAsFactors = FALSE
  )
}

reverse_prep_installed_snapshot <- function(library) {
  value <- utils::installed.packages(
    lib.loc = library,
    fields = c("Built", "Repository"),
    noCache = TRUE
  )
  if (!nrow(value)) {
    return(data.frame(
      package = character(), version = character(), built = character(),
      repository = character(), stringsAsFactors = FALSE
    ))
  }
  output <- data.frame(
    package = unname(value[, "Package"]),
    version = unname(value[, "Version"]),
    built = unname(value[, "Built"]),
    repository = unname(value[, "Repository"]),
    stringsAsFactors = FALSE
  )
  output[is.na(output) | output == ""] <- "-"
  output[order(output$package, method = "radix"), , drop = FALSE]
}
