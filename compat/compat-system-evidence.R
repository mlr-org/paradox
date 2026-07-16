# Bind the optional Linux compatibility-system overlay to retained consumer
# evidence. This helper is sourced by compatibility harnesses; it has no
# package runtime role.

compat_system_is_symbolic <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

compat_system_require_plain_file <- function(path, label) {
  if (!file.exists(path) || dir.exists(path) || compat_system_is_symbolic(path)) {
    stop(label, " is absent, not regular, or symbolic: ", path, call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

compat_system_require_plain_directory <- function(path, label) {
  if (!dir.exists(path) || compat_system_is_symbolic(path)) {
    stop(label, " is absent, not a directory, or symbolic: ", path, call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

compat_system_write_tsv <- function(value, path) {
  temporary <- paste0(path, ".new")
  if (file.exists(temporary) || compat_system_is_symbolic(temporary)) {
    stop("compatibility-system temporary evidence path exists: ", temporary,
      call. = FALSE)
  }
  on.exit(unlink(temporary), add = TRUE)
  utils::write.table(
    value,
    temporary,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) {
    stop("could not atomically retain compatibility-system evidence: ", path,
      call. = FALSE)
  }
  invisible(path)
}

compat_system_run_verifier <- function(bootstrap, context) {
  output <- suppressWarnings(system2(
    bootstrap,
    "--verify",
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status", exact = TRUE)
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    stop(
      "compatibility-system verification failed ", context,
      if (length(output)) paste0(": ", paste(tail(output, 20L), collapse = "\n")) else "",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

compat_system_child_environment <- function() {
  active_root <- Sys.getenv("PARADOX_COMPAT_SYSTEM_ACTIVE_ROOT", unset = "")
  receipt <- Sys.getenv("PARADOX_COMPAT_SYSTEM_RECEIPT", unset = "")
  if (xor(nzchar(active_root), nzchar(receipt))) {
    stop("compatibility-system activation environment is incomplete", call. = FALSE)
  }
  if (!nzchar(active_root)) return(character())
  child_path <- Sys.getenv(
    "PARADOX_COMPAT_SYSTEM_CHILD_PATH", unset = NA_character_
  )
  child_path_missing <- length(child_path) != 1L || anyNA(child_path) ||
    !nzchar(child_path)
  variables <- c(
    "PARADOX_COMPAT_SYSTEM_ACTIVE_ROOT", "PARADOX_COMPAT_SYSTEM_ROOT",
    "PARADOX_COMPAT_SYSTEM_GEO", "PARADOX_COMPAT_SYSTEM_P1",
    "PARADOX_COMPAT_SYSTEM_RECEIPT",
    "PKG_CONFIG", "PKG_CONFIG_PATH", "PKG_CONFIG_LIBDIR",
    "CMAKE_PREFIX_PATH",
    "PROJ_DATA", "GDAL_DATA", "UDUNITS2_XML_PATH",
    "UDUNITS2_INCLUDE", "UDUNITS2_LIBS",
    "JAVA_HOME", "JAVA", "JAVAC", "JAR", "JAVA_CPPFLAGS", "JAVA_LIBS",
    "JAVA_LD_LIBRARY_PATH", "R_JAVA_LD_LIBRARY_PATH", "LD_LIBRARY_PATH",
    "CARGO_HOME", "RUSTUP_HOME",
    "CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER",
    "MPI_ROOT", "MPI_INCLUDE", "MPI_LIB_PATH", "MPI_TYPE", "OPAL_PREFIX",
    "OMPI_CC", "OMPI_CXX", "OMPI_FC", "OMPI_MCA_plm_ssh_agent",
    "PRTE_MCA_rmaps_default_mapping_policy",
    "FONTCONFIG_PATH", "FONTCONFIG_FILE", "R_MAKEVARS_USER"
  )
  values <- Sys.getenv(variables, unset = NA_character_)
  if (child_path_missing || anyNA(values) || any(!nzchar(values))) {
    stop(
      "active compatibility-system environment lacks: ",
      paste(c(
        if (child_path_missing) {
          "PARADOX_COMPAT_SYSTEM_CHILD_PATH"
        },
        variables[is.na(values) | !nzchar(values)]
      ), collapse = ", "),
      call. = FALSE
    )
  }
  names(values) <- variables
  c(PATH = child_path, values)
}

compat_system_capture_evidence <- function(root, metadata_directory) {
  root <- compat_system_require_plain_directory(root, "compatibility repository root")
  metadata_directory <- compat_system_require_plain_directory(
    metadata_directory,
    "compatibility evidence metadata directory"
  )
  active_root <- Sys.getenv("PARADOX_COMPAT_SYSTEM_ACTIVE_ROOT", unset = "")
  receipt_environment <- Sys.getenv("PARADOX_COMPAT_SYSTEM_RECEIPT", unset = "")
  if (xor(nzchar(active_root), nzchar(receipt_environment))) {
    stop("compatibility-system activation environment is incomplete", call. = FALSE)
  }
  active <- nzchar(active_root)
  helper <- compat_system_require_plain_file(
    file.path(root, "compat", "compat-system-evidence.R"),
    "compatibility-system evidence helper"
  )
  status_path <- file.path(metadata_directory, "compat-system.tsv")
  if (file.exists(status_path) || compat_system_is_symbolic(status_path)) {
    stop("compatibility-system status evidence already exists: ", status_path,
      call. = FALSE)
  }

  if (!active) {
    retained_helper <- file.path(metadata_directory, "compat-system-evidence.R")
    if (compat_system_is_symbolic(retained_helper) || dir.exists(retained_helper)) {
      stop("retained compatibility-system evidence helper is not a plain file",
        call. = FALSE)
    }
    if (!file.exists(retained_helper) &&
        !file.copy(helper, retained_helper, copy.mode = TRUE, copy.date = TRUE)) {
      stop("could not retain compatibility-system evidence helper", call. = FALSE)
    }
    helper_sha256 <- unname(tools::sha256sum(helper))
    if (!identical(
      helper_sha256,
      unname(tools::sha256sum(retained_helper))
    )) {
      stop("retained compatibility-system evidence helper differs from source",
        call. = FALSE)
    }
    status <- data.frame(
      field = c("schema", "active", "evidence_helper_sha256"),
      value = c("1", "false", helper_sha256),
      stringsAsFactors = FALSE
    )
    compat_system_write_tsv(status, status_path)
    return(list(
      active = FALSE,
      active_root = "",
      receipt_environment = "",
      child_environment = character(),
      source_paths = helper,
      source_sha256 = helper_sha256,
      retained_paths = c(retained_helper, status_path),
      retained_sha256 = unname(tools::sha256sum(c(
        retained_helper, status_path
      )))
    ))
  }

  active_root <- compat_system_require_plain_directory(
    active_root,
    "active compatibility-system repository root"
  )
  if (!identical(active_root, root)) {
    stop("compatibility-system activation selected another checkout", call. = FALSE)
  }
  receipt_expected <- file.path(root, ".local", "receipts", "compat-system", "receipt.tsv")
  receipt_environment <- compat_system_require_plain_file(
    receipt_environment,
    "active compatibility-system receipt"
  )
  receipt_expected <- compat_system_require_plain_file(
    receipt_expected,
    "expected compatibility-system receipt"
  )
  if (!identical(receipt_environment, receipt_expected)) {
    stop("compatibility-system receipt does not belong to this checkout",
      call. = FALSE)
  }
  child_environment <- compat_system_child_environment()

  source_paths <- c(
    receipt = receipt_expected,
    receipt_seal = file.path(dirname(receipt_expected), "receipt.sha256"),
    geo_lock = file.path(root, "environment", "compat-system-geo-linux-64.lock"),
    p1_lock = file.path(root, "environment", "compat-system-p1-linux-64.lock"),
    bootstrap = file.path(root, "scripts", "bootstrap-compat-system"),
    activation = file.path(root, "scripts", "activate-compat-system"),
    makevars = file.path(root, ".local", "compat", "system", "Makevars"),
    evidence_helper = helper
  )
  source_paths <- vapply(
    seq_along(source_paths),
    function(index) compat_system_require_plain_file(
      source_paths[[index]],
      paste0("compatibility-system ", names(source_paths)[[index]])
    ),
    character(1L)
  )
  names(source_paths) <- c(
    "receipt", "receipt_seal", "geo_lock", "p1_lock", "bootstrap",
    "activation", "makevars", "evidence_helper"
  )
  bootstrap <- unname(source_paths[["bootstrap"]])
  compat_system_run_verifier(bootstrap, "before retaining consumer evidence")
  source_sha256 <- unname(vapply(source_paths, tools::sha256sum, character(1L)))

  retained_names <- c(
    "compat-system-receipt.tsv",
    "compat-system-receipt.sha256",
    "compat-system-geo-linux-64.lock",
    "compat-system-p1-linux-64.lock",
    "bootstrap-compat-system",
    "activate-compat-system",
    "compat-system-Makevars",
    "compat-system-evidence.R"
  )
  retained_paths <- file.path(metadata_directory, retained_names)
  for (index in seq_along(retained_paths)) {
    destination <- retained_paths[[index]]
    if (compat_system_is_symbolic(destination) || dir.exists(destination)) {
      stop("compatibility-system retained input is not a plain file: ",
        destination, call. = FALSE)
    }
    if (file.exists(destination)) {
      if (!identical(
        unname(tools::sha256sum(source_paths[[index]])),
        unname(tools::sha256sum(destination))
      )) {
        stop("pre-retained compatibility-system input differs from source",
          call. = FALSE)
      }
    } else if (!file.copy(
      source_paths[[index]],
      destination,
      copy.mode = TRUE,
      copy.date = TRUE
    )) {
      stop("could not retain compatibility-system input: ", destination,
        call. = FALSE)
    }
  }
  retained_sha256 <- unname(vapply(retained_paths, tools::sha256sum, character(1L)))
  if (!identical(source_sha256, retained_sha256)) {
    stop("retained compatibility-system inputs differ from live inputs",
      call. = FALSE)
  }

  status <- data.frame(
    field = c(
      "schema", "active", "platform", "receipt_sha256",
      "receipt_seal_sha256", "geo_lock_sha256", "p1_lock_sha256",
      "bootstrap_sha256", "activation_sha256", "makevars_sha256",
      "evidence_helper_sha256"
    ),
    value = c("1", "true", "linux-64", source_sha256),
    stringsAsFactors = FALSE
  )
  compat_system_write_tsv(status, status_path)
  retained_paths <- c(
    retained_paths,
    status = status_path
  )
  retained_sha256 <- unname(vapply(retained_paths, tools::sha256sum, character(1L)))
  list(
    active = TRUE,
    active_root = active_root,
    receipt_environment = receipt_environment,
    child_environment = child_environment,
    source_paths = unname(source_paths),
    source_sha256 = source_sha256,
    retained_paths = unname(retained_paths),
    retained_sha256 = retained_sha256
  )
}

compat_system_verify_evidence <- function(state, context) {
  if (!is.list(state) ||
      !identical(state$active, nzchar(state$active_root)) ||
      !is.character(context) || length(context) != 1L || !nzchar(context)) {
    stop("invalid compatibility-system evidence state", call. = FALSE)
  }
  active_root <- Sys.getenv("PARADOX_COMPAT_SYSTEM_ACTIVE_ROOT", unset = "")
  receipt_environment <- Sys.getenv("PARADOX_COMPAT_SYSTEM_RECEIPT", unset = "")
  if (!identical(active_root, state$active_root) ||
      !identical(receipt_environment, state$receipt_environment)) {
    stop("compatibility-system activation changed ", context, call. = FALSE)
  }
  if (!identical(
      compat_system_child_environment(),
      state$child_environment
    )) {
    stop("compatibility-system child environment changed ", context,
      call. = FALSE)
  }
  source_sha256 <- unname(vapply(
    state$source_paths,
    function(path) {
      compat_system_require_plain_file(path, "protected compatibility-system input")
      tools::sha256sum(path)
    },
    character(1L)
  ))
  retained_sha256 <- unname(vapply(
    state$retained_paths,
    function(path) {
      compat_system_require_plain_file(path, "retained compatibility-system input")
      tools::sha256sum(path)
    },
    character(1L)
  ))
  if (!identical(source_sha256, state$source_sha256) ||
      !identical(retained_sha256, state$retained_sha256)) {
    stop("compatibility-system provenance changed ", context, call. = FALSE)
  }
  if (state$active) {
    bootstrap <- state$source_paths[grepl("/bootstrap-compat-system$", state$source_paths)]
    if (length(bootstrap) != 1L) {
      stop("compatibility-system bootstrap binding is ambiguous", call. = FALSE)
    }
    compat_system_run_verifier(bootstrap, context)
  }
  invisible(TRUE)
}
