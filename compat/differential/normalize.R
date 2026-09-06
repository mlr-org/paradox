# Normalization for cross-process paradox observations.
#
# The result deliberately consists only of base R objects so that it can be
# serialized and compared without loading either paradox installation.  This
# is not a general-purpose object serializer: opaque environments and external
# pointers are rejected instead of being silently discarded.

.differential_normalization_version <- 3L

.differential_abort_case <- function(case_name, condition) {
  if (length(case_name) != 1L || !is.character(case_name) ||
      is.na(case_name) || !nzchar(case_name)) {
    stop("Differential case failure has no valid case name", call. = FALSE)
  }
  if (!inherits(condition, "condition")) {
    stop(
      sprintf("Differential case `%s` failed with a malformed condition", case_name),
      call. = FALSE
    )
  }
  stop(
    sprintf(
      "Differential case `%s` aborted before returning observations: %s",
      case_name,
      conditionMessage(condition)
    ),
    call. = FALSE
  )
}

.differential_has_exact_names <- function(value, expected_names) {
  is.list(value) && identical(names(value), expected_names)
}

.differential_normalized_outcome_status <- function(case) {
  if (!.differential_has_exact_names(
        case,
        c("kind", "values", "attributes")
      ) || !identical(case[["kind", exact = TRUE]], "list")) {
    return(NA_character_)
  }
  case_values <- case[["values", exact = TRUE]]
  if (!.differential_has_exact_names(
        case_values,
        c(
          "description", "outcome", "warnings", "messages", "stdout",
          "rng_state_after"
        )
      )) {
    return(NA_character_)
  }
  outcome <- case_values[["outcome", exact = TRUE]]
  if (!.differential_has_exact_names(
        outcome,
        c("kind", "values", "attributes")
      ) || !identical(outcome[["kind", exact = TRUE]], "list")) {
    return(NA_character_)
  }
  outcome_values <- outcome[["values", exact = TRUE]]
  value_schema <- c("status", "value")
  error_schema <- c("status", "condition")
  if (!.differential_has_exact_names(outcome_values, value_schema) &&
      !.differential_has_exact_names(outcome_values, error_schema)) {
    return(NA_character_)
  }
  status <- outcome_values[["status", exact = TRUE]]
  if (!.differential_has_exact_names(
        status,
        c("kind", "type", "value", "attributes")
      ) || !identical(status[["kind", exact = TRUE]], "atomic") ||
      !identical(status[["type", exact = TRUE]], "character")) {
    return(NA_character_)
  }
  value <- status[["value", exact = TRUE]]
  if (length(value) != 1L || !is.character(value) || is.na(value) ||
      !nzchar(value)) {
    return(NA_character_)
  }
  if ((identical(value, "value") && !identical(names(outcome_values), value_schema)) ||
      (identical(value, "error") && !identical(names(outcome_values), error_schema))) {
    return(NA_character_)
  }
  value
}

.differential_assert_value_case_outcomes <- function(cases, label) {
  if (length(label) != 1L || !is.character(label) || is.na(label) ||
      !nzchar(label)) {
    stop("Differential capture outcome validation has no valid label", call. = FALSE)
  }
  if (!is.list(cases) || is.null(names(cases)) || anyNA(names(cases)) ||
      any(!nzchar(names(cases))) || anyDuplicated(names(cases))) {
    stop(sprintf("%s differential capture has a malformed case inventory", label),
      call. = FALSE)
  }
  statuses <- vapply(
    cases,
    .differential_normalized_outcome_status,
    character(1L)
  )
  invalid <- is.na(statuses) | statuses != "value"
  if (any(invalid)) {
    observed <- ifelse(is.na(statuses[invalid]), "<malformed>", statuses[invalid])
    stop(
      sprintf(
        "%s differential capture has non-value top-level case outcomes: %s",
        label,
        paste(sprintf("`%s`=%s", names(observed), observed), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.differential_harness_roles <- c(
  "runner",
  "cases",
  "normalizer",
  "test-normalizer",
  "capture",
  "compare",
  "expected-differences"
)

.differential_sha256_file <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  info <- file.info(path)
  if (is.na(info$isdir) || info$isdir) {
    stop(sprintf("Expected a regular file for SHA-256: %s", path), call. = FALSE)
  }
  hash <- unname(tools::sha256sum(path))
  if (length(hash) != 1L || is.na(hash) ||
      !grepl("^[[:xdigit:]]{64}$", hash)) {
    stop(sprintf("Could not compute SHA-256 for %s", path), call. = FALSE)
  }
  tolower(hash)
}

.differential_validate_harness_manifest <- function(path) {
  manifest_path <- normalizePath(path, mustWork = TRUE)
  manifest_hash_before <- .differential_sha256_file(manifest_path)
  manifest <- tryCatch(
    utils::read.delim(
      manifest_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    ),
    error = function(condition) {
      stop(
        sprintf(
          "Cannot read differential harness manifest %s: %s",
          manifest_path,
          conditionMessage(condition)
        ),
        call. = FALSE
      )
    }
  )

  expected_columns <- c("role", "file", "sha256", "origin")
  if (!identical(names(manifest), expected_columns) ||
      !identical(manifest$role, .differential_harness_roles) ||
      anyNA(manifest$file) || any(!nzchar(manifest$file)) ||
      anyDuplicated(manifest$file) ||
      any(dirname(manifest$file) != ".") ||
      any(basename(manifest$file) != manifest$file) ||
      anyNA(manifest$sha256) ||
      any(!grepl("^[[:xdigit:]]{64}$", manifest$sha256)) ||
      anyNA(manifest$origin) ||
      any(!manifest$origin %in% c(
        "candidate-snapshot",
        "external-override",
        "strict-empty"
      )) ||
      any(
        !manifest$role %in% c("cases", "expected-differences") &
          manifest$origin != "candidate-snapshot"
      ) ||
      manifest$origin[[match("cases", manifest$role)]] == "strict-empty") {
    stop(
      paste0(
        "Differential harness manifest must contain the exact ordered roles ",
        paste(.differential_harness_roles, collapse = ", "),
        paste0(
          " with unique local filenames, 64-digit SHA-256 values, and valid ",
          "candidate-snapshot/external-override/strict-empty origins"
        )
      ),
      call. = FALSE
    )
  }

  root <- dirname(manifest_path)
  paths <- file.path(root, manifest$file)
  missing <- !file.exists(paths) | file.info(paths)$isdir
  if (any(missing)) {
    stop(
      sprintf(
        "Differential harness file for role `%s` is missing: %s",
        manifest$role[[which(missing)[[1L]]]],
        paths[[which(missing)[[1L]]]]
      ),
      call. = FALSE
    )
  }

  observed <- vapply(paths, .differential_sha256_file, character(1L))
  expected <- tolower(manifest$sha256)
  mismatch <- observed != expected
  if (any(mismatch)) {
    index <- which(mismatch)[[1L]]
    stop(
      sprintf(
        paste0(
          "Differential harness SHA-256 mismatch for role `%s`: ",
          "expected %s, observed %s"
        ),
        manifest$role[[index]],
        expected[[index]],
        observed[[index]]
      ),
      call. = FALSE
    )
  }

  manifest_hash_after <- .differential_sha256_file(manifest_path)
  if (!identical(manifest_hash_before, manifest_hash_after)) {
    stop("Differential harness manifest changed during validation", call. = FALSE)
  }

  names(paths) <- manifest$role
  file_sha256 <- expected
  names(file_sha256) <- manifest$role
  origins <- manifest$origin
  names(origins) <- manifest$role
  list(
    manifest_path = manifest_path,
    manifest_sha256 = manifest_hash_after,
    file_sha256 = file_sha256,
    origins = origins,
    paths = paths
  )
}

.differential_harness_record <- function(harness) {
  list(
    manifest_sha256 = harness$manifest_sha256,
    file_sha256 = harness$file_sha256,
    origins = harness$origins
  )
}

.differential_assert_harness_path <- function(harness, role, path) {
  if (!role %in% names(harness$paths)) {
    stop(sprintf("Unknown differential harness role `%s`", role), call. = FALSE)
  }
  observed <- normalizePath(path, mustWork = TRUE)
  expected <- normalizePath(harness$paths[[role]], mustWork = TRUE)
  if (!identical(observed, expected)) {
    stop(
      sprintf(
        "Differential harness role `%s` resolved to %s, not authenticated path %s",
        role,
        observed,
        expected
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.differential_assert_harness_record <- function(record, harness, label) {
  expected <- .differential_harness_record(harness)
  if (!identical(record, expected)) {
    stop(
      sprintf(
        "%s differential capture does not match the authenticated harness manifest",
        label
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.differential_current_script_path <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", arguments, value = TRUE)
  if (length(file_argument) != 1L) {
    stop("Cannot identify the current Rscript file for harness authentication", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_argument), mustWork = TRUE)
}

.diff_unsafe_closure_calls <- c(
  ".C", ".Call", ".External", ".Fortran", ".Internal",
  "as.environment", "asNamespace", "assign", "attach", "baseenv",
  "callGeneric", "callNextMethod", "delayedAssign", "detach", "do.call",
  "dynGet", "emptyenv",
  "environment", "environment<-", "eval", "eval.parent", "evalq",
  "exists", "get", "get0", "getExportedValue", "getFromNamespace",
  "getGeneric", "getMethod", "getNamespace", "getOption", "getS3method",
  "globalenv", "library", "load", "loadNamespace", "local",
  "makeActiveBinding", "match.fun", "mget", "NextMethod", "options",
  "parent.env", "parent.frame", "parse", "pos.to.env", "require",
  "requireNamespace", "rm", "selectMethod", "source", "standardGeneric",
  "substitute", "Sys.getenv", "Sys.setenv", "Sys.unsetenv", "sys.frame",
  "sys.frames", "sys.function", "sys.source", "topenv",
  "unloadNamespace", "UseMethod"
)

.diff_unsafe_closure_symbols <- c(".BaseNamespaceEnv", ".GlobalEnv")
.diff_missing_argument <- unname(as.list(alist(value = )))

.diff_path <- function(path, name) {
  if (is.null(name) || is.na(name) || name == "") {
    sprintf("%s[[?]]", path)
  } else if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", name)) {
    sprintf("%s$%s", path, name)
  } else {
    sprintf("%s[[%s]]", path, encodeString(name, quote = "\""))
  }
}

.diff_deparse <- function(x) {
  paste(deparse(x, width.cutoff = 500L, control = "all"), collapse = "\n")
}

.diff_environment_label <- function(env) {
  if (identical(env, emptyenv())) return("empty")
  if (identical(env, baseenv())) return("base")
  if (identical(env, globalenv())) return("global")

  namespace_name <- tryCatch(getNamespaceName(env), error = function(...) NULL)
  if (!is.null(namespace_name)) return(sprintf("namespace:%s", namespace_name))

  name <- environmentName(env)
  if (startsWith(name, "package:")) return(name)
  if (nzchar(name)) return(sprintf("named:%s", name))
  "custom"
}

.diff_find_binding_environment <- function(name, env) {
  current <- env
  repeat {
    if (exists(name, envir = current, inherits = FALSE)) return(current)
    if (identical(current, emptyenv())) return(NULL)
    current <- parent.env(current)
  }
}

.diff_call_target <- function(call) {
  head <- call[[1L]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && length(head) == 3L &&
      is.symbol(head[[1L]]) &&
      as.character(head[[1L]]) %in% c("::", ":::") &&
      (is.symbol(head[[3L]]) ||
        (is.character(head[[3L]]) && length(head[[3L]]) == 1L))) {
    return(as.character(head[[3L]]))
  }
  NULL
}

.diff_find_unsafe_closure_constructs <- function(body) {
  pending <- list(body)
  unsafe <- character()

  while (length(pending)) {
    index <- length(pending)
    element <- pending[index]
    pending[[index]] <- NULL
    if (identical(unname(element), .diff_missing_argument)) next
    node <- element[[1L]]

    if (is.symbol(node)) {
      name <- as.character(node)
      if (name %in% .diff_unsafe_closure_symbols) unsafe <- c(unsafe, name)
      next
    }
    if (is.call(node)) {
      target <- .diff_call_target(node)
      if (!is.null(target) && target %in% .diff_unsafe_closure_calls) {
        unsafe <- c(unsafe, target)
      }
      pending <- c(pending, as.list(node))
      next
    }
    if (is.pairlist(node) || is.expression(node)) {
      pending <- c(pending, as.list(node))
    }
  }

  sort(unique(unsafe))
}

.diff_binding_is_lazy <- function(name, env, path) {
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop(
      paste0(
        "Cannot safely inspect closure binding `", name, "` at ", path,
        ": the pinned `rlang` package is unavailable"
      ),
      call. = FALSE
    )
  }
  lazy <- tryCatch(
    rlang::env_binding_are_lazy(env, name),
    error = function(condition) {
      stop(
        paste0(
          "Cannot safely inspect closure binding `", name, "` at ", path,
          ": ", conditionMessage(condition)
        ),
        call. = FALSE
      )
    }
  )
  if (length(lazy) != 1L || is.na(lazy)) {
    stop(
      sprintf("Cannot determine whether closure binding `%s` at %s is lazy", name, path),
      call. = FALSE
    )
  }
  isTRUE(unname(lazy))
}

.diff_source_attribute <- function(name) {
  name %in% c("srcref", "srcfile", "wholeSrcref")
}

.diff_data_table_selfref <- function(table, selfref) {
  status <- tryCatch(
    {
      fun <- get("selfrefok", envir = getNamespace("data.table"), inherits = FALSE)
      value <- fun(table, verbose = FALSE)
      list(kind = "value", type = typeof(value), value = unname(value))
    },
    error = function(condition) {
      list(
        kind = "error",
        class = class(condition),
        message = conditionMessage(condition)
      )
    }
  )

  list(
    kind = "data.table-selfref",
    type = typeof(selfref),
    status = status
  )
}

.diff_normalize_attributes <- function(x, path, state, closure = FALSE) {
  attrs <- attributes(x)
  if (is.null(attrs)) return(NULL)

  attr_names <- names(attrs)
  keep <- rep(TRUE, length(attrs))
  if (closure) {
    keep <- keep & !vapply(attr_names, .diff_source_attribute, logical(1L))
  }
  attrs <- attrs[keep]
  if (!length(attrs)) return(NULL)

  out <- vector("list", length(attrs))
  names(out) <- names(attrs)
  for (i in seq_along(attrs)) {
    attribute_path <- .diff_path(path, names(attrs)[[i]])
    if (inherits(x, "data.table") && identical(names(attrs)[[i]], ".internal.selfref")) {
      out[[i]] <- .diff_normalize(
        .diff_data_table_selfref(x, attrs[[i]]),
        attribute_path,
        state
      )
    } else {
      out[[i]] <- .diff_normalize(attrs[[i]], attribute_path, state)
    }
  }
  out
}

.diff_normalize_closure_environment <- function(fun, path, state) {
  env <- environment(fun)
  globals <- tryCatch(
    codetools::findGlobals(fun, merge = FALSE),
    error = function(condition) {
      stop(
        sprintf(
          "Cannot safely analyze closure globals at %s: %s",
          path,
          conditionMessage(condition)
        ),
        call. = FALSE
      )
    }
  )
  if (!is.list(globals) ||
      !is.character(globals$functions) ||
      !is.character(globals$variables)) {
    stop(sprintf("Cannot safely analyze closure globals at %s", path), call. = FALSE)
  }

  unsafe <- sort(unique(c(
    .diff_find_unsafe_closure_constructs(formals(fun)),
    .diff_find_unsafe_closure_constructs(body(fun))
  )))
  if (length(unsafe)) {
    stop(
      sprintf(
        paste0(
          "Cannot safely normalize closure at %s: dynamic lookup or ",
          "runtime evaluation via %s; return an explicit projection instead"
        ),
        path,
        paste(sprintf("`%s`", unsafe), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  referenced <- sort(unique(c(globals$functions, globals$variables)))
  bindings <- vector("list", length(referenced))
  names(bindings) <- referenced

  for (i in seq_along(referenced)) {
    name <- referenced[[i]]
    binding_env <- .diff_find_binding_environment(name, env)
    if (is.null(binding_env)) {
      bindings[[i]] <- list(status = "missing")
      next
    }

    origin <- .diff_environment_label(binding_env)
    active <- bindingIsActive(name, binding_env)
    if (active) {
      stop(
        sprintf(
          paste0(
            "Cannot safely normalize closure at %s: referenced binding `%s` ",
            "is active and would execute during inspection; return an explicit projection instead"
          ),
          path,
          name
        ),
        call. = FALSE
      )
    }
    if (.diff_binding_is_lazy(name, binding_env, path)) {
      stop(
        sprintf(
          paste0(
            "Cannot safely normalize closure at %s: referenced binding `%s` ",
            "is a promise and would be forced during inspection; return an explicit projection instead"
          ),
          path,
          name
        ),
        call. = FALSE
      )
    }

    binding <- list(
      status = "found",
      origin = origin,
      active = active,
      locked = bindingIsLocked(name, binding_env)
    )

    value <- tryCatch(
      get(name, envir = binding_env, inherits = FALSE),
      error = function(condition) {
        stop(
          sprintf(
            "Cannot read ordinary closure binding `%s` at %s: %s",
            name,
            path,
            conditionMessage(condition)
          ),
          call. = FALSE
        )
      }
    )
    value_path <- sprintf("%s<environment>::%s", path, name)
    owned_environment <- origin == "base" ||
      startsWith(origin, "namespace:") || startsWith(origin, "package:")
    if (owned_environment && is.function(value) && typeof(value) != "closure") {
      # Base and package code commonly refers to primitive functions.  They
      # cannot be traversed like closures, but their printed .Primitive(name)
      # representation is stable and distinguishes a rebound primitive.
      binding$value <- list(
        kind = "primitive-function",
        type = typeof(value),
        representation = .diff_deparse(value),
        attributes = .diff_normalize_attributes(
          value,
          paste0(value_path, "<attributes>"),
          state,
          closure = TRUE
        )
      )
    } else {
      binding$value <- .diff_normalize(value, value_path, state)
    }
    bindings[[i]] <- binding
  }

  list(
    kind = .diff_environment_label(env),
    parent_kind = if (identical(env, emptyenv())) NULL else .diff_environment_label(parent.env(env)),
    referenced_bindings = bindings
  )
}

.diff_seen_function <- function(fun, state) {
  if (!length(state$functions)) return(NULL)
  hits <- vapply(state$functions, function(previous) identical(fun, previous$value), logical(1L))
  if (!any(hits)) return(NULL)
  state$functions[[which(hits)[[1L]]]]$path
}

.diff_normalize <- function(x, path, state) {
  if (is.null(x)) return(list(kind = "NULL"))

  if (is.function(x)) {
    if (typeof(x) != "closure") {
      stop(
        sprintf(
          "Cannot normalize a %s function at %s; return an explicit projection instead",
          typeof(x),
          path
        ),
        call. = FALSE
      )
    }
    previous_path <- .diff_seen_function(x, state)
    if (!is.null(previous_path)) {
      return(list(kind = "closure-reference", target = previous_path))
    }
    state$functions[[length(state$functions) + 1L]] <- list(value = x, path = path)
    return(list(
      kind = "closure",
      formals = .diff_normalize(formals(x), paste0(path, "<formals>"), state),
      body = .diff_deparse(body(x)),
      environment = .diff_normalize_closure_environment(x, path, state),
      attributes = .diff_normalize_attributes(x, paste0(path, "<attributes>"), state, closure = TRUE)
    ))
  }

  if (inherits(x, "R6")) {
    stop(
      sprintf(
        "Cannot normalize an R6 object at %s; return an explicit projection of its observable fields instead",
        path
      ),
      call. = FALSE
    )
  }
  if (is.environment(x)) {
    stop(
      sprintf(
        "Cannot normalize an opaque environment at %s; return an explicit projection or a closure that references it",
        path
      ),
      call. = FALSE
    )
  }
  if (typeof(x) %in% c("externalptr", "weakref")) {
    stop(sprintf("Cannot normalize %s at %s", typeof(x), path), call. = FALSE)
  }

  attrs <- .diff_normalize_attributes(x, paste0(path, "<attributes>"), state)

  if (isS4(x)) {
    slot_names <- methods::slotNames(x)
    slots <- vector("list", length(slot_names))
    names(slots) <- slot_names
    for (i in seq_along(slot_names)) {
      slots[[i]] <- .diff_normalize(
        methods::slot(x, slot_names[[i]]),
        sprintf("%s@%s", path, slot_names[[i]]),
        state
      )
    }
    return(list(kind = "S4", class = class(x), slots = slots, attributes = attrs))
  }

  type <- typeof(x)
  if (type %in% c("logical", "integer", "double", "complex", "character", "raw")) {
    value <- x
    attributes(value) <- NULL
    return(list(kind = "atomic", type = type, value = value, attributes = attrs))
  }

  if (type == "list") {
    values <- vector("list", length(x))
    names(values) <- names(x)
    for (i in seq_along(x)) {
      element_name <- if (is.null(names(x))) as.character(i) else names(x)[[i]]
      values[[i]] <- .diff_normalize(x[[i]], .diff_path(path, element_name), state)
    }
    return(list(kind = "list", values = values, attributes = attrs))
  }

  if (type == "pairlist") {
    values <- as.list(x)
    normalized <- vector("list", length(values))
    names(normalized) <- names(values)
    for (i in seq_along(values)) {
      element_name <- if (is.null(names(values))) as.character(i) else names(values)[[i]]
      normalized[[i]] <- .diff_normalize(values[[i]], .diff_path(path, element_name), state)
    }
    return(list(kind = "pairlist", values = normalized, attributes = attrs))
  }

  if (type %in% c("language", "symbol", "expression")) {
    return(list(kind = type, value = .diff_deparse(x), attributes = attrs))
  }

  stop(sprintf("Unsupported R type '%s' at %s", type, path), call. = FALSE)
}

differential_normalize <- function(x) {
  state <- new.env(parent = emptyenv())
  state$functions <- list()
  .diff_normalize(x, "$", state)
}
