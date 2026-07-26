args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(1L, 2L)) {
  stop("Usage: test-normalize.R NORMALIZE.R [HARNESS-SHA256.tsv]", call. = FALSE)
}

normalizer <- new.env(parent = baseenv())
sys.source(normalizePath(args[[1L]], mustWork = TRUE), envir = normalizer)
if (!identical(normalizer$.differential_normalization_version, 3L)) {
  stop("Unexpected differential normalization version", call. = FALSE)
}

expect_error <- function(action, patterns, label) {
  condition <- tryCatch(
    {
      action()
      NULL
    },
    error = identity
  )
  if (is.null(condition) ||
      any(!vapply(patterns, grepl, logical(1L), x = conditionMessage(condition), fixed = TRUE))) {
    stop(sprintf("Expected actionable failure for %s", label), call. = FALSE)
  }
  invisible(condition)
}

if (length(args) == 2L) {
  harness <- normalizer$.differential_validate_harness_manifest(args[[2L]])
  normalizer$.differential_assert_harness_path(harness, "normalizer", args[[1L]])
  normalizer$.differential_assert_harness_path(
    harness,
    "test-normalizer",
    normalizer$.differential_current_script_path()
  )
}

local({
  valid_case <- normalizer$differential_normalize(list(
    description = "valid policy fixture",
    outcome = list(status = "value", value = 1L),
    warnings = list(),
    messages = list(),
    stdout = character(),
    rng_state_after = 1L
  ))
  normalizer$.differential_assert_value_case_outcomes(
    list(sentinel = valid_case),
    "Valid fixture"
  )
  expect_malformed <- function(case, label) {
    expect_error(
      function() normalizer$.differential_assert_value_case_outcomes(
        list(sentinel = case),
        label
      ),
      c(label, "sentinel", "<malformed>"),
      paste(label, "normalized outcome policy")
    )
  }
  expect_malformed(list(), "Malformed fixture")

  partial_case <- valid_case
  names(partial_case$values)[names(partial_case$values) == "outcome"] <-
    "outcomes"
  expect_malformed(partial_case, "Partial-match fixture")

  duplicate_wrapper <- append(
    valid_case,
    list(kind = valid_case$kind),
    after = 1L
  )
  expect_malformed(duplicate_wrapper, "Duplicate wrapper fixture")

  duplicate_outcome <- valid_case
  duplicate_outcome$values <- append(
    duplicate_outcome$values,
    list(outcome = duplicate_outcome$values$outcome),
    after = 2L
  )
  expect_malformed(duplicate_outcome, "Duplicate outcome fixture")

  duplicate_status <- valid_case
  duplicate_status$values$outcome$values <- append(
    duplicate_status$values$outcome$values,
    list(status = duplicate_status$values$outcome$values$status),
    after = 1L
  )
  expect_malformed(duplicate_status, "Duplicate status fixture")

  duplicate_value <- valid_case
  duplicate_value$values$outcome$values <- append(
    duplicate_value$values$outcome$values,
    list(value = duplicate_value$values$outcome$values$value),
    after = 2L
  )
  expect_malformed(duplicate_value, "Duplicate value fixture")
})

case_audit_path <- NULL
if (length(args) == 2L &&
    identical(harness$origins[["cases"]], "candidate-snapshot")) {
  case_audit_path <- harness$paths[["cases"]]
} else if (length(args) == 1L) {
  sibling_cases <- file.path(
    dirname(normalizePath(args[[1L]], mustWork = TRUE)),
    "cases.R"
  )
  if (file.exists(sibling_cases)) case_audit_path <- sibling_cases
}

if (!is.null(case_audit_path)) local({
  # Keep the few deliberate NSE/delayed bindings explicit; every other global
  # must resolve from the minimal case environment or baseenv().
  if (!requireNamespace("codetools", quietly = TRUE)) {
    stop("The authenticated minimal-parent audit requires codetools", call. = FALSE)
  }
  case_environment <- new.env(parent = baseenv())
  sys.source(case_audit_path, envir = case_environment)
  cases <- case_environment$paradox_differential_cases
  required_contract_cases <- c(
    "representation_inputs",
    "closed_extension_boundary",
    "additive_paramset_subclass",
    "stored_callback_srcrefs",
    "tune_token_search_space",
    "tune_token_closed_shape",
    "semantic_equality",
    "collection_detachment",
    "paramset_shadow",
    "dormant_assignment",
    "default_aware_check",
    "default_active_presence",
    "default_active_required_values",
    "filtered_constraint_input",
    "dormant_store_point_check"
  )
  missing_contract_cases <- setdiff(required_contract_cases, names(cases))
  if (length(missing_contract_cases)) {
    stop(
      paste0(
        "Default differential cases omit contract-reset observations: ",
        paste(missing_contract_cases, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  case_source <- readLines(
    case_audit_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  private_state_pattern <- paste0(
    "\\$\\.__enclos_env__|private\\$\\.",
    "(params|values|tags|deps|trafos|core|sets|translation)"
  )
  if (any(grepl(private_state_pattern, case_source, perl = TRUE))) {
    stop(
      "Default differential cases must project public state, not Paradox private fields",
      call. = FALSE
    )
  }
  functions <- list(
    `.helper:diff_case` = case_environment$diff_case,
    `.helper:observe_call` = case_environment$observe_call,
    `.helper:project_domain` = case_environment$project_domain
  )
  functions <- c(functions, lapply(cases, function(case) case$run))

  unresolved <- character()
  for (context in names(functions)) {
    globals <- codetools::findGlobals(functions[[context]], merge = FALSE)
    for (kind in c("functions", "variables")) {
      symbols <- globals[[kind]]
      missing <- symbols[
        !vapply(symbols, exists, logical(1L), envir = baseenv(), inherits = FALSE) &
          !vapply(
            symbols,
            exists,
            logical(1L),
            envir = case_environment,
            inherits = FALSE
          )
      ]
      unresolved <- c(
        unresolved,
        sprintf("%s\t%s\t%s", context, sub("s$", "", kind), missing)
      )
    }
  }
  expected_unresolved <- c(
    "additive_paramset_subclass\tvariable\tprivate",
    "additive_paramset_subclass\tvariable\tself",
    "additive_paramset_subclass\tvariable\tsuper",
    "default_active_presence\tvariable\tgate",
    "default_active_required_values\tvariable\tgate",
    "default_aware_check\tvariable\tgate",
    "diagnostics\tvariable\tabsent",
    "diagnostics\tvariable\ton",
    "domain_lazy_arguments\tvariable\tinvalid_dependency",
    "dormant_assignment\tvariable\tgate",
    "dormant_store_point_check\tvariable\tgate",
    "filtered_constraint_input\tvariable\tgate",
    "validation\tfunction\t:=",
    "validation\tvariable\tcount"
  )
  if (!identical(sort(unresolved), sort(expected_unresolved))) {
    stop(
      paste0(
        "Default differential cases have an unexpected minimal-parent global ledger: ",
        paste(sort(unresolved), collapse = "; ")
      ),
      call. = FALSE
    )
  }
})

if (length(args) == 2L) local({
  # Exercise the authenticated scripts themselves.  Calling only the shared
  # helpers would not prove that capture and comparison actually fail closed.
  directory <- tempfile("paradox-differential-outcome-policy-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE))

  roles <- normalizer$.differential_harness_roles
  fixture_paths <- file.path(directory, basename(harness$paths[roles]))
  names(fixture_paths) <- roles
  copied <- file.copy(
    harness$paths[roles],
    fixture_paths,
    overwrite = FALSE,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (any(!copied)) {
    stop("Could not create the differential outcome-policy harness", call. = FALSE)
  }
  Sys.chmod(fixture_paths, mode = "0644")

  writeLines(c(
    "diff_case <- function(description, run, seed = 1L) {",
    "  list(description = description, run = run, seed = as.integer(seed))",
    "}",
    "paradox_differential_cases <- list(",
    "  sentinel = diff_case(",
    "    \"sentinel outcome-policy fixture\",",
    "    function() stop(\"sentinel top-level abort\", call. = FALSE)",
    "  )",
    ")"
  ), fixture_paths[["cases"]], useBytes = TRUE)
  writeLines(
    "case\tbaseline_fingerprint\tcandidate_fingerprint\treason",
    fixture_paths[["expected-differences"]],
    useBytes = TRUE
  )

  origins <- rep("candidate-snapshot", length(roles))
  names(origins) <- roles
  origins[["cases"]] <- "external-override"
  origins[["expected-differences"]] <- "strict-empty"
  manifest <- data.frame(
    role = roles,
    file = basename(fixture_paths),
    sha256 = vapply(
      fixture_paths,
      normalizer$.differential_sha256_file,
      character(1L)
    ),
    origin = unname(origins),
    stringsAsFactors = FALSE
  )
  manifest_path <- file.path(directory, "harness-sha256.tsv")
  utils::write.table(
    manifest,
    file = manifest_path,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE
  )

  fixture_normalizer <- new.env(parent = baseenv())
  sys.source(fixture_paths[["normalizer"]], envir = fixture_normalizer)
  fixture_harness <- fixture_normalizer$.differential_validate_harness_manifest(
    manifest_path
  )

  run_directory <- dirname(dirname(harness$manifest_path))
  candidate_library <- normalizePath(
    file.path(run_directory, "library-candidate"),
    mustWork = TRUE
  )
  if (!dir.exists(file.path(candidate_library, "paradox"))) {
    stop("Differential outcome-policy fixture has no candidate paradox", call. = FALSE)
  }
  shared_libraries <- .libPaths()
  shared_libraries <- shared_libraries[
    normalizePath(shared_libraries, mustWork = TRUE) != candidate_library
  ]
  shared_library <- paste(shared_libraries, collapse = .Platform$path.sep)
  if (!nzchar(shared_library)) {
    stop("Differential outcome-policy fixture has no shared R library", call. = FALSE)
  }

  rscript <- file.path(R.home("bin"), "Rscript")
  run_rscript <- function(script, arguments) {
    output <- suppressWarnings(system2(
      rscript,
      c("--vanilla", shQuote(script), vapply(arguments, shQuote, character(1L))),
      stdout = TRUE,
      stderr = TRUE
    ))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    list(status = status, output = output)
  }
  assert_failed <- function(process, patterns, label) {
    if (identical(process$status, 0L) ||
        any(!vapply(
          patterns,
          grepl,
          logical(1L),
          x = paste(process$output, collapse = "\n"),
          fixed = TRUE
        ))) {
      stop(
        paste0(
          "Expected actionable subprocess failure for ", label, ":\n",
          paste(process$output, collapse = "\n")
        ),
        call. = FALSE
      )
    }
  }

  capture_output <- file.path(directory, "sentinel-capture.rds")
  capture_process <- run_rscript(
    fixture_paths[["capture"]],
    c(
      "--library", candidate_library,
      "--shared-library", shared_library,
      "--cases", fixture_paths[["cases"]],
      "--normalizer", fixture_paths[["normalizer"]],
      "--harness-manifest", manifest_path,
      "--output", capture_output,
      "--label", "sentinel",
      "--revision", "sentinel"
    )
  )
  assert_failed(
    capture_process,
    c("sentinel", "sentinel top-level abort", "aborted before returning observations"),
    "real capture top-level error policy"
  )
  if (file.exists(capture_output)) {
    stop("Failed capture wrote an observation artifact", call. = FALSE)
  }

  normalized_error <- fixture_normalizer$differential_normalize(list(
    description = "sentinel outcome-policy fixture",
    outcome = list(
      status = "error",
      condition = list(
        class = c("simpleError", "error", "condition"),
        message = "sentinel normalized abort",
        call = NULL
      )
    ),
    warnings = list(),
    messages = list(),
    stdout = character(),
    rng_state_after = 1L
  ))
  capture_metadata <- list(
    label = "sentinel",
    revision = "sentinel",
    package_version = "0.0.0",
    package_path = file.path(candidate_library, "paradox"),
    R_version = R.version.string,
    case_file = normalizePath(fixture_paths[["cases"]], mustWork = TRUE),
    case_file_sha256 = fixture_harness$file_sha256[["cases"]],
    harness = fixture_normalizer$.differential_harness_record(fixture_harness),
    available_cases = "sentinel",
    selected_cases = "sentinel"
  )
  identical_error_capture <- list(
    format_version = 2L,
    normalization_version = fixture_normalizer$.differential_normalization_version,
    metadata = capture_metadata,
    cases = list(sentinel = normalized_error)
  )
  baseline_path <- file.path(directory, "identical-error-baseline.rds")
  candidate_path <- file.path(directory, "identical-error-candidate.rds")
  saveRDS(identical_error_capture, baseline_path, version = 3L)
  saveRDS(identical_error_capture, candidate_path, version = 3L)
  report_path <- file.path(directory, "identical-error-report.rds")
  report_text_path <- file.path(directory, "identical-error-report.txt")
  compare_process <- run_rscript(
    fixture_paths[["compare"]],
    c(
      baseline_path,
      candidate_path,
      report_path,
      report_text_path,
      manifest_path,
      fixture_paths[["expected-differences"]]
    )
  )
  assert_failed(
    compare_process,
    c("Baseline", "sentinel", "error", "non-value top-level"),
    "real comparison identical-error policy"
  )
  if (file.exists(report_path) || file.exists(report_text_path)) {
    stop("Rejected identical-error comparison wrote a report", call. = FALSE)
  }
})

valid <- data.table::data.table(value = 1:2)
second_valid <- data.table::data.table(value = 1:2)
valid_marker <- normalizer$.diff_data_table_selfref(
  valid,
  attr(valid, ".internal.selfref", exact = TRUE)
)
if (!identical(valid_marker$type, "externalptr") ||
    !identical(valid_marker$status, list(kind = "value", type = "integer", value = 1L))) {
  stop("A valid data.table self-reference was not recorded correctly", call. = FALSE)
}

valid_attributes <- normalizer$.diff_normalize_attributes(
  valid,
  "$<attributes>",
  new.env(parent = emptyenv())
)
if (!identical(names(valid_attributes), names(attributes(valid))) ||
    is.null(valid_attributes$.internal.selfref)) {
  stop("Self-reference presence or attribute ordering was lost", call. = FALSE)
}
if (!identical(
  normalizer$differential_normalize(valid),
  normalizer$differential_normalize(second_valid)
)) {
  stop("Process-local data.table pointer addresses affected normalization", call. = FALSE)
}

missing <- valid
attr(missing, ".internal.selfref") <- NULL
missing_attributes <- normalizer$.diff_normalize_attributes(
  missing,
  "$<attributes>",
  new.env(parent = emptyenv())
)
if (".internal.selfref" %in% names(missing_attributes) ||
    identical(
      normalizer$differential_normalize(valid),
      normalizer$differential_normalize(missing)
    )) {
  stop("A missing data.table self-reference was hidden", call. = FALSE)
}

invalid <- valid
attr(invalid, ".internal.selfref") <- new.env(parent = emptyenv())
invalid_marker <- normalizer$.diff_data_table_selfref(
  invalid,
  attr(invalid, ".internal.selfref", exact = TRUE)
)
if (!identical(invalid_marker$type, "environment") ||
    !identical(invalid_marker$status, list(kind = "value", type = "integer", value = 0L)) ||
    identical(
      normalizer$differential_normalize(valid),
      normalizer$differential_normalize(invalid)
    )) {
  stop("An invalid data.table self-reference was hidden", call. = FALSE)
}

# A broken pointer has the same R storage type as a valid self-reference. This
# proves the comparison depends on data.table's validity result, not merely on
# retaining the attribute name and pointer type.
invalid_pointer <- valid
attr(invalid_pointer, ".internal.selfref") <- methods::new("externalptr")
invalid_pointer_marker <- normalizer$.diff_data_table_selfref(
  invalid_pointer,
  attr(invalid_pointer, ".internal.selfref", exact = TRUE)
)
if (!identical(invalid_pointer_marker$type, "externalptr") ||
    !identical(invalid_pointer_marker$status, list(kind = "value", type = "integer", value = -1L)) ||
    identical(
      normalizer$differential_normalize(valid),
      normalizer$differential_normalize(invalid_pointer)
    )) {
  stop("A broken external-pointer data.table self-reference was hidden", call. = FALSE)
}

pointer_error <- tryCatch(
  {
    normalizer$differential_normalize(attr(valid, ".internal.selfref", exact = TRUE))
    NULL
  },
  error = identity
)
if (is.null(pointer_error) || !grepl("Cannot normalize externalptr", conditionMessage(pointer_error), fixed = TRUE)) {
  stop("External pointers outside the data.table self-reference exception were accepted", call. = FALSE)
}

make_offset_closure <- function(offset) {
  env <- new.env(parent = baseenv())
  env$offset <- offset
  eval(quote(function(value) value + offset), envir = env)
}
safe_one <- normalizer$differential_normalize(make_offset_closure(1L))
safe_two <- normalizer$differential_normalize(make_offset_closure(2L))
if (identical(safe_one, safe_two)) {
  stop("Ordinary lexical closure captures were conflated", call. = FALSE)
}
plus_binding <- safe_one$environment$referenced_bindings[["+"]]
if (!identical(plus_binding$value$kind, "primitive-function") ||
    !identical(plus_binding$value$type, "builtin") ||
    !identical(plus_binding$value$representation, ".Primitive(\"+\")")) {
  stop("A referenced base primitive was not captured deterministically", call. = FALSE)
}
expect_error(
  function() normalizer$differential_normalize(get("+", envir = baseenv())),
  "Cannot normalize a builtin function",
  "a primitive outside the narrow closure-binding representation"
)

promise_env <- new.env(parent = baseenv())
promise_env$forced <- 0L
delayedAssign(
  "offset",
  {
    forced <- forced + 1L
    3L
  },
  eval.env = promise_env,
  assign.env = promise_env
)
promise_closure <- eval(quote(function(value) value + offset), envir = promise_env)
expect_error(
  function() normalizer$differential_normalize(promise_closure),
  c("binding `offset`", "promise"),
  "a delayed closure binding"
)
if (!identical(promise_env$forced, 0L)) {
  stop("Closure normalization forced a delayed binding", call. = FALSE)
}

active_env <- new.env(parent = baseenv())
active_reads <- 0L
makeActiveBinding(
  "offset",
  local({
    target <- active_env
    function(value) {
      if (!missing(value)) stop("read-only test binding")
      active_reads <<- active_reads + 1L
      eval(
        substitute(function(input) input + offset + N, list(N = active_reads)),
        envir = target
      )
    }
  }),
  active_env
)
active_closure <- eval(quote(function(value) value + offset), envir = active_env)
expect_error(
  function() normalizer$differential_normalize(active_closure),
  c("binding `offset`", "active"),
  "an active closure binding"
)
if (!identical(active_reads, 0L)) {
  stop("Closure normalization executed an active binding", call. = FALSE)
}

dynamic_env <- new.env(parent = baseenv())
dynamic_env$x <- 1L
dynamic_closure <- eval(quote(function() get("x")), envir = dynamic_env)
expect_error(
  function() normalizer$differential_normalize(dynamic_closure),
  c("dynamic lookup", "`get`"),
  "dynamic get() lookup"
)
qualified_dynamic <- eval(quote(function() base::get("x")), envir = dynamic_env)
expect_error(
  function() normalizer$differential_normalize(qualified_dynamic),
  c("dynamic lookup", "`get`"),
  "namespace-qualified dynamic get() lookup"
)

default_dynamic <- eval(
  quote(function(value = get("x")) value),
  envir = dynamic_env
)
expect_error(
  function() normalizer$differential_normalize(default_dynamic),
  c("dynamic lookup", "`get`"),
  "dynamic lookup in a default argument"
)

unsafe_dynamic_closures <- list(
  getFromNamespace = quote(function() getFromNamespace("letters", "base")),
  qualified_getFromNamespace = quote(
    function() base::getFromNamespace("letters", "base")
  ),
  getOption = quote(function() getOption("paradox.differential.audit")),
  qualified_getOption = quote(
    function() base::getOption("paradox.differential.audit")
  ),
  Sys.getenv = quote(function() Sys.getenv("PARADOX_DIFFERENTIAL_AUDIT")),
  qualified_Sys.getenv = quote(
    function() base::Sys.getenv("PARADOX_DIFFERENTIAL_AUDIT")
  ),
  UseMethod = quote(function(object) UseMethod("paradox_differential_audit")),
  qualified_UseMethod = quote(
    function(object) base::UseMethod("paradox_differential_audit")
  )
)
unsafe_targets <- c(
  getFromNamespace = "getFromNamespace",
  qualified_getFromNamespace = "getFromNamespace",
  getOption = "getOption",
  qualified_getOption = "getOption",
  Sys.getenv = "Sys.getenv",
  qualified_Sys.getenv = "Sys.getenv",
  UseMethod = "UseMethod",
  qualified_UseMethod = "UseMethod"
)
for (label in names(unsafe_dynamic_closures)) {
  closure <- eval(unsafe_dynamic_closures[[label]], envir = dynamic_env)
  target <- unsafe_targets[[label]]
  expect_error(
    function() normalizer$differential_normalize(closure),
    c("dynamic lookup", sprintf("`%s`", target)),
    sprintf("%s dynamic closure access", label)
  )
}

local({
  namespace <- asNamespace("stats")
  candidates <- c(
    ".__global__", ".noGenerics", ".packageName", "p.adjust.methods",
    "port_cpos", "port_v_nms"
  )
  candidates <- candidates[vapply(
    candidates,
    exists,
    logical(1L),
    envir = namespace,
    inherits = FALSE
  )]
  lazy <- rlang::env_binding_are_lazy(namespace, candidates)
  candidates <- candidates[unname(lazy)]
  if (!length(candidates)) {
    stop("No untouched lazy stats namespace binding was available", call. = FALSE)
  }
  name <- candidates[[1L]]
  namespace_closure <- eval(
    as.call(list(as.name("function"), pairlist(), as.name(name))),
    envir = namespace
  )
  expect_error(
    function() normalizer$differential_normalize(namespace_closure),
    c(sprintf("binding `%s`", name), "promise"),
    "a lazy namespace binding"
  )
  if (!isTRUE(unname(rlang::env_binding_are_lazy(namespace, name)))) {
    stop("Closure normalization forced a lazy namespace binding", call. = FALSE)
  }

  original <- get(name, envir = namespace, inherits = FALSE)
  if (!is.atomic(original) || !length(original)) {
    stop("The forced namespace fixture was not an ordinary atomic value", call. = FALSE)
  }
  if (isTRUE(unname(rlang::env_binding_are_lazy(namespace, name)))) {
    stop("Reading the namespace fixture did not force its promise", call. = FALSE)
  }
  before <- normalizer$differential_normalize(namespace_closure)
  was_locked <- bindingIsLocked(name, namespace)
  on.exit({
    if (bindingIsLocked(name, namespace)) unlockBinding(name, namespace)
    assign(name, original, envir = namespace)
    if (was_locked) lockBinding(name, namespace)
  })
  if (was_locked) unlockBinding(name, namespace)
  assign(name, c(original, original[[1L]]), envir = namespace)
  if (was_locked) lockBinding(name, namespace)
  after <- normalizer$differential_normalize(namespace_closure)
  if (identical(before, after)) {
    stop("A changed namespace binding was omitted from closure normalization", call. = FALSE)
  }
})

local({
  directory <- tempfile("paradox-differential-cross-process-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE))
  probe <- file.path(directory, "probe.R")
  outputs <- file.path(directory, c("first.rds", "second.rds"))
  writeLines(c(
    "arguments <- commandArgs(trailingOnly = TRUE)",
    "normalizer <- new.env(parent = baseenv())",
    "sys.source(normalizePath(arguments[[1L]], mustWork = TRUE), envir = normalizer)",
    "closure_env <- new.env(parent = baseenv())",
    "closure_env$offset <- 3L",
    "closure <- eval(quote(function(value = 1L) value + offset), envir = closure_env)",
    "attr(closure, \"audit\") <- c(\"x\", \"y\")",
    "fixture <- list(",
    "  atomics = list(",
    "    c(TRUE, FALSE, NA), c(NA_integer_, 1L),",
    "    c(NA_real_, NaN, Inf, -Inf, -0), c(1 + 2i, NA_complex_),",
    "    c(\"a\", NA_character_), as.raw(0:3)",
    "  ),",
    "  nested = structure(list(a = 1L, b = list(z = \"x\")), class = \"audit\"),",
    "  frame = data.frame(x = 1:2, y = c(\"a\", \"b\"), stringsAsFactors = FALSE),",
    "  table = data.table::data.table(x = 1:2),",
    "  language = list(quote(foo(a = 1L)), as.name(\"odd name\"), expression(x + 1)),",
    "  pair = pairlist(a = 1L, b = quote(x)),",
    "  closure = closure",
    ")",
    "saveRDS(normalizer$differential_normalize(fixture), arguments[[2L]], version = 3L)"
  ), probe, useBytes = TRUE)

  rscript <- file.path(R.home("bin"), "Rscript")
  normalizer_path <- normalizePath(args[[1L]], mustWork = TRUE)
  for (output in outputs) {
    process_output <- suppressWarnings(system2(
      rscript,
      c("--vanilla", shQuote(probe), shQuote(normalizer_path), shQuote(output)),
      stdout = TRUE,
      stderr = TRUE
    ))
    status <- attr(process_output, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L) {
      stop(
        paste(
          "Cross-process normalization fixture failed:",
          paste(process_output, collapse = "\n")
        ),
        call. = FALSE
      )
    }
  }
  hashes <- vapply(outputs, normalizer$.differential_sha256_file, character(1L))
  if (!identical(hashes[[1L]], hashes[[2L]])) {
    stop("Ordinary objects or closures normalized differently across processes", call. = FALSE)
  }
})

local({
  directory <- tempfile("paradox-differential-provenance-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE))

  roles <- normalizer$.differential_harness_roles
  files <- c(
    runner = "run",
    cases = "cases.R",
    normalizer = "normalize.R",
    `test-normalizer` = "test-normalize.R",
    capture = "capture.R",
    compare = "compare.R",
    `expected-differences` = "expected-differences.tsv"
  )
  for (role in roles) {
    writeLines(sprintf("fixture for %s", role), file.path(directory, files[[role]]))
  }
  manifest <- data.frame(
    role = roles,
    file = unname(files[roles]),
    sha256 = vapply(
      file.path(directory, unname(files[roles])),
      normalizer$.differential_sha256_file,
      character(1L)
    ),
    origin = rep("candidate-snapshot", length(roles)),
    stringsAsFactors = FALSE
  )
  manifest_path <- file.path(directory, "harness-sha256.tsv")
  utils::write.table(
    manifest,
    manifest_path,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )

  validated <- normalizer$.differential_validate_harness_manifest(manifest_path)
  record <- normalizer$.differential_harness_record(validated)
  normalizer$.differential_assert_harness_record(record, validated, "Fixture")
  mismatched_record <- record
  mismatched_record$manifest_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    function() normalizer$.differential_assert_harness_record(
      mismatched_record,
      validated,
      "Fixture"
    ),
    "does not match the authenticated harness manifest",
    "capture provenance mismatch"
  )

  write("tamper", file.path(directory, files[["cases"]]), append = TRUE)
  expect_error(
    function() normalizer$.differential_validate_harness_manifest(manifest_path),
    c("SHA-256 mismatch", "role `cases`"),
    "harness file hash mismatch"
  )
})

cat("differential normalizer self-test: PASS\n")
