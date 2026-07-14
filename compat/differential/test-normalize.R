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
