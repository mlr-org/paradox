args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  values <- list()
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (!startsWith(key, "--") || i == length(args)) {
      stop("Expected --name value arguments; got: ", key, call. = FALSE)
    }
    values[[substring(key, 3L)]] <- args[[i + 1L]]
    i <- i + 2L
  }
  values
}

options <- parse_args(args)
required <- c(
  "library",
  "shared-library",
  "cases",
  "normalizer",
  "harness-manifest",
  "output",
  "label",
  "revision"
)
missing <- setdiff(required, names(options))
if (length(missing)) stop("Missing arguments: ", paste(missing, collapse = ", "), call. = FALSE)

private_library <- normalizePath(options$library, mustWork = TRUE)
shared_libraries <- strsplit(options$`shared-library`, .Platform$path.sep, fixed = TRUE)[[1L]]
shared_libraries <- shared_libraries[nzchar(shared_libraries)]
.libPaths(unique(c(private_library, shared_libraries, .Library)))

package_path <- normalizePath(find.package("paradox", lib.loc = private_library), mustWork = TRUE)
if (!identical(normalizePath(dirname(package_path), mustWork = TRUE), private_library)) {
  stop("paradox was not resolved from the requested private library", call. = FALSE)
}
invisible(loadNamespace("paradox", lib.loc = private_library))

normalizer_env <- new.env(parent = baseenv())
sys.source(normalizePath(options$normalizer, mustWork = TRUE), envir = normalizer_env)
if (!identical(normalizer_env$.differential_normalization_version, 3L)) {
  stop("Capture requires differential normalization version 3", call. = FALSE)
}
harness <- normalizer_env$.differential_validate_harness_manifest(
  options$`harness-manifest`
)
normalizer_env$.differential_assert_harness_path(
  harness,
  "normalizer",
  options$normalizer
)
normalizer_env$.differential_assert_harness_path(
  harness,
  "capture",
  normalizer_env$.differential_current_script_path()
)
normalizer_env$.differential_assert_harness_path(harness, "cases", options$cases)

case_env <- new.env(parent = baseenv())
sys.source(normalizePath(options$cases, mustWork = TRUE), envir = case_env)
if (!exists("paradox_differential_cases", envir = case_env, inherits = FALSE)) {
  stop("Case file must define 'paradox_differential_cases'", call. = FALSE)
}
cases <- get("paradox_differential_cases", envir = case_env, inherits = FALSE)
if (!is.list(cases) || is.null(names(cases)) || any(!nzchar(names(cases))) || anyDuplicated(names(cases))) {
  stop("'paradox_differential_cases' must be a uniquely named list", call. = FALSE)
}
available_cases <- names(cases)

selection <- options$select
if (!is.null(selection) && nzchar(selection)) {
  selection <- strsplit(selection, ",", fixed = TRUE)[[1L]]
  unknown <- setdiff(selection, names(cases))
  if (length(unknown)) stop("Unknown differential cases: ", paste(unknown, collapse = ", "), call. = FALSE)
  cases <- cases[selection]
}

validate_case <- function(case, name) {
  if (!is.list(case) || !is.function(case$run)) {
    stop(sprintf("Case '%s' must be a list with a run function", name), call. = FALSE)
  }
  if (length(case$description) != 1L || !is.character(case$description) || is.na(case$description)) {
    stop(sprintf("Case '%s' must have one description", name), call. = FALSE)
  }
  if (length(case$seed) != 1L || !is.numeric(case$seed) || is.na(case$seed)) {
    stop(sprintf("Case '%s' must have one numeric seed", name), call. = FALSE)
  }
}

condition_projection <- function(condition) {
  list(
    class = class(condition),
    message = conditionMessage(condition),
    call = if (is.null(conditionCall(condition))) NULL else paste(deparse(conditionCall(condition)), collapse = "\n")
  )
}

run_case <- function(case, name) {
  validate_case(case, name)
  warnings <- list()
  messages <- list()
  outcome <- NULL

  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  set.seed(as.integer(case$seed))

  stdout <- capture.output({
    outcome <- tryCatch(
      withCallingHandlers(
        list(status = "value", value = case$run()),
        warning = function(w) {
          warnings[[length(warnings) + 1L]] <<- condition_projection(w)
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          messages[[length(messages) + 1L]] <<- condition_projection(m)
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) list(status = "error", condition = condition_projection(e))
    )
  }, type = "output")

  normalizer_env$differential_normalize(list(
    description = case$description,
    outcome = outcome,
    warnings = warnings,
    messages = messages,
    stdout = stdout,
    rng_state_after = .Random.seed
  ))
}

results <- vector("list", length(cases))
names(results) <- names(cases)
for (i in seq_along(cases)) {
  results[[i]] <- run_case(cases[[i]], names(cases)[[i]])
}

capture <- list(
  format_version = 2L,
  normalization_version = normalizer_env$.differential_normalization_version,
  metadata = list(
    label = options$label,
    revision = options$revision,
    package_version = as.character(utils::packageVersion("paradox", lib.loc = private_library)),
    package_path = package_path,
    R_version = R.version.string,
    case_file = normalizePath(options$cases, mustWork = TRUE),
    case_file_sha256 = harness$file_sha256[["cases"]],
    harness = normalizer_env$.differential_harness_record(harness),
    available_cases = available_cases,
    selected_cases = names(cases)
  ),
  cases = results
)

saveRDS(capture, file = options$output, version = 3L)
