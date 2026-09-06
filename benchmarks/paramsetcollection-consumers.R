args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5L || length(args) > 6L) {
  stop(paste(
    "usage: paramsetcollection-consumers.R LABEL PARADOX_LIBRARY OUTPUT_CSV",
    "BRIDGE_LIBRARY DEPENDENCY_LIBRARY [SAMPLES_CSV]"
  ), call. = FALSE)
}

label <- args[[1L]]
paradox_library <- normalizePath(args[[2L]], mustWork = TRUE)
output <- args[[3L]]
bridge_library <- normalizePath(args[[4L]], mustWork = TRUE)
dependency_library <- normalizePath(args[[5L]], mustWork = TRUE)
samples_output <- if (length(args) >= 6L) {
  args[[6L]]
} else {
  paste0(tools::file_path_sans_ext(output), "-samples.csv")
}
if (identical(normalizePath(dirname(output), mustWork = TRUE),
    normalizePath(dirname(samples_output), mustWork = TRUE)) &&
    identical(basename(output), basename(samples_output))) {
  stop("summary and sample output paths must differ", call. = FALSE)
}
ordinary_library <- normalizePath(".local/R/library", mustWork = TRUE)
.libPaths(unique(c(
  paradox_library,
  bridge_library,
  dependency_library,
  ordinary_library,
  .libPaths()
)))

suppressPackageStartupMessages(library(paradox, lib.loc = paradox_library))
suppressPackageStartupMessages(library(miesmuschel, lib.loc = bridge_library))
suppressPackageStartupMessages(library(
  mlr3pipelines,
  lib.loc = bridge_library
))
suppressPackageStartupMessages(library(bench))

if (!identical(
    normalizePath(find.package("paradox")),
    normalizePath(file.path(paradox_library, "paradox"))
  )) {
  stop("loaded the wrong paradox installation", call. = FALSE)
}
for (package in c("miesmuschel", "mlr3pipelines")) {
  if (!identical(
      normalizePath(find.package(package)),
      normalizePath(file.path(bridge_library, package))
    )) {
    stop("loaded the wrong reviewed bridge installation for ", package,
      call. = FALSE)
  }
}

objects <- list(
  mies_mutator_maybe = mut("maybe", mut("gauss"))$param_set,
  mies_optimizer = OptimizerMies$new()$param_set,
  mlr3pipelines_graph = (po("scale") %>>% po("imputemean"))$param_set
)
stopifnot(all(vapply(
  objects,
  function(object) identical(
    class(object),
    c("ParamSetCollection", "ParamSet", "R6")
  ),
  logical(1L)
)))

summary_rows <- list()
sample_rows <- list()
row_index <- 0L
for (case in names(objects)) {
  object <- objects[[case]]
  expected_ids <- object$ids()
  expected_values <- object$values
  expected_get_values <- object$get_values(check_required = FALSE)
  operations <- list(
    params = list(
      run = function() object$params,
      validate = function(observed) {
        inherits(observed, "data.table") &&
          identical(observed$id, expected_ids)
      }
    ),
    values = list(
      run = function() object$values,
      validate = function(observed) {
        is.list(observed) &&
          !is.null(names(observed)) &&
          identical(observed, expected_values)
      }
    ),
    get_values_unchecked = list(
      run = function() object$get_values(check_required = FALSE),
      validate = function(observed) {
        is.list(observed) &&
          !is.null(names(observed)) &&
          identical(observed, expected_get_values)
      }
    )
  )
  for (operation_name in names(operations)) {
    operation <- operations[[operation_name]]$run
    validate <- operations[[operation_name]]$validate
    observed <- operation()
    stopifnot(validate(observed))
    measurement <- bench::mark(
      operation(),
      iterations = 100L,
      min_time = 0,
      check = FALSE,
      memory = TRUE,
      filter_gc = FALSE
    )
    elapsed_ns <- as.numeric(measurement$time[[1L]]) * 1e9
    if (length(elapsed_ns) != 100L || any(!is.finite(elapsed_ns)) ||
        any(elapsed_ns <= 0)) {
      stop("focused consumer timing produced invalid samples", call. = FALSE)
    }
    quantiles <- unname(stats::quantile(
      elapsed_ns, c(0, 0.25, 0.5, 0.75, 1), type = 8
    ))
    row_index <- row_index + 1L
    summary_rows[[row_index]] <- data.frame(
      label = label,
      consumer_case = case,
      operation = operation_name,
      n_sets = length(object$sets),
      n_params = object$length,
      iterations = 100L,
      min_ns = quantiles[[1L]],
      q25_ns = quantiles[[2L]],
      median_ns = quantiles[[3L]],
      q75_ns = quantiles[[4L]],
      max_ns = quantiles[[5L]],
      mem_alloc_bytes = as.numeric(measurement$mem_alloc),
      stringsAsFactors = FALSE
    )
    sample_rows[[row_index]] <- data.frame(
      label = label,
      consumer_case = case,
      operation = operation_name,
      iteration = seq_along(elapsed_ns),
      elapsed_ns = elapsed_ns,
      stringsAsFactors = FALSE
    )
  }
}

result <- do.call(rbind, summary_rows)
samples <- do.call(rbind, sample_rows)
write.csv(result, output, row.names = FALSE)
write.csv(samples, samples_output, row.names = FALSE)
print(result, row.names = FALSE)
