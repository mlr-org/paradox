args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L || length(args) > 5L) {
  stop(paste(
    "usage: paramsetcollection-consumers.R LABEL PARADOX_LIBRARY OUTPUT_CSV",
    "[MIES_LIBRARY] [DEPENDENCY_LIBRARY]"
  ), call. = FALSE)
}

label <- args[[1L]]
paradox_library <- normalizePath(args[[2L]], mustWork = TRUE)
output <- args[[3L]]
mies_library <- normalizePath(
  if (length(args) >= 4L) args[[4L]] else
    ".local/compat/R/library-mies-diagnose",
  mustWork = TRUE
)
dependency_library <- normalizePath(
  if (length(args) >= 5L) args[[5L]] else
    ".local/compat/R/library-dependencies",
  mustWork = TRUE
)
ordinary_library <- normalizePath(".local/R/library", mustWork = TRUE)
.libPaths(unique(c(
  paradox_library,
  mies_library,
  dependency_library,
  ordinary_library,
  .libPaths()
)))

suppressPackageStartupMessages(library(paradox, lib.loc = paradox_library))
suppressPackageStartupMessages(library(miesmuschel, lib.loc = mies_library))
suppressPackageStartupMessages(library(
  mlr3pipelines,
  lib.loc = dependency_library
))
suppressPackageStartupMessages(library(bench))

if (!identical(
    normalizePath(find.package("paradox")),
    normalizePath(file.path(paradox_library, "paradox"))
  )) {
  stop("loaded the wrong paradox installation", call. = FALSE)
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

rows <- unlist(lapply(names(objects), function(case) {
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
  lapply(names(operations), function(operation_name) {
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
    data.frame(
      label = label,
      consumer_case = case,
      operation = operation_name,
      n_sets = length(object$sets),
      n_params = object$length,
      iterations = 100L,
      median_ns = as.numeric(measurement$median) * 1e9,
      mem_alloc_bytes = as.numeric(measurement$mem_alloc),
      stringsAsFactors = FALSE
    )
  })
}), recursive = FALSE)

result <- do.call(rbind, rows)
write.csv(result, output, row.names = FALSE)
print(result, row.names = FALSE)
