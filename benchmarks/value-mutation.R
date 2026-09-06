args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) {
  stop(paste(
    "usage: value-mutation.R LABEL PARADOX_LIBRARY OUTPUT_CSV",
    "MIES_LIBRARY DEPENDENCY_LIBRARY"
  ), call. = FALSE)
}

label <- args[[1L]]
paradox_library <- normalizePath(args[[2L]], mustWork = TRUE)
output <- args[[3L]]
mies_library <- normalizePath(args[[4L]], mustWork = TRUE)
dependency_library <- normalizePath(args[[5L]], mustWork = TRUE)
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

integer_param_set <- function(size) {
  params <- setNames(
    replicate(size, p_int(0L, 100L), simplify = FALSE),
    sprintf("p%02d", seq_len(size))
  )
  do.call(ps, params)
}

integer_collection <- function(size, set_count) {
  stopifnot(size %% set_count == 0L)
  child_size <- size %/% set_count
  sets <- setNames(lapply(seq_len(set_count), function(set_index) {
    params <- setNames(
      replicate(child_size, p_int(0L, 100L), simplify = FALSE),
      sprintf("p%02d", seq_len(child_size))
    )
    do.call(ps, params)
  }), sprintf("s%02d", seq_len(set_count)))
  ParamSetCollection$new(sets)
}

make_case <- function(name, iterations, object, operation, expected) {
  observed <- operation()
  stopifnot(identical(object$values, expected))
  list(
    name = name,
    iterations = iterations,
    object = object,
    operation = operation,
    expected = expected,
    observed = observed
  )
}

small <- integer_param_set(8L)
small_initial <- setNames(as.list(rep.int(1L, 8L)), small$ids())
small$values <- small_initial
small_update <- list(p02 = 2L, p07 = 7L)
small_expected <- small_initial
small_expected[names(small_update)] <- small_update

large <- integer_param_set(64L)
large_values <- setNames(as.list(rep.int(2L, 64L)), rev(large$ids()))
large_expected <- large_values[match(large$ids(), names(large_values))]

small_collection <- integer_collection(16L, 4L)
small_collection_values <- setNames(
  as.list(rep.int(3L, small_collection$length)),
  rev(small_collection$ids())
)
small_collection_expected <- small_collection_values[match(
  small_collection$ids(),
  names(small_collection_values)
)]

large_collection <- integer_collection(64L, 8L)
large_collection_values <- setNames(
  as.list(rep.int(4L, large_collection$length)),
  rev(large_collection$ids())
)
large_collection_expected <- large_collection_values[match(
  large_collection$ids(),
  names(large_collection_values)
)]

cases <- list(
  make_case(
    "paramset_set_values_insert_8",
    500L,
    small,
    function() small$set_values(.values = small_update),
    small_expected
  ),
  make_case(
    "paramset_set_values_replace_64",
    200L,
    large,
    function() large$set_values(.values = large_values, .insert = FALSE),
    large_expected
  ),
  make_case(
    "collection_assign_16",
    300L,
    small_collection,
    function() small_collection$values <- small_collection_values,
    small_collection_expected
  ),
  make_case(
    "collection_assign_64",
    150L,
    large_collection,
    function() large_collection$values <- large_collection_values,
    large_collection_expected
  )
)

consumer_objects <- list(
  mies_mutator_maybe = mut("maybe", mut("gauss"))$param_set,
  mies_optimizer = OptimizerMies$new()$param_set,
  mlr3pipelines_graph = (po("scale") %>>% po("imputemean"))$param_set
)
for (consumer_name in names(consumer_objects)) {
  object <- consumer_objects[[consumer_name]]
  values <- object$values
  cases[[length(cases) + 1L]] <- local({
    target <- object
    target_values <- values
    make_case(
      paste0(consumer_name, "_set_values_replace"),
      100L,
      target,
      function() target$set_values(
        .values = target_values,
        .insert = FALSE
      ),
      target_values
    )
  })
}

rows <- lapply(cases, function(case) {
  measurement <- bench::mark(
    case$operation(),
    iterations = case$iterations,
    min_time = 0,
    check = FALSE,
    memory = TRUE,
    filter_gc = FALSE
  )
  stopifnot(identical(case$object$values, case$expected))
  data.frame(
    label = label,
    workload = case$name,
    n_params = case$object$length,
    iterations = case$iterations,
    median_ns = as.numeric(measurement$median) * 1e9,
    mem_alloc_bytes = as.numeric(measurement$mem_alloc),
    stringsAsFactors = FALSE
  )
})

result <- do.call(rbind, rows)
write.csv(result, output, row.names = FALSE)
print(result, row.names = FALSE)
