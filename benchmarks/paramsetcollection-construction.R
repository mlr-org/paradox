args = commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) {
  stop(
    paste(
      "usage: paramsetcollection-construction.R LABEL PARADOX_LIBRARY",
      "OUTPUT_CSV MIES_LIBRARY DEPENDENCY_LIBRARY"
    ),
    call. = FALSE
  )
}

label = args[[1L]]
paradox_library = normalizePath(args[[2L]], mustWork = TRUE)
output = args[[3L]]
mies_library = normalizePath(args[[4L]], mustWork = TRUE)
dependency_library = normalizePath(args[[5L]], mustWork = TRUE)
libraries = c(
  paradox_library,
  mies_library,
  dependency_library,
  ".local/compat/R/library-mlr3verse-core",
  ".local/R/library"
)
.libPaths(unique(c(
  vapply(libraries, normalizePath, character(1L), mustWork = TRUE),
  .libPaths()
)))

suppressPackageStartupMessages(library(paradox, lib.loc = paradox_library))
suppressPackageStartupMessages(library(bench))
stopifnot(identical(
  normalizePath(find.package("paradox")),
  normalizePath(file.path(paradox_library, "paradox"))
))

make_child = function(child_index, rich, n_params = 8L) {
  ids = sprintf("p%02d", seq_len(n_params))
  domains = lapply(seq_len(n_params), function(index) {
    tags = if (rich) {
      c(sprintf("child_%02d", child_index), if (index %% 2L) "odd" else "even")
    } else {
      character()
    }
    switch(as.character((index - 1L) %% 4L),
      "0" = p_dbl(
        -10,
        10,
        tags = tags,
        trafo = if (rich) function(x) exp(x / 10) else NULL
      ),
      "1" = p_int(-20L, 20L, tags = tags),
      "2" = p_fct(letters[1:8], tags = tags),
      "3" = p_lgl(tags = tags)
    )
  })
  names(domains) = ids
  ParamSet$new(domains)
}

make_spec = function(n_children, rich) {
  sets = lapply(seq_len(n_children), make_child, rich = rich)
  names(sets) = sprintf("set%03d", seq_len(n_children))
  list(
    sets = sets,
    tag_sets = rich,
    tag_params = rich,
    postfix_names = FALSE
  )
}

specs = list(
  plain_8x8 = make_spec(8L, FALSE),
  rich_8x8 = make_spec(8L, TRUE),
  plain_32x8 = make_spec(32L, FALSE),
  rich_32x8 = make_spec(32L, TRUE)
)

suppressPackageStartupMessages(library(
  miesmuschel,
  lib.loc = mies_library
))
specs$mies_mutator_maybe = list(
  sets = mut("maybe", mut("gauss"))$param_set$sets,
  tag_sets = FALSE,
  tag_params = FALSE,
  postfix_names = FALSE
)
specs$mies_optimizer = list(
  sets = OptimizerMies$new()$param_set$sets,
  tag_sets = FALSE,
  tag_params = FALSE,
  postfix_names = FALSE
)

suppressPackageStartupMessages(library(
  mlr3pipelines,
  lib.loc = dependency_library
))
specs$mlr3pipelines_graph = list(
  sets = (po("scale") %>>% po("imputemean"))$param_set$sets,
  tag_sets = FALSE,
  tag_params = FALSE,
  postfix_names = FALSE
)

iterations = c(
  plain_8x8 = 30L,
  rich_8x8 = 20L,
  plain_32x8 = 8L,
  rich_32x8 = 5L,
  mies_mutator_maybe = 50L,
  mies_optimizer = 50L,
  mlr3pipelines_graph = 50L
)

native_symbol = get0(
  "C_param_set_collection_construct",
  envir = asNamespace("paradox"),
  inherits = FALSE
)

rows = Map(function(spec, scenario) {
  operation = function() {
    ParamSetCollection$new(
      spec$sets,
      tag_sets = spec$tag_sets,
      tag_params = spec$tag_params,
      postfix_names = spec$postfix_names
    )
  }
  expected = operation()
  validate = function(observed) {
    identical(observed$ids(), expected$ids()) &&
      identical(observed$tags, expected$tags) &&
      length(observed$sets) == length(spec$sets) &&
      all(vapply(seq_along(spec$sets), function(index) {
        identical(observed$sets[[index]], spec$sets[[index]])
      }, logical(1L)))
  }
  stopifnot(validate(operation()))
  for (warmup in seq_len(3L)) invisible(operation())
  invisible(gc())
  measurement = bench::mark(
    operation(),
    iterations = iterations[[scenario]],
    min_time = 0,
    check = FALSE,
    memory = TRUE,
    filter_gc = FALSE
  )
  observed = operation()
  stopifnot(validate(observed))
  native_accepted = if (is.null(native_symbol)) {
    NA
  } else {
    !is.null(.Call(
      native_symbol,
      spec$sets,
      spec$tag_sets,
      spec$tag_params,
      spec$postfix_names
    ))
  }
  data.frame(
    label = label,
    scenario = scenario,
    n_sets = length(spec$sets),
    n_params = observed$length,
    iterations = iterations[[scenario]],
    native_accepted = native_accepted,
    median_ns = as.numeric(measurement$median) * 1e9,
    mem_alloc_bytes = as.numeric(measurement$mem_alloc),
    gc_per_second = as.numeric(measurement$`gc/sec`),
    stringsAsFactors = FALSE
  )
}, specs, names(specs))

result = do.call(rbind, rows)
write.csv(result, output, row.names = FALSE)
print(result, row.names = FALSE)
