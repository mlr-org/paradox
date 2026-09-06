#!/usr/bin/env Rscript

# Installed-library development comparison; never installs or mutates a source.
# Run fresh processes in A/B/B/A order with identical CPU affinity and flags.
# Usage: Rscript --vanilla benchmarks/getters-20260905.R LIB OUTPUT.tsv [GROUP [CASE_REGEX]]
args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% 2:4)
lib = normalizePath(args[[1L]], mustWork = TRUE)
output = args[[2L]]
group = if (length(args) >= 3L) args[[3L]] else "all"
stopifnot(group %in% c("all", "columns", "related", "derived", "controls", "validation"),
  !file.exists(output), dir.exists(dirname(output)))
.libPaths(unique(c(lib, .libPaths())))
library(paradox, lib.loc = lib)
data.table::setDTthreads(1L)

make_base = function(n, type) {
  domains = switch(type,
    numeric = list(p_dbl(-10, 10), p_int(0, 100)),
    categorical = list(p_fct(c("a", "b", "c")), p_lgl()),
    mixed = list(p_dbl(-10, 10), p_int(0, 100), p_fct(c("a", "b", "c")),
      p_lgl(), p_uty())
  )
  ids = if (n == 0L) character() else paste0("x", seq_len(n))
  ParamSet$new(setNames(rep(domains, length.out = n), ids))
}
columns = c("class", "lower", "upper", "levels", "storage_type")
related = c("nlevels", "is_number", "is_categ", "is_bounded", "all_numeric",
  "all_categorical", "all_bounded", "default", "has_trafo_param", "is_logscale")
cases = list()
add = function(name, node, property) {
  force(node)
  fun = eval(substitute(function() node$PROPERTY,
    list(PROPERTY = as.name(property))))
  expected = fun()
  cases[[name]] <<- list(fun = fun, expected = expected)
}
for (n in c(0L, 5L, 50L, 500L)) {
  for (type in if (n == 0L) "mixed" else c("numeric", "categorical", "mixed")) {
    node = make_base(n, type)
    fields = c(if (group %in% c("all", "columns")) columns,
      if (group %in% c("all", "related")) related,
      if (group %in% c("all", "controls")) c("length", "is_empty", "has_deps"))
    for (field in fields) add(paste("base", type, n, field, sep = "/"), node, field)
    if (group %in% c("all", "controls")) {
      local({
        selected = node
        cases[[paste("base", type, n, "ids", sep = "/")]] <<-
          list(fun = function() selected$ids(), expected = selected$ids())
      })
    }
  }
}
if (group %in% c("all", "derived")) {
  for (n in c(5L, 50L, 500L)) {
    child = make_base(n, "mixed")
    collection = ParamSetCollection$new(list(left = child, right = child))
    shadow = ParamSetShadow$new(child, "x1")
    nodes = list(collection = collection, shadow = shadow,
      nested = ParamSetCollection$new(list(top = collection)),
      shadow_collection = ParamSetCollection$new(list(view = shadow)))
    for (kind in names(nodes)) {
      for (field in c("class", "lower", "upper", "levels", "is_number")) {
        add(paste(kind, "mixed", n, field, sep = "/"), nodes[[kind]], field)
      }
    }
  }
}
if (group %in% c("all", "controls")) {
  control_set = ps(x = p_dbl(0, 1), n = p_int(0, 10), f = p_fct(c("a", "b")))
  control_set$values = list(x = 0.5, n = 1L, f = "b")
  controls = list(
    construct_double = function() p_dbl(0, 1)$lower,
    construct_factor = function() p_fct(c("a", "b", "c"))$levels,
    construct_factor_unicode = function() p_fct(c("\u00e9", "\u00e8", "e"))$levels,
    construct_set = function() ps(x = p_dbl(0, 1), n = p_int(0, 10))$ids(),
    check_values = function() control_set$test(list(x = 0.5, n = 1L, f = "b")),
    raw_values = function() control_set$values,
    get_values = function() control_set$get_values()
  )
  for (name in names(controls)) {
    cases[[paste0("control/", name)]] = list(fun = controls[[name]],
      expected = controls[[name]]())
  }
}

if (group == "validation") {
  for (n in c(5L, 500L)) {
    for (type in c("numeric", "categorical")) local({
      node = make_base(n, type)
      # Sampling must exercise free dimensions, not the fixed-value expansion
      # of the separately populated value-reader fixture below.
      design_node = make_base(n, type)
      values = setNames(rep(if (type == "numeric") list(0.5, 2L) else list("a", TRUE),
        length.out = n), node$ids())
      node$values = values
      units = matrix(rep(0.5, n), nrow = 1L, dimnames = list(NULL, node$ids()))
      operations = list(
        values = function() node$values,
        get_values = function() node$get_values(),
        test = function() node$test(values),
        trafo = function() node$trafo(values),
        params = function() node$params$lower,
        domains = function() lapply(node$domains, function(x) x$lower),
        tags = function() node$tags,
        qunif = function() node$qunif(units),
        random = function() { set.seed(1L); generate_design_random(design_node, 8L)$data },
        subset = function() node$subset(node$ids()[1:3])$lower
      )
      for (operation in names(operations)) {
        fun = operations[[operation]]
        cases[[paste("validation", type, n, operation, sep = "/")]] <<-
          list(fun = fun, expected = fun())
      }
    })
  }
  logscale = function() p_int(1, 100, logscale = TRUE)$lower
  cases[["validation/construct_logscale"]] = list(fun = logscale, expected = logscale())
  for (type in c("numeric", "categorical")) local({
    node = make_base(5L, type)
    grid = function() generate_design_grid(node, resolution = 3L)$data
    cases[[paste0("validation/grid_", type)]] <<- list(fun = grid, expected = grid())
  })
}

if (length(args) == 4L) cases = cases[grepl(args[[4L]], names(cases))]
stopifnot(length(cases) > 0L)

# Bring the selected CPU out of its initial idle state before collecting the
# first row. This does not change a governor or assume a particular machine.
warmup_seconds = as.numeric(Sys.getenv("PARADOX_BENCH_WARMUP_SECONDS", "1"))
stopifnot(is.finite(warmup_seconds), warmup_seconds >= 1)
warmup_until = proc.time()[["elapsed"]] + warmup_seconds
repeat {
  for (i in seq_len(5000L)) invisible(cases[[1L]]$fun())
  if (proc.time()[["elapsed"]] >= warmup_until) break
}
rows = list()
iterations = as.integer(Sys.getenv("PARADOX_BENCH_ITERATIONS", "1500"))
stopifnot(length(iterations) == 1L, !is.na(iterations), iterations >= 100L)
for (name in names(cases)) {
  case = cases[[name]]
  fun = case$fun
  key = digest::digest(case$expected, algo = "sha256", serializeVersion = 2L)
  for (warmup in seq_len(10L)) stopifnot(identical(fun(), case$expected))
  for (block in seq_len(3L)) {
    invisible(gc())
    measured = bench::mark(fun(), iterations = iterations, check = FALSE,
      filter_gc = FALSE, memory = TRUE)
    rows[[length(rows) + 1L]] = data.frame(
      case = name, block = block,
      median_seconds = as.numeric(measured$median),
      mean_seconds = mean(as.numeric(measured$time[[1L]])),
      allocation_bytes = as.numeric(measured$mem_alloc),
      iterations = measured$n_itr, collections = measured$n_gc, result_key = key
    )
  }
}
write.table(do.call(rbind, rows), output, sep = "\t", quote = FALSE, row.names = FALSE)
cat("R=", as.character(getRversion()), "\n", sep = "")
cat("paradox=", as.character(packageVersion("paradox")), "\n", sep = "")
cat("warmup_seconds=", warmup_seconds, "\n", sep = "")
dll = getLoadedDLLs()[["paradox"]]
if (!is.null(dll)) {
  cat("dso=", dll[["path"]], "\n", sep = "")
  cat("dso_sha256=", unname(tools::sha256sum(dll[["path"]])), "\n", sep = "")
}
cat("output=", output, "\n", sep = "")
