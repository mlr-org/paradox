library(bench)
library(paradox)

arguments = commandArgs(trailingOnly = TRUE)
n = if (length(arguments)) as.integer(arguments[[1L]]) else 500L
iterations = if (length(arguments) > 1L) as.integer(arguments[[2L]]) else 2000L

domain_prototypes = list(
  p_int(1, 100, tags = c("train", "bounded")),
  p_dbl(-1, 1, tags = c("train", "bounded")),
  p_fct(letters[1:4], tags = c("train", "mode")),
  p_lgl(tags = "flag")
)
domains = rep(domain_prototypes, length.out = n)
# Reverse lexical order makes accidental key/table order visible in results.
names(domains) = sprintf("parameter_%05d", rev(seq_len(n)))
param_set = ParamSet$new(domains)

stopifnot(identical(param_set$ids(), names(domains)))

cat(
  "paradox", as.character(packageVersion("paradox")),
  "R", as.character(getRversion()),
  "parameters", n,
  "iterations", iterations,
  "\n"
)

results = bench::mark(
  all = param_set$ids(),
  class = param_set$ids(class = c("ParamInt", "ParamFct")),
  one_tag = param_set$ids(tags = "train"),
  all_tags = param_set$ids(tags = c("train", "bounded")),
  any_tags = param_set$ids(any_tags = c("train", "mode")),
  iterations = iterations,
  check = FALSE,
  filter_gc = FALSE
)

print(results[, c("expression", "min", "median", "itr/sec", "mem_alloc", "gc/sec")])
