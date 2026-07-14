library(bench)
library(paradox)

arguments = commandArgs(trailingOnly = TRUE)
n = if (length(arguments)) as.integer(arguments[[1L]]) else 500L
iterations = if (length(arguments) > 1L) as.integer(arguments[[2L]]) else 500L
stopifnot(length(n) == 1L, !is.na(n), n >= 1L)
stopifnot(length(iterations) == 1L, !is.na(iterations), iterations >= 1L)

make_domain = function(index) {
  switch(
    as.character((index - 1L) %% 5L),
    "0" = p_dbl(
      -10,
      10,
      tags = c("train", "bounded"),
      trafo = function(x) exp(x / 10)
    ),
    "1" = p_int(-20L, 20L, tags = c("train", "bounded")),
    "2" = p_fct(letters[1:8], tags = c("train", "categorical")),
    "3" = p_lgl(tags = c("flag", "categorical"), init = FALSE),
    "4" = p_uty(custom_check = function(x) TRUE, tags = "payload")
  )
}

domains = lapply(seq_len(n), make_domain)
# Reverse lexical order makes key construction observable and non-trivial.
names(domains) = sprintf("parameter_%06d", rev(seq_len(n)))
fallback_domains = lapply(domains, function(domain) {
  domain = data.table::copy(domain)
  class(domain) = c("ParamSetConstructionFallback", class(domain))
  domain
})
names(fallback_domains) = names(domains)

native_result = ParamSet$new(domains)
fallback_result = ParamSet$new(fallback_domains)
stopifnot(
  identical(native_result$ids(), names(domains)),
  identical(native_result$ids(), fallback_result$ids()),
  identical(native_result$tags, fallback_result$tags),
  identical(native_result$values, fallback_result$values),
  identical(
    as.list(native_result$.__enclos_env__$private$.params),
    as.list(fallback_result$.__enclos_env__$private$.params)
  )
)

cat(
  "paradox", as.character(packageVersion("paradox")),
  "R", as.character(getRversion()),
  "parameters", n,
  "iterations", iterations,
  "\n"
)

results = bench::mark(
  native_builtin = ParamSet$new(domains),
  retained_fallback = ParamSet$new(fallback_domains),
  iterations = iterations,
  check = FALSE,
  filter_gc = FALSE
)

print(results[, c("expression", "min", "median", "itr/sec", "mem_alloc", "gc/sec")])
