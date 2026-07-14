library(bench)
library(paradox)

make_space <- function(n = 100L) {
  domains <- rep(list(p_dbl(-10, 10, tags = c("train", "bounded"))), n)
  names(domains) <- sprintf("x%04d", seq_len(n))
  ParamSet$new(domains)
}

space <- make_space()
values <- setNames(as.list(seq(-1, 1, length.out = space$length)), space$ids())
matrix_input <- matrix(
  runif(1000L * space$length),
  ncol = space$length,
  dimnames = list(NULL, space$ids())
)

results <- bench::mark(
  construct = make_space(),
  ids_all = space$ids(),
  ids_tag = space$ids(tags = "train"),
  check_scalar = space$check(values),
  check_batch = space$check_dt(as.data.frame(matrix_input)),
  qunif = space$qunif(matrix_input),
  iterations = 20L,
  check = FALSE,
  filter_gc = FALSE
)

print(results)
