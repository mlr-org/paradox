# `names<-` has to duplicate a referenced argument, and R returns a wrapper
# ALTREP for a list of 64 or more elements. ParamSetCollection$new() applied
# that to an unnamed `sets` argument and then handed the wrapper to the native
# boundary, which admits only ordinary containers. `$search_space()` builds one
# part per TuneToken and unions them as an unnamed list, so a set with 64 tuned
# parameters could not produce a search space at all.

test_that("a search space can be built from 64 or more tune tokens", {
  for (count in c(63L, 64L, 200L)) {
    ids = paste0("p", seq_len(count))
    set = do.call(ps, setNames(replicate(count, p_dbl(0, 1), simplify = FALSE), ids))
    set$values = setNames(replicate(count, to_tune(), simplify = FALSE), ids)
    expect_equal(set$search_space()$ids(), ids)

    set$values = setNames(replicate(count, to_tune(0.1, 0.9), simplify = FALSE), ids)
    expect_equal(set$search_space()$ids(), ids)
  }
})

test_that("an unnamed sets list of any length constructs a collection", {
  for (count in c(63L, 64L, 200L)) {
    sets = lapply(seq_len(count), function(index) {
      do.call(ps, setNames(list(p_dbl(0, 1)), paste0("u", index)))
    })
    ids = paste0("u", seq_len(count))
    expect_equal(ParamSetCollection$new(sets)$ids(), ids)
    expect_equal(ps_union(sets)$ids(), ids)
    expect_equal(do.call(c, sets)$ids(), ids)
    expect_equal(
      length(ps_replicate(ps(x = p_dbl(0, 1)), times = count)$ids()),
      count
    )
  }
})
