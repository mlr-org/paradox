# resolution int(1) - resolution used for each parameter
# param_resolutions int() - resolution given per parameter (named vector)
# n int(1) - approx. maximum number of samples in grid

#' @export
generate_design_grid = function(param_set, n = NULL, resolution = NULL, param_resolutions = NULL) {
  if (sum(!is.null(resolution), !is.null(param_resolutions), !is.null(n)) != 1) {
    stop("You can only specify one of the arguments!")
  }

  seq_gen = function(r) seq(0, 1, length.out = r)

  if (!is.null(resolution)) {
    # build for resolution
    assert_int(resolution, lower = 1L)
    grid_vec = replicate(param_set$length, seq_gen(resolution), simplify = FALSE)
    names(grid_vec) = param_set$ids
    res = as.list(param_set$denorm(grid_vec))
  } else {
    # build for n: calculate param_resolutions
    if (!is.null(n)) {
      assert_int(n, lower = 1L)
      param_resolutions = opt_grid_res(n, param_set$nlevels)
    }
    # build for param_resolutions
    assert_integerish(param_resolutions, lower = 1L, any.missing = FALSE, names = "strict")
    assert_names(names(param_resolutions), permutation.of = param_set$ids)
    grid_vec = lapply(param_resolutions, seq_gen)
    res = lapply(names(grid_vec), function(z) param_set$params[[z]]$denorm_vector(x = grid_vec[[z]]))
    names(res) = names(grid_vec)
  }
  res = lapply(res, unique)
  res = do.call(CJ, as.list(res))
  if (!is.null(param_set$restriction)) {
    ind_valid = vectorized_for_param_set_flat(res, param_set$test)
    return(res[ind_valid, ])
  } else {
    return(res)
  }
}
