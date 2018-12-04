# resolution int(1) - resolution used for each parameter
# param_resolutions int() - resolution given per parameter (named vector)

#' @title Generate a space-filling LHS design
#' @export
generate_design_grid = function(param_set, resolution = NULL, param_resolutions = NULL) {
  if (!is.null(resolution) && !is.null(param_resolutions))
    stop("You can only specify one of the arguments!")

  seq_gen = function(r) seq(0, 1, length.out = r)

  if (!is.null(resolution)) { # build param_resolutions as repeated resolution
    resolution = asInt(resolution, lower = 1L)
    param_resolutions = map_int(param_set$params, function(p) resolution)
  }
  assert_integerish(param_resolutions, lower = 1L, any.missing = FALSE, names = "strict")
  assert_names(names(param_resolutions), permutation.of = param_set$ids)
  grid_vec = lapply(param_resolutions, seq_gen)
  res = lapply(names(grid_vec), function(z) param_set$params[[z]]$denorm_vector(x = grid_vec[[z]]))
  names(res) = names(grid_vec)
  res = lapply(res, unique)
  res = do.call(CJ, as.list(res))
  if (!is.null(param_set$restriction)) {
    ind_valid = vectorized_for_param_set_flat(res, param_set$test)
    return(res[ind_valid, ])
  } else {
    return(res)
  }
}
