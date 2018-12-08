# resolution int(1) - resolution used for each parameter
# param_resolutions int() - resolution given per parameter (named vector)

#' @title Generate a grid design.
#'
#' @description
#' Generate a grid with specified resolution in the parameter space.
#'
#' @param param_set [\code{\link{ParamSet}}].
#' @param resolution [\code{integer(1)}]:\cr
#'   Resolution to be used for each parameter in the \code{param_set}.
#' @param param_resolutions [named \code{integer()}]:\cr
#'   Resolution given per parameter, as named integer vector.
#'
#' @return [\code{\link[data.table]{data.table}}].
#'
#' @export
generate_design_grid = function(param_set, resolution = NULL, param_resolutions = NULL) {
  if (!xor(is.null(resolution), is.null(param_resolutions)))
    stop("You must specify resolution (x)or param_resolutions!")

  if (!is.null(resolution)) {
    resolution = assert_count(resolution, positive = TRUE, coerce = TRUE)
    param_resolutions = set_names(rep.int(resolution, param_set$length), param_set$ids)
  } else { # !is.null(param_resolutions)
    param_resolutions = assert_integerish(param_resolutions, lower = 1L, any.missing = FALSE, coerce = TRUE)
    assert_names(names(param_resolutions), "strict", permutation.of = param_set$ids)
  }

  grid_vec = lapply(param_resolutions, function(r) seq(0, 1, length.out = r))
  res = imap(grid_vec, function(value, id) unique(param_set$params[[id]]$denorm_vector(x = value)))
  res = do.call(CJ, res)

  return(res)
}
