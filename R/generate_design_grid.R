# resolution int(1) - resolution used for each parameter
# param_resolutions int() - resolution given per parameter (named vector)

#' @title Generate a grid design.
#'
#' @description
#' Generate a grid with specified resolution in the parameter space.
#' NB: The resolution for categorical parameters is ignored, these parameters
#' always produce a grid over all their valid levels.
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
#' @family generate_design
generate_design_grid = function(param_set, resolution = NULL, param_resolutions = NULL) {

  #FIXME: cannot be called on untyped params

  if (!xor(is.null(resolution), is.null(param_resolutions)))
    stop("You must specify resolution (x)or param_resolutions!")
  ids = param_set$ids
  if (!is.null(resolution)) {
    # create param_resolutions list, constant entry, same length as ids and named with ids
    resolution = assert_count(resolution, positive = TRUE, coerce = TRUE)
    param_resolutions = set_names(rep.int(resolution, param_set$length), ids)
  } else {
    param_resolutions = assert_integerish(param_resolutions, lower = 1L, any.missing = FALSE, coerce = TRUE)
    assert_names(names(param_resolutions), permutation.of = ids)
  }

  # overwrite the resolution for categorical stuff with the number of levels they have
  ids_cat = param_set$ids_cat
  if (length(ids_cat) > 0L)
    param_resolutions[ids_cat] = param_set$nlevels[ids_cat]

  # generate regular grid from 0,1 then map it to the values of the param,
  # then do a crossproduct
  grid_vec = lapply(param_resolutions, function(r) seq(0, 1, length.out = r))
  res = imap(grid_vec, function(value, id) param_set$get_param(id)$map_unitint_to_values(x = value))
  res = do.call(function(...) CJ(..., sorted = FALSE), res) #FIXME: Will throw error if res has sorted column.
  return(res)
}
