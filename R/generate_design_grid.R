#' @title Generate a grid design.
#'
#' @description
#' Generate a grid with specified resolution in the parameter space.
#' NB1: The resolution for categorical parameters is ignored, these parameters
#' always produce a grid over all their valid levels.
#' NB2: For number params the endpoints of the params are always included in the grid.
#'
#' @param param_set :: [ParamSet].
#' @param resolution :: `integer(1)` \cr
#'   Global resolution for all params.
#' @param param_resolutions :: named `integer` \cr
#'   Resolution per param, named by param ID.
#' @return [data.table]
#'
#' @family generate_design
#' @export
generate_design_grid = function(param_set, resolution = NULL, param_resolutions = NULL) {
  assert_paramset(param_set, no_untyped = TRUE)

  if (!xor(is.null(resolution), is.null(param_resolutions)))
    stop("You must specify resolution (x)or param_resolutions!")
  ids = param_set$ids()
  if (!is.null(resolution)) {
    # create param_resolutions list, constant entry, same length as ids and named with ids
    resolution = assert_count(resolution, positive = TRUE, coerce = TRUE)
    param_resolutions = set_names(rep.int(resolution, param_set$length), ids)
  } else {
    param_resolutions = assert_integerish(param_resolutions, lower = 1L, any.missing = FALSE, coerce = TRUE)
    assert_names(names(param_resolutions), permutation.of = ids)
  }

  # overwrite the resolution for categorical stuff with the number of levels they have
  isc = param_set$is_categ
  param_resolutions[isc] = param_set$nlevels[isc]
  # generate regular grid from 0,1 then map it to the values of the param,
  # then do a crossproduct
  grid_vec = lapply(param_resolutions, function(r) seq(0, 1, length.out = r))
  res = imap(grid_vec, function(value, id) param_set$params[[id]]$qunif(x = value))
  # the un / renaming sucks a bit here, caused by dotdotdot-interface of CJ. would like to have a better way, but dont know
  # FIXME: mini helper in mlr3misc for this?
  ns = names(res); res = unname(res)
  res = do.call(CJ, c(res, sorted = FALSE))
  Design$new(param_set, set_names(res, ids), remove_dupl = TRUE)
}
