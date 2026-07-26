#' @title Generate a Grid Design
#'
#' @description
#' Generate a grid with a specified resolution in the parameter space.
#' The resolution for categorical parameters is ignored, these parameters
#' always produce a grid over all their valid levels.
#' For number params the endpoints of the params are always included in the grid.
#'
#' Grid generation works on realized parameter values. Values made identical by
#' integer rounding or another built-in mapping are collapsed before the
#' Cartesian product is expanded. Stored parameter values form fixed axes, and
#' dependency-inactive branches are not expanded. The row order is the stable
#' order obtained by generating the nominal Cartesian product, applying fixed
#' values and dependencies, and retaining the first occurrence of each row.
#' A parameter with a nominal resolution of zero makes the complete design
#' empty, including when that parameter currently has a stored fixed value.
#' A valid fixed special value whose storage differs from its parameter,
#' including `NULL` or an S4 special, is preserved as an identity-preserving
#' list-column value.
#' A stored [`TuneToken`] cannot represent a concrete grid value and raises an
#' error.
#'
#' @param param_set ([`ParamSet`]).
#' @param resolution (`integer(1)`)\cr
#'   Global resolution for all parameters.
#' @param param_resolutions (named `integer()`)\cr
#'   Resolution per [`Domain`], named by parameter ID.
#' @param upper_limit (`integer(1)` | `NULL`)\cr
#'   Optional upper bound on the number of rows in the realized design, after
#'   fixed values, dependencies, and duplicated realized values have been
#'   accounted for. An error is raised instead of returning a larger design.
#' @return [`Design`].
#'
#' @family generate_design
#' @export
#' @examples
#' pset = ps(
#'   ratio = p_dbl(lower = 0, upper = 1),
#'   letters = p_fct(levels = letters[1:3])
#' )
#' generate_design_grid(pset, 10)
generate_design_grid = function(param_set, resolution = NULL, param_resolutions = NULL,
    upper_limit = NULL) {

  assert_param_set(param_set, no_untyped = TRUE)
  if (!is.null(upper_limit)) {
    # Like the other public scalar controls, a name introduced by ordinary
    # indexing is representation-only. The native boundary receives the
    # canonical unclassed count.
    upper_limit = unname(assert_count(upper_limit, coerce = TRUE))
  }
  ids = param_set$ids()
  ids_num = ids[param_set$is_number]

  par_res = set_names(integer(0L), character(0L)) # here we construct the resolution for each param
  if (length(ids_num) > 0L) { # Categorical-only spaces need no numeric-resolution check.
    if (is.null(resolution) && is.null(param_resolutions)) {
      stop("You must specify 'resolution' or 'param_resolutions'!")
    }
    if (!is.null(resolution)) {
      # create param_resolutions list, constant entry, same length as ids and named with ids
      resolution = assert_count(resolution, coerce = TRUE)
      par_res = set_names(rep.int(resolution, param_set$length), ids)
    }
    if (!is.null(param_resolutions)) {
      assert_integerish(param_resolutions, lower = 1L, any.missing = FALSE, coerce = TRUE)
      # user only needs to pass num params (categ resolutions are overwritten anyway)
      assert_names(names(param_resolutions), subset.of = ids_num)
      par_res = insert_named(par_res, param_resolutions)
    }
    ids_miss = setdiff(ids_num, names(par_res))
    if (length(ids_miss) > 0L) {
      stopf("Resolution settings missing for some numerical params: %s", str_collapse(ids_miss))
    }
  }
  # overwrite the resolution for categorical stuff with the number of levels they have
  isc = param_set$is_categ
  par_res = insert_named(par_res, param_set$nlevels[isc])

  # Closed built-in schemas, including the zero-dimensional schema, use one
  # output-sensitive allocation-and-fill kernel and never branch to a second R
  # implementation. It returns the final fixed, dependency-masked, deduplicated
  # table, so the Design constructor must not normalize it a second time.
  res = .Call(
    C_generate_design_grid_builtin,
    get_private(param_set),
    param_set,
    par_res,
    upper_limit
  )
  Design$new(param_set, res, remove_dupl = .design_prepared_grid)
}
