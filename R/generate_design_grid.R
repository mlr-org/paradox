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

  # The native operation admits the closed Domain schema and normalizes both
  # resolution controls against that same graph generation.  In particular,
  # do not derive IDs, numeric kinds, or factor level counts through separate
  # active-binding reads here: allocation while preparing a later control can
  # run a finalizer that swaps `.core`.
  assert_r6(param_set, "ParamSet")
  if (!is.null(resolution)) {
    resolution = unname(assert_count(resolution, coerce = TRUE))
  }
  if (!is.null(param_resolutions)) {
    param_resolutions = assert_integerish(
      param_resolutions,
      lower = 0L,
      any.missing = FALSE,
      coerce = TRUE
    )
    assert_names(names(param_resolutions), type = "unique")
  }
  if (!is.null(upper_limit)) {
    # Like the other public scalar controls, a name introduced by ordinary
    # indexing is representation-only. The native boundary receives the
    # canonical unclassed count.
    upper_limit = unname(assert_count(upper_limit, coerce = TRUE))
  }

  # Closed built-in schemas, including the zero-dimensional schema, use one
  # output-sensitive allocation-and-fill kernel and never branch to a second R
  # implementation. It returns the final fixed, dependency-masked, deduplicated
  # table, so the Design constructor must not normalize it a second time.
  generated = .Call(
    C_generate_design_grid_builtin,
    get_private(param_set),
    param_set,
    list(
      resolution = resolution,
      param_resolutions = param_resolutions
    ),
    upper_limit
  )
  design = Design$new(
    param_set,
    generated[[1L]],
    remove_dupl = .design_prepared_grid
  )
  # Design construction allocates its R6 shell after the native grid has been
  # completed. The compact receipt returned with that exact grid generation is
  # therefore scanned once more as the terminal operation; a finalizer which
  # moves the caller's live ParamSet during the handoff wins instead of
  # yielding a Design whose data and source never coexisted.
  .Call(C_param_set_generation_receipt, generated[[2L]])
  design
}
