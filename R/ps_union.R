#' @title Create a ParamSet from a list of ParamSets
#'
#' @description
#' This emulates `ParamSetCollection$new(sets)`, except that the result is a flat [`ParamSet`], not a [`ParamSetCollection`].
#' The resulting object is decoupled from the input [`ParamSet`] objects: Unlike [`ParamSetCollection`], changing `$values` of
#' the resulting object will not change the input [`ParamSet`] `$values` by reference.
#'
#' This emulates `ParamSetCollection$new(sets)`, which in particular means that the resulting [`ParamSet`] has all the [`Domain`]s
#' from the input `sets`, but some `$id`s are changed: If the [`ParamSet`] is given in `sets` with a name, then the [`Domain`]s will
#' have their `<id>` changed to `<name in "sets">.<id>`. This is also reflected in deps.
#'
#' The `c()` operator, applied to [`ParamSet`]s, is a synony for `ps_union()`.
#' @param sets (`list` of [`ParamSet`])\cr
#'   This may be a named list, in which case non-empty names are prefixed to parameters in the corresponding [`ParamSet`].
#' @param tag_sets (`logical(1)`)\cr
#'   Whether to add tags of the form `"set_<set_id>"` to each parameter originating from a given `ParamSet` given with name `<name in "sets">`.
#' @param tag_params (`logical(1)`)\cr
#'   Whether to add tags of the form `"param_<param_id>"` to each parameter with original ID `<param_id>`.
#' @examples
#' ps1 = ps(x = p_dbl())
#' ps1$values = list(x = 1)
#'
#' ps2 = ps(y = p_lgl())
#'
#' pu = ps_union(list(ps1, ps2))
#' # same as:
#' pu = c(ps1, ps2)
#'
#' pu
#'
#' pu$values
#'
#' pu$values$x = 2
#' pu$values
#'
#' # p1 is unchanged:
#' ps1$values
#'
#' # Prefixes automatically created for named elements.
#' # This allows repeating components.
#' pu2 = c(one = ps1, two = ps1, ps2)
#' pu2
#'
#' pu2$values
#'
#' @export
ps_union = function(sets, tag_sets = FALSE, tag_params = FALSE) {
  assert_list(sets, types = "ParamSet")
  if (!length(sets)) return(ParamSet$new())
  ParamSetCollection$new(sets, tag_sets = tag_sets, tag_params = tag_params)$flatten()
}

#' @export
c.ParamSet = function(..., .tag_sets = FALSE, .tag_params = FALSE) {
  ps_union(list(...), tag_sets = .tag_sets, tag_params = .tag_params)
}
