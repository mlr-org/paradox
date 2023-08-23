# Create a ParamSet from a list of ParamSets
# This emulates `ParamSetCollection$new(sets)`, except that
# - The result is a `ParamSet`, not a `ParamSetCollection`
# - The ParamSets are allowed to have `$trafo`, which are collected together into a single function.
# This emulates `ParamSetCollection$new(sets)`, which in particular means that the resulting ParamSet has all the Params
# from the input `sets`, but some `$id`s are changed: If the ParamSet has a non-empty `set_id`, then the Params will
# have their <id> changed to <set_id>.<id>. This is also reflected in deps and in `$trafo`.
# @param sets: list of ParamSet
ps_union = function(sets, tag_sets = FALSE, tag_params = FALSE) {
  assert_list(sets, types = "ParamSet")
  if (!length(sets)) return(ParamSet$new())
  ParamSetCollection$new(sets, tag_sets = tag_sets, tag_params = tag_params)$flatten()
}

#' @export
c.ParamSet = function(..., .tag_sets = FALSE, .tag_params = FALSE) {
  ps_union(list(...), tag_sets = .tag_sets, tag_params = .tag_params)
}
