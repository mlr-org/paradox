#' @title Create a ParamSet by Repeating a Given ParamSet
#'
#' @description
#' Repeat a [`ParamSet`] a given number of times and thus create a larger [`ParamSet`].
#' By default, the resulting parameters are prefixed with the string `"repX.", where `X` counts up from 1.
#' It is also possible to tag parameters by their original name and by their prefix, making grouped retrieval e.g. using `$get_values()` easier.
#'
#' @param set ([`ParamSet`])\cr
#'   [`ParamSet`] to use as template.
#' @param times (`integer(1)`)\cr
#'   Number of times to repeat `set`.
#'   Should not be given if `prefixes` is provided.
#' @param prefixes (`character`)\cr
#'   A `character` vector indicating the prefixes to use for each repetition of `set`.
#'   If this is given, `times` is inferred from `length(prefixes)` and should not be given separately.
#'   If `times` is given, this defaults to `"repX"`, with `X` counting up from 1.
#' @param tag_sets (`logical(1)`)\cr
#'   Whether to add a tag of the form `"set_<prefixes[[i]]>"` to each parameter in the result, indicating the repetition each parameter belongs to.
#' @param tag_params (`logical(1)`)\cr
#'   Whether to add a tag of the form `"param_<id>"` to each parameter in the result, indicating the original parameter ID inside `set`.
#' @examples
#' pset = ps(
#'   i = p_int(),
#'   z = p_lgl()
#' )
#'
#' ps_replicate(pset, 3)
#'
#' ps_replicate(pset, prefixes = c("first", "last"))
#'
#' pset$values = list(i = 1, z = FALSE)
#'
#' psr = ps_replicate(pset, 2, tag_sets = TRUE, tag_params = TRUE)
#'
#' # observe the effect of tag_sets, tag_params:
#' psr$tags
#'
#' # note that values are repeated as well
#' psr$values
#'
#' psr$set_values(rep1.i = 10, rep2.z = TRUE)
#' psr$values
#'
#' # use `any_tags` to get subset of values.
#' # `any_tags = ` is preferable to `tags = `, since parameters
#' # could also have other tags. `tags = ` would require the
#' # selected params to have the given tags exclusively.
#'
#' # get all values associated with the original parameter `i`
#' psr$get_values(any_tags = "param_i")
#'
#' # get all values associated with the first repetition "rep1"
#' psr$get_values(any_tags = "set_rep1")
#' @export
ps_replicate = function(set, times = length(prefixes), prefixes = sprintf("rep%s", seq_len(times)), tag_sets = FALSE, tag_params = FALSE) {
  assert_count(times)
  assert_character(prefixes, any.missing = FALSE, unique = TRUE, len = times)

  ps_union(named_list(prefixes, set), tag_sets = tag_sets, tag_params = tag_params)
}
