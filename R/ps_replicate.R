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
#'   Should not be given if `affixes` is provided.
#' @param affixes (`character`)\cr
#'   A `character` vector indicating the prefixes / postfixes to use for each repetition of `set`.
#'   Per default, these are prefixes; if `postfix` is `TRUE`, these values are postfixed instead.
#'   If this is given, `times` is inferred from `length(affixes)` and should not be given separately.
#'   If `times` is given, this defaults to `"repX"`, with `X` counting up from 1.
#' @param postfix (`logical(1)`)\cr
#'   Whether to use `affixes` as a postfix instead of a prefix.
#'   Default `FALSE` (use prefixes).
#' @param tag_sets (`logical(1)`)\cr
#'   Whether to add a tag of the form `"set_<affixes[[i]]>"` to each parameter in the result, indicating the repetition each parameter belongs to.
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
#' ps_replicate(pset, affixes = c("first", "last"))
#'
#' ps_replicate(pset, affixes = c("first", "last"), postfix = TRUE)
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
ps_replicate = function(set, times = length(affixes), affixes = sprintf("rep%s", seq_len(times)), postfix = FALSE, tag_sets = FALSE, tag_params = FALSE) {
  assert_count(times)
  assert_character(affixes, any.missing = FALSE, unique = TRUE, len = times)

  ps_union(named_list(affixes, set, postfix_names = postfix), tag_sets = tag_sets, tag_params = tag_params)
}
