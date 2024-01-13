#' @title
#' @export
ps_replicate = function(set, times = length(prefixes), prefixes = sprintf("rep%s", seq_len(times)), tag_sets = FALSE, tag_params = FALSE) {
  assert_count(times)
  assert_character(prefixes, any.missing = FALSE, unique = TRUE, len = times)

  ps_union(named_list(prefixes, set), tag_sets = tag_sets, tag_params = tag_params)
}
