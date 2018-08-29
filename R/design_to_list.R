#' @export
design_to_list = function(design) {
  assert_data_table(design)
  .mapply(list, design, list())
}