#' @title Design to List
#' @description
#' Converts a design or any `data.table` to a list where each list element contains one row.
#'
#' @param design [`data.table`]:
#'   The `data.table` that should be converted
#'
#' @return `list`
#' @export
design_to_list = function(design) {
  assert_data_table(design)
  .mapply(list, design, list())
}
