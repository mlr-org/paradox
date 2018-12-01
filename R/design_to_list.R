#' @title Convert Design to List
#'
#' @description
#' Convert design to a list where each list item represents one line of the design.
#'
#' @param design [\code{data.table}]\cr
#'   The object that contains the design.
#' @return list
#' @export
design_to_list = function(design) {
  assert_data_table(design)
  .mapply(list, design, list())
}
