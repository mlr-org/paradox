#' @export
createCollectionParam = function(n = 1L, param, special.vals = NA) {
  collection.id = paste0(param$id, ".collection")
  lapply(seq_len(n), function(i) {
    this.param = param$clone()
    this.param$id = paste0(collection.id, ".", i)
    this.param$tags = c(this.param$tags, collection.id)
    this.param
  })
}
