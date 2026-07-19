repr = function(x) {
  str_collapse(utils::capture.output(print(x)), "\n")
}

as_type = function(x, type) {
  switch(type,
    logical = as.logical(x),
    integer = as.integer(x),
    numeric = as.numeric(x),
    character = as.character(x),
    list = as.list(x),
    stopf("Invalid storage type '%s'", type)
  )
}
