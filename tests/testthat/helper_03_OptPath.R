# OptPath

th_opt_path_full = function() {

  op = OptPath$new(param_set = th_paramset_full())

  for (i in 1:10) {
    s = SamplerUnif$new(op$param_set)
    x = s$sample(1)
    xx = list(
      x = x,
      y = i,
      message = sprintf("message: %i", i),
      exec_time = i,
      extra = list(th_ex1 = i, th_ex2 = "th_ex2.string")
      )
  # first half of optpath is without error, send 5 elements have error
    if (i >= 6L)
      xx$error = sprintf("error: %i", i)
    do.call(op$add, xx)
  }

  return(op)
}

# Multi-objective opt_path with transformations

th_opt_path_multiobjective = function() {
  op = OptPath$new(param_set = th_paramset_repeated(), y_names = c('y1.min', 'y2.max'), minimize = c(TRUE, FALSE))
for (i in 1:10) {
  s = SamplerUnif$new(op$param_set)
  x = s$sample(1)
  op$add(x = x, y = c(y2.max = i, y1.min = 11-i))
}

return(op)

}
