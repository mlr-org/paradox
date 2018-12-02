# OptPath

th_opt_path_full = OptPath$new(param_set = th_paramset_full)

for (i in 1:10) {
  s = SamplerUnif$new(th_opt_path_full$param_set)
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
  do.call(th_opt_path_full$add, xx)
}

# Multi-objective opt_path with transformations

th_opt_path_multiobjective = OptPath$new(param_set = th_paramset_repeated, y_names = c('y1.min', 'y2.max'), minimize = c(TRUE, FALSE))
for (i in 1:10) {
  s = SamplerUnif$new(th_opt_path_multiobjective$param_set)
  x = s$sample(1)
  th_opt_path_multiobjective$add(x = x, y = c(y2.max = i, y1.min = 11-i))
}
