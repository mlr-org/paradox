# OptPath

th_opt_path_full = OptPath$new(par_set = th_paramset_flat_full)

for (i in 1:10) {
  x = th_opt_path_full$par_set$sample(1)
  th_opt_path_full$add(
    x = x,
    y = i, 
    message = sprintf("message: %i", i),
    error = sprintf("error: %i", i),
    exec_time = i,
    extra = list(th_ex1 = i, th_ex2 = "th_ex2.string")
  )
}

# Multi-objective opt_path with transformations

th_opt_path_multiobjective = OptPath$new(par_set = th_paramset_flat_repeated, y_names = c('y1.min', 'y2.max'), minimize = c(TRUE, FALSE))
for (i in 1:10) {
  x = th_opt_path_multiobjective$par_set$sample(1)
  th_opt_path_multiobjective$add(x = x, y = c(y2.max = i, y1.min = 11-i))
}
