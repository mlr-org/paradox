# OptPath

th.opt.path.full = OptPath$new(par.set = th.paramset.flat.full)

for (i in 1:10) {
  x = th.opt.path.full$par.set$sample(1)
  th.opt.path.full$add(
    x = x,
    y = i, 
    message = sprintf("message: %i", i),
    error = sprintf("error: %i", i),
    exec.time = i,
    extra = list(th.ex1 = i, th.ex2 = "th.ex2.string")
  )
}

# Multi-objective opt.path with transformations

th.opt.path.multiobjective = OptPath$new(par.set = th.paramset.flat.collection, y.names = c('y1.min', 'y2.max'), minimize = c(TRUE, FALSE))
for (i in 1:10) {
  x = th.opt.path.multiobjective$par.set$sample(1)
  th.opt.path.multiobjective$add(x = x, y = c(y2.max = i, y1.min = 11-i))
}
