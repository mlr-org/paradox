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

