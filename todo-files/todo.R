library(devtools)
load_all()
pint = ParamInt$new(id = "test", lower = 1L, upper = 10L)
pint$sample()



