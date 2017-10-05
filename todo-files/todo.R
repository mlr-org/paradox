library(devtools)
load_all()

# notes
# check functions as public method? Has to be to make Lower Upper Flexible
# 

par.set = ParamSetFlat$new(
  id = "ps.svm",
  params = list(
    ParamFactor$new(id = "type", default = "C-classification", values = c("C-classification", "nu-classification"), tags = "tunable"),
    ParamReal$new(id = "cost", id = "cost",  default = 1, lower = 0, tags = "tunable"),
    ParamReal$new(id = "nu", default = 0.5, tags = "tunable"),
    # ParamGenerator(id = "class.weights", ParamReal$new(lower = 0))
    ParamFactor$new(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid"), tags = "tunable"),
    ParamInt$new(id = "degree", default = 3L, lower = 1L, tags = "tunable"),
    ParamReal$new(id = "coef0", default = 0, tags = "tunable"),
    ParamReal$new(id = "gamma", lower = 0, tags = "tunable"),
    ParamReal$new(id = "cachesize", default = 40L, tags = "tunable"),
    ParamReal$new(id = "tolerance", default = 0.001, lower = 0, tags = "tunable"),
    ParamLogical$new(id = "shrinking", default = TRUE, tags = "tunable"),
    ParamInt$new(id = "cross", default = 0L, lower = 0L)
    ParamLogical$new(id = "fitted", default = TRUE)
    # ParamGenerator(id = "scale", default = TRUE)
    ),
  restriction = (
    (is.na(cost) & type != "C-classification") & 
    (is.na(nu) & type != "nu-classification") & 
    (is.na(degree) & kernel != "polynomial") & 
    (is.na(coef0) & !(kernel == "polynomial" || kernel == "sigmoid")) &
    (is.na(gamma) & kernel == "linear")
  )
)