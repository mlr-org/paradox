# Objects Used for Testing
# use th_ to indicate a test helper object

# Parameter
th_param_int = ParamInt$new(id = 'th_param_int', default = 0, lower = -10, upper = 10)
th_param_nat = ParamInt$new(id = 'th_param_nat', default = 1L, lower = 1L, upper = 4L)
th_param_real = ParamFloat$new(id = 'th_param_real', default = 0, lower = -10, upper = 10)
th_param_real_na = ParamFloat$new(id = 'th_param_real_na', default = 0, lower = -10, upper = 10, special_vals = NA)
th_param_categ = ParamCateg$new(id = 'th_param_categ', default = 'a', values = letters[1:3])
th_param_bool = ParamBool$new(id = 'th_param_bool', default = FALSE)
th_param_untyped = ParamUntyped$new(id = 'th_param_untyped')
