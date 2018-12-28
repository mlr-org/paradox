# Objects Used for Testing
# use th_ to indicate a test helper object

# Param
th_param_int = function() ParamInt$new(id = 'th_param_int', default = 0, lower = -10, upper = 10)
th_param_nat = function() ParamInt$new(id = 'th_param_nat', default = 1L, lower = 1L, upper = 4L)
th_param_dbl = function() ParamDbl$new(id = 'th_param_dbl', default = 0, lower = -10, upper = 10)
th_param_dbl_na = function() ParamDbl$new(id = 'th_param_dbl_na', default = 0, lower = -10, upper = 10, special_vals = list(NA))
th_param_fct = function() ParamFct$new(id = 'th_param_fct', default = 'a', values = letters[1:3])
th_param_lgl = function() ParamLgl$new(id = 'th_param_lgl', default = FALSE)
th_param_uty = function() ParamUty$new(id = 'th_param_uty')
