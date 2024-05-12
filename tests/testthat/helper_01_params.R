# Objects Used for Testing
# use th_ to indicate a test helper object

# Param
th_param_int = function() ps(th_param_int = p_int(default = 0, lower = -10, upper = 10))
th_param_nat = function() ps(th_param_nat = p_int(default = 1L, lower = 1L, upper = 4L))
th_param_dbl = function() ps(th_param_dbl = p_dbl(default = 0, lower = -10, upper = 10))
th_param_dbl_na = function() ps(th_param_dbl_na = p_dbl(default = 0, lower = -10, upper = 10, special_vals = list(NA)))
th_param_fct = function() ps(th_param_fct = p_fct(default = "a", levels = letters[1:3]))
th_param_lgl = function() ps(th_param_lgl = p_lgl(default = FALSE))
th_param_uty = function() ps(th_param_uty = p_uty())


