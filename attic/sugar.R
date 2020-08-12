

p_dbl = function(id, lower = -Inf, upper = Inf,
  special_vals = list(), default = NO_DEF, tags = character()) {
  ParamDbl$new(id, lower, upper, special_vals, default, tags)
}

p_int = function(id, lower = -Inf, upper = Inf,
  special_vals = list(), default = NO_DEF, tags = character()) {
  ParamInt$new(id, lower, upper, special_vals, default, tags)
}

p_fct = function(id, levels,
  special_vals = list(), default = NO_DEF, tags = character()) {
  ParamFct$new(id, levels, special_vals, default, tags)
}

p_lgl = function(id,
  special_vals = list(), default = NO_DEF, tags = character()) {
  ParamLgl$new(id, special_vals, default, tags)
}

p_uty = function(id, default = NO_DEF, tags = character(), custom_check = NULL) {
  ParamUty$new(id, default, tags, custom_check)
}

pss = function(params = named_list()) {
  ParamSet$new(params)
}
