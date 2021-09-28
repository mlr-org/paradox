
p_dbl(0, 1, init = 1)

p = ps(prob = p_dbl(0, 1, init = 1), param = p_int(0, logscale = TRUE))

p$values
p$values$prob = 0.5
p$values
p$values$param = 0
p$values$param = -100


