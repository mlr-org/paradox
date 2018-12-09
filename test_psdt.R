load_all()
# test(filter = "design")

# ps = ParamSet$new()
# print(ps)
# print(ps$ids)
p1 = ParamFct$new("p1", values = c("a", "b"))
print(p1)
print(p1$nlevels)
print(p1$denorm(0.1))
p2 = ParamDbl$new("p2", lower = 1, special_vals = list(NULL))
print(p2)

# print(p2$has_finite_bounds)
# print(p2$check(4))
# print(p2$special_vals)
# print(p2$check(NULL))

ps = ParamSet$new(list(p1, p2))

# print(ps)
# print(ps$data)
# print(ps$ids)
# print(ps$lowers)
# print(ps$pclasses)


# ps$add_dependency(Dependency$new(child = p2, parent = p1))

