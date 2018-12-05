load_all()

p1 = ParamFct$new("p1", values = c("a", "b"))
p2 = ParamDbl$new("p2")
p3 = ParamDbl$new("p3")

ps = ParamSet$new(list(p1, p2, p3))

print(ps)

# ps$add_dependency(Dependency$new(child = p2, parent = p1))

    # # check function that checks whether a (named) list of values is a feasible configuration
    # check = function(x, na.ok = FALSE, null.ok = FALSE) {
    #   assert_list(x)
    #   assert_set_equal(names(x), self$ids)
