# special new data type for no-default. we really cant use anything else.
# NULL, NA, etc can all be valid defaults
NoDefault = R6Class("NoDefault",
  public = list(
    initialize = function() {}
  ),
)
NO_DEF = NoDefault$new()
is_nodefault = function(x) test_r6(x, "NoDefault")
is_proper_default = function(x) !is_nodefault(x)

