# # FIXME: can we get rid of this?



# ParamNumber = R6Class( "ParamNumber", inherit = Parameter,
#   public = list(

#     initialize = function(id, storage_type, special_vals = NULL, default = NULL, lower = -Inf, upper = Inf, tags = NULL, check) {
#       assert_true(lower <= upper)
#       super$initialize(
#         id = id,
#         storage_type = storage_type,
#         lower = lower,
#         upper = upper,
#         special_vals = special_vals,
#         default = default,
#         check = check,
#         tags = tags
#       )
#     }
#   ),

#   active = list(
#     range = function() c(self$lower, self$upper),
#     has_finite_bounds = function() all(is.finite(self$range))
#   )
# )

