# allows user to set up a collection of sets.
# FIXME: can we somehow simply "delegate" to the sets below?
# without explicitly storing a combined set? mayve we can write an AB "params" and "deps"??
# FIXME: we should probably not allow the user to add individual params, only sets?
# FIXME: should the set allow the setting of params? i guess in tuningwe need this?
# FIXME: i guess we dont allow trafos, we dont allow to set them? but must execute all of them?
# FIXME: disallow:
# - add_dep
#  - add(param), maybe allow add(set)
# - trafo = foo



# ParamSetCollection = R6Class("ParamSetCollection", inherit = ParamSet,
#   public = list(
#     initialize = function(sets) {
#       assert_list(sets, types = "ParamSet")
#       setids = map_chr(sets, "set_id")
#       assert_names(setids, type = "unique")
#       private$.sets = sets
#       combined = ParamSet$new()
#       for (i in 1:length(sets)) {
#         s = sets[[i]]
#         for (j in seq_along(s$params)) {
#           p = s$params[[j]]
#           pid = paste(s$set_id, p$id, sep = ".") #FIXME: which sep? use ":" ?
#           p$id = pid; names(s$params)[j] = pid
#         }
#         print(s)
#         combined$add(s)
#       }
#       self$params = combined$params
#       self$deps = combined$deps
#     }
#     add_dep()

#   ),
#   private = list(
#     .sets = NULL
#   )

# )
