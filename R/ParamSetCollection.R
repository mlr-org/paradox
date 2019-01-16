# FIXME: check get deps, and on deps

# FIXME: we need to be able to add deps across the sets, i fear. especially for the brnaching
# can we somehow provide some sugar fox this?

# FIXME: if we add deps, what format / names do we have to use to stay valied?

#FIXME: test that params sets on contru dont have travo

ParamSetCollection = R6Class("ParamSetCollection", inherit = ParamSet,
  public = list(
    initialize = function(sets) {
      assert_list(sets, types = "ParamSet")
      setids = map_chr(sets, "set_id")
      assert_names(setids, type = "unique")
      if (any(map_lgl(sets, "has_trafo")))  # we need to be a ble to have a trafo on the collection, not sure how to mix this with indivdual trafos yet.
        stop("Building a collection out sets, where a ParamSet has a trafo is currently unsupported!")
      private$.sets = sets
      private$build_params()
      private$build_deps()
    },

    add = function(p) stop("not allowed"),

    add_dep = function(p) stop("not allowed"),

    subset = function(ids) stop("not allowed")

  ),

  private = list(
    .sets = NULL,
    build_params = function() {
      private$.params = unlist(map(private$.sets, function(s)  {
        ps = map(s$params, function(p) p$clone(deep = TRUE))
        for (p in ps)
          p$id = paste(s$set_id, p$id, sep = ".")
        set_names(ps, map_chr(ps, "id"))
      }), recursive = FALSE)
    },

    build_deps = function() {
      private$.deps = unlist(map(private$.sets, function(s)  {
        ps = map(s$deps, function(p) p$clone(deep = TRUE))
      }), recursive = FALSE)
    }
  )
)
