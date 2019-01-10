# FIXME: we need to be able to set a trafo in the complete collection, but maybe then no
# trafos should exist on the individuell sets?

# FIXME: check get deps, and on deps

# FIXME: can we cache the params in the collection? so we dont have to build them all on the fly?

# FIXME: we need to be able to add deps across the sets, i fear. especially for the brnaching

# FIXME: if we add deps, what format / names do we have to use to stay valied?

ParamSetCollection = R6Class("ParamSetCollection", inherit = ParamSet,
  public = list(
    initialize = function(sets) {
      assert_list(sets, types = "ParamSet")
      setids = map_chr(sets, "set_id")
      assert_names(setids, type = "unique")
      private$.sets = sets
      private$build_params()
      private$build_deps()
    },

    add = function(p) stop("not allowed"),

    add_dep = function(p) stop("not allowed")

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
