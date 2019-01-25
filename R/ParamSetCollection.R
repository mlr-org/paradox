# FIXME: check get deps, and on deps

# FIXME: we need to be able to add deps across the sets, i fear. especially for the brnaching
# can we somehow provide some sugar fox this?

#FIXME: test that params sets on contru dont have travo

#' @title ParamSetCollection
#' @format [R6Class] object. Inherits from [ParamSet].
#'
#' @description
#' A collection of multiple [ParamSet] objects.
#' * In order to ensure unique param names, every param in the collection is named
#'   "<set_id>.<param_id>".
#' * The following state-changing methods are not allowed for a collection: `add`, `subset`.
#'
#' @section Public methods:
#' * `new(sets)` \cr
#'   list of [ParamSet] -> `self`
#'   Parameter objects are cloned.
#'
#' @name ParamSetCollection
NULL

#' @export
ParamSetCollection = R6Class("ParamSetCollection", inherit = ParamSet,
  public = list(
    initialize = function(sets) {
      assert_list(sets, types = "ParamSet")
      setids = map_chr(sets, "set_id")
      assert_names(setids, type = "unique")
      if (any(map_lgl(sets, "has_trafo")))  # we need to be able to have a trafo on the collection, not sure how to mix this with individual trafos yet.
        stop("Building a collection out sets, where a ParamSet has a trafo is currently unsupported!")
      private$.sets = sets
      private$build_params()
      private$build_deps()
    },

    add = function(p) stop("not allowed"),

    add_dep = function(p) stop("not allowed"),

    subset = function(ids) stop("not allowed")

  ),

  active = list(
    set_id = function(v) stopf("not allowed")
  ),

  private = list(
    .sets = NULL,
    # FIXME: can we somehow void copying all the params and changing them?
    # FIXME: we need to copy params AND deps and the same time and change refs
    build_params = function() {
      for (s in private$.sets) {
        n = s$length
        copy_map = data.table(addr = character(n), new_obj = vector("list", n))
        for (i in seq_len(n)) {
          p = s$params[[i]]
          pp = p$clone(deep = TRUE)
          pp$id = paste(s$set_id, p$id, sep = ".")
          copy_map[i, c("addr", "new_obj") := list(address(p), list(pp))]
        }
        # print(copy_map)
        # d = s$deps_on
        for (d in s$deps) {
          dd = d$clone(deep = TRUE)
          dd$param = copy_map[addr == address(dd$param), "new_obj"]
          dd$parent = copy_map[addr == address(dd$parent), "new_obj"]
          # print(dd)
        }
      }
      # private$.params = unlist(map(private$.sets, function(s)  {
        # ps = map(s$params, function(p) p$clone(deep = TRUE))
        # for (p in ps)
          # p$id = paste(s$set_id, p$id, sep = ".")
        # set_names(ps, map_chr(ps, "id"))
      # }), recursive = FALSE)
    },

    build_deps = function() {
      private$.deps = unlist(map(private$.sets, function(s)  {
        ps = map(s$deps, function(p) p$clone(deep = TRUE))
      }), recursive = FALSE)
      # for (d in deps) {
        # d$param
      # }
    }
  )
)
