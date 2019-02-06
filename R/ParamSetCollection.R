#' @title ParamSetCollection
#' @format [R6Class] object. Inherits from [ParamSet].
#'
#' @description
#' A collection of multiple [ParamSet] objects.
#' * The collection is basically a light-weight wrapper / container around references to multiple sets.
#' * In order to ensure unique param names, every param in the collection is referred to with
#'   "<set_id>.<param_id>".
#' * The following state-changing methods are not allowed for a collection: `add`, `subset`.
#' * When you either ask for 'param_vals' or set them, the operation is delegated to the individual,
#'   contained param set references. The collection itself does not maintain a `param_vals` state.
#'   This also implies that if you directly change `param_vals` in one of the referenced sets,
#'   this change is reflected in the collection.
#'
#' @section Public methods:
#' * `new(sets)` \cr
#'   list of [ParamSet] -> `self` \cr
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
      private$build_internal()
      self$set_id = "Collection"
    },

    add = function(p) stop("not allowed"),

    add_dep = function(p) stop("not allowed"),

    subset = function(ids) stop("not allowed")

  ),

  active = list(
    param_vals = function(xs) {
      if (missing(xs)) {
        vals = lapply(private$.sets, function(s) {
          v = s$param_vals
          if (length(v) > 0L)
            names(v) = paste(s$set_id, names(v), sep = ".")
          return(v)
        })
        return(unlist(vals, recursive = FALSE))
      } else {
        assert_list(xs)
        self$assert(xs) # make sure everything is valid and feasible
        # extract everything before 1st dot
        set_ids = stri_split_fixed(names(xs), ".", n = 1L, tokens_only = TRUE)[[1L]]
        xs = split(xs, set_ids) # partition xs into parts wrt to setids
        for (s in private$.sets) {
          # retrieve sublist for each set, then assign it in set (after removing prefix)
          pv = xs[[s$set_id]]
          if (is.null(pv))
            pv = list()
          names(pv) = stri_replace_first_fixed(names(pv), paste0(s$set_id, "."), "")
          s$param_vals = pv
        }
      }
    }
  ),

  private = list(
    .sets = NULL,
    build_internal = function() {
      deps_list = list()
      for (s in private$.sets) { # iter thru sets
        n = s$length
        ids_old = s$ids()
        ids_new = paste(s$set_id, ids_old, sep = ".")
        for (i in seq_len(n)) { # clone each param into new params-list and prefix id
          p = s$params[[i]]$clone()
          p$id = ids_new[i]
          private$.params[[length(private$.params) + 1L]] = p
        }
        # copy all deps and rename ids to prefixed versions
        d = copy(s$deps)
        d$id = map_values(d$id, ids_old, ids_new)
        d$on = map_values(d$on, ids_old, ids_new)
        deps_list[[length(deps_list) + 1L]] = d
      }
      names(private$.params) = map_chr(private$.params, "id")
      private$.deps = rbindlist(deps_list)
    }
  )
)
