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
      names(sets) = NULL # we drop names here, otherwise a problem happens when we unlist in AB "params"
      private$.sets = sets
      self$set_id = "collection"
    },

    add = function(p) stop("not allowed"),

    subset = function(ids) stop("not allowed")

  ),

  active = list(
    params = function(v) {
      if (length(private$.sets) == 0L)
        return(named_list())
      private$.params = named_list()
      # clone each param into new params-list and prefix id
      ps_all = lapply(private$.sets, function(s) {
        ss = s$clone(deep = TRUE)
        ps = ss$params
        if (length(ps) > 0L) # paste with empty vec creates a string...
          names(ps) = paste(s$set_id, names(ps), sep = ".")
        return(ps)
      })
      ps_all = unlist(ps_all, recursive = FALSE)
      if (length(ps_all) == 0L)  # unlist before drops names for empty list....
        ps_all = named_list()
      imap(ps_all, function(x, n) x$id = n)
      return(ps_all)
    },

    deps = function(v) {
      d_all = lapply(private$.sets, function(s) {
        # copy all deps and rename ids to prefixed versions
        dd = copy(s$deps)
        ids_old = s$ids()
        ids_new = paste(s$set_id, ids_old, sep = ".")
        dd$id = map_values(dd$id, ids_old, ids_new)
        dd$on = map_values(dd$on, ids_old, ids_new)
        return(dd)
      })
      rbindlist(c(d_all, list(private$.deps)))
    },

    param_vals = function(xs) {
      if (missing(xs)) {
        vals = lapply(private$.sets, function(s) {
          v = s$param_vals
          if (length(v) > 0L)
            names(v) = paste(s$set_id, names(v), sep = ".")
          return(v)
        })
        vals = unlist(vals, recursive = FALSE)
        if (length(vals) == 0L) return(named_list()) # this is bullshit
        return(vals)
      } else {
        assert_list(xs)
        self$assert(xs) # make sure everything is valid and feasible
        # extract everything before 1st dot
        set_ids = sub("^([^.]+)\\..+", "\\1", names(xs))
        xs = split(xs, set_ids) # partition xs into parts wrt to setids
        for (s in private$.sets) {
          # retrieve sublist for each set, then assign it in set (after removing prefix)
          pv = xs[[s$set_id]]
          if (is.null(pv))
            pv = list()
          names(pv) = sub(sprintf("^%s.", s$set_id), "", names(pv))
          s$param_vals = pv
        }
      }
    }
  ),

  private = list(
    .sets = NULL
  )
)
