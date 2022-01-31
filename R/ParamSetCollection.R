#' @title ParamSetCollection
#'
#' @description
#' A collection of multiple [ParamSet] objects.
#' * The collection is basically a light-weight wrapper / container around references to multiple sets.
#' * In order to ensure unique param names, every param in the collection is referred to with
#'   "<set_id>.<param_id>". Parameters from ParamSets with empty (i.e. `""`) `$set_id` are referenced
#'   directly. Multiple ParamSets with `$set_id` `""` can be combined, but their parameter names
#'   must be unique.
#' * Operation `subset` is currently not allowed.
#' * Operation `add` currently only works when adding complete sets not single params.
#' * When you either ask for 'values' or set them, the operation is delegated to the individual,
#'   contained param set references. The collection itself does not maintain a `values` state.
#'   This also implies that if you directly change `values` in one of the referenced sets,
#'   this change is reflected in the collection.
#' * Dependencies: It is possible to currently handle dependencies
#'      * regarding parameters inside of the same set - in this case simply
#'        add the dependency to the set, best before adding the set to the collection
#'      * across sets, where a param from one set depends on the state
#'        of a param from another set - in this case add call `add_dep` on the collection.
#'
#'   If you call `deps` on the collection, you are returned a complete table of dependencies, from sets and across sets.
#'
#' @include ParamSet.R
#' @export
ParamSetCollection = R6Class("ParamSetCollection", inherit = ParamSet,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param sets (`list()` of [ParamSet])\cr
    #'   Parameter objects are cloned.
    initialize = function(sets) {

      assert_list(sets, types = "ParamSet")
      split_sets = split(sets, map_lgl(sets, function(x) x$set_id == ""))
      nameless_sets = split_sets$`TRUE`
      named_sets = split_sets$`FALSE`
      setids = map_chr(named_sets, "set_id")
      assert_names(setids, type = "unique")
      assert_names(unlist(map(nameless_sets, function(x) names(x$params_unid))) %??% character(0), type = "unique")
      if (any(map_lgl(sets, "has_trafo"))) {
        # we need to be able to have a trafo on the collection, not sure how to mix this with individual trafos yet.
        stop("Building a collection out sets, where a ParamSet has a trafo is currently unsupported!")
      }
      private$.sets = sets
      self$set_id = ""
      pnames = names(self$params_unid)
      dups = duplicated(pnames)
      if (any(dups)) {
        stopf("ParamSetCollection would contain duplicated parameter names: %s",
          str_collapse(unique(pnames[dups])))
      }
    },

    #' @description
    #' Adds a set to this collection.
    #'
    #' @param p ([ParamSet]).
    add = function(p) {
      assert_r6(p, "ParamSet")
      setnames = map_chr(private$.sets, "set_id")
      if (p$set_id != "" && p$set_id %in% setnames) {
        stopf("Setid '%s' already present in collection!", p$set_id)
      }
      if (p$has_trafo) {
        stop("Building a collection out sets, where a ParamSet has a trafo is currently unsupported!")
      }
      pnames = names(p$params_unid)
      nameclashes = intersect(
        ifelse(p$set_id != "", sprintf("%s.%s", p$set_id, pnames), pnames),
        names(self$params_unid)
      )
      if (length(nameclashes)) {
        stopf("Adding parameter set would lead to nameclashes: %s", str_collapse(nameclashes))
      }
      private$.sets = c(private$.sets, list(p))
      invisible(self)
    },

    #' @description
    #' Removes sets of given ids from collection.
    #'
    #' @param ids (`character()`).
    remove_sets = function(ids) {
      setnames = map_chr(private$.sets, "set_id")
      assert_subset(ids, setnames)
      private$.sets[setnames %in% ids] = NULL
      invisible(self)
    },

    #' @description
    #' Only included for consistency. Not allowed to perform on [ParamSetCollection]s.
    #'
    #' @param ids (`character()`).
    subset = function(ids) stop("not allowed")

  ),

  active = list(
    #' @template field_params
    params = function(v) {
      ps_all = self$params_unid
      imap(ps_all, function(x, n) {
        x = x$clone(deep = TRUE)
        x$id = n
        x
      })
    },
    #' @template field_params_unid
    params_unid = function(v) {
      sets = private$.sets
      names(sets) = map_chr(sets, "set_id")
      if (length(sets) == 0L) {
        return(named_list())
      }
      private$.params = named_list()
      # clone each param into new params-list and prefix id
      ps_all = lapply(sets, function(s) s$params_unid)
      ps_all = unlist(ps_all, recursive = FALSE)
      if (!length(ps_all)) ps_all = named_list()
      ps_all
    },
    #' @template field_deps
    deps = function(v) {
      d_all = lapply(private$.sets, function(s) {
        # copy all deps and rename ids to prefixed versions
        dd = copy(s$deps)
        ids_old = s$ids()
        if (s$set_id != "" && nrow(dd)) {
          ids_new = sprintf("%s.%s", s$set_id, ids_old)
          dd$id = map_values(dd$id, ids_old, ids_new)
          dd$on = map_values(dd$on, ids_old, ids_new)
        }
        dd
      })
      rbindlist(c(d_all, list(private$.deps)), use.names = TRUE)
    },

    #' @template field_values
    values = function(xs) {
      sets = private$.sets
      names(sets) = map_chr(sets, "set_id")
      if (!missing(xs)) {
        assert_list(xs)
        self$assert(xs) # make sure everything is valid and feasible

        for (s in sets) {
          # retrieve sublist for each set, then assign it in set (after removing prefix)
          psids = names(s$params_unid)
          if (s$set_id != "") {
            psids = sprintf("%s.%s", s$set_id, psids)
          }
          pv = xs[intersect(psids, names(xs))]
          if (s$set_id != "") {
            names(pv) = substr(names(pv), nchar(s$set_id) + 2, nchar(names(pv)))
          }
          s$values = pv
        }
      }
      vals = map(sets, "values")
      vals = unlist(vals, recursive = FALSE)
      if (!length(vals)) vals = named_list()
      vals
    }
  ),

  private = list(
    .sets = NULL
  )
)
