#' @title Design of configurations
#'
#' @description
#' A lightweight wrapper around a [ParamSet] and a [data.table], where the latter is a design
#' of configurations somehow produced from the former - e.g., by calling a
#' `generate_design` function or sampling.
#'
#' @section Public members / active bindings:
#' * `param_set`       :: [ParamSet]
#' * `data`            :: [data.table]
#'
#' @section Public methods:
#' * `new(param_set, data, remove_dupl)` \cr
#'   [ParamSet], [data.table], `logical(1)` -> `self`
#'   Note that the first 2 arguments are NOT cloned on construction!
#'   Has an option to remove duplicated rows.
#' * `transpose(filter_na = TRUE, trafo = FALSE)`
#'   `logical(1)`, `logical(1)` -> `list` of `list`
#'   Converts `data` into a list of lists of row-configurations, possibly removes NA entries of
#'   inactive parameter values due to unsatisfied dependencies,
#'   and possibly calls the `trafo` function of the param set.
#' @name Design
#' @export
NULL

Design = R6Class("Design",
  public = list(
    param_set = NULL,
    data = NULL,

    initialize = function(param_set, data, remove_dupl) {
      assert_paramset(param_set)
      assert_data_table(data, ncols = param_set$length)
      assert_names(colnames(data), permutation.of = param_set$ids())
      self$param_set = param_set
      self$data = data
      if (param_set$has_deps)
        private$set_deps_to_na()
      # FIXME: this can be problematic, as we now might generate designs which do not have
      # enough rows anymore, as the user requested? create an issue!
      # NB: duplicated rows can happen to to NA setting
      if (remove_dupl)
        self$data = unique(self$data) # remove duplicated rows
    },

    print = function(...) { # simply print the included dt
      catf("<Design> with %i rows:", nrow(self$data))
      print(self$data)
    },

    transpose = function(filter_na = TRUE, trafo = FALSE) {
      assert_flag(filter_na)
      assert_flag(trafo)
      xs = mlr3misc::transpose(self$data)
      if (filter_na)
        xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
      if (trafo) {
        ps = self$param_set
        if (!ps$has_trafo)
          stopf("Design was not generated from a param set with a trafo!")
        xs = map(xs, function(x) ps$trafo(x, ps))
      }
      return(xs)
    }
  ),

  private = list(
    # function to set unsatisfied deps to NA in the design dt "data":
    # walk thru all params, toposorted order, then walk thru all deps
    # and set values in x to NA which where the dep is not OK
    set_deps_to_na = function(remove_dupl) {
      ps = self$param_set
      deps_on = ps$deps_on
      graph = deps_on[,1:2]
      colnames(graph) = c("id", "parents")
      topo = topo_sort(graph)
      pids_sorted = topo$id
      for (param_id in pids_sorted) {
        deps = deps_on[param_id, on = "id"]$deps[[1L]]
        for (d in deps) {
          pcol = self$data[[d$parent$id]]
          not_ok = which(is.na(pcol) | !d$cond$test(pcol)) # we are ok if parent was active and cond on parent is OK
          set(self$data, not_ok, j = param_id, value = as(NA, d$param$storage_type))
        }
      }
    }
  )
)
