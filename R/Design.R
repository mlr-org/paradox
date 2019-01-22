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
#' * `new(param_set, data)` \cr
#'   [ParamSet], [data.table] -> `self`
#'   Note that both arguments are NOT cloned on construction!
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

    initialize = function(param_set, data) {
      assert_paramset(param_set)
      assert_data_table(data, ncols = param_set$length)
      assert_names(colnames(data), permutation.of = param_set$ids())
      self$param_set = param_set
      self$data = data
    },

    print = function(...) { # simply print the included dt
      # FIXME: maybe show that this is not JUST a dt.
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
  )
)
