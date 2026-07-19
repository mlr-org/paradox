#' @title Design of Configurations
#'
#' @description
#' A lightweight wrapper around a [ParamSet] and a [data.table::data.table()], where the
#' latter is a design of configurations produced from the former - e.g.,
#' by calling a [generate_design_grid()] or by sampling.
#' The public table has ordinary non-ALTREP/non-S4 structural
#' names/row/dim/dimnames/list metadata. Native Design operations materialize
#' an exact-class, allowed-attribute top-level VECSXP ALTREP shell once; base R's
#' lazy attribute-only duplicate is the common case. Admitted semantic atomic
#' columns may be stable ALTREP and are likewise materialized once.
#'
#' @export
Design = R6Class("Design",
  public = list(
    #' @field param_set ([ParamSet]).
    param_set = NULL,

    #' @field data ([data.table::data.table()])\cr
    #' Stored data.table shell. An exact-class, allowed-attribute top-level
    #' VECSXP ALTREP is accepted once. Semantic atomic columns may be stable
    #' ALTREP; structural metadata may not.
    data = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param param_set ([ParamSet]).
    #' @param data ([data.table::data.table()])\cr
    #'   Stored `data`. Structural metadata must be ordinary non-ALTREP/non-S4;
    #'   an exact-class, allowed-attribute top-level VECSXP ALTREP is accepted
    #'   once, and admitted semantic atomic columns may be stable ALTREP.
    #' @param remove_dupl (`logical(1)`)\cr
    #'   Remove duplicates?
    initialize = function(param_set, data, remove_dupl) {

      assert_param_set(param_set)
      assert_data_table(data, ncols = param_set$length)
      assert_names(colnames(data), permutation.of = param_set$ids())
      self$param_set = param_set
      # Apply fixed values at this one generator-independent boundary. Random,
      # grid, Sobol, and LHS designs therefore share identical overwrite and
      # dependency behavior even when a generator produced placeholder data.
      imap(param_set$values, function(v, n) set(data, j = n, value = v))
      self$data = data
      private$set_deps_to_na()
      # NB: duplicated rows can happen to to NA setting
      if (remove_dupl) {
        self$data = unique(self$data)
      } # remove duplicated rows
    },


    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function(...) {
      # simply print the included dt
      catf("<Design> with %i rows:", nrow(self$data))
      print(self$data)
    },

    #' @description
    #' Converts `data` into a list of lists of row-configurations,
    #' possibly removes `NA` entries of inactive parameter values due to unsatisfied dependencies,
    #' and possibly calls the `trafo` function of the [ParamSet].
    #' The stored table must continue to satisfy the documented public-table
    #' boundary: an exact-class, allowed-attribute top-level VECSXP ALTREP is
    #' materialized once, structural metadata remains ordinary, and admitted
    #' stable ALTREP atomic columns are materialized once.
    #'
    #' @param filter_na (`logical(1)`)\cr
    #'   Should `NA` entries of inactive parameter values due to unsatisfied
    #'   dependencies be removed?
    #' @param trafo (`logical(1)`)\cr
    #'   Should the `trafo` function of the [ParamSet] be called?
    transpose = function(filter_na = TRUE, trafo = TRUE) {
      assert_flag(filter_na)
      assert_flag(trafo)
      ps = self$param_set
      xs = .Call(C_design_transpose, self$data, filter_na)
      if (trafo) xs = .Call(C_design_transpose_trafos, xs, ps)
      return(xs)
    }
  ),

  private = list(
    # function to set unsatisfied deps to NA in the design dt "data":
    # walk thru all params, toposorted order, then walk thru all deps
    # and set values in x to NA which where the dep is not OK
    set_deps_to_na = function() {

      ps = self$param_set
      native = .Call(C_design_dependency_plan, self$data, ps)
      # Keep the public Design data.table mutable by reference, but all graph
      # planning and condition evaluation comes from the single native engine.
      for (edge in seq_along(native$rows)) {
        set(
          self$data,
          i = native$rows[[edge]],
          j = native$columns[[edge]],
          value = native$values[[edge]]
        )
      }
      invisible(NULL)
    }
  )
)
