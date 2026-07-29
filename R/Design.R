.design_prepared_grid = new.env(parent = emptyenv())

#' @title Design of Configurations
#'
#' @description
#' A lightweight wrapper around a [ParamSet] and a [data.table::data.table()], where the
#' latter is a design of configurations produced from the former - e.g.,
#' by calling a [generate_design_grid()] or by sampling.
#' Design operations use Paradox's shared structural data-frame/data-table
#' boundary. A well-formed ordinary class vector ends in `"data.frame"` or
#' `c("data.table", "data.frame")`; leading additive classes are
#' representation-only and never dispatch. The classifier does not copy or
#' materialize an ordinary shell merely to remove the prefix, and native
#' semantic snapshots ignore it; an already-required ALTREP snapshot installs
#' only the canonical suffix. Malformed class vectors
#' reject. Names and accepted data.table cache
#' carriers are ordinary structure. Raw row names are attribute-free, nonobject, non-S4 integer or
#' character vectors; compact counts and stable ALTREP lengths are supported,
#' but row labels are not interpreted. An admitted top-level VECSXP ALTREP
#' shell is materialized once after names/classes are owned; semantic atomic
#' columns may likewise be stable ALTREP. Package-created tables retain
#' canonical ordinary metadata.
#'
#' @export
Design = R6Class("Design",
  public = list(
    #' @field param_set ([ParamSet]).
    param_set = NULL,

    #' @field data ([data.table::data.table()])\cr
    #' Stored data.table shell. The shared suffix-aware public-table boundary and its
    #' count-only row-name rules apply. An allowed top-level VECSXP ALTREP is
    #' accepted once, and semantic atomic columns may be stable ALTREP.
    data = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param param_set ([ParamSet]).
    #' @param data ([data.table::data.table()])\cr
    #'   Stored `data`. The shared suffix-aware public-table boundary and its count-only
    #'   row-name rules apply. An allowed top-level VECSXP ALTREP is accepted
    #'   once, and admitted semantic atomic columns may be stable ALTREP.
    #' @param remove_dupl (`logical(1)`)\cr
    #'   Remove duplicates?
    initialize = function(param_set, data, remove_dupl) {

      assert_param_set(param_set)
      assert_data_table(data, ncols = param_set$length)
      assert_names(colnames(data), permutation.of = param_set$ids())
      self$param_set = param_set
      self$data = data
      if (!identical(remove_dupl, .design_prepared_grid)) {
        # Apply fixed values at this one generator-independent boundary. Random,
        # Sobol, LHS, and directly constructed designs therefore share identical
        # overwrite and dependency behavior even when their input contains
        # placeholder data. The native grid generator has already performed this
        # normalization while pruning its search and enters with the package's
        # namespace-owned prepared-grid token.
        storage_types = param_set$storage_type
        imap(param_set$values, function(v, n) {
          # Mirror the native grid generator: one ordinary element of the
          # parameter's own storage type collapses into that typed column,
          # while every other legal stored value -- `NULL`, a multi-element or
          # cross-storage special value, an S4 or list leaf -- keeps its
          # identity as a list-column entry. Assigning such a value as a plain
          # column would instead coerce it to `NA`, recycle or reject it by
          # length, or (for `NULL`) delete the column outright.
          if (inherits(v, "TuneToken")) {
            stopf("Design generation cannot materialize the stored TuneToken value of parameter '%s'.", n)
          }
          if (design_column_value_is_plain(v, storage_types[[n]])) {
            set(data, j = n, value = v)
          } else {
            set(data, j = n, value = rep(list(v), nrow(data)))
          }
        })
        private$set_deps_to_na()
        # NB: duplicated rows can happen due to NA setting.
        if (remove_dupl) {
          self$data = unique(self$data)
        }
      }
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
    #' The stored table must continue to satisfy the shared public-table
    #' boundary. Its captured row-name count must match its columns. A
    #' zero-column data.frame retains that count, so this method returns one
    #' empty configuration per row; an unclassed empty list represents zero
    #' rows. An allowed top-level VECSXP ALTREP and stable atomic columns are
    #' each materialized once.
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

# A stored parameter value collapses into an ordinary typed design column only
# when it is exactly one attribute-free element of the parameter's own storage
# type. `p_uty()` stores into a list column, so its values never collapse.
design_column_value_is_plain = function(value, storage_type) {
  if (storage_type == "list" || !is.atomic(value) || is.object(value) ||
    length(value) != 1L || !is.null(attributes(value))) {
    return(FALSE)
  }
  switch(storage_type,
    numeric = is.double(value) || is.integer(value),
    integer = is.integer(value),
    character = is.character(value),
    logical = is.logical(value),
    FALSE
  )
}
