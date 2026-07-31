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

      assert_r6(param_set, "ParamSet")
      # The namespace-owned grid token certifies a table produced and fully
      # normalized by the one-shot native grid operation. Keeping this branch
      # free of a second schema snapshot is both the grid hot path and the
      # reason the token is an identity object rather than a public flag.
      if (identical(remove_dupl, .design_prepared_grid)) {
        self$param_set = param_set
        self$data = data
        return(invisible(NULL))
      }

      assert_data_table(data)
      # Admit the complete public table first and then one complete ParamSet
      # graph. The native plan owns fixed-value classification, column
      # permutation validation, and dependency evaluation from that same
      # generation; separate `$length`, `$ids`, `$storage_type`, `$values`,
      # and `$deps` reads could combine generations around a finalizer.
      native = .Call(C_design_dependency_plan, data, param_set)
      self$param_set = param_set
      self$data = data
      for (index in seq_along(native$fixed_columns)) {
        value = native$fixed_values[[index]]
        # The replacement is always passed as a one-column list: a bare list
        # value of length one would take data.table's unwrap path and store
        # the leaf's content instead of the wrapped leaf in a 1-row design.
        set(
          self$data,
          j = native$fixed_columns[[index]],
          value = if (native$fixed_plain[[index]]) {
            value
          } else {
            list(rep(list(value), nrow(self$data)))
          }
        )
      }
      for (edge in seq_along(native$rows)) {
        set(
          self$data,
          i = native$rows[[edge]],
          j = native$columns[[edge]],
          value = native$values[[edge]]
        )
      }
      # NB: duplicated rows can happen due to NA setting.
      if (remove_dupl) {
        self$data = unique(self$data)
      }
      # The native plan retains the exact BASE capsule or complete
      # COLLECTION/SHADOW graph it used. All callback-capable R work is now
      # complete, so one allocation-free scan is the terminal barrier: a
      # finalizer or custom callback which moved the live support during the
      # by-reference patch wave wins, and this constructor refuses to return a
      # Design whose rows describe the earlier generation.
      .Call(C_param_set_generation_receipt, native$receipt)
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
  )
)
