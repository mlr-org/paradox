#' @title Design of Configurations
#'
#' @description
#' A lightweight wrapper around a [ParamSet] and a [data.table::data.table()], where the
#' latter is a design of configurations produced from the former - e.g.,
#' by calling a [generate_design_grid()] or by sampling.
#'
#' @export
Design = R6Class("Design",
  public = list(
    #' @field param_set ([ParamSet]).
    param_set = NULL,

    #' @field data ([data.table::data.table()])\cr
    #' Stored `data`.
    data = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param param_set ([ParamSet]).
    #' @param data ([data.table::data.table()])\cr
    #'   Stored `data`.
    #' @param remove_dupl (`logical(1)`)\cr
    #'   Remove duplicates?
    initialize = function(param_set, data, remove_dupl) {

      assert_param_set(param_set)
      assert_data_table(data, ncols = param_set$length)
      assert_names(colnames(data), permutation.of = param_set$ids())
      self$param_set = param_set
      # FIXME: this works in general but is not really fast, as we generate the col first, then overwrite it,
      # OTOH this is really robust
      # set fixed param vals to their constant values
      # FIXME: this might also be problematic for LHS
      # do we still create an LHS like this?
      imap(param_set$values, function(v, n) set(data, j = n, value = v))
      self$data = data
      if (param_set$has_deps) {
        private$set_deps_to_na()
      }
      # NB: duplicated rows can happen to to NA setting
      if (remove_dupl) {
        self$data = unique(self$data)
      } # remove duplicated rows
    },


    #' @description
    #' Helper for print outputs.
    format = function() {
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
      xs = transpose_list(self$data)
      if (filter_na) {
        xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
      }
      if (ps$has_trafo && trafo) {
        xs = map(xs, function(x) ps$trafo(x))
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
      graph = ps$deps[, 1:2]
      colnames(graph) = c("id", "parents")
      # we need to make sure that every param has a (maybe empty) row in the graph table
      fillin = data.table(id = ps$ids(), parents = list(character(0L)))
      graph = rbind(graph, fillin[fillin$id %nin% graph$id, ])
      graph = graph[, list("parents" = list(unlist(get("parents")))), by = "id"]
      topo = topo_sort(graph)
      pids_sorted = topo$id
      storage_types = ps$storage_type
      for (param_id in pids_sorted) {
        dd = ps$deps[get("id") == param_id, ]
        for (j in seq_row(dd)) {
          pcol = self$data[[dd$on[j]]]
          # we are ok if parent was active and cond on parent is OK
          not_ok = which(is.na(pcol) | !dd$cond[[j]]$test(pcol))
          set(self$data, not_ok, j = param_id, value = as_type(NA, storage_types[[param_id]]))
        }
      }
    }
  )
)
