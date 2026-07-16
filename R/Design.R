design_transpose_param_trafos = function(xs, param_set) {
  # Retain the historical call expression for overridden methods and their
  # errors.  In particular, conditionCall() remains `ps$trafo(x)`.
  ps = param_set
  fallback = function() map(xs, function(x) ps$trafo(x))

  # Exact package-generated log-scale transformations are pure scalar kernels.
  # The native lane authenticates their complete ParamSet/function state and
  # declines before invoking any user callback; everything else retains the
  # callback-preserving implementation below.
  native = .Call(C_design_transpose_logscale_builtin, xs, param_set)
  if (!is.null(native)) {
    return(native)
  }

  if (!isTRUE(.Call(C_param_set_surface_auth, param_set, 1L)) ||
      !identical(class(param_set), c("ParamSet", "R6")) ||
      !is.null(param_set$extra_trafo)) {
    return(fallback())
  }

  enclosure = param_set$.__enclos_env__
  if (!is.environment(enclosure) || !is.environment(enclosure$private)) {
    return(fallback())
  }
  trafos = enclosure$private$.trafos
  if (typeof(trafos) != "list" ||
      !identical(class(trafos), c("data.table", "data.frame")) ||
      !identical(names(trafos), c("id", "trafo")) ||
      !identical(attr(trafos, "sorted", exact = TRUE), "id") ||
      typeof(trafos$id) != "character" ||
      typeof(trafos$trafo) != "list" ||
      length(trafos$id) != length(trafos$trafo)) {
    return(fallback())
  }

  # Keep independent vector shells: data.table permits callbacks to replace
  # list elements by reference, while ParamSet$trafo() snapshots the joined
  # table before invoking the first callback in a row.
  trafo_ids = trafos$id[]
  trafo_functions = trafos$trafo[]
  if (anyNA(trafo_ids) || any(!nzchar(trafo_ids)) ||
      anyDuplicated(trafo_ids) ||
      !all(vapply(trafo_functions, is.function, logical(1L)))) {
    return(fallback())
  }

  # Validate and match every row before running user code. If a public field was
  # altered after construction, the fallback must not repeat callbacks that
  # have already run.
  matches = vector("list", length(xs))
  for (row in seq_along(xs)) {
    x = xs[[row]]
    row_ids = names(x)
    if (typeof(x) != "list" ||
        !identical(names(attributes(x)), "names") ||
        is.null(row_ids) || anyNA(row_ids) || any(!nzchar(row_ids)) ||
        anyDuplicated(row_ids)) {
      return(fallback())
    }
    matches[[row]] = match(row_ids, trafo_ids, nomatch = 0L)
  }

  # Snapshot the canonical function list once, but retain the historical
  # row/parameter callback order and call expression. Single-bracket
  # assignment keeps NULL and vector-valued results as one list element.
  for (row in seq_along(xs)) {
    x = xs[[row]]
    matched = matches[[row]]
    for (column in which(matched != 0L)) {
      id = trafo_ids[[matched[[column]]]]
      trafo = trafo_functions[[matched[[column]]]]
      value = x[[column]]
      x[column] = list(param_set_call_trafo(id, trafo, value))
    }

    # An individual callback may install an extra transformation. Historically
    # ParamSet$trafo() reads it after all individual callbacks, so it still
    # applies to the current row.
    extra_trafo = param_set$extra_trafo
    added_extra_trafo = !is.null(extra_trafo)
    if (added_extra_trafo) {
      xin = x
      if (test_function(extra_trafo, args = c("x", "param_set"))) {
        x = extra_trafo(x = xin, param_set = param_set)
      } else {
        x = extra_trafo(xin)
      }
    }
    xs[row] = list(x)

    # A callback can also replace or mutate the canonical table by reference.
    # The current row correctly uses its pre-callback snapshot; hand only the
    # untouched tail to the public method so later rows see the new state.
    current_trafos = enclosure$private$.trafos
    same_trafos = typeof(current_trafos) == "list" &&
      identical(class(current_trafos), c("data.table", "data.frame")) &&
      identical(names(current_trafos), c("id", "trafo")) &&
      identical(attr(current_trafos, "sorted", exact = TRUE), "id") &&
      identical(current_trafos$id, trafo_ids) &&
      identical(current_trafos$trafo, trafo_functions)
    same_surface = isTRUE(.Call(C_param_set_surface_auth, param_set, 1L))
    if ((!same_surface || !same_trafos || added_extra_trafo) &&
        row < length(xs)) {
      remaining = seq.int(row + 1L, length(xs))
      xs[remaining] = map(xs[remaining], function(x) ps$trafo(x))
      return(xs)
    }
  }
  xs
}

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
      if (is.null(xs)) {
        xs = transpose_list(self$data)
        if (filter_na) {
          xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
        }
      }
      if (ps$has_trafo && trafo) {
        xs = design_transpose_param_trafos(xs, ps)
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
      native = .Call(C_design_dependency_plan_builtin, self$data, ps)
      if (!is.null(native)) {
        # The native lane is a read-only, all-or-nothing planner. Keep the
        # established data.table by-reference mutation and one set() call for
        # every dependency row, including empty row selections.
        for (edge in seq_along(native$rows)) {
          set(
            self$data,
            i = native$rows[[edge]],
            j = native$columns[[edge]],
            value = native$values[[edge]]
          )
        }
        return(invisible(NULL))
      }

      graph = ps$deps[, 1:2]
      colnames(graph) = c("id", "parents")
      # we need to make sure that every param has a (maybe empty) row in the graph table
      fillin = data.table(id = ps$ids(), parents = list(character(0L)))
      graph = rbind(graph, fillin[fillin$id %nin% graph$id, ])
      graph = graph[, list("parents" = list(unlist(get("parents"), use.names = FALSE))), by = "id"]
      topo = topo_sort(graph)
      pids_sorted = topo$id
      storage_types = ps$storage_type
      for (param_id in pids_sorted) {
        dd = ps$deps[get("id") == param_id, ]
        for (j in seq_row(dd)) {
          pcol = self$data[[dd$on[j]]]
          # we are ok if parent was active and cond on parent is OK
          not_ok = which(is.na(pcol) | !condition_test(dd$cond[[j]], pcol))
          set(self$data, not_ok, j = param_id, value = as_type(NA, storage_types[[param_id]]))
        }
      }
    }
  )
)
