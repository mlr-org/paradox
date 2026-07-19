# Return a fresh canonical zero-row dependency store. Capsule tables are plain
# data.frames; data.table metadata is added only to a detached public facade.
new_empty_deps = function() {
  param_set_internal_table(structure(
    list(id = character(0L), on = character(0L), cond = list()),
    row.names = integer(0L),
    class = "data.frame"
  ))
}

# Capsule tables are deliberately ordinary data.frames, never data.tables.
# They are fixed-width aligned column stores: package code replaces a complete
# table when it mutates state, while public accessors construct detached
# data.table facades. Keeping this conversion in one place prevents a
# constructor or legacy upgrader from accidentally retaining data.table's
# mutable indices, spare capacity, or external self-reference in the capsule.
param_set_internal_table = function(x) {
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x))) {
    stop("Internal error: invalid ParamSet state table", call. = FALSE)
  }
  columns = unname(as.list(x))
  sizes = lengths(columns)
  rows = if (length(sizes)) sizes[[1L]] else 0L
  if (length(sizes) && any(sizes != rows)) {
    stop("Internal error: unaligned ParamSet state table", call. = FALSE)
  }
  structure(
    columns,
    names = names(x),
    row.names = if (rows) seq_len(rows) else integer(),
    class = "data.frame"
  )
}

# Construct the outward mutable facade without invoking data.table. The native
# finalizer owns the shell/names and installs a valid self-reference; capsule
# columns stay shared only through R's copy-on-write rules and the capsule never
# retains the facade itself.
param_set_data_table_facade = function(x) {
  x = param_set_internal_table(x)
  table = structure(
    lapply(unname(as.list(x)), function(column) column[seq_along(column)]),
    names = names(x),
    row.names = if (nrow(x)) seq_len(nrow(x)) else integer(),
    class = c("data.table", "data.frame")
  )
  finalize_domain_data_table(table)
}

param_set_table_rows = function(table, rows) {
  param_set_internal_table(table[rows, , drop = FALSE])
}

param_set_table_match = function(table, values, column = "id",
    nomatch = 0L) {
  rows = match(values, table[[column]], nomatch = nomatch)
  if (nomatch == 0L) rows = rows[rows != 0L]
  param_set_table_rows(table, rows)
}

param_set_table_first = function(table, value, result, column = "id") {
  row = match(value, table[[column]])
  if (is.na(row)) NULL else table[[result]][[row]]
}

param_set_dependencies_snapshot = function(deps) {
  .Call(C_param_set_dependency_table_snapshot, deps)
}

# The only mutable binding in a ParamSet's private environment is `.core`.
# Its NULL-address external pointer protects this fixed, serializable payload.
# Package code captures the payload once per operation; every mutation installs
# a fresh capsule, so callbacks can mutate the object without changing the
# snapshot already being consumed by the outer operation.
param_set_core_new = function(kind, params, values = named_list(), tags,
    deps = new_empty_deps(), trafos, extra_trafo = NULL,
    constraint = NULL, sets = NULL, translation = NULL, postfix = FALSE) {
  .Call(C_param_set_core_new, as.integer(kind), list(
    .params = param_set_internal_table(params),
    .values = values,
    .tags = param_set_internal_table(tags),
    .deps = param_set_dependencies_snapshot(deps),
    .trafos = param_set_internal_table(trafos),
    .extra_trafo = extra_trafo,
    .constraint = constraint,
    .sets = sets,
    .translation = if (is.null(translation)) NULL else param_set_internal_table(translation),
    .postfix = postfix
  ))
}

param_set_core_state = function(private) {
  .Call(C_param_set_core_state, private)
}

param_set_core_replace = function(private, ...) {
  updates = list(...)
  table_fields = intersect(names(updates), c("params", "tags", "trafos", "translation"))
  for (field in table_fields) {
    if (!is.null(updates[[field]])) {
      updates[[field]] = param_set_internal_table(updates[[field]])
    }
  }
  if ("deps" %in% names(updates)) {
    updates$deps = param_set_dependencies_snapshot(updates$deps)
  }
  if (length(updates)) {
    names(updates) = paste0(".", names(updates))
  }
  invisible(.Call(C_param_set_core_replace, private, updates))
}

# R6 invokes this hook while it is constructing the root shell. Recursing via
# `child$clone(deep = TRUE)` here would start an independent clone transaction
# for every edge: a shared child would be duplicated and a Shadow's origin
# could disagree with the origin stored in its capsule. Instead, discover the
# complete package node graph first, shallow-clone each non-root shell once,
# and install post-order capsule copies using one identity memo. Clone is a
# cold operation, so the deliberately simple linear identity lookup is a much
# better tradeoff than exposing a second native graph engine.
param_set_core_deep_clone = function(self, core) {
  node_index = function(node, nodes) {
    for (index in seq_along(nodes)) {
      if (identical(node, nodes[[index]])) return(index)
    }
    0L
  }

  node_private = function(node) {
    private = get_private(node)
    if (!inherits(node, "ParamSet") || !is.environment(private)) {
      stop("Corrupt ParamSet node in capsule graph", call. = FALSE)
    }
    private
  }

  node_snapshot = function(node, expected_core = NULL) {
    private = node_private(node)
    current_core = private$.core
    if (!is.null(expected_core) && !identical(current_core, expected_core)) {
      stop("ParamSet capsule changed during deep clone", call. = FALSE)
    }
    kind = .Call(C_param_set_core_kind, current_core)
    if (identical(kind, 3L)) {
      # A clone rebuilds derived Shadow metadata against cloned identities, but
      # it must first admit the source through the same authoritative refresh
      # as every semantic reader. Malformed signatures therefore error instead
      # of being silently healed by cloning the protected payload alone.
      current_core = .Call(C_param_set_shadow_refresh, node, private)
    }
    state = .Call(C_param_set_core_state, current_core)
    sets = state$.sets
    if (identical(kind, 1L)) {
      if (!is.null(sets)) {
        stop("Corrupt BASE ParamSet graph edges", call. = FALSE)
      }
      sets = list()
    } else if (identical(kind, 2L)) {
      if (!is.list(sets) || is.object(sets)) {
        stop("Corrupt COLLECTION ParamSet graph edges", call. = FALSE)
      }
    } else if (identical(kind, 3L)) {
      if (!is.list(sets) || is.object(sets) || length(sets) != 1L) {
        stop("Corrupt SHADOW ParamSet graph edge", call. = FALSE)
      }
    } else {
      stop("Corrupt ParamSet capsule kind", call. = FALSE)
    }
    if (length(sets) && any(!vapply(
        sets,
        function(child) inherits(child, "ParamSet") && is.environment(child),
        logical(1L)
      ))) {
      stop("Corrupt ParamSet capsule graph child", call. = FALSE)
    }
    list(private = private, core = current_core, kind = kind, state = state,
      sets = sets)
  }

  same_edges = function(left, right) {
    length(left) == length(right) && identical(names(left), names(right)) &&
      all(vapply(seq_along(left), function(index) {
        identical(left[[index]], right[[index]])
      }, logical(1L)))
  }

  # Phase one is callback-free topology discovery. The three colors distinguish
  # a shared completed node from a repeated node on the active path.
  root = node_snapshot(self, core)
  nodes = list(self)
  snapshots = list(root)
  edges = list(root$sets)
  colors = 1L
  next_child = 1L
  stack = 1L
  postorder = integer()

  while (length(stack)) {
    index = stack[[length(stack)]]
    children = edges[[index]]
    child_position = next_child[[index]]
    if (child_position <= length(children)) {
      child = children[[child_position]]
      next_child[[index]] = child_position + 1L
      child_index = node_index(child, nodes)
      if (child_index && colors[[child_index]] == 1L) {
        stop("ParamSet capsule graph contains a cycle", call. = FALSE)
      }
      if (child_index) next

      child_snapshot = node_snapshot(child)
      child_index = length(nodes) + 1L
      nodes[[child_index]] = child
      snapshots[[child_index]] = child_snapshot
      edges[[child_index]] = child_snapshot$sets
      colors[[child_index]] = 1L
      next_child[[child_index]] = 1L
      stack[[length(stack) + 1L]] = child_index
      next
    }

    colors[[index]] = 2L
    postorder[[length(postorder) + 1L]] = index
    stack = stack[-length(stack)]
  }

  edge_indices = lapply(edges, function(children) {
    vapply(children, node_index, integer(1L), nodes = nodes)
  })

  # Retain established ParamUty deep-clone behavior: each top-level R6 value
  # occurrence is cloned independently, while opaque nested containers and
  # non-R6 environments keep their ordinary R identity semantics.
  clone_value = function(value) {
    is_r6 = is.environment(value) && inherits(value, "R6") &&
      is.function(tryCatch(value$clone, error = function(error) NULL))
    if (is_r6) value$clone(deep = TRUE) else value
  }

  clone_payload = function(snapshot, cloned_sets) {
    state = snapshot$state
    if (identical(snapshot$kind, 3L)) {
      template = param_set_core_new(
        3L,
        params = state$.params,
        values = named_list(),
        tags = state$.tags,
        deps = new_empty_deps(),
        trafos = state$.trafos,
        extra_trafo = NULL,
        constraint = NULL,
        sets = cloned_sets,
        translation = NULL,
        postfix = FALSE
      )
      return(.Call(
        C_param_set_shadow_core_new,
        template,
        cloned_sets[[1L]]
      ))
    }
    values = lapply(state$.values, clone_value)
    deps = state$.deps
    trafos = state$.trafos
    extra_trafo = state$.extra_trafo
    constraint = state$.constraint
    param_set_core_new(
      snapshot$kind,
      params = state$.params,
      values = values,
      tags = state$.tags,
      deps = deps,
      trafos = trafos,
      extra_trafo = extra_trafo,
      constraint = constraint,
      sets = if (identical(snapshot$kind, 1L)) NULL else cloned_sets,
      translation = state$.translation,
      postfix = state$.postfix
    )
  }

  # Origins are cloned before Shadows and children before Collections. The
  # authoritative native Shadow builder therefore derives dynamic fields from
  # the already rewired origin. Its immutable edge is checked again before use.
  clones = vector("list", length(nodes))
  cloned_cores = vector("list", length(nodes))
  for (index in postorder) {
    snapshot = snapshots[[index]]
    if (identical(snapshot$kind, 3L)) {
      current = node_snapshot(nodes[[index]])
      if (!identical(current$kind, snapshot$kind) ||
          !same_edges(current$sets, snapshot$sets)) {
        stop("ParamSet capsule graph changed during deep clone", call. = FALSE)
      }
      snapshot = current
    }

    children = edge_indices[[index]]
    cloned_sets = if (!length(children)) list() else {
      result = lapply(children, function(child) clones[[child]])
      names(result) = names(snapshot$sets)
      result
    }
    cloned_core = clone_payload(snapshot, cloned_sets)
    cloned_cores[[index]] = cloned_core
    if (index != 1L) {
      clone = nodes[[index]]$clone(deep = FALSE)
      if (!inherits(clone, "ParamSet") || !is.environment(clone) ||
          identical(clone, nodes[[index]])) {
        stop("Cannot clone ParamSet capsule graph child", call. = FALSE)
      }
      clone_private = node_private(clone)
      clone_private$.core = cloned_core
      clones[[index]] = clone
    }
  }
  cloned_cores[[1L]]
}

# Install outward data.table metadata on a detached table shell. This helper is
# presentation-only; semantic state has already been constructed and validated
# by the native engine.
finalize_domain_data_table = function(table) {
  .Call(C_finalize_data_table, table)
}

# A native `$params` result has a valid data.table self-reference but no spare
# column-pointer capacity. Adding a column therefore makes data.table take a
# shallow copy and assign that temporary back to the extraction expression.
# Preserve the historical `ps$params[, new := value]` and
# `data.table::set(ps$params, ...)` behavior without making the active binding
# generally writable. Function identity keeps this exception narrower and
# more robust than inspecting deparsed calls or error text.
params_data_table_temporary_reassignment = function() {
  depth = sys.nframe()
  data_table_namespace = asNamespace("data.table")

  reassign = get0(
    ".reassign_extracted_table",
    envir = data_table_namespace,
    inherits = FALSE
  )
  if (depth > 4L && !is.null(reassign) &&
      identical(sys.function(depth - 4L), reassign)) {
    return(TRUE)
  }

  set = get0("set", envir = data_table_namespace, inherits = FALSE)
  depth > 5L && !is.null(set) && identical(sys.function(depth - 5L), set)
}

# Build and consume package-owned one-row BASE capsules. The native call owns
# the complete node snapshot; the R loop performs only the unavoidable public
# R6 shell construction and never reimplements subset semantics.
param_set_subspace_shells = function(param_set, private, ids) {
  tokens = .Call(
    C_param_set_subspace_states,
    private,
    param_set,
    ids,
    param_set$extra_trafo
  )
  result = vector("list", length(tokens))
  for (index in seq_along(tokens)) {
    result[[index]] = ParamSet$new(tokens[[index]])
  }
  names(result) = names(tokens)
  result
}

#' @title ParamSet
#'
#' @description
#' An object representing the space of possible parametrizations of a function or another object.
#' `ParamSet`s are used on the side of objects being parameterized, where they function as a configuration space determining the set of possible configurations accepted by these objects.
#' They can also be used to specify search spaces for optimization, indicating the set of legal configurations to try out.
#' It is often convenient to generate search spaces from configuration spaces, which can be done using the `$search_space()` method in combination with `to_tune()` / [`TuneToken`] objects.
#'
#' Individual dimensions of a `ParamSet` are specified by [`Domain`] objects, created as [`p_dbl()`], [`p_lgl()`] etc.
#' The field `$values` can be used to store an active configuration or to partially fix
#' some parameters to constant values -- the precise effect can be determined by the object being parameterized.
#'
#' Constructing a `ParamSet` can be done using `ParamSet$new()` in combination with a named list of [`Domain`] objects.
#' This route is recommended when the set of dimensions (i.e. the members of this named list) is dynamically created, such as when the number of parameters is variable.
#' `ParamSet`s can also be created using the [`ps()`] shorthand, which is the recommended way when the set of parameters is fixed.
#' In practice, the majority of cases where a `ParamSet` is created, the [`ps()`] should be used.
#'
#' Public parameter/dependency tables and reconstructed [`Domain`] objects are
#' detached from capsule state. Mutating a returned table, list-column, or
#' [`Condition`] does not mutate the `ParamSet`; use documented setters such as
#' `$values`, `$tags`, `$deps`, and `$add_dep()` instead. Third-party inheritance
#' from the ParamSet family is additive only: Paradox core methods, active
#' bindings, and private capsule state may not be replaced.
#' Interpreted outer list/table shells and their structural metadata are
#' ordinary non-ALTREP/non-S4 objects. Stable ALTREP is supported for admitted
#' semantic atomic values and table columns, with the one documented
#' `set_values(.values=)` shell exception described below.
#'
#' @section S3 methods and type converters:
#' * `as.data.table()`\cr
#'   `ParamSet` -> [data.table::data.table()]\cr
#'   Detached compact representation as a data table. Mutating it does not
#'   mutate the `ParamSet`. Column types are:\cr
#'     - id: character
#'     - class: character
#'     - lower, upper: numeric
#'     - levels: list col, with NULL elements
#'     - nlevels: integer valued numeric
#'     - is_bounded: logical
#'     - special_vals: list col of list
#'     - default: list col
#'     - storage_type: character
#'     - tags: list col of character vectors
#' @examples
#' pset = ParamSet$new(
#'   params = list(
#'     d = p_dbl(lower = -5, upper = 5, default = 0, trafo = function(x) 2^x),
#'     f = p_fct(levels = letters[1:3])
#'   )
#' )
#'
#' # alternative, recommended way of construction in this case since the
#' # parameter list is not dynamic:
#' pset = ps(
#'   d = p_dbl(lower = -5, upper = 5, default = 0, trafo = function(x) 2^x),
#'   f = p_fct(levels = letters[1:3])
#' )
#'
#' pset$check(list(d = 2.1, f = "a"))
#'
#' pset$check(list(d = 2.1, f = "d"))
#' @export
ParamSet = R6Class("ParamSet",
  public = list(

    #' @field assert_values (`logical(1)`)\cr
    #' Should values be checked for validity during assignment to active binding `$values`?
    #' Default is `TRUE`, only switch this off if you know what you are doing.
    assert_values = TRUE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param params (named `list()`)\cr
    #'   Ordinary non-ALTREP/non-S4 list of [`Domain`] objects, named with their
    #'   respective ID. Its names/list metadata is interpreted structure.
    #' @param allow_dangling_dependencies (`character(1)`)\cr
    #'   Whether dependencies depending on parameters that are not present should be allowed. A parameter `x` having
    #'   `depends = y == 0` if `y` is not present would usually throw an error, but if dangling
    #'   dependencies are allowed, the dependency is added regardless. This is mainly for internal
    #'   use.
    initialize = function(params = named_list(), allow_dangling_dependencies = FALSE) {
      # Native subset state is a private, validated, single-use hand-off.
      # Keeping it in the existing `params` formal preserves the public R6
      # constructor signature and makes ordinary external pointers continue
      # through the established validation path.
      if (typeof(params) == "externalptr") {
        adopted = .Call(C_param_set_adopt_subset_state, private, params)
        if (!isTRUE(adopted)) {
          stop(
            "Invalid or already consumed ParamSet subset state",
            call. = FALSE
          )
        }
        return(invisible(NULL))
      }

      deps = new_empty_deps()

      # Additive subclasses share the same sealed state. Overriding core
      # methods or replacing generator members is intentionally unsupported;
      # subclass identity is therefore not a reason to duplicate construction
      # in R.
      native = .Call(C_param_set_construct, params)
      paramtbl = native$params
      tags = native$tags
      trafos = native$trafos
      initvalues = native$init_values
      private$.core = param_set_core_new(
        1L,
        params = paramtbl,
        tags = tags,
        deps = deps,
        trafos = trafos
      )

      for (row in seq_along(native$requirements)) {
        for (req in native$requirements[[row]]) {
          invoke(
            self$add_dep,
            id = paramtbl$id[[row]],
            allow_dangling_dependencies = allow_dangling_dependencies,
            .args = req
          )
        }
      }

      # The native constructor always returns an ordinary named list. Avoid
      # installing an identical empty value generation when no Domain supplied
      # an initial value.
      if (length(initvalues)) self$values = initvalues
      invisible(initvalues)
    },

    #' @description
    #' Retrieves IDs of contained parameters based on some filter criteria
    #' selections, `NULL` means no restriction.
    #' Only returns IDs of parameters that satisfy all conditions.
    #'
    #' @param class (`character()`)\cr
    #'   Typically a subset of `"ParamDbl"`, `"ParamInt"`, `"ParamFct"`, `"ParamLgl"`, `"ParamUty"`.
    #'   Return only IDs of dimensions with the given class.
    #' @param tags (`character()`).
    #'   Return only IDs of dimensions that have *all* tags given in this argument.
    #' @param any_tags (`character()`).
    #'   Return only IDs of dimensions that have at least one of the tags given in this argument.
    #' @return `character()`.
    ids = function(class = NULL, tags = NULL, any_tags = NULL) {
      .Call(C_param_set_ids_lazy, private, environment())
    },

    #' @description
    #' Retrieves parameter values based on some selections, `NULL` means no
    #' restriction and is equivalent to `$values`.
    #' Only returns values of parameters that satisfy all conditions.
    #'
    #' @param class (`character()`). See `$ids()`.
    #' @param tags (`character()`). See `$ids()`.
    #' @param any_tags (`character()`). See `$ids()`.
    #' @param type (`character(1)`)\cr
    #'   Return values `"with_token"` (i.e. all values),
    #    `"without_token"` (all values that are not [`TuneToken`] objects), `"only_token"` (only [`TuneToken`] objects)
    #    or `"with_internal"` (all values that are no not `InternalTuneToken`)?
    #' @param check_required (`logical(1)`)\cr
    #'   Check if all required parameters are set?
    #' @param remove_dependencies (`logical(1)`)\cr
    #'   If `TRUE`, set values with dependencies that are not fulfilled to `NULL`.
    #' @return Named `list()`.
    get_values = function(class = NULL, tags = NULL, any_tags = NULL,
      type = "with_token", check_required = TRUE, remove_dependencies = TRUE) {
      .Call(C_param_set_get_values, private, self, environment())
    },

    #' @description
    #' Modifies (and overwrites) or replaces the parameter values.
    #' Per default already set values are being kept unless new values are being provided.
    #'
    #' @param ... (any)\cr
    #'   Named parameter values.
    #' @param .values (named `list()`)\cr
    #'   Named list with parameter values. Names must not already appear in
    #'   `...`. This is the sole public structural-list ALTREP exception: native
    #'   code snapshots the supplied shell once before interpreting its names
    #'   and elements. S4 shells remain unsupported. Direct `$values <-`
    #'   assignment does not share this exception.
    #' @param .insert (`logical(1)`)\cr
    #'   Whether to insert the values (old values are being kept, if not overwritten), or to
    #'   replace all values. Default is TRUE.
    #'
    set_values = function(..., .values = list(), .insert = TRUE) {
      dots = list(...)
      # Capture language inputs in documented left-to-right order before the
      # native merge observes them. Validation itself remains native-only.
      force(.values)
      if (!identical(.insert, TRUE) && !identical(.insert, FALSE)) {
        stop("`.insert` must be TRUE or FALSE", call. = FALSE)
      }
      new_values = .Call(
        C_param_set_values_merge,
        dots,
        .values,
        if (.insert) self$values else NULL,
        .insert
      )
      self$values = new_values
      invisible(self)
    },

    #' @description
    #' Perform transformation specified by the `trafo` of [`Domain`] objects, as well as the `$extra_trafo` field.
    #' @param x (named `list()` | `data.frame`)\cr
    #'   The value(s) to be transformed. The outer list or documented ordinary
    #'   data-frame shell and its structural names/list metadata must be
    #'   non-ALTREP/non-S4. Admitted semantic atomic leaves or columns may be
    #'   stable ALTREP. A callback result has the same ordinary list-shell
    #'   requirement; there is no R fallback or replay.
    #' @param param_set (`ParamSet`)\cr
    #'   Passed to `extra_trafo()`. Note that the `extra_trafo` of `self` is used, not the `extra_trafo` of the
    #'   `ParamSet` given in the `param_set` argument.
    #'   In almost all cases, the default `param_set = self` should be used.
    trafo = function(x, param_set = self) {
      .Call(C_param_set_trafo, private, self, x, param_set)
    },

    #' @description
    #'
    #' Aggregate parameter values according to their aggregation rules.
    #'
    #' @param x (named `list()` of `list()`s)\cr
    #'   The value(s) to be aggregated. Names are parameter values.
    #'   The aggregation function is selected based on the parameter.
    #'
    #' @return (named `list()`)
    aggr_internal_tuned_values = function(x) {
      assert_list(x, types = "list")
      params = private$.state()$.params
      present = vapply(params$cargo, function(cargo) is.function(cargo$aggr), logical(1L))
      aggr_ids = params$id[present]
      aggrs = lapply(params$cargo[present], "[[", "aggr")
      names(aggrs) = aggr_ids
      assert_subset(names(x), aggr_ids)
      if (!length(x)) {
        return(named_list())
      }
      imap(x, function(value, .id) {
        if (!length(value)) {
          stopf("Trying to aggregate values of parameters '%s', but there are no values", .id)
        }
        aggrs[[.id]](value)
      })
    },

    #' @description
    #'
    #' Set the parameter values so that internal tuning for the selected parameters is disabled.
    #'
    #' @param ids (`character()`)\cr
    #'   The ids of the parameters for which to disable internal tuning.
    #' @return `Self`
    disable_internal_tuning = function(ids) {
      assert_subset(ids, self$ids(tags = "internal_tuning"))
      state = private$.state()
      cargos = state$.params$cargo[match(ids, state$.params$id)]
      pvs = Reduce(c, map(cargos, "disable_in_tune")) %??% named_list()
      self$set_values(.values = pvs)
    },

    #' @description
    #' Convert all parameters from the search space to parameter values using the transformation given by
    #' `in_tune_fn`.
    #' @param search_space ([`ParamSet`])\cr
    #'   The internal search space.
    #' @return (named `list()`)
    convert_internal_search_space = function(search_space) {
      assert_class(search_space, "ParamSet")
      # A Shadow value read refreshes its dynamic origin snapshot. Capture the
      # resulting capsule generation only afterward; BASE reads select the
      # same generation without an extra semantic path.
      param_vals = private$.get_values()
      state = private$.state()
      domains = search_space$domains
      converters = lapply(names(domains), function(.id) {
        converter = param_set_table_first(
          state$.params, .id, "cargo"
        )$in_tune_fn
        if (!is.function(converter)) {
          stopf("No converter exists for parameter '%s'", .id)
        }
        converter
      })
      names(converters) = names(domains)

      imap(domains, function(token, .id) {
        converters[[.id]](token, param_vals)
      })
    },

    #' @description
    #' \pkg{checkmate}-like test-function. Takes a named list.
    #' Return `FALSE` if the given `$constraint` is not satisfied, `TRUE` otherwise.
    #' Note this is different from satisfying the bounds or types given by the `ParamSet` itself:
    #' If `x` does not satisfy these, an error will be thrown, given that `assert_value` is `TRUE`.
    #' @param x (named `list()`)\cr
    #'   The value to test. Its outer shell must be an ordinary
    #'   non-ALTREP/non-S4 list; admitted semantic atomic leaves may be stable
    #'   ALTREP.
    #' @param assert_value (`logical(1)`)\cr
    #'   Whether to verify that `x` satisfies the bounds and types given by this `ParamSet`.
    #'   Should be `TRUE` unless this was already checked before.
    #' @return `logical(1)`: Whether `x` satisfies the `$constraint`.
    test_constraint = function(x, assert_value = TRUE) {
      .Call(
        C_param_set_test_constraint_builtin,
        private,
        self,
        x,
        assert_value
      )
    },

    #' @description
    #' \pkg{checkmate}-like test-function. Takes a [`data.table`][data.table::data.table].
    #' For each row, return `FALSE` if the given `$constraint` is not satisfied, `TRUE` otherwise.
    #' Note this is different from satisfying the bounds or types given by the `ParamSet` itself:
    #' If `x` does not satisfy these, an error will be thrown, given that `assert_value` is `TRUE`.
    #' @param x (`data.table`)\cr
    #'   The values to test. The documented data.table shell and its structural
    #'   metadata must be ordinary non-ALTREP/non-S4; admitted semantic atomic
    #'   columns may be stable ALTREP.
    #' @param assert_value (`logical(1)`)\cr
    #'   Whether to verify that `x` satisfies the bounds and types given by this `ParamSet`.
    #'   Should be `TRUE` unless this was already checked before.
    #' @return `logical`: For each row in `x`, whether it satisfies the `$constraint`.
    test_constraint_dt = function(x, assert_value = TRUE) {
      .Call(
        C_param_set_test_constraint_dt_builtin,
        private,
        self,
        x,
        assert_value
      )
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a setting of parameters as a named list.
    #' A point x is feasible, if it configures a subset of params in a valid manner, i.e.,
    #' the param type is correct, and param bounds, constraints and dependencies are satisfied.
    #' Params for which dependencies are not satisfied should not be part of `x` (and not set to `NA`).
    #' Param dependencies and `$constraint` are not checked when `check_strict` is `FALSE` (but data type and bounds are checked).
    #' This is sometimes useful when you only want to check the validity of individual params in intermediate objects.
    #' Use `presence = "all"` to check that all parameters are present in `xs`, except for parameters with unsatisfied dependencies.
    #' 'presence = "none"' is often useful when you want to check the validity of settings you want to assign when defaults are already present,
    #' 'presence = "all"' is often useful when configurations are created
    #' but some algorithm from a search space param set in optimization.
    #'
    #' @param xs (named `list()`).
    #'   The outer container must be an ordinary non-ALTREP list. An S3-classed
    #'   list carrying only `names` and `class` is accepted as representation
    #'   metadata and its class is discarded; no S3 method is dispatched. S4
    #'   shells remain unsupported.
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param sanitize (`logical(1)`)\cr
    #'   Whether to move values that are slightly outside bounds to valid values.
    #'   These values are accepted independent of `sanitize` (depending on the
    #'   `tolerance` arguments of `p_dbl()` and `p_int()`) . If `sanitize`
    #'   is `TRUE`, the additional effect is that, should checks pass, the
    #'   sanitized values of `xs` are added to the result as attribute `"sanitized"`.
    #' @param presence (`character(1)`)\cr
    #'   If `"none"` (default), no check is performed for the presence of parameters.
    #'   If `"all"`, all parameters must be present in `xs`, except for parameters with unsatisfied dependencies.
    #'   If `"required"`, parameters with the `"required"` tag must be present in `xs`, except for parameters with unsatisfied dependencies.
    #'   For `"all"` and `"required"`, `TuneToken`s are not allowed to be present in `xs`.
    #' @param allow_token (`logical(1)`)\cr
    #'   Whether to allow `TuneToken`s to be present in `xs`.
    #'   Default is `TRUE`.
    #' @return If successful `TRUE`, if not a string with an error message for
    #'   ordinary value infeasibility. Malformed exact TuneToken or Domain
    #'   structure raises a boundary error instead of returning a value
    #'   diagnostic.
    check = function(xs, check_strict = TRUE, sanitize = FALSE, presence = "none",  allow_token = TRUE) {
      .Call(
        C_param_set_check_builtin,
        private,
        self,
        xs,
        check_strict,
        sanitize,
        presence,
        allow_token
      )
    },

    #' @description
    #' \pkg{checkmate}-like check-function that checks only parameter
    #' dependencies. `xs` must be an ordinary non-ALTREP/non-S4 base list with complete, unique
    #' names; classed list containers are not admitted by this dependency-only
    #' boundary. Unknown parameter IDs are diagnosed even when the set has no
    #' dependencies. A dependent value or its parent supplied as a
    #' [`TuneToken`] is skipped, matching `$check()` dependency semantics.
    #'
    #' @param xs (uniquely named base `list()`).
    #' @return If successful `TRUE`, otherwise the first dependency or input
    #'   diagnostic as a string.
    check_dependencies = function(xs) {
      .Call(C_param_set_check_dependencies_builtin, private, self, xs)
    },

    #' @description
    #' \pkg{checkmate}-like test-function (s. `$check()`).
    #'
    #' @param xs (named `list()`).
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param presence (`character(1)`)\cr
    #'   If `"none"` (default), no check is performed for the presence of parameters.
    #'   If `"all"`, all parameters must be present in `xs`, except for parameters with unsatisfied dependencies.
    #'   If `"required"`, required parameters must be present in `xs`, except for parameters with unsatisfied dependencies.
    #'   For `"all"` and `"required"`, `TuneToken`s are not allowed to be present in `xs`.
    #' @param allow_token (`logical(1)`)\cr
    #'   Whether to allow `TuneToken`s to be present in `xs`.
    #'   Default is `TRUE`.
    #' @return If successful `TRUE`, if not `FALSE`.
    test = function(xs, check_strict = TRUE, presence = "none", allow_token = TRUE) makeTest(self$check(xs, check_strict = check_strict, presence = presence, allow_token = allow_token)),

    #' @description
    #' \pkg{checkmate}-like assert-function (s. `$check()`).
    #'
    #' @param xs (named `list()`).
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param .var.name (`character(1)`)\cr
    #'   Name of the checked object to print in error messages.\cr
    #'   Defaults to the heuristic implemented in [vname][checkmate::vname].
    #' @param sanitize (`logical(1)`)\cr
    #'   Whether to move values that are slightly outside bounds to valid values.
    #'   These values are accepted independent of `sanitize` (depending on the
    #'   `tolerance` arguments of `p_dbl()` and `p_int()`) . If `sanitize`
    #'   is `TRUE`, the additional effect is that `xs` is converted to within bounds.
    #' @param presence (`character(1)`)\cr
    #'   If `"none"` (default), no check is performed for the presence of parameters.
    #'   If `"all"`, all parameters must be present in `xs`, except for parameters with unsatisfied dependencies.
    #'   If `"required"`, required parameters must be present in `xs`, except for parameters with unsatisfied dependencies.
    #'   For `"all"` and `"required"`, `TuneToken`s are not allowed to be present in `xs`.
    #' @param allow_token (`logical(1)`)\cr
    #'   Whether to allow `TuneToken`s to be present in `xs`.
    #'   Default is `TRUE`.
    assert = function(xs, check_strict = TRUE, presence = "none", .var.name = vname(xs), sanitize = FALSE, allow_token = TRUE) {
      checkresult = self$check(xs, check_strict = check_strict, sanitize = sanitize, presence = presence, allow_token = allow_token)
      makeAssertion(if (sanitize) attr(checkresult, "sanitized") else xs, checkresult, .var.name, NULL)  # nolint
    },

    #' @description
    #' \pkg{checkmate}-like check-function. Takes a [data.table::data.table]
    #' where rows are points and columns are parameters.
    #' Checks in a similar manner as `$check(xs)`.
    #' A point x is feasible, if it configures a subset of params,
    #' all individual param constraints are satisfied and all dependencies are satisfied.
    #' Params for which dependencies are not satisfied should be set to `NA` in `xdt`.
    #' Dependencies and `$constraint` are not checked when `check_strict` is `FALSE`.
    #' Note that checking only subsets implies that `xdt` is therefore allowed to
    #' have fewer columns as there are params in the set.
    #'
    #' @param xdt ([data.table::data.table] | `data.frame()`).
    #'   An ordinary non-ALTREP/non-S4 table shell whose structural names,
    #'   row/dim/dimnames, and list metadata are ordinary. Admitted semantic
    #'   atomic columns may be stable ALTREP.
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param presence (`character(1)`)\cr
    #'   If `"none"` (default), no check is performed for the presence of parameters.
    #'   If `"all"`, all parameters must be present in `xdt` and not 'NA' if their dependencies are satisfied.
    #'   If `"required"`, required parameters must be present in `xdt` and not 'NA' if their dependencies are satisfied.
    #'   For `"all"` and `"required"`, `TuneToken`s are not allowed to be present in `xdt`.
    #' @param allow_token (`logical(1)`)\cr
    #'   Whether to allow `TuneToken`s to be present in `xdt`.
    #'   Default is `TRUE`.
    #' @return If successful `TRUE`, if not a string with the error message.
    check_dt = function(xdt, check_strict = TRUE, presence = "none", allow_token = TRUE) {
      .Call(
        C_param_set_check_dt_builtin,
        private,
        self,
        xdt,
        check_strict,
        presence,
        allow_token
      )
    },

    #' @description
    #' \pkg{checkmate}-like test-function (s. `$check_dt()`).
    #'
    #' @param xdt ([data.table::data.table]).
    #'   The table-shell and semantic-column boundary is the same as
    #'   `$check_dt()`.
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param presence (`character(1)`)\cr
    #'   If `"none"` (default), no check is performed for the presence of parameters.
    #'   If `"all"`, all parameters must be present in `xdt` and not 'NA' if their dependencies are satisfied.
    #'   If `"required"`, required parameters must be present in `xdt` and not 'NA' if their dependencies are satisfied.
    #'   For `"all"` and `"required"`, `TuneToken`s are not allowed to be present in `xdt`.
    #' @param allow_token (`logical(1)`)\cr
    #'   Whether to allow `TuneToken`s to be present in `xdt`.
    #'   Default is `TRUE`.
    #' @return If successful `TRUE`, if not `FALSE`.
    test_dt = function(xdt, check_strict = TRUE, presence = "none", allow_token = TRUE) makeTest(res = self$check_dt(xdt, check_strict = check_strict, presence = presence, allow_token = allow_token)),

    #' @description
    #' \pkg{checkmate}-like assert-function (s. `$check_dt()`).
    #'
    #' @param xdt ([data.table::data.table]).
    #'   The table-shell and semantic-column boundary is the same as
    #'   `$check_dt()`.
    #' @param check_strict (`logical(1)`)\cr
    #'   Whether to check that constraints and dependencies are satisfied.
    #' @param .var.name (`character(1)`)\cr
    #'   Name of the checked object to print in error messages.\cr
    #'   Defaults to the heuristic implemented in [vname][checkmate::vname].
    #' @param presence (`character(1)`)\cr
    #'   If `"none"` (default), no check is performed for the presence of parameters.
    #'   If `"all"`, all parameters must be present in `xdt` and not 'NA' if their dependencies are satisfied.
    #'   If `"required"`, required parameters must be present in `xdt` and not 'NA' if their dependencies are satisfied.
    #'   For `"all"` and `"required"`, `TuneToken`s are not allowed to be present in `xdt`.
    #' @param allow_token (`logical(1)`)\cr
    #'   Whether to allow `TuneToken`s to be present in `xdt`.
    #'   Default is `TRUE`.
    #' @return If successful `xs` invisibly, if not, an error is generated.
    assert_dt = function(xdt, check_strict = TRUE, presence = "none", .var.name = vname(xdt), allow_token = TRUE) makeAssertion(xdt, self$check_dt(xdt, check_strict = check_strict, presence = presence, allow_token = allow_token), .var.name, NULL), # nolint

    #' @description
    #' Map an unclassed numeric `matrix`, numeric `data.frame`, or
    #' `data.table` of values between 0 and 1 to proportional values inside the
    #' feasible intervals of individual parameters.
    #'
    #' @param x (`matrix` | `data.frame` | `data.table`)\cr
    #'   Values to map. Columns must be unclassed integer or double vectors.
    #'   Column names must be unique and a subset of the parameter IDs.
    #'   Data-frame/data-table shells and all structural names, row, dim,
    #'   dimnames, and list metadata must be ordinary non-ALTREP/non-S4. A
    #'   matrix or admitted atomic column may have stable ALTREP semantic
    #'   storage, which native admission materializes once.
    #'   [`ParamUty`][Domain] parameters do not define a quantile mapping.
    #' @return `data.table`.
    qunif = function(x) {
      .Call(C_param_set_qunif_builtin, private, self, x)
    },

    #' @description
    #' get the [`Domain`] object that could be used to create a given parameter.
    #'
    #' @param id (`character(1)`).
    #' @return [`Domain`].
    get_domain = function(id) {
      .Call(C_param_set_get_domain, private, self, id)
    },

    #' @description
    #' Create a new `ParamSet` restricted to the passed IDs.
    #' @param ids (`character()`).
    #' @param allow_dangling_dependencies (`logical(1)`)\cr
    #'   Whether to allow subsets that cut across parameter dependencies.
    #'   Dependencies that point to dropped parameters are kept (but will be "dangling", i.e. their `"on"` will not be present).
    #' @param keep_constraint (`logical(1)`)\cr
    #'   Whether to keep the `$constraint` function.
    #' @param keep_trafo (`logical(1)`)\cr
    #'   Whether to keep per-parameter transformations and the `$extra_trafo`
    #'   function. All three subset control flags must be unclassed,
    #'   attribute-free, non-missing logical scalars.
    #' @return `ParamSet`.
    subset = function(ids, allow_dangling_dependencies = FALSE,
      keep_constraint = TRUE, keep_trafo = TRUE) {
      token = .Call(
        C_param_set_subset_state,
        private,
        self,
        ids,
        allow_dangling_dependencies,
        keep_constraint,
        self$constraint,
        self$extra_trafo,
        keep_trafo
      )
      ParamSet$new(token)
    },

    #' @description
    #' Create new one-dimensional `ParamSet`s for each dimension.
    #' @param ids (`character()`)\cr
    #'   IDs for which to create `ParamSet`s. Defaults to all IDs.
    #' @return named `list()` of `ParamSet`.
    subspaces = function(ids = self$ids()) {
      param_set_subspace_shells(self, private, ids)
    },

    #' @description
    #' Create a `ParamSet` from this object, even if this object itself is not
    #' a `ParamSet` but e.g. a [`ParamSetCollection`].
    flatten = function() self$subset(private$.state()$.params$id, allow_dangling_dependencies = TRUE),

    #' @description
    #' Construct a [`ParamSet`] to tune over. Constructed from [`TuneToken`] in `$values`, see [`to_tune()`].
    #'
    #' @param values (`named list`)
    #'   Optional ordinary non-ALTREP named list, or ordinary non-ALTREP
    #'   S3-classed named list carrying only
    #'   `names` and `class`, of exact-shape [`TuneToken`] objects to convert in
    #'   place of `$values`. The outer class is discarded and no subsetting
    #'   method is dispatched. ALTREP, S4/list-like, and other attributed
    #'   containers are rejected. Construct tokens with [`to_tune()`]; subclasses and
    #'   metadata-extended tokens are not supported. Exact creator provenance is
    #'   not authenticated, so an indistinguishable manual copy may pass even
    #'   though the representation is not an API.
    search_space = function(values = self$values) {
      pars = private$get_tune_ps(values)
      on = NULL  # pacify static code check
      dangling_deps = pars$deps[!pars$ids(), on = "on"]
      if (nrow(dangling_deps)) {
        stopf("Dangling dependencies not allowed: Dependencies on %s dangling.", str_collapse(dangling_deps$on))
      }
      pars
    },

    #' @description
    #' Adds a dependency to this set, so that param `id` now depends on param
    #' `on`. Unlike bulk `$deps` assignment, this authoring method requires the
    #' Condition right-hand side to be feasible in the parent Domain.
    #'
    #' @param id (`character(1)`).
    #' @param on (`character(1)`).
    #' @param allow_dangling_dependencies (`logical(1)`): Whether to allow dependencies on parameters that are not present.
    #' @param cond ([Condition]).
    add_dep = function(id, on, cond, allow_dangling_dependencies = FALSE) {
      invisible(.Call(
        C_param_set_add_dependency,
        private,
        self,
        id,
        on,
        cond,
        allow_dangling_dependencies
      ))
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function() {
      sprintf("<%s(%s)>", class(self)[[1L]], self$length)
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    #' @param hide_cols (`character()`)\cr
    #'   Which fields should not be printed? Default is `"levels"`,
    #'   `"is_bounded"`, `"special_vals"`, `"tags"`, and `"storage_type"`.
    # printer, prints the set as a datatable, with the option to hide some cols
    print = function(..., hide_cols = c("levels", "is_bounded", "special_vals", "tags", "storage_type")) {
      catf(format(self))
      d = as.data.table(self)
      if (!nrow(d)) {
        catf("Empty.")
      } else {
        assert_subset(hide_cols, names(d))
        deps = self$deps
        if (nrow(deps)) { # add a nice extra charvec-col to the tab, which lists all parents-ids
          on = NULL
          dd = deps[, list(parents = list(unlist(on, use.names = FALSE))), by = "id"]
          d = merge(d, dd, by = "id", all.x = TRUE)
        }
        v = named_list(d$id) # add values to last col of print-dt as list col
        v = insert_named(v, self$values)
        d$value = list(v)
        print(d[, setdiff(colnames(d), hide_cols), with = FALSE])
      }
      if (self$has_trafo) {
        catf("Trafo is set.")
      } # Transformation functions can be very long, so omit them from the compact display.
    }
  ),

  active = list(

    #' @field data (`data.table`) Detached `data.table` representation of the
    #'   `ParamSet`. Mutating it does not mutate capsule state. Its table shell
    #'   and structural metadata are ordinary non-ALTREP/non-S4.
    data = function(v) {
      if (!missing(v)) stop("data is read-only")
      params = private$.state()$.params
      param_set_data_table_facade(list(
        id = params$id,
        class = params$cls,
        lower = params$lower,
        upper = params$upper,
        levels = params$levels,
        nlevels = self$nlevels,
        is_bounded = self$is_bounded,
        special_vals = params$special_vals,
        default = params$default,
        storage_type = params$storage_type,
        tags = self$tags
      ))
    },

    #' @template field_values
    values = function(xs) {
      if (missing(xs)) {
        return(private$.get_values())
      }
      if (self$assert_values) {
        # One native transaction snapshots the complete capsule graph,
        # validates strict dependency/constraint semantics, and commits every
        # ultimate BASE target only after all callbacks have returned. Both
        # checked and unchecked native stores reject a structural outer ALTREP
        # before observation. The native store canonicalizes the Paradox-1
        # NULL/ordinary zero-length clear-values spellings; R never
        # pre-observes that shell.
        xs = .Call(C_param_set_assign_values_checked, private, self, xs)
        return(xs)
      }
      private$.store_values(xs)
      xs
    },

    #' @template field_tags
    tags = function(v) {
      if (!missing(v)) {
        return(.Call(C_param_set_set_tags, private, self, v))
      }
      .Call(C_param_set_get_tags, private, self)
    },

    #' @template field_params
    params = function(rhs) {
      if (!missing(rhs)) {
        if (params_data_table_temporary_reassignment()) {
          return(rhs)
        }
        stop("params is read-only.")
      }

      .Call(C_param_set_params, private, self)
    },

    #' @field domains (named `list` of [`Domain`])
    #' Detached [`Domain`] objects that could be used to initialize this
    #' `ParamSet`. Mutating them does not mutate capsule state.
    domains = function(rhs) {
      if (!missing(rhs)) {
        stop("domains is read-only.")
      }
      .Call(C_param_set_domains, private, self)
    },

    #' @template field_extra_trafo
    extra_trafo = function(f) {
      if (missing(f)) {
        private$.state()$.extra_trafo
      } else {
        .Call(C_param_set_set_callback, private, self, f, 0L)
      }
    },

    #' @template field_constraint
    constraint = function(f) {
      if (missing(f)) {
        private$.state()$.constraint
      } else {
        .Call(C_param_set_set_callback, private, self, f, 1L)
      }
    },


    #' @template field_deps
    deps = function(v) {
      if (missing(v)) {
        param_set_data_table_facade(
          .Call(C_param_set_dependencies, private, self)
        )
      } else {
        .Call(C_param_set_set_dependencies, private, self, v)
      }
    },

    ############################
    # ParamSet flags

    #' @field length (`integer(1)`)\cr Number of contained parameters.
    length = function() nrow(private$.state()$.params),
    #' @field is_empty (`logical(1)`)\cr Is the `ParamSet` empty? Named with parameter IDs.
    is_empty = function() nrow(private$.state()$.params) == 0L,
    #' @field has_trafo (`logical(1)`)\cr Whether a `trafo` function is present, in parameters or in `extra_trafo`.
    has_trafo = function() !is.null(self$extra_trafo) || nrow(private$.state()$.trafos),
    #' @field has_extra_trafo (`logical(1)`)\cr Whether `extra_trafo` is set.
    has_extra_trafo = function() !is.null(self$extra_trafo),
    #' @field has_deps (`logical(1)`)\cr Whether the parameter dependencies are present
    has_deps = function() {
      .Call(C_param_set_has_dependencies, private, self)
    },
    #' @field has_constraint (`logical(1)`)\cr Whether parameter constraint is set.
    has_constraint = function() {
      if (inherits(self, "ParamSetCollection")) {
        !is.null(self$constraint)
      } else {
        !is.null(private$.state()$.constraint)
      }
    },
    #' @field all_numeric (`logical(1)`)\cr Is `TRUE` if all parameters are [`p_dbl()`] or [`p_int()`].
    all_numeric = function() all(self$is_number),
    #' @field all_categorical (`logical(1)`)\cr Is `TRUE` if all parameters are [`p_fct()`] and [`p_lgl()`].
    all_categorical = function() all(self$is_categ),
    #' @field all_bounded (`logical(1)`)\cr Is `TRUE` if all parameters are bounded.
    all_bounded = function() all(self$is_bounded),

    ############################
    # Per-Parameter properties

    #' @field class (named `character()`)\cr Classes of contained parameters. Named with parameter IDs.
    class = function() with(private$.state()$.params, set_names(cls, id)),
    #' @field lower (named `double()`)\cr Lower bounds of numeric parameters (`NA` for non-numerics). Named with parameter IDs.
    lower = function() with(private$.state()$.params, set_names(lower, id)),
    #' @field upper (named `double()`)\cr Upper bounds of numeric parameters (`NA` for non-numerics). Named with parameter IDs.
    upper = function() with(private$.state()$.params, set_names(upper, id)),
    #' @field levels (named `list()` of `character`)\cr Allowed levels of categorical parameters (`NULL` for non-categoricals).
    #' Named with parameter IDs.
    levels = function() with(private$.state()$.params, set_names(levels, id)),
    #' @field storage_type (`character()`)\cr Data types of parameters when stored in tables. Named with parameter IDs.
    storage_type = function() with(private$.state()$.params, set_names(storage_type, id)),
    #' @field special_vals (named `list()` of `list()`)\cr Special values for all parameters. Named with parameter IDs.
    special_vals = function() with(private$.state()$.params, set_names(special_vals, id)),
    #' @field default (named `list()`)\cr Default values of all parameters. If no default exists, element is not present.
    #' Named with parameter IDs.
    default = function() {
      params = private$.state()$.params
      keep = !map_lgl(params$default, is_nodefault)
      set_names(params$default[keep], params$id[keep])
    },
    #' @field has_trafo_param (`logical()`)\cr Whether `trafo` is set for any parameter.
    has_trafo_param = function() with(private$.state()$.params, set_names(id %in% private$.state()$.trafos$id, id)),
    #' @field is_logscale (`logical()`)\cr Whether `trafo` was set to `logscale` during construction.\cr
    #' Note that this only refers to the `logscale` flag set during construction, e.g. `p_dbl(logscale = TRUE)`.
    #' If the parameter was set to logscale manually, e.g. through `p_dbl(trafo = exp)`,
    #' this `is_logscale` will be `FALSE`.
    is_logscale = function() with(private$.state()$.params, set_names(cls %in% c("ParamDbl", "ParamInt") & map_lgl(cargo, function(x) isTRUE(x$logscale)), id)),

    ############################
    # Per-parameter properties for the five maintained native Domain kinds

    #' @field nlevels (named `integer()`)\cr Number of distinct levels of parameters. `Inf` for double parameters or unbounded integer parameters.
    #' Named with param IDs.
    nlevels = function() {
      .Call(C_param_set_property, private$.state()$.params, 0L)
    },

    #' @field is_number (named `logical()`)\cr Whether parameter is [`p_dbl()`] or [`p_int()`]. Named with parameter IDs.
    is_number = function() {
      .Call(C_param_set_property, private$.state()$.params, 1L)
    },

    #' @field is_categ (named `logical()`)\cr Whether parameter is [`p_fct()`] or [`p_lgl()`]. Named with parameter IDs.
    is_categ = function() {
      .Call(C_param_set_property, private$.state()$.params, 2L)
    },

    #' @field is_bounded (named `logical()`)\cr Whether parameters have finite bounds. Named with parameter IDs.
    is_bounded = function() {
      .Call(C_param_set_property, private$.state()$.params, 3L)
    }
  ),

  private = list(
    .core = NULL,
    .state = function() param_set_core_state(private),
    .store_values = function(xs) {
      invisible(.Call(C_param_set_store_values, private, self, xs))
    },
    .get_values = function() private$.state()$.values,

    get_tune_ps = function(values) {
      # C receives the complete container so S3 `[` methods cannot participate
      # in filtering. It selects exact built-in TuneTokens, snapshots their
      # target Domains (including dependency requirements), and seals exact
      # BASE ParamSet candidates before any candidate callback can run.
      admitted = .Call(C_tune_token_snapshot_list, private, self, values)
      values = admitted$tokens
      if (!length(values)) return(ParamSet$new())
      params = admitted$targets

      # Reconstruct the relevant dependency rows solely from the detached
      # entry-state Domain requirements. Candidate callbacks may mutate the
      # source ParamSet; this operation still uses the generation selected at
      # its native admission boundary, while the next call observes mutation.
      requirement_counts = vapply(
        params,
        function(param) length(param$.requirements[[1L]]),
        integer(1L)
      )
      source_deps = vector("list", sum(requirement_counts))
      output = 0L
      for (index in seq_along(params)) {
        requirements = params[[index]]$.requirements[[1L]]
        if (is.null(requirements)) next
        for (requirement in requirements) {
          output = output + 1L
          source_deps[[output]] = list(
            id = names(params)[[index]],
            on = requirement$on[[1L]],
            cond = requirement$cond
          )
        }
      }

      partsets = pmap(list(values, params), tunetoken_to_ps)
      pars = ps_union(partsets)  # partsets does not have names here, wihch is what we want.

      names(partsets) = names(values)
      idmapping = map(partsets, function(x) x$ids())

      # Only add dependencies whose child and parent both survived token
      # selection. Keep this as an ordinary-list loop: no temporary
      # data.table facade or second dependency engine is needed.
      for (dependency in source_deps) {
        id = dependency$id
        on = dependency$on
        if (!(id %in% names(idmapping)) || !(on %in% names(partsets))) next
        cond = dependency$cond
        onpar = partsets[[on]]
        if (onpar$has_trafo || !identical(onpar$ids(), on)) {
          # cannot have dependency on a parameter that is being trafo'd
          next
        }
        # remove infeasible values from condition
        cond$rhs = keep(cond$rhs, function(x) partsets[[on]]$test(set_names(list(x), on)))
        if (!length(cond$rhs)) {
          # no value is feasible, but there may be a trafo that fixes this
          # so we are forgiving here.
          next
        }
        for (idname in idmapping[[id]]) {
          pars$add_dep(idname, on, cond)
        }
      }
      pars
    },

    deep_clone = function(name, value) {
      switch(name,
        .core = param_set_core_deep_clone(self, value),
        value
      )
    }
  )
)

recover_domain = function(sd) {
  class(sd) = c(sd$cls[1], "Domain", class(sd))
  sd
}

#' @export
as.data.table.ParamSet = function(x, ...) { # nolint
  x$data
}

#' @export
rd_info.ParamSet = function(obj, descriptions = character(), ...) { # nolint
  if (obj$length == 0L) {
    return("Empty ParamSet")
  }

  params = as.data.table(obj)[, c("id", "storage_type", "default", "lower", "upper", "levels"), with = FALSE]
  cargo = obj$params$cargo

  if (length(descriptions)) {
    params = merge(params, enframe(descriptions, name = "id", value = "description"), all.x = TRUE, by = "id")
    description = NULL
    params[is.na(description), description := ""]
    setcolorder(params, c("id", "description"))
  }
  is_default = map_lgl(params$default, inherits, "NoDefault")
  is_uty = params$storage_type == "list"
  set(params, i = which(is_uty & !is_default), j = "default",
      value = map(cargo[!is_default & is_uty], function(x) x$repr))
  set(params, i = which(is_uty), j = "storage_type", value = list("untyped"))
  set(params, i = which(is_default), j = "default", value = list("-"))

  if (!allMissing(params$lower) || !allMissing(params$upper)) {
    set(params, j = "range", value = pmap_chr(params[, c("lower", "upper"), with = FALSE], rd_format_range))
  }
  remove_named(params, c("lower", "upper"))

  if (all(lengths(params$levels) == 0L)) {
    remove_named(params, "levels")
  } else {
    set(params, j = "levels", value = map_chr(params$levels, str_collapse, n = 10L))
  }
  setnames(params, "storage_type", "type")
  x = c("", knitr::kable(params, col.names = capitalize(names(params))))
  paste(x, collapse = "\n")
}
