# The native engine selects and snapshots the exact origin callback and hidden
# values. The retained closure has one fixed plan binding and immediately
# enters the native adapter; it contains no second merge or result-admission
# implementation.
param_set_shadow_constraint_closure = function(plan) {
  force(plan)
  function(x) {
    .Call(C_param_set_shadow_constraint, plan, x)
  }
}

param_set_shadow_constraint_factory = function(callback, hidden_values) {
  param_set_shadow_constraint_closure(list(
    callback = callback,
    hidden_values = hidden_values
  ))
}

#' @title A Live View on a ParamSet
#'
#' @description
#' `ParamSetShadow` presents a fixed subset of another [`ParamSet`] while
#' retaining a live connection to that set. The IDs supplied in `shadowed` are
#' hidden. Values are read from and written through to the origin, and current
#' dependencies, constraints, and transformations are used for every
#' operation.
#'
#' The visible parameter schema is captured when the shadow is constructed.
#' Later changes to structural metadata in the origin therefore do not change
#' the view. Dependencies may change, but a dependency crossing the
#' visible/hidden boundary is always an error.
#'
#' @param set ([`ParamSet`])
#'   Parameter set to view. A [`ParamSetCollection`] is accepted. A direct
#'   `ParamSetShadow` origin is rejected; construct the combined view from its
#'   underlying origin instead.
#' @param shadowed (`character()`)
#'   IDs to hide. Must be a subset of `set$ids()`.
#'
#' @examples
#' origin = ps(x = p_dbl(0, 1), flag = p_lgl())
#' origin$values = list(x = 0.5, flag = TRUE)
#'
#' view = ParamSetShadow$new(origin, "x")
#' view$values
#' view$values = list(flag = FALSE)
#' origin$values
#'
#' @export
ParamSetShadow = R6Class("ParamSetShadow", inherit = ParamSet,
  public = list(
    #' @description
    #' Creates a `ParamSetShadow`.
    initialize = function(set, shadowed) {
      # Native construction captures the immutable visible schema and the
      # first complete live-origin snapshot in one transaction. It therefore
      # owns type, uniqueness, subset, graph, and corruption diagnostics; the
      # R shell neither scans IDs nor stores a second visible/hidden schema.
      private$.core = .Call(
        C_param_set_shadow_construct,
        set,
        shadowed
      )
      invisible(self)
    },

    #' @description
    #' Adds a dependency between two visible parameters to the origin.
    #' @param id (`character(1)`) Dependent parameter ID.
    #' @param on (`character(1)`) Parent parameter ID.
    #' @param cond ([`Condition`]) Dependency condition.
    #' @param allow_dangling_dependencies (`logical(1)`) Retained for API
    #'   compatibility. A shadow cannot admit a dangling parent because it
    #'   would cross its fixed visible boundary.
    #' @return `self`, invisibly.
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
    #' Reconstructs a visible Domain from the synchronized view state.
    get_domain = function(id) {
      super$get_domain(id)
    },

    #' @description
    #' Creates a detached ParamSet subset of the synchronized view.
    subset = function(ids, allow_dangling_dependencies = FALSE,
      keep_constraint = TRUE, keep_trafo = TRUE) {
      super$subset(
        ids,
        allow_dangling_dependencies = allow_dangling_dependencies,
        keep_constraint = keep_constraint,
        keep_trafo = keep_trafo
      )
    },

    #' @description
    #' Creates detached one-dimensional ParamSets from the synchronized view.
    subspaces = function(ids = self$ids()) {
      super$subspaces(ids)
    }
  ),

  active = list(
    #' @field origin ([`ParamSet`]) Exact wrapped object. Read-only.
    origin = function(value) {
      origin = private$.origin()
      if (!missing(value)) {
        # Permit R's synthetic write-back after `shadow$origin$values <- ...`;
        # replacing the edge with a different object remains unsupported.
        if (!identical(value, origin)) stop("origin is read-only.")
        return(value)
      }
      origin
    },

    #' @field params ([data.table::data.table])
    #' Detached facade of the fixed visible parameter schema, with an ordinary
    #' non-ALTREP/non-S4 table shell and structural metadata.
    params = function(value) {
      if (!missing(value)) stop("params is read-only.")
      super$params
    },

    #' @field domains (named `list` of [`Domain`])
    #' Detached Domains reconstructed from the fixed visible schema and the
    #' current dependency/transformation snapshot.
    domains = function(value) {
      if (!missing(value)) stop("domains is read-only.")
      super$domains
    },

    #' @field deps ([data.table::data.table])
    #' Current origin dependencies wholly inside the visible schema. Read-only;
    #' the detached facade has ordinary non-ALTREP/non-S4 structural metadata.
    deps = function(value) {
      if (!missing(value)) stop("deps is read-only.")
      # The inherited native dependency reader owns the authoritative Shadow
      # refresh and snapshots the resulting generation. Do not preflight the
      # same graph from R.
      super$deps
    },

    #' @field tags (named `list()`)
    #' Tags captured with the visible schema. Read-only for a shadow.
    tags = function(value) {
      if (!missing(value)) stop("tags is read-only.")
      super$tags
    },

    #' @field constraint (`function` or `NULL`)
    #' Live origin constraint adapted to the visible schema. Read-only.
    constraint = function(value) {
      if (!missing(value)) {
        stop("ParamSetShadow does not allow setting constraint.")
      }
      invisible(.Call(C_param_set_shadow_refresh, self, private))
      private$.state()$.constraint
    },

    #' @field extra_trafo (`function` or `NULL`)
    #' The origin's live extra transformation. Assignment writes through.
    extra_trafo = function(value) {
      if (missing(value)) {
        invisible(.Call(C_param_set_shadow_refresh, self, private))
        return(private$.state()$.extra_trafo)
      }
      # Validate and refresh the immutable origin edge before performing
      # the outward write. Corrupt snapshot metadata must never allow a write
      # to reach an otherwise valid origin.
      origin = private$.origin()
      origin$extra_trafo = value
      value
    },

    #' @field has_constraint (`logical(1)`)
    #' Whether the origin currently has a constraint.
    has_constraint = function() {
      invisible(.Call(C_param_set_shadow_refresh, self, private))
      !is.null(private$.state()$.constraint)
    }
  ),

  private = list(
    .origin = function() {
      # `$origin` and every origin-directed mutation are semantic Shadow reads.
      # Admit the complete metadata signature and live origin graph before
      # exposing the edge or executing any operation through it.
      invisible(.Call(C_param_set_shadow_refresh, self, private))
      if (!identical(.Call(C_param_set_core_kind, private), 3L)) {
        stop("Corrupt ParamSetShadow capsule kind", call. = FALSE)
      }
      sets = private$.state()$.sets
      if (!is.list(sets) || length(sets) != 1L ||
          !inherits(sets[[1L]], "ParamSet") || !is.environment(sets[[1L]])) {
        stop("Corrupt ParamSetShadow origin edge", call. = FALSE)
      }
      sets[[1L]]
    },

    .get_values = function() {
      invisible(.Call(C_param_set_shadow_refresh, self, private))
      private$.state()$.values
    },

    deep_clone = function(name, value) {
      switch(name,
        .core = param_set_core_deep_clone(self, value),
        value
      )
    }
  )
)
