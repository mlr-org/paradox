# The native engine selects and snapshots the exact origin callback and the
# already activity-filtered hidden values. Package-owned graph operations also
# filter the visible slice before invoking this schema-free carrier. The
# retained closure has one fixed plan binding and immediately enters the native
# merge adapter; it contains no second activity, merge, or result-admission
# implementation.
param_set_shadow_constraint_closure = function(plan) {
  force(plan)
  .paradox_strip_srcref(function(x) {
    .Call(C_param_set_shadow_constraint, plan, x)
  })
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
#' operation. `$values` is the raw visible store and may include dormant
#' entries. The default `$get_values()` view evaluates the current visible
#' dependency graph using recorded defaults and omits dormant visible entries.
#'
#' What the shadow fixes is the *hidden* set, not the visible schema: the view
#' is "origin minus hidden", computed live. A parameter the origin gains later
#' is therefore visible, and changed structural metadata -- tags, for
#' instance -- is reflected. Dependencies may change too, but a dependency
#' crossing the visible/hidden boundary is always an error. A dependency on a
#' parameter that does not exist in the origin at all does not cross anything:
#' it is shown verbatim and enforced as never satisfiable, exactly as in the
#' origin, and becomes an ordinary dependency as soon as the origin gains that
#' parameter. Because the hidden set is fixed at construction and IDs are
#' unique, such a parent always arrives visible.
#'
#' Mutations through the view reach the origin: `$values<-`, `$add_dep()`, and
#' `$extra_trafo<-` write through, while `$constraint<-` and `$deps<-` are
#' refused (change them on the origin). `$add_dep()` needs `id` to be a visible
#' parameter, and `on` either visible or absent from the origin -- an `on` this
#' view hides is refused, while an absent one follows
#' `allow_dangling_dependencies` as it does on a plain [`ParamSet`]. `$tags<-`
#' is the one exception to write-through: it is this view's own answer for the
#' IDs it names and leaves the origin's tags untouched.
#'
#' Checked assignment validates every supplied visible value, including a
#' dependency-inactive value, and preserves hidden origin values. If the live
#' origin has a constraint, the complete merged configuration is filtered for
#' activity before its visible and hidden slices are merged for that callback.
#' The write-through operation remains one atomic graph transaction.
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
    #' Adds a dependency to the origin. `id` must be visible; `on` must be
    #' visible or absent from the origin, because a parameter this view hides
    #' cannot be depended on across its boundary.
    #' @param id (`character(1)`) Dependent parameter ID.
    #' @param on (`character(1)`) Parent parameter ID.
    #' @param cond ([`Condition`]) Dependency condition.
    #' @param allow_dangling_dependencies (`logical(1)`) Whether to allow a
    #'   parent the origin does not have. Refusing a hidden `on` does not
    #'   depend on this flag.
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

    #' @field constraint (`function` or `NULL`)
    #' Live origin constraint adapted to the visible schema. During
    #' package-owned check, test, and assignment operations it receives the
    #' merged active visible and hidden subsets. Read-only. Source-reference
    #' normalization occurs both when the callback is assigned to the origin
    #' and when the package-generated visible-schema adapter is created.
    constraint = function(value) {
      if (!missing(value)) {
        stop("ParamSetShadow does not allow setting constraint.")
      }
      private$.state()$.constraint
    },

    #' @field extra_trafo (`function` or `NULL`)
    #' The origin's live extra transformation. Assignment writes through and
    #' therefore applies the origin's source-reference normalization.
    extra_trafo = function(value) {
      if (missing(value)) {
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
      !is.null(private$.state()$.constraint)
    }
  ),

  private = list(
    .origin = function() {
      # `$origin` and every origin-directed mutation are semantic Shadow reads.
      # Admit the complete metadata signature and live origin graph before
      # exposing the edge or executing any operation through it.
      invisible(.Call(C_param_set_core_refresh, self, private))
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
