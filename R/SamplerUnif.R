#' @title SamplerUnif Class
#'
#' @description
#' Uniform random sampling for an arbitrary (bounded) [ParamSet].
#' Constructs 1 uniform sampler per parameter, then passes them to [SamplerHierarchical].
#' Hence, also works for [ParamSet]s sets with dependencies.
#'
#' @template param_param_set
#'
#' @family Sampler
#' @include SamplerHierarchical.R
#' @export
SamplerUnif = R6Class("SamplerUnif", inherit = SamplerHierarchical,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param param_set ([`ParamSet`])\cr
    #'   The [`ParamSet`] to associated with this `SamplerUnif`.
    initialize = function(param_set) {
      assert_param_set(param_set, must_bounded = TRUE, no_deps = FALSE, no_untyped = TRUE)
      fallback_subspaces = NULL
      native = if (isTRUE(.Call(C_param_set_surface_auth, param_set, 4L))) {
        private = param_set$.__enclos_env__$private
        # Match ParamSet$subspaces(): exact base objects snapshot public values
        # before observing the private ID order. Altered/subclass surfaces do
        # not gain a speculative getter observation.
        values = param_set$values
        ids = private$.params$id
        if (length(ids)) {
          result = param_set_call_subspace_plans(
            param_set,
            private,
            ids,
            values
          )
          if (is.null(result)) {
            fallback_subspaces = param_set_subspaces_fallback(
              param_set,
              private,
              ids,
              values
            )
          }
          result
        } else {
          fallback_subspaces = named_list()
          NULL
        }
      }
      samplers = if (!is.null(fallback_subspaces)) {
        lapply(fallback_subspaces, Sampler1DUnif$new)
      } else if (is.null(native)) {
        lapply(param_set$subspaces(), Sampler1DUnif$new)
      } else {
        bulk = .Call(
          C_sampler_1d_unif_bulk_shells,
          ParamSet,
          sampler_1d_unif_generators(),
          native,
          values
        )
        if (is.null(bulk)) {
          lapply(native, function(plan) Sampler1DUnif$new(plan$state))
        } else {
          bulk
        }
      }
      super$initialize(param_set, samplers)
    }
  ),
  private = list(
    # Exact package-built samplers draw and map every independent dimension in
    # one native pass. Altered objects, subclasses, unsupported state, and old
    # R runtimes return the NULL sentinel before touching the RNG and retain
    # the complete hierarchical implementation.
    .sample = function(n) {
      native = .Call(
        C_sampler_unif_sample_builtin,
        self,
        self$param_set,
        self$samplers,
        n
      )
      if (!is.null(native)) return(native)
      super$.sample(n)
    }
  )
)
