#' @title SamplerUnif Class
#'
#' @description
#' Uniform random sampling for an arbitrary (bounded) [ParamSet].
#' Sampling itself is one native operation over the parameter-set capsule and
#' works for base sets, collections, live shadows, and dependencies.
#' A zero-level categorical parameter can produce a typed zero-row design;
#' requesting one or more rows from it errors before randomness is consumed.
#'
#' The inherited `$samplers` field remains a list of [`Sampler1DUnif`] objects
#' for source compatibility and introspection. It is descriptive: changing a
#' child object does not change the uniform engine. Replacing or reordering the
#' list is rejected; construct a [`SamplerHierarchical`] when custom one-
#' dimensional sampler behavior is required.
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
    #'   The [`ParamSet`] to associate with this `SamplerUnif`.
    initialize = function(param_set) {
      assert_param_set(param_set, must_bounded = TRUE, no_deps = FALSE, no_untyped = TRUE)
      # The native issuer constructs fresh singleton subset states and wraps
      # each in a SamplerUnif-only, single-use ownership handoff. This avoids
      # defensively cloning a shell that has never been exposed or aliased;
      # ordinary public Sampler1DUnif construction keeps its clone boundary.
      handoffs = .Call(
        C_sampler_unif_subspace_handoffs,
        param_set,
        param_set$ids(),
        param_set$extra_trafo
      )
      samplers = lapply(handoffs, Sampler1DUnif$new)
      super$initialize(param_set, samplers)
      private$.canonical_samplers = self$samplers
    }
  ),
  private = list(
    .canonical_samplers = NULL,
    .sample = function(n) {
      if (!identical(self$samplers, private$.canonical_samplers)) {
        stop(
          "`SamplerUnif$samplers` is read-only; use `SamplerHierarchical` for custom samplers",
          call. = FALSE
        )
      }
      .Call(C_sampler_unif_sample_builtin, self$param_set, n)
    }
  )
)
