#' @title Sampler Class
#'
#' @description
#' This is the abstract base class for sampling objects like [Sampler1D], [SamplerHierarchical] or [SamplerJointIndep].
#' The Sampler family is intentionally subclassable: custom subclasses
#' implement `private$.sample()` (and optionally `private$.print()`). The
#' additive-only inheritance restriction of Paradox 2 applies to the ParamSet
#' family, not to this documented Sampler extension point.
#'
#' @template param_param_set
#'
#' @family Sampler
#' @export
Sampler = R6Class("Sampler",
  public = list(
    #' @field param_set ([`ParamSet`])\cr
    #' Domain / support of the distribution we want to sample from.
    param_set = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via derived classes,
    #' e.g., [Sampler1D].
    #' @param param_set ([`ParamSet`])\cr
    #'   The [`ParamSet`] to associate with this `Sampler`.
    initialize = function(param_set) {
      assert_r6(param_set, "ParamSet")
      # Own one graph generation before deriving any sampler metadata. An
      # assertion followed by a later clone could otherwise validate one
      # generation and retain another after an allocation finalizer.
      owned = param_set$clone(deep = TRUE)
      assert_param_set(owned, no_untyped = TRUE)
      self$param_set = owned
    },

    #' @description
    #' Sample `n` values from the distribution.
    #'
    #' @param n (`integer(1)`).
    #' @return [`Design`].
    sample = function(n) {
      assert_count(n) # we do argcheck on toplevel
      # A custom Sampler is allowed to execute arbitrary R code in `.sample()`.
      # Select its support before that callback and require the public field to
      # retain the same shell afterward. Otherwise rows generated from one
      # support can be handed to Design with a later, rebound ParamSet. The
      # identity comparison is allocation-free and does not traverse the
      # ParamSet graph; native built-in samplers retain their existing hot
      # path apart from this one pointer comparison.
      param_set = self$param_set
      data = private$.sample(n)
      if (!identical(self$param_set, param_set)) {
        stop(
          "Sampler changed its ParamSet while sampling",
          call. = FALSE
        )
      }
      # Preserve the requested number of points.
      Design$new(param_set, data, remove_dupl = FALSE)
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
      catf(format(self))
      catf("For params: %s", str_trunc(str_collapse(self$param_set$ids()), width = 40L))
      private$.print()
    }
  ),
  private = list(
    .sample = function(n) stop("abstract"), # inheriting classes have to implement this
    .print = function() {
    } # inheriting classes can overwrite to add lines
  )
)
