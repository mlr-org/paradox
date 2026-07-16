#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6Class
#' @importFrom stats runif rnorm
#' @useDynLib paradox, .registration = TRUE, .fixes = "C_"
"_PACKAGE"



# data.table-variables to announce:
# .init_given, .trafo

utils::globalVariables(c("J", "id", "original_id", "owner_ps_index", ".tags", "tag", ".trafo", "trafo", ".", "cargo", "default", "cls", "cond", "on", "required"))

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)

  # Native dependency filtering authenticates these exact closures without
  # ever forcing a user-supplied delayed binding. Resolve the package's own
  # lazy-load promises once here, while the namespace is still canonical.
  invisible(list(
    condition_test,
    condition_test.CondEqual,
    condition_test.CondAnyOf,
    topo_sort,
    seq_row,
    set,
    as_type
  ))
  invisible(.Call(
    C_ps_builtin_runtime,
    list(
      p_dbl = p_dbl,
      p_int = p_int,
      p_fct = p_fct,
      p_lgl = p_lgl
    ),
    NO_DEF
  ))
  invisible(.Call(
    C_domain_builtin_runtime,
    list(
      p_dbl = p_dbl,
      p_int = p_int,
      p_fct = p_fct,
      p_lgl = p_lgl
    ),
    NO_DEF,
    base::sort
  ))
  invisible(.Call(
    C_design_dependency_runtime,
    getNamespaceVersion("mlr3misc")[[1L]]
  ))

  # data.table's secondary-index attribute is an internal format. Probe the
  # loaded version once; native ParamSet construction synthesizes indices only
  # when the exact ordering and metadata schema match this process.
  params_probe = data.table(
    id = c("z", "a", "m"),
    cls = rep("ParamDbl", 3L),
    grouping = rep("ParamDbl", 3L)
  )
  setindexv(params_probe, c("id", "cls", "grouping"))
  identity_probe = data.table(
    id = c("a", "m", "z"),
    cls = rep("ParamDbl", 3L),
    grouping = rep("ParamDbl", 3L)
  )
  setindexv(identity_probe, c("id", "cls", "grouping"))
  tags_probe = data.table(tag = c("a", "B", "", "_", "b", "A", "a"))
  setindexv(tags_probe, "tag")
  empty_probe = data.table(tag = character())
  setindexv(empty_probe, "tag")
  invisible(.Call(
    C_param_set_index_layout,
    getNamespaceVersion("data.table")[[1L]],
    attr(params_probe, "index", exact = TRUE),
    attr(tags_probe, "index", exact = TRUE),
    attr(identity_probe, "index", exact = TRUE),
    attr(empty_probe, "index", exact = TRUE)
  ))

  invisible(.Call(
    C_param_set_bulk_shell_register,
    ParamSet$new(),
    ParamSet
  ))
  # The optimized constructor bypasses these package-owned lazy bindings.
  # Force them once so native authentication can snapshot evaluated closures
  # without ever forcing a delayed binding itself.
  invisible(list(
    .__ParamSet__initialize,
    .__ParamSet__extra_trafo,
    .__Sampler1D__initialize,
    .__Sampler1DUnif__initialize,
    .__Sampler__initialize
  ))
  invisible(.Call(
    C_sampler_1d_unif_bulk_register,
    Sampler1DUnif$new(ps(.paradox_sampler_shell = p_lgl())),
    sampler_1d_unif_generators()
  ))

  register_namespace_callback(pkgname, "ParamHelpers", function(...) {
    warning("Packages 'paradox' and 'ParamHelpers' are conflicting and should not be loaded in the same session")
  })
} # nocov end

.onUnload = function(libpath) { # nocov start
  # unloadNamespace() unregisters the R namespace but does not unload its
  # shared object.  Explicitly close it so R_unload_paradox() can release the
  # process-global native snapshots retained during .onLoad().
  library.dynam.unload("paradox", libpath)
} # nocov end

leanify_package()
