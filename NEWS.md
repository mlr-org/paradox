# paradox 2.0.0

## Native core

* Native data.table facades now allocate a minimal column-pointer shell through
  the exported `alloc.col()` interface when used with data.table versions older
  than 1.18. This fixes shallow subsetting and update joins on supported R 4.3
  installations without copying columns or changing the fast path for current
  data.table releases.
* The retained pre-R-4.6 Domain reconstruction path now returns independently
  owned names and a valid data.table self-reference with the same observable
  attribute order as the native facade. Its registered finalizer owns the
  outer table shell and names before normalization, so aliases are not changed,
  and the fallback preserves Latin-1 and other marked ID encodings.
* `Design$transpose()` now builds ordinary row configurations in registered C
  code. Classed or otherwise dispatch-sensitive columns retain the historical R
  path, while common numeric, integer, logical, character, and list-column
  designs avoid the generic transpose/filter pipeline. Callback-capable ALTREP
  containers, names, and columns are declined before native observation and
  are handled solely by that R path. Exact base `ParamSet`s
  without an `extra_trafo` also apply their canonical individual parameter
  transformations without rebuilding and joining a data.table for every row;
  callback order, errors, side effects, and scalar, `NULL`, or vector-valued
  results retain the established behavior. The generated public transformation
  wrapper is authenticated without forcing delayed replacements and is
  rechecked between rows, while callback calls and frames remain identical to
  `ParamSet$trafo()`.
* Construction of the five built-in `Domain` types now takes a conservative
  registered C fast path. R still captures constructor expressions and
  dependency language objects, while unsupported or malformed inputs retain
  the historical validation path and diagnostics. Opaque `default` and `init`
  promises are retained at their historical forcing points, after earlier
  argument and dependency-expression failures. ParamSet subclasses retain the
  R constructor path because overridden `add_dep()` methods can observe the
  transient Domain-shaped parameter table during initialization.
* Built-in Domain validation, sanitization, and quantile mapping now use
  native kernels for canonical inputs. Unknown Domain classes and uncommon R
  object shapes continue through the established S3 methods. New Domain
  classes remain extensible through S3; replacing paradox's own methods for a
  built-in `Param*` class is not an extension boundary and may be bypassed by
  the native kernel.
* Began the Paradox 2 native rewrite. Built-in `ParamSet` static properties are
  now computed by registered C17 routines without dispatching through
  data.table; unknown third-party Domain classes retain an R/S3 fallback.
* `ParamSet$ids()` filtering now runs in registered native code, preserves
  parameter order, validates `class`, `tags`, and `any_tags` promises in their
  historical sequence, and avoids checkmate and data.table on this frequent
  path.
* Exact base `ParamSet` and `ParamSetCollection` `$get_values()` calls now use
  one native operation after forcing `type` and `check_required` in their
  established order. Values and dependencies retain their separate snapshots;
  dependency conditions run row by row with live S3 dispatch, TuneToken and
  required filtering retains its historical priority, and final class/tag
  promises remain late and sequential. Callback reentry, in-place dependency
  mutation, and R copy-on-write detachment of the local values shell are
  preserved. Subclasses, custom Domains, changed wrappers, unsupported
  encodings, and malformed state fall back before callbacks. Custom Condition
  methods receive the same values, dispatch, order, and error/side-effect
  priority, but callback-language introspection (`substitute()`, `sys.call()`,
  and `parent.frame()`) sees the native generic call rather than the former R
  loop frame; subclasses retain that uncommon introspective behavior.
* Public value mutation now uses registered native routines for plain-list
  merging, authenticated validated assignment and parameter-ordered storage on
  exact base `ParamSet`s, and child-distribution planning for exact
  `ParamSetCollection`s. Insert/replace `NULL` rules, ordering, explicit empty
  shapes, sanitization, atomic validation failure, and public child-subclass
  dispatch remain compatible. Replaced, reparented, active, delayed, custom,
  or malformed state conservatively executes the established R path before
  callbacks.
* Construction of `ParamSet`s from canonical built-in Domains now assembles
  the permanent parameter, tag, transformation, requirement, and initial-value
  state in C. Custom Domain classes retain the established R/data.table path.
* Construction of exact base `ParamSetCollection`s now assembles the parameter,
  tag, transformation, and name-translation tables in one registered C pass.
  Parameter order, prefix/postfix naming, duplicate tags, opaque leaves, child
  references, and live child state retain their established behavior. Custom
  classes, unsupported encodings, active or delayed private stores, malformed
  state, and duplicate translated IDs retain the complete historical R path
  and diagnostics.
* Common scalar `ParamSet$check()` and column-oriented `ParamSet$check_dt()`
  calls now validate canonical built-in values in one native pass. Special
  values, TuneTokens, callbacks, dependencies, constraints, and diagnostic
  failures deliberately fall back to the compatible R implementation.
* `ParamSet$qunif()` now maps canonical built-in matrix slices in one native
  column-wise pass. It preserves requested column order and logical, integer,
  double, and character storage, including zero-row results. Data frames retain
  their established validation and matrix normalization; selected custom or
  utility Domains and unsupported storage shapes retain grouped S3 dispatch.
* `ParamSet$get_domain()` and `$domains` now reconstruct canonical built-in
  Domain views directly from ordinary private state. Collection callbacks keep
  their historical order and are isolated from native snapshots so legitimate
  child extensions cannot invalidate cached R objects during allocation.
  Callback-capable requested IDs are left entirely to the R fallback, and a
  collection callback is never replayed after a later state-corruption error.
* Exact base `ParamSetCollection$domains` calls now preflight the complete
  nested object graph without callbacks, snapshot all permanent rows, and read
  live values once before live dependencies once. Shared-child DAGs, nested
  prefix/postfix names, all dependency rows, explicit `NULL` values, output
  ownership, and post-callback tags and transformations remain compatible.
  Subclasses, replaced generated R6 wrappers, custom Domains, and malformed
  graphs fall back before callbacks; cycles and callback-corrupted state raise
  deterministic errors without replaying side effects.
* Exact base `ParamSetCollection$values` calls now preflight the complete
  nested graph without invoking child bindings and assemble the result in one
  registered C pass. Prefix and postfix names, nested collections, named empty
  results, current leaf values, explicit `NULL`, sibling DAG reuse, and shallow
  sharing of opaque leaves retain their established behavior, while every
  result and names shell is independently owned. Subclasses (including
  `ParamSetShadow`), replaced or reparented R6 wrappers, delayed/custom
  bindings, unsupported encodings, custom Domains, and malformed state fall
  back before callbacks; a cycle on the current path raises a deterministic
  error.
* Exact base `ParamSetCollection$deps` calls now preflight the complete nested
  graph and aggregate dependency rows in one registered C pass. Depth-first
  row order, collection-local rows, nested prefix/postfix translation,
  duplicates, dangling strings, higher-layer coincidental remapping, shared
  sibling DAGs, output ownership, and Condition duplication remain compatible.
  Subclasses, custom Domains, replaced generated methods, promises, malformed
  state, and non-ASCII or bytes-encoded names retain the established R path
  before callbacks; cycles raise a deterministic error instead of entering the
  recursive fallback.
* Exact base `ParamSet` `$params` calls now construct the complete enriched
  table—including tags, transformations, dependencies, and explicit `NULL`
  initial values—in one registered C pass. Returned tables own their shells and
  metadata while retaining the historical shallow sharing of opaque leaves.
* Exact base `ParamSetCollection` `$params` calls now preflight the complete
  nested collection graph and construct static snapshot columns in native C,
  then read live dependencies and values once in their established order.
  Prefix/postfix translation, nested and empty collections, duplicate
  dependency last-match behavior, clone/serialization semantics, output
  ownership, and temporary data.table updates remain compatible; extensions
  and malformed graphs retain the R path before callbacks. Generated R6
  bindings (including the inherited tags binding and superclass proxy) are
  verified before admission, dangling callback-produced dependency rows retain
  data.table join behavior, and a callback-invalidated snapshot is never
  silently retried through R with its side effects repeated.
* `ParamSet$subset()` now slices canonical state and transfers it through an
  authenticated, single-use native plan. The same transfer now admits exact,
  callback-free `ParamSetCollection` graphs by taking authenticated native
  dependency and value snapshots; collection subsets and `$flatten()` avoid
  rebuilding their tables through data.table. Ordering, duplicate requests,
  prefix/postfix and nested IDs, dependencies, values, tags, transformations,
  detached callbacks, and nested leaf sharing retain their established
  behavior. Exact graphs with no constraints, extra transformations, or
  internal-tuning cargo also skip the formerly unconditional R traversal.
  Generated constraint and transformation bindings are authenticated before
  private feature discovery; replacements, extensions, and malformed state
  fall back as a unit.
* `generate_design_random()` now reduces canonical built-in `ParamSet` and
  `ParamSetCollection` generation to one deep clone, one column-major uniform
  draw, one native bulk quantile mapping, and one `Design` construction. Values,
  RNG state, validation order, dependencies, and fixed parameters retain
  `SamplerUnif` behavior; subclasses, custom Domains, and zero-dimensional
  spaces retain the complete sampler path. Exact generated sampling methods
  and active bindings are authenticated recursively before admission;
  replaced, reparented, or delayed wrappers fail closed without being forced.
* Native routines use forced symbol registration, the strict R headers, long
  vector lengths, defensive canonical-storage checks, and interruptible loops.
  Native data.table-shaped results install a valid public object-level
  self-reference without calling data.table code, so immediate `set()` and
  `:=` use remains safe and warning-free.
* Paradox 2 requires R 4.3 or newer so source builds can select C17 portably on
  CRAN's Unix and Windows toolchains.
* Version-dependent R access is centralized behind a documented public-C-API
  facade. R 4.3--4.5 delegate fast paths requiring exact, non-forcing binding
  classification to the established R implementation; R 4.6 uses the public
  binding API. A raw-token release gate prevents legacy R internals from
  re-entering shipped C, including through inactive compatibility branches.

## Compatibility and correctness

* Preserve the established R6 classes, public bindings, serializable ordinary-R
  state, data.table-shaped views, clone/reference behavior, and the private
  layouts used by important mlr3 ecosystem consumers.
* Empty settings now honor `presence = "all"` and `presence = "required"`.
* Double Domains with one-sided or fixed infinite bounds now apply tolerance
  coherently. In particular, zero tolerance and fixed `-Inf`/`Inf` points no
  longer create `NaN` comparison bounds.
* `domain_qunif()` now rejects input lengths that cannot be distributed over
  the supplied Domain rows.
* Child transformations in a `ParamSetCollection` run exactly once and receive
  the supported callback signature. Child constraints receive only their
  unprefixed child values, and strict collection checks no longer skip those
  live child constraints. The inherited `$has_constraint` flag now reports
  those live child constraints as well.
* Value assignment through a `ParamSetCollection` now dispatches through the
  public value binding of child subclasses. This fixes silent no-op assignments
  to miesmuschel `ParamSetShadow` children while retaining the private fast path
  for exact base `ParamSet` and `ParamSetCollection` objects.
* ParamSet-based TuneToken plausibility checks are deterministic and restore the
  caller's exact random-number state, including an initially absent
  `.Random.seed`.
* Grouped double-parameter sanitization now clamps every value against its own
  bounds without recycling warnings.
* Zero-tolerance and fixed infinite double bounds no longer produce `NaN`
  while expanding their accepted interval. Domain, scalar ParamSet, and
  tabular checks now agree and accept the represented infinite endpoint.
* `ParamSet$ids(tags = character())` now returns `character(0)` rather than
  `NULL`; overlapping `any_tags` matches no longer duplicate IDs or reorder
  them.
* `ParamSet$subset()` now supports arbitrarily repeated requested IDs even when
  those parameters have multiple tags; the former data.table join could abort
  with an unrelated Cartesian-product limit.
* Empty dependency tables are now owned by each `ParamSet` and
  `ParamSetCollection` instance. Optional data.table indexes can no longer
  leak through the shared R6 class default and make equality or tests depend
  on the order in which unrelated parameter sets were used.

# paradox 1.0.1-9000

* `ParamSetCollection$flatten()` now detaches `$extra_trafo` completely from original ParamSetCollection.
* Option to postfix, instead of prefix, in `ParamSetCollection`, `c()`/`ps_union()`, and `ps_replicate()`.
* Add `presence` argument with options `"all"`, `"required"`, and `"none"` (default) to `ParamSet$check()`, `$test()`, `$assert()`, `$check_dt()`, `$test_dt()`, and `$assert_dt()` to optionally check that all or all required parameters are present in the parameter set, except for parameters with unsatisfied dependencies.

# paradox 1.0.1

* Performance improvements.

# paradox 1.0.0

* Removed `Param` objects. `ParamSet` now uses a `data.table` internally; individual parameters are more like `Domain` objects now. `ParamSets` should be constructed using the `ps()` shorthand and `Domain` objects. This entails the following major changes:
    * `ParamSet` now supports `extra_trafo` natively; it behaves like `.extra_trafo` of the `ps()` call.
    * `ParamSet` has `$constraint`
    * `ParamSet` objects are now less mutable. The only properties that can be changed are `values`, `tags`, `deps`, `constraint` and `extra_trafo`.
    * `ParamSet$is_bounded` is a vector with an entry for each parameter. Use `$all_bounded` for the previous behavior.
    * `Condition` objects are now S3 objects and can be constructed with `CondEqual()` and `CondAnyOf()`, instead of `CondXyz$new()`. (It is recommended to use the `Domain` interface for conditions, which has not changed)
    * `ParamSet` has new fields `$is_logscale`, `$has_trafo_param` (per-param), and `$has_trafo_param` (scalar for the whole set).
* Added a vignette which was previously a chapter in the `mlr3book`
* feat: added support for `InternalTuneToken`s

# paradox 0.11.1

* Minor bug fixes.

# paradox 0.11.0

* feat: The function `generate_design_sobol()` generates a space-filling Sobol sequence design.
* refactor: `$set_values` returns the parameter set invisible.

# paradox 0.10.0

* Reset `.has_extra_trafo` to `FALSE` when trafo is set to `NULL`.
* `rd_info.ParamSet` collapses vector with `"\n"` due changes in roxygen 7.2.0
* Add method `set_values()` to conveniently add parameter values.

# paradox 0.9.0

* Added `default_values()` function to extract default values from `ParamSet`
  objects.

# paradox 0.8.0

* Parameters now have a new (optional) field `description`.
* Improved printing of parameters in documentation (#355).
* A warning is now signaled if the package `ParamHelpers` is also loaded.
* Fixed some links.

# paradox 0.7.1

* `Sampler1D` also accept `ParamSet`s with one `Param` now (#335).
* Fixed sampling zero rows in `Sampler1DRfun` (#338).
* `to_tune()`, `p_dbl()`, and `p_int()` accept `logscale` argument for tuning on
  a logarithmic scale.
* `to_tune` can be called with only `lower` or only `upper` now and will infer
  the other bound if possible.

# paradox 0.7.0

* `ParamSet$get_values()` checks whether all required parameter values are set.
  Required parameter are not checked anymore when new values are added to the
  parameter set.
* `ParamSet$check_dt()` accepts `data.frame`s.
* Rename `is_numeric` and `is_categorical` to `all_numeric` and
  `all_categorical`.
* Rename `requires` to `depends`.

# paradox 0.6.0

* `ps()` shortcuts for `ParamSet` construction, with new `Domain` construct and
  constructors `p_dbl`, `p_int`, `p_lgl`, `p_fct`, and `p_uty`.
* `ParamSet$search_space()` method that constructs tunable `ParamSet` from
  `TuneToken` objects, which are constructed with `to_tune()`.

# paradox 0.5.0

* Compact in-memory representation of R6 objects to save space when
  saving objects via saveRDS(), serialize() etc.
* Improved performance for `ParamSetCollection`.

# paradox 0.4.0

* New public methods `is_numeric()` and `is_categorical()` for parameter sets.
* Fixed a test for upcoming release of `data.table()`.
* Added a helper function to format parameter sets in Rd files.

# paradox 0.3.0

* New function `transpose()` converts `data.table` of parameter values to a list
  of lists.
* New methods `ParamSet$check_dt()`, `$assert_dt()` and `test_dt()` can check a
  `data.table` for valid parameter values.
* Documentation updated.
* Unified style for object printers.

# paradox 0.2.0

* Fixed warnings about partial argument matching.
* Enforce integer bounds in ParamInt (#258).
* Reexport `data.table::as.data.table()`.
* Deep cloning of `ParamSet$values` (#273).

# paradox 0.1.0

* Initial release.
