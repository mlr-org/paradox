# paradox 2.0.0

This is a major native rewrite focused on speed, memory safety, and a simpler
maintainable extension boundary. Ordinary documented Paradox use remains
compatible; code that mutates private R6 state or registers new Domain/
Condition implementations must migrate.
The stricter structural-container boundary is deliberately part of this major
release: supporting exotic ALTREP/S4 shells or parallel fallback admission has
no known maintained use and would preserve complexity and multi-observation
hazards that Paradox 2 is intended to remove.

## Native state and execution

* `ParamSet`, `ParamSetCollection`, and the new exported `ParamSetShadow` keep
  their serializable R6 public shells while using an opaque versioned native
  capsule. The three node kinds are base sets, collections, and live shadows.
  Shared collection graphs are supported and cycles are rejected. The public
  `assert_values` flag remains the sole stateful shell policy outside the
  capsule and selects checked versus unchecked native value storage.
* Performance-sensitive constructors, checks, value access/mutation, Domain
  operations, collection traversal, designs, and samplers enter registered
  portable C17 operations directly. Current objects have one semantic engine;
  operations are not retried through a second R/checkmate/data.table/S3 path.
* Checked assignment of a ParamSet-bearing `ObjectTuneToken` now accepts only an
  exact nonempty bounded BASE `ParamSet` capsule, not a collection, shadow, or
  additive subclass, and executes no candidate callback during admission.
  It retains a rooted candidate-generation receipt through all callback and
  allocation work and performs one final allocation-free reauthentication
  immediately before commit. A callback/finalizer mutation therefore wins and
  the outer assignment errors without storing anything.
  Deterministic `$search_space()` construction is the sole boundary that runs
  its transformation and checks one-dimensional target compatibility. Thus a
  structurally valid but output-incompatible candidate errors when the search
  space is requested rather than during assignment; corrupt candidates still
  fail atomically before storage. Before that conversion invokes R, the native
  boundary replaces each live candidate with a sealed, single-use BASE subset
  capability, so conversion never calls or rereads the original shell.
* TuneTokens now have one closed package-defined representation and native
  exact-shape boundary. Supported tokens have the exact Full, Range, Object,
  Internal-Full, and Internal-Range forms produced by `to_tune()`, each with
  exactly `{content, call}` and its built-in class/content shape. Hand-built or
  copied tokens that are structurally indistinguishable are not authenticated
  by creator provenance; callers should still use `to_tune()` because the
  representation is not an API. Subclasses, extra/reordered fields, classes or
  attributes, S4 structure, malformed calls/content, and recursively attached
  metadata fail before traversal.
  Scalar names introduced by ordinary indexing are representation-only and are
  normalized away. TuneToken internals remain non-API; callers should use
  `to_tune()`.
  Object Domain content must be bounded and able to produce a value, so an
  unbounded `p_uty()` or zero-level `p_fct()` Domain is rejected as a tuning
  range. Zero-level factors remain valid for the typed empty operations below.
  Other bounded typed Domains can still retain admitted opaque leaves, and
  the exact BASE-ParamSet form can construct an opaque target value.
* Operations force documented arguments once, materialize stable semantic
  ALTREP vectors once at native admission, snapshot their state/callbacks, and
  execute callbacks exactly once. A callback's mutation of Paradox state does
  not replace the schema, dependencies, or callbacks already selected for the
  remaining rows; ordinary external callback side effects still occur in row
  order. Base compact sequences such as `1:n` remain supported. A
  state-changing custom ALTREP observed earlier by R-side language or
  representation capture has no exact value/printed-representation
  compatibility guarantee; it is rejected
  or consumed from the one native snapshot without replay. Typed Dbl/Int/Fct/
  Lgl Domain special-value leaves are deliberately narrower and reject ALTREP
  before observation. Interpreted outer general-list/internal-table/Domain/
  Condition/token/capsule shells, ParamSet `params` lists, non-table
  transformation inputs and all transformation result shells, Domain cargo/
  interpreted cargo entries, rows, dimnames, class/name vectors, and other list
  metadata must be ordinary non-ALTREP and non-S4 structure. The six documented
  public data.frame/data.table ingresses now share one strict structural
  classifier. A well-formed ordinary class vector may have additive leading
  classes before a terminal `"data.frame"` or
  `c("data.table", "data.frame")` suffix. Native admission treats those
  leading classes as representation only and never dispatches through them.
  The classifier does not copy or materialize an ordinary shell merely to
  remove the prefix, and native semantic snapshots ignore it; an
  already-required ALTREP shell snapshot installs only the canonical suffix.
  Malformed, reversed, non-suffix,
  reserved-label, and duplicate class vectors reject. Table names/classes and
  accepted data.table cache carriers are ordinary, and cache attributes are
  ignored rather than interpreted. Raw row names must be
  attribute-free, nonobject, non-S4 integer or character vectors; compact
  positive/negative counts are decoded, and stable row-name ALTREP is observed
  once for length without reading labels. Row-consuming operations compare the
  count with their columns. Direct transformation and dependency planning with
  no edges avoid extra column observations for a dimension they do not use.
  The narrow top-shell exception materializes a suffix-classified,
  allowed-attribute VECSXP ALTREP once after owning names/classes; base R's lazy
  attribute-copy duplicate is the common motivating case. Semantic atomic
  columns may be stable ALTREP. Zero-column data.frames may omit names and keep
  their row count; Design transpose returns one empty configuration per row.
  Direct checked and unchecked `$values <-`
  reject an outer ALTREP before observing it. The Paradox-1 clear-values
  spellings—`NULL`, an ordinary attribute-free zero-length atomic/expression
  vector, or an accepted empty list container—are canonicalized to a named
  native `list()`. The sole general-list exception is
  `set_values(.values=)`, which snapshots its supplied shell once before
  interpreting it.
  The outer `special_vals` list is structural for every Domain kind and follows
  the ordinary non-ALTREP/non-S4 rule; only its leaves follow the typed or
  opaque ParamUty policies below.
* Base `extra_trafo` callbacks retain unnamed list results, including the
  one-dimensional form used by `to_tune(ParamSet)`. Collection child callbacks
  require names so their output can be translated into the collection namespace.
  Transformation results and non-table input shells are ordinary
  non-ALTREP/non-S4 lists. A documented data-frame input may use the exact
  top-level ALTREP table boundary above; admitted semantic atomic leaves and
  columns may still be stable ALTREP.
* Live collection callbacks and the detached callbacks produced by subset,
  flatten, or a Shadow over a collection now use one registered native
  evaluator family with shared semantic helpers. Their thin R closures contain
  no duplicate callback selection, translation, merge, or
  constraint-validation engine and do not dispatch through overridden child
  ParamSet methods. Retained/untransformed inputs remain in input order,
  followed by changed child outputs in callback-plan order; omitted child
  outputs are removed.
* Removed the dormant, unexported namespace-level R `transpose()` engine and
  the unused `col_to_nl()` and `rbindlist_proto()` helpers. `Design$transpose()`
  has one registered native implementation; these internals had no callers in
  Paradox or the maintained/downstream corpus and are not compatibility APIs.
* `ParamSet$subset()` now has an additive final `keep_trafo = TRUE` argument,
  also supported by collections and shadows. `keep_trafo = FALSE` removes both
  per-parameter transformations and `extra_trafo` in the native subset
  transaction while preserving the independently selected constraint. This is
  the supported way for consumers such as mlr3mbo to derive an untransformed
  search space without mutating private Domain tables. Subset control flags are
  unclassed, attribute-free logical scalars; collection callback detachment no
  longer reinterprets them through R's generic `!`.
* `ParamSet$check_dependencies()` now enters the same native graph/point/
  dependency kernel as `$check()`. It accepts an ordinary uniquely named base
  list, skips TuneToken dependency edges, diagnoses unknown IDs even when no
  dependency rows exist, and returns the first diagnostic instead of building
  and newline-collapsing every error through data.table and `pmap()`.
* `condition_test()` now enters the closed built-in Condition comparator
  directly. It supports `NULL` and plain logical, integer, double, or character
  vectors, preserves names, and materializes stable ALTREP inputs once.
  Classed, dimensional, or otherwise attributed operands now fail explicitly
  instead of selecting `Ops`/`%in%` S3 behavior. Built-in Condition RHS values
  use the same four unclassed types without missing values; they are rooted and
  materialized once when a dependency or direct comparison admits them.
* `ParamSet$test_constraint()` and `$test_constraint_dt()` now share the native
  check graph, point admission, and constraint kernel. With value assertion
  enabled, the table method validates every row before invoking any constraint
  callback, then calls the operation's snapshotted constraints once per row;
  reentrant mutation affects only later public operations. ParamUty custom
  checks may still run during the preceding Domain-value validation phase.
* Tag access/replacement, dependency snapshot/access/replacement/append, and
  BASE constraint/extra-transformation callback replacement are native capsule
  operations. Bulk dependency assignment is a callback-free structural
  snapshot and preserves predicates made partially or wholly infeasible by
  parent-Domain narrowing. `$add_dep()` remains the stricter authoring API: RHS
  feasibility uses the shared check kernel, callback reentry is
  generation-checked, and Shadow append routes to the origin only when both
  endpoints remain visible.
* `$has_deps` is now a native scalar read. BASE and live SHADOW nodes validate
  their canonical dependency state directly; COLLECTION nodes retain complete
  graph admission and use its subtree count. The flag no longer constructs a
  detached dependency table or data.table facade solely to test whether it is
  empty.
* `ParamSetCollection$add()` now validates the complete current and proposed
  child graphs—including Shadow origin edges—in one native transaction. It
  rejects existing/proposed cycles, corruption, collisions, and reentrant
  graph changes before atomically installing the replacement generation.
* Exactly two narrow cold R semantic-orchestration families remain, neither as a
  fallback. Internal-tuning aggregation, disabling, internal search-space
  conversion, and post-flatten cargo rebinding form the first because
  their documented payload is lexical R callbacks. They capture required
  cargo/translation/Domain/owner-value state before callbacks and commit only
  through native mutation; they are not fallback engines.
  Exact-TuneToken `$search_space()` conversion is the second: it consumes one
  rooted native token/target-Domain snapshot whose live BASE candidates have
  already become sealed one-use capabilities, switches only over the package's
  built-in token kinds, and owns callback-dependent one-dimensional output
  compatibility without another native/R conversion path.
* A BASE-origin Shadow constraint uses an exact two-field callback/hidden-value
  plan and a thin native evaluator. Hidden and visible values are merged
  manually without `c.*` dispatch, opaque leaves retain identity, the callback
  runs once, and its result must be one non-missing logical value.
* Internal tables are canonical base data.frames. data.table >= 1.18.4 is used
  only for independently owned outward-facing facades; returned tables remain
  safe to mutate with normal data.table operations without changing the
  ParamSet. Documented data.frame/data.table operation inputs use the shared
  suffix-aware classifier and snapshot an allowed top-level VECSXP ALTREP once.
  Package-owned table/facade metadata remains canonical ordinary structure;
  admitted semantic atomic columns may be stable ALTREP.
* Numeric/list-valued `p_fct()` and log-scale `p_int()` create their small
  serializable mapping closures directly instead of compiling a fresh
  `crate()` closure for every Domain instance.
* Constructor final-state checking, ParamSet construction, and ObjectTuneToken
  Domain admission now share one canonical native built-in Domain-row owner.
  Malformed kind/storage, cargo, grouping, bounds, levels, default, tags,
  requirements, initialization, and special-value/transformation combinations
  therefore reject consistently without duplicate token-specific rules.
  Structural Domain, Condition, TuneToken, and ParamSet metadata now rejects
  ALTREP and S4 explicitly. A typed S4 special leaf is an opaque identity token:
  it, and a typed S4 default/init, matches only the pointer-identical admitted
  special value. ParamUty values/defaults/initial values/special leaves remain
  opaque and may be S4; Paradox-1 special membership is preserved exactly with
  base `identical()` and performs no S3/S4 dispatch. Malformed exact-token or Domain
  structure raises a hard boundary error, while an ordinary infeasible value
  retains the normal character check diagnostic.
* The wrapper used for a user-supplied `to_tune(ParamSet)` transformation now
  performs its single-list-result check and output naming directly, avoiding
  checkmate/mlr3misc dispatch on every callback execution.
* Live Shadow state validation uses a temporary native ID index instead of
  repeatedly scanning all origin IDs. It keeps the encoding-correct slow case
  and complete corrupt-state validation while making ordinary reads and writes
  substantially faster.
* Collection validation compares equal-encoding UTF-8/Latin-1 strings and
  native ASCII IDs without repeated transcoding. Mixed encodings and non-ASCII
  native strings keep the translating path, and every read still performs the
  complete corrupt-state validation.
* A final measured hot-path pass skips empty constructor value transactions,
  reuses already resolved BASE rows while translating admitted collection
  values, and removes a redundant R-side Shadow dependency refresh. Paired
  forward/reverse development measurements improved small construction by
  about 4--7%, bulk construction by 8--12%, rich collection reads by 5%, and
  nested reads by about 18%; plain reads remained within timer noise.
* Static ParamSet properties return directly from the closed native kind
  switch; the former allocation of a compatibility mask and grouped S3 replay
  path have been removed.
* `SamplerUnif` and `generate_design_random()` now share one capsule-driven
  uniform engine for base sets, collections, and live shadows. The inherited
  `$samplers` list remains descriptive; replacing/reordering it is an error,
  and custom executable child samplers belong in `SamplerHierarchical`.
* The package now requires R >= 4.3 and a C17 compiler. Linux, Windows x86-64,
  and Apple-silicon macOS are supported without architecture-specific code.

## Public model and migration

* Ordinary non-ALTREP named configuration lists may retain an outer S3 class
  when assigned through `$values`; Paradox ignores and removes that container
  class instead of using it for dispatch. Checked and unchecked direct
  assignment reject an outer ALTREP before observation and canonicalize an
  accepted empty shell to native `list()`. This keeps ordinary classed controls
  interoperable without reopening the removed S3 extension engine. Explicit
  `$search_space(values=)` has the same ordinary-or-representation-only-S3
  named-list boundary and selects tokens natively without `[` dispatch;
  ALTREP, S4/list-like, or otherwise attributed containers reject. Only
  `set_values(.values=)` has the documented one-snapshot outer-list ALTREP
  boundary.
* Names attached to scalar Domain bounds, tolerances, tags, and constructor
  flags by ordinary R indexing are treated as representation metadata and
  removed from the canonical Domain row.
* `all.equal()` now compares a detached ParamSet-family semantic graph: class,
  validation mode, params, values, tags, dependencies, BASE callbacks,
  COLLECTION children, complete SHADOW origins, and canonical shared-node
  topology. Independently built equivalent DAGs compare equal, while a shared
  node and two duplicated nodes differ. In particular, comparing two
  `ParamSetCollection`s no longer evaluates an inherited BASE active binding
  and errors on the COLLECTION capsule.
  Equality of a larger third-party R6 object graph remains that package's
  responsibility; downstream fidelity tests should project documented public
  state instead of recursively comparing Paradox private environments.
* `ParamSetShadow$new(set, shadowed)` is now provided by Paradox. It exposes a
  fixed visible schema with live origin values, dependencies, constraints, and
  transformations. Visible assignments write through while preserving hidden
  values, and dependencies crossing the shadow boundary are rejected. A direct
  Shadow origin is rejected; combine hidden IDs over its BASE/COLLECTION origin.
  Construction enters C directly and keeps no duplicate visible/hidden schema
  in R6 private fields.
* Third-party subclasses of the ParamSet family may call `super$initialize()`
  and add nonconflicting behavior. Overriding ParamSet core methods/active
  bindings, replacing generated wrappers, or reading/writing private capsule
  state is no longer supported. The documented Sampler subclass API remains.
  miesmuschel can use the official shadow class; bbotk uses the public
  collection `$sets` accessor and retains additive `Codomain` inheritance.
  This additive support does not extend to ParamSet content inside an
  `ObjectTuneToken`, which is BASE-only. A shell alias retaining the exact
  genuine BASE private/core linkage may be indistinguishable and pass safely;
  native token operations use the core and never invoke alias methods.
* Domain execution is closed over `ParamDbl`, `ParamInt`, `ParamFct`,
  `ParamLgl`, and `ParamUty`. `p_uty(custom_check=)` remains the supported
  general validation escape hatch. Registering third-party `domain_*` S3
  methods is no longer an extension contract.
* Numeric Domain bounds and logscale normalization now enter the row
  constructor once. Empty Domain operations and zero-dimensional grids also
  enter their native engines instead of taking R-side special cases.
* Dependency Conditions are closed over `CondEqual` and `CondAnyOf`. Their
  exported constructors, `$new()` adapters, list/class shape, mutable `rhs`,
  formatting, and serialization remain. Unknown Condition classes are rejected
  when a dependency is added instead of being dispatched later.
* Current Paradox 2 objects serialize normally. Objects serialized by Paradox
  1.x must be passed explicitly to `upgrade_paradox_object()` after loading.
  The upgrader does not mutate or execute the legacy object, preserves valid
  shared graphs and callbacks, and rejects cycles, malformed private state,
  unknown extensions, and core-overriding subclasses. Downstream packages own
  migration of their legacy third-party subclasses.
* Documented Paradox validation messages and deliberately maintained
  package-owned fragments remain informative and stable. Exact checkmate-era
  wording asserted only by downstream tests is not a compatibility promise and
  may use a Paradox-major-gated expectation. Implementation call frames,
  side-effecting promise quirks, generated-closure layout, and exotic ALTREP
  multi-observation behavior are likewise not compatibility promises.

## Correctness fixes

* Converting a Domain's current `to_tune()` value into a search space no longer
  copies that TuneToken back as a fixed design value. Exact native clones now
  preserve ordinary fixed values without making a FullTuneToken's two fields
  a length-two assignment to every generated row.
* Empty settings honor `presence = "all"` and `"required"`.
* `domain_qunif()` rejects incompatible input dimensions.
* Collection child transformations run exactly once and child constraints
  receive the correct unprefixed values; strict checks consult live child
  constraints.
* Collection assignment reaches live child state, including a shadow's origin.
* ParamSet quantile and grid kernels now consume canonical plain capsule tables
  on every supported R release, including R 4.3/4.5. Zero-axis grids retain a
  typed empty result; this includes zero-level factor Domains, whose empty
  quantile maps and categorical/mixed grids retain `character(0)` columns.
  Zero-row uniform sampling does the same. A nonempty quantile map or
  positive-row uniform sample against a zero-level factor errors informatively
  before consuming RNG state.
  Out-of-range infinite integer mappings warn once and produce `NA_integer_`
  inside the native engine.
* Deep cloning now preserves shared COLLECTION/SHADOW graph identity and
  rebuilds Shadow constraint adapters from the cloned origin; it no longer
  independently clones the same node once per incoming edge.
* Callback-dependent TuneToken search-space plausibility sampling is
  deterministic and restores caller RNG kind/state. Native exact-token
  admission itself does not sample or execute candidate callbacks, and Domain
  candidates use the shared canonical built-in row owner rather than a
  reclassified internal table row.
* `ids(tags = character())` returns `character(0)` and overlapping `any_tags`
  matches are deduplicated in parameter order.
* Grouped numeric sanitization uses each parameter's own bounds, and
  one-sided/fixed infinite domains avoid accidental `NaN` results.
* Repeated subset IDs no longer depend on data.table join-size heuristics.
* `ParamSet$set_values(.insert=)` now requires exactly `TRUE` or `FALSE`
  instead of relying on R's length/coercion quirks, and value-list merge
  validation is performed once in native code without redundant checkmate
  scans.
* Checked value assignment is now one graph-wide transaction across base
  sets, collections, and shadows. Shared ultimate targets are updated once
  with deterministic last-owner semantics; validation or callback failure
  leaves every target unchanged; and a nested callback assignment wins while
  the outer assignment raises before committing anything. Hidden Shadow
  values remain intact.
* Empty dependency state is object-local and cannot be contaminated through a
  shared mutable table.
* Malformed/corrupt capsules, tables, graphs, callbacks, and direct native
  inputs produce deterministic errors rather than fallback replay or unsafe
  memory access.

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
