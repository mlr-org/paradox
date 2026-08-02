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
  portable C99 operations directly. Current objects have one semantic engine;
  operations are not retried through a second R/checkmate/data.table/S3 path.
  Simple Domain IDs use the same bounded native representation renderer on
  R 3.6 and current R. Before R 4.5 its `scipen` snapshot uses public
  `base::getOption()`; newer R uses documented `Rf_GetOption1`. Ordinary old-R
  constructors no longer fall back unconditionally to `deparse1()`, while
  unsupported or unstable representations retain that correctness fallback.
* Invalid `aggr`, `in_tune_fn`, and `disable_in_tune` constructor arguments
  are diagnosed by argument name, uniformly for all five Domain constructors,
  including the Paradox-1 internal-tuning pairing messages (tag required,
  both-present, aggregation function required).
* Unknown parameter diagnostics again include a native
  `"Did you mean ...?"` hint when a close ID exists. Paradox 1 accidentally
  ranked the numeric position of the unknown entry instead of its name; the
  native matcher now uses the actual misspelling, retains the established
  case-insensitive partial-edit threshold and three-candidate limit, and
  therefore discounts prefixes introduced by `ParamSetCollection`. Suggestion
  work runs only after the exact ID lookup has failed.
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
  the exact BASE-ParamSet form can construct an opaque target value. The
  printable Domain `repr` carrier is itself ordinary non-ALTREP/non-S4 but is
  retained as one exact opaque presentation identity; this keeps documented
  function-valued factor tokens assignable without weakening generic
  data-table metadata validation.
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
  opaque ParamUty policies below. A typed special-value leaf is a semantic
  value rather than an identity token, so a stable atomic ALTREP spelling of
  one—`special_vals = list(1:5)`, a deferred string conversion, and the other
  ordinary base-R representations—is admitted at construction and materialized
  once into an ordinary vector, exactly as a `default` or initial value is. The
  Domain that results is identical to one built from the materialized twin.
  Leaves the value owner would not materialize, namely S4 and non-atomic ones,
  keep their identity contract and stay rejected. Operation-time masked
  admission still rejects every ALTREP leaf structurally, without observing an
  element: a constructed Domain stores the materialized copy, so an ALTREP leaf
  in a live table means the column was written by reference afterwards.
* Every public operation on a typed built-in Domain validates the exact
  complete sixteen-column outward shell, including typed zero-row and
  empty-value exits; the canonical zero-column empty Domain retains its
  dedicated exact validator. The identity spine is always interpreted, while
  each operation declares only the bounds, levels, special values, cargo,
  tags, and transformation rules it consumes. One native closure function
  expands rule dependencies, `domain_check()` requests every rule, and no
  operation restates a rule locally. Thus malformed semantic state outside an
  operation's closed mask is reported by the first operation that interprets
  it. Constructor-owned `default`, requirement, and initialization contents
  stay opaque at this public boundary, but their columns must have canonical
  presence, uniqueness, storage type, and row count. `domain_check()` and
  `domain_qunif()` perform a bounded shape probe before an observable ALTREP
  Length callback and one complete masked admission afterward; quantile
  mapping observes that Length exactly once, so it cannot combine
  pre-callback Domain metadata with a post-callback table generation.
  Operations that complete and root admission before a later value-side
  callback, including `domain_sanitize()`, retain that already coherent
  snapshot.
* The printable Domain `repr` carrier's shape rule has one owner. Its content
  stays an opaque print-only payload, but an ALTREP or S4 carrier is rejected
  by every Domain operation and by `ParamSet` construction alike, rather than
  only by construction.
* Structural defects of a Domain are reported as corrupt storage rather than
  as change during admission. An ordinary `row.names` carrier that disagrees
  with the admitted row count, or that is not an ordinary integer/character
  vector, now reports `Corrupt Domain storage`, and the `id` column takes the
  same diagnostic-rich route as the other fifteen columns. `Domain changed
  during admission` is retained for divergence detected after a window in
  which change was possible.
* A Domain carrying data.table's `index` or `sorted` cache attribute -- which
  ordinary filtering or keying installs by reference -- is rejected with a
  message naming that attribute and how to clear it.
* `domain_qunif()` on a zero-row Domain validates `x` on the same rule as a
  nonzero Domain and returns the Domain kind's mapped empty vector rather than
  `logical(0)`; a zero-row `ParamUty` Domain reports the same undefined-mapping
  error as a nonzero one.
* The numeric Domain capsule's bounds and tolerance rule has one spelling
  shared by row admission, capsule validation, and quantile mapping. A capsule
  whose integer tolerance exceeds `0.5`, or whose integer bounds are neither
  integer-valued nor infinite, is rejected by `$qunif()` and grid generation as
  well as by `$check()`.
* Names, classes, and other closed ASCII labels are compared by bytes, so a
  `bytes`-encoded spelling of a canonical name is accepted; semantic equality
  of arbitrary strings such as parameter IDs never equates a `bytes` string
  with another encoding. The two comparators answer different questions.
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
  ParamSet methods. Activity filtering is performed by the authoritative graph
  check/test/assignment site before these schema-free carriers run.
  Retained/untransformed inputs remain in input order,
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
  materialized once when a dependency or direct comparison admits them. Wide
  stored-value reads now use one operation-local ID index and reuse each
  dependency validator's admitted RHS, avoiding quadratic endpoint scans and
  duplicate Condition admission while retaining exact corruption checks,
  informative diagnostics, mixed-encoding matching, and detached results.
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
  generation-checked, and Shadow append routes to the origin with the
  dependent parameter visible and the parent either visible or absent from the
  origin; `allow_dangling_dependencies` decides the absent case exactly as it
  does on a plain set. Design masking and grid generation consistently
  treat an infeasible predicate or dangling parent as unsatisfied, so its child
  is inactive instead of failing in a later vectorized comparison.
* `$has_deps` is now a native scalar read. BASE and live SHADOW nodes validate
  their canonical dependency state directly; COLLECTION nodes retain complete
  graph admission and use its subtree count. The flag no longer constructs a
  detached dependency table or data.table facade solely to test whether it is
  empty.
* A `ParamSetCollection`'s flattened schema and a `ParamSetShadow`'s visible
  schema are now live views of the sets they are derived from, in the same way
  their values, dependencies, and callbacks always were. Adding a parameter to
  a contained set, or assigning tags to it, is reflected by every containing
  collection and by any shadow over it, on every read surface -- `$ids()`,
  `$params`, `$tags`, `$lower`, `as.data.table()`, `$check()`, `$values`,
  `$qunif()`, `$subset()`, and the design and sampler entry points. Paradox 1
  left the flattened tables of an outer collection stale in this situation
  while its `$values` already reported the new parameter, so an outer set could
  report values under names its own `$ids()` did not contain; Paradox 2 makes
  every ancestor behave as if it had just been constructed from its current
  sets. A name collision that only a later change creates is reported when the
  affected collection is next read, naming both the set that changed and the
  colliding ID.
* Tags are the one part of a derived schema a derived set owns outright.
  `ParamSetCollection$tags<-` keeps working and now *stays* worked: the
  assignment becomes the collection's own answer for the IDs it named and
  survives every later re-derivation, while the contained sets are left
  untouched. `ParamSetShadow$tags<-`, previously read-only, works the same way,
  so two views over one set may tag the same parameter differently. An ID that
  no assignment named -- a parameter a contained set gains later, or one added
  by `$add()` -- is still derived from the sets. Upgrading a Paradox-1
  collection recovers the per-edge `tag_sets`/`tag_params` flags from the tags
  they generated so those keep being produced, and preserves any remaining rows
  that no set accounts for as the upgraded collection's own answer.
* `ParamSetCollection$add()` now validates the complete current and proposed
  child graphs—including Shadow origin edges—in one native transaction. It
  rejects existing/proposed cycles, corruption, collisions, and reentrant
  graph changes before atomically installing the replacement generation.
  Native active-path validation retains both each shell and the exact capsule
  generation selected from it in a managed root carrier. This closes a
  capsule-graph lifetime hole on R 3.6--4.1, where an optional binding lookup may
  enter the evaluator and a pending finalizer could otherwise detach an
  ancestor generation still needed by the traversal. Shared-DAG and
  active-path-cycle behavior is unchanged.
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
  plan and a thin native evaluator. The authoritative Shadow graph site filters
  hidden and visible inputs for activity before this schema-free plan merges
  them manually without `c.*` dispatch. Opaque leaves retain identity, the
  callback runs once, and its result must be one non-missing logical value.
* Internal tables are canonical base data.frames. data.table >= 1.18.4 is used
  only for independently owned outward-facing facades; returned tables remain
  safe to mutate with normal data.table operations without changing the
  ParamSet. Documented data.frame/data.table operation inputs use the shared
  suffix-aware classifier and snapshot an allowed top-level VECSXP ALTREP once.
  Package-owned table/facade metadata remains canonical ordinary structure;
  admitted semantic atomic columns may be stable ALTREP. Fresh Domain and
  BASE/SHADOW dependency results now complete their outward data.table facades
  natively instead of copying/finalizing the same new shell again in R;
  caller-owned input continues to use the defensive finalizer.
  Caller-owned attribute spines are now copied through bounded package-owned
  metadata operations instead of R's general pairlist/recursive duplicators.
  Public detached built-in metadata supports ordinary acyclic graphs up to 64
  attributes/recursive frames and 65,536 nodes, and reproduces the selected
  attribute order. Stable atomic ALTREP attribute values -- the deferred
  strings, compact sequences, and wrappers that `names(x) <- as.character(...)`
  and `attr(x, "i") <- 1:n` produce -- are materialized once into ordinary
  vectors, so such values construct, store, and migrate. Cycles, S4 nodes,
  closures/`DOTSXP` used as presentation metadata, structural
  list/expression ALTREP attribute values, overbound graphs, and ALTREP
  providers that answer differently on a second observation still reject
  cleanly; a raw spelling that public R setters would normalize now reports
  that spelling rather than a concurrent change. The already-receipted Domain
  `repr` carrier above is the sole package-defined opaque exception. Quantile
  and typed-list compatibility retain shallow nested metadata identity while
  applying the same 64-attribute top-level bound.
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
* An earlier measured hot-path pass skips empty constructor value transactions,
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
  `SamplerUnif` construction also transfers its fresh singleton subspaces
  through package-private single-use ownership carriers, removing redundant
  child clones. Ordinary public sampler constructors still defensively clone;
  malformed, reused, or serialized carriers reject and are not part of the
  public sampler topology.
* The final bounded performance pass also reduces collection-reader scratch
  for common graphs, reuses the first prior-node lookup, and completes the
  wide-getter, Condition, facade, and sampler optimizations above. These are
  operation-local ownership/lookup improvements, not persistent validation
  caches or weaker graph admission.
* Collection parameter reads now pass the already admitted, rooted core
  directly to the shared params loader. This removes a temporary environment
  allocation and redundant capsule lookup and avoids requiring the
  post-R-3.6 `R_NewEnv()` API.
* The package supports R >= 3.6 and uses portable C99. Normal current-R source
  installation selects a language dialect no later than C17, avoiding an
  implicit switch to a compiler's C23 default. The release harness separately
  verifies explicit C23 installation with GCC 15 and recent Clang; the strict
  GNU C99 gates remain unchanged. Current R releases keep their public-API fast
  paths; small version adapters cover old API spellings without a second
  semantic implementation. The reusable runtime harness now runs the complete
  package suite on R 3.6.3 and every R 4 minor through R 4.5.2: 4.0.5, 4.1.3,
  4.2.3, 4.3.3, 4.4.3, and 4.5.2. The full native lane owns the same complete
  execution on current R 4.6.1 rather than duplicating it as an eighth runtime
  stage. Strict compilation separately covers R 3.6.0, 4.0.0, and 4.2.0
  transition headers.
  The R 3.6 stage also reinstalls the candidate against every exact declared
  direct dependency floor, authenticates every installed package identity and
  dependency namespace origin, and verifies the candidate Paradox DLL origin
  and registration. A complete runtime matrix serializes a representative
  current Paradox-2 graph on R 4.0.5 and loads, mutates, and reserializes it on
  R 3.6.3.
  Retained stages authenticate detached startup, Makevars, and
  build/install/check environment inputs, so caller files or concurrent
  checkout edits cannot change a frozen candidate.
  Exact old-runtime API exceptions are centralized and count-audited. One raw
  attribute iterator remains through R 4.5. Before R 4.5, the exact ledgered
  `FORMALS`, `R_ClosureExpr`, and `CLOENV` accessors capture one coherent,
  allocation-free closure generation for callback admission and recursive
  traversal; all three compile out from R 4.5 onward. Only a directly reached
  old-runtime bytecode object uses the cold public
  `as.function.default()`/`body()` bridge.
  Callback-backed `UserDefinedDatabase` environments are rejected before
  native binding inspection; they are not supported ParamSet/R6 frames, and
  old binding helpers assume the ordinary frame layout.
  Recursive discovery now recognizes a namespace imports environment only
  when its `imports:` name and base-namespace parent agree. A user environment
  that merely has a similar name is no longer silently skipped.
  Strict native gates now compile the package as C99. Graph paths use bounded
  decimal arithmetic and diagnostic long-vector positions use exact
  `%.0f`/double formatting, so shipped native code has no `%lld`, `%I64`, or
  `j`/`z`/`t` integer-length dependency on old Windows R toolchains.
  Native ParamSet diagnostics also use bounded real-buffer formatting instead
  of `vsnprintf(NULL, 0, ...)`, preserving informative errors under
  Rtools35's historical MSVCRT behavior.
  Legacy ParamSet-family migration has one R-3.6-only limitation: because that
  runtime exposes no accessor for an active-binding function, both direct and
  recursive migration fail closed and ask the user to perform the upgrade under
  R >= 4.0. Paradox-1 ParamSet-family R6 shells themselves use active bindings,
  so their practical object/graph migration needs that newer runtime. Exact
  built-in current Paradox-2 shells remain recursively traversable through
  their authenticated native capsule. Package active facades and relocked
  method replacements are opaque on R 3.6: these unsupported replacements
  cannot be distinguished when the receipts available on that runtime remain
  intact. Their closures are not traversed, and an active binding is never
  invoked. Additive shells and modifications that fail exact authentication
  instead fail closed. Current
  operations, standalone legacy Domain/Condition conversion, and migration
  graphs without arbitrary active bindings remain supported. R 3.6 also cannot
  construct the package's list-ALTREP adversarial test fixture; list ALTREP
  does not exist there, so this does not narrow production behavior. Linux,
  Windows x86-64, and Apple-silicon macOS are supported without
  architecture-specific code.

## Dependency semantics

* Checked `$values <-` and `set_values()` assignment now accept
  Domain-valid values whose dependencies are currently unsatisfied. These
  dormant values remain in raw `$values`, are omitted by the default
  `$get_values()` view, and reactivate automatically after a later parent
  change. Dormant values still receive the same type, bounds, special-value,
  ParamUty custom-check, sanitization, TuneToken, unknown-ID, and atomic
  graph-transaction validation as active values.
* Dependency evaluation is now default-aware (#265). If an active parent is
  absent from the candidate point or stored-value basis, its recorded default
  is tested; an explicit value overrides the default, `NoDefault` remains
  unsatisfied, and a default on an inactive parent cannot activate a
  descendant. `$check()`, `$check_dt()`, `check_dependencies()`,
  `$get_values()`, and constraints share this one native activity engine.
* `presence = "all"` and `"required"` now use default-aware point activity.
  A child whose absent parent has a satisfying default is active and may
  therefore be reported missing where the former default-blind check exempted
  it.
* `get_values(check_required = TRUE)` likewise checks required parameters after
  default-aware dependency filtering. Required dormant parameters remain
  exempt, while a required parameter made active by a parent default is now
  demanded.
* Constraint callbacks now receive only the dependency-active entries of the
  configuration being validated. Collection activity is evaluated in the full
  translated namespace before each child receives an unprefixed active slice;
  Shadow activity uses complete merged origin state before its schema-free
  native merge adapter runs. Activity is selected by the authoritative
  check/test/assignment site, not reimplemented inside detached callback
  carriers. Callback count, order, snapshots, and scalar-result validation are
  unchanged.
* A legal raw value store is no longer necessarily a valid explicit point:
  `$check(ps$values)` may fail when `$values` contains dormant entries.
  Check-family methods remain store-blind, point-strict by default, and never
  fill a candidate from stored values. Use `$check(ps$get_values())` when the
  intended point is the active configuration.
* Dependency failures that still arise from strict point checking retain the
  established `"can only be set if"` diagnostic fragment. Assignment no longer
  raises that diagnostic merely for storing a dormant value; an absent parent
  whose default does not satisfy may include additional default context without
  changing the leading compatibility fragment.
* Existing BASE dependency mutation admission is unchanged and can construct a
  cycle. Every activity consumer now fails safely and deterministically on that
  cycle rather than looping, overflowing, or returning a partial active set.
* A dangling dependency -- one whose `on` names no existing parameter -- is
  resolved in one scope by every consumer. Inside a `ParamSetCollection` the
  parent is translated outward through the enclosing namespaces, exactly as
  `$deps` displays it, and then looked up in the reading set's own flat
  schema; a name no namespace supplies stays never-satisfiable. `$deps`,
  `$check()`, `$get_values()`, a child constraint's active slice, designs, and
  samplers therefore give one answer about one edge, and the Paradox-1 pattern
  in which a child declares a dependency on a sibling that is added later --
  the edge starting to be enforced once the union is complete -- keeps
  working.
* A dangling dependency produces `NA` for its child in generated designs and
  samples, where Paradox 1 failed with a raw internal assertion, and a checked
  `$values <-` stores that child as a dormant value where Paradox 1 refused
  the assignment. `$search_space()` still drops a dependency whose parent is
  not itself tuned, as in Paradox 1; `paramset_to_configspace()` now refuses
  such a set by name instead of failing with `subscript out of bounds`.

## Public model and migration

* Ordinary non-ALTREP named configuration lists may retain an outer S3 class
  when assigned through `$values`; Paradox ignores and removes that container
  class instead of using it for dispatch. Checked and unchecked direct
  assignment reject an outer ALTREP before observation and canonicalize an
  accepted empty shell to native `list()`. Unchecked assignment does not check
  values, but it does classify each leaf's class shape exactly as every reader
  does, so it can no longer commit a value -- an `NA` class label, say -- that
  makes every later read of the same object report corrupt state. This keeps
  ordinary classed controls interoperable without reopening the removed S3
  extension engine. Explicit
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
  fixed hidden set -- the visible schema is "origin minus hidden", computed
  live -- with live origin values, dependencies, constraints, and
  transformations. Visible assignments write through while preserving hidden
  values. A dependency that spans the visible/hidden boundary is rejected in
  either direction, naming both ends; a dependency on a parameter the origin
  does not have is not such a crossing and is shown and enforced exactly as
  the origin does, so a view over a set with a dangling dependency is an
  ordinary view and picks the parent up automatically once the origin gains
  it. A direct Shadow origin is rejected; combine hidden IDs over its
  BASE/COLLECTION origin.
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
* Stored `custom_check`, per-parameter `trafo`, `extra_trafo`, `constraint`,
  aggregation, and internal-tuning callbacks now discard recursive `srcref`,
  `srcfile`, and `wholeSrcref` metadata at admission. This makes serialized
  ParamSets smaller and their bytes independent of the source file that
  defined an otherwise equivalent callback. Printable Domain representations
  containing source-bearing inline functions now use canonical deparse
  formatting, without source comments. For a callback that required
  stripping, source breakpoints on the original function no longer affect the
  stored copy; set
  `options(paradox.strip_srcrefs = FALSE)` before constructing or assigning
  callbacks when source-level debugging is needed. The option is
  admission-time only. Source-reference normalization deliberately leaves
  function-valued `$values`, defaults, special values, initial values, and
  other opaque payloads untouched; their owner may use
  `utils::removeSource()` or `options(keep.source = FALSE)` when desired.
* Numeric Domain bounds and logscale normalization now enter the row
  constructor once. Empty Domain operations and zero-dimensional grids also
  enter their native engines instead of taking R-side special cases.
* Dependency Conditions are closed over `CondEqual` and `CondAnyOf`. Their
  exported constructors, `$new()` adapters, list/class shape, mutable `rhs`,
  formatting, and serialization remain. Unknown Condition classes are rejected
  when a dependency is added instead of being dispatched later.
* Current Paradox 2 objects serialize normally.
  `upgrade_paradox_object()` remains the pure, non-mutating converter for one
  explicitly supplied built-in ParamSet-family, Domain, or Condition object.
  The new `upgrade_paradox_object_graph()` instead searches a containing object
  graph and upgrades every admitted legacy ParamSet-family R6 shell in place,
  preserving the shell and shared-node identities, each shell's public
  `assert_values` policy, and returning the original root invisibly. Its
  iterative native crawler follows ordinary containers,
  attributes/S4 slots, environments, closures and bytecode expressions, active
  binding functions, and promises without forcing them. It does not enter the
  global/search/package/namespace environment infrastructure, invoke an active
  binding or serialized method, or inspect generic external-pointer or weak
  reference internals. Authenticated Paradox core payloads remain traversable.
  The precomputed search-boundary identities stay rooted for the complete
  allocating crawl, including if a pending finalizer detaches an environment.
  Attributes and primary edges are selected as one rooted node generation.
  The exact old-R closure facade is allocation-free once its carrier is rooted;
  the allocating old-R bytecode bridge and environments must reproduce one
  exact complete snapshot twice or migration fails closed. Structural
  list/expression ALTREP is rejected before its provider or attributes are
  observed.
  A native direct-binding classifier distinguishes realized language/symbol
  values from delayed promises without using `substitute()` or evaluating
  either. R 3.6--4.4 inspect reached promises through their compatibility
  accessors. R 4.5's compiled-code policy rejects those accessors and provides
  no replacement, so recursive migration fails closed on a reached promise and
  asks the caller to migrate under R 4.0--4.4 or R >= 4.6. R >= 4.6 uses its
  public binding/dots inspection API; detached promises outside those cells
  remain opaque. `R_getVar` is deliberately excluded before R 4.6 because it
  could force a delayed binding without that release's classifier. Migration
  performs a full semantic and shell-shape preflight,
  including a joint native validation of every prepared/current root before
  the first transplant, then commits valid nodes in post-order. Current Shadow
  preflight builds its authoritative live projection without installing it and
  keeps the selected source core separate from that semantic preview. After a
  child transplant, its prepared parent is rebased to the original child
  identity. All already-current identity roots plus that newly rebased parent
  are jointly revalidated before its transplant; unrebased parents remain
  offside templates until their own turn. The current identity-root set is
  jointly checked again after each transplanted original joins it. Each
  `.__enclos_env__` is replaced last. A
  catastrophic allocation failure inside a binding wave retains the old
  completion marker and remains retryable; already committed nodes are valid.
  Already-current shells are semantically validated in the same preflight, so
  corrupt current state cannot permit a legacy sibling to change first.
  Pending finalizers from unrelated user objects are outside the atomic
  transplant contract: if one mutates a selected root inside the R binding
  wave, the post-transplant barrier detects it and errors, but does not roll
  back a completed transplant or promise retry of the externally corrupted
  graph.
* Migration output is closed under the operations that consume it. Base R
  answers `names<-` on a referenced list of at least 64 elements with a
  structural wrapper the crawler must reject before observing it, so
  `upgrade_paradox_object_graph()` materializes the caller-owned top-level
  container once before the crawl, and `p_fct()` materializes a list `levels`
  carrier before its transformation closure captures it. A nested container of
  that shape is still rejected, now naming the ordinary-copy remedy. A
  standalone legacy `CondEqual`/`CondAnyOf` is admitted through the same closed
  Condition engine that `$add_dep()` and `condition_test()` use, so a complex,
  raw, or attributed right-hand side fails at migration time instead of
  producing an object no engine accepts; the exported constructors stay as
  permissive as Paradox 1, and the engine remains the single owner of the
  testable shape rule. A legacy Domain `repr` must be the ordinary
  non-ALTREP/non-S4 object that construction admits, so a migrated Domain still
  builds; its content stays opaque print-only metadata. Migrated typed values
  whose attribute metadata contains a closure still fail migration closed, and
  a migrated `ParamUty` value leaf keeps the legacy object's exact identity even
  when its outward representation is an atomic vector, which remains a
  documented aliasing channel.
* New Paradox 2 R6 objects call versioned namespace targets directly.
  Historical unversioned ParamSet-family leanification targets are cold
  compatibility gateways. Current shell authentication accepts an ordinary
  unique additive class chain ending in the appropriate
  BASE/COLLECTION/SHADOW `ParamSet`/`R6` family suffix; it requires an exact
  `assert_values` flag and canonical matching core. The gateway selects the
  superclass enclosure that defines the historical target and replays one
  rooted native context. It never evaluates the old stub's serialized
  `private`/`super` promises or rereads a guessed top slice. A shell without an
  authenticated current context reports the legacy object by default and
  directs users to
  `upgrade_paradox_object_graph()`. Setting
  `options(paradox.legacy_object_action = "upgrade")` enables silent
  identity-preserving first-use migration before the requested operation
  continues.
* Owner packages can use `register_paradox_object_upgrader()` for one exact
  direct `c(<owner class>, "ParamSet", "R6")` legacy class. The registry stores
  authenticated namespace-local inspector/rebuilder names, not serialized
  callbacks, and performs neither S3 dispatch nor superclass search. It
  supports an additive bridge for bbotk's legacy `Codomain` and a replacement
  bridge from miesmuschel's legacy Shadow to Paradox's `ParamSetShadow`,
  including explicit errors for its retired `params_unid` and `set_id`
  bindings. Additive inspectors declare no owner dependencies; authenticated
  BASE callback-carrier dependencies are composed internally. Replacements
  have exactly one `origin` and must produce a current Shadow. Because the
  rebuilt shell must carry the exact registered class and admission requires
  its class kind to equal its capsule kind, registration refuses the two
  unsatisfiable combinations up front: a replacement bridge is accepted only
  for the exact `c("ParamSetShadow", "ParamSet", "R6")` vector, and an
  additive bridge never for it. Owner classes with R6
  finalizers are rejected because their registrations cannot be transplanted
  safely. Unknown subclasses still fail closed. Legacy method provenance is
  authenticated by exact loaded namespace identity rather than spoofable
  namespace metadata, and the cold migration/gateway layer uses the same
  native non-forcing binding classifier as the graph crawler.
* Built-in Domain and ParamSet value failures now use one package-owned C
  classifier and failure-only formatter. Missingness, type/shape, integerish,
  bounds, and factor-membership errors retain informative checkmate-style
  categories and established message fragments without calling checkmate or
  repeating validation in R. Scalar missing values retain the useful
  missingness diagnosis even if their storage mode differs from the Domain.
  Byte-identical reproduction of every checkmate quirk, `conditionCall()`,
  implementation frame, side-effecting promise behavior, or unsupported
  exotic-object semantics is not promised.

## Correctness fixes

* Assigning values through a `ParamSet` that one assignment reaches by more
  than one path -- a set contained twice in a `ParamSetCollection`, or a
  `ParamSetShadow` next to its own origin -- now warns when the two paths plan
  conflicting values. Each path plans a complete replacement of that set's
  store, so the later one wins; assigning through only one alias therefore
  discarded the value entirely. The outcome is unchanged and still
  deterministic, but it is no longer silent. An ordinary graph reaches no
  duplicate target and pays nothing for the check.
* A `ParamSetCollection` or `ParamSetShadow` graph that shares a subtree
  exposing no parameters, dependencies, values, or callbacks now validates that
  subtree once instead of once per path through it. `$check()` on a 41-object
  alternating shared graph took 19 seconds and allocated a node snapshot per
  path (2^20 of them) to produce an empty result; it is now flat. A shared
  subtree that does contribute is still expanded once per occurrence, because
  each occurrence exposes its own affixed IDs.
* Every clause of the canonical Domain-state gate reports the argument it
  guards. `p_fct(c("a", "a"))`, `p_dbl(0, 1, tags = NA_character_)`, and
  `p_dbl(0, 1, trafo = 1)` reported "Invalid built-in Domain state; Paradox 2
  supports only canonical p_dbl, p_int, p_fct, p_lgl, and p_uty Domains",
  which names no argument and reads like an internal failure. The clause
  evaluation order is unchanged, so an input wrong in two places still reports
  the same one it always did.
* `$check()` results follow checkmate's convention that a check result is an
  unpunctuated sentence fragment which the assertion wrapper terminates.
  Paradox's own fragments carried a trailing period, so every assertion built
  on one ended in `..` -- for example
  `Assertion on 'xs' failed: Parameter 'nope' not available..`. The native
  assertion wrapper additionally terminates a fragment only when it does not
  already end a sentence itself, which is what the two-sentence
  `"Did you mean ...?"` hint does.
* Converting a Domain's current `to_tune()` value into a search space no longer
  copies that TuneToken back as a fixed design value. Exact native clones now
  preserve ordinary fixed values without making a FullTuneToken's two fields
  a length-two assignment to every generated row.
* Empty settings honor `presence = "all"` and `"required"`.
* `domain_qunif()` rejects incompatible input dimensions.
* Unbounded integer Domains reject integer-valued doubles outside R's integer
  storage range instead of accepting them and later coercing them to
  `NA_integer_`.
* Collection child transformations run exactly once and child constraints
  receive the correct active unprefixed values; strict checks consult live
  child constraints.
* Collection assignment reaches live child state, including a shadow's origin.
* `generate_design_grid()` now deduplicates each realized built-in axis,
  collapses stored fixed values, and prunes dependency-inactive branches before
  materializing their Cartesian product. It retains the established
  first-nominal-occurrence row order while avoiding nominal products that may
  be vastly larger than the returned design. The new optional `upper_limit`
  argument bounds the final realized row count. A nominal zero axis still
  makes the result empty, even when that parameter has a stored fixed value.
  Valid fixed cross-storage, `NULL`, and S4 special values retain their identity
  as list-column values; a fixed TuneToken now raises an informative error
  instead of being treated as a concrete grid value.
* ParamSet quantile and grid kernels now consume canonical plain capsule tables
  on every supported R release, including R 3.6/4.3/4.5. Zero-axis grids retain a
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
* Single-element cells taken out of an attributed public-table column are now
  independently owned. Previously the cells that `Design$transpose()` and the
  table check/constraint entry points cut out of a logical column shared R's
  global `TRUE`/`FALSE`/`NA` singletons, so copying that column's attributes
  onto a cell attached them to every logical scalar in the session; and a
  column attribute that encodes the column's own length (`names`, `dim`,
  `dimnames`) was copied verbatim onto the one-element cell, producing a vector
  whose metadata described more elements than it stored. Ordinary column
  classes such as `factor` and `Date` still reach the cell unchanged.
* The `ObjectTuneToken` receipt set of a checked value assignment is kept
  rooted for the whole operation. It was previously displaced from the
  protection stack by the first receipt it stored, so a collection during any
  later validation, callback, or allocation could invalidate it.
* A capsule table whose columns disagree in length is rejected by the shared
  exact-table validator instead of being admitted on the strength of its first
  column and then indexed out of bounds by whichever reader reached it first.
* `rd_info()` reports untyped defaults against the right parameters when
  `descriptions` is supplied. The description merge reorders the table by id
  while the untyped defaults still followed the parameter table, so generated
  documentation could show one parameter's `p_uty()` default under another.
* Deep cloning a `ParamSetCollection` with no children produces a usable
  collection again. The rebuilt edge list lost its zero-length `names`, which
  the capsule validator requires, so every operation except `$ids()`, cloning,
  and serialization reported corrupt collection state on the clone.
* `generate_design_random()` rejects a factor `n` instead of silently sampling
  as many rows as the level's internal code. Every other scalar attribute
  remains tolerated, and `Sampler$sample()` already rejected it.
* Random, LHS, Sobol, and directly constructed designs now write fixed
  `$values` exactly as the native grid generator already did. Only one ordinary
  element of the parameter's own storage type collapses into a typed column;
  a `NULL`, multi-element, cross-storage, or container special value keeps its
  identity as a list-column entry. Previously such a value was written with
  plain data.table column semantics, so a cross-storage special became `NA`
  with coercion warnings, a container value was unwrapped and recycled, a
  multi-element value errored on length, and a `NULL` deleted the column from
  the design entirely. A stored `TuneToken` now raises the same informative
  error on these generators that grid generation already raised, instead of
  scattering the token's fields across the design.
* The `$get_values(type = "with_internal")` documentation described the
  complement of what the filter returns; it selects only `InternalTuneToken`
  values. The behaviour is unchanged.
* `upgrade_paradox_object()` migrates a Domain whose numeric bounds are stored
  as integers. `p_int()` stores integer bounds in both Paradox 1 and 2 and the
  native capsule admits either storage, so requiring a double refused an
  ordinary legacy `p_int(1L, 10L)`. A legacy collection's `owner_ps_index` is
  likewise accepted when Paradox 1's `$add()` promoted it to a double.
* Upgrading a Domain without `depends` no longer denormalizes its requirements
  to an empty list, so an upgraded object is `identical()` to the object the
  current constructors produce.
* Source stripping removes a parsed `function(...)` node's srcref cell instead
  of emptying it. The cell kept its `"srcref"` class after losing `srcfile`, so
  a closure built from such a node -- for example one a `.extra_trafo` creates
  at call time -- printed as `<srcref: file "" chars ...>` instead of its body,
  and the package's own scan then reported the callback as already clean.
* `$search_space()` no longer emits `non-uniform 'Rounding' sampler used` (or
  the Kinderman-Ramage note) once per converted TuneToken for callers who set
  a legacy `sample.kind`/`normal.kind`. Restoring the caller's own setting is
  bookkeeping, not a new choice; under `options(warn = 2)` that warning also
  aborted the exit handler before `.Random.seed` was put back, leaving the
  caller's random stream desynchronized.
* The `ParamSetShadow` constraint adapter admits only an ordinary names vector,
  like every sibling gate. It indexes those names element by element, so an
  ALTREP or S4 names object could pair each value with another parameter's name
  and silently change the constraint's verdict.
* `$deps<-` measures the dependency columns it indexes rather than the lengths
  observed before its name and type checks. Those checks allocate and can
  dispatch an ALTREP `Length` method, so a callback replacing a column of the
  supplied table left the row loop reading past the end of a retained column --
  a segfault on the older supported runtimes.
* Collection callback detachment roots each generated affix before the next
  allocation. The fresh strings were parked in operation-local storage that the
  collector does not scan, and R sweeps its string cache on every collection,
  so building a plan for a large collection could raise
  `Corrupt ParamSetCollection affix translation` on valid input, emit a plan
  whose affixes compare unequal to their own text, or crash. Its route scratch
  is also allocated once per plan instead of once per selected parameter.
* A snapshotted `default`/`init` value leaf keeps the materialized ordinary
  `names` the snapshot produced. Copying the source's attribute set afterwards
  replaced the whole attribute list, so the caller's original names object went
  into the capsule instead -- including an ALTREP one, and including one whose
  length could later disagree with the value it names.
* An unsupported `disable_in_tune`, `logscale`, or `repr` cargo entry is
  reported by argument name again. A closure, symbol, call, or environment
  reached the internal value snapshot first and produced
  `Cannot snapshot semantic value of type ...`.
* `$qunif()` takes a data.frame's column names and column identities from one
  generation of the input. The names were captured before the row-name Length
  observation and the columns after it, so a reentrant callback -- a hostile
  ALTREP `row.names` method, or a finalizer running at the intervening
  allocation -- could pair each name with a different column and silently map
  every value through another parameter's Domain.
* The Domain `id` column re-read on a failed built-in check is bound to the
  admitted row count. Every other column re-read in that kernel already was.
  A hostile ALTREP `special_vals` entry re-enters R while `identical()`
  dispatches its `Length` method, so the column could be shorter by the time
  the diagnostic indexed it; on R releases whose `STRING_ELT` is unchecked
  that was an out-of-bounds read handed to the diagnostic builder.
* Diagnostics escape a fragment whose bytes are not valid UTF-8 in the current
  locale instead of failing the message builder. An unknown parameter name
  carrying such bytes reported `Internal error: invalid UTF-8 parameter
  identifier` (or `... a message fragment is not valid UTF-8`) from `$check()`,
  `$test()`, `$values<-`, `$subset()`, and `$get_domain()` rather than the
  operation's own "not available" message; and a `custom_check` diagnostic was
  minted as UTF-8 without validation, so `domain_check()` could return a string
  that declared an encoding its bytes did not satisfy and that `nchar()`
  rejected. Declared `latin1` and `bytes` names keep their existing behaviour.
* `domain_check()` observes its `internal` flag exactly once. It was validated
  and then read a second time after value admission, which may materialize a
  semantic ALTREP and reenter R; a second answer of `NA` was treated as
  "internal" and silently stopped honoring `special_vals`.
* `p_uty()` values that are symbols or unevaluated calls now reach
  `custom_check` and per-parameter `trafo` as themselves. They were previously
  spliced into the callback call as an expression, so the callback received the
  *evaluation* of the value (silently checking or transforming something other
  than what is stored), failed on names that are not visible in the base
  environment, or ran the call. Every other R value is self-evaluating and was
  and remains unaffected.
* `paramset_to_configspace()` exports single-element sequences as sequences.
  A one-level `p_fct()` and a single-value `CondAnyOf()` reached Python as a
  scalar string, which ConfigSpace iterates character by character: the
  conversion either silently produced one level (or one condition value) per
  character, or failed outright when a character repeated. Dependencies on a
  `p_lgl()` parent now use its exported `"TRUE"`/`"FALSE"` levels instead of
  passing a Python `bool` that no parent level can match, and exported
  `meta$tags` is always a Python list.
* `to_tune(<Domain>)` again keeps an `init` given on that Domain as a fixed
  value of the generated search space, as `ps()` does for the same Domain.
  Paradox 2 dropped it, so `to_tune(p_int(1, 10, init = 5))` produced a free
  three-point grid axis instead of the single fixed point Paradox 1 produced.
  Tuning the whole parameter with `to_tune()` still contributes no value: the
  `.init` a recovered Domain facade projects there is the parameter's own
  current value, which in that branch is the `TuneToken` itself.
* `ParamSet$search_space(values = )` rejects a name that is not a parameter ID
  of the set, restoring the Paradox 1 `subset.of` assertion natively. An entry
  that was not a `TuneToken` was silently dropped, so one misspelled name
  returned an empty search space instead of an error. Resolving the container
  now uses one hashed ID index instead of a per-name linear scan.
* `ParamSetCollection$new()`, `$add()`, `ps_union()`, `psc()`, `c.ParamSet()`,
  and `ps_replicate()` require a set name or affix to keep the IDs it creates
  inside the parameter-ID grammar: a prefix must itself match
  `^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$`, a postfix must use only ASCII letters,
  digits, `.`, and `_`, and the empty name still affixes nothing. A looser
  name built a collection whose generated IDs its own `$search_space()` and
  `ParamSet$new(<collection>$domains)` then rejected. This is the Paradox 1
  `assert_names(type = "strict")` check, which `$add()` had never applied.
* `condition_test()`, and the dependency evaluation behind `$check()`,
  `$test()`, `$get_values()`, and design generation, report a comparison
  between a character value and a logical, integer, or numeric right-hand side
  (or the reverse) as not satisfied, instead of raising "Condition comparison
  operands have incompatible types" or reporting the operand's shape as
  unsupported. A value of a type the right-hand side cannot equal does not
  satisfy the Condition.
  This is deliberately *not* Paradox 1's answer: Paradox 1 compared through
  R's `==`, so `condition_test(CondEqual(1), "1")` was `TRUE` there because
  `1` was coerced to `"1"`. The closed comparator never reinterprets a value
  as another type. Like Paradox 1, it does not raise for this.
  The structural admission gates — a non-atomic, classed, S4, or
  complex/raw operand — still error.
* Validating the capsule graph visits each node once. A graph that shares one
  child between two parents was re-walked once per distinct path, so cost grew
  as `2^depth`: 56 objects took 0.2s and deeper shares were unusable. Cycle
  detection is now the same node colouring instead of a per-edge scan of the
  active path, which also removes a quadratic term for deep chains.
  The collection traversals behind `$check()`, `$get_values()`, `$domains()`,
  and `$add()` remain proportional to the number of distinct root-to-set
  paths, because each path exposes its own set of prefixed parameter IDs;
  that count is the size of the collection's own parameter table whenever its
  shared sets carry parameters.
* `ParamSetCollection$new()` accepts an unnamed `sets` list of any length, and
  with it `ps_union()`, `psc()`, `c.ParamSet()`, `ps_replicate()`, and
  `$search_space()`. Naming the argument had to duplicate it, and R returns a
  wrapper ALTREP for a list of 64 or more elements, which the native boundary
  refuses. `$search_space()` unions one part per `TuneToken`, so a `ParamSet`
  with 64 or more tuned parameters could not produce a search space at all.
* A non-finite `tolerance` is reported as a `` `tolerance` `` argument error.
  `p_dbl(0, 1, tolerance = Inf)` passed the named-argument gate and was
  rejected by the final canonical-state check, which can only name the whole
  `lower/upper/tolerance` field group.

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
