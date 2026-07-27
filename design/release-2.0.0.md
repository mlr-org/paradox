# Paradox 2.0.0 active release ledger

## Status

**The package-facing candidate is frozen; `release-core` is green, and the
memory source-proof rerun is pending after a validation-harness repair.**
The active candidate is
`refs/paradox-release/candidate-20260727T152133Z`, commit
`dbbdcc156cb52793e84e8767f0ce84b6ecbb85ea`, tree
`b60b75e3923cdcb49f1ef2fb0f9b386d5cac291d`. It contains the complete
contract-first implementation, R 3.6/C99 compatibility work, and the managed
active-path graph carrier with ordinary and instrumented regressions.

The exact `release-candidate-dbbdcc1` run passed all eight `release-core`
tasks: controller/native/runtime/validation harnesses, differential, API
headers, native release, and the supported R 3.6.3/4.0.5/4.3.3/4.5.2 runtime
matrix. The coordinator completion, JSON summary, and TSV summary SHA-256
values are respectively
`511da5ccf2ef4e779db97bc6e79ab458f861148b4841625132e613a2b285d090`,
`fe9b41d7552d73e2d342cfa1f3c666cb672508d4d29aef6d3c7dc063ee380e44`,
and
`507160680c2a861ece452f8a23f12caceb590897afec00a38a5757229709b522`.

The native child
`release-candidate-dbbdcc1-native-release-a001` is not an eligible memory
source donor even though its package work passed. Its source manifest retained
the original worktree modes while its copied source tree retained
umask-filtered modes. The first combined-memory attempt,
`release-candidate-dbbdcc1-memory`, then failed source-proof preflight because
the relocated validator lacked its trusted
`create-offline-check-repository.R` sibling. It stopped before loading the
candidate or starting GCT, Valgrind, or rchk, so it is neither a package failure
nor memory evidence.

The six-file harness repair makes snapshot and replay modes exact and
umask-independent, retains/hashes the offline-repository helper, validates the
same exact relocated bundle independently, and tests the generic sibling-set
relationship. The old run directories remain immutable. A fresh full native
run under the repaired harness must produce the source proof for a fresh
combined-memory run. The repair is confined to package-excluded harness paths;
its eventual commit still requires the ordinary package-facing-source identity
proof before any candidate conclusion transfers.

The finalized repair passed Bash syntax and R parse checks and the full
activated validation-hardening suite in about 226 seconds. Its adversarial
fixtures cover exact `0664`/`0775` initial and replayed modes under umask
`0077`, missing/tampered relocated helpers, and equality of all 21 copied,
hashed, and independently validated harness inputs. Actual R 3.6.3 and current
R probes also confirmed exact mode restoration with `use_umask = FALSE`.

Normative contract: [`contract-first-2.0.0.md`](contract-first-2.0.0.md).
Implementation map: [`architecture.md`](architecture.md). Compatibility and
migration policy: [`compatibility.md`](compatibility.md). Validation sequencing:
[`validation.md`](validation.md).

## Decisions frozen for the first public release

- Version is 2.0.0; R >= 3.6; portable C99; data.table >= 1.18.4.
- R C API use is public except for the exact centralized, versioned
  raw-attribute, hot closure-formals, and non-forcing stored-binding/promise compatibility
  entries described below. Raw attribute selection uses `R_mapAttrib()` on
  R >= 4.6 and one ledgered `ATTRIB` traversal on R 3.6--4.5 without
  evaluating R or data.table code. Before R 4.5, one ledgered `FORMALS`
  accessor preserves callback hot-path speed; cold closure inspection uses
  public base calls instead of native accessors that were not yet API.
  R 3.6--4.1 use `base::exists(..., inherits = FALSE)` only for cold optional
  existence queries; required ordinary-frame binding snapshots stay
  allocation-free, and the terminal optional receipt scan uses exact old-only
  `R_HasFancyBindings()` to fail closed for a fancy frame.
  On newer R the authenticated ordinary-frame path is likewise
  allocation-free. Callers retain one conservative rooting proof across the
  supported API branches because hostile class metadata can allocate during
  facade admission.
  The facade rejects the recognized `UserDefinedDatabase` class before all
  binding APIs; callback-backed object tables are not ParamSet/R6 shells, and
  old `R_HasFancyBindings()` is valid only for ordinary frame layouts.
  The compatibility facade uses exported `Rf_findVarInFrame` on R 3.6--4.5 to
  retrieve a stored frame cell. R 3.6--4.4 may inspect a returned `PROMSXP`
  through the header-declared/exported `R_PromiseExpr`, `PRENV`, and `PRVALUE`.
  R 4.5 compiled-code policy classifies those accessors as non-API, so recursive
  migration fails closed on a reached promise and requests R 4.0--4.4 or
  R >= 4.6. Ordinary factory callback frames can retain such formal promises
  even when the argument was forced or unused.
  R >= 4.6 instead uses only its experimental binding/delayed-binding/dots
  APIs. An R >= 4.5 DSO excludes all three detached-promise accessors. On
  R >= 4.6, a `PROMSXP` reached outside a binding/dots cell is opaque.
  Public `R_getVar` is also excluded before R 4.6 because, without the binding
  classifier introduced there, it can force a delayed binding. Current-R code
  has three reviewed call sites, each after `R_GetBindingType` proves a direct
  or already-forced value; the DSO has one undefined-symbol inventory row.
  Simple Domain rendering is not a second old-R implementation: one native
  renderer receives `scipen` from public `base::getOption()` on R 3.6--4.4 and
  from documented `Rf_GetOption1` on R >= 4.5. Exact DSO inventories forbid
  that symbol on the former runtimes and require it once on the latter.
  An R-level `substitute()` workaround is non-forcing but unsound for
  simultaneous receipt and recursive object-graph scans because it returns an
  expression, not a binding-kind/generation receipt.
  The native direct-binding projection distinguishes realized language/symbol
  values from delayed promises carrying the same expression types without
  evaluating either.
  Each exceptional symbol, version, source count/path, and rationale must be
  recorded in `environment/r-api-exceptions.tsv` and pass raw-token, DSO,
  pinned-header, and real-runtime audits before freeze. None is a CRAN allowlist
  or broader internal-API permission.
  The R 4.5.2 runtime stage additionally retains a manifest-bound zero-issue
  receipt from that runtime's own `tools:::check_compiled_code()` over the
  installed package.
  R 3.6 has no accessor for an active-binding function. Direct and recursive
  legacy ParamSet-family migration therefore fail closed when its inspection is
  required and ask for R >= 4.0; because Paradox-1 ParamSet-family R6 shells use
  active bindings, practical migration of those objects requires R >= 4.0.
  Exact built-in current Paradox-2 shells retain recursive traversal through
  their authenticated capsule. Package active facades are opaque on R 3.6:
  unsupported in-place replacement cannot be distinguished when the exact
  shell receipts remain unchanged, and its closure is not traversed or invoked.
  Additive shells and modifications that fail exact authentication instead
  fail closed. Current
  operations, idempotent current-object conversion, and standalone legacy
  Domain/Condition conversion remain supported on R 3.6.
- Public old-header adapters for raw/complex setters and default
  `identical()` flags are inline. Collection parameter reads pass the admitted,
  rooted core directly to the shared loader, eliminating both a temporary
  environment allocation and the need for post-3.6 `R_NewEnv()`.
- R 3.6 cannot construct VECSXP ALTREP because R exposes that facility only
  from R 4.3. Its runtime row therefore version-gates only the adversarial
  list-ALTREP fixture; atomic ALTREP tests still run and the corresponding
  production list branch is vacuous.
- One opaque v1 capsule and BASE/COLLECTION/SHADOW node graph are the current
  state model.
- The public `assert_values` flag is the sole stateful R-shell policy outside
  that model. It selects checked versus unchecked native storage and remains
  clone/serialization/equality-visible without changing the ten-field capsule.
- Capsule tables are plain data.frames; data.table is outward-only.
- Domain and Condition kinds are closed; ParamUty custom checking remains.
- Canonical built-in Domain-row semantics have one native admission owner shared
  by constructor final-state validation, ParamSet construction, and
  ObjectTuneToken Domain admission. Boundary-specific outward table/class checks
  do not duplicate row semantics.
- Standalone `condition_test()` uses the registered closed comparator for
  `NULL` or plain logical/integer/double/character vectors with names only;
  stable ALTREP operands materialize once and classed/attributed operands do
  not dispatch through `Ops` or `%in%`.
- Built-in Condition RHS values are attribute-free, non-missing vectors of the
  same four kinds. `CondEqual` has one element and `CondAnyOf` is non-empty and
  unique. Public admission roots and materializes the RHS once; strict capsule
  validation accepts only that ordinary snapshot.
- Additive-only ParamSet-family subclassing is supported; core ParamSet
  overrides/private state are not. The documented Sampler subclass API remains.
- ParamSetShadow belongs to Paradox and replaces miesmuschel's private-layout
  implementation on the Paradox-2 branch.
- `ParamSetCollection$add()` is an atomic native replacement transaction. It
  follows Shadow origins, rejects corruption and existing/proposed cycles
  before commit, and generation-checks the complete current/child graphs.
- Native capsule graph-path admission roots every active shell and exact
  selected core generation in one managed carrier. Raw frame storage is
  scratch only; carrier slots are cleared on pop and grown before an allocating
  frame/child transition. This protects ancestor edges across old-R optional
  binding evaluation and finalizer-driven `.core` replacement without a
  separate old-R implementation.
- Shadow live refresh is native and generation-based. Its sole non-payload
  attribute is an exact derived origin-graph signature; `.sets[[1L]]` is the
  only origin authority and fixed factories are not cached as state. The
  signature is package-rebuildable cache data but mandatory and exact on every
  current SHADOW core.
- Checked value assignment is one graph-wide native transaction over ultimate
  BASE targets, including collections and shadows. It deduplicates shared
  targets, validates once, preserves nested callback writes, and commits all
  replacements atomically without a second R or child-store pass. ParamSet
  Object-token inputs add rooted generation receipts and one final allocation-
  free candidate reauthentication immediately before commit.
- Standalone Domain checks and ParamSet scalar/table checks share one
  package-owned C value classifier and failure-only formatter. Ordinary
  missingness, type/shape, integerish, bounds, and factor-membership failures
  use informative checkmate-style categories and established fragments.
  Successful validation does not construct diagnostics; the native validation
  path neither calls checkmate nor repeats validation in R. Byte-identical
  reproduction of every checkmate quirk, `conditionCall()`, or unsupported
  exotic-object behavior is outside the contract.
- `check_dependencies()` reuses the native check graph/point/dependency kernel,
  accepts only an ordinary uniquely named base list, validates unknown IDs even
  without dependency rows, skips TuneToken edges, and returns the first
  diagnostic rather than reproducing R/pmap multi-error collapse.
- `test_constraint()` and `test_constraint_dt()` reuse the native check graph,
  point admission, and constraint kernel. A validating table call admits every
  row before any constraint callback and then evaluates one immutable
  constraint snapshot once per row; ParamUty custom checks may run during
  Domain admission, and reentrant mutation affects only later public operations.
- Tag access/mutation, dependency snapshot/access/mutation/append, and BASE
  callback replacement are native capsule operations. Bulk dependency
  replacement is a callback-free structural snapshot and preserves predicates
  made infeasible by parent-Domain narrowing. `$add_dep()` remains the strict
  authoring operation: dependency feasibility uses the shared check kernel and
  generation-checks callback reentry; Shadow append routes natively only within
  the fixed visible schema.
- `$has_deps` is one registered scalar reader. BASE validates its canonical
  dependency table, SHADOW performs one live refresh before validating its
  table, and COLLECTION admits the complete graph before reading the root
  subtree count. It never constructs a detached dependency/data.table facade
  or uses a cached or reduced-integrity graph path.
- A BASE-origin Shadow constraint closure contains exactly a callback and
  hidden-values plan. Its native evaluator performs the hidden-first merge
  without S3 dispatch, preserves leaf identity, calls once, and admits one
  non-missing logical result; collection origins stay on the collection native
  evaluator family.
- Current objects serialize normally. `upgrade_paradox_object()` remains the
  pure single-object converter, including standalone Domain/Condition
  normalization. `upgrade_paradox_object_graph()` iteratively discovers a
  containing graph and transplants admitted legacy ParamSet-family shells in
  place after complete preflight. It traverses ordinary containers,
  attributes/S4 slots, local environments, closure/bytecode structure, active
  binding functions, and promises without forcing or invoking serialized
  behavior; global/search/package/namespace infrastructure and generic
  external-pointer/weak-reference internals are boundaries, while authenticated
  Paradox core payloads remain traversable. Commit is post-order and monotonic,
  with `.__enclos_env__` as each shell's last completion point. Direct native
  binding classification distinguishes realized language/symbol values from
  promises without forcing either. Current shell admission uses one ordinary
  additive BASE/COLLECTION/SHADOW suffix classifier, exact `assert_values`, and
  canonical-core agreement. Read-only Shadow preflight retains separate
  source-generation and authoritative semantic-preview cores and constructs
  callback detachment from the admitted graph. Every prepared/current root is
  jointly validated before the first transplant; after each child transplant,
  its prepared parent is identity-rebased. All already-current identity roots
  plus that newly rebased prepared root are jointly validated before the
  parent changes; unrebased parents remain offside templates until their turn.
  The current identity-root set is checked again after the transplanted
  original joins it. A catastrophic
  partial binding wave retains the old authoritative enclosure and remains
  authenticated for retry; completed nodes are valid current objects. Current
  shells are preflight candidates as well as traversal carriers, so a corrupt
  current capsule anywhere in the selected graph aborts before any legacy
  mutation. Pending finalizers from unrelated user objects are explicitly
  outside this atomicity promise: the post-transplant barrier detects a
  selected-root mutation inside the R binding wave, but does not roll back a
  completed transplant or promise retry of the externally corrupted graph.
- Current R6 stubs call versioned namespace targets directly. Historical
  unversioned targets are cold first-use gateways: default error, or silent
  migration when `options(paradox.legacy_object_action = "upgrade")` is set.
  Direct forwarding uses one native rooted context: it selects the
  defining-family enclosure through the authenticated additive superclass
  chain, requires exact `assert_values` and a canonical matching core, and
  ignores the serialized stub's `private`/`super` promises rather than
  replaying or rereading a top enclosure slice.
  The exact owner registry supports bbotk's additive legacy `Codomain` and
  miesmuschel's single-origin current-Shadow replacement/retired fields without
  S3 dispatch or serialized hook functions. Additive dependencies are empty,
  replacement dependencies are exactly `origin`, and owner R6 finalizers are
  rejected; unknown subclasses fail closed.
  Built-in and owner method provenance is checked against the exact currently
  loaded namespace environments; namespace names alone are not authority.
- Stable/base ALTREP support is materialize-once in admitted semantic atomic
  positions. Configuration/search-space/trafo and ParamSet-`params` lists,
  internal table/row/Domain/Condition/token/capsule shells, Domain cargo/
  interpreted cargo entries, dimnames, class/name vectors, and other list
  metadata remain ordinary non-ALTREP/non-S4. The six public-table ingresses
  use one suffix-aware, allowed-attribute classifier. An ordinary well-formed
  class vector may have leading additive classes before its terminal
  `"data.frame"` or `c("data.table", "data.frame")` suffix. Those leading
  classes never dispatch. The classifier does not copy or materialize an
  ordinary shell merely to remove the prefix, and semantic snapshots ignore it;
  an already-required ALTREP snapshot installs the canonical suffix. Malformed,
  reversed, non-suffix,
  reserved-label, and duplicate class vectors reject. Names/classes and
  admitted data.table cache carriers are ordinary; caches are discarded. Raw row names
  are attribute-free, nonobject, non-S4 integer/character vectors: ordinary
  compact `+/-n` forms decode to their count, while stable row-name ALTREP pays
  one Length and no Elt. Row-consuming operations compare this count with their
  columns; direct `trafo` and a no-edge Design dependency plan do not add
  column observations for an unused dimension. A zero-column data.frame may
  omit names and retains its row count in Design transpose. Admitted top-level
  VECSXP ALTREP snapshots own names/class before callback-capable observation,
  use one Length/one Elt per column, and may retain stable semantic ALTREP
  columns. Base R's lazy attribute-copy duplicate is the common motivating
  case. Direct
  checked/unchecked `$values <-` rejects an outer ALTREP before observation and
  natively canonicalizes the Paradox-1 empty spellings (`NULL`, an ordinary
  attribute-free zero-length atomic/expression vector, or an accepted empty
  list container) to a named list; only
  `set_values(.values=)` has an operation-specific outer-list snapshot.
  Hostile state-changing custom ALTREP across prior R-side representation capture has
  no exact semantic/printed-representation guarantee. Paradox must neither
  replay nor itself cause a crash or memory corruption.
- Base `extra_trafo` results may remain unnamed for public and TuneToken
  compatibility; collection child results require complete unique names for
  namespace translation. Transformation results and non-table inputs have
  ordinary non-ALTREP/non-S4 shells. A documented data-frame input may use the
  suffix-classified top-level ALTREP table boundary above, while admitted atomic leaves and
  columns may be stable ALTREP. Both use the single native transformation engine.
- The unreachable namespace-level R `transpose()` implementation and unused
  `col_to_nl()`/`rbindlist_proto()` table helpers are deleted. Known consumers
  call the public `Design$transpose()` method; the removed internals were neither
  exported nor used by the maintained/downstream corpus.
- Live collection callback bindings, detached subset/flatten callback
  factories, and SHADOW adapters over COLLECTION origins all use that same
  native evaluator family. Their R closures retain only exact validated
  owner/mapping plans and contain no parallel callback selection/translation
  engine. Retained/untransformed inputs remain in input order, followed by
  changed child outputs in callback-plan order; omissions remove owned inputs.
- `ParamSet$subset()` has one additive final `keep_trafo = TRUE` argument,
  shared by COLLECTION and SHADOW. Setting it to `FALSE` strips both selected
  per-parameter transformations and `extra_trafo` in the native subset
  transaction while leaving `keep_constraint` independent. This public API
  replaces mlr3mbo's private Domain-table mutation; malformed Domains are not
  admitted for compatibility. Subset flags are exact attribute-free logical
  scalars, and COLLECTION callback detachment follows the admitted result
  without applying R generics to the original controls.
- Exactly two narrow cold R semantic-orchestration families remain, and neither
  is a fallback. The first contains the three internal-tuning operations—
  aggregation, disabling, and internal search-space conversion—as single R
  implementations over one captured cargo/translation/Domain/owner-value
  snapshot and commits through native mutation.
  After native flattening, the same cold family may rebind documented `cargo`
  closures and replace that one column in the detached BASE result. It is the
  first narrow exception to thin wrappers, not a second graph/check/value/
  callback-selection engine. The second is exact-TuneToken `$search_space()`
  conversion: it consumes one rooted native snapshot, switches only over the
  package's built-in token kinds, and solely owns callback-dependent
  one-dimensional output compatibility and outward search-space construction.
  It has no S3 extension or competing native/R conversion path. One-way legacy
  migration is outside this current-operation count: it authenticates a
  retired schema and orchestrates current native construction/validation plus
  R6 shell transplant, never an alternate current semantic engine.
- Ordinary non-ALTREP S3-classed named value-list containers are admitted with
  the outer class discarded; scalar Domain argument names are likewise
  representation-only. Direct checked/unchecked assignment rejects an outer
  ALTREP before observation and canonicalizes empty input in native code.
  Neither is an extension/dispatch mechanism.
- TuneTokens have one native exact-shape snapshot boundary: exact `{content, call}`
  names, five built-in class vectors, exact Full/Range/Internal content, and an
  admitted bounded value-producing built-in Domain or exact BASE
  `c("ParamSet", "R6")` shell/core for
  Object content. An unbounded `ParamUty` Domain rejects; bounded typed Domain
  coverage retains opaque leaves without treating ParamUty itself as a range.
  COLLECTION, SHADOW, and additive subclasses reject. Exact
  creator provenance is not inferred: a shell alias retaining genuine BASE
  private/core linkage may pass safely because C never calls alias methods.
  Scalar names are normalized away. Subclasses, extra/reordered metadata,
  S4 structure, malformed calls/content, and recursive forgery reject before
  traversal. `$search_space(values=)` accepts an ordinary or names/class-only S3
  named list without dispatch, enters this same admission, and replaces every
  live BASE candidate with a sealed single-use capability before closed
  conversion.
- Apart from the public-table and `set_values(.values=)` boundaries above,
  Domain/Condition/token/ParamSet and every other interpreted structural
  ALTREP/S4 shell is rejected. The outer `special_vals` list is ordinary
  non-ALTREP/non-S4 for every Domain kind. Typed Domain special leaves reject ALTREP; an admitted
  typed S4 special, default, or init matches only by pointer identity. ParamUty
  leaves remain opaque, including S4, while Paradox-1 special membership alone
  uses base `identical()` without S3/S4 dispatch. Malformed exact-token/Domain
  structure is a hard boundary error, while ordinary value infeasibility
  remains a check diagnostic.
- ParamSet-family equality is a detached complete-state graph comparison and
  never walks private/inherited R6 active bindings. Canonical node references
  distinguish shared from duplicated topology without distinguishing
  independently built equivalent DAGs.
- Grid generation is one output-sensitive native graph operation. It shares
  dependency planning/comparison with Design masking, preserves exact ordinary
  first-nominal-occurrence order, and applies `upper_limit` to the final
  realized design rather than a nominal or intermediate product.
- All major compatibility breaks above ship now. They are not deferred to a
  later release.

Changing one of these requires an explicit contract/design/NEWS/test update,
not a local compatibility workaround.

## Implementation convergence checklist

Checked entries below record implemented architectural components or
historical exact-payload conclusions. They do not make the reopened worktree
release-ready and do not transfer an earlier candidate's green gates. The
remaining convergence and acceptance steps are:

- [x] converge the dormant-values/default-aware-activity implementation,
  focused contract tests, documentation, differential cases, and dependency-
  rich benchmark workloads;
- [x] replace nominal Cartesian grid materialization with the native
  output-sensitive fixed/dependency-aware engine, focused exact-order and graph
  tests, final-size ceiling, and collapse/pruning benchmark workloads;
- [x] normalize source references on package-interpreted callbacks and legacy
  migration while preserving opaque function-valued payloads and the
  admission-time debugging opt-out;
- [x] complete the pre-specified final performance batch with focused
  correctness, balanced A/B evidence, direct routine coverage, strict
  GCC/Clang builds, and the retained integrity-validation stop boundary;
- [x] complete R 3.6 compatibility and its header/runtime/portability harness;
- [x] repair the independently discovered active graph-frame GC lifetime defect
  with one managed carrier plus ordinary and compile-time-instrumented
  regressions;
- [x] freeze the replacement candidate at `dbbdcc1`;
- [x] run the eight-task `release-core` profile; all rows pass, while the
  native child source proof is retained only as informative execution evidence
  because its copied modes do not replay;
- [ ] create a fresh replayable native source run with the repaired harness and
  complete the combined-memory gate from that exact source proof;
- [ ] run and retain the complete applicable gate matrix against that exact
  candidate, including the remaining downstream, documentation, benchmark, and
  hosted portability stages.

### State and public model

- [x] v1 NULL-address external-pointer capsule with ordinary protected truth;
- [x] fixed ten-field BASE/COLLECTION/SHADOW schema;
- [x] canonical plain internal table constructors/validators;
- [x] package-owned exported ParamSetShadow shell and initial bridge contract;
- [x] closed Domain and Condition public dispatch;
- [x] standalone Condition comparison and scalar/table constraint-only calls
  enter registered native operations with no S3 or R row-evaluation engine;
- [x] native collection-add and tag/dependency/callback mutation planners
  replace the remaining R/checkmate/data.table canonical mutation paths;
- [x] pure single-object legacy upgrader with CRAN-1.0.1 and `mbo_config`
  fixtures in the historical candidate;
- [x] recursive identity-preserving graph upgrader, versioned current targets,
  cold first-use gateways, and exact owner registry are complete and pass their
  focused source/fixture/adversarial checks on the now-frozen payload;
- [x] complete live Shadow synchronization, clone/serialization/DAG behavior,
  and all graph-reader coverage confirmed after converged install;
- [x] value, tag, dependency, callback, and collection-add mutators use
  validated capsule replacement/generation semantics; `assert_values` is the
  explicitly separate public shell policy;
- [x] no current object path reads legacy private tables as semantic authority.

### Single native engine

- [x] native Domain/ParamSet constructors replace former fast/slow constructor
  pairs;
- [x] constructor, ParamSet, and ObjectTuneToken Domain paths share the sole
  canonical built-in Domain-row semantic admission owner;
- [x] bounded value-producing Domain (excluding unbounded ParamUty and
  zero-level ParamFct) and exact BASE-only ObjectTuneToken
  admission, safe genuine-core aliasing, generation receipts/final commit scan,
  sealed search capabilities, ALTREP/S4 fail-closed structure, pointer-only
  typed-S4 special matching, and ParamUty base-`identical()` special membership
  are confirmed against the converged install;
- [x] unified BASE/COLLECTION/SHADOW `check` and `check_dt` implementation is
  integrated at source level;
- [x] standalone Domain and ParamSet scalar/table built-in admission use one C
  classifier and failure-only informative formatter, with no R/checkmate
  duplicate;
- [x] live and detached collection transformation/constraint factories use one
  registered native evaluator family, including subset, flatten, and
  Shadow-origin paths, with their final deterministic merge-order fix rechecked
  against the converged install;
- [x] native `check_dependencies()` and BASE-Shadow constraint-plan boundaries
  are integrated with focused graph, classed-input, callback-once, and
  malformed-state regressions; final combined-install evidence remains below;
- [x] values, domains, params, dependencies, transformations, subset/flatten,
  design, and sampler operations are capsule-authoritative and contain no
  semantic fallback or generated-R6 authentication; the documented cold
  internal-tuning and exact-TuneToken search-space families are the two R
  semantic-orchestration exceptions; cold clone and detached equality remain
  non-semantic shell/presentation glue;
- [x] all temporary former-auth aliases and obsolete translation units are
  deleted;
- [x] every registered routine has one fixed signature, direct probe, and
  synchronized coverage ledger.

### Tests and docs

- [x] contract-first design and compatibility documents replace conflicting
  old design guidance;
- [x] NEWS/DESCRIPTION/NAMESPACE begin the 2.0.0 contract reset;
- [x] all tests that assert superseded private/sentinel/S3 behavior are removed
  or rewritten, with preserved ordinary behavior still covered;
- [x] the exact focused ordinary-value matrix covers missingness, type/length,
  integerish, lower/upper bounds, factor membership/type mismatch, checked
  assignment, and Domain/ParamSet scalar/table message parity;
- [x] the historical candidate's complete capsule, graph, callback/reentry,
  structural-versus-semantic
  ALTREP/S4, direct-assignment versus `set_values(.values=)`, shared public-
  table classifier/row-name/cache/name-reentry, semantic-column, data.table
  facade, zero-column Design, corruption, serialization, exact-
  TuneToken/receipt/capability, and upgrade contract suite passes on the former
  frozen candidate payload, authenticated through the sealed `a4617ca` to
  `10c6a0e` package-payload equivalence proof and exact-candidate static/memory
  stages; this does not validate the active migration payload;
- [x] recursive migration tests cover identity, sharing/cycles, attributes/S4,
  environments/closures/bytecode, active-binding non-invocation, promise
  non-forcing, traversal boundaries, current-core payloads, full-preflight
  failure, monotonic retry, first-use modes, exact owner bridges/retired fields,
  authentic Paradox-1/downstream fixtures, and hostile malformed state;
- [x] package reference documentation, vignettes, migration guide, website,
  and downstream bridge docs describe the new migration behavior consistently;
  the active 17-workload documentation stage passes every mandatory row, while
  the unrelated `mlr3book` full-render and two legacy `mlr3gallery` dependency
  rows remain advisory exclusions;
- [x] historical routine/analyzer/runtime ledgers dynamically discovered their
  candidate files and contained no hard-coded test counts; the final old-R run
  staged the reviewed `mbo_config` Git-object bundle before worker admission and
  executed its upgrade test without an environment skip;
- [x] the registered-routine inventory and bounded-rchk policy are regenerated
  for the frozen graph routine; the exact discovery2 report is reviewed below;
- [x] R-API-exception, remaining symbol-audit, and runtime ledgers are verified
  for non-forcing promise inspection in the final exact-candidate gates.

### Downstream coordination

- [x] local bbotk bridge `4d49750` and miesmuschel bridge `d4c7f79` form the
  recorded pre-migration baseline;
- [x] bbotk adds the exact additive legacy-Codomain owner registration and
  miesmuschel adds the exact legacy-Shadow replacement registration, retired
  `params_unid`/`set_id` behavior, and any required owner-local cold gateways;
  both receive focused explicit/first-use migration tests on both Paradox axes;
- [x] the tested post-migration bridge trees are committed as bbotk `29f1806`
  and miesmuschel `2734db0`; the latter keeps its unavoidable dual-version
  namespace rebinding allowlisted and registers relocking before the first
  historical target is changed;
- [x] mlr3mbo `1a1c0ab`, celecx `6da5102`, mlr3pipelines `c85b2f4`, and
  mlr3fda `c1cdad5` are prepared on their recorded branches; the mlr3
  `35e30a9` and mlr3fselect `ae8e1d1` diagnostic-only PRs are documented for
  closure without replacement;
- [x] all eight profile heads were authenticated against the last sealed
  candidate;
  the complete priority-zero/one Paradox-2 repository corpus ran once with
  `jobs = 2` in 14 admitted waves, the five final Paradox-2 source-package
  checks are green, and the Paradox-1 five-package conclusion is explicitly
  composed from three exact final-head rows, the reviewed Paradox-1-neutral
  mlr3fda snapshot-only delta, and the final miesmuschel rerun;
- [x] diagnostic-only downstream changes remain pruned and the current
  committed bbotk/miesmuschel owner bridges pass focused tests plus authentic
  default/opt-in migration fixtures against the then-reopened Paradox-2
  development payload;
- [x] the scoped priority consumer corpus and active documentation are rerun
  against the exact frozen payload and reviewed bridge heads: 20 of 28 exact
  repositories are green, eight are reviewed non-Paradox/environmental
  exclusions, and all mandatory documentation rows pass;
- [ ] user has manually pushed branches and opened the required PRs (agents
  have no remote-write authorization).

### Performance and correctness

The exact frozen candidate's source-bound bounded-rchk discovery is under
`.local/checks/serialized-migration-release-8797f11-memory-20260724/modes/rchk`.
It analyzed 870 functions and 30,245 states, with 80 reviewed Function blocks,
238 UP diagnostics, and 13 PB diagnostics. Its original raw bcheck report
SHA-256 is
`02b08085ea0fadc906fb8e8fdd3f5211a6eb2a7eb08e922205e69f4d25361072`.
The final combined memory run under validation tooling
`a05cd51a5570c4a674b6c80d6cd38c7898223635`, tree
`c58b6bea97d66e23b542a34864479e28c5e5e02f`, produced raw bcheck report
SHA-256
`0226275247eb16736ab317dfb3e1c7f836ee006fb59683276998632aa120cd9d`
and the same reviewed inventory with ordering-insensitive semantic SHA-256
`f3dc5caccc4508f9f9263d8d912455820dfba428cb00f7d0454e1734adf6da18`.
The original and final raw reports are not byte-identical; their sealed
semantic comparison, not a raw-byte claim, supports reuse of the review.
Maacheck is byte-empty
(`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`).
Fficheck reports 79 registered functions and one checked registration call
(`565392164712e15df4bbdd0b34fb852b35eeab380f612c0983f2ccab69c31370`).
The final policy, block table, and unchanged rationale table SHA-256 values are
`e6010f58c58dfb8e952ee0e515a1a2352decd143e01bda50af7c800b4aa0470d`,
`50445fd2be3da7cbeb05f689377802deac7597ee2eeed8acc34f683989e3a71d`,
and `c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
The final combined GCT/Valgrind/rchk completion SHA-256 is
`a07df9509c78976d6d8a31a9b4f3415787f512e73bf85a2311d6ced672e9b226`.
The `a05cd51` tooling diff is package-facing-source identical to the candidate;
it is not described as package-identical.

The preceding discovery exposed a real protection imbalance in
`schedule_vector()`. R explicitly permits an ALTREP `Duplicate` method to
return its input, but the former implementation protected both the input and
duplicate result and then used pointer inequality to decide whether to release
the second protection. The replacement keeps one indexed root and uses
`REPROTECT`; the candidate also contains a test-only ALTREP list whose
`Duplicate` method returns itself and a graph-discovery regression that captures
R's direct stack-imbalance diagnostic. `schedule_vector`'s former one-UP/two-PB
block is absent. The 80 current blocks and rationale assignments are the exact
reviewed inventory. The retained `a05cd51` combined memory completion, not
discovery alone, closes the release gate.

The following historical results and hashes bind the superseded pre-migration
payload. They remain useful engineering evidence but do not close a release
gate for either the last sealed candidate or the reopened cleanup source.

The historical candidate's rchk policy bound the refreshed reviewed
public-table source and its bounded-analyzer reports. Bcheck
analyzed 782 functions and 28,140 states, with 77 exact Function blocks, 196 UP
diagnostics, and 13 PB diagnostics; its report SHA-256 is
`4a405e12807da7ee5347a6ad610530fc41b0f5331bd65221285399fcc2e12655`.
Maacheck is byte-empty (`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`),
and that source's fficheck reports 72 registered routines and one checked
registration call
(`92364873a511f8bd20e0f64854db8ed749d20b9e5d5d6acb730dcb3a045da39f`).
The generated policy, block table, and rationale table SHA-256 values are
`d4b4c38a683b4e6f5110bf83d41eb4a0909a2d42fbea1b25723ed0d4265a46c1`,
`a9b33fb5d53180fc1e1d688cc0b7c5a549d03d901ba199d78ae6a35927132d4b`,
and `c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
The first pre-freeze report exposed a real `snapshot_dependencies()` root-
lifetime defect across callback-capable feasibility validation. The result and
its columns now remain protected through that validation, a successful
allocating-callback regression covers the commit path, and the superseded raw
run was discarded before generating its policy. The refreshed policy includes
the registered `$has_deps` reader; its one address-taken graph-root diagnostic
is reviewed as `ADDRESS_TAKEN_MODEL`. The public-table classifier added three
registered diagnostic routines and shifted only analyzer-generated helper
suffixes and line locations; the reviewed UP/PB block inventory and rationale
assignments are unchanged. The exact frozen-candidate memory gate retained the
same authenticated report and passed Gctorture, Valgrind, and bounded rchk.

The checklist below is the historical `8797f11` payload ledger. It is retained
to explain old evidence and is not the acceptance state of the active R-3.6
candidate. Current candidate progress is:

- [x] strict C99 compilation against R 3.6.0 and every later pinned header/API
  branch;
- [x] an authenticated real R 3.6.3 source-library build/install/test stage,
  including its complete-test closure, the precise active-binding and
  list-ALTREP capability results, and the exact bounded
  four-missing-Suggests package-check NOTE;
- [x] the separate exact R 3.6 declared-floor install/smoke authenticates every
  package identity and dependency namespace origin plus the candidate Paradox
  DLL, with ambient `R_DEFAULT_PACKAGES` isolated;
- [x] a complete four-runtime selection seals the exact
  R 4.0.5-to-R 3.6.3 current-v2 serialization handoff; a partial selection
  makes no cross-runtime claim;
- [x] the source-derived bounded `NOT_CRAN=true` GC/reentry slice passes all
  exact selected targets under both R 3.6.3 and R 4.0.5, including the
  selector isolation self-test and independently regenerated title filter;
- [ ] an authenticated hosted Windows x86-64 R 3.6.3/Rtools35 source-build,
  PE-DLL load/registration, and focused smoke/check artifact;
- [x] the current-R full native lane verifies both sealed ConfigSpace
  environments and all locked local CRAN/BioC repository indexes before and
  after its networkless, read-only test and package-check execution; its
  functional result is green, but a fresh replayable source proof is still
  required for memory;
- [ ] the complete applicable current-R, portability, memory, compatibility,
  documentation, and benchmark gates after source freeze.

- [x] directly affected development tests pass from stable cached
  installations;
- [x] the complete unit suite, CRAN-style package check, and depends-only check
  pass for the historical frozen migration payload under
  `.local/checks/serialized-migration-release-8797f11-20260724`;
- [x] for the historical sealed migration payload, profiling was closed:
  sparse search-target projection and a bulk-dependency constructor
  transaction were measured no-gos for 2.0.0;
- [x] measured hot-path changes remain intact under the bounded refreshed
  comparison recorded below;
- [x] the historical sealed-candidate benchmark has 77 policy/decision rows:
  73 pass, four bounded marginal reviews, and zero failures;
- [x] strict GCC/Clang, both analyzers, cppcheck, symbol/registration audit,
  ASan, UBSan, and R-API/exception-ledger checks are sealed for the historical
  payload;
- [x] the combined GCT, Valgrind, and bounded-rchk memory completion is sealed
  for the historical payload;
- [x] real R 4.3.3 and 4.5.2 runtime execution plus the current local R 4.6.1
  native execution are sealed for the historical payload;
- [ ] Windows x86-64 and real macOS ARM64 are clean for the exact frozen
  candidate and independently retained;
- [x] priority consumer, documentation, differential, and benchmark gates are
  rerun and accepted for the historical payload.

Current profiling diagnostics are implementation guidance, not release
benchmark evidence:

- the bounded migration-payload comparison in
  `.local/benchmarks/migration-hotpaths-20260724/` ran 200 samples after 10
  warmups for eight representative paths. Seven improved by 4--24% and all
  eight retained identical allocation counts. The sole `ids()` shift was under
  3 microseconds, its R and C implementations are byte-identical to the
  superseded candidate, and a current/current process comparison showed 7.2%
  variance, so no destabilizing source change was made. Authentic legacy graph
  scaling under `.local/benchmarks/migration-scaling-20260724/` is flat through
  128 aliases and linear at roughly 18--20 ms per distinct shell;
- the operation-local SHADOW ID index is retained in
  `.local/benchmarks/dev-shadow-pointer-index-ab-20260718`; its A/B medians
  improved construction by 1.39x, live values by 7.72x (293 to 38 microseconds),
  live domains by 2.17x, and assignment by 1.70x while keeping complete
  corrupt-state validation;
- the direct base checks in the `to_tune(ParamSet)` callback wrapper measured
  4.47 microseconds versus 24.31 microseconds for the former
  checkmate/mlr3misc layers in the focused probe. The final paired gate confirms
  the representative end-to-end workloads on the frozen candidate payload;
- an earlier isolated `check_dependencies()` and BASE-Shadow constraint-plan
  stage compiled with the complete strict C17 warning set under GCC 14 and
  Clang 22 without a diagnostic. Its immutable GCC installation (DSO SHA-256
  `ff1755578568856af74b11f868a7da981b13395c974329f2096ea7e31f76b456`)
  first passed 35 focused test blocks with 241 expectations. After adding two
  test-only first-diagnostic assertions, the unchanged installation reran the
  affected dependency file (14 blocks and 111 expectations), leaving 243
  focused expectations for that source. Its 55 registered calls, 55 direct
  probes, and four hazard probes passed. Evidence is retained in
  `.local/tmp/native-semantic-leftovers-20260718/`;
- the subsequent isolated standalone-Condition and constraint-only stage (DSO
  SHA-256
  `b8de34b72ee5fac27ade0777058f7a7a70ac1d4e7c5d33c0cff025a44f313c4e`)
  passed five affected files, 37 test blocks, and 243 expectations, plus 58
  registrations, 58 direct probes, and four hazards. GCC 14 and Clang 22 were
  warning-clean. Its 10,000-row constraint batch measured 0.0148 seconds per
  call versus 0.511 seconds for the old R row engine, a 34.5x improvement.
  Evidence is retained in
  `.local/tmp/native-constraint-stage2-20260718/`. A later measured
  `CondEqual` pointer fast path and the merged native mutation/add operations
  changed the source again. The vector fast path is now guarded by the
  permanent `condition_equal_vector` workload and regression-policy row;
- a final collection-reader profile attributed about 86% of the representative
  rich-read instruction count to complete graph admission. Within that required
  validation, encoding translation and affixed-ID comparison were measured hot
  spots. Two conservative byte fast paths were retained: equal UTF-8/Latin-1
  encodings and native ASCII compare without translation, while mixed encodings
  and non-ASCII native strings keep the UTF-8 path. With 5,000 samples pinned to
  one CPU and both execution orders, the shared string change improved rich
  reads by 1.127--1.131x and nested reads by 1.253--1.294x; the affixed-ID change
  then improved plain reads by 1.105--1.121x, rich reads by 1.083--1.104x, and
  nested reads by 1.106--1.115x. Allocations were unchanged. The tested stage
  DSO was
  `40784de682305cd1ca7322b375e5504aa3b483428944b149e9d6958e47503ea7`;
  retained A/B evidence is under
  `.local/benchmarks/collection-values-ab-{string,affix}-long-20260719` and the
  corresponding `-reverse-` runs. Translation caches, alternate validation
  modes, and skipped corruption checks were rejected;
- a fused Shadow-values reader experiment was also rejected and fully reverted:
  100-sample exact A/B medians were 60.345 versus 60.205 microseconds (1.002x)
  with identical 2,200-byte allocation. It added a routine and duplicated
  reader surface for no material gain. Evidence remains under
  `.local/benchmarks/fused-shadow-values-ab-20260719`;
- the final low-hanging pass retained four compact changes: skip an empty value
  transaction when a ParamSet has no initial values, reuse the already resolved
  BASE row while translating admitted collection values, and let the inherited
  native Shadow dependency reader own refresh. It also replaces the
  `$has_deps` dependency-table/data.table projection with the registered scalar
  reader described above. Forward/reverse paired evidence for the first three
  is retained in `.local/benchmarks/final-hotpath-ab-20260719` and
  `.local/benchmarks/final-hotpath-ab-reverse-20260719`. Small construction was
  4.3--6.5% faster with 880 fewer allocated bytes; 64-parameter bulk
  construction was 8.0--12.3% faster with 1,744 fewer bytes; rich collection
  reads were 4.6--5.8% faster and nested reads 18.1--18.8% faster. Plain reads
  remained within 1% timing noise. Collection reads used 192 additional
  operation-local bytes. The production delta was 21 source lines and 320 DSO
  bytes, with no persistent cache or weaker validation mode. The separate
  `$has_deps` A/B evidence is retained under
  `.local/benchmarks/has-deps-scalar-ab-final-20260719`: 50 evaluations after
  three warmups moved the median from 498.575 to 156.065 microseconds (3.195x),
  with the same 12,688 bytes in 14 `Rprofmem` records on each side;
- the paired release policy now records the unavoidable major-version integrity
  cost rather than treating it as an ordinary hot-path regression. Exactly
  seven rows use integrity tiers. `shadow_values_live` receives the finite
  `integrity-shadow-read` median/q75 ceilings 3.25/3.50. The three synthetic
  `collection_values_{plain,rich,nested}` rows and the three real consumer
  `$values` rows for `mies_mutator_maybe`, `mies_optimizer`, and
  `mlr3pipelines_graph` receive `integrity-collection-read` ceilings 2.75/3.00.
  Post-index Shadow profiling measured 2.623/2.605; the final-focus rich
  collection diagnostic measured 1.546 at the median, while the last common
  pre-final-fast-path nested diagnostic measured 2.361/2.349. The collection
  ceiling still rejects the retained pre-optimization 3.365/3.591 stage.
  Consumer `$params`, `get_values_unchecked`, filtered getters, domains,
  dependencies, mutation, and all other real consumer operations keep their
  strict ordinary tiers; the integrity rows also retain the `hot` allocation
  budget.
  The sealed release benchmark subsequently recorded 73 pass, four bounded
  marginal reviews, and zero failures. The four marginals are
  `shadow_values_live` timing plus allocation for
  `collection_values_{plain,rich,nested}`; the three real consumer `$values`
  integrity rows pass;
- a sparse-target `$search_space()` facade experiment was rejected. The
  conversion is cold and the representative maintained end-to-end workload
  moved only about 2%, which did not justify an additional projection path and
  validation surface;
- a native bulk-dependency constructor transaction was also rejected for this
  release after measurement. The isolated 64-parameter/27-requirement estimate
  moved from 6.57 ms to 4.11 ms, with requirement-heavy estimates spanning
  roughly 1.4--2.5x, but representative xgboost learner construction improved
  only about 6--7%. Implementing it requires a moderate-risk new native batch
  transaction, and the maintained release workload currently lacks dependency-
  rich constructor coverage. Under the release-steering policy this is not
  low-hanging enough to reopen the implementation. The existing low-risk wins
  remain; this internal optimization can be reconsidered later without another
  compatibility/API break and is not included in any claimed speedup above;
- none of the staged DSOs above is current combined evidence. The candidate
  table and retained-evidence record below name the immutable source and the
  applicable release gates; those final rows, not the development diagnostics,
  support the local release conclusion.

### Historical informative-diagnostic focused evidence

The informative-diagnostic source before the object-graph migration had bounded
evidence appropriate to that change. It is historical for the current payload
and is not a replacement for the complete release matrix:

- the ordinary GCC 14 development install has DSO SHA-256
  `2d1636ab5e0c10e23336f7bf33389e5963facdfaa08f7e23e2dd84372ecc4b49`;
- 68 ordinary Domain, ParamSet, constructor, and checked-assignment diagnostics
  are byte-identical to Paradox 1.0.1.9000, including cross-storage scalar
  missing values. Both retained TSVs have SHA-256
  `1d466842f042b60769a65d49aea54e95f4ff17f8cc8b592e1a7ded42343e3307`
  under
  `.local/checks/informative-diagnostics-final-differential-20260723/`;
- the affected package tests, exact constructor/assignment assertions, and a
  bounded `gctorture2(10, 1, 0)` formatter/admission loop pass. Strict GCC 14
  and Clang 22 C17 warning-as-error builds, the bounded analyzer corpus, all
  registered direct probes, and all four callback/allocation hazards pass under
  `.local/checks/informative-diagnostics-final-native-r2-20260723/`;
- a CPU-pinned randomized comparison against frozen `10c6a0e` found no material
  accepted-path regression. Raw current/baseline ratios were 0.988 for mixed
  `$check()`, 1.008/1.044/0.962/0.998 for double/integer/factor/logical
  `domain_check()`, and 1.056/1.040 for `p_dbl()`/`p_int()` construction, while
  the process-local control itself was 1.073x slower;
- the same focused six-consumer matrix passes 2,022/2,022 expectations with
  zero failures, errors, warnings, or skips on each of Paradox 1.0.1.9000 and
  the final Paradox 2 development DSO. Evidence is retained under
  `.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
  and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`.

## Current local downstream branches

These are the exact final local handoff heads after the object-graph migration,
owner-registry bridges, informative native diagnostics, and pruning of
redundant downstream adaptations. Their focused tests and retained active
profile checks are complete. Repository policy still requires the user to push
the retained branches and create, update, or close PRs manually. The obsolete
mlr3 and mlr3fselect branches are evidence only and must not be published as
replacements.

| Package | Worktree | Branch | Commits | Intent |
|---|---|---|---|---|
| miesmuschel | `.local/compat/github-release-refresh-20260720/miesmuschel` | `codex/paradox-paramsetshadow-bridge` | head `2734db0d896745926dbe0c14c2ede272affa9495` | Official ParamSetShadow/public-state bridge, Paradox-1 construction, cache-independent comparisons, dual-major Rd links, exact replacement registration, retired `params_unid`/`set_id` contract, and the bounded owner-local cold gateways needed by historical overrides. Its separate official-Shadow graph-boundary diagnostic remains intentional. |
| bbotk | `.local/compat/github-release-refresh-20260720/bbotk` | `codex/public-paramsetcollection-sets` | head `29f18061b03fe1d31bfd2d1955e3fe6be5cec0c0` | Public `.sets` migration, detached-snapshot rooting, and exact additive legacy-Codomain inspector/rebuilder registration without private state access. |
| mlr3mbo | `.local/compat/github-release-refresh-20260720/mlr3mbo` | `codex/paradox2-transformless-subset` | head `1a1c0abe95f59cd314f1fbc19c596cb6ac15f067` (base `d1ce6189b637dd552fac95d56c53a39503bae889`, runtime change `a8a988a64b66e651043b75f63dfdfb4604185e3f`) | Use public `subset(..., keep_trafo = FALSE)` on Paradox 2 while retaining Paradox-1 paths and document the migration. |
| celecx | `.local/compat/github-release-refresh-20260720/celecx` | `codex/paradox2-diagnostics` | head `6da5102ca948b8182aae13575c48a932812b05c6` | Retain only the independent cycle/dependency adaptation and compatible mlr3mbo bridge requirement. |
| mlr3 | `.local/compat/github-release-refresh-20260720/mlr3` | `codex/paradox2-diagnostics` | obsolete head `35e30a9` | Close without replacement; removing its numeric-diagnostic gates leaves an empty effective diff. |
| mlr3fselect | `.local/compat/github-release-refresh-20260720/mlr3fselect` | `codex/paradox2-diagnostics` | obsolete head `ae8e1d1` | Close without replacement; removing its feature-fraction diagnostic gate leaves an empty effective diff. |
| mlr3pipelines | `.local/compat/github-release-refresh-20260720/mlr3pipelines` | `codex/paradox-diagnostic-compat` | head `c85b2f4165e056934f892c5db37391869cd40e38` | Retain only the GraphLearner deep-clone ownership fix and mutation-isolation regression. |
| mlr3fda | `.local/compat/github-release-refresh-20260720/mlr3fda` | `paradox2-snapshots` | head `c1cdad5a78913c9a47fec1003de8d4309275c80c` (base `8f5a3dfa297ad236812cda57fab02de75fec375a`) | Preserve byte-identical Paradox-1 messages; the four Paradox-2 headers now name `.__paradox2_ParamSet__values()` while their diagnostic bodies remain unchanged. |

The final Paradox-2 source-package stage builds and checks bbotk, miesmuschel,
mlr3mbo, celecx, and mlr3fda from their exact Git archives; all five rows and
all five retained final statuses are green. The Paradox-1 source-package
conclusion is an explicit composition. The r2 stage passed the exact final
bbotk, mlr3mbo, and celecx heads plus mlr3fda base `8f5a3df`; final mlr3fda
`c1cdad5` changes only four call headers in
`tests/testthat/_snaps/paradox-2/PipeOpFDAWavelets.md`, so its Paradox-1
selected tests and runtime source are unchanged. The final miesmuschel row
passed in r3. The r2 stage's obsolete miesmuschel row failed, so neither that
whole stage nor the final mlr3fda head is described as an exact five-head
Paradox-1 check.

The handoff in `compat/downstream-pr-handoff.md` records the final retained
heads, manual push/PR text, and the two redundant PRs to close. For historical
context only, the same focused 2,022 expectations passed with zero failures,
errors, warnings, or skips on both superseded Paradox axes under
`.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`.
Those runs and the earlier `bf64490` to `9e87556` test-only composition belong
to the superseded payload and do not authorize the active result. The active
`release-refresh-20260720` profile has separate candidate, tooling, overlay,
axis, and source-check receipts. A check is green only when its retained final
status has no ERROR or WARNING, independently of process exit status.

## Development evidence policy

While source is changing, record only focused diagnostic results. A source
parse, strict translation-unit compile, or focused test is useful development
evidence but not a release gate. Broad checks are deliberately delayed until
the architecture converges so that package/dependency binaries are not rebuilt
and entire suites are not rerun for each isolated failure.

Use one stable copied source tree and one disposable installation for a
coherent batch. Run independent test files/consumer rows in memory-aware outer
parallel waves, with nested compilation/test/BLAS/OpenMP at one. Mine every
failed wave for its full failure set, fix the shared cause, rerun affected rows,
then perform one final broad confirmation.

Authenticated toolchain, package-download, dependency-library, consumer-install,
reference-source, header, analyzer-runtime, and container caches remain valid
when their byte-affecting inputs match. A candidate DSO, package installation,
memory report, differential, consumer result, documentation result, or
benchmark is not transferable across a changed distributable payload.

The 2026-07-25 unknown-parameter-suggestion cleanup has development evidence
only: an incremental package installation; warning-as-error GCC 14 and Clang 22
C17 compiles of the changed translation units (also independently reviewed
against R 4.3 headers); focused `NOT_CRAN=true` runs of the new suggestion,
native ParamSet check, native value-mutation, and characterization
value-mutation files; and 500 randomized comparisons with the intended
Paradox-1 `mlr3misc::did_you_mean()` policy. All passed. No broad, downstream,
memory, or release benchmark gate was rerun for this isolated item.

The dormant-values/default-aware-activity batch follows the same development
boundary. During implementation, strict changed-unit compilation and focused
dependency/value/check/constraint tests are useful diagnostics only. The full
unit, compatibility, runtime, memory, documentation, benchmark, and hosted
portability gates are intentionally deferred because additional pre-release
todos remain. Any focused result must retain its actual source identity and
must not be entered in the candidate evidence tables below.

A completed package-facing row may be reused across refs only through a sealed,
independently replayed proof that every Git change is excluded by the exact
`.Rbuildignore` and that clean builds have the same complete payload inventory
and bytes after removing only R's generated `Packaged:` record. The donor run
keeps its original identity; the target ledger names the proof and transfer
scope. Tooling/policy/docs/profile/benchmark/portability inputs are not covered
by package-payload identity. A downstream test-only change reopens that
package's affected rows, not consumers whose head and production source remain
unchanged.

For an R/docs-only inner-loop change, a development DSO may be reused only
after recording byte identity of every native build input plus compiler/profile,
`NAMESPACE`, and `DESCRIPTION`, reinstalling the R/help databases, and verifying
the loaded DSO hash. This exception is diagnostic-only. The final immutable
distributable payload receives one clean full source build per executed
R/compiler/instrumentation profile; compatible evidence families may share
that exact authenticated installation, and a sealed identical-payload ref may
inherit the donor conclusion, never development component objects.

## Last sealed candidate freeze record

The last sealed immutable package candidate, now historical after the
2026-07-25 cleanup reopened package-facing source, is:

| Field | Value |
|---|---|
| Full candidate ref | `refs/paradox-release/candidate-20260724T105215Z` |
| Commit | `8797f1163fe612cb01d1facf517834d3f516a697` |
| Tree | `81e6f901266754b97a0906f88a472bf04795f13c` |
| Candidate content SHA-256 | `ced2390bc756b01805e7bdcf32fdb4a6ff2c1bd010dd3d576194d939e491f644` |
| Version | 2.0.0 |
| Exact bounded-rchk discovery | `.local/checks/serialized-migration-release-8797f11-memory-20260724/modes/rchk` |
| Final memory tooling | commit `a05cd51a5570c4a674b6c80d6cd38c7898223635`, tree `c58b6bea97d66e23b542a34864479e28c5e5e02f` |
| Final documentation/consumer/benchmark tooling | commit `fc92edd7f1ab612468066fe06bd3d9fc7afea41c`, tree `05cc4e5213c5ee73d0bc764c3d102c15e4c57141` |
| Portability companion | `refs/paradox-release/portability-harness-5305ead`, commit `5305eaddbc9c2159fe194e6be10388c17b4c506a`, tree `e1fe00ddad08af89566e3df12460bc47d3e98292` |

This ref froze package source, package tests, help, and package-facing
documentation at that point. The exact candidate-to-`a05cd51` and
candidate-to-`fc92edd`
diffs contain no package-facing path; these relationships are
package-facing-source identity, not claims of complete package-payload byte
identity. The later cleanup source is not identical to this payload. Its native,
R API, runtime, combined-memory, differential, downstream, documentation,
benchmark, and hosted portability conclusions must be established after a
replacement candidate is frozen.

## Historical candidate freeze record (superseded payload)

The table below records the superseded `10c6a0e` payload only. It does not
describe the last sealed candidate above and authorizes no current release
conclusion.

An immutable package candidate is committed after package implementation,
tests, help, and package-facing documentation converge and the primary checkout
is clean. A commit cannot contain its own commit, tree, or archive identity
without a circular mutation. Therefore the candidate's own copy of this table
and the axis registry is necessarily pending. This post-freeze ledger records
the resolved identity below. The package/release tag continues to point to the
candidate, never to validation infrastructure, the final evidence ledger, or a
portability harness.

Post-freeze validation infrastructure may populate exact candidate/profile rows
and repair validation-only drivers in `AGENTS.md`, `benchmarks/`, `compat/`,
`design/`, `environment/`, and `scripts/`. Each such commit must remain clean
and prove that its diff from the candidate changes no package source, package
tests, help, or package-facing documentation. Source-bound results still name
the managed detached candidate, while each validation result separately records
the tooling commit/tree/status that produced it. Ordinarily freeze one final
validation-tooling commit and reuse its named overlay read-only. The recorded
release composition is narrower and explicit: documentation and benchmark used
`bf64490`; `9e87556` changes only the final miesmuschel test head/profile ledgers,
so new final overlays and the affected miesmuschel rows were built on both axes
while unaffected conclusions retain their original identities. This is the
non-circular model for the `release-refresh-20260720` profile, not authority to
mutate candidate bytes, replay arbitrary older tooling evidence, or relabel
execution.

After the remote gate and publication handoff complete, the final evidence-
ledger commit changes only this file and changes the decision from pending to
accepted. The portability companion changes only
`.github/workflows/r-cmd-check.yml`. Creating either reopens only its own
structural and ledger checks. Any package-facing post-freeze change requires a
new candidate and new source-bound evidence.

| Field | Value |
|---|---|
| Full candidate ref | `refs/paradox-release/candidate-20260720T053518Z` |
| Commit | `10c6a0e65910206c8face91dac6c3dd1115e0bed` |
| Tree | `a205205194f0bc62114106504853721f678fa340` |
| Detached source | `.local/compat/candidate-snapshots/10c6a0e65910206c8face91dac6c3dd1115e0bed` |
| Clean `R CMD build --no-manual` archive SHA-256 | `917ea2a497f9e80ce4cf1d10c081cea8914d36eb7deb647c036dca49994ad558` |
| Deterministic Git archive SHA-256 | `0a712fa5c5f572ca2f2968fda211973fe0bede45b80a7fcee0848af863b1d7ca` |
| Normalized 217-file package-payload manifest | `e6e767b8fa3cd1a9273d62039c208d7c3ae1aea0b12d7446560297d4750dcc3e` |
| Version | 2.0.0 |
| Tracked source files | 486 |
| Routine inventory | 72 rows; `environment/native-routine-coverage.tsv` SHA-256 `bc7a9b382e62372954b9e197906d30733e37147b5f09c82ebf371b63cf30fa4c` |
| Test inventory | 84 `tests/testthat/test*.R` files; exact `mode/content-SHA-256/path` slice SHA-256 `8c7118167fe1e82e6c756661bf5a3a13d3795093e4c11a9982f3da9392551ebf` |
| Final downstream-profile tooling | commit `9e875567ef0462e659906dd6aa0acfdc8fba3044`, tree `107ce3e060932000701d872024494971878675f8` |
| Downstream bridge manifest | `compat/github-bridge-provenance.release-refresh-20260720.tsv`, SHA-256 `79596dc32d9030e4b86bfc13310bad277fe9070577a2ff703b295262378e9e5f` |
| Portability companion | `paradox-2.0.0-ci-10c6a0e-harness-cc06c18`, commit `cc06c182949af09ce80e335ddbfc63a8078692e6` (remote evidence pending) |

The eight exact bridge heads are bbotk `6cae9559cfa2133b02b19e9762211aa49ec4c1c7`,
mlr3 `35e30a91e305936e57328b65e15b60f3ab00eef3`, miesmuschel
`d4c7f79750cd15c8174415fb0ba059c597f4f055`, mlr3pipelines
`1c4bc6e52005d40d61fdba27b047f09fd6a6d29a`, mlr3fselect
`ae8e1d163bc7d8a2dd9f12e61d704e5b0d8430d7`, mlr3mbo
`1a1c0abe95f59cd314f1fbc19c596cb6ac15f067`, celecx
`a2975550c14f824c6abc86db9db32e982908c3ea`, and mlr3fda
`035da5bb8d1c2ae22f04898718355e9653c382b2`.

No annotated tag or remote branch is created by an agent. The user performs
all remote writes after reviewing this record.

## Mandatory release evidence

For the exact candidate ref, retain and verify:

1. strict GCC and Clang C99 warning-clean builds, registration/probe audit,
   ASan/UBSan, complete unit tests, examples, and `R CMD check --as-cran`;
2. real R 3.6.3, 4.0.5, 4.3.3, and 4.5.2 runtime stages plus development R;
   compilation against all seven pinned R 3.6.0--4.6.1 header axes; and the
   exact raw-attribute/hot-closure-formals/stored-binding/promise exception
   ledger, raw-token/version-gated DSO audit, and option-access symbol policy.
   This includes the authenticated R-3.6/R-4.0 complete-test source-package
   closures, the R-4.3 data.table 1.18.4 overlay, the separate exact R-3.6
   declared-floor smoke with `R_DEFAULT_PACKAGES` isolated, and the full-only
   sealed R-4.0.5-to-R-3.6.3 serialization handoff;
3. normalized Paradox-1 differential with reviewed intentional 2.0 deltas;
4. every exact default head in `compat/github-bridge-provenance.tsv`, the four
   superseding heads in the `release-refresh-20260720` profile against both
   Paradox majors, then priority-zero/one reverse dependencies and maintained
   mlr-org repositories;
5. GCT, instrumented-R Valgrind, bounded rchk, direct routine/hazard probes,
   and adversarial corrupt-capsule/graph/ALTREP cases, treating hostile
   state-changing custom ALTREP as a safety/no-replay gate rather than an exact
   representation-equivalence gate;
6. package manuals/vignettes, active book/gallery/website/cheatsheets, and
   pure/recursive/default/opt-in/owner-bridge legacy serialized configuration
   upgrades;
7. GitHub current Windows x86-64, exact Windows x86-64 R 3.6.3/Rtools35, and
   macOS Apple-silicon ARM64 checks whose failure status is correctly
   propagated and whose exact source provenance is retained;
8. representative paired benchmarks on an idle host, including downstream
   call patterns, with raw distributions and regression thresholds reviewed.

Every accepted row receives a unique run ID, exact source ref/commit/tree,
commands, versions, logs, manifests, and completion seal. A transferred row
retains its donor execution identity and additionally names the target and exact
equivalence proof; verifier-only changes never relabel old execution as a new
package run.

### Historical provisional R 3.6 release-core evidence

The exact immutable ref `refs/paradox-release/r36-39855c9`, commit
`39855c919beec323fd5940e4c46286e8df1be8ff`, tree
`48e347b0c31a0f7ea966458586d13c1ab9d82764`, passed all eight tasks in
`.local/verify/runs/r36-release-39855c9`. This included strict native,
seven-axis header/API, differential, all four supported runtimes, declared
dependency floors, old-R stress, and the cross-runtime serialization handoff.
The coordinator completion, summary, native completion, API/header completion,
runtime top seal, and differential seal SHA-256 values are
`4a5d240f88ca5ed7593e0322e38749d38c515e02f5cb951b809d72d780225847`,
`8b5b7026d75dd7718d52ad7af7dbb5025e45e5a0bf26635714f3c6e933778ecd`,
`e12a74029b671db15bd5005b11aa4c243e7635e4d7efd67a1dd7eebe5e689cb6`,
`a9deab9d413adf7a0063b8665bac9df074ec324a535187cbd70df13be78415c2`,
`baf2e8462528d2814a06557b15d5f250cda7551dc84c7c2395210f870bbde58b`,
and
`18883e5b93c7625dd6d17710e23b00907e453f73e882f32c179e173d7d6c87b7`.
`release-core` excludes the separate combined memory gate. More importantly,
the subsequent graph-frame lifetime repair changes native package source, so
no row in this otherwise successful run transfers to the replacement
candidate.

### Historical retained local release evidence for `8797f11`

| Gate | Retained evidence and result |
|---|---|
| Full native, sanitizer, tests, and package checks | `.local/checks/serialized-migration-release-8797f11-20260724`; completion `0f1a0543ed0226ad956ec7a29114be7132f0de5945bcf2fd1a433ca7b325a998`; complete suite, CRAN-style and depends-only checks, strict compilers, analyzers, and sanitizers pass |
| R API/header matrix | `.local/checks/serialized-migration-release-8797f11-r-api-20260724`; completion `b979909684d10434056241670a241ba113427af5284f60155b0f523043116b80`; four R versions, 35 translation units, and two compilers pass |
| R 4.3.3 and 4.5.2 runtimes | `.local/checks/serialized-migration-release-8797f11-runtime-20260724/runtime-matrix`; top completion seal `33d0869e2af7416594a85c5e3cda500b6785ca169431b4e8e1adf35c99cea193`; each runtime passes 88 files and 6,227 expectations with 18 expected skips |
| Combined memory/adversarial | `.local/checks/serialized-migration-release-validation-a05cd51-memory-final-r1-20260724`; completion `a07df9509c78976d6d8a31a9b4f3415787f512e73bf85a2311d6ced672e9b226`; GCT, Valgrind, and bounded rchk pass, with the raw-versus-semantic analyzer identities recorded above |
| Exact Paradox-1 differential | `.local/compat/differential/runs/20260724T152627Z-3659648`; manifest `15fc85a7cf2236dd4f17eb8d36ab63c45e2920ea37d10f348825842e33e513e9`, seal `1055758ff59aee28ab9c67e3e35f0f8dfda3baadcace6a274abf0b78c71dad66`; 26 cases, three equal, 23 reviewed expected differences, zero unexpected |
| Paradox-2 exact source-package checks | `.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1/repository-checks-release-refresh-20260720-paradox2`; independently verified, five rows, zero failures, five final `Status: OK`; completion/results/manifest/seal `a43064cd48b78fd433b4a4995a2da9cfad422da7b3b66f53391de05fbd289179` / `f443e9ace27450057e4c8fcb6f9d93da85d595028abfff133272cc32f11a364c` / `aa6d0ad380f0453db8c87888bdd5b7d18d54bc2698d21b9a4e67ce1040bcd731` / `7de4719e3fa0016fb05d804aa240dea34a0e5de76763c7f1d5d885e2e581acf1` |
| Paradox-1 source-package compatibility | Exact final bbotk/mlr3mbo/celecx heads and mlr3fda base `8f5a3df` pass in `.local/compat/runs/migration-release-final-p1-cdcc8e6-221c95e-r2/repository-checks-release-refresh-20260720-paradox1` (manifest/seal `e224bc9a13043d6af6ad3715f3c9cc285e3095f2d53b7389094f90dff59bb0bc` / `c0ddb3e7de1d73a890b845f7cb19e6ff61cb5fb1e12eada0d868a6e34c54cfba`); final mlr3fda `c1cdad5` changes only the Paradox-2 snapshot file. The exact final miesmuschel pass is retained in `.local/compat/runs/migration-release-final-p1-cdcc8e6-2771f5d-r3/repository-checks-release-refresh-20260720-paradox1` (manifest/seal `4f56dc540e92e82474a2341774533963614ea11f43dfc4c7d11b7e89a7d41d57` / `b21a13cd65f08ba2094deddd5fd70876ca5573c92722f0fb972868107454860d`). The r2 stage itself is `completed_with_failures` because its obsolete miesmuschel row failed. |
| Scoped broad repository corpus | `.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1/repository-tests-priority-1-release-refresh-20260720-paradox2`; `jobs = 2`, 14 waves, 20 of 28 exact repositories green; completion/manifest/seal `cd0de96b8617d37d1f5e8e88ebb39ea34e02fae2378a5d198d215da813c6703e` / `376ed637f497a0980599396c6f1fffa52e087b68794e4cce765af1471c310c41` / `611ed1b2745143b34910f56ed1d6a3ce4f14031728955e1d4fe9d6cf389d4b9c` |
| Documentation | `.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1-documentation/documentation`; 17 workloads and every mandatory row pass; manifest/seal `dbd01bf40a1e6c776e8d82ea7d93b3494622b3c28b6fb9e21635257dc12ff528` / `0b2f5cf1df7dd386c34660daf8090fa6dbf135a7bba278ddb5ad85df5c49fef7` |
| Sealed release benchmark | `.local/benchmarks/serialized-migration-release-8797f11-sealed-r5-fc92edd-20260724`; 77 policy/decision rows (68 workloads plus nine consumer operations), 73 pass, four bounded marginal reviews, zero failures; completion/manifest/seal `5d615637bd6448bb95b8ba798cc8f7e23d40a78e1a63a4e3988eb436914109a0` / `a16d403c20ed8ab80233a877e81ec146b4a5c128fdf0a765be440993eb325499` / `d7a4bbefdba3e1d0a800b2e6bdae6e10ddde22b612cf05bebc5d019eba198fc7` |
| Windows/macOS portability | Direct-child companion `refs/paradox-release/portability-harness-5305ead`, commit `5305eaddbc9c2159fe194e6be10388c17b4c506a`, tree `e1fe00ddad08af89566e3df12460bc47d3e98292`; sole workflow SHA-256 `14c4c8d1cc8d8e07aea1829d1203f6217464ae9c6b94efc1050bf192638196e3`; hosted Windows x86-64/macOS ARM64 execution and retained artifacts pending |

The eight scoped broad-corpus exclusions are `mlr3tuningspaces`, `mlr3cluster`,
`mlr3filters`, `mlr3torch`, `xplainfi`, `mlr3extralearners`, `mlr3forecast`,
and `mlr3resampling`. Their retained logs diagnose upstream API drift,
optional-runtime or external-system absence, package-local defects, or bounded
environmental timeouts rather than a Paradox failure. The documentation stage's
nonrequired `mlr3book` full render is excluded for unrelated
`mlr3fairness::MeasureFairness` API drift; two legacy `mlr3gallery` rows are
excluded for the absent `distill` dependency. Focused Paradox book, website,
cheatsheet, `mbo_config`, and target rows pass.

### Historical retained local release evidence (superseded payload)

| Gate | Retained evidence and result |
|---|---|
| Package-payload equivalence | `.local/checks/package-equivalence-a461-10c6`; evidence manifest `e558864a318a465edf058013f743c6ae386b34bcd7894037a02c66938a986fb3`, completion `71de496c80ef836b3f3a8ce32a59883adf7c847b41eaa39dc05dc31dd8262a65`, normalized payload `e6e767b8fa3cd1a9273d62039c208d7c3ae1aea0b12d7446560297d4750dcc3e` |
| Exact candidate static/native source | `.local/checks/release-final-20260720T053518Z-10c6a0e-native-static`; completion `b42b77a87c210f5e2318bb0b25ec03abc16d220c5a99ab674c12ce2cfcde3b05` |
| Full native, sanitizer, tests, and package check | donor `.local/checks/release-final-20260720T022410Z-a4617ca-native`; completion `2b873adcc505de6e9cd7a4f08a61806bcff83417d1afc9cb901f972eeb95356f`; transferred only through the package-payload proof above |
| R API/header matrix | donor `.local/checks/release-final-20260720T022410Z-a4617ca-r-api`; completion `17e068591a3c7b59766ee5c41a55bd81c64830546097b8411a444334b1762725`; transferred only through the package-payload proof |
| R 4.3.3 and 4.5.2 runtimes | donor `.local/checks/release-final-20260720T022410Z-a4617ca-runtime`; top seal `2f396214bf4e3d53751b6306ac99531a71ee1e0e77fc4b7b3366f36dd546b235`; 84 files and 5,720 expectations on each runtime; transferred only through the package-payload proof |
| Exact candidate memory/adversarial | `.local/checks/release-final-20260720T053518Z-10c6a0e-memory`; completion `3220ee02820baf7e4ba9dfe90dacd79132f044bf088d045e4d8fb70036003d15`; Gctorture, Valgrind, and bounded rchk pass |
| Exact differential | `.local/compat/differential/runs/20260720T072548Z-375776`; manifest `460869a0511691eb4684dbb7bcc5a5a437b6e4477d95932892cbb808df27a9fd`, seal `1c485dfcad2e92a760e910a9ca805c5af98a2b55478689ff487284d07fe245c4`; maintained baseline `06091b5b64a78807d332ec95c5cdc1aaac5899b9`, 26 cases, 23 reviewed differences, zero unexpected |
| Exact payload priority consumers | donor `.local/compat/runs/release-final-20260720T022410Z-a4617ca-r3/repository-tests-priority-1-release-refresh-20260720-paradox2`; seven of seven passed, manifest/seal `e5dbb7ed0bf760c2082189aa9428500db05eeba787fd11ba99afaa03165db383` / `4adc9a9f1d0afd7ad515ccc939143ecf173f6fb3f4218f05ac72cf0d9a9860fa`; transferred through the package-payload proof, with final changed-profile rows below |
| Paradox-2 downstream profile | final r8 overlay manifest/seal `32f337eff8e2fe01c6e2575f5a45d631dad1ca3afb8b2c81d037ccb51a4f49be` / `d91de3224009f270ea69fadee13e7a2fdd19e4fc3c4eb2bf26ffc83a8b840fb7`; final miesmuschel suite `b781723ef6935fe2491dba02483c45eb8eb5d4eb34330ccd7d11044de3ffe34a` and check `6b93a0b7de555673aa5489c454a5ce62d267653a747f999e9eb4539b80716d3f`; unchanged three-head full-check manifest/seal from r6 `d1004b19ccdd5ef0ff9198b1b3ef1491aeaefc75c73e99ed19c166e34fd66ae4` / `a47586f5f4837fdd0c8a6583a72b86d54a52d1ab683afd39e6866f7ab51ea446` |
| Paradox-1 downstream profile | final r2 overlay manifest/seal `e48b62a5eb7bf16439cf9bb270965d26c1a8450cd622072a6f85d1124d1982d8` / `2885e9e01b71cc9d2d1276837dfe0fe4ff309b2a8a8269c70bf9dc8903be20ce`; final miesmuschel suite `f6c36c961fe1ebeeafda59b1c99ebc60f857a9f5e0ea06799e17ca65e427f991` and check `8cf57abb41df4743bc14520acbedcd8c9c04cefa6984250e1bde54a8c42209a5`; unchanged mlr3mbo/celecx/mlr3fda passed rows remain in the sealed r1 completed-with-failures stage |
| Documentation | `.local/compat/runs/release-final-20260720T053518Z-10c6a0e-r7-documentation/documentation`; manifest `142079a9a64525c3efbad3ce4f5b04a966581173c00a7ccbffe57109ea947dbe`, seal `ad3dfab8db699c66e7d013f00dca88f41d72fce5859180cc767d1f1d363046f2`; all 17 workloads completed and all mandatory rows pass |
| Release benchmark | `.local/benchmarks/release-final-20260720T053518Z-10c6a0e-r7-release`; manifest `ee70b0bc9ca2b66710e56642c3641a1deb02816dba208e6851957b58ce8d938e`, seal `be525645f72ac8fa43f3f4064a0d00ba4ae1b69b8826441ca913e104b66a70e3`; 72 pass, five bounded marginal reviews, zero failures |
| Windows/macOS portability | local harness fixtures pass; exact remote run for candidate tag `paradox-2.0.0-ci-10c6a0e` remains pending |

## Release decision

The release decision is `pending`. The managed graph-root fix is committed, the
replacement candidate is frozen at `dbbdcc1`, and its eight-task
`release-core` run is green. Its native child is not a replayable memory donor,
however, and the first memory attempt stopped at harness provenance preflight.
A fresh full native source proof and combined-memory gate under the six-file
harness repair remain mandatory, followed by the applicable documentation,
downstream, source-package, benchmark, and hosted Windows x86-64/macOS ARM64
gates. User publication of prepared downstream branches/PRs, the release tag,
and workflow also remains required.

## Historical rejected or superseded refs

The compatibility-first candidate at
`refs/paradox-release/candidate-20260717T083921Z`, commit
`2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa`, once satisfied a different
contract that preserved private surfaces and dual engines. The later
contract-first development candidate at
`refs/paradox-release/candidate-20260719T104709Z`, commit
`5e40d2ba9b9ce3a75615b90420fb9bc298c19ecf`, and its direct-child portability
companion `refs/paradox-release/portability-harness-268ccff` at
`268ccff27ee68bfea71c6370b0616a9c969a94cf` predate the final public-subset,
bulk-dependency, downstream-bridge, and `$has_deps` changes. All of these refs,
hashes, logs, and artifacts are historical only. They authorize no conclusion
about current package bytes and must not be copied into the candidate fields
above.

The still later candidate
`refs/paradox-release/candidate-20260719T150831Z`, commit
`612345ceb403c70a0ea6c1149c367c6782d9870b`, passed the native, R-API,
runtime, differential, and memory gates recorded for its exact bytes. Its final
release benchmark then found a release-blocking correctness defect before any
candidate timing: R 4.6 exposed the benchmark's ordinary wide data.frame as a
top-level base `wrap_list` ALTREP, which `check_dt()` rejected. The benchmark
remains deliberately unsealed. This candidate and every package-byte-bound
result for it are superseded; the retained evidence explains the replacement
public-table snapshot boundary but cannot be promoted to the new candidate.

The next candidate,
`refs/paradox-release/candidate-20260719T175053Z`, commit
`4f28327f894fe17324410a45cabaf7221e6eca45`, passed its exact-byte native,
R-API, runtime, differential, and downstream-bridge gates. Its memory run was
stopped and deliberately left unsealed after adversarial review found two
release blockers in the shared public-table path: missing, S4, and
dimension-mismatched `row.names` were not consistently rejected, and a hostile
top-shell Elt callback could mutate a still-shared names vector with
`data.table::setnames()`. This candidate is superseded. No completed or partial
package-byte evidence from it transfers to a replacement candidate; the
unsealed memory run is retained only as diagnostic history.

The following candidate,
`refs/paradox-release/candidate-20260719T194741Z`, commit
`60704fcc6a899c508f5adffbec35bb61723d3704`, installed successfully and built
the exact downstream bridge overlay, but failed its R-API gate before release
convergence. Strict Clang against the pinned R 4.3 and 4.4 headers rejected two
implicit signedness conversions from the `Rboolean` result of `Rf_isObject()`
to `int`. Native and runtime work already in progress was stopped; all partial
or completed package-byte evidence for this candidate is superseded and may
not be promoted. The fix uses explicit truth comparisons and requires a new
candidate with a complete fresh gate set.

Candidate `refs/paradox-release/candidate-20260719T200549Z`, commit
`70c6d728785464c98ffc8c658f20c1937467a593`, passed the replacement R-API gate
and built its exact package and bridge bytes. The R 4.3 runtime stage then
rejected an internal test which assumed that reinstalling an attribute on a
wide data.table always creates base R's top-level `wrap_list` ALTREP. R 4.3
left that input ordinary, for which the materializer correctly returns the
table unchanged; only an admitted ALTREP shell is normalized and stripped of
ignored caches. The fix makes the cache-disposal assertion use the portable
native ALTREP fixture and retains the base wrapper as conditional realistic
coverage. Expanding production copying to ordinary tables was rejected as an
unnecessary hot-path cost. The partial native/runtime results and completed
API/package/bridge evidence are all superseded.

Candidate `refs/paradox-release/candidate-20260719T202524Z`, commit
`e3741ab1d3cb8a5f3e7f357af6b7ab90c6e50fb7`, passed its exact-byte native,
R-API, and runtime gates, then failed the maintained repository sweep in
mlr3fselect. `mlr3::BenchmarkResult$aggregate()` produces
the ordinary additive class vector
`c("bmr_aggregate", "data.table", "data.frame")`, and data.table 1.18.4
deliberately preserves that class through a narrow `with = FALSE` subset before
bbotk calls `ParamSet$assert_dt()`. The exact-class classifier rejected this
non-dispatching representation even though Paradox 1 accepted it. The
replacement contract admits well-formed additive leading classes, drops them
from an already-required ALTREP snapshot while avoiding any prefix-only copy of
ordinary input, and retains the strict table attribute/cache boundary.
All evidence bound to `e3741ab` is superseded and requires a fresh candidate.

Candidate `refs/paradox-release/candidate-20260720T022410Z`, commit
`a4617ca769ff5373a7da16c7ce333e36c68fd9b2`, fixed that public-table boundary
and passed its exact R-API, native, runtime, differential, focused downstream,
and bounded-performance gates. Gctorture and every retained Valgrind diagnostic
inventory were clean. Its combined memory run remained unsealed because the
source-bound rchk policy still named the pre-classifier report hashes and
69-routine count. The actual bounded report preserved the reviewed 77 Function
blocks, 196 UP diagnostics, and 13 PB diagnostics; the classifier's three
diagnostic registrations raised the routine count to 72. The replacement
candidate incorporates that exact refreshed policy and final validation
tooling without changing package-facing files. The independently replayed
`.local/checks/package-equivalence-a461-10c6` proof establishes that all 19
changed Git paths are `.Rbuildignore`-excluded and that both clean builds have
the same 217-file package payload after removing only R's generated `Packaged:`
record. Thus its completed R-API, full native/sanitizer/test/check, runtime, and
focused-consumer conclusions transfer to the identical `10c6a0e` package
payload while retaining their donor identity. Neither the partial `a4617ca`
memory directory nor its earlier tooling-bound overlays transfer; exact
`10c6a0e` static, memory, differential, documentation, downstream, and benchmark
evidence is retained separately above.
