# Paradox 2.0.0 active release ledger

## Status

**Package source reopened for the informative native diagnostic contract.**
The former frozen candidate and its local correctness, compatibility,
documentation, memory, and performance gates remain historical evidence only.
They do not transfer to the changed package payload. During this focused source
change, run strict compilation, the diagnostic matrix, targeted differentials,
and affected consumers; defer the complete release suite until the package is
refrozen. The release decision also remains pending on exact Windows
x86-64/macOS ARM64 portability evidence and user publication of the final
downstream branches/PRs. No conclusion from a semantically different package
payload is accepted.

Normative contract: [`contract-first-2.0.0.md`](contract-first-2.0.0.md).
Implementation map: [`architecture.md`](architecture.md). Compatibility and
migration policy: [`compatibility.md`](compatibility.md). Validation sequencing:
[`validation.md`](validation.md).

## Decisions frozen for the first public release

- Version is 2.0.0; R >= 4.3; portable C17; data.table >= 1.18.4.
- R C API use is public except for one centralized R < 4.6 compatibility call.
  Raw attribute selection uses `R_mapAttrib()` on R >= 4.6 and the established
  `ATTRIB` traversal on R 4.3--4.5 without evaluating R or data.table code. The
  sole exception is separate from that adapter: `src/r_api_compat.c`
  declares/calls exported `Rf_findVarInFrame`, rejects
  `PROMSXP`, and R >= 4.6 instead uses the documented experimental API
  `R_GetBindingType`. R 4.3--4.5 has no public non-forcing binding classifier,
  so an R-level `substitute()`
  workaround is unsound for the simultaneous generation/receipt scan. The exact
  exception is ledgered in `environment/r-api-exceptions.tsv`, raw-token-audited
  to one occurrence and path, and pinned-header/runtime tested. It is not
  CRAN-allowlisted for the supported pre-4.6 build path: those runtime DSOs must
  contain the symbol and
  every R >= 4.6 DSO audit must prove it absent. No broader internal-API
  permission exists.
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
- Current objects serialize normally; old objects use explicit
  `upgrade_paradox_object()`.
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
  It has no S3 extension or competing native/R conversion path.
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
- All major compatibility breaks above ship now. They are not deferred to a
  later release.

Changing one of these requires an explicit contract/design/NEWS/test update,
not a local compatibility workaround.

## Implementation convergence checklist

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
- [x] explicit legacy upgrader with CRAN-1.0.1 and `mbo_config` fixtures;
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
- [x] complete capsule, graph, callback/reentry, structural-versus-semantic
  ALTREP/S4, direct-assignment versus `set_values(.values=)`, shared public-
  table classifier/row-name/cache/name-reentry, semantic-column, data.table
  facade, zero-column Design, corruption, serialization, exact-
  TuneToken/receipt/capability, and upgrade contract suite passes on the final
  frozen candidate payload, authenticated through the sealed `a4617ca` to
  `10c6a0e` package-payload equivalence proof and exact-candidate static/memory
  stages;
- [x] package reference documentation, vignettes, migration guide, website,
  and downstream bridge docs describe the final behavior consistently; the
  final 17-workload documentation stage has every mandatory row green;
- [x] routine/analyzer/runtime ledgers discover current files dynamically and
  contain no historical hard-coded test counts; the final old-R run stages the
  reviewed `mbo_config` Git-object bundle before worker admission and executes
  its upgrade test without an environment skip.

### Downstream coordination

- [x] local bbotk bridge `4d49750` and miesmuschel bridge `d4c7f79` are
  prepared on their recorded branches;
- [x] mlr3mbo `1a1c0ab`, celecx `6da5102`, mlr3pipelines `c85b2f4`, and
  mlr3fda `8f5a3df` are prepared on their recorded branches; the mlr3
  `35e30a9` and mlr3fselect `ae8e1d1` diagnostic-only PRs are documented for
  closure without replacement;
- [x] all eight profile bridge heads are authenticated and the four refreshed
  heads are retested and fully checked against both exact Paradox axes;
- [x] diagnostic-only downstream changes are pruned, independent runtime fixes
  are retained, and the resulting affected heads receive focused dual-axis
  retesting before a new handoff is issued;
- [x] the retained priority consumer corpus and active documentation are tested
  against their exact reviewed revisions; the later final miesmuschel delta is
  test-only and its affected rows were rerun on both axes;
- [ ] user has manually pushed branches and opened the required PRs (agents
  have no remote-write authorization).

### Performance and correctness

The checked-in `environment/rchk-bcheck-policy/` binds the refreshed reviewed
public-table candidate source and its bounded-analyzer reports. Bcheck
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

- [x] directly affected development tests pass from stable cached
  installations;
- [x] the complete unit suite passes for the frozen candidate package payload;
  the donor execution and target payload identity remain separately recorded;
- [x] the final profiling decisions are closed: sparse search-target projection
  and a bulk-dependency constructor transaction are measured no-gos for 2.0.0;
- [x] measured hot-path changes are implemented and revalidated; the sealed
  release benchmark has 72 passes, five bounded marginal reviews, and no
  failure;
- [x] strict GCC/Clang, sanitizers, GCT, Valgrind, rchk, adversarial corruption,
  and R-API/exception-ledger checks are clean;
- [x] R 4.3.3, 4.5.2, and development-R local execution are clean for the
  identical package payload;
- [ ] Windows x86-64 and real macOS ARM64 are clean for the exact frozen
  candidate and independently retained;
- [x] priority consumer, documentation, differential, and benchmark gates are
  accepted with retained evidence and explicit transfer scope.

Current profiling diagnostics are implementation guidance, not release
benchmark evidence:

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
  cost rather than treating it as an ordinary hot-path regression. Only
  `shadow_values_live` receives the finite `integrity-shadow-read` median/q75
  ceilings 3.25/3.50, and only the three direct `collection_values_*` rows
  receive `integrity-collection-read` ceilings 2.75/3.00. Post-index Shadow
  profiling measured 2.623/2.605; the final-focus rich collection diagnostic
  measured 1.546 at the median, while the last common pre-final-fast-path nested
  diagnostic measured 2.361/2.349. The collection ceiling still rejects the
  retained pre-optimization 3.365/3.591 stage. Filtered getters, domains,
  dependencies, params, mutation, and every real consumer row keep their strict
  ordinary tiers; the integrity rows also retain the `hot` allocation budget.
  The sealed release benchmark subsequently accepted all timing rows and
  retained five allocation/integrity rows for bounded marginal review;
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

### Informative-diagnostic focused evidence

The reopened source has bounded evidence appropriate to this change; it is not
yet a replacement for the complete release matrix:

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

These are the final locally prepared dispositions after restoring informative
native diagnostics and pruning redundant downstream adaptations. Repository
policy requires the user to push retained branches and create, update, or close
PRs manually. The obsolete mlr3 and mlr3fselect branches are evidence only and
must not be published as replacements.

| Package | Worktree | Branch | Commits | Intent |
|---|---|---|---|---|
| miesmuschel | `.local/compat/github-release-refresh-20260720/miesmuschel` | `codex/paradox-paramsetshadow-bridge` | head `d4c7f79750cd15c8174415fb0ba059c597f4f055` | Retain the official ParamSetShadow/public-state bridge, Paradox-1 construction, cache-independent comparisons, and dual-major Rd links. Its separate official-Shadow graph-boundary diagnostic remains intentional. |
| bbotk | `.local/compat/github-release-refresh-20260720/bbotk` | `codex/public-paramsetcollection-sets` | head `4d497506ef2a03a97024002bb3c10d906583fada` | Retain only the public `.sets` migration and detached-snapshot rooting; the follow-up removes temporary diagnostic gates. |
| mlr3mbo | `.local/compat/github-release-refresh-20260720/mlr3mbo` | `codex/paradox2-transformless-subset` | head `1a1c0abe95f59cd314f1fbc19c596cb6ac15f067` (base `d1ce6189b637dd552fac95d56c53a39503bae889`, runtime change `a8a988a64b66e651043b75f63dfdfb4604185e3f`) | Use public `subset(..., keep_trafo = FALSE)` on Paradox 2 while retaining Paradox-1 paths and document the migration. |
| celecx | `.local/compat/github-release-refresh-20260720/celecx` | `codex/paradox2-diagnostics` | head `6da5102ca948b8182aae13575c48a932812b05c6` | Retain only the independent cycle/dependency adaptation and compatible mlr3mbo bridge requirement. |
| mlr3 | `.local/compat/github-release-refresh-20260720/mlr3` | `codex/paradox2-diagnostics` | obsolete head `35e30a9` | Close without replacement; removing its numeric-diagnostic gates leaves an empty effective diff. |
| mlr3fselect | `.local/compat/github-release-refresh-20260720/mlr3fselect` | `codex/paradox2-diagnostics` | obsolete head `ae8e1d1` | Close without replacement; removing its feature-fraction diagnostic gate leaves an empty effective diff. |
| mlr3pipelines | `.local/compat/github-release-refresh-20260720/mlr3pipelines` | `codex/paradox-diagnostic-compat` | head `c85b2f4165e056934f892c5db37391869cd40e38` | Retain only the GraphLearner deep-clone ownership fix and mutation-isolation regression. |
| mlr3fda | `.local/compat/github-release-refresh-20260720/mlr3fda` | `paradox2-snapshots` | head `8f5a3dfa297ad236812cda57fab02de75fec375a` | Preserve byte-identical Paradox-1 messages; the Paradox-2 variant now differs only in its internal assignment call header. |

The unchanged miesmuschel head passed its complete repository suite and source-
package check against both exact Paradox axes. Its last commit changes only
three deep test comparisons to ignore data.table cache attributes; no production
source changed. The final Paradox-2 affected-row evidence is retained under
`.local/compat/runs/release-final-20260720T053518Z-10c6a0e-r8`, and Paradox-1
under `.local/compat/runs/release-final-20260720-v1.0.1-paradox1-r2`.

The regenerated handoff in `compat/downstream-pr-handoff.md` records the final
retained heads, manual push/PR text, and the two redundant PRs to close. The
same focused 2,022 expectations pass with zero failures, errors, warnings, or
skips on both Paradox axes under
`.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`.
The named `release-refresh-20260720` evidence profile remains the immutable
authority for the substantive pre-cleanup bridge/runtime changes; the two
focused diagnostic runs authenticate the tested cleanup diffs later recorded
as the four follow-up commits. Neither rewrites nor relabels the earlier
full-corpus candidate evidence. The profile uses a separate primary-checkout
namespace,
an axis-neutral run-local receipt for the unchanged external dependency
closure, and axis-specific overlay/test/check stages. The axis registry pins
the frozen Paradox 2 candidate and released Paradox 1.0.1 by exact
ref/commit/tree/version. Both axes require sealed exact-head `R CMD check`
results as well as the focused repository suites; this is what validates the
miesmuschel Rd-link repair. A check is green only when its retained final
status has no ERROR or WARNING, independently of the R process exit status.
The check receipt binds the candidate, bridge, `mlr3verse`, any other configured
extra library, and dependency library by ordered content fingerprints.

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

## Candidate freeze record

The immutable package candidate is committed after package implementation,
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

1. strict GCC and Clang C17 warning-clean builds, registration/probe audit,
   ASan/UBSan, complete unit tests, examples, and `R CMD check --as-cran`;
2. real R 4.3.3 and 4.5.2 runtime stages plus development R, pinned-header
   compilation, and the exact R-API-exception ledger/raw-token/version-gated DSO
   audit, including the authenticated R-4.3 data.table 1.18.4 overlay;
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
   legacy serialized configuration upgrades;
7. GitHub Windows x86-64 and macOS Apple-silicon ARM64 checks whose failure
   status is correctly propagated and whose exact source provenance is retained;
8. representative paired benchmarks on an idle host, including downstream
   call patterns, with raw distributions and regression thresholds reviewed.

Every accepted row receives a unique run ID, exact source ref/commit/tree,
commands, versions, logs, manifests, and completion seal. A transferred row
retains its donor execution identity and additionally names the target and exact
equivalence proof; verifier-only changes never relabel old execution as a new
package run.

### Retained local release evidence

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

The release decision is `pending`. All local gates and the benchmark review are
accepted. Only two items remain: independent verification of the exact remote
Windows x86-64/macOS ARM64 portability run, and confirmation that the user has
published/opened the prepared downstream branches and PRs. The final ledger-only
commit may change the decision to `accepted` after those facts are recorded.

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
