# Paradox 2.0.0 active release ledger

## Status

**Not release-ready.** The contract-first source rewrite is in implementation
convergence. No package-byte-dependent result from the superseded
compatibility-first candidate is accepted for this source. This ledger changes
to release-ready only after one clean immutable ref satisfies every mandatory
gate below.

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
  use one exact-class/allowed-attribute classifier. Names/classes and admitted
  data.table cache carriers are ordinary; caches are discarded. Raw row names
  are attribute-free, nonobject, non-S4 integer/character vectors: ordinary
  compact `+/-n` forms decode to their count, while stable row-name ALTREP pays
  one Length and no Elt. Row-consuming operations compare this count with their
  columns; direct `trafo` and a no-edge Design dependency plan do not add
  column observations for an unused dimension. A zero-column data.frame may
  omit names and retains its row count in Design transpose. Exact top-level
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
  exact top-level ALTREP table boundary above, while admitted atomic leaves and
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
- [ ] complete capsule, graph, callback/reentry, structural-versus-semantic
  ALTREP/S4, direct-assignment versus `set_values(.values=)`, shared public-
  table classifier/row-name/cache/name-reentry, semantic-column, data.table
  facade, zero-column Design, corruption, serialization, exact-
  TuneToken/receipt/capability, and upgrade contract suite passes on the final
  frozen candidate (the prior suite and current affected development tests are
  green);
- [ ] package reference documentation, vignettes, migration guide, website,
  and downstream bridge docs describe the final behavior consistently;
- [x] routine/analyzer/runtime ledgers discover current files dynamically and
  contain no historical hard-coded test counts; the final old-R run stages the
  reviewed `mbo_config` Git-object bundle before worker admission and executes
  its upgrade test without an environment skip.

### Downstream coordination

- [x] local bbotk bridge `6cae955` and miesmuschel bridge `d9d5c01` are
  prepared on their recorded branches;
- [x] mlr3mbo `569c184`, celecx `cef7a4f`, mlr3 `35e30a9`, mlr3fselect
  `ae8e1d1`, and mlr3pipelines `1c4bc6e` are prepared on their recorded
  branches;
- [ ] all seven bridge heads are authenticated and retested against the exact
  frozen candidate;
- [ ] other priority packages and active documentation tested against exact
  reviewed revisions;
- [ ] user has manually pushed branches and opened the required PRs (agents
  have no remote-write authorization).

### Performance and correctness

The checked-in `environment/rchk-bcheck-policy/` binds the refreshed reviewed
pre-freeze source seal and its bounded-analyzer reports. Bcheck
analyzed 770 functions and 27,856 states, with 77 exact Function blocks, 196 UP
diagnostics, and 13 PB diagnostics; its report SHA-256 is
`dbb6687723b2e23fa813f814b54f7e0e48db1227210af5409275ceab79be77ee`.
Maacheck is byte-empty (`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`),
and that source's fficheck reports 69 registered routines and one checked
registration call
(`456d9e66d48216c75ba828d755263e012ccf5e5d14b8bc78037a5b237cd26ef6`).
The generated policy, block table, and rationale table SHA-256 values are
`b4656bdd8ae0395065fec587a83c5aa76b818f5a444aa4012c8807f0fcb56f30`,
`d39128eabef44b8c0386369f66a0adfa76961629c92b7b687aa8e1c7779a3c9e`,
and `c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
The first pre-freeze report exposed a real `snapshot_dependencies()` root-
lifetime defect across callback-capable feasibility validation. The result and
its columns now remain protected through that validation, a successful
allocating-callback regression covers the commit path, and the superseded raw
run was discarded before generating its policy. The refreshed policy includes
the registered `$has_deps` reader; its one address-taken graph-root diagnostic
is reviewed as `ADDRESS_TAKEN_MODEL`. This remains pre-freeze evidence only:
the final frozen-candidate memory gate must rerun on the exact candidate bytes
and deliberately regenerate the policy if its authenticated report changes.

- [x] directly affected development tests pass from stable cached
  installations;
- [ ] the complete unit suite passes from the new frozen candidate
  installation;
- [x] the final profiling decisions are closed: sparse search-target projection
  and a bulk-dependency constructor transaction are measured no-gos for 2.0.0;
- [x] measured hot-path changes are implemented and revalidated in affected
  development tests; the sealed release benchmark remains pending below;
- [ ] strict GCC/Clang, sanitizers, GCT, Valgrind, rchk, adversarial corruption,
  and R-API/exception-ledger checks are clean;
- [ ] R 4.3.3, 4.5.2, development R, Windows x86-64, and real macOS ARM64 are
  clean for the same source;
- [ ] priority consumer, documentation, differential, and benchmark gates are
  accepted with retained source-bound evidence.

Current profiling diagnostics are implementation guidance, not release
benchmark evidence:

- the operation-local SHADOW ID index is retained in
  `.local/benchmarks/dev-shadow-pointer-index-ab-20260718`; its A/B medians
  improved construction by 1.39x, live values by 7.72x (293 to 38 microseconds),
  live domains by 2.17x, and assignment by 1.70x while keeping complete
  corrupt-state validation;
- the direct base checks in the `to_tune(ParamSet)` callback wrapper measured
  4.47 microseconds versus 24.31 microseconds for the former
  checkmate/mlr3misc layers in the focused probe. The final paired gate must
  still validate representative end-to-end workloads on the frozen candidate;
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
  These diagnostics justify the authenticated policy but do not replace the
  pending sealed benchmark;
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
- none of the staged DSOs above is current combined evidence. The implementation
  has since converged, but release conclusions remain pending until the candidate
  table below names the immutable source and each applicable retained gate is
  recorded against it. These staged diagnostics do not replace any candidate
  compiler, suite, benchmark, or analyzer gate.

## Current local downstream branches

These are local PR preparation only. Repository policy requires the user to
push and create PRs manually.

| Package | Worktree | Branch | Commits | Intent |
|---|---|---|---|---|
| miesmuschel | `.local/compat/github/miesmuschel` | `codex/paradox-paramsetshadow-bridge` | `68686ef`, `f0e4736`, `cf64981`, `98e3e47`, `f27d8fb`, `d31f613`, `a9fbf37`, `ca665a6`, `2ca3030`, `d9d5c01` | Select/re-export official ParamSetShadow at load time on Paradox 2, complete the public-state adaptation, construct the legacy generator on Paradox 1, compare operators without opaque R6 internals, and retain version-gated expectations for the two Shadow dependency diagnostics. |
| bbotk | `.local/compat/github/bbotk` | `codex/public-paramsetcollection-sets` | `0909e60`, `94e4c22`, `6cae955` | Replace one private collection `.sets` read, accept version-gated native diagnostics, and root detached public search-space snapshots for their complete native pointer lifetime. |
| mlr3mbo | `.local/compat/github/mlr3mbo` | `codex/paradox2-transformless-subset` | `569c184` | Use public `subset(..., keep_trafo = FALSE)` on Paradox 2 while retaining Paradox-1 paths. |
| celecx | `.local/compat/github/celecx` | `codex/paradox2-diagnostics` | `cef7a4f` | Version-gate Paradox validation fragments in tests; runtime behavior is unchanged. |
| mlr3 | `.local/compat/github/mlr3` | `codex/paradox2-diagnostics` | `35e30a9` | Version-gate two numeric-Domain diagnostic assertions. |
| mlr3fselect | `.local/compat/github/mlr3fselect` | `codex/paradox2-diagnostics` | `ae8e1d1` | Version-gate one feature-fraction diagnostic assertion. |
| mlr3pipelines | `.local/compat/github/mlr3pipelines` | `codex/paradox-diagnostic-compat` | `1c4bc6e` | Decouple PICV tests from exact Paradox-1 wording and fix GraphLearner state deep-clone ownership with an explicit mutation-isolation regression. |

As a development diagnostic, the final miesmuschel public-state equality commit
passed its dictionary (693 expectations) and shortform (20 expectations) files
against both development candidate C and legacy Paradox. This is evidence for
the bridge shape, not a substitute for retesting the exact branch head against
the exact frozen candidate.

The final handoff must include exact push commands and PR title/body text only
after retesting these heads against the exact frozen candidate.

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
benchmark is not transferable across source changes.

For an R/docs-only inner-loop change, a development DSO may be reused only
after recording byte identity of every native build input plus compiler/profile,
`NAMESPACE`, and `DESCRIPTION`, reinstalling the R/help databases, and verifying
the loaded DSO hash. This exception is diagnostic-only. The final immutable
candidate receives one clean full source build per distinct
R/compiler/instrumentation profile; compatible evidence families may share
that exact authenticated installation, never development component objects.

## Candidate freeze record

The immutable package candidate is committed only after all implementation,
test, documentation, and downstream-bridge work is staged and the primary
checkout is clean. A commit cannot contain its own commit, tree, or archive
identity without a circular mutation. Therefore the candidate intentionally
ships this table as pending. After every local and remote gate has completed, a
separate evidence-ledger commit is created as a direct child of the candidate;
its only changed path is this file, and it populates the table and release
decision. The package/release tag continues to point to the candidate, never to
the evidence-ledger or portability-harness commit.

The two allowed direct-child records are siblings: the evidence ledger changes
only `design/release-2.0.0.md`, while the portability harness changes only
`.github/workflows/r-cmd-check.yml`. Every package/source-dependent result names
the candidate. Creating the evidence ledger reopens only repository hygiene and
ledger cross-checks; creating or correcting the portability harness reopens only
its structural tests, `actionlint`, and the two real-platform portability rows.
Any other post-freeze change requires a new candidate and new source-bound
evidence.

| Field | Value |
|---|---|
| Full candidate ref | pending |
| Commit | pending |
| Tree | pending |
| Source archive SHA-256 | pending |
| Version | 2.0.0 |
| Source file count | pending |
| Routine inventory hash | pending |
| Test inventory hash | pending |
| Downstream bridge commits | pending final retest |

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
4. every exact head in `compat/github-bridge-provenance.tsv`, then
   priority-zero/one reverse dependencies and maintained mlr-org repositories;
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
commands, versions, logs, manifests, and completion seal. Verifier-only changes
never relabel old execution as a new package run.

## Release decision

The release decision is `pending`. It becomes `accepted` only when every box in
the convergence checklist is complete, the candidate table is immutable, all
mandatory evidence rows name that exact source, downstream migration paths are
available, and the benchmark review finds no release-relevant low-hanging
regression.

## Historical rejected candidates

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
about current package bytes and must not be copied into the pending fields
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
