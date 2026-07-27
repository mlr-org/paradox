# Paradox 2 C rewrite: maintainer and agent notes

## Authority

The first public Paradox 2 release is a contract reset, not a continuation of
the compatibility-first native candidate frozen in July 2026. The normative
behavioral and architectural contract is
[`design/contract-first-2.0.0.md`](design/contract-first-2.0.0.md). The shorter
implementation map is [`design/architecture.md`](design/architecture.md), the
intentional compatibility boundary is
[`design/compatibility.md`](design/compatibility.md), and the active release
ledger is [`design/release-2.0.0.md`](design/release-2.0.0.md).

Old candidate commits, refs, logs, and artifacts are historical unless a
retained sealed proof transfers an explicitly bounded conclusion. Git history
retains the former long design narratives; do not copy their R6-surface
authentication, S3 fallback, sentinel replay, or pre-data.table-1.18 decisions
back into current source. The independently replayed `a4617ca` to `10c6a0e`
package-payload proof is one historical transfer for that superseded payload;
it establishes nothing about the active implementation.

The last sealed package-facing candidate is
`refs/paradox-release/candidate-20260724T105215Z`, commit
`8797f1163fe612cb01d1facf517834d3f516a697`, tree
`81e6f901266754b97a0906f88a472bf04795f13c`, with authenticated candidate
content SHA-256
`ced2390bc756b01805e7bdcf32fdb4a6ff2c1bd010dd3d576194d939e491f644`.
Final pre-release cleanup reopened package source after that seal on
2026-07-25. The ref and all source-bound gates are therefore historical until a
new candidate is frozen; there is currently no active frozen package candidate.
The R 3.6 compatibility implementation is now complete in the working tree:
create one new candidate and rerun the applicable final gates once.
Post-freeze release-policy, validation, and ledger commits for that
future candidate must prove package-facing paths unchanged; call that
relationship *package-facing-source identical*, not package-identical, unless a
separate sealed complete-payload byte proof exists.

The original source-bound bcheck discovery under
`.local/checks/serialized-migration-release-8797f11-memory-20260724/modes/rchk`
analyzed 870 functions and 30,245 states and produced raw report SHA-256
`02b08085ea0fadc906fb8e8fdd3f5211a6eb2a7eb08e922205e69f4d25361072`.
The final combined memory run under validation tooling
`a05cd51a5570c4a674b6c80d6cd38c7898223635` reproduced the same reviewed
semantics—80 blocks, 238 UP diagnostics, and 13 PB diagnostics—with final raw
report SHA-256
`0226275247eb16736ab317dfb3e1c7f836ee006fb59683276998632aa120cd9d`
and ordering-insensitive semantic SHA-256
`f3dc5caccc4508f9f9263d8d912455820dfba428cb00f7d0454e1734adf6da18`.
The two raw reports are not byte-identical. Maacheck is byte-empty
(`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`);
fficheck records 79 functions and one registration call
(`565392164712e15df4bbdd0b34fb852b35eeab380f612c0983f2ccab69c31370`).
The final policy, block table, and rationale table SHA-256 values are
`e6010f58c58dfb8e952ee0e515a1a2352decd143e01bda50af7c800b4aa0470d`,
`50445fd2be3da7cbeb05f689377802deac7597ee2eeed8acc34f683989e3a71d`,
and `c9e94a9f49b5570838df94fba7f45ef870999b78d03b7b4824412dc34645d8a4`.
The `a05cd51` tooling tree
`c58b6bea97d66e23b542a34864479e28c5e5e02f` is package-facing-source
identical to the candidate. Its retained GCT/Valgrind/rchk completion is
`a07df9509c78976d6d8a31a9b4f3415787f512e73bf85a2311d6ced672e9b226`;
that combined completion, not discovery alone, owns the memory conclusion.

The discovery exposed a real protection imbalance in `schedule_vector()`: an
ALTREP `Duplicate` method may legally return its input, but pointer equality was
incorrectly used to decide whether to release the second protection. One
indexed root plus `REPROTECT` fixes the imbalance, and the candidate includes a
self-returning ALTREP-list regression that captures R's direct stack-imbalance
diagnostic. `schedule_vector` is absent from the final reviewed report.

Post-freeze validation uses the named `release-refresh-20260720` profile and
explicit `paradox2`/`paradox1` axes. Candidate, tooling, overlay, dependency,
repository, documentation, and benchmark identities remain separate and must
never be relabeled. The final benchmark/documentation tooling is
`fc92edd7f1ab612468066fe06bd3d9fc7afea41c`, tree
`05cc4e5213c5ee73d0bc764c3d102c15e4c57141`; its diff from the candidate
contains no package-facing path. The sealed 77-row release benchmark passes
with 73 pass, four bounded marginal reviews, and zero failures; completion,
manifest, and seal SHA-256 values are
`5d615637bd6448bb95b8ba798cc8f7e23d40a78e1a63a4e3988eb436914109a0`,
`a16d403c20ed8ab80233a877e81ec146b4a5c128fdf0a765be440993eb325499`,
and `d7a4bbefdba3e1d0a800b2e6bdae6e10ddde22b612cf05bebc5d019eba198fc7`.
Its policy has exactly seven integrity rows: `shadow_values_live`, the three
synthetic `collection_values_{plain,rich,nested}` rows, and the three real
miesmuschel/mlr3pipelines consumer `$values` rows. Consumer `$params`,
`get_values_unchecked`, and all other consumer operations retain their ordinary
`hot` tier.

The broad repository corpus used `jobs = 2` in 14 admitted waves and has 20 of
28 exact repositories green; the eight retained non-green rows are reviewed
non-Paradox upstream, optional-runtime, environmental-dependency, or bounded-
timeout exclusions. The final exact Paradox-2 source-package check independently
builds and checks bbotk, miesmuschel, mlr3mbo, celecx, and mlr3fda; all five
finish with `Status: OK`. All mandatory documentation rows pass; advisory
`mlr3book` full-render and legacy `mlr3gallery` dependency rows are excluded and
do not weaken the focused Paradox documentation conclusion. All local release
gates were complete for that historical payload; none is a completion claim
for the reopened dormant-value source.

The direct-child portability companion is
`refs/paradox-release/portability-harness-5305ead`, commit
`5305eaddbc9c2159fe194e6be10388c17b4c506a`, tree
`e1fe00ddad08af89566e3df12460bc47d3e98292`. It changes only
`.github/workflows/r-cmd-check.yml` (SHA-256
`14c4c8d1cc8d8e07aea1829d1203f6217464ae9c6b94efc1050bf192638196e3`)
and is package-facing-source identical to the candidate. The only remaining
release gates for that historical candidate were retained hosted Windows
x86-64/macOS ARM64 results and the user-performed downstream branch/PR
publication handoff. The replacement dormant-value candidate instead requires
the applicable local matrix again after source convergence. Agents must not
perform either remote write.

The structural boundary below is an intentional Paradox-2 break made in this
release, not a migration shim to relax later. Supporting exotic structural
ALTREP/S4 shells or parallel R/native admission would preserve no known
maintained use while retaining duplicate authority, dispatch, and
multi-observation hazards. Ordinary documented containers and stable ALTREP
semantic vectors remain supported at their stated positions.

The pre-release dependency contract is also intentionally settled here:
Domain-valid values may be stored while dependency-inactive, defaults
participate in recursive activity for absent parents, and constraints observe
only active entries. This is not a policy toggle or a deferred compatibility
shim. Explicit check-family calls remain strict point validators and are
store-blind, so a legal raw dormant store is not itself promised to pass
`$check()`.

The final pre-release performance batch is governed by
[`design/final-performance-implementation-plan.md`](design/final-performance-implementation-plan.md).
That plan was recorded at package commit `b2e1649` before implementation and
locks both the measured targets and their compatibility/integrity proof
obligations. P5 landed at `81cbccf`; P1--P4, the profile-led Condition/RHS
follow-ups, direct native probes, and focused tests landed at `387c1cd`.
Balanced per-slice evidence is complete and recorded in the plan, including
the measured stop boundary at the remaining single exact integrity pass. Do
not rerun those slice A/B experiments during minor cleanup. Do not run the
full compatibility, memory, portability, or release matrices until source
converges and the final release gate begins. Operation-local indexes and
one-use package-private ownership handoffs are allowed optimizations;
persistent validation caches, trusted caller metadata, skipped graph checks,
and weakened generation reauthentication remain prohibited.

The final compatibility batch is governed by
[`design/r-3.6-compatibility-implementation-plan.md`](design/r-3.6-compatibility-implementation-plan.md).
Paradox 2 supports R >= 3.6 and uses portable C99. Old-runtime adaptation stays
inside the small R API facade, except for the graph crawler's narrow
capability gates where an old runtime cannot expose an edge at all; it is never
a second semantic engine. The real supported-runtime matrix includes R 3.6.3,
and the header matrix includes the 3.6.0 minimum plus the 4.0.0 and 4.2.0 API
transition releases before the existing later axes. Historical candidate evidence that
started at R 4.3 remains historical and cannot prove this reopened source.
The R 3.6 stage has two separate dependency proofs: its complete-test closure
and a cached, sealed six-package closure at the exact five direct
`DESCRIPTION` floors plus `digest` 0.6.39. The latter installs the same
candidate tarball into a fresh one-package library and runs only a bounded
dependency-origin/import smoke; it is not a duplicate test suite. A complete
four-runtime run also serializes a representative current-v2 nested graph on
R 4.0.5 and loads, exercises, mutates, and reserializes those exact bytes on
R 3.6.3. Every selected R 3.6 stage owns the floor proof; only a complete
runtime selection may claim the cross-version handoff.
Hosted portability also has one separate exact Windows x86-64 R 3.6.3/Rtools35
job. It installs the reviewed seven-package runtime closure from the existing
SHA-256 source lock, builds and loads the candidate DLL, runs focused semantic
smoke probes, and performs a bounded runtime-import-only check. It complements
the complete local R 3.6 behavior stage; it is the compiler/linker/loader and
old-Windows ABI proof and is mandatory in the release companion.
Do not authenticate that lane from paths or self-reported banners alone:
`design/portability-ci.md` records the official Rtools35 GCC, G++, `objdump`,
and Make byte digests, and the workflow, source installer, retained
`rtools35.tsv`, and offline verifier must agree on all four. Exact R 3.6.3
Windows already defines default `CXX` as the authenticated G++ plus one
`-std=gnu++11`. Locked `digest` 0.6.39 is the sentinel proving that ordinary
path with four distinct raw compile commands; no dependency override is
needed. One shared helper clears the reviewed hostile R/compiler/make inputs,
binds six empty read-only startup/Makevars files, and creates fresh
phase-specific home, temporary, and library state before all three execution
phases. It also binds `R_BUILD_ENVIRON`, `R_CHECK_ENVIRON`, and
`R_INSTALL_ENVIRON` to the authenticated empty environment file.
`R_PKG_CXX_STD` therefore enters the installer absent. The three exact
policy receipts are mandatory artifact evidence. An earlier raw-G++ audit
accidentally used the R 3.6.0 header source rather than the hosted R 3.6.3
configuration; do not reintroduce its discarded override.

The focused old-R binding benchmark is recorded in the final-results ledger of
[`design/r-3.6-compatibility-implementation-plan.md`](design/r-3.6-compatibility-implementation-plan.md).
On actual R 3.6.3, replacing absence-tolerant lookup at already-required
ownership/core/collection reads produced old/required median ratios from
1.08x on BASE `$params` through 1.24--1.40x on collection reads and 1.50x on
BASE `$values`; paired current-R controls show no regression.  The retained
development-evidence hash list is
`cccced57e09e66a656f89d50cff144602ef1dc3d3df7d6a49207411f3806eb7b`.
This is not release evidence and need not be rerun during cleanup.  Keep the
compile-time-specialized required and optional shell readers: remaining
optional sites are candidate/graph admission, fresh-destination detection, or
absence-sensitive generation reauthentication, not trusted hot-path reads.
Do not recover the old-R gain through caller-trust metadata or a second
semantic path.

## Non-negotiable design decisions

- `ParamSet`, `ParamSetCollection`, and `ParamSetShadow` are serializable R6
  shells over one package-owned `.core` capsule. Public R6 names and ordinary
  behavior remain; private layout is not an API.
- `.core` is a NULL-address external pointer with no allocation or finalizer.
  Its tag is `paradox.core.base.v1`, `paradox.core.collection.v1`, or
  `paradox.core.shadow.v1`; its protected slot is the complete ordinary-R
  capsule/model state. The sole stateful R-shell policy outside it is the
  documented public `assert_values` flag, which selects checked versus
  unchecked native value assignment. It is serialized/cloned with the R6 shell
  and compared by `all.equal()`, but is not graph/schema/value authority and
  does not change the ten-field ABI.
- The v1 payload is the fixed ten-field list `.params`, `.values`, `.tags`,
  `.deps`, `.trafos`, `.extra_trafo`, `.constraint`, `.sets`, `.translation`,
  and `.postfix`. It is one internal schema shared by all three node kinds.
- A SHADOW `.core` has exactly one package-private derived-cache attribute,
  `.paradox.shadow.snapshot.v1`. Its value is an ordinary, attribute-free list
  alternating every origin-graph shell with the exact capsule generation used
  to build the protected payload. It is only a derived refresh signature:
  origin authority remains `.sets[[1L]]`, callbacks come from the locked
  Paradox namespace on a rebuild, and neither origin nor callback factories
  are duplicated in the attribute. Deep clone rebuilds the signature against
  the memoized cloned graph; serialization may preserve its graph-relative
  identities. Every read validates the exact attribute/signature shape, and a
  missing, extra, or malformed attribute is corrupt state, never a fallback.
- Capsule tables are canonical plain base `data.frame`s. They have no
  data.table key, index, spare capacity, or self-reference. Public table
  accessors return valid data.table facades whose mutable shells and columns
  are newly owned or detached from capsule state. A native producer that owns
  a genuinely fresh shell and metadata may complete that facade directly;
  caller-owned/public-ingress tables still require defensive materialization
  and finalization. Attribute values, classes, names, and row names are
  contractual by attribute name; incidental pairlist order inherited from an
  R/data.table version is not. Native grid facades use one fixed
  `row.names`/`class`/`names` construction order on every supported runtime.
- Mutations build and validate replacement capsules and swap `.core`
  atomically. Value assignment plans the complete BASE/COLLECTION/SHADOW
  graph through ultimate BASE targets, deduplicates shared targets with
  deterministic last-owner semantics, and commits every replacement in one
  allocation- and callback-free wave. A callback mutation of any planned
  target wins and makes the outer assignment error before any target is
  changed. Native readers retain the capsule chosen at operation entry.
  Checked assignment validates every supplied entry's Domain, special-value,
  custom-check, sanitization, TuneToken, and structural contract, but does not
  require its dependencies to be satisfied. A valid dependency-inactive entry
  is stored as a dormant value in raw `$values`; the default
  `$get_values(remove_dependencies = TRUE)` view omits it and makes it visible
  again when a later state makes it active. Assignment computes activity only
  when a constraint exists, solely to pass that callback the active subset.
  Unchecked assignment keeps its existing structural-only boundary.
- `BASE` owns a schema and mutable values. Each `COLLECTION` capsule generation
  owns ordered child references and an immutable translation snapshot; `$add()`
  is one native transaction that validates both complete graphs, rejects an
  existing or proposed cycle (including a SHADOW origin path), checks every
  admitted generation again, and only then installs a replacement generation.
  Corruption, name collision, reentry, or allocation leaves the old collection
  unchanged. `SHADOW` owns one origin edge and a
  fixed visible schema while reading/writing dynamic origin semantics live.
  Shared DAG nodes are valid; a repeated node on one active path is a cycle.
  Deep clone memoizes the complete graph and clones each shell once; never
  restore independent per-edge `$clone(deep = TRUE)` recursion.
- `ParamSetShadow` is supplied by Paradox. Visible writes preserve hidden
  origin values; dependencies, constraints, and transformations are live;
  dependencies crossing the visible/hidden boundary are errors. Its capsule
  origin edge is the only origin authority; do not add a parallel private
  origin field. A direct SHADOW-to-SHADOW origin is rejected; construct the
  combined view over the ultimate BASE or COLLECTION origin instead. The R6
  private environment must not cache a second `.visible`/`.shadowed` schema;
  constructor `shadowed` input is not retained authority. The fixed capsule
  `.params` plus current origin IDs define the visible/hidden partition.
- A BASE-origin Shadow constraint adapter has one exact two-field
  `{callback, hidden_values}` plan and a thin native evaluator. The evaluator
  validates and snapshots both inputs, merges the already activity-filtered
  hidden values before the already filtered visible values without `c()`/S3
  dispatch, preserves opaque leaf identity, executes the callback once, and
  requires one non-missing logical result. The authoritative Shadow graph
  operation computes activity before constructing/invoking this exact plan;
  the two-field adapter has no schema and must not grow a duplicate activity
  evaluator.
- Third-party inheritance from the ParamSet family is additive only. Core
  ParamSet method/active-binding replacement, generated-wrapper mutation,
  reparenting, delayed bindings, and capsule/private-table writes are
  unsupported. `bbotk::Codomain` is the maintained additive-subclass case.
  This restriction does not remove the documented `Sampler` subclass API.
- Domain dispatch is closed over `ParamDbl`, `ParamInt`, `ParamFct`, `ParamLgl`,
  and `ParamUty`. Condition dispatch is closed over `CondEqual` and `CondAnyOf`.
  The existing exported names and built-in outward shapes remain; third-party
  S3 methods are not an extension contract. Standalone `condition_test()` uses
  the same exact built-in admission and comparison semantics as dependency
  execution, with an optimized native vector kernel for direct input.
  It accepts `NULL` or a plain logical, integer, double, or character vector
  with at most names, materializes a stable ALTREP operand once, and rejects
  classed or otherwise attributed vectors rather than invoking `Ops`/`%in%`
  dispatch. `condition_as_string()` is cold presentation glue, not a second
  evaluator. `p_uty(custom_check=)` remains the general callback escape hatch.
  A built-in Condition RHS is likewise an attribute-free logical, integer,
  double, or character vector without missing values: `CondEqual` has one
  element and `CondAnyOf` is non-empty and unique. Native dependency or direct
  comparison admission roots that selected RHS, materializes every element
  once, and validates the ordinary snapshot. The strict capsule validator does
  not accept ALTREP; it validates only the already admitted snapshot. An
  operation may retain pointers to the exact admitted RHS values in temporary
  workspace while their dependency/Condition owner graph remains rooted. It
  must not run the same exact Condition admission a second time merely to
  recover those operands.
  Numeric bounds/logscale admission and row construction are one registered
  operation; exact empty Domain operations and zero-dimensional grids also
  enter C and have no R special-case engine. Canonical semantic admission of
  one built-in Domain row has exactly one native owner, shared by constructor
  final-state validation, ParamSet construction, and ObjectTuneToken Domain
  admission. Boundary code may validate the outward table/class container, but
  it must not restate cargo, kind/storage, grouping, bounds, levels, default,
  tags, requirements, initialization, or special-value/transformation rules.
  After admission it receives the closed kind and validated fields and handles
  only its operation-specific work.
  A canonical `ParamFct` may have zero levels. Quantile/grid/uniform-sampling
  operations preserve its typed `character(0)` result when zero rows are
  requested; any positive-row quantile or sampling request errors before RNG
  entry or indexing. Empty levels are semantic emptiness, not corrupt schema.
- Interpreted structure must be ordinary non-ALTREP and non-S4. This includes
  outer general-list/internal-table/Domain/Condition/TuneToken/capsule shells,
  ParamSet constructor `params` lists, non-table transformation inputs and all
  transformation result list shells, Domain cargo containers and interpreted
  cargo entries, class/name vectors, row containers, dimnames and other list
  metadata. A narrow documented public-table ingress may first materialize a
  suffix-classified, allowed-attribute top-level VECSXP ALTREP shell once; base R's
  lazy attribute-only duplicate is the common motivating case. Public-table
  names/classes and ignored data.table cache carriers remain strict ordinary
  structure, while row names have the count-only integer/character exception
  specified below. Admitted semantic atomic leaves and columns may be stable
  ALTREP and are materialized once.
  In particular, the outer `special_vals` list, its names, and list metadata
  are structural for every Domain kind and must be ordinary non-ALTREP/non-S4.
  `ParamDbl`, `ParamInt`, `ParamFct`, and `ParamLgl` reject an ALTREP
  `special_vals` leaf before observing it. An S4 special leaf for those kinds
  is an opaque identity token: it matches only the pointer-identical object,
  and an S4 `default` or `init` is accepted only when pointer-identical to an
  already admitted special leaf. `ParamUty` values, defaults, initial values,
  and special leaves are opaque and may be S4; its retained Paradox-1 special
  membership is exactly base `identical()` over the admitted leaves, including
  S4, and is the sole narrow native observation of those objects. Neither rule
  invokes S3/S4 dispatch. Condition structure is never opaque.
- Every capsule operation has one native semantic implementation. Thin R
  wrappers may capture R language constructs and call documented callbacks,
  but there is no complete R/checkmate/data.table/S3 fallback, no `NULL`
  sentinel replay, and no second implementation used to authenticate the first.
  Ordinary built-in value admission uses one package-owned C classifier shared
  by standalone Domain checks and ParamSet scalar/table checks. It distinguishes
  missingness, type/shape, integerish, bounds, and factor-level failures on the
  native hot path. Accepted values do not pay for message construction; one
  native failure-only formatter produces informative, checkmate-style
  diagnostics. Do not call or link to checkmate from that native validation
  engine, rebuild its checks in R, or add a second formatter. Compatibility
  gives scalar missingness priority over an otherwise irrelevant storage-mode
  mismatch, as the useful checkmate diagnostics did; that extra classification
  is entered only on the already-invalid type path, not on accepted typed
  values. Compatibility targets the established useful categories and message
  fragments, not
  byte-identical reproduction of every checkmate quirk, `conditionCall()`,
  implementation frame, or exotic classed/ALTREP object behavior outside the
  structural contract.
  Unknown-ID suggestions belong to the same failure-only native diagnostic
  boundary. Rank the actual unknown name—not its position—by case-insensitive
  partial edit distance, retain the 20% threshold, stable top-three order, and
  cheap collection affixes. Exact ID hits must not allocate, calculate a
  distance, or construct suggestion text.
  Dependency activity likewise has one native list-basis kernel shared by the
  check family, `check_dependencies()`, stored-value filtering, and every
  authoritative constraint check/assignment site. Activity is transitive and
  conjunctive. After cycle validation, a TuneToken child skips its incoming
  edges. Otherwise an inactive parent never satisfies its child, even if it
  carries a value or default. For an active parent, an explicit basis value
  wins; a TuneToken parent skips that edge; otherwise an admitted recorded
  default is used, while `NoDefault` leaves the edge unsatisfied. Explicit
  point checks are store-blind:
  they use only the candidate point plus defaults, never stored `$values`.
  Stored-value reads use the raw store plus defaults. The complete-row
  Design/sampler dependency masker remains its specialized vector kernel
  because every parent value is already present; it must stay semantically
  equivalent where the two domains overlap and must not become a second
  list/point activity implementation. BASE dependency mutation admission is
  unchanged and can currently admit a cycle; every activity consumer tracks
  the active dependency path and raises a deterministic cycle error rather than
  recursing forever or reading partial state.
  There are exactly two narrow cold R semantic-orchestration families. The
  first is internal tuning.
  `$aggr_internal_tuned_values()`, `$disable_internal_tuning()`, and
  `$convert_internal_search_space()` are single R implementations over one
  captured capsule/cargo/translation/value snapshot and execute only their
  documented cargo callbacks; commits still use native value/capsule mutation.
  After native flatten semantics are complete, R may also rebind cargo closures
  whose lexical environments must change. Its only canonical write is a
  package-owned replacement of the detached result's rewritten `cargo` column.
  This family supplies no alternate structural admission, checking,
  callback-selection, or graph engine and is never a fallback. The second is
  exact-TuneToken `$search_space()` conversion. It consumes one natively
  admitted and rooted exact token/target-Domain snapshot, with live BASE
  candidates already replaced by sealed single-use capabilities, switches only
  over the package's built-in token kinds, and solely owns callback-dependent
  one-dimensional output compatibility and construction of the outward search
  space. It performs no independent token/Domain/graph admission, exposes no S3
  extension seam, and has no competing native or R conversion path.
- Deep clone is cold R6 shell-lifecycle orchestration, not another semantic
  exception. It uses an explicit work stack only to preserve shell identity and
  graph topology while shallow-cloning shells; native capsule validation,
  replacement, generation checks, and Shadow signature rebuilding remain the
  authority. Thus the two families above are the complete cold R *semantic*
  orchestration boundary, while clone and detached equality are outward
  shell/presentation glue and must not acquire independent admission or
  mutation rules. The one-way legacy upgrader is outside that current-operation
  count: its R code authenticates a retired private schema, invokes current
  constructors/native validators, and transplants R6 lifecycle state. It is
  never a second implementation or fallback for an operation on a current
  capsule.
- Collection callbacks are not an exception. Live `$extra_trafo` and
  `$constraint` access, detached subset/flatten callbacks, and the corresponding
  adapters used when a SHADOW wraps a COLLECTION all enter one registered
  native evaluator family with shared semantic helpers. Package-owned closures
  retain exactly three plan fields: translation, callback carriers, and owner
  indices. An extra-transformation carrier additionally contains the detached
  BASE shell required for its documented `param_set` argument; a constraint
  carrier does not. Callback selection, translation, merging, result admission,
  and constraint scalar validation are not reimplemented in R and never
  dispatch through an overridden child ParamSet method. Collection
  check/test/assignment sites compute activity over the complete translated
  collection configuration, including collection-level cross-child
  dependencies, and pass each child callback only its active child-scope
  entries with prefixes removed. Detached BASE checking likewise filters
  before it invokes a detached carrier. Activity is derived at the
  authoritative graph site and is never another carrier field or a duplicate
  evaluator inside the schema-free carrier. Collection extra-transformation
  merging keeps retained/untransformed inputs in input order, then appends all
  changed child outputs in callback-plan order (and in each callback's result
  order). Child-owned inputs omitted by their callback disappear, and a changed
  name that collides with retained input is an error.
  Every transformation result and non-table input outer list is ordinary
  non-ALTREP and non-S4. A documented data-frame input may use the exact
  public-table ALTREP boundary above. Semantic atomic leaves and admitted
  columns may be stable ALTREP and enter the same native admission.
  Do not restore the removed namespace-level R `transpose()` implementation or
  the unused table helpers: `Design$transpose()` has one registered native
  semantic engine.
- Direct public `$values <-` assignment accepts an ordinary named base list or
  an ordinary S3-classed named list container. Checked and unchecked assignment
  both reject an outer ALTREP shell before observing its length, names, or
  elements. Both preserve the Paradox-1 clear-values spellings: `NULL`, an
  ordinary attribute-free zero-length atomic/expression vector, or an accepted
  empty list container is canonicalized to a named native `list()`. The outer
  S3 class is representation-only and is discarded before
  validation/storage; it never selects dispatch or another value engine.
  S4/list-like objects and semantic attributes other than `names` and `class`
  remain unsupported. This preserves ordinary configuration objects such as
  bbotk's `local_search_control` without reopening S3 extension seams.
  `$values` is the raw store and therefore includes dormant entries.
  `$get_values(remove_dependencies = TRUE)` (the default) applies the shared
  activity kernel, and `check_required` is evaluated against that filtered
  view. Dependency filtering occurs at the node whose dependency rows own the
  rule: a collection-level cross-child edge filters the collection read, not a
  direct read from either child. The getter builds one operation-local
  open-addressed index over admitted CHARSXP IDs and reuses it for stored-value
  names, tags, and both dependency endpoints. Pointer identity is the common
  path and the encoding-aware comparator is the fallback. A raw read with no
  dependency filtering and no required parameters skips activity evaluation,
  but never skips parameter/dependency/Condition admission. With no ID/tag
  filter it may reuse the rooted admitted schema IDs internally; the outward
  values list and names remain fresh and detached.
- Explicit `$search_space(values=)` input has the same outer-container
  representation boundary: an ordinary named list or an S3-classed named list
  carrying only `names` and `class`. Native code discards the class and selects
  tokens without `[`/S3 dispatch. S4/list-like containers and other semantic
  attributes reject.
- TuneToken admission is closed over exactly five format class vectors:
  `c("FullTuneToken", "TuneToken")`,
  `c("RangeTuneToken", "TuneToken")`,
  `c("ObjectTuneToken", "TuneToken")`, and the two corresponding Full/Range
  vectors prefixed by `"InternalTuneToken"`. The token is an ordinary named
  list with exactly `{content, call}` and no attributes other than exact names
  and class. `call` is an attribute-free, non-missing `character(1)`.
  Full content is exactly `{logscale}`; Range content is exactly
  `{lower, upper, logscale}`; Internal Full/Range content may append one `aggr`
  function and requires false `logscale`; Object content is one admitted
  bounded, value-producing built-in Domain or an exact
  `c("ParamSet", "R6")` shell linked through
  ordinary `self`/`private` bindings to a canonical BASE core. A `ParamUty`
  Domain is unbounded and therefore invalid in the Domain form. A canonical
  zero-level `ParamFct` remains valid for typed empty operations but is not a
  value-producing tuning range. Opaque leaves may still be retained through a
  bounded typed Domain, and opaque target
  results may be constructed through the BASE-ParamSet form. COLLECTION, SHADOW, and
  additive-subclass content is rejected. Names on public scalar
  bounds/flags are representation-only and are discarded from the native
  snapshot. Every interpreted token shell/container/class/name/scalar is
  non-S4. Token internals are not an API: subclasses, extra or
  reordered fields/classes/attributes, malformed calls/content, and recursive
  metadata are rejected before traversal. Opaque documented leaves retain
  identity; Paradox never recursively interprets arbitrary token metadata.
- Exact creator provenance is deliberately not authenticated by generated-R6
  surface inspection. A shell alias that retains the exact genuine BASE
  `self`/`private`/core linkage may therefore be indistinguishable and pass.
  This is safe, but not an extension API: C never calls a candidate/alias method
  and admits only the selected capsule generation. Do not claim that every
  manually assembled look-alike is detected merely because it was not returned
  by a package constructor.
- An `ObjectTuneToken` containing a ParamSet is admitted by `$check()` and
  checked value assignment only after the exact token snapshot above and native
  validation of its nonempty, bounded BASE capsule. Admission executes no
  candidate callback. A rooted private receipt records the exact
  `{shell, private, core}` generation before validation callbacks; all receipts
  are reauthenticated after callback-capable work, and checked assignment ends
  with one allocation-free scan immediately before its atomic commit. A changed
  candidate wins and the outer operation errors without committing.
  Explicit `$search_space(values=)` input enters the same structural boundary,
  then replaces every live ParamSet candidate with a sealed, single-use BASE
  subset capability before any R callback. The cold converter constructs its
  detached search-space ParamSet from that capability and never invokes or
  rereads the original shell.
  The one deterministic `$search_space()` conversion remains the sole boundary
  that evaluates the candidate transformation, one-dimensional result, and
  compatibility with the target Domain. Consequently a structurally valid but
  output-incompatible candidate stores successfully and errors when its search
  space is requested. This intentional Paradox-2 timing change avoids an
  unsnapshotted R callback preflight racing an atomic native commit; malformed
  or corrupt candidate state still fails before storage without mutation.
  Malformed exact-token or Domain structure is a hard boundary error, including
  when encountered by `$check()`; ordinary target-value infeasibility remains a
  returned check diagnostic. Do not turn structural forgery into an ordinary
  value diagnostic or add a recovery path.
- `ParamSet$check_dependencies()` is deliberately narrower than value
  assignment: it accepts one ordinary, uniquely named base list with only its
  names attribute. It reuses the native `$check()` graph snapshot, point
  initializer, and dependency kernel; validates unknown IDs even when there are
  no dependency rows; skips a dependency whose child or parent value is a
  TuneToken; consults recorded defaults only for parents absent from that
  candidate point; and returns `TRUE` or the first diagnostic. It is
  store-blind and does not consult the object's stored `$values`. Do not restore
  the R data.table/pmap traversal or newline-collapsed multi-error result.
- `ParamSet$test_constraint()` and `$test_constraint_dt()` reuse the native
  check graph, point admission, and constraint kernel; there is no scalar or
  per-row R constraint engine. Each callback receives only the default-aware
  active subset of its candidate point. With `assert_value = TRUE`, the table
  method validates every row before running any constraint callback, then calls
  the snapshotted callback set once per row in order. Reentrant callback
  mutation is visible only to the next public operation. The table boundary
  continues to require a data.table.
- Public tag get/set, dependency snapshot/get/set/add, and BASE constraint/
  extra-transformation callback replacement enter registered native mutators.
  The public `$has_deps` flag is a separate registered scalar reader: BASE and
  SHADOW validate the selected canonical dependency table, SHADOW refreshes
  from its live origin once, and COLLECTION performs the same complete graph
  admission as `$deps` before reading the root subtree count. It must never
  construct a detached dependency/data.table facade merely to answer the flag,
  cache graph validity, or use a weaker collection admission mode.
  Dependency projection and bulk `$deps <-` build one owned canonical structural
  snapshot: exact built-in Conditions, valid child IDs, no self-edges, and the
  established allowance for dangling parents. Bulk assignment deliberately
  preserves partially or wholly infeasible predicates and runs no Domain/custom
  check callback. This is required when a consumer copies a dependency graph
  after narrowing a parent Domain: a predicate that became impossible simply
  makes its child permanently inactive. `$add_dep()` is the authoring boundary;
  it additionally checks RHS feasibility in the shared check kernel, detects
  callback reentry by capsule generation, and swaps only after validation.
  SHADOW `$add_dep()` routes through that same strict native append and rejects
  any edge that leaves its visible schema. Do not merge bulk assignment and
  append back into one feasibility mode, or restore R/checkmate/data.table
  mutation planners for these fields.
- Names attached by ordinary R subsetting/arithmetic to scalar Domain
  constructor arguments are representation-only and are discarded from the
  owned native snapshot. Classes and other attributes remain fail-closed.
  Named scalar bounds are common R behavior, not a third-party Domain kind.
- Source references on package-interpreted callbacks are representation-only
  admission metadata. Recursively remove `srcref`, `srcfile`, and
  `wholeSrcref` from stored `custom_check`, individual/extra transformations,
  constraints, aggregation, and internal-tuning callbacks, and from printable
  Domain representation language. Normalize package-generated
  Shadow/Collection callback adapters and flattened internal-tuning namespace
  adapters under the same rule. At ordinary current-object admission, return a
  clean callback unchanged; a stripped copy must retain the exact enclosing
  environment. Read
  `options(paradox.strip_srcrefs = FALSE)` only at admission as a debugging
  opt-out. The recursive source-reference walk never enters arbitrary callback
  environments or anything reached through `$values`, defaults, specials, or
  initialization payloads. Exact legacy-crate authentication may snapshot only
  its fixed known binding set. The independently specified legacy graph crawler
  may still discover a legacy ParamSet shell stored in an opaque value. Apply
  normalization during legacy preparation, before any graph transplant.
  Authenticate only the exact known Paradox-1 package-generated crate shapes:
  categorical mapping, collection-flattened `in_tune_fn`, tuning-ParamSet
  transformation, and detached collection transformation/constraint adapters.
  Rebuild those wrappers without mutating the serialized input. An authenticated
  detached collection wrapper always receives a fresh closure environment so
  carriers can be rebound safely; with stripping disabled, preserve its source
  metadata but do not promise wrapper pointer/environment identity. Treat
  ParamSet carriers captured by an authenticated detached collection adapter as
  explicit migration dependencies, preserving aliases and rebasing them during
  graph transplant; this is not permission to traverse arbitrary callback
  environments. Before R observes such a carrier list,
  `C_upgrade_carrier_list_snapshot` rejects ALTREP/S4/object shells and takes
  one shallow ordinary-list snapshot; never replace that boundary with
  `length()`/`seq_along()` on untrusted legacy state.
- `$subset(..., keep_trafo = FALSE)` is the public way to derive an
  untransformed search space. The final additive argument defaults to `TRUE`
  for BASE, COLLECTION, and SHADOW. `FALSE` makes the single native subset
  transaction omit every selected per-parameter transformation and the
  `extra_trafo` callback while independently preserving the constraint. Do not
  restore downstream mutation of Domain `.trafo`/ParamSet `.trafos`, permissive
  admission of malformed Domains, or a second R reconstruction path for this
  operation. Its three control flags are exact attribute-free, non-missing
  logical scalars. COLLECTION callback detachment follows the callbacks retained
  in the admitted BASE result and must never reinterpret the original flags in
  R or invoke `!`/S3 dispatch. mlr3mbo's Paradox-2 bridge must use this public
  boundary.
- `all.equal()` on the ParamSet family compares a detached semantic view.
  Never delegate equality to `all.equal.environment()`: evaluating inherited
  R6 active bindings can select the wrong parent reader for a COLLECTION, and
  private capsule environments are not the equality contract. This S3 method
  may use base R equality over state projected by the native readers; it has no
  competing C/R equality path and does not validate or interpret capsule state
  independently. The projection contains node class and `assert_values`,
  detached params, values, tags, and dependencies, BASE callbacks, COLLECTION
  children, and the complete SHADOW origin state. It is one flat ordinary list
  with `root`, traversal-ordered `nodes`, and per-node `edge_kind`, `edge_names`,
  and canonical `edge_nodes` IDs. Build it with an explicit work stack: two
  independently constructed equivalent DAGs compare equal while shared and
  duplicated topology compare different; an active-path cycle errors. Derived
  COLLECTION/SHADOW adapter closures are omitted because their authoritative
  child/origin state is already compared.
- A base `ParamSet$extra_trafo` may return an unnamed list; the native engine
  retains it because `to_tune(ParamSet)` and maintained callers use unnamed
  one-dimensional results. If names are supplied they must be complete and
  unique. A child `extra_trafo` in a `ParamSetCollection` must return complete,
  unique names because translating child output into the collection namespace
  is semantically required. This distinction is one native result-admission
  branch, not an R fallback or a second transformation engine.
- `SamplerUnif` and `generate_design_random()` share one capsule-driven native
  uniform engine. `SamplerUnif$samplers` remains descriptive compatibility
  metadata: replacing/reordering the list is an error and child mutation never
  selects another engine. `SamplerUnif` may move each freshly created,
  unexposed singleton subspace through its package-private process-local
  single-use ownership carrier, avoiding a redundant child clone. Reused,
  serialized, malformed, generic, or fabricated carriers reject. Ordinary
  public `Sampler`/`Sampler1DUnif` construction retains defensive cloning, and
  child order/classes/IDs plus serialized public topology are unchanged. Use
  `SamplerHierarchical` for custom 1-D samplers.
  Random, Sobol, LHS, hierarchical, and directly constructed designs retain
  fixed-value overwrite and dependency masking at the ordinary `Design$new()`
  boundary. `generate_design_grid()` is the one deliberate prepared-design
  exception: its registered native operation snapshots the complete
  BASE/COLLECTION/SHADOW graph, realizes and deduplicates axes, collapses fixed
  axes, enumerates dependency-valid branches, restores first-nominal-occurrence
  row order, and returns the final table. It enters `Design$new()` with the
  namespace-owned prepared-grid token and must not pay for or risk divergence
  from a second normalization pass. The grid and Design vector masker share
  the same dependency graph planner and built-in Condition comparator; neither
  may grow a parallel R/data.table evaluator.
  A nominal zero-resolution or zero-level axis still makes the complete grid
  empty before fixed collapse or quantile warnings, but dependency topology is
  admitted first so an empty result cannot hide a cycle. `upper_limit` applies
  to the exact final realized row count. Ordinary same-storage fixed scalars
  retain atomic columns; a valid cross-storage, `NULL`, or S4 special fixed
  leaf is one identity-preserving list-column cell, and fixed TuneTokens receive
  a direct informative grid error.
- Stable ALTREP inputs, including base compact sequences such as `1:n`, are
  supported in documented semantic-vector positions. Structural containers
  remain deliberately ordinary non-ALTREP and non-S4: configuration/search-
  space and transformation list shells, ParamSet constructor `params` lists,
  Domain/Condition/TuneToken/capsule shells, Domain cargo/interpreted cargo
  entries, internal table shells, rows, dimnames, class/name vectors, and other
  list metadata. One shared native classifier governs the six public-table
  ingresses: `check_dt`, `test_constraint_dt`, `qunif`, data-frame `trafo`
  input, `Design$transpose()`, and Design dependency planning. A well-formed
  class vector ends in `"data.frame"` or in
  `c("data.table", "data.frame")`; ordinary leading additive classes are
  representation-only. Native admission never dispatches on those leading
  classes. The classifier does not copy or materialize an ordinary shell merely
  to remove the prefix, and semantic readers ignore it; an ALTREP shell's
  already-required materialized snapshot drops it and installs the canonical
  suffix. Every class
  label is non-missing, non-empty, non-bytes, and unique. The reserved labels
  `"data.table"` and `"data.frame"` may occur only in the recognized terminal
  suffix, so reversed, duplicated, or non-suffix uses reject. The remaining
  attributes must be those allowed for the recognized table kind. Names and
  classes are ordinary, attribute-free character vectors; the historical zero-column
  `structure(list(), class = "data.frame", row.names = ...)` spelling may omit
  names. A data.table's optional `.internal.selfref` is an attribute-free,
  non-S4, nonobject external pointer; `sorted` is an attribute-free ordinary
  character vector; and `index` is an ordinary non-S4, nonobject integer(0)
  carrier. Attributes
  below that `index` carrier are uninspected cache payload. All three carriers
  are discarded; Paradox never consumes their cache contents.

  The raw `row.names` value is an integer or character vector that is non-S4,
  non-object, and attribute-free. Ordinary compact `c(NA_integer_, -n)` and
  `c(NA_integer_, n)` encodings are decoded. A stable integer or character
  ALTREP row-name vector is observed with one Length and no Elt calls because
  labels are irrelevant to all six operations. Row-consuming paths compare
  that count with their admitted columns. Direct `trafo` and dependency
  planning with no dependency rows validate row-name structure but do not add
  column observations solely to compare an otherwise unused dimension. A
  zero-column data.frame retains its row count: `Design$transpose()` returns
  one empty configuration for each row, while an unclassed empty list still
  represents zero rows.

  An admitted suffix-classified top-level VECSXP ALTREP shell is materialized
  once. That owned snapshot installs only the canonical recognized class
  suffix; ordinary admitted shells are not copied merely to remove an inert
  prefix. Native
  admission owns names and classes before any callback-capable row-name Length
  or top-shell Length/Elt observation, calls top-shell Length once and Elt once
  per column, preserves column identities, canonicalizes captured row names by
  count, and drops ignored data.table caches. Base R's lazy attribute-only
  duplicate is the common motivating case; the contract does not depend on its
  current internal width threshold. Admitted semantic atomic columns may be
  stable ALTREP. Package-owned capsule tables and returned facades keep
  canonical ordinary metadata; this input exception does not admit ALTREP
  capsule, Domain, Condition, TuneToken, row, callback-result, or general list
  shells. Raw attribute selection uses `R_mapAttrib()` on R >= 4.6 and one
  exact, ledgered `ATTRIB` compatibility exception on R 3.6--4.5. Neither path
  invokes R or data.table fallback logic. Direct checked or unchecked `$values <-`
  assignment rejects an outer
  ALTREP before observation and canonicalizes the accepted Paradox-1 empty
  spellings (`NULL`, an ordinary attribute-free zero-length atomic/expression
  vector, or an accepted empty list container) to a named list in C.
  `set_values(.values=)` is the sole general-list exception: its merge boundary
  snapshots the supplied shell once before validation. This is
  an explicit operation contract, not a general list-ALTREP fallback. The
  semantic materialize-once guarantee begins at native
  admission, after a thin R wrapper may have captured documented language or
  printable representation metadata. Native admission materializes each
  semantic vector once into rooted ordinary storage before the operation
  snapshot, and that native snapshot is the sole semantic authority. A hostile
  custom ALTREP whose observation changes between R-side capture and native
  admission is unsupported: its printed representation need not agree with the
  admitted value, but it must be rejected or handled without replay or Paradox
  itself causing a crash or memory corruption. Capsules never permanently
  store semantic ALTREP vectors; canonical compact data-frame row names are the
  representation-only exception. This general support does not override the
  typed-Domain rule above: an ALTREP special-value leaf for Dbl/Int/Fct/Lgl is
  rejected rather than observed; an admitted S4 special matches only by pointer
  identity. ParamUty opaque leaves are not materialized, except that special
  membership uses base `identical()` without S3/S4 dispatch.
- data.table >= 1.18.4 is an outward interoperability dependency only. Do not
  restore the old `alloc.col()` capacity bridge or call data.table internals.
  The sole cold presentation exception is the identity lookup in
  `R/ParamSet.R` for data.table's unexported `.reassign_extracted_table` and
  exported `set` R functions. It calls neither function: it only recognizes
  the active data.table stack so detached `$params` facades retain historical
  `:=`/`set()` reassignment behavior. This lookup supplies no semantic engine,
  private C API, capacity/version bridge, or permission for another unexported
  API.
- `upgrade_paradox_object()` remains the pure, non-mutating converter for one
  directly supplied Paradox object. It returns a new current ParamSet-family
  graph for a canonical built-in legacy ParamSet/Collection, returns an
  authenticated current object unchanged, and also owns standalone built-in
  Domain/Condition normalization. On R 3.6 its attempt to authenticate a
  legacy ParamSet-family R6 shell fails closed because active-binding functions
  cannot be inspected; run that conversion under R >= 4.0. It never becomes an
  in-place crawler.
- `upgrade_paradox_object_graph(x)` is the identity-preserving migration
  boundary for a containing object graph. It returns `x` invisibly and
  transplants every admitted legacy ParamSet-family R6 shell in place, including
  shells below a current `.core` payload. Every replacement preserves the
  shell's public `assert_values` policy; this is the sole serialized shell
  state outside the capsule and is not a constructor default. Its native
  iterative discovery uses
  pointer identity, an explicit work stack, and periodic interrupt checks. It
  follows ordinary lists, pairlists/language/expressions, attributes and S4
  slots, environment bindings and parents, active-binding *functions*, closure
  environments/formals/bodies and bytecode expressions, and promise-binding
  expression/environment or forced-value state—including `...` cells—where a
  policy-compliant API exposes them, without forcing an unforced promise.
  R 3.6--4.4 can inspect a reached promise through their compatibility
  accessors. R 4.5 classifies those accessors as non-API and has no replacement,
  so recursive migration fails closed on a reached promise and directs the
  caller to R >= 4.6. R >= 4.6 uses the public binding/dots API, while a
  detached `PROMSXP` outside such a cell remains opaque. `R_getVar` is excluded
  before R 4.6 because it could force a delayed binding without the new
  classifier. Current source has three reviewed retrieval call sites—one
  direct-value facade plus graph direct/forced-value branches—and every site
  follows `R_GetBindingType`; the DSO has one undefined-symbol inventory row.
  It never invokes an active binding or a serialized method.
  R 3.6 exposes no accessor for an active binding's function; encountering an
  arbitrary one during recursive migration therefore fails closed with an
  instruction to perform that migration under R >= 4.0. The one narrow
  exception is an exact built-in current Paradox-2 shell: native topology,
  class, policy, and capsule receipts admit its capsule as the graph authority,
  locked methods are skipped, and unlocked replacement closures remain graph
  edges. Package active facades are necessarily opaque on R 3.6. Replacing a
  core method or active binding is unsupported; an in-place active-binding
  replacement, or a method replacement that is relocked, preserves every
  receipt available on that runtime and cannot be distinguished from generated
  code. Anything reachable only from such a closure is not traversed (and an
  active binding is never invoked). Additive shells and
  modifications that prevent exact authentication fail closed instead of
  receiving this exception. Paradox-1 ParamSet-family R6 shells contain active
  bindings, so their practical object/graph migration requires R >= 4.0.
  Ordinary current-object operations, exact built-in current-object graph
  traversal, idempotent conversion of current objects, standalone legacy
  Domain/Condition conversion, and graphs without arbitrary active bindings
  remain supported on R 3.6.
  Direct environment bindings are classified natively rather than inferred
  from an R expression: a realized language object or symbol remains a realized
  value, while a delayed promise with the same apparent expression is identified
  as a promise and never evaluated.
  `.GlobalEnv`, namespaces, package/import environments, attached search-path
  infrastructure, Autoloads, base, and the empty environment are hard
  boundaries. An `imports:` display name alone is not authentication: a
  genuine imports boundary must have an ordinary scalar raw `name` attribute
  with that prefix and the exact base namespace as its direct parent. A user
  environment with a spoofed prefix remains traversable. Generic
  external-pointer internals and weak references are
  opaque; the protected ordinary-R payload of an authenticated Paradox `.core`
  is the sole external-pointer exception.
- Current ParamSet-family shell admission has one native inert classifier.
  The class attribute must be an ordinary, attribute-free, non-ALTREP character
  vector with unique nonempty labels and the terminal suffix
  `c("ParamSet", "R6")`. An immediately preceding
  `"ParamSetCollection"` or `"ParamSetShadow"` selects that family; any earlier
  nonreserved labels are additive R6 subclass layers. With no family marker the
  shell is BASE. The selected family must agree with a canonical package-owned
  core, every Shadow must have exact snapshot metadata, and `assert_values`
  must be an exact attribute-free non-missing `logical(1)`. This current-shell
  classifier is deliberately broader than the exact one-owner legacy registry
  key below; neither invokes S3 or a shell method.
- A graph migration performs complete discovery, authentication, offside
  semantic preparation, replacement construction, and shell-shape auditing.
  It then validates every prepared/current root together in one native
  read-only barrier before the first transplant; all selected generations stay
  rooted until the complete set has passed an allocation-free receipt scan.
  Discovery reports current as well as legacy
  ParamSet-family shells: every current capsule graph is semantically
  validated during that same preflight, even when the current shell itself
  needs no transplant. A corrupt current capsule hidden beside a valid legacy
  shell therefore aborts before the legacy shell changes; a shallow carrier
  check is not a sufficient migration boundary. Current Shadow validation is
  authoritative but read-only: it builds the live semantic generation without
  installing it, retains the selected private `.core` as a separate source
  receipt, and derives collection callback detachment from that same admitted
  graph rather than rereading live shells.
  Commit is post-order, identity-preserving, and monotonic. Once a child has
  been transplanted, its prepared parent's child/origin edge is rebased to the
  identity-preserved original shell. Because rebasing can allocate or execute a
  registered replacement-owner factory, every rebase is followed immediately
  by a joint validation of all already-current identity roots plus that newly
  rebased prepared root, ending in an allocation-free receipt scan. Unrebased
  parents are offside templates rather than live shells after their prepared
  child's enclosure has moved; each re-enters the barrier when its own
  dependencies are rebased. After each transplant, the identity-preserved
  original joins the current-root set and that set is jointly validated again:
  each shell swaps `.__enclos_env__` only after every other binding and
  enclosure link is ready. A completed node is a valid current object; a
  catastrophic allocation failure inside a binding wave leaves the old
  enclosure as its completion marker and authenticates both original and
  already-refreshed package-owned methods (including an interrupted unlocked
  method) for retry. Rerunning the idempotent graph upgrader completes the
  remainder. Ordinary validation or owner-bridge failure occurs during
  preflight and mutates nothing. This atomicity statement does not cover a
  pending finalizer from an unrelated user object that deliberately mutates a
  selected root inside the R-level binding wave: `suspendInterrupts()` does not
  suppress such finalizers. The post-transplant joint barrier detects that
  mutation and errors, but a completed transplant is not rolled back and the
  externally corrupted graph is not promised to be retryable.
- The cold transplant resolves base `unlockBinding` with an explicit
  `get(..., baseenv())` call. R's package-tampering checker otherwise reports
  every syntactic `unlockBinding(name, owner)` whose environment is not the
  literal Paradox namespace, even though `owner` here is a fully preflighted R6
  shell/enclosure. Do not replace this with namespace mutation or remove the
  authentication/lock restoration around it.
- Current Paradox R6 stubs call versioned `.__paradox2_*` namespace targets
  directly. Historical unversioned `.__ParamSet*`,
  `.__ParamSetCollection*`, and pre-release `.__ParamSetShadow*` names are cold
  first-use gateways only. One native context snapshot applies the same
  ordinary suffix classifier, requires the exact public `assert_values` policy
  and a canonical matching core, follows the authenticated additive superclass
  chain to the enclosure that defines the requested BASE/COLLECTION/SHADOW
  target, and roots the resulting enclosure/private/super/core receipt. It does
  not evaluate the serialized stub's `private` or `super` promises, replay a
  guessed top enclosure slice, or reread the shell in R after authentication.
  Borrowing another current object's enclosure is not authentication. In
  particular, the actual pre-release Paradox-2 Shadow needs no owner registry.
  A shell without an authenticated current context defaults to a precise error
  directing the caller to `upgrade_paradox_object_graph()`. Setting
  `options(paradox.legacy_object_action = "upgrade")` opts into silent
  identity-preserving first-use migration and then resumes the requested
  operation. Invalid option values fail closed. Do not put a gateway on a
  current hot path or restore direct `mlr3misc::leanify_package()` use.
- `register_paradox_object_upgrader()` is the sole narrow owner-package
  extension for serialized ParamSet subclasses. It exact-matches one full
  `c(<owner class>, "ParamSet", "R6")` vector and records only an authenticated
  owner namespace plus namespace-local inspector/rebuilder names, migration
  kind (`"additive"` or `"replacement"`), and declared retired bindings. It
  has no S3/superclass dispatch and never stores or calls a function recovered
  from serialized bytes. An additive inspector returns an empty named
  dependency list because its authenticated BASE is the sole inherited
  dependency. A replacement inspector returns exactly one dependency named
  `origin`; its rebuilder must return the exact registered class backed by a
  current `ParamSetShadow` capsule. Public/private R6 finalizers are rejected:
  transplanting their registration between environment identities risks
  premature or double cleanup. bbotk's legacy `Codomain` is the maintained
  additive case. miesmuschel's legacy Shadow is the maintained replacement,
  with its old-only `params_unid` and `set_id` bindings retired explicitly.
  Unknown classes and undeclared owner fields fail closed. Registration cannot
  intercept serialized owner-local
  lean targets: every owner package that emitted them must reserve the exact
  historical names as cold default-error/opt-in-upgrade gateways and replay
  from the transplanted enclosure. In particular, bbotk must cover all
  `.__Codomain__*` targets, including `$clone()`, and miesmuschel must cover
  all historical Shadow overrides.
- Built-in legacy method enclosures must have the exact currently loaded
  Paradox namespace as their parent. Registered owner enclosures must likewise
  have the exact namespace incarnation retained by the registry, followed by
  the exact current Paradox namespace. `isNamespace()` plus
  `environmentName()` is descriptive metadata and is not authentication: an
  ordinary environment can spoof both.
- Shipped C is portable C99 and supports R >= 3.6. API spelling selection is
  centralized in `src/r_api_compat.c`; the graph walker has only the narrow
  capability gates required to select which inert edges a runtime can expose,
  never a duplicated old-R engine. Current runtimes retain their public,
  allocation-free ordinary-frame fast paths. The compatibility boundary keeps
  one conservative rooting proof across all supported branches because hostile
  class metadata can allocate during facade admission, while recognized
  callback-backed user databases are rejected before binding APIs.
  R 3.6--4.5 use one exact, ledgered `ATTRIB` occurrence for raw attribute
  iteration that cannot be expressed through the earlier API without
  expanding compact `row.names`. R 3.6--4.4 retain one ledgered `FORMALS`
  occurrence because transformation callback admission is a semantic hot path;
  cold closure-body/environment traversal instead uses public base calls and
  must not compile `R_ClosureExpr` or `R_BytecodeExpr` before they become API.
  R 3.6--4.1 use a cold
  `base::exists(..., inherits = FALSE)` query only where absence is an accepted
  result. Candidate-shell and fresh-destination classifiers use that optional
  path, while admitted core/generation reads keep the required native path so
  old R does not evaluate `base::exists()` on every hot operation. Required
  binding snapshots remain allocation-free. Their terminal
  optional receipt scan fails closed when `R_HasFancyBindings()` reports a
  locked or active frame, then uses the same stored-cell path; this old-only
  exception avoids either evaluator allocation or invocation of an active
  binding. The facade rejects `UserDefinedDatabase` environments through the
  same public inheritance predicate R uses before any binding operation:
  their callback-backed table is unsupported, and old
  `R_HasFancyBindings()` assumes an incompatible ordinary-frame layout.
  The graph walker applies this boundary before namespace/package
  classification because old R implements those predicates through an
  object-table lookup.
  R 3.6--4.5 use the declared/exported `Rf_findVarInFrame` to obtain the stored
  frame cell. Only R 3.6--4.4 inspect a returned `PROMSXP`, through the three
  header-declared/exported accessors `R_PromiseExpr`, `PRENV`, and `PRVALUE`.
  R 4.5's compiled-code policy classifies those accessors as non-API, so its
  crawler fails closed when it reaches a promise and requests migration under
  R >= 4.6. R >= 4.6 uses only the documented experimental binding/dots APIs:
  the DSO contains none of the three detached-promise accessors, and a
  structurally reached non-binding `PROMSXP` is opaque. An R-level
  `substitute()` workaround is
  insufficient for receipt scans and recursive graph discovery: although
  non-forcing, it returns a promise expression and cannot distinguish that
  expression from a realized language/symbol value or provide a stable
  binding-generation receipt. Every such symbol/version/source occurrence must be listed
  exactly in `environment/r-api-exceptions.tsv`, raw-token and DSO audited, and
  tested against pinned headers and real runtimes before freeze.
  Although `R_getVar` is public from R 4.5, it may force a delayed binding
  before R 4.6's classifier can distinguish the cell. All pre-4.6 inventories
  therefore forbid it. The raw-token audit requires the exact three current
  call sites and every call follows `R_GetBindingType` after direct or forced
  value classification; the DSO inventory requires one undefined-symbol row.
  Printable simple-Domain IDs retain one native renderer on every supported
  R. R >= 4.5 snapshots `scipen` with the documented allocation-free
  `Rf_GetOption1`; R 3.6--4.4 call public `base::getOption()` through the
  compatibility facade and then stay in the same native renderer. The older
  runtimes must not call their header-declared but then-undocumented
  `Rf_GetOption1`, and they must not fall back unconditionally to R
  `deparse1()`. Runtime DSO inventories forbid that symbol through R 4.4 and
  require it exactly once beginning with R 4.5. A malformed or changing option,
  unsupported representation, or overlong output still takes the existing
  correctness fallback.
  `R_HasFancyBindings`, `Rf_findVarInFrame`, `R_PromiseExpr`, `PRENV`, and
  `PRVALUE` are confined to their exact old-runtime branches and ledgered; the
  last three are absent beginning with R 4.5.
  The exact R 4.5.2 runtime stage also runs that runtime's own
  `tools:::check_compiled_code()` against the installed package and retains an
  authenticated zero-issue receipt. The explicit DSO inventory is a semantic
  branch proof, not a substitute for R's policy checker.
  Pre-4.6 required snapshots authenticate once and then use the centralized
  stored-cell selector; do not reintroduce a duplicate active/class boundary
  into this hot path.
  None is a
  CRAN allowlist or permission for another internal API or semantic path.
  Old-Windows portability does not rely on `%lld`, `%I64`, or `j`/`z`/`t`
  integer-length formats: graph indices use bounded decimal arithmetic and
  diagnostic-only long-vector positions use exact `%.0f`/double formatting
  within R's 2^52 long-vector limit. The strict source/header gate rejects
  those integer format spellings, including decorated variants. Formatted
  ParamSet failures never size with `vsnprintf(NULL, 0, ...)`: Rtools35's
  MSVCRT path returns a negative value for that idiom and for truncated real
  buffers. The single formatter instead uses a real local buffer, `va_copy`,
  bounded growth, and supports both the C99 required-length and old-MSVCRT
  negative-on-truncation conventions. All of its format arguments are package-
  bounded; the 64-KiB corruption/formatter-failure ceiling therefore truncates
  no supported diagnostic. The source gate rejects null-buffer printf sizing.
  GCC-only
  diagnostic pragmas are compiler-
  version gated, and strict/analyzer/sanitizer profiles compile the package as
  GNU C99 rather than proving only that C17 accepts it.
  `src/binding_snapshot.c` exposes the same centralized classifier to the cold
  R migration/gateway code as one registered native call: it returns an exact
  realized ordinary frame value, or a negative result for absent, inherited,
  active, or delayed bindings, without evaluation. Do not recreate a
  `substitute()`-based classifier in R; realized language objects and symbols
  as well as literal promises whose expressions are `TRUE`, `NULL`, a language
  object, a symbol, an environment, a closure, or an external pointer prove why
  expression/type shape cannot distinguish a delayed binding from a realized
  value.
  Treat R API predicates as predicates rather than assuming a stable
  integer typedef: when storing their result, normalize it with an explicit
  comparison such as `predicate(...) != FALSE`. The pinned old-header compiler
  matrix is authoritative for signedness and declaration drift. The params
  reader accepts an already admitted, rooted core directly; collection reads
  do not allocate a temporary environment or depend on `R_NewEnv()`. R 3.6
  lacks list ALTREP, so only the package's adversarial VECSXP ALTREP test
  fixture is unavailable there. Production list-ALTREP branches are vacuous
  below R 4.3, while atomic ALTREP and every ordinary-container contract remain
  tested. Linux, Windows x86-64, and Apple ARM64 remain first-class targets.
  Corrupt/forged state must error and must never cause an out-of-bounds access,
  stale pointer, double evaluation, or segfault.

Do not leave obsolete compatibility code merely unreachable. Before release,
all semantic translation units must be free of generated-closure/body
authentication and sentinel-to-R replay. Temporary migration adapters must be
marked, have no alternate semantics, and be deleted before the candidate ref.
The documented versioned-target/historical-gateway layer and exact owner
registry are the permanent serialized-object migration boundary, not temporary
adapters.

## Repository-local environment

The host R 3.6.3 and host/user libraries are out of scope and must not be
modified. Never use `sudo`, edit shell startup files, or install into HOME,
`/usr`, or a system R library.

Provision once, then activate from the repository root:

```sh
scripts/bootstrap
. scripts/activate
```

Activation selects the pinned local R 4.6.1 toolchain and
`.local/R/library`, clears inherited compiler/library variables, and redirects
temporary and cache state below the repository. Confirm retained work with:

```sh
test "$PARADOX_ACTIVE_ROOT" = "$(pwd -P)"
test "$(command -v R)" = "$PARADOX_ROOT/.local/toolchain/bin/R"
test "$(command -v Rscript)" = "$PARADOX_ROOT/.local/toolchain/bin/Rscript"
test "$(R RHOME)" = "$PARADOX_ROOT/.local/toolchain/lib/R"
```

Core authenticated inputs include:

- `environment/toolchain-linux-64.lock`: local development toolchain;
- `environment/r-packages-linux-64.lock`: exact source-package closure;
- `environment/runtime-r-3.6.3-linux-64.lock`,
  `environment/runtime-r-4.0.5-linux-64.lock`,
  `environment/runtime-r-4.3.3-linux-64.lock`, and
  `environment/runtime-r-4.5.2-linux-64.lock`: supported-runtime prefixes;
- `environment/runtime-r-3.6.3-packages.lock` and
  `environment/runtime-r-4.0.5-packages.lock`: exact source-package closures
  for the two older runtime axes;
- `environment/runtime-r-3.6.3-declared-floor-packages.lock`: the separate
  exact declared-floor source closure;
- `environment/runtime-r-3.6.3-prefix-repair.lock`: the authenticated repair
  boundary for the managed R 3.6 prefix;
- `environment/runtime-matrix.tsv`: authenticated runtime-axis registry;
- `environment/runtime-matrix-old-r-stress.tsv`: exact bounded old-runtime
  stress targets;
- `environment/r-api-sources.tsv`: local reference R sources/manuals;
- `environment/r-api-exceptions.tsv`: the exact reviewed versioned R C API
  exception ledger;
- `environment/valgrind-r-packages.tsv`: instrumented-R package closure.

`scripts/environment/runtime-matrix-trusted-inputs` is the exhaustive
runtime-matrix input enumerator. It also binds the floor-smoke,
old-runtime-stress, cross-serialization, result-skip, and verification policy
programs into retained evidence; do not maintain a competing hand-written
exhaustive list here.

Bulky state is deliberately ignored below `.local/` and `.cache/`. Bootstrap
is idempotent and checksum-verifying; reuse valid downloads and installations
instead of rebuilding them. A lock mismatch fails closed and requires an
explicit reviewed lock refresh.

The R 4.3.3 conda prefix contains data.table 1.17.8 because no matching
conda-forge R-4.3 build of 1.18.4 exists. `scripts/test-runtime-matrix` copies
the SHA-256-pinned cached source
`.cache/downloads/r-packages/data.table_1.18.4.tar.gz` into the isolated R-4.3
stage and installs it before Paradox. R 4.5.2 and the primary library already
contain 1.18.4. Never weaken `DESCRIPTION` or restore the capacity bridge for
this test-infrastructure detail.

Provision and inspect real older runtimes with:

```sh
scripts/bootstrap-runtime-matrix
scripts/bootstrap-runtime-matrix --verify
. scripts/activate-runtime-matrix 3.6.3   # or 4.0.5 / 4.3.3 / 4.5.2
. scripts/activate                        # return to R 4.6.1
```

The source-with-version spelling requires Bash or zsh. dash does not forward
arguments to its dot builtin; repository automation uses Bash even though the
activation body itself remains portable shell.

Micromamba must create every supported-runtime prefix with `--always-copy`.
Never seal a prefix whose regular files are hard-linked to the writable
micromamba package cache or to another prefix: a single in-place cache write
would mutate several supposedly immutable runtimes. The complete prefix
receipt rejects every multiply-linked regular file in addition to checking
content, modes, paths, and internal symlinks. A prefix made by the older
hard-linking bootstrap must be deliberately moved aside or otherwise
reprovisioned from the exact lock; do not weaken or refresh its receipt to
admit shared inodes, and do not auto-delete it.

R 3.6.3 and R 4.0.5 install their exact complete-test source locks once into sealed,
content- and identity-receipted libraries. Interactive activation fully
verifies the selected library; a retained runtime worker instead consumes the
coordinator's exact authenticated receipt handoff and does not repeat the full
tree check. A source-library build key is scoped to the selected runtime's
artifact fields, exact locks, authenticated prefix-content manifest, and the
explicit install schema; verifier/helper identity remains refreshable receipt
provenance. Therefore an unrelated registry row or verification-only edit must
not rebuild an unchanged package closure. Any install-algorithm or isolated
build-environment change must bump `SOURCE_LIBRARY_BUILD_SCHEMA`. One
source-derived trusted-input manifest covers every helper,
registry, policy, dependency lock, and prefix-repair lock. The coordinator
checks it immediately before and after the worker wave and binds it into the
top-level and per-stage evidence. The declared-minimum R 3.6 package check uses
`--no-tests`; the separately receipted complete-source stage already runs the
supported test suite once. Its locked local test library contains five of the
nine direct Suggests and deliberately omits exactly `reticulate`, `rmarkdown`,
`mlr3learners`, and `e1071`. The check must therefore exit zero with exactly
that one missing-Suggests dependency NOTE, no other NOTE/WARNING/ERROR/halt,
and one sole final `Status: 1 NOTE`; `Status: OK` is not a truthful contract for
this axis.

`scripts/bootstrap-runtime-matrix` alone owns persistent runtime and
dependency-library provisioning. For an R 3.6.3 selection, provision mode owns
both the complete-test and declared-floor closures, while `--verify` checks
both without mutation. `scripts/test-runtime-matrix` may only consume those
verified caches and copy their receipts into a retained run. Its trusted-input
inventory includes `environment/Renviron`, `environment/Rprofile.R`, and
`environment/Makevars`; every retained stage points the corresponding R
variables, including `R_BUILD_ENVIRON`, `R_CHECK_ENVIRON`, and
`R_INSTALL_ENVIRON`, to the detached authenticated copies below its retained
inputs. Stage activation selects those copies before its first R child;
interactive activation alone selects the live developer files. Never restore
coordinator-side cache repair or a live
checkout startup path inside an admitted stage. Successful bootstrap
verification also removes its owned empty scratch directories after closing
their contents; it must not leak one directory per runtime replay.

The R 3.6 stage separately consumes the `declared-floor` profile of the same
sealed library manager. Its exact lock contains backports 1.1.7, checkmate
2.0.0, data.table 1.18.4, mlr3misc 0.10.0, R6 2.6.1, and the sole transitive
dependency digest 0.6.39. The stage rebuilds only Paradox into a fresh
candidate library against those floors, authenticates every installed package
identity and dependency namespace origin, plus the candidate Paradox DLL origin
and registration, and exercises constructor/deparse, checked dormant/dependency
behavior, diagnostics, and grid/data.table/R6 paths. Cache keys and receipt
handoffs keep this bounded lane reusable; ambient `R_DEFAULT_PACKAGES` is
removed so an operator startup choice cannot preload a reviewed dependency.

R 3.6.3 and R 4.0.5 additionally run the bounded `NOT_CRAN=true`
`runtime-matrix-old-r-stress.tsv` slice. Its literal test titles are validated
against the authenticated source, and a deterministically regenerated final
test helper leaves top-level setup intact while declining to force every
unselected `test_that()` body. The retained staged sources, title filter,
per-block ledger, log, and counts must replay exactly. Each old runtime first
proves that an unselected body is unforced and that a selected braced
expression still receives testthat's isolated evaluation environment;
selected targets may neither skip nor warn. Do not replace this with full-file
`NOT_CRAN=true` runs
or a frozen expected target count, and do not repeat it on newer runtimes.

After every selected stage is sealed, a complete `--runtime all` run performs
one R 4.0.5 -> R 3.6.3 serialization handoff. The producer and consumer are
bound to their exact stage package and DSO paths. The fixture covers base,
collection, and shadow capsules; shared topology; an attribute-hidden closure;
dormant values including named `NULL`; callbacks, checks, transformations,
mutation, and an R 3.6 round trip. Its artifacts, isolated state, logs,
per-stage seals, candidate DSOs, and receipts are independently sealed and
replayed. A partial matrix records `not-applicable` and retains no such
artifact.

`scripts/test-runtime-matrix` validates both the deliberately empty pre-R-4.6
exclusion policy and the reviewed result-skip manifest against the exact
extracted candidate's current `skip_on_cran` test titles before resource
admission or any runtime build/install worker starts. Keep that source-derived
policy preflight shared with the old-runtime test runner. The coordinator also
stages and authenticates the mandatory `mbo_config` upgrade inputs before that
same boundary, so the fixture test must execute and may not become an
unreviewed environment-dependent skip. Malformed or stale input therefore
fails cheaply instead of after the runtime package installations. In
particular, validate the header-only exclusion manifest as zero rows; do not
construct a synthetic file name from its empty `context` column.

Reference R source, Writing R Extensions, R Internals, data.table source, and
the analyzer sources are populated by `scripts/fetch-reference-sources`.
Consult those pinned local sources rather than remembered C-API behavior.

The mandatory legacy-upgrade fixtures originate below the reviewed
`mbo_config` commit's `common/` directory, not at its repository root. Before
an ad-hoc direct complete unit-test run, bind the directory that actually owns
the two RDS files:

```sh
fixture_bundle="$PARADOX_ROOT/.local/tmp/mbo-config-fixtures-development"
if test -d "$fixture_bundle"; then
  scripts/environment/mbo-config-fixtures verify "$PARADOX_ROOT" \
    "$PARADOX_ROOT/.local/compat/github/mbo_config" "$fixture_bundle"
else
  scripts/environment/mbo-config-fixtures stage "$PARADOX_ROOT" \
    "$PARADOX_ROOT/.local/compat/github/mbo_config" "$fixture_bundle"
fi
export PARADOX_MBO_CONFIG_ROOT="$fixture_bundle/common"
test -f "$PARADOX_MBO_CONFIG_ROOT/mixed_search_space.rds"
test -f "$PARADOX_MBO_CONFIG_ROOT/numeric_search_space.rds"
```

An unset value deliberately skips that optional development fixture; every
release run sets it and treats either missing file as a failure. The shared
`scripts/environment/mbo-config-fixtures` helper requires
`compat/github-snapshot.tsv` and `compat/mlr-org-review.tsv` to agree on the
exact reviewed commit/tree, reads the two files from immutable Git objects
rather than the checkout worktree, and publishes a read-only, receipted
bundle. `scripts/native-check` retains that bundle inside the functional
mode's sealed artifact tree. `scripts/test-runtime-matrix` stages one shared
bundle before resource admission and gives both old-R workers its `common/`
directory; the evidence verifier reauthenticates the repository manifests,
Git tree, file bytes, and complete bundle receipt. A dirty or differently
checked-out worktree is irrelevant provided the reviewed Git objects remain
present. The documentation and consumer runners have their own exact-corpus
receipts and do not infer this path from HOME.

Documentation generation is optional development tooling and is deliberately
kept out of the pinned runtime library.  The current generator is roxygen2
8.0.0 in `.local/R/tooling-library`; its cached source archive is
`.cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz` with SHA-256
`75816bf3a25554f5752254b985b7242490b0fabe68a5ada335c340b820ba34e8`.
Recreate it in the separate tooling library:

```sh
. scripts/activate
mkdir -p .local/R/tooling-library .cache/downloads/r-tooling
if [ ! -f .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz ]; then
  curl -fL https://cran.r-project.org/src/contrib/roxygen2_8.0.0.tar.gz \
    -o .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz ||
    curl -fL https://cran.r-project.org/src/contrib/Archive/roxygen2/roxygen2_8.0.0.tar.gz \
      -o .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz
fi
test "$(sha256sum .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz | cut -d' ' -f1)" = \
  75816bf3a25554f5752254b985b7242490b0fabe68a5ada335c340b820ba34e8
R CMD INSTALL --library=.local/R/tooling-library \
  .cache/downloads/r-tooling/roxygen2_8.0.0.tar.gz
Rscript -e '.libPaths(c(".local/R/tooling-library", .libPaths())); roxygen2::roxygenise(".")'
rm -f src/*.o src/paradox.so
```

The hard dependencies of that generator are already in the pinned development
library. Roxygen 8 needs the R6 method source references supplied by its normal
pkgload method; its source-only loader exits successfully but writes incomplete
R6 method documentation. Consequently a documentation refresh performs one
disposable debug build. Run it only after the R surface has converged, remove
the live-root build artifacts immediately, and do not treat that build as
candidate evidence. Do not install an older generator closure into the runtime
library.

## Editing and native-code rules

- Preserve unrelated user changes and dirty-worktree state.
- Use `apply_patch` for source edits. Generated documentation may be refreshed
  with its normal generator only when that is the intended mechanical change.
- Register every `.Call` routine with a fixed arity, disable dynamic symbol
  lookup, and keep headers, `src/init.c`, the routine coverage ledger, and
  direct probes synchronized.
- Validate every R type, length, name, attribute, row count, graph edge,
  arithmetic conversion, and index before use. A version tag is not a safety
  proof.
- Never retain an unprotected R object or raw vector pointer across allocation,
  callback evaluation, interrupt polling, ALTREP access, or error construction.
- Use checked `R_xlen_t`/`size_t` arithmetic and iterative graph traversal.
- Snapshot callback-bearing state before execution. A reentrant nested
  operation sees current state; the enclosing operation finishes from its
  snapshot. Mutating commits detect intervening capsule replacement and fail
  without overwriting it.
- Construct public data.table facades at the boundary, finalize their public
  self-reference, and ensure every mutable shell and column is newly owned or
  detached from the capsule. The compact fresh-facade helper is only for a
  package-owned shell with package-owned metadata; caller-owned input uses the
  defensive finalizer. Never synthesize private indices or call private C APIs.
- Prefer a compact readable native operation over layers of helpers that only
  existed to reproduce an R vectorization pattern. Avoid per-row R calls except
  documented callbacks.
- Package-owned mapping closures should be created by small fixed factories
  with only their required bindings. Do not compile a fresh closure through
  `crate()` for every Domain construction; this was a measured hot-path cost
  and provides no additional state isolation.
- A ParamSet constructor with no initial values does not enter an empty native
  value-store transaction. Complete collection-value admission retains the
  already resolved BASE parameter row in its operation-local plan rather than
  searching the same ID again during upward translation. The inherited native
  Shadow dependency reader owns refresh; its R binding must not refresh a
  second time. `$has_deps` reads the validated native dependency count directly
  instead of projecting and wrapping `$deps`; its retained 64-parameter
  BASE/COLLECTION/SHADOW probe moved from 498.575 to 156.065 microseconds
  (3.195x) without a weaker graph path. The one-evaluation allocation profile
  remained exactly 12,688 bytes in 14 records on both sides, so record this as
  a latency improvement, not an allocation improvement. The exact libraries,
  fingerprints, 50-iteration/three-warmup method, and raw evidence path are in
  `benchmarks/README.md`. These measured shortcuts remove redundant work
  without caching graph validity or weakening admission.
- Collection graph readers keep bounded inline scratch for 16 nodes, then grow
  all coordinated node/path/postorder arrays together with checked temporary
  allocation. Admission retains and reuses the first prior-node lookup instead
  of rescanning the prefix. This removes 2,232 bytes of scratch at 16 nodes and
  7,008 bytes at 64/256 nodes; it does not create a trusted-node bit, skip a
  graph/generation check, or change shared-DAG/cycle behavior.
- Keep unavoidable package-owned callback wrappers thin. In particular,
  `to_tune(ParamSet)` may call the supplied transformation and perform its
  one-list-result/name boundary directly; it must not add checkmate or
  mlr3misc layers to every callback execution.
- A sparse-target `$search_space()` facade redesign was measured and rejected:
  search-space conversion is cold and the end-to-end maintained workload moved
  only about 2%, which did not justify another target-projection path. Do not
  restore that experiment without new representative evidence. A native bulk-
  dependency constructor transaction is also a measured no-go for the 2.0.0
  release. A 64-parameter/27-requirement isolated estimate moved from 6.57 ms to
  4.11 ms (with requirement-heavy estimates spanning roughly 1.4--2.5x), but
  representative xgboost learner construction improved only about 6--7%. The
  change requires a moderate-risk new native batch transaction, while the
  maintained release workload currently lacks dependency-rich constructor
  coverage. Under release steering this is not low-hanging. Retain the landed
  low-risk wins; the batch design may be revisited as an internal optimization
  without another compatibility or API break.
- Keep the shared public-table classifier and row-count admission explicit at
  each ingress. The final bounded audit found representative hardened paths
  flat to 2.8% faster than the superseded candidate; deliberately minimal
  keyed/indexed data.table paths paid only 0.6--1.1 microseconds. Repeated
  symbol lookup costs under 79 ns per table and a complete keyed/indexed
  classifier costs under 0.47 microseconds, so cached symbols or plumbing a
  prior classification through six operations is not release-worthy
  low-hanging fruit. The exact DSO identities, allocation result, method, and
  raw evidence paths are recorded in `benchmarks/README.md`.
- CHARSXP equality uses pointer identity first. Equal UTF-8, Latin-1, or bytes
  encodings may compare their stored bytes; native-encoded strings may do so
  only when both are ASCII. Mixed encodings and non-ASCII native strings must
  retain the translating UTF-8 comparison. Collection affixed-ID validation
  follows the same rule. These portable fast paths were measured; do not add a
  cached graph-validation mode, skip corrupt-state checks, or use raw bytes
  outside this boundary merely to accelerate collection reads.

Useful pre-build audits:

```sh
git diff --check
rg -n 'surface_auth|fallback sentinel|R_ClosureExpr|R_BindingIsActive' src R
rg -n 'UseMethod\("(domain_|condition_|tunetoken_|pslike_)|checkmate::|data\.table::' R src
rg -n 'PARADOX_CORE_(BASE|COLLECTION|SHADOW)' src
```

Some words such as “fallback” legitimately describe mathematical/default
choices. Review findings in context; the forbidden case is a second semantic
execution or mutable-surface authentication path.

## Information-efficient development verification

Verification should be trustworthy and proportional. During implementation:

1. parse changed R/tests and run `git diff --check`;
2. compile only changed C translation units with the strict C99 warning set;
3. install one stable source snapshot into one disposable library;
4. run all directly affected test files in one batch and fix a coherent batch
   of failures, not one failure per complete rerun;
5. run the full Paradox unit suite once after affected tests converge.

Do not run `R CMD check`, the entire consumer corpus, Valgrind/rchk, all
runtimes, documentation, and benchmarks in the inner loop. A changing source
invalidates those expensive results, and repeating them has low information
value. Do not build from the live root while another worker is compiling there;
copy package sources (excluding `.o`/`.so`) to a stable stage first.
At the release boundary, do not infer full-check success from process status:
the retained final `Status:` line must contain neither ERROR nor WARNING.
For an exact downstream head, first run repository-local `R CMD build` on its
authenticated Git archive and check the resulting retained package tarball,
never the raw source directory. The evidence must bind the build exit/log and
tarball path/hash separately from the check exit/log; this also preserves R's
required `Authors@R` metadata expansion before R 4.6 package checks.
Receipts for downstream checks must fingerprint every library on the actual
search path, including bridge and `mlr3verse` extra libraries, in order.
Keep diagnostic probes bounded as well: do not use recursive
`.Internal(inspect())` on R6/capsule graphs, because environments and shared
edges can produce unbounded traversal and output. Inspect exact attributes,
classes, payload fields, and identities explicitly instead.
In fail-closed Bash validators that enable `pipefail`, do not pipe a long
captured string from `printf` into `grep -q`: `grep` may exit after its match
and turn the producer's `SIGPIPE` into a nondeterministic false failure. Match
the captured value through a here-string or a regular retained file instead.

Use `scripts/environment/resource-jobs` before parallel work. Parallelize
independent test files, consumers, and runtime stages at the outer level while
keeping nested make/testthat/BLAS/OpenMP pools at one. Honor the reported
memory-aware ceiling. Never multiply every layer by the CPU count, and retain
enough RAM that the controlling Codex process cannot be OOM-killed.
`PARADOX_API_JOBS` and `PARADOX_BRIDGE_COMPILE_JOBS` are lowering-only release
knobs for the R-API matrix and downstream bridge compilation respectively; the
resource scheduler remains the upper bound. On this 32-CPU, no-swap host use 4
compile jobs and at most 2 independent R/test consumers unless a fresh resource
report requires less.
`scripts/memory-check` performs this admission itself for its serial heavy
modes: Valgrind receives one 16-GiB working-set allowance while at least 16 GiB
remains reserved for the host, and rchk receives its one 20-GiB analyzer
allowance with the same minimum reserve. The reports are release evidence;
do not bypass or hand-edit them. Do not impose an address-space limit on
Valgrind merely to mirror rchk: Valgrind's shadow mappings make virtual address
space a poor resident-memory/OOM estimate.
The retained `validate-native-source-run` authenticates the source-run copies
of the `mbo_config` and runtime-matrix Git/receipt helpers against their active
repository-root copies before executing them; do not resolve those
root-dependent helpers relative to a relocated validator. The memory harness
receipt hashes both native test workers as well as the runner and verifier.

Caching policy:

- toolchains, package downloads, installed dependency libraries, reference
  sources, analyzer runtimes, and content-addressed consumer installs are
  reused when their authenticated inputs match;
- package objects/installations may be reused only for identical source and
  compiler keys. During development only, an already compiled DSO may survive
  an R/docs-only change when every native build input (all `src/` bytes,
  headers, generated registration inputs, `NAMESPACE`, `DESCRIPTION`, compiler,
  flags, and platform) is byte-identical; reinstall the R/help databases and
  verify the loaded DSO hash. Record that identity check with the diagnostic;
- every frozen distributable payload receives a clean full source build in each
  executed profile. DSO component reuse is development evidence only and never
  satisfies a release, check, runtime, memory, downstream, or benchmark row;
  reuse across refs is conclusion transfer from a clean donor execution after
  sealed complete-payload identity, never reuse of development objects;
- each distinct frozen R/compiler/instrumentation profile builds/installs the
  distributable payload once from clean source, then shares that immutable
  installation across its tests and any gates that explicitly authenticate the
  identical profile and package bytes;
- a later ref may reuse a completed package-facing gate only when a sealed,
  independently replayed equivalence stage proves all changed Git paths are
  excluded by the exact `.Rbuildignore`, both clean `R CMD build` payloads have
  the same complete file inventory, and every payload byte is identical after
  removing only R's generated `Packaged:` record. The ledger must name both
  commits/trees, the normalized manifest, and each transferred gate. This does
  not transfer source-tree tooling, policy, documentation, downstream-profile,
  benchmark, memory, or portability conclusions whose own inputs changed;
- a failed broad run is mined for the complete failure set and logs before a
  rerun; rerun affected rows first, then one final broad confirmation.

Candidate downstream bridges are one build-once release artifact, not setup
performed independently by each consumer gate. After the candidate and the
priority-one dependency library have been authenticated, run
`compat/install-downstream-bridges --candidate-source "$candidate_source"
--evidence-profile release-refresh-20260720 --paradox-axis paradox2` once. It
installs the exact reviewed heads, in the fixed order bbotk, mlr3, miesmuschel,
mlr3pipelines, mlr3fselect, mlr3mbo, celecx, and mlr3fda, into the suffixed
`.local/compat/runs/$PARADOX_CANDIDATE_RUN_ID/library-downstream-bridges-release-refresh-20260720-paradox2`
and publishes it only after complete verification. The final overlay is
read-only;
its sealed evidence binds the candidate ref/commit/tree/content and provenance,
dependency-library content, reviewed ledgers and verifier, installer, exact Git
archives, installed versions, and installed package content. Completion schema
3 additionally binds the named profile and axis, validation-tooling commit and
tree, profile/axis registries and resolver, repository-evidence verifier, and
resource scheduler used by the build. Schema 2 and the unsuffixed seven-package
overlay describe only the historical default profile. Construction is
serialized by one candidate-run owner. The library
and its sealed evidence are published as separate atomic, no-clobber directory
renames; failed cleanup may remove only paths still matching both that owner
and the recorded filesystem device/inode. It must never delete or repair a
raced replacement. An existing or partial destination is never rebuilt in
place: use the helper's `--verify` mode, or use a new candidate run after
removing a failed unpublished stage.

The repository, documentation, and release-benchmark entrypoints must require
that exact overlay as their first extra library and invoke the read-only
verifier before loading a bridge package or doing retained work. They must not
silently install, repair, or substitute bridge packages. An entrypoint may use
`--protected-content-preverified` only after it has itself authenticated the
exact candidate and dependency-library content in the same operation.
The release benchmark additionally requires a named profile and the exact
`.local/compat/candidate-snapshots/<commit>` detached worktree. A stage's normal
verifier requires its overlay completion's tooling commit/tree and every
retained input to match that exact stage tooling. Reuse one sealed overlay when
those inputs remain identical. If a later reviewed downstream commit changes
tests only, build a new exact overlay and rerun that package's affected rows;
retain other conclusions only with an explicit production-byte/test-only delta
record. The `bf64490` to `9e87556` composition belongs only to the superseded
`10c6a0e` release history and is not active release evidence. There is no
arbitrary older-tooling replay or migration mode. Post-freeze infrastructure
paths may include `benchmarks/`, but package source, package tests, help, and
package-facing documentation remain forbidden changes.
Whenever this contract, helper, or its hooks change, run `bash -n`, `shellcheck`,
and `scripts/environment/test-downstream-bridge-installer`; the latter is the
cheap structural self-test and is not a substitute for constructing and
verifying the overlay once for the frozen candidate.

## Test contract

The package suite must directly cover, before downstream packages are used:

- exact capsule schema/version validation and corrupt-state no-crash behavior;
- BASE, nested/shared COLLECTION, and live SHADOW graphs, including cycles;
- collection add rejects existing/proposed cycles and corruption before commit
  and generation-checks both admitted graphs without rejecting shared DAGs;
- additive subclasses and deterministic rejection/non-support of core
  overrides or private replacement;
- all five Domain kinds, two Condition kinds, and unknown-kind rejection; the
  constructor and ObjectTuneToken boundaries exercise the same sole canonical
  built-in Domain-row admission owner. Object-token Domain coverage admits only
  bounded value-producing built-in Domains, rejects unbounded `ParamUty` and
  zero-level `ParamFct` tuning ranges, and demonstrates
  opaque-leaf identity through a bounded typed Domain rather than treating
  `ParamUty` itself as a tuning range;
  standalone Condition comparison also covers names, stable ALTREP operands,
  separate snapshots under reentry, and fail-closed class/attribute/type/S4
  cases; Domain coverage includes ordinary non-ALTREP/non-S4 structural shells,
  typed special-leaf ALTREP rejection, pointer-only typed S4 special/default/
  init matching, and opaque ParamUty S4 leaves whose special membership uses
  base `identical()` without dispatch;
- values, dependencies, transformations, constraints, TuneTokens, special
  values, presence modes, sanitization, required tags, named NULL, and errors;
  checked assignment stores Domain-valid dependency-inactive entries as dormant
  values, continues to validate dormant custom/type/bounds/token inputs, and
  preserves graph-wide atomicity. Raw `$values` exposes dormant entries while
  default `$get_values()` filters them and later parent changes reactivate them.
  Tests cover transitive chains, diamonds/conjunctions, explicit-value
  precedence over defaults, `NoDefault`, TuneToken edge skipping, collection
  cross-child state, Shadow visible/hidden state, serialization/clone/equality,
  and the intentional non-invariant that `$check(ps$values)` may fail while the
  store is legal;
  TuneToken coverage includes all five exact class/content shapes, scalar-name
  normalization, serialization, explicit `$search_space(values=)`, and
  fail-closed S4/subclass/extra-field/attribute/deep-or-cyclic-metadata cases;
  Object content covers bounded value-producing built-in Domain acceptance,
  unbounded `ParamUty` and zero-level `ParamFct` Domain rejection, exact BASE
  acceptance, COLLECTION/SHADOW/additive-
  subclass rejection, safe genuine-core shell aliases without method dispatch,
  rooted generation receipts through allocation/finalizers, final no-allocation
  commit reauthentication, and sealed single-use search capabilities;
  explicit search values cover ordinary non-ALTREP and representation-only S3
  named-list containers without dispatch, plus S4/semantic-attribute rejection;
  direct checked/unchecked `$values <-` rejects an outer ALTREP before
  observation and natively canonicalizes the accepted Paradox-1 empty
  spellings (`NULL`, an ordinary attribute-free zero-length atomic/expression
  vector, or an accepted empty list container), while only
  `set_values(.values=)` exercises the one-snapshot list-ALTREP exception;
  malformed
  exact-token/Domain structure raises while ordinary value mismatch remains a
  check diagnostic;
- `assert_values` shell-policy clone/serialization/equality behavior plus
  native tag/dependency/callback admission, ownership, and reentry conflicts;
- callback order, reentry, mutation snapshots, warning/error propagation, and
  no replay;
- strict native dependency-only checking across the graph, including
  ordinary-list admission, unknown IDs, TuneToken edges, first diagnostics,
  default-aware recursive activity, store-blind point semantics, and safe cycle
  errors for cycles admitted by the unchanged BASE mutation boundary. Check
  and presence modes remain point-strict: every supplied entry must be active,
  and a satisfying default can make an absent required child newly required;
- scalar/table constraint-only checking uses the native graph/point/constraint
  kernels, validates all table rows before callbacks, calls once per row from
  one callback snapshot, passes only the active subset of each point, and
  isolates reentrant mutation to later operations;
- live collection callback bindings and detached subset/flatten/Shadow-origin
  plans select capsule callbacks, enter the shared native evaluator family, and
  preserve the specified retained-then-changed order and omission behavior.
  Authoritative collection graph sites filter once in the translated
  collection namespace and give each child only active unprefixed child
  entries before invoking schema-free carriers;
- BASE-Shadow constraint plans merge hidden/visible values natively without S3
  dispatch after the authoritative Shadow graph site filters the complete
  origin configuration, preserve leaf identity, call once, and validate the
  scalar result;
- materialize-once stable/base ALTREP under allocation/finalizers/reentry, plus
  rejection or admission of hostile state-changing custom ALTREP without
  replay or Paradox-caused crash/memory corruption; interpreted ParamSet
  `params`, non-table trafo input/result, Domain cargo, internal table,
  dimnames, and list metadata shells reject ALTREP/S4 before semantic
  observation, while the six documented public-table ingresses share one
  strict classifier. Coverage includes canonical and additive class suffixes,
  no prefix-induced ordinary-shell copy, ALTREP-snapshot canonicalization
  without S3 dispatch, malformed/reversed/non-suffix/reserved/
  duplicate class rejection, allowed data.table cache-carrier shapes and cache
  disposal, missing/S4/attributed/mismatched row names, compact positive and
  negative counts, stable integer/character ALTREP row names with one Length
  and no Elt, shared mutable names under top-shell Elt reentry, and
  row-consuming versus non-row-consuming dimension checks. The public-table
  top shell is materialized once and admitted atomic leaves and columns remain
  supported;
- detached data.table facades, documented data.frame/data.table input including
  suffix-classified top-level ALTREP shells, additive presentation classes,
  and base R's lazy duplicate, stable
  semantic ALTREP columns, and public mutation isolation;
- detached ParamSet-family equality covers complete state and distinguishes
  shared from duplicated DAG topology without traversing private R6 bindings;
- serialization and both migration APIs against CRAN 1.0.1: pure single-object
  conversion; identity-preserving recursive upgrade through shared/nested/
  cyclic ordinary containers, closures, attributes, environments, active
  binding functions, non-forced promises, and current-core payloads; traversal
  boundaries and opaque weak/external pointers; full-preflight failure; retry
  after monotonic partial commit; default and opt-in first-use gateways; exact
  owner-registry admission/rejection; both pinned `mbo_config` fixtures; and
  authentic bbotk/miesmuschel/container snapshots;
- constructor, accessor, subset/flatten, design, sampler, and hot-path
  equivalence against maintained ordinary behavior.

If a consumer exposes a gap, add the smallest package regression that would
have caught it before fixing the consumer-facing issue.

## Downstream transition worktrees

Remote writes by an agentic process are forbidden. Agents may edit, test, and
commit in local downstream worktrees, but the user must push branches and open
or submit PRs manually.

The bbotk and miesmuschel heads below were the committed post-migration handoff
before dormant values reopened Paradox source; the remaining heads retain their
previously reviewed bridge changes. They are starting points, not final
replacement-candidate evidence. In particular, miesmuschel needs the focused
dual-version dormant-assignment expectation recorded in
`compat/downstream-pr-handoff.md`, followed by affected-row validation. Only
after that refresh does the user-performed remote publication handoff remain:

- bbotk `codex/public-paramsetcollection-sets` at `29f1806`: public collection
  state and rooted detached native search-space snapshots remain, with the
  authenticated exact-class additive `Codomain` inspector/rebuilder
  registration without restoring private ParamSet state access;
- miesmuschel `codex/paradox-paramsetshadow-bridge` at `2734db0`: extends the
  dual-version official `ParamSetShadow` bridge, public-state tests, and
  dual-major documentation link with the exact-class replacement
  inspector/rebuilder registration for serialized Paradox-1 Shadows and any
  owner-local cold gateways needed by old overrides. The bridge retires
  `params_unid` and `set_id`; deep comparisons remain independent of data.table
  secondary-index caches. Its Shadow dependency diagnostic gate remains
  because it covers the intentionally different official Shadow graph
  boundary, not ordinary built-in value admission. Separately, the legacy
  Paradox-1 Shadow's explicit pre-write assert remains strict, while the
  official Paradox-2 Shadow must test dormant storage and filtered
  reactivation. The unavoidable load-time
  namespace rebinding is restricted to the exported generator and eleven
  historical package-owned leanification targets; relocking is registered
  before the first unlock, and this bridge exception is not a public API;
- mlr3mbo `codex/paradox2-transformless-subset` at `1a1c0ab`: public
  transformation-free subset construction on Paradox 2 plus release notes;
- celecx `codex/paradox2-diagnostics` at `6da5102`: only the independent
  cycle/dependency bridge and compatible mlr3mbo requirement remain;
- mlr3pipelines `codex/paradox-diagnostic-compat` at `c85b2f4`: only the
  GraphLearner deep-clone ownership fix and mutation-isolation regression
  remain;
- mlr3fda `paradox2-snapshots` at `c1cdad5`: the Paradox-1 snapshot stays
  byte-identical while the four Paradox-2 headers name the current versioned
  `.__paradox2_ParamSet__values()` gateway; diagnostic bodies are unchanged;
- mlr3 `codex/paradox2-diagnostics` at `35e30a9` and mlr3fselect
  `codex/paradox2-diagnostics` at `ae8e1d1` are wholly redundant. Close those
  PRs without replacement; there is no cleanup commit to publish.

The historical exact Paradox-2 source-package check was
`.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1/repository-checks-release-refresh-20260720-paradox2`.
It has five rows, zero failures, and five final `Status: OK` results. Its
completion is independently verified; its completion TSV, results, manifest,
and seal-file SHA-256 values are
`a43064cd48b78fd433b4a4995a2da9cfad422da7b3b66f53391de05fbd289179`,
`f443e9ace27450057e4c8fcb6f9d93da85d595028abfff133272cc32f11a364c`,
`aa6d0ad380f0453db8c87888bdd5b7d18d54bc2698d21b9a4e67ce1040bcd731`,
and `7de4719e3fa0016fb05d804aa240dea34a0e5de76763c7f1d5d885e2e581acf1`.
The pre-dormant Paradox-1 five-package conclusion is an explicit composition.
The
`migration-release-final-p1-cdcc8e6-221c95e-r2` stage passed the exact final
bbotk, mlr3mbo, and celecx heads plus mlr3fda base `8f5a3df`; final mlr3fda
`c1cdad5` differs from that base only in four Paradox-2 snapshot headers, so it
does not alter the selected Paradox-1 tests or runtime source. The final
miesmuschel row passed in
`migration-release-final-p1-cdcc8e6-2771f5d-r3`. The r2 donor stage as a whole
is `completed_with_failures` because it exercised the superseded miesmuschel
head; never describe that whole stage or the mlr3fda final head as an exact
five-head Paradox-1 check. The historical broad Paradox-2 corpus used two
workers over 14 waves: 20 of 28 exact repositories passed, and the remaining
eight were reviewed non-Paradox or environmental exclusions. The replacement
source must rerun the applicable downstream waves rather than inherit that
conclusion.

Before the legacy object-graph migration reopened package and downstream
source, the six affected consumer selections passed the same 2,022
expectations with zero failures, errors, warnings, or skips on both Paradox
1.0.1.9000 and the then-focused Paradox 2 DSO. That historical evidence is
retained under
`.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`.
Those historical follow-up commits record only the then-tested cleanup trees.
The then-reopened migration payload has separate focused evidence under
`.local/checks/downstream-upgrader-latest-20260724/`: bbotk's exact Codomain
upgrader tests pass 8/8, miesmuschel's Shadow suite passes 62/62, and authentic
Paradox-1 fixtures pass every historical default/opt-in target plus explicit
migration, clone, and RDS-roundtrip checks. This is affected-bridge evidence,
not a transfer of the historical 2,022-per-axis full consumer conclusion.
Never push, open or close a remote PR, publish a tag, or otherwise alter remote
state yourself; the user performs every remote write. Exact commands and PR
text live in `compat/downstream-pr-handoff.md`.

The maintained priority consumers include bbotk, miesmuschel, mlr3mbo,
ConfigSpace, celecx, mlr3, mlr3tuning, mlr3pipelines, and active mlr-org book,
gallery, website, and serialized configuration workloads. Very old repositories
that do not use current Paradox are evidence inventory, not release blockers.

## Release convergence

The top-level verification coordinator is `scripts/verify`, with reviewed task
and profile data in `verification/tasks.json` and its normative operator/design
contract in `verification/README.md`. It schedules the existing retained gate
drivers; it does not reimplement their semantic verifiers. Run
`scripts/verify doctor` and `scripts/verify plan --profile focused` before a
long development run. `scripts/verify self-test` is the cheap, daemon-free
controller regression.

Hard parallel task execution has two supported forms. Per-worker
Podman/Docker containment requires proof of the requested memory, no-swap,
PID, and CPU cgroup ceilings plus an actual OOM-killed sacrificial worker.
Aggregate containment requires the controller and every local rootless Podman
payload to inherit one dedicated root-created system service with authenticated
finite memory/CPU/PID ceilings, no swap, systemd kill/accounting properties,
and stable cgroup/event state. Both probes execute as the eventual worker UID
and prove the read-only checkout plus nested writable-bind pattern; workers
disable SELinux labels explicitly rather than depending on host relabel
defaults. Never infer containment from accepted flags or an environment
marker.

This host's rootless Podman/cgroup-v1 setup cannot enforce per-worker limits.
After the manually reviewed root-owned launcher from `verification/systemd/`
is installed, ordinary `scripts/verify doctor|plan|run` commands enter its
aggregate service by default and can schedule independent coarse tasks in
parallel. Repository code never runs as root: systemd changes to the configured
UID/GID first. Aggregate Podman workers use `--cgroups=disabled
--cgroupns=host` and omit ineffective individual resource flags. Docker/remote
engines are ineligible because their daemon can escape the service.
`PARADOX_VERIFY_SYSTEMD_DEFAULT=off` is the machine-local escape hatch;
`--containment worker|aggregate` requires a specific mechanism.
`--best-effort` remains an explicit serial development-only
RLIMIT/RSS-watchdog fallback and cannot produce release evidence.
All shipped local tasks currently inherit the repository's Linux x86-64
toolchain constraint. The planner must reject another host explicitly; macOS
ARM64 and Windows x86-64 remain hosted-workflow evidence until a task supplies
a self-contained platform toolchain image and overrides that constraint.

The coordinator preserves at least 12 GiB/25% live memory and 8 GiB free disk
outside its work for Codex, the OS, and unrelated processes. It continuously
admits ready tasks by summed CPU, memory, PID, and scratch reservations;
tasks start at reviewed CPU/RAM minima and receive spare capacity up to their
reviewed ceilings in information order. Make receives only a task's actual
allocation while BLAS/OpenMP/testthat nested parallelism stays one. One
per-user machine execution lock prevents independent controllers—even from
different checkouts—from double-spending that aggregate budget. Cgroup-aware
live memory, disk, and PID availability are refreshed before new waves and at
a bounded cadence; transient outside pressure waits with a bounded timeout.
Static fit reserves the configured fraction of physical/parent-cgroup
capacity, while live admission recomputes that fraction from current
availability. Startup pressure therefore neither becomes a permanent ceiling
nor withholds a capacity-sized reserve on a busy large machine.
Under aggregate containment, task allocations are scheduler reservations
rather than individual cgroup limits. Admission uses the smaller of aggregate
headroom and global available memory outside the protected reserve, without
subtracting that reserve twice. `verify-task-entry` and `resource-jobs` must
cap nested work directly by the assigned CPU/RAM envelope; their independent
live-resource gate may still lower the result. Any aggregate memory/PID
event-counter increase or
cgroup/systemd/path/limit change is a fatal infrastructure failure that
terminates every sibling. A real payload proves worker inheritance at startup;
runtime monitoring then authenticates the controller cgroup and systemd unit.
The aggregate `TasksMax` is a host-protection boundary, not a per-worker PID
limit; complete saturation may defer engine cleanup until
`KillMode=control-group` tears down the transient service.
Permanent task incompatibility is decided against separate physical/cgroup
memory, filesystem, PID, CPU, platform, and architecture ceilings—not against
momentary free resources. A scheduler-created capacity/dependency/policy block
makes the profile incomplete but never masquerades as an executed failure or
cancels an otherwise independent branch, even under adaptive/fail-fast policy.
The checkout and toolchain are
read-only in workers; only reviewed task paths are writable, with task-private
temporary/runtime state and protected downstream libraries remounted
read-only. Retry HOME/tmp/runtime state is attempt-private. The default
adaptive policy completes independent peers in the
current phase, blocks failed descendants and later expensive phases, and
aborts globally only for fatal provenance, cache, containment, host pressure,
container-cleanup, or source-integrity failures.

The prepared worker is intentionally a minimal Linux userland, but its
reviewed command contract includes `rg`, GNU `timeout`, util-linux `setsid`,
and procps. Git operations against the read-only checkout must disable
optional locks, lazy fetching, prompts, filesystem monitors, and the untracked
cache; synthetic Git mutations belong in a scratch repository, never the real
`.git`. Runtime-matrix stages redirect their package library, temp, cache, and
runtime state to the attempt-private writable mount only after authenticating
the coordinator's prefix receipt handoff. Pinned source archives remain
read-only and are inspected below process-private temporary state. Runtime
prefix receipts force the C locale before enumerating and sorting members, and
the bootstrap's explicit-package lock/installed-inventory comparisons use the
same byte order. Their bytes and authentication result therefore cannot vary
with host or worker collation. Receipt verification writes its comparison only
below the caller's process-private `TMPDIR`, which must be a plain directory
outside the authenticated tree; retained manifests and their read-only parent
directories never need write access. The helper owns a private `077` umask, so
hostile caller settings cannot make its scratch files unreadable or leak their
contents.

The worker also maps ordinary activation's generic state and the runtime
matrix's mutable development library, temporary, cache, and runtime subtrees
to attempt-private directories. Prefixes, prefix/dependency receipts, and
sealed dependency libraries are deliberately not in that list and remain
read-only. This lets synthetic activation checks execute without granting
write access to retained compatibility evidence.

Child output and GNU-timeout diagnostics must use separate descriptors.
Deadline evidence accepts the exact full-path or basename diagnostic emitted
by supported GNU coreutils versions, and classifies status 124/137 as a
timeout only with the matching singleton TERM/KILL evidence; unknown,
duplicate, or impossible supervisor output fails closed. Plans bind and
reauthenticate the timeout, wrapper, shell, and worker bytes before launch and
after each batch. Reverse-worker process-group cleanup treats a group as
quiescent only when `kill -0` finds it and a successful procps snapshot proves
that every remaining member is a zombie. This permits completion under a
non-reaping container PID 1 without weakening cleanup for any executable
descendant.

Exact-key semantic cache hits below `.local/verify` accelerate development.
They are never release-evidence transfers: release profiles disable generic
result reuse/publication and retain the existing source-bound gate receipts.
Task identity includes reviewed OS/architecture, toolchain content,
hard-backend/cgroup generation, and immutable worker-image content. Kernel release, engine
version/path/storage, live capacity, and local aliases for that image belong
only to the per-invocation receipt after containment is freshly proved. The
non-release best-effort fallback remains keyed to its exact host/Python.
The activated repository R-library tree is always semantic, including for
release/prepared profiles that disable generic cache publication, because
coordinator resume can still reuse their completed rows.
Changed-file impact rules affect development selection and priority only.
Downstream runs consume one authenticated candidate-context JSON below
`.local`; never hand-template divergent ref/commit/tree/source/library/content
values across tasks. Every retried coarse gate gets an attempt-specific child
run ID. Execution results are immutable per attempt, every coordinator resume
adds a current host/engine invocation receipt, and a retained success is
accepted only when its latest view, immutable attempt, exact log, and original
invocation receipt authenticate one another. Tasks opting into resume
revalidation invalidate their complete descendant closure as well. Only
genuine execution failures—not capacity/dependency blocks—raise future
scheduling priority. A
compatibility gate whose public stage is candidate-wide uses that ID
for private work and verifies an already-published stage on resume. A release
`source_ref` must resolve to the exact clean HEAD commit/tree used by every
other release-foundation gate.

Prepared compatibility has four explicit profiles. `prepared-downstream`
remains axis-neutral. `prepared-reverse` and `prepared-documentation` run the
real Paradox-2-only gates, while `prepared-release-compat` runs downstream,
priority-zero/one reverse dependencies, and all-scope documentation as one
keep-going DAG. The real gates source the authenticated
`scripts/activate-compat-system` layer inside their workers; ordinary native,
API, runtime, and differential tasks must not inherit it. Reverse execution
uses a stable run ID and an explicit controller-owned attempt number: attempt
one requires a fresh reservation, while a retry resumes only a
candidate/options/harness-bound initialized marker. The completed reverse task
is revalidated rather than blindly trusted on a later coordinator resume.
Documentation uses an
attempt-specific ID because it has no resume mode. Both receive a narrowly
reserved exact writable output directory rather than a writable historical
evidence parent. Candidate contexts keep consumer extras separate from
documentation-only libraries so the latter cannot alter repository dependency
resolution; reverse-only profiles neither require nor authenticate those
unused layers. All documentation-essential rows run before any advisory full
render, and its task reserves one CPU because the retained driver is serial.
Their additional compatibility-system, TinyTeX, and documentation contracts
remain Linux x86-64 as well.

The coordinator self-test is the routine harness-change gate. It must cover
strict container exited-state/exit agreement, fail-closed stale cleanup,
attempt/interrupt cleanup, adaptive minima/ceilings, aggregate PID admission,
cgroup-aware live memory, optional future package inputs, locking, cache
tamper rejection, immutable attempt/invocation receipts, static-versus-live
capacity, aggregate v1/v2 and systemd proof, non-duplicated reserve
accounting, event-counter failure, actual parallel aggregate workers, platform
constraints, reserved-output recovery, and source reauthentication. Harness
work alone does not
authorize launching the multi-hour real R, reverse, documentation, or memory
suites.

`release-core` intentionally excludes the combined memory driver. The current
`scripts/memory-check --mode all` launches pinned rchk Podman from inside the
driver and applies its own 16-GiB reserve; putting it in the generic worker
would require unsupported nested Podman and duplicate the reserve. Run that
existing source-bound gate directly after the release foundation, using the
`native-release` task result's `child_run_id`. A future integration must make
the rchk image the outer worker and must not claim nominal containment before
that split is implemented and tested.

Only freeze a candidate after code, contract tests, docs, downstream bridges,
and profiling converge. The active status and exact evidence IDs belong in
`design/release-2.0.0.md`; never encode stale pass counts in scripts as a proxy
for test discovery.

Run release gates against one clean immutable full ref, broadly in this order:

1. strict GCC/Clang C99, registered-routine/probe audit, ASan/UBSan, complete
   package suite, and clean `R CMD check --as-cran`;
2. actual R 3.6.3, 4.0.5, 4.3.3, 4.5.2, and development R plus compilation
   against all seven pinned R 3.6.0--4.6.1 header axes; the exact
   raw-attribute/hot-closure-formals/stored-binding/promise exception ledger,
   raw-token/version-gated DSO audit, and option-access symbol policy; the
   authenticated R 3.6 complete-test closure and separate exact declared-floor
   smoke with `R_DEFAULT_PACKAGES` isolated; and the full-only sealed
   R 4.0.5-to-R 3.6.3 serialization handoff;
3. upstream differential with reviewed intentional Paradox-2 deltas;
4. all exact reviewed downstream bridge heads, then priority-zero/one reverse
   and GitHub consumers and documentation workloads;
5. GCT, instrumented-R Valgrind, bounded rchk, adversarial corruption, and
   direct coverage of every registered routine/hazard family;
6. examples, vignettes, manuals, pkgdown/book/gallery/website and legacy
   upgrade workloads;
7. current Windows x86-64, exact Windows x86-64 R 3.6.3/Rtools35, and real
   macOS Apple-silicon ARM64 CI for the exact ref;
8. paired release benchmarks on an otherwise idle host.

Primary retained drivers include `scripts/native-check`,
`scripts/check-r-api-compatibility`, `scripts/test-runtime-matrix`,
`scripts/memory-check`, the `compat/` runners/verifiers, and
`benchmarks/release`. Read their `--help` before use; do not copy historical
run IDs or expected counts. Expensive analyzer and consumer gates run only
after cheaper package/bridge gates are green.

Never accept a green GitHub matrix label as portability evidence by itself.
Each platform row must reject a nonzero `rcmdcheck` child status and require one
sole final `Status: OK`, except for the separate R 3.6.3/Rtools35 row. R 3.6's
`_R_CHECK_DEPENDS_ONLY_` does not suppress its dependency-inventory NOTE, so
that runtime-import-only row instead requires child exit zero, exactly one
missing-Suggests NOTE naming the complete expected nine-package set, no other
NOTE/WARNING/ERROR/halt, and one sole final `Status: 1 NOTE`. Current Windows
and macOS retain exact `Status: OK`. An always-run completion job must then
reject the complete matrix aggregate unless it is exactly `success`. The offline verifier
independently requires four successful REST jobs (macOS ARM64, current Windows
x86-64, exact R 3.6.3/Rtools35 Windows x86-64, and completion), all three exact
platform artifacts, their check logs, and frozen candidate provenance. A
release-only direct-child companion changes only the workflow to pin and check
out the immutable candidate and to reduce the ordinary matrix; it must retain
the separate old-Windows job and both completion layers.
Create that workflow with
`scripts/environment/render-portability-release-workflow.R`, never by a
one-job hand edit: the renderer pins both checkouts credential-free, inserts
both exact commit assertions, and runs the release structural validator before
publishing an absent output path.

Profile representative constructor, `check`/`check_dt`/`check_dependencies`,
`has_deps`, values, domains/params/dependencies, subset/collection, live Shadow
constraint and read/write paths, design, and sampler workloads. Optimize only
measured hot paths, retain portable scalar code unless a portable
architecture-neutral improvement is proven, and rerun affected correctness
tests after every optimization. Freeze performance changes before the final
memory/portability matrix.

The Paradox-1 comparison has exactly seven narrowly ledgered integrity-read
rows. `shadow_values_live` uses `integrity-shadow-read` (3.25 median/3.50 q75).
The three synthetic `collection_values_{plain,rich,nested}` rows and the real
`$values` rows for `mies_mutator_maybe`, `mies_optimizer`, and
`mlr3pipelines_graph` use `integrity-collection-read` (2.75/3.00). They perform
generation/signature or complete capsule-DAG admission that the cached legacy
surface did not. The exception is timing-only: both tiers keep the `hot` 1.25
ratio and 16-KiB minimum allocation thresholds. Consumer `$params`,
`get_values_unchecked`, filtered getters, mutation/constraint/domain paths, and
all other real consumer operations keep their ordinary `hot`/`standard` tiers.
Never widen a global tier or add another integrity row without retained profile
evidence and explicit design review. Treat non-pass integrity rows as required
raw-distribution review, and normally retire these contract-reset tiers once
Paradox 2 is the authenticated baseline.

## Last sealed candidate (historical after final cleanup)

The last sealed package candidate is
`refs/paradox-release/candidate-20260724T105215Z`, commit
`8797f1163fe612cb01d1facf517834d3f516a697`, tree
`81e6f901266754b97a0906f88a472bf04795f13c`, candidate content SHA-256
`ced2390bc756b01805e7bdcf32fdb4a6ff2c1bd010dd3d576194d939e491f644`.
It contains the informative native diagnostics, recursive legacy object-graph
migration, authenticated native gateway contexts and generation barriers, the
indexed-root `schedule_vector()` repair, and the self-returning ALTREP duplicate
regression.
`AGENTS.md`, `design/`, and `environment/` are excluded from the package build,
and the exact candidate-to-`fc92edd` diff contained no package-facing source,
so that post-freeze validation and release-ledger work did not alter the sealed
source. The later 2026-07-25 cleanup does alter package-facing source and
invalidates this transfer. This was package-facing-source identity, not an
unproved complete package-payload byte identity. Do not infer it for any other
path: authenticate the exact diff before transferring a source-bound result.

The exact candidate's bcheck/maacheck/fficheck reports and refreshed policy are
recorded in the Authority section above. The complete package suite, CRAN-style
and depends-only checks, strict compilers, analyzers, sanitizers, R API/header
matrix, real R 4.3.3/4.5.2 runtime matrix, retained combined memory run,
normalized differential, both downstream axes, 20-of-28 scoped broad corpus,
mandatory documentation, five-package exact Paradox-2 source check, and sealed
77-row benchmark were green or accepted under their recorded reviewed
exclusion policies for that payload. They are historical development evidence
for the cleanup source, not completed release gates. A replacement candidate
needs the applicable local and hosted gates plus the user-performed downstream
branch/PR, tag, and workflow publication handoff.

## Historical candidates

The rejected compatibility-first candidate ref
`refs/paradox-release/candidate-20260717T083921Z` at
`2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa` was once green under a different
contract. The superseded contract-first candidate
`refs/paradox-release/candidate-20260719T104709Z` at
`5e40d2ba9b9ce3a75615b90420fb9bc298c19ecf` and its stale portability companion
`refs/paradox-release/portability-harness-268ccff` at
`268ccff27ee68bfea71c6370b0616a9c969a94cf` predate the final subset,
dependency, downstream, and `$has_deps` work. Their evidence remains below
ignored `.local/` paths and in Git history, but none is a baseline for current
source completeness, compatibility policy, routine inventory, test counts, or
release readiness.

The serialized-migration candidate
`refs/paradox-release/candidate-20260724T011004Z` at `348539ad` was superseded
after adversarial review tightened native shell authentication, read-only
Shadow receipts, joint graph validation, and postorder commit barriers.
Candidate `a362365` contained that hardening but failed the strict enum
conversion build; `2bcce2b` fixed the compiler warning but retained a
cppcheck-only conservative null-flow diagnostic. The later `8797f11`
candidate makes the admitted graph guard explicit to the analyzer. None of
these rejected refs, nor the now-historical `8797f11` gates, is release
evidence for the reopened cleanup payload.

The later candidate `refs/paradox-release/candidate-20260719T150831Z` at
`612345ceb403c70a0ea6c1149c367c6782d9870b` passed its native, R-API,
runtime, differential, and memory gates, but the final release benchmark
correctly rejected it before candidate timing: R 4.6 had represented the
benchmark's ordinary wide data.frame as a top-level base `wrap_list` ALTREP,
and `check_dt()` rejected that common representation. Its unsealed benchmark
is defect evidence, not a performance result. The replacement design
materializes admitted public-table ALTREP shells once at the six documented
ingresses and does not weaken general structural admission.

The replacement candidate
`refs/paradox-release/candidate-20260719T175053Z` at
`4f28327f894fe17324410a45cabaf7221e6eca45` passed its exact-byte native,
R-API, runtime, differential, and downstream-bridge gates. Its memory run was
stopped and left unsealed when adversarial review found that the shared table
boundary still admitted missing, S4, or dimension-mismatched `row.names` and
could retain a shared mutable names vector across a hostile top-shell Elt
callback using `data.table::setnames()`. The candidate is superseded. None of
its package-byte evidence transfers to a replacement candidate, including the
completed gates; the partial memory directory is diagnostic evidence only.

The subsequent candidate
`refs/paradox-release/candidate-20260719T194741Z` at
`60704fcc6a899c508f5adffbec35bb61723d3704` reached package installation and
downstream-bridge construction, but its exact R-API gate rejected two direct
assignments of `Rf_isObject()` to `int` under the pinned R 4.3 and 4.4 headers:
those headers expose an `Rboolean` return type and strict Clang correctly
reported the implicit signedness conversion. The in-flight native and runtime
runs were stopped immediately. Their partial directories, the installed
candidate, and the bridge overlay are diagnostic only and transfer no release
conclusion. The replacement normalizes both predicate results explicitly and
must rerun every package-byte-bound gate.

The next candidate
`refs/paradox-release/candidate-20260719T200549Z` at
`70c6d728785464c98ffc8c658f20c1937467a593` passed the exact old-header API
gate and installed the candidate and bridge overlay. Its R 4.3 runtime stage
then exposed a test-fixture portability error: unlike R 4.6, R 4.3 did not
choose a top-level base `wrap_list` ALTREP for the wide data.table after an
attribute-only duplicate, so an internal materializer test incorrectly
expected ALTREP-only cache removal from an unchanged ordinary table. This is
not grounds for copying every ordinary table at a hot ingress. The native and
runtime runs were stopped; none of this candidate's package-byte evidence
transfers. Tests that require a top-level ALTREP must use the registered native
fixture, while the base wrapper remains realistic conditional coverage when a
runtime selects that optimization.

Candidate `refs/paradox-release/candidate-20260719T202524Z`, commit
`e3741ab1d3cb8a5f3e7f357af6b7ab90c6e50fb7`, passed its exact native, R-API,
and runtime gates, then failed the maintained repository sweep in
mlr3fselect. `mlr3::BenchmarkResult$aggregate()` produces the ordinary
additive class vector `c("bmr_aggregate", "data.table", "data.frame")`, which
data.table preserves through a narrow subset before bbotk calls
`ParamSet$assert_dt()`. The exact-class classifier rejected that
non-dispatching representation even though Paradox 1 accepted it. The
replacement contract admits well-formed additive leading classes without
dispatching them, and canonicalizes them only while taking an already-required
ALTREP snapshot. All evidence bound to `e3741ab` is superseded.

Candidate `refs/paradox-release/candidate-20260720T022410Z`, commit
`a4617ca769ff5373a7da16c7ce333e36c68fd9b2`, fixed that public-table boundary
and passed its exact R-API, native, runtime, differential, focused downstream,
and bounded performance gates. Gctorture and all four Valgrind diagnostic
inventories were also clean. Its combined memory run remained unsealed because
the source-bound rchk policy still named the pre-classifier bcheck/fficheck
hashes and 69-routine count. The actual bounded report retained the same 77
reviewed Function blocks, 196 UP diagnostics, and 13 PB diagnostics, while the
classifier's three diagnostic registrations raised the routine count to 72.
The refreshed policy binds that exact 782-function/28,140-state report. The
next candidate incorporates the policy and final validation tooling without
changing any package-facing file; do not promote the partial `a4617ca` memory
directory or its earlier tooling-bound overlays as final evidence.

The replacement is frozen at
`refs/paradox-release/candidate-20260720T053518Z`, commit
`10c6a0e65910206c8face91dac6c3dd1115e0bed`, tree
`a205205194f0bc62114106504853721f678fa340`. The immediate child validation
commit changes only the Paradox evidence-axis registry, its exact fixture, and
this ledger; all candidate execution must continue to authenticate the managed
detached `10c6a0e` source rather than the validation worktree. Evidence bound
to earlier candidate commits or tooling identities remains historical except
for the exact, explicitly scoped byte-identity proof below.

The sealed equivalence stage
`.local/checks/package-equivalence-a461-10c6` is the narrow exception. It
authenticates 486 tracked files per ref, 467 identical blobs, and 19 changed
paths, all excluded by the candidate's exact `.Rbuildignore`. Clean builds have
217 payload files each; the only raw difference is R's generated `Packaged:`
line in `DESCRIPTION`. Their normalized manifest is
`e6e767b8fa3cd1a9273d62039c208d7c3ae1aea0b12d7446560297d4750dcc3e`
and the retained evidence manifest is
`e558864a318a465edf058013f743c6ae386b34bcd7894037a02c66938a986fb3`.
Consequently the exact `a4617ca` R-API, full native/sanitizer/test/check, R 4.3.3
and 4.5.2 runtime, and focused-consumer conclusions apply to the identical
installed/package-facing `10c6a0e` payload. This does not promote the unsealed
`a4617ca` memory directory or any older validation-tooling overlay. Exact
`10c6a0e` source-static, memory, differential, documentation, downstream, and
benchmark stages were retained separately.

The first full-check stage under tooling `3f02899` correctly built source
tarballs before checking them, but incorrectly passed `--no-build-vignettes`
to `R CMD build`. Three changed heads passed; celecx's source contains a
vignette and therefore produced two missing-`inst/doc` warnings even though its
tests and vignette code passed. Full-check subjects now use ordinary serial
`R CMD build --no-manual` so built vignettes are present, followed by
`R CMD check --no-manual --no-build-vignettes` to inspect rather than rebuild
them. The sealed failed stage remains diagnostic. A fresh run and overlay after
this harness-only repair passed all four selected source-package checks; no
earlier result was relabeled.

The first all-scope documentation execution under tooling `4d4b637` completed
all 17 workloads with every mandatory row passing, but its process correctly
failed instead of overwriting `metadata/evidence-manifest.tsv`. The harness had
retained the downstream overlay's own manifest and seal under those reserved
basenames before trying to create the documentation stage's manifest and seal.
Retain those four overlay inputs under explicit `downstream-bridge-*` names;
reserve `evidence-manifest.tsv` and `completion.seal` solely for the enclosing
stage. The completed-but-unsealed directory is diagnostic only. The final
documentation stage under tooling `bf64490` was rebuilt and sealed without
changing candidate bytes; all 17 workloads completed and all mandatory rows
passed.

The first Paradox-1 release-refresh execution found one miesmuschel-only test
failure in both direct tests and `R CMD check`: three deep `expect_equal()`
assertions compared R6 ParamSets through private data.table secondary-index
caches. The public state and all 3,041 other expectations passed, and the same
objects differed only by `index` attributes. Miesmuschel head `d4c7f797` uses
semantic equivalence for those three assertions; focused 216-expectation runs
pass on both Paradox majors. This changes only downstream tests, not
miesmuschel production or Paradox candidate bytes. The named overlays were
rebuilt to bind the final reviewed head, the affected miesmuschel rows were
rerun on both axes, and the sealed non-miesmuschel, documentation, and benchmark
conclusions retain their original identities with this exact test-only scope.

That affected-row confirmation is complete. The final Paradox-2 overlay/test/
check evidence is under
`.local/compat/runs/release-final-20260720T053518Z-10c6a0e-r8`; the final
Paradox-1 evidence is under
`.local/compat/runs/release-final-20260720-v1.0.1-paradox1-r2`. Miesmuschel
`d4c7f79750cd15c8174415fb0ba059c597f4f055` passes its full repository suite
and source-package check on both axes. The three other refreshed heads already
passed on each axis, and the final miesmuschel delta changes only three test
comparison calls, so their sealed conclusions transfer without rerunning them.
The final documentation stage has 17 completed workloads with every mandatory
row green; the release benchmark has 72 passes, five bounded marginal reviews,
and no failure. Gctorture, Valgrind, bounded rchk, exact differential, strict
static source checks, and their independent evidence replays are green for the
frozen candidate.

The final portability candidate tag is `paradox-2.0.0-ci-10c6a0e`. Its
direct-child harness is
`paradox-2.0.0-ci-10c6a0e-harness-cc06c18` at
`cc06c182949af09ce80e335ddbfc63a8078692e6`; it changes only
`.github/workflows/r-cmd-check.yml`, whose SHA-256 is
`e6bf11775a59c783a5cbd164ea729a9877a6752edfc96adc0491723b7bf05bfc`.
The local regression/adversarial fixture, `actionlint`, ancestry/diff checks,
and offline evidence-verifier fixture pass. The user must publish those two
tags and dispatch the workflow; Windows x86-64 and macOS ARM64 remain open until
that remote run and its retained artifacts pass independent verification.
