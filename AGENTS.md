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

Old candidate commits, refs, logs, and artifacts are historical evidence for
different bytes only. Git history retains the former long design narratives;
do not copy their R6-surface authentication, S3 fallback, sentinel replay, or
pre-data.table-1.18 decisions back into current source.

Post-freeze downstream-only validation is the one narrow exception to keeping
all release tooling byte-identical to the package candidate. It uses a named,
tracked profile from `compat/downstream-evidence-profiles.tsv` and one clean
infrastructure commit whose diff against the candidate is empty for package
source, tests, help, and package-facing documentation. The dependency receipt
is deliberately profile-specific and axis-neutral: it prepares only the
unchanged external dependency closure and is isolated by the candidate run ID.
Overlay, lock, repository-test, full-check, and completion identities include
both the profile and the exact `paradox2`/`paradox1` axis. Each axis registry
row pins the complete candidate ref/commit/tree/version tuple, not merely its
major version. Never edit/relabel the default evidence, pass an arbitrary
manifest path, or run Paradox-1 compatibility in a Paradox-2 candidate stage.
The active release refresh is
`release-refresh-20260720`; only miesmuschel, mlr3mbo, celecx, and mlr3fda
repository rows are rerun, with one worker, while the overlay retains all
eight reviewed support packages in dependency order. Its refreshed heads live
in a separate authenticated primary-checkout namespace; dependency preparation
continues to use the unchanged default heads, because refreshed mlr3mbo is a
bridge package in the ordered overlay rather than a shared-library dependency.
The sealed release benchmark is part of that post-freeze validation tooling,
not part of the package candidate. It must take the managed detached candidate
worktree plus explicit profile and axis, derive the suffixed overlay/evidence,
and record the separate candidate ref/commit/tree and current clean tooling
commit/tree/status identities. Freeze the final validation-tooling commit first,
construct one fresh named overlay with that exact tooling, and reuse it read-only
for documentation, full checks, and the benchmark. Never require tooling
`HEAD == candidate`, accept a caller-selected bridge path, or rebuild the default
unsuffixed overlay merely to run the final benchmark.

The structural boundary below is an intentional Paradox-2 break made in this
release, not a migration shim to relax later. Supporting exotic structural
ALTREP/S4 shells or parallel R/native admission would preserve no known
maintained use while retaining duplicate authority, dispatch, and
multi-observation hazards. Ordinary documented containers and stable ALTREP
semantic vectors remain supported at their stated positions.

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
  accessors return detached, valid data.table facades. Attribute values,
  classes, names, and row names are contractual by attribute name; incidental
  pairlist order inherited from an R/data.table version is not. Native grid
  facades use one fixed `row.names`/`class`/`names` construction order on every
  supported runtime.
- Mutations build and validate replacement capsules and swap `.core`
  atomically. Value assignment plans the complete BASE/COLLECTION/SHADOW
  graph through ultimate BASE targets, deduplicates shared targets with
  deterministic last-owner semantics, and commits every replacement in one
  allocation- and callback-free wave. A callback mutation of any planned
  target wins and makes the outer assignment error before any target is
  changed. Native readers retain the capsule chosen at operation entry.
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
  validates and snapshots both inputs, merges hidden values before visible
  values without `c()`/S3 dispatch, preserves opaque leaf identity, executes
  the callback once, and requires one non-missing logical result.
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
  not accept ALTREP; it validates only the already admitted snapshot.
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
  mutation rules.
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
  extra-transformation merging keeps retained/untransformed inputs in input
  order, then appends all changed child outputs in callback-plan order (and in
  each callback's result order). Child-owned inputs omitted by their callback
  disappear, and a changed name that collides with retained input is an error.
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
  TuneToken; and returns `TRUE` or the first diagnostic. Do not restore the R
  data.table/pmap traversal or newline-collapsed multi-error result.
- `ParamSet$test_constraint()` and `$test_constraint_dt()` reuse the native
  check graph, point admission, and constraint kernel; there is no scalar or
  per-row R constraint engine. With `assert_value = TRUE`, the table method
  validates every row before running any constraint callback, then calls the
  snapshotted callback set once per row in order. Reentrant callback mutation
  is visible only to the next public operation. The table boundary continues
  to require a data.table.
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
  selects another engine. Use `SamplerHierarchical` for custom 1-D samplers.
  Fixed values and dependency masking remain the single `Design$new()` boundary.
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
  shells. Raw attribute selection uses `R_mapAttrib()` on R >= 4.6 and the
  established `ATTRIB` traversal backport on R 4.3--4.5. Neither path invokes R
  or data.table fallback logic. Direct checked or unchecked `$values <-`
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
- Legacy Paradox objects require explicit `upgrade_paradox_object()`. The
  upgrader is non-mutating, callback-free while inspecting, preserves valid DAG
  identity, accepts current graphs idempotently after full validation, and
  rejects cycles, unknown extensions, core overrides, and malformed state.
- Shipped C is portable C17 and otherwise uses public R C APIs available in
  R >= 4.3. There is exactly one centralized compatibility exception:
  `src/r_api_compat.c` calls the declared/exported `Rf_findVarInFrame` only when
  compiling for R < 4.6, and rejects a returned `PROMSXP`; R >= 4.6 uses the
  documented experimental API `R_GetBindingType`. R 4.3--4.5 exposes no public
  non-forcing binding classifier, and an R-level `substitute()` workaround is
  forcing/unsound for
  the simultaneous generation and TuneToken receipt scans that require this
  helper. The exception is ledgered in `environment/r-api-exceptions.tsv`, raw-
  token-audited to exactly one source occurrence/path, and tested against pinned
  headers and real runtimes. It is not CRAN-allowlisted for the supported
  pre-4.6 build path: the older runtime DSOs must contain the symbol, while
  every current-R (R >= 4.6) DSO
  audit must prove that it is absent. It is no permission for any other internal
  R API. Treat R API predicates as predicates rather than assuming a stable
  integer typedef: when storing their result, normalize it with an explicit
  comparison such as `predicate(...) != FALSE`. The pinned old-header compiler
  matrix is authoritative for signedness and declaration drift. Linux, Windows x86-64, and Apple
  ARM64 remain first-class targets. Corrupt/forged state must error and must
  never cause an out-of-bounds access, stale pointer, double evaluation, or
  segfault.

Do not leave obsolete compatibility code merely unreachable. Before release,
all semantic translation units must be free of generated-closure/body
authentication and sentinel-to-R replay. Temporary migration adapters must be
marked, have no alternate semantics, and be deleted before the candidate ref.

## Repository-local environment

The host R 3.6.3 and host/user libraries are out of scope and must not be
modified. Never use `sudo`, edit shell startup files, or install into HOME,
`/usr`, or a system R library.

Provision once, then activate from the repository root:

```sh
scripts/bootstrap
. scripts/activate
```

Activation selects the pinned local R 4.6.1/C17 toolchain and
`.local/R/library`, clears inherited compiler/library variables, and redirects
temporary and cache state below the repository. Confirm retained work with:

```sh
test "$PARADOX_ACTIVE_ROOT" = "$(pwd -P)"
test "$(command -v R)" = "$PARADOX_ROOT/.local/toolchain/bin/R"
test "$(command -v Rscript)" = "$PARADOX_ROOT/.local/toolchain/bin/Rscript"
test "$(R RHOME)" = "$PARADOX_ROOT/.local/toolchain/lib/R"
```

Authoritative inputs are:

- `environment/toolchain-linux-64.lock`: local development toolchain;
- `environment/r-packages-linux-64.lock`: exact source-package closure;
- `environment/runtime-r-4.3.3-linux-64.lock` and
  `environment/runtime-r-4.5.2-linux-64.lock`: supported-runtime prefixes;
- `environment/r-api-sources.tsv`: local reference R sources/manuals;
- `environment/r-api-exceptions.tsv`: the sole reviewed R C API exception;
- `environment/valgrind-r-packages.tsv`: instrumented-R package closure.

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
. scripts/activate-runtime-matrix 4.3.3   # or 4.5.2
. scripts/activate                        # return to R 4.6.1
```

`scripts/test-runtime-matrix` validates both the deliberately empty pre-R-4.6
exclusion policy and the reviewed result-skip manifest against the exact
extracted candidate's current `skip_on_cran` test titles before resource
admission or any runtime build/install worker starts. Keep that source-derived
policy preflight shared with the old-runtime test runner. The coordinator also
stages and authenticates the mandatory `mbo_config` upgrade inputs before that
same boundary, so the fixture test must execute and may not become an
unreviewed environment-dependent skip. Malformed or stale input therefore
fails cheaply instead of after two package installations. In
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
  self-reference, and ensure every mutable column shell is detached from the
  capsule. Never synthesize private indices or call private C APIs.
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
2. compile only changed C translation units with the strict C17 warning set;
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
- a frozen release candidate always receives a clean full source build. DSO
  component reuse is development evidence only and never satisfies a release,
  check, runtime, memory, downstream, or benchmark row;
- each distinct frozen R/compiler/instrumentation profile builds/installs the
  candidate once from clean source, then shares that immutable installation
  across its tests and any gates that explicitly authenticate the identical
  profile and candidate bytes;
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
`.local/compat/candidate-snapshots/<commit>` detached worktree. Final validation
tooling must be frozen before one fresh named profile overlay is built. The
normal verifier requires the overlay completion's tooling commit/tree and every
retained input to match that exact current tooling, so the same sealed overlay
can then be reused read-only for documentation, full checks, and the benchmark.
There is no older-tooling replay or migration mode. Post-freeze infrastructure
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
  ordinary-list admission, unknown IDs, TuneToken edges, and first diagnostics;
- scalar/table constraint-only checking uses the native graph/point/constraint
  kernels, validates all table rows before callbacks, calls once per row from
  one callback snapshot, and isolates reentrant mutation to later operations;
- live collection callback bindings and detached subset/flatten/Shadow-origin
  plans select capsule callbacks, enter the shared native evaluator family, and
  preserve the specified retained-then-changed order and omission behavior;
- BASE-Shadow constraint plans merge hidden/visible values natively without S3
  dispatch, preserve leaf identity, call once, and validate the scalar result;
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
- serialization and explicit upgrades of CRAN 1.0.1, shared/nested graphs,
  both pinned `mbo_config` fixtures, and rejected legacy extensions;
- constructor, accessor, subset/flatten, design, sampler, and hot-path
  equivalence against maintained ordinary behavior.

If a consumer exposes a gap, add the smallest package regression that would
have caught it before fixing the consumer-facing issue.

## Downstream transition worktrees

Remote writes by an agentic process are forbidden. Agents may edit, test, and
commit in local downstream worktrees, but the user must push branches and open
or submit PRs manually.

Current PR-ready local branches are:

- bbotk `codex/public-paramsetcollection-sets` at `6cae955`: public collection
  state, dual-version diagnostics, and rooted detached native search-space
  snapshots;
- miesmuschel `codex/paradox-paramsetshadow-bridge` at `6255050`: the
  dual-version official `ParamSetShadow` bridge, public-state tests, and
  dual-major documentation link;
- mlr3mbo `codex/paradox2-transformless-subset` at `1a1c0ab`: public
  transformation-free subset construction on Paradox 2 plus release notes;
- celecx `codex/paradox2-diagnostics` at `a297555`, mlr3
  `codex/paradox2-diagnostics` at `35e30a9`, and mlr3fselect
  `codex/paradox2-diagnostics` at `ae8e1d1`: small dual-version test-diagnostic
  adaptations with unchanged runtime behavior;
- mlr3pipelines `codex/paradox-diagnostic-compat` at `1c4bc6e`: exact-error
  decoupling plus an independently required GraphLearner deep-clone ownership
  fix and mutation-isolation regression;
- mlr3fda `paradox2-snapshots` at `035da5b`: Paradox-2 diagnostic snapshots
  selected without changing the Paradox-1 snapshot baseline.

Before handoff, rebase only if the user requests it, test each exact branch
against the exact frozen candidate, record the commands/results, and provide
the explicit `git -C ... push <remote> <branch>` commands plus PR title/body
text for the user. Never push, open a remote PR, publish a tag, or alter remote
state yourself. The
reviewed exact heads, proposed titles/bodies, and manual commands live in
`compat/downstream-pr-handoff.md`; update that file if any branch changes.

The maintained priority consumers include bbotk, miesmuschel, mlr3mbo,
ConfigSpace, celecx, mlr3, mlr3tuning, mlr3pipelines, and active mlr-org book,
gallery, website, and serialized configuration workloads. Very old repositories
that do not use current Paradox are evidence inventory, not release blockers.

## Release convergence

Only freeze a candidate after code, contract tests, docs, downstream bridges,
and profiling converge. The active status and exact evidence IDs belong in
`design/release-2.0.0.md`; never encode stale pass counts in scripts as a proxy
for test discovery.

Run release gates against one clean immutable full ref, broadly in this order:

1. strict GCC/Clang C17, registered-routine/probe audit, ASan/UBSan, complete
   package suite, and clean `R CMD check --as-cran`;
2. actual R 4.3.3, 4.5.2, and development R plus pinned-header compilation and
   exact `environment/r-api-exceptions.tsv`/raw-token/version-gated DSO audit;
3. upstream differential with reviewed intentional Paradox-2 deltas;
4. all exact reviewed downstream bridge heads, then priority-zero/one reverse
   and GitHub consumers and documentation workloads;
5. GCT, instrumented-R Valgrind, bounded rchk, adversarial corruption, and
   direct coverage of every registered routine/hazard family;
6. examples, vignettes, manuals, pkgdown/book/gallery/website and legacy
   upgrade workloads;
7. Windows x86-64 and real macOS Apple-silicon ARM64 CI for the exact ref;
8. paired release benchmarks on an otherwise idle host.

Primary retained drivers include `scripts/native-check`,
`scripts/check-r-api-compatibility`, `scripts/test-runtime-matrix`,
`scripts/memory-check`, the `compat/` runners/verifiers, and
`benchmarks/release`. Read their `--help` before use; do not copy historical
run IDs or expected counts. Expensive analyzer and consumer gates run only
after cheaper package/bridge gates are green.

Never accept a green GitHub matrix label as portability evidence by itself.
Each platform row must reject a nonzero `rcmdcheck` child status and require one
sole final `Status: OK`; an always-run completion job must then reject the
complete matrix aggregate unless it is exactly `success`. The offline verifier
independently requires three successful REST jobs (macOS ARM64, Windows x86-64,
and completion), both exact platform artifacts, their check logs, and frozen
candidate provenance. A release-only direct-child companion changes only the
workflow to pin and check out the immutable candidate and to reduce the matrix;
it must retain both completion layers.

Profile representative constructor, `check`/`check_dt`/`check_dependencies`,
`has_deps`, values, domains/params/dependencies, subset/collection, live Shadow
constraint and read/write paths, design, and sampler workloads. Optimize only
measured hot paths, retain portable scalar code unless a portable
architecture-neutral improvement is proven, and rerun affected correctness
tests after every optimization. Freeze performance changes before the final
memory/portability matrix.

The Paradox-1 comparison has two narrowly ledgered integrity-read budgets. Only
`shadow_values_live` uses `integrity-shadow-read` (3.25 median/3.50 q75), and
only `collection_values_{plain,rich,nested}` use
`integrity-collection-read` (2.75/3.00). These synthetic direct reads perform
generation/signature or complete capsule-DAG admission that the cached legacy
surface did not. The exception is timing-only: both keep the `hot` 1.25 ratio
and 16-KiB minimum allocation thresholds. All filtered getters,
mutation/constraint/domain paths, and real miesmuschel/mlr3pipelines consumer
rows keep their `hot`/`standard` tiers.
Never widen a global tier or add another integrity row without retained profile
evidence and explicit design review. Treat non-pass integrity rows as required
raw-distribution review, and normally retire these contract-reset tiers once
Paradox 2 is the authenticated baseline.

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
to earlier candidate commits or tooling identities remains historical.
