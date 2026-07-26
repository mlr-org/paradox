# Paradox 2 compatibility boundary

Paradox 2 spends one unreleased major-version break on removing compatibility
that was theoretical, accidental, or disproportionately expensive. The goal is
near-drop-in compatibility for ordinary documented code, not emulation of every
private mutation or exotic R object that Paradox 1 happened to accept.

The normative details are in
[`contract-first-2.0.0.md`](contract-first-2.0.0.md). This document is the
reviewable impact summary for maintainers and downstream authors.

## Preserved ordinary behavior

Subject to the legacy-object migration boundary described below, preserve:

- exported constructors, functions, R6 generators, S3 methods unrelated to the
  closed Domain/Condition engines, and normal argument meanings;
- `ParamSet$new()`, `ps()`, `psc()`, `c.ParamSet()`, `ps_union()`,
  `ps_replicate()`, `ParamSetCollection$new()`, and ordinary clone/serialize
  behavior;
- package-owned R6 class vectors, public method and active-binding names,
  reference semantics, parameter order, names, and printable representations
  for ordinary inputs and stable ALTREP inputs in documented semantic atomic
  positions;
- the five built-in parameter kinds, bounds, levels, grouping, storage types,
  tolerances, defaults, initial values, special values, tags, and cargo;
- named-list and `$values$x <- value` assignment, unset-via-`NULL`, explicit
  named NULL, required tags, exact TuneTokens constructed through `to_tune()`,
  presence modes, sanitization,
  dependencies, constraints, transformations, aggregation, and internal
  tuning;
- the public `assert_values` assignment-policy flag. It remains on the R6 shell
  and chooses checked versus unchecked native storage; it is the sole stateful
  shell policy outside the capsule model and remains clone/serialization/
  equality-visible;
- ordinary non-ALTREP S3-classed named configuration lists such as bbotk
  control objects; their outer class is ignored and not an extension/dispatch
  seam, including when supplied explicitly to `$search_space(values=)`;
- ordinary data.frame/data.table inputs with well-formed additive leading
  classes. Their terminal `"data.frame"` or
  `c("data.table", "data.frame")` suffix selects only the structural table
  kind; leading classes never dispatch and are ignored without copying an
  ordinary shell. An already-required ALTREP snapshot canonicalizes the suffix;
- named scalar bounds/tolerances produced by ordinary R indexing and arithmetic;
- `all.equal()` on ParamSet-family objects through detached public semantic
  state, including values/dependencies/callbacks and complete COLLECTION/
  SHADOW graphs, rather than private R6 environment traversal. Canonical node
  references preserve shared-vs-duplicated DAG topology while allowing
  independently built equivalent graphs to compare equal;
- informative built-in value diagnostics from one package-owned native
  classifier/formatter. Missingness, type/shape, integerish, bounds, and factor
  membership retain their established checkmate-style categories and useful
  message fragments. An ordinary scalar missing value keeps the missingness
  diagnosis even when its storage mode would otherwise be incompatible.
  Unknown parameter diagnostics use the actual misspelled ID for their
  failure-only native `"Did you mean ...?"` ranking. They preserve the
  case-insensitive partial-distance threshold and stable three-candidate limit,
  so omitted `ParamSetCollection` affixes are inexpensive; this deliberately
  fixes Paradox 1's ranking by the unknown entry's integer position.
  Byte-identical reproduction of every checkmate quirk, `conditionCall()`, or
  unsupported exotic-object behavior is not required;
- the historical distinction between callback-free structural `$deps <-`
  replacement and feasibility-checking `$add_dep()`. Copying dependencies after
  narrowing a parent Domain may leave a partially or wholly impossible
  predicate; that predicate is preserved and its child is inactive rather than
  making the copy fail;
- public data.table-shaped results with documented columns/types/order and
  safe `set()`/`:=` use on detached results;
- collection prefix/postfix spelling, nested order, shared-child DAGs,
  detached flatten/union results, named empty results, and opaque leaf identity;
- Condition constructor adapters, class/list shape, mutable/readable `rhs`,
  formatting, comparison, and serialization for CondEqual and CondAnyOf;
- documented callback order, values, warning/error propagation, and exactly-once
  execution, including unnamed list output from a base `extra_trafo`; collection
  child outputs remain named so they can be translated into the parent
  namespace. Live and detached collection callback closures remain callable R
  functions, but all select and translate callbacks through the same native
  capsule evaluator. Retained/untransformed inputs remain in input order,
  followed by changed child outputs in callback-plan order; omitted child
  outputs disappear. The three cold internal-tuning operations remain their
  single R implementations because their public payload is cargo callbacks;
  they capture required state before callbacks and commit through native
  mutation rather than selecting a fallback engine. Exact-TuneToken
  `$search_space()` conversion is the second narrow cold R orchestration: it
  receives one natively admitted snapshot, switches only over built-in token
  kinds, and owns callback-dependent output compatibility without another
  native/R conversion path.

Maintained constructor-corpus evidence remains important: prior scans found
thousands of `p_dbl`, `p_int`, `p_lgl`, `p_uty`, and `p_fct` calls across active
mlr-org sources. Representation and common constructor spelling must therefore
be tested against that corpus even though the constructor implementation is
now native.

## Intentional contract changes

| Former behavior | Paradox 2 decision | Consequence |
|---|---|---|
| Downstream reads/writes `.params`, `.values`, `.deps`, `.trafos`, `.sets`, reconstructed Domain `.trafo`, or generated R6 internals | Private state is opaque; use public accessors/setters and `$subset(..., keep_trafo = FALSE)` for an untransformed detached search space | Dark-matter scripts using internals must migrate. bbotk uses public `$sets`, miesmuschel uses the official shadow node, and mlr3mbo uses the transform-stripping subset API. Malformed Domains created by deleting private columns remain rejected. |
| A subclass overrides a core ParamSet method/active binding | Only additive inheritance is supported | Additive `Codomain` remains viable. A new semantic node belongs in Paradox. Modified shells are not authenticated/emulated. |
| Third-party `domain_*` S3 methods add a parameter kind | Domain system is closed over five kinds | Use `ParamUty(custom_check=)` for opaque values or propose a maintained Paradox kind implemented across all operations. |
| Third-party Condition S3 dispatch | Conditions are closed over CondEqual/CondAnyOf | Unknown conditions are rejected at dependency admission. ConfigSpace/celecx retain built-in function names and shapes. |
| A subclassed TuneToken, extra token metadata, or S3 method participates in conversion | TuneTokens have one package-defined exact `{content, call}` shape and five exact built-in class vectors; native admission precedes closed `$search_space()` switching | Use `to_tune()`. Extra/reordered fields, classes, attributes, S4 structure, malformed calls/content, and recursive metadata reject before traversal. Structurally indistinguishable manual copies are not creator-authenticated, but the representation remains non-API. |
| A forged ObjectTuneToken Domain is interpreted by a token-specific partial validator | Constructor, ParamSet, and token paths share the sole canonical built-in Domain-row admission owner; token content must additionally be a bounded, value-producing tuning Domain, and malformed structure raises a hard boundary error | Ordinary package-built bounded Domains remain compatible; unbounded `ParamUty` and zero-level `ParamFct` remain invalid as Domain tuning ranges. A zero-level factor is still canonical for typed empty operations. Malformed cargo, kind/storage, grouping, bounds, levels, defaults, tags, requirements, initialization, and special-value/transformation combinations do not become ordinary check diagnostics. |
| Stored user callbacks retain source-reference metadata and pointer identity with the object supplied by the caller | Callback admission recursively discards `srcref`, `srcfile`, and `wholeSrcref` from the stored copy; a callback with no such metadata is stored unchanged | Serialized objects no longer retain complete source-file text through package-interpreted callbacks. The stripped copy keeps the exact enclosing environment and ordinary callback results, but source display/introspection can change and `debug()`/`setBreakpoint()` on the original function does not modify it. `options(paradox.strip_srcrefs = FALSE)` is an admission-time debugging opt-out. Source-reference normalization leaves function-valued `$values` and all opaque value payloads untouched. Authenticated legacy detached-collection wrappers are always rebuilt in a fresh environment for safe child rebinding; disabling stripping retains their metadata, not wrapper identity. |
| Direct `condition_test()` can inherit class/dimension behavior from `Ops` or `%in%` | One closed native comparator accepts `NULL` or plain logical/integer/double/character vectors with names only | Ordinary internal and direct vector comparison remains. Factors, dates, arrays, and other classed/attributed operands must be converted explicitly; they cannot install another Condition engine. Stable ALTREP operands are materialized once. |
| Native fast path returns a sentinel and R/checkmate/data.table repeats the operation | One native semantic engine plus one shared C value-failure classifier/formatter | No callback replay or duplicate validation. Successful admission does not format messages; ordinary failures retain informative checkmate-style categories/fragments without a runtime checkmate call. Byte identity for every quirk or call frame is not promised. |
| Checked assignment rejects a Domain-valid entry whenever its dependencies are unsatisfied | Checked assignment stores the entry as a dormant value; raw `$values` retains it and default `$get_values()` filters it until its dependencies hold | Type, bounds, special-value, ParamUty custom-check, TuneToken, sanitization, unknown-ID, and graph-atomicity checks still run for dormant entries. Switching a parent later reactivates the stored value without reconstructing it. |
| An absent dependency parent is unsatisfied even when it has a satisfying recorded default | One recursive activity kernel consults a recorded default when an active parent is absent; `NoDefault` remains unsatisfied and an explicit value overrides the default | This resolves #265 consistently across point checks, stored-value filtering, dependency-only checks, and constraints. A default on an inactive parent cannot reactivate a descendant. |
| `presence = "all"`/`"required"` exempts a child whenever its parent is absent, even if the parent default satisfies the edge | Presence uses default-aware point activity | A default-active absent child may now be reported missing. For `"all"`, the missing parent itself often remains the first diagnostic. |
| `get_values(check_required = TRUE)` exempts a required child whenever its parent is absent, even if the parent default satisfies the edge | Required-value checking runs after default-aware filtering of the raw store | Required dormant entries remain exempt, but a default-active required parameter is demanded. |
| Constraint callbacks receive every supplied/stored entry, including dependency-inactive entries | Constraints receive only the active subset of the configuration | Authoritative BASE/COLLECTION/SHADOW check/test/assignment sites filter once before invoking callbacks or schema-free detached/Shadow adapters. COLLECTION uses its full translated namespace before passing unprefixed child slices; SHADOW uses complete merged origin state before its native merge adapter. Callback count, order, snapshots, and scalar-result admission are unchanged. Code intentionally inspecting inactive entries must adapt. |
| A checked `$values <-` store is assumed to satisfy `$check(ps$values)` | Legal stores may contain dormant values while strict checks continue to validate explicit points | `$check()` remains store-blind and never fills a candidate from stored values. Use `$check(ps$get_values())`, `remove_dependencies = FALSE` only for raw-state inspection, or `check_strict = FALSE` when dependency validity is intentionally not being tested. |
| Dependency diagnostics are emitted only for strict point failures, with downstream code matching `"can only be set if"` | Assignment no longer emits dependency diagnostics for dormant storage; the still-failing point paths retain the `"can only be set if"` fragment | Downstream message matches remain useful. An absent parent whose default does not satisfy may add default context, but retains the established leading diagnostic. |
| `check_dependencies()` traverses a data.table with `pmap()` and concatenates every failing edge | One strict native dependency-only operation returns the first diagnostic | Ordinary uniquely named base-list calls and TuneToken edge skipping remain. It shares default-aware, store-blind point activity with `$check()`. Classed/attributed list containers and code matching a newline-collapsed multi-error string must adapt. Unknown IDs are checked even when there are zero dependency rows. |
| Constraint-only table checks validate and invoke callbacks through an R row loop | One native two-phase graph/point/constraint operation | With value assertion enabled, all rows validate before any constraint callback; ParamUty custom checks may run during that validation phase. Constraints then run once per row from the operation snapshot. Reentrant mutation affects the next operation, not later rows. The input remains data.table-only. |
| `$check()`/checked assignment runs a ParamSet `ObjectTuneToken` transformation to prove target compatibility | Native admission accepts only an exact nonempty bounded BASE ParamSet capsule without callbacks; COLLECTION, SHADOW, and additive subclasses reject. `$search_space()` alone evaluates one-dimensional output compatibility | Valid BASE candidates for numeric, categorical, logical, and utility targets remain storable. A structurally valid but output-incompatible candidate now errors when its search space is requested, not during assignment. Corrupt candidates still fail before commit. |
| A live ParamSet token candidate can mutate after validation but before the outer write/search conversion uses it | Checking roots generation receipts and checked assignment performs a final allocation-free reauthentication; search replaces the live candidate with a sealed single-use BASE subset capability before callbacks | A nested/finalizer mutation wins and the outer assignment errors without committing. Search conversion never calls or rereads the original candidate shell. A safe alias retaining exact genuine BASE private/core linkage may pass because C uses only the sealed core, not alias methods. |
| A third-party or layered Shadow reconstructs/overrides ParamSet internals | Paradox owns one native Shadow node; a direct Shadow origin is rejected | miesmuschel uses the official class. Layered views must combine hidden IDs over their ultimate BASE/COLLECTION origin. |
| A classed visible-list input can dispatch through `c.*` while a Shadow constraint restores hidden values | The native Shadow adapter manually builds an ordinary hidden-first/visible-second list | After the authoritative graph site selects the active hidden/visible slices, the callback receives an ordinary filtered list with the same opaque leaf identities. The outer list class is representation-only and cannot replace merge semantics. |
| Mutating `SamplerUnif$samplers` changes or triggers the hierarchical execution graph | The list is descriptive compatibility metadata; replacement/reordering errors and child state is ignored | Ordinary construction/introspection remains. Use `SamplerHierarchical` when custom 1-D samplers must execute. |
| Grid generation always materializes the nominal Cartesian product, then data.table overwrites fixed values, masks dependencies, and removes duplicates | One native output-sensitive grid operation deduplicates realized axes, collapses fixed axes, and enumerates dependency-valid branches while retaining first-nominal-occurrence order | Ordinary configurations, types, column order, and row order remain. Nominally enormous but small realized grids now work, and `upper_limit` bounds the final result. A nominal zero axis remains empty. Valid cross-storage/`NULL`/S4 fixed specials retain identity in list columns instead of inheriting accidental data.table coercion/deletion; a fixed TuneToken receives an informative error. |
| Strange ALTREP behavior is preserved by declining to R | Stable/base ALTREP is materialized once in admitted semantic atomic positions. Interpreted configuration/search-space/trafo and ParamSet-`params` lists, internal table/row/Domain/Condition/token/capsule shells, Domain cargo/interpreted cargo entries, dimnames, class/name vectors, and other list metadata remain ordinary non-ALTREP/non-S4 structure. Direct checked/unchecked `$values <-` rejects an outer ALTREP before observation; `set_values(.values=)` alone snapshots an outer list. A state-changing custom ALTREP across prior R-side representation capture is unsupported. | Compact sequences such as `1:n` work where atomic vector semantics are documented. General ALTREP structural shells still reject. Typed Domain special leaves reject ALTREP; an admitted typed S4 special matches only by pointer identity. ParamUty leaves remain opaque, with Paradox-1 special membership implemented by base `identical()` (including S4) and no dispatch. Native materialized state is semantic authority, and Paradox never replays or itself causes a crash or memory corruption. |
| Each public table operation recognizes metadata independently, dispatches through a table subclass, or trusts row labels/cache payload | All six ingresses share one strict structural classifier. A well-formed ordinary class vector ends in `"data.frame"` or `c("data.table", "data.frame")`; leading additive classes are representation-only and never dispatched. The classifier does not copy or materialize an ordinary shell merely to remove the prefix, and semantic snapshots ignore it; an already-required ALTREP snapshot installs only the canonical suffix. Missing/empty/bytes/duplicate labels and reversed, non-suffix, or repeated reserved labels reject. Names/classes and allowed data.table cache carriers are ordinary; caches are discarded. Raw integer/character row names are count-only: ordinary compact `+/-n` is decoded and stable ALTREP pays one Length/no Elt. An admitted top-shell ALTREP owns names/class before callback-capable observation and copies one Length/one Elt per column. | Canonical and additively classed tables such as mlr3's `bmr_aggregate`, base lazy duplicates, and zero-column data.frames remain supported without reopening S3 semantics or adding a prefix-only ordinary-shell copy. Missing, S4, object, attributed, or row-count-mismatched metadata rejects on row-consuming paths. Direct `trafo` and a no-edge Design dependency plan validate row-name structure without extra column observations. `Design$transpose()` returns one empty configuration per row of a zero-column data.frame; an empty unclassed list remains zero rows. Package tables/facades remain canonical ordinary objects. |
| Internal state is a data.table and old versions receive spare-capacity repair | Internal tables are base data.frames; data.table >= 1.18.4 is outward-only | Users need the new minimum. Returned facades remain usable; private capacity/index layout is gone. |
| Old serialized objects execute through their legacy private layout | `upgrade_paradox_object()` is the pure single-object converter; `upgrade_paradox_object_graph()` recursively transplants nested ParamSet-family shells by identity. Historical method targets are cold error/opt-in-upgrade gateways, never current hot paths. | Callers can upgrade a containing R6/container graph without locating every nested ParamSet. First use errors by default; `options(paradox.legacy_object_action = "upgrade")` opts into silent migration. Standalone Domain/Condition normalization remains pure. Direct bindings are classified natively, so realized language/symbol values are not confused with delayed promises and neither is forced. Current shells use an ordinary additive BASE/COLLECTION/SHADOW class-suffix classifier plus exact `assert_values` and canonical-core agreement. Every prepared/current root is jointly validated before the first transplant; a stale Shadow is admitted through a non-installing authoritative preview with separate source/semantic core receipts. Commit then rebases each parent to already-transplanted child identities in post-order. Before each transplant it jointly validates all already-current identity roots plus that newly rebased prepared root; unrebased parents remain offside templates until their turn. Afterward it jointly validates the current identity roots with the transplanted original added. Gateway forwarding selects the defining-family enclosure and ignores the old stub's serialized `private`/`super` promises. Built-in and owner method provenance requires exact loaded namespace identity, not a spoofable namespace label. Unknown legacy subclasses fail unless their exact owner class has an authenticated registry bridge. That bridge is deliberately limited to additive BASE state with no owner-declared dependency (implicit authenticated BASE callback-carrier dependencies are composed internally) or a single-origin current Shadow replacement; owner R6 finalizers cannot be transplanted safely and are rejected. Exact generated detached callback wrappers contribute captured child ParamSets as migration dependencies only after an inert ordinary-list snapshot. Package/global/search environments remain traversal boundaries. A pending finalizer from an unrelated user object can still run inside the R-level binding wave; a selected-root mutation is detected afterward, but a completed transplant is not rolled back and the externally corrupted graph is outside the retry guarantee. |
| Exact implementation frames and side-effecting promise/fallback priority are reproduced | Documented force/callback order and messages are preserved, internal frames are not | `substitute()`, `sys.call()`, `parent.frame()`, closure-body introspection, and mutation during private admission are not compatibility targets. |
| An outer S3 class on a named value or explicit search-space list can influence generic dispatch | An ordinary non-ALTREP list may carry representation-only names/class; the class is discarded. Direct checked/unchecked assignment rejects an outer ALTREP before any observation and canonicalizes the Paradox-1 empty spellings (`NULL`, an ordinary attribute-free zero-length atomic/expression vector, or an accepted empty list container) natively. | Classed controls keep working for assignment and `$search_space(values=)`, but the class cannot replace Paradox value/token semantics. S4/list-like, ALTREP-shell, and other attributed containers reject. Only `set_values(.values=)` has the one-snapshot shell exception. |
| S4 bits on Domain/Condition/token structure or typed defaults happen to pass ordinary S3/type checks | All interpreted shells/metadata are explicitly non-ALTREP/non-S4, including the outer `special_vals` list for every kind. A typed S4 special/default/init is admitted or matched only by pointer identity. ParamUty value/default/init/special leaves remain opaque, including S4, while special membership alone uses base `identical()` without dispatch. | Ordinary built-in objects are unchanged. Code using ALTREP/S4 as structural metadata must migrate; explicit opaque ParamUty and typed-special identity use remains. Condition structure never treats S4 as opaque. |
| R releases older than 4.3 are rejected | Minimum R is now 3.6 and shipped code uses portable C99 | Current releases keep their public, allocation-free API path. R 3.6--4.1 uses `base::exists(..., inherits = FALSE)` only for cold optional existence checks; required binding snapshots remain native and allocation-free, and the terminal optional receipt scan uses ledgered old-only `R_HasFancyBindings()` to fail closed on a fancy frame. R 3.6--4.5 obtains stored cells through ledgered `Rf_findVarInFrame` and may inspect a returned promise through ledgered `R_PromiseExpr`/`PRENV`/`PRVALUE`; R >= 4.6 uses experimental public binding/dots APIs and treats a detached non-binding `PROMSXP` as opaque. R 3.6 has no accessor for an active-binding function, so recursive migration of a graph containing one fails closed and must be run under R >= 4.0. Paradox-1 ParamSet-family R6 shells themselves use active bindings; current Paradox-2 objects, standalone legacy Domain/Condition conversion, and graphs without active bindings remain supported. R 3.6 also lacks list ALTREP itself; only the list-ALTREP test fixture is unavailable and the production branch is vacuous. Every exceptional entry is centralized, version-bounded, and audited; none is a general CRAN allowlist. |

These changes happen in the first public Paradox 2 release, not in later
staggered breaks. Delaying them would make users pay the migration cost twice
and would preserve expensive duplicate machinery without observed value. In
particular, no maintained consumer was found that needs exotic structural
ALTREP/S4 shells; retaining them would reintroduce multi-observation and dual
R/native admission costs without protecting ordinary semantic ALTREP vectors.

## Why these breaks are proportionate

The maintained consumer census found real reliance on public constructors,
values, data.table-shaped results, collections, callbacks, and built-in
Conditions. It found only a small number of private-layout uses:

- miesmuschel's `ParamSetShadow` was a real semantic feature implemented by
  reconstructing ParamSet internals. Paradox now owns that feature, so the
  downstream bridge becomes simpler and safer rather than losing behavior;
- bbotk read collection `.sets` once; the public `$sets` accessor is an exact
  replacement;
- `Codomain` relies on additive inheritance, which remains supported;
- no maintained package required a genuinely new Domain kind, arbitrary
  Condition class, subclassed TuneToken or extra token metadata,
  generated-wrapper mutation, or exact state-changing exotic ALTREP observation
  across the R-capture/native boundary;
- the maintained corpus contained no ObjectTuneToken whose ParamSet content was
  a collection, shadow, or additive subclass. Concrete uses construct ordinary
  BASE candidates with `ps(...)`, so narrowing this one content form removes
  graph/method complexity without a known caller migration.

Unknown external scripts may use internals, but continuing to authenticate
every mutable R6 surface imposed large code, performance, and safety costs on
every ordinary operation. A major release plus substantial speedup, explicit
NEWS/migration notes, and small maintained downstream bridges is the chosen
tradeoff.

## Downstream transition

### miesmuschel

The maintained bridge selects and re-exports `paradox::ParamSetShadow` when
Paradox >= 2 is installed and retains its legacy class for Paradox 1. It must
test origin identity, fixed visible schema, live values/dependencies/
constraint/transformations, visible write-through, hidden-value preservation,
cross-boundary rejection, collection nesting, clone, and serialization.

Miesmuschel registers its exact legacy
`c("ParamSetShadow", "ParamSet", "R6")` owner class with Paradox 2. Its inert
inspector returns the authenticated origin/hidden-ID dependency state; its
current rebuilder constructs Paradox's package-owned Shadow. The recursive
upgrader then transplants that current surface into the old shell identity and
installs precise retired-field errors for `params_unid` and `set_id`. This
narrow exact-class bridge does not make arbitrary third-party subclass
internals interpretable.

Miesmuschel's constructor/dictionary fidelity tests compare operator class,
public representation, and public ParamSet values rather than recursively
comparing opaque R6/private environments. This preserves the behavior those
tests intended to assert without turning Paradox's capsule or inherited R6
binding layout into a downstream equality contract.

### bbotk

bbotk changes the private `private$.sets` access to public `$sets`. Its
`Codomain` tests must demonstrate that calling `super$initialize()` and adding
nonconflicting behavior remains supported. It registers the exact legacy
`c("Codomain", "ParamSet", "R6")` class with an inert inspector and current
additive rebuilder; Paradox authenticates and injects the prepared BASE state
before the old shell identity is transplanted. Native code that retains
pointers into detached public
`$data` or `$deps` facades must root the complete returned owner objects for the
pointer lifetime; Paradox does not preserve a hidden private alias as a GC root.

### Other maintained packages

mlr3mbo uses `$subset(..., keep_trafo = FALSE)` on Paradox 2 rather than
mutating detached/private Domain transformation state. ConfigSpace, celecx,
bbotk, mlr3, mlr3fselect, mlr3tuning, mlr3pipelines, the active
book/gallery/website, and priority CRAN reverse dependencies are tested against
the closed built-in names and ordinary public behavior. Downstream tests that
only gated ordinary missing/type/length/bounds/levels diagnostics because the
early Paradox-2 messages were less informative should be returned to their
pre-gate assertions where the native compatibility matrix now matches. Tests
may still avoid pinning internal call frames or `conditionCall()`, and genuine
behavioral bridges remain. None of this creates a runtime compatibility branch.
An old repository that no longer imports/uses Paradox is not a release blocker
merely because it exists in the organization census.

Dormant storage removes the hierarchical AutoTuner refit failure in which a
previously configured child became inactive before checked assignment, and
ordinary `lrn(...)`/`$configure()` calls may now retain such settings. The
default-aware rule makes common cases such as an SVM child depending on the
default learner type active rather than silently discarding them. Downstream
hashes that deliberately want a canonical *active* configuration should hash
`$get_values()`; hashes over raw `$values` intentionally include dormant state.
A dependency owned by a `ParamSetCollection` filters the collection read only,
so child consumers continue to see their own child-level state.

The Paradox-1 miesmuschel Shadow explicitly calls `self$assert(rhs)` and
therefore remains strict on that compatibility axis. Its Paradox-2 bridge uses
the official `paradox::ParamSetShadow`, which inherits dormant storage and the
single native activity contract without another downstream implementation.

Dual-version downstream branches should be available before Paradox 2 is
submitted. Agents prepare and locally test branches, but repository policy
requires the user to push them and open PRs manually.

## Retained bug fixes

Paradox 2 intentionally fixes behavior that is erroneous rather than preserving
it for byte-for-byte compatibility:

- empty settings honor `presence = "all"` and `"required"`;
- `domain_qunif()` rejects incompatible input dimensions;
- collection child transformations run exactly once with the child signature;
- child assignment reaches the child's public semantic state, including a
  Shadow origin;
- child constraints receive active unprefixed child values and strict
  collection checks use live child constraints;
- `check_dependencies()` validates unknown IDs even when the ParamSet has no
  dependency rows and uses the same live graph snapshot as `$check()`;
- dependency activity consults recorded defaults for absent active parents,
  recursively, while explicit values override defaults and `NoDefault` remains
  unsatisfied (#265);
- BASE dependency cycles remain constructible through the existing mutation
  surface, but every shared activity consumer detects them and errors
  deterministically instead of looping or producing a partial active set. A
  raw `$get_values(remove_dependencies = FALSE, check_required = FALSE)` read
  is not an activity consumer; it still validates the dependency/Condition
  structure but returns the admitted raw store without traversing the cycle;
- callback-dependent TuneToken search-space plausibility sampling is
  deterministic and restores caller RNG kind/state; native token admission does
  not sample or execute candidate callbacks;
- `ids(tags = character())` is `character(0)` and overlapping `any_tags`
  matches are deduplicated in parameter order;
- grouped double sanitization uses each parameter's own bounds;
- one-sided/fixed infinite double domains avoid accidental `NaN` while keeping
  defined infinite quantile endpoints;
- repeated subset IDs do not hit data.table join heuristics;
- `set_values(.insert=)` accepts exactly one unclassed, non-missing logical
  value, and its list merge is validated once by the native operation rather
  than inheriting R's condition-length/coercion accidents;
- empty dependency state is per object and cannot be contaminated through a
  shared data.table default;
- malformed constructor/domain row counts and stale Shadow transformation
  state fail or synchronize correctly instead of silently returning wrong
  results;
- a TuneToken recovered as a Domain `.init` value is cleared when that Domain
  becomes its own search space; it is not installed as a length-two fixed
  design value;
- collection equality no longer evaluates the inherited BASE `$params` active
  binding through `all.equal.environment()`.
- `ParamSetCollection$add()` rejects an existing or proposed cycle—including a
  path through a Shadow origin—before replacing the collection generation;
  failed or reentrant addition leaves the old collection unchanged.

Each retained fix requires an isolated package regression. New bugs discovered
during downstream testing receive the same treatment before the downstream row
is rerun.

## Compatibility acceptance

Release acceptance is based on:

1. direct contract tests for the preserved/broken boundaries above;
2. an upstream Paradox-1 differential with every intentional delta reviewed;
3. every exact reviewed downstream revision, including the bbotk,
   miesmuschel, and mlr3mbo runtime migrations plus dual-version test
   adaptations;
4. maintained priority-zero/one reverse dependencies and mlr-org repositories;
5. active documentation and serialized workload migration;
6. no-crash adversarial tests for unsupported subclasses, corrupt capsules,
   closed-kind and exact-TuneToken violations, ALTREP reentry, and hostile state
   changes across R-side representation capture and native admission.

Exact private shapes, old callback implementation frames, old data.table
capacity, and unknown S3 extensions are rejection/no-crash tests, not success
criteria.
