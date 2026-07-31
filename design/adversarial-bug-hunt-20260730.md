# Final adversarial bug hunt (2026-07-30)

Status: focused source-convergence review closed on the exact retained r9
snapshot described below. Package-facing source is not yet a committed,
immutable release candidate, and every full release gate remains pending. The
review was paused once at the requested bounded wrap boundary and subsequently
resumed. The historical starting point is clean package commit
`c1b0a283f6087adf35baf32b01acb4632cde7d50`, tree
`0d403c4aca3f49bea7a86d6239db4b712a5f3e2b`.

## Objective and constraints

Find and fix remaining defects that could cause a crash, memory corruption,
stale or aliased state, partial transaction, incorrect result, fail-open
migration, or misleading compatibility behavior. Add the smallest permanent
regression for each confirmed defect. A fix must not weaken the contract in
`AGENTS.md` or add accepted-path work merely as speculative defensive
programming; validation already required by an operation may be reordered or
made complete, and cold/error-path checks may become stricter.

This is a source-convergence review, not a release-evidence run. The
multi-hour compatibility, complete runtime, memory, benchmark, documentation,
and hosted portability matrices remain deferred until the source freezes.
Targeted strict compilers, static analyzers, native probes, focused
sanitizers/GCT/Valgrind, directly affected test files, and one final package
suite are appropriate.

## Independent review lanes

1. Native lifetime and portability:
   GC rooting, PROTECT balance, raw pointers across allocation/callback/
   interrupt/ALTREP observation, overflow, C undefined behavior, R 3.6--4.6
   API branches, formatting, and routine registration.
2. Graph, transaction, and migration:
   capsule generations, stamps and epochs, COLLECTION/SHADOW healing, shared
   DAGs and cycles, reentry/atomic commit, clone/equality, serialization,
   recursive migration, promises/environments, gateways, and owner bridges.
3. Semantic/public surface:
   Domain/Condition/TuneToken admission, values and dormant dependency
   activity, callbacks, public table facades, grid/design/sampler operations,
   collection translation, Shadow behavior, R6 wrappers, and diagnostics.
4. Cross-cutting root review:
   analyzer output, registration/coverage agreement, corruption matrices,
   randomized state-machine/property probes, test blind spots, performance
   impact, and consistency with the normative design.

Every lane reports exact locations, a reproducer, severity, and a
performance-neutral repair. A suspicious pattern is not a finding until the
relevant ownership, rooting, validation, or semantic invariant has been
traced end to end.

## Finding ledger

| ID | Severity | Area | Status | Regression / evidence |
|---|---|---|---|---|
| A1 | build blocker | compact data-frame row-name construction | fixed, proof pending | cppcheck `integerOverflowCond` at `paramset_domain_common.c`; negate while still `R_xlen_t`, after the existing `INT_MAX` proof |
| A2 | build blocker | numeric Domain snapshot | fixed, proof pending | Clang analyzer nullable `tolerance` report; split required-tolerance and bounds-only snapshots so the data flow encodes the contract without an accepted-path guard |
| I1 | high | internal-tuning multi-owner reads and commits | fixed, focused tests green | one native graph snapshot now supplies routes, cargo, owner/root values, and receipts; native stores validate all receipts before one atomic commit |
| I2 | high | subset/subspace callback detachment | fixed, focused tests green | subset state and detached callback plans now come from one admitted generation; callback carriers retain the exact selected leaf core, including nested Collection/Shadow cases |
| I3 | high | same-pointer Shadow signature mutation | fixed, focused tests green | receipted graph selection precedes dynamic admission and terminal checks compare carrier plus every entry; deterministic subset and deep-clone mutator regressions cover the former core-pointer blind spot |
| I4 | medium | `SamplerUnif` construction | fixed, focused tests green | clone and validate one owned graph, select all IDs natively from that graph, and derive singleton callback bundles without rereading the caller |
| I5 | high | deep-clone terminal generation barrier | fixed, focused tests green | the allocation-free native wave now authenticates initial Shadow carrier/content receipts as well as every private `.core` binding |
| I6 | memory safety on corrupt internal path | SamplerUnif malformed handoff bundle | fixed, compile and focused tests green | corrected `UNPROTECT(6)` to the four live protections; ordinary accepted path is unchanged |
| I7 | high | derived-core verification stamps | fixed, current/R-3.6 focused tests green | removed the finite Shadow-signature fingerprint, which could only probabilistically authenticate same-pointer list mutation; SHADOW and Shadow-bearing COLLECTION graphs remain unstamped and therefore take the exact authoritative comparison on every entry, while BASE and proven no-Shadow COLLECTION reads retain the same O(1) path. The address mixer is now a `uintptr_t`-width bijection, avoiding the old 64-to-32-bit truncation collision. Direct, nested, shared-DAG, in-place-entry, GCT, strict GCC/Clang, Clang-analyzer, and R-3.6 probes cover the repair. |
| R1 | medium compatibility/correctness | LHS/Sobol source ownership | fixed, focused tests green | restored the historical exact caller ParamSet reference without generation tearing: generation and Design normalization use one owned clone, a complete source receipt spans the operation, the caller reference is bound without replacement-method dispatch, and one allocation-free receipt scan is terminal. Vendored R 3.6.0 and 4.6.1 sources confirm ordinary `$<-` itself allocates before its environment write, so the bind deliberately precedes the barrier. |
| R2 | medium | zero-level categorical sampler | fixed, focused tests green | `Sampler1DCateg` now accepts the canonical empty probability vector, returns typed `character(0)` for zero rows, and errors before RNG entry for a positive request |
| R3 | high | `SamplerHierarchical` source-generation splice | fixed, focused tests green | construction now owns and validates one deep-cloned graph before consulting arbitrary Sampler subclass `$param` bindings; duplicate IDs reject and a reentrant binding cannot pair old IDs with a later source generation |
| R4 | test-contract defect | structural versus semantic ALTREP | fixed, focused tests green | removed a forged ALTREP `.params$id` success premise that contradicted strict capsule structure; the replacement regression places ALTREP in an admitted atomic point value and verifies safe reentry against an immutable selected graph |
| R5 | high | internal-tuning terminal receipt result | fixed, strict compile and focused tests green | the receipt entry returns `R_NilValue` instead of allocating a logical scalar after its exact scan, so LHS/Sobol and cold internal-tuning callers have a genuine allocation-free generation barrier |
| R6 | high | Collection/Shadow flatten generation splice | fixed, strict compile, direct probe, benchmark and focused tests green | no path carries preliminary R-side IDs into subset. Namespace-sensitive flatten uses one all-ID route/cargo/owner snapshot plus terminal receipt; callback-free flatten uses a dedicated all-current-ID native subset and rejects newly appeared metadata. Deterministic Shadow schema growth and post-plan mutation regressions cover both sides. The dedicated path avoided the provisional all-route performance regression: at 10,000 callback-free parameters the same-process elapsed sample was 0.040 s versus 6.383 s for unconditional route planning (explicit subset 0.014 s). |
| R7 | memory safety | internal-tuning root-value snapshot | fixed, focused regression added | removed a duplicate protection of the Collection root-value result which left R's protection stack one entry high on return; the focused Collection snapshot now makes any stack-imbalance warning an explicit failure |
| R8 | high | BASE flatten and default subspace generation splice | fixed, current/R-3.6 focused, direct and GCT probes green | BASE flatten and omitted-`ids` subspaces now reuse the all-current-ID native transactions; a deterministic Collection growth regression proves the default promise is not evaluated into a stale R-side ID vector, while explicit IDs retain their selected-ID path |
| R9 | high | default search-space value/Domain generation splice | fixed, current/R-3.6 focused, direct and GCT probes green | the lean R6 stub preserves the public `values = self$values` formal but forwards omission without forcing it; native admission now selects the current raw store and every target Domain from one capsule/graph generation, while explicit values keep their caller-snapshot path |
| R10 | low | Collection `$sets` synthetic write-back | fixed, parse/focused collection coverage green | one selected state now supplies both the read-only identity comparison and return value; this removes a redundant graph read and prevents a finalizer window from comparing one child list and returning another |
| R11 | low correctness/diagnostics | hand-built dependency language | fixed, current/R-3.6 focused proof green | `parse_depends()` now requires exact binary arity for `&&`, `==`, and `%in%`; malformed calls can no longer discard extra operands or leak a subscript error for a missing operand |
| R12 | high | `set_values()` merge/store linearization and direct assignment policy | fixed, current/R-3.6 strict build and focused proof green | `set_values()` now selects raw values, exact `assert_values`, merges, validates, and commits in one native transaction with a BASE core or complete graph receipt. A semantic list-ALTREP callback which commits during merge proves that the nested commit wins instead of being overwritten. Direct `$values<-` now enters one policy-selecting native transaction, rejects malformed policy shapes, and terminally authenticates the exact selected policy. The BASE benchmark is performance-positive: median 10,000-operation elapsed time fell from 0.616 s to 0.472 s for `set_values()` and from 0.441 s to 0.385 s for direct assignment in the same randomized five-slice process. |
| R13 | medium | `p_uty(custom_check=)` callback identity | fixed, current/R-3.6 strict build and focused proof green | construction now retains the exact callback supplied to one native probe. The callback runs in a fresh portable R evaluation frame, so ordinary `parent.frame()` scratch writes remain harmless and cannot replace the function between validation and storage. The callback path is constructor-cold; value checks are unchanged. |
| R14 | medium correctness | `ParamSetShadow$origin` multi-observation read | fixed, current/R-3.6 strict build, direct probe, and focused Shadow proof green | the active binding now refreshes, validates, and returns the origin from one exact SHADOW generation in one native call. It no longer obtains a core and then makes two more independently refreshable public reads before returning the shell. The operation is read-only and replaces three R/native transitions with one. |
| R15 | medium corruption/diagnostic boundary | constructor flags, empty union, and omitted replication controls | fixed, current/R-3.6 focused proof green | `ParamSet$new()` now admits `allow_dangling_dependencies` inside the existing native construction call even when `params` is empty; `ps_union(list())` no longer bypasses collection flag admission; and `ps_replicate()` reports the intended missing-control error without recursively forcing one missing promise through the other's default. No accepted hot operation gained another boundary. |
| R16 | high | `assert_param_set()` cross-generation decisions | fixed, current/R-3.6 strict build, direct probe, and focused BASE/COLLECTION/SHADOW proof green | one native assertion snapshot now supplies both the canonical class table and dependency presence. COLLECTION roots use one admitted graph, while BASE and SHADOW roots retain and validate one exact capsule generation; the first implementation's collection-only dispatch was caught by the focused suite and corrected. Bounds are computed from that same returned table, so a callback/finalizer cannot splice classes, bounds, and dependency answers from different live generations. |
| R17 | medium correctness | `all.equal.ParamSet()` multi-reader chimera | fixed, current/R-3.6 focused proof green | equality is cold, so it now uses the already receipted deep-clone transaction to detach one coherent graph before its presentation-oriented active reads. Self-comparison snapshots once, preserving reflexivity even when an opaque utility value's documented R6 clone callback mutates its source. This adds no work to ordinary ParamSet access or mutation. |
| R18 | high for custom Sampler subclasses | sampled rows paired with a rebound support | fixed, current/R-3.6 focused proof green | `Sampler$sample()` captures the support shell before invoking the subclass extension callback and performs one allocation-free identity comparison afterward. A callback which rebinds `self$param_set` wins, but the outer operation errors instead of constructing a Design from rows and a different support. Built-in sampling pays one pointer comparison and does not traverse or duplicate the graph. |
| R19 | high | `Design$new()` native-plan/R patch handoff | fixed, current/R-3.6 focused, ALTREP and GCT proof green | the native dependency plan now returns one compact receipt for the exact BASE capsule or complete COLLECTION/SHADOW graph it selected. After all callback-capable data.table patching and deduplication, one allocation-free terminal scan compares every selected live `.core` binding and exact Shadow carrier/content. A mocked patch which commits to the source proves the nested mutation wins and the stale Design is refused. |
| R20 | high | one-shot grid/R6 ownership handoff | fixed, current/R-3.6 focused, strict build/analyzer, ALTREP and GCT proof green | the native grid returns its final prepared table together with the exact source receipt, authenticates that receipt after allocating the bundle, and the public wrapper authenticates it again after constructing the R6 Design shell. BASE and child-Collection callback mutations during that handoff are both deterministic regressions. The scanner does not revalidate immutable capsule payloads: it compares each already-admitted plain binding directly and reserves the exact metadata walk for Shadows. In the isolated A/B, an eight-child scan cost about 0.6 microseconds, complete `Design$new()` was unchanged, and BASE-grid medians differed by about 1%, within the measured noise envelope. |
| R21 | medium compatibility | grid axis order without a global resolution | fixed, randomized current/R-3.6 focused proof green | restored the established split contract caught by the expanded output-sensitive suite: a global resolution emits canonical ParamSet order and treats per-parameter controls only as count overrides; without a global resolution, explicit numeric controls determine numeric-axis order and categorical axes follow in schema order; nominally empty typed grids use canonical schema order. The native builder resolves each output axis back to its admitted parameter row, so ordering does not duplicate quantile semantics. |
| V1 | high correctness | shared-DAG value owner order | fixed, current/R-3.6 focused proof and strict compile green | collection routing had sorted touched children before untouched children, so both `a.x` and `b.x` alone were cleared in `{a = shared, b = shared}` even though `b` is the later owner. Plans now retain exact child order, remove one redundant routing pass, and preserve the specified depth-first last-owner rule symmetrically. Direct replacement and insertion regressions cover both sides. |
| V2 | high transactional correctness | direct graph value-assignment receipt | fixed, current/R-3.6 focused, strict GCC/Clang and analyzer proof green | direct Collection/Shadow assignment retained ultimate BASE target generations but not every routed node. A callback could replace the collection root/edge while leaving the selected BASE unchanged, or an earlier Shadow hidden-value read could be paired with a later origin target generation. The existing traversal now records capsule/Shadow-signature entries in rooted native workspace, with one planning-epoch coherence comparison and allocation-free scans before callbacks and at commit. BASE assignment allocates no receipt; the common eight-node graph uses inline C workspace; ordinary entries append in amortized O(1) time, while only repeated Shadows reuse their allocated exact signature snapshot. `set_values()` and internal-tuning paths reuse their already-selected graph receipt. Root replacement, conflict-warning reentry, same-pointer Shadow metadata mutation, and list-ALTREP merge reentry are permanent regressions. |
| V3 | high adversarial correctness | same-pointer assignment-policy mutation | fixed, current/R-3.6 focused and strict compile green | terminal `assert_values` authentication compared only the binding pointer, allowing an in-place attribute/content rewrite of that exact logical object to evade the barrier. The allocation-free tail now also re-admits its exact scalar contents and compares the selected boolean; a custom-check regression proves the outer write remains atomic. This adds only scalar content admission at the already-required transaction barriers; it allocates nothing and does not add a callback or R transition. |
| C1 | high | Design dependency graph generation | fixed, focused test green | Collection-backed Design planning now uses the receipted graph builder and terminally authenticates every exact Shadow carrier/content receipt as well as live shell bindings; the existing nested-Shadow GC mutator now covers this path |
| C2 | high | native field-mutation transaction | fixed, static and focused mutation proof green | `replace_one()` and the derived tag update now construct all update carriers before comparing the caller-selected core; `core_replace()` then owns the allocation-capable successor construction and its own terminal binding receipt, so a finalizer cannot validate one generation and update another |
| C3 | memory safety under direct native/adversarial call | grid-resolution controls | fixed, strict build/analyzer green | the selected `param_resolutions` leaf is rooted before destination allocations; a finalizer rewriting a caller-owned controls list can no longer leave the native local as the sole reference |
| C4 | high | check, Collection-reader, and Collection-constructor graph coherence | fixed, static lifetime proof and semantic-reentry control green | each allocating multi-node selector now captures the session state epoch after its initial heal and compares it once after selection. This rejects supported finalizer mutations that could otherwise combine child cores which never coexisted, with one comparison per operation and no per-node hot-path receipt. Reentry after a graph is completely selected remains valid and is separately tested. |
| C5 | high | Design ALTREP column metadata generation | fixed, focused adversarial test green | Design transpose admits the attributes copied onto its owned result after all Elt observations; dependency planning similarly admits class metadata after its element pass. Valid columns still perform one metadata scan, while an Elt callback can no longer replace class/other structural metadata after admission |
| C6 | high | forwarded `ParamSetShadow$add_dep()` | fixed, focused adversarial test green | the Shadow keeps the exact validated origin core rooted through Condition RHS admission and compares the live origin binding immediately before delegation; a callback mutation wins and the outer dependency is not installed |
| C7 | memory lifetime | select-all internal-tuning ID snapshot | fixed, proof pending | the newly allocated ID vector was retained only in a C local through an interruptible 65,536-element copy and entered its indexed protection only afterward. It is now installed in the existing indexed root before the first interrupt check; the copy and accepted-path allocation count are otherwise unchanged. |
| M1 | high | deep-clone shell/policy generation | fixed, strict current/R-3.6/R-4.0 compile, analyzer, and current/R-3.6 focused tests green | clone discovery now snapshots each node's exact `assert_values` policy, the allocation-free terminal native receipt authenticates shell-to-enclosure-to-private topology, shell policy, private capsule, and Shadow signature together, and every non-root shallow shell clone is reset to that selected policy. Previously a finalizer could rewire a shell to a different private environment after R discovery while the earlier private/core pair remained unchanged, or an opaque ParamUty R6 clone callback could mutate a source/child policy after capsule selection. A direct topology-drift receipt and adversarial root/child policy callbacks cover both gaps without adding work outside the cold clone operation. |
| M2 | medium corruption boundary | legacy structural list admission | fixed, strict current/R-3.6/R-4.0 compile, analyzer, and current/R-3.6 focused tests green | one inert native representation predicate now makes migration distinguish ordinary VECSXP lists from pairlists, S4, and structural ALTREP at every interpreted list shell/list-column boundary, while list-shell attributes survive until their owning validator admits or rejects them. Forged pairlist `.values`/Conditions, structural ALTREP `.values`, and unclassed extra attributes on `.values`, Condition, or a list column used to be silently normalized away as if they were canonical Paradox-1 state; focused malformed-state tests require fail-closed rejection without observing ALTREP elements. This changes only the cold legacy upgrader. |
| M3 | high on R 3.6--4.1 | first-use/current-shell gateway snapshot | fixed, strict current/R-3.6 build and focused smoke green | old-R optional binding lookup allocates through `base::exists()`. Gateway topology is now completed before state selection; both policy/core existence probes finish before allocation-free snapshots, so a pending finalizer cannot mutate an already selected same-pointer policy or capsule during the other lookup and defeat the terminal receipt. Current R keeps the same allocation-free read count and path. |
| M4 | high correctness | historical gateway default omission | fixed, current/R-3.6 focused tests green | Paradox-1 and pre-release lean stubs always pass their formal defaults to the unversioned target, so the old generic gateway made omitted `$subspaces(ids = ...)` and `$search_space(values = ...)` arguments look explicit. On first-use migration, and forever for a restored current-core shell retaining an unversioned stub, this evaluated an R-side IDs/value view before the native generation transaction. The three affected cold gateways inspect `missing()` in the still-active historical stub frame without forcing its promise, remove only a genuinely omitted argument, and preserve explicit arguments lazily. Current versioned stubs and all hot operations are unchanged; BASE and Shadow subspace plus search-space omitted/explicit regressions cover the bridge. |
| M5 | high compatibility/correctness | serialized Sampler first use | fixed, actual Paradox-1.0.1 fixture current/R-3.6 focused proof green | A Paradox-1 Sampler's inherited `.__Sampler__sample` stub was plain-aliased to the current method. It therefore reached native sampling through its old private child methods before any nested legacy ParamSet invoked a family gateway, producing the raw “must be a current ParamSet” error even with auto-upgrade enabled. The old Sampler sample target now shares Design transpose's cold embedded-graph gateway: default use reports the actionable migration error, opt-in upgrades the complete Sampler/child graph by identity before replay, and a healed or reserialized old stub forwards directly. Current versioned Sampler stubs remain gateway-free. A retained text-encoded RDS generated by Paradox 1.0.1 covers default no-mutation, nested identity preservation, auto-upgrade/sample, and healed round-trip replay; R 3.6 exercises its intentional non-mutating active-binding-inspection boundary. |
| M6 | test portability | serialized legacy fixture decoding on R 3.6 | fixed, current/R-3.6 focused files green | The fixture's base64 bytes were decoded identically on both runtimes, but R 3.6's `memDecompress(type = "gzip")` rejected the valid gzip stream that `readRDS()` accepted. The test helper now streams those bytes through `gzcon(rawConnection(...))`, and the one legacy-Collection success case is skipped at the already documented R >= 4.0 active-binding inspection boundary. This changes no package path or accepted operation. |
| M7 | high under adversarial finalizer mutation | legacy table/list/value generation pairing | fixed; strict current/R-3.6 syntax and installs, focused current/R-3.6 ownership smoke, and the 232-record current native inventory are green; exact-source release analyzers remain pending | Migration formerly read a table's names/class, allocated while reading columns, and later reread optional attributes; its plain-list copier likewise copied elements before rereading names. Owning only top-level columns and direct atomic list leaves was still insufficient: canonical dependency and Domain tables contain package-interpreted nested Conditions, requirements, cargo, and special-value carriers. A pending callback/finalizer could mutate one of those shared structures after its sibling atomic column had been detached and create a result generation that never existed. It also copied every atomic legacy stored value, violating the exact-identity contract for ParamUty. The cold native table snapshot now recognizes only exact canonical dependency/Domain schemas and reuses the closed native owners for each interpreted field. Conditions, requirements, cargo, special-value shells, and typed leaves are detached; ParamUty leaves and callbacks remain opaque identities, with `NoDefault` still interpreted only in schema default/init positions. A separate native value-store snapshot uses the already-owned parameter id/class generation to detach typed leaves while preserving every ParamUty value, including atomic/classed values. Private shallow carriers retain source-cell identities. Every admitted nested stable-ALTREP Length finishes before one callback-free recursive receipt over all source and owned carriers; source table attributes and outer identities are authenticated afterward. R validates only surrounding legacy references, constructs table facades, and normalizes callbacks; it neither repeats leaf ownership nor reconstructs built-in Conditions/requirements. Unsupported or changing structure fails closed; current operations and hot paths are unchanged. |
| M8 | high data corruption | owner-rebuilder public/private/enclosure freshness | fixed, exact current build and focused current tests green; R-3.6 parse green | A replacement owner rebuilder could return one prepared shell for two distinct legacy owners, or return a distinct public shell borrowing a selected current/prepared node's private or method-enclosure environment. Preparation accepted those aliases; commit could then partially upgrade the graph, silently change an existing current Shadow's origin, or leave two public identities sharing mutable private state. Migration now discovers the complete session before construction and, before modifying a hook result, requires its public, private, and complete superclass-enclosure environment set to be internally canonical and disjoint from every original/current session node and every prior prepared node. Valid shared origins/dependencies and current-node identity reuse remain accepted. Focused regressions prove rejection is pre-commit and preserves serialized bytes, enclosures, cores, and policies; the checks are confined to cold migration. |
| M9 | high under adversarial finalizer mutation | migration public-shell commit receipt | fixed, strict current/R-3.6 compile, analyzer, direct probe, focused current tests, and R-3.6 load/smoke green | Joint capsule validation after a transplant authenticated the shell/enclosure/private/core topology and policy, but not the complete public method/active-binding surface or its lock bits. A pending finalizer could therefore replace a public method during the R binding wave; migration returned success with an authenticated current capsule behind the hostile method. Preparation now captures the exact complete public generation selected from every prepared current shell. A genuine locked R6 public environment is required, making the selected name inventory complete: locked environments may replace unlocked bindings but cannot add or remove them. Commit ends with one native barrier which performs its allocation-capable graph admission and then, in the same allocation-free tail, scans both every exact graph receipt and a batch receipt over every transplanted and already-current selected public shell: exact class pointer, environment lock, every binding symbol/value, active kind, and lock bit must still match. Deliberate mutations of a transplanted shell, a selected already-current shell, and a lock bit all fail closed. A completed transplant remains monotonic and is not rolled back. This work exists only on explicit/cold migration. |
| M10 | medium corruption boundary | legacy internal-table row metadata | fixed, authentic fixtures plus current/R-3.6 focused and native-probe proof green | The coherent legacy table snapshot required names and class but merely allowed `row.names`; migration therefore silently normalized a table declaring one row around a two-element column. The same allocation-free terminal table generation scan now requires any present metadata to be ordinary canonical integer row metadata—empty, compact `c(NA, +/-n)`, or exact `1:n`—matching the selected independently owned first column's length. The complete pinned `mbo_config` fixtures exposed that Paradox 1's actual classed-list-plus-`setkeyv()` construction omits `row.names` even from populated keyed transformation tables. Absence declares no competing row count and is therefore admitted; the exact native-owned columns define the count and the R-side migration validator rejects inconsistent ordinary lengths without constructing a second semantic copy. Present mismatched metadata still rejects. Stable semantic ALTREP columns are materialized and receive their final Length observation before the terminal source/attribute receipt; callback-capable or arbitrary row-name structure never enters migration. No current or hot operation changes. |
| M11 | medium old-runtime compatibility | current-only graph migration on R 3.6 | fixed, focused current/R-3.6 proof and current direct inventory green | The M9 terminal public receipt was unnecessarily constructed even when discovery found only current shells. Besides doing cold work for a semantic no-op, this called the deliberately unavailable R-3.6 active-binding-function accessor and made `upgrade_paradox_object_graph(current)` error despite the documented current-graph no-op. A current-only session now ends after the same joint native capsule validation it already performed. Sessions with any transplant retain the complete final public receipt over both transplanted and selected current shells and still fail closed on R 3.6 before mutation. This removes work and restores compatibility without weakening a mutation boundary, because an all-current session has no binding wave. |
| O1 | high data corruption | public schema/Domain/property ownership | fixed, current focused proof green | `$params`, `$domains`, `$data`, and static property active bindings previously detached the outer table or vector while sharing nested mutable levels, cargo, special/default/init leaves, Condition carriers, and atomic-leaf attribute values with capsule state. Their common native projection path now owns every interpreted carrier and built-in typed atomic leaf including complete supported ordinary, acyclic, bounded nested attribute metadata. Closure-valued presentation metadata rejects cleanly because supported old R has no allocation-free public terminal receipt for R's duplicated closure shell; functions used as semantic leaves/callbacks remain opaque under their documented identity rules. ParamUty leaves, callbacks, environments, external pointers, and typed S4 identity tokens keep exact identity. The work is confined to outward access; internal schema operations are unchanged. |
| O2 | high data corruption | public raw/filtered value ownership across BASE/COLLECTION/SHADOW | fixed, current focused proof green | raw `$values` for BASE and SHADOW returned capsule-backed leaves while COLLECTION took a different path; filtered values likewise exposed typed atomic leaves. One registered graph-aware raw reader now serves all node kinds, and both raw and filtered projections detach their names/list carrier and typed atomic leaves by admitted parameter kind. Internal store planning was moved to the retained graph reader, so assignment pays no public-detachment cost. Opaque utility leaves preserve identity, including a utility payload whose arbitrary outward class happens to be `"NoDefault"`; marker interpretation remains confined to schema default/init positions. |
| O3 | high data corruption | generic data.table finalizer and Collection `$sets` carrier | fixed, current focused proof green | the defensive data.table finalizer copied only the outer table and named columns, allowing `set()` on an unnamed atomic/list-column spine to mutate caller storage. It now materializes stable ALTREP payloads once, owns every ordinary/ALTREP column spine and bounded arbitrary attribute metadata without passing a caller-owned attribute spine to R's duplicator, and preserves opaque list-column leaves. `$sets` similarly returns a fresh named list carrier around the exact child shells instead of exposing the capsule's carrier. |
| O4 | memory safety under translated diagnostics | transient UTF-8 translation buffer | fixed, current clean build and ordinary/forced-collection focused proof green | diagnostic escaping and several affix/value/table paths retained the pointer returned by `Rf_translateCharUTF8()` across a later transient allocation. One shared helper now roots the CHARSXP, measures translation under a bounded vmax scope, allocates the destination, reacquires the translation, verifies its length, and only then consumes its bytes. Multi-piece affix builders retain and recheck every measured size. Ordinary valid UTF-8 diagnostics still return without allocating a copy. |
| O5 | high data corruption | public ID/name carriers | fixed, current focused proof green | the unfiltered `$ids()` fast path returned the canonical capsule ID column directly, while the R-computed `$has_trafo_param` and `$is_logscale` payloads attached that same column as names. A by-reference attribute setter could corrupt every later schema read. Unfiltered IDs now own one public vector, filtered IDs already did, and both R properties attach an owned names carrier. BASE, COLLECTION, and SHADOW regressions exercise the fast path. |
| O6 | high correctness | terminal ownership receipt for public leaves and columns | fixed, current clean build and focused ownership proof green | the first ownership repair used `R_compute_identical()` as a final comparison after detaching arbitrary attributes. That operation can itself allocate and recurse, so it was not a terminal barrier. Ordinary built-in leaves and public table columns now compare exact payload bytes/element identities plus a bounded recursive receipt for the complete selected attribute graph in one allocation-free tail; stable ALTREP leaves materialize once and close the same structural generation around that observation. The attribute-free ordinary hot path is one package-owned allocation plus one memcpy/pointer loop and allocates no receipt. |
| O7 | high memory safety under adversarial metadata | caller-owned attribute-spine and nested-metadata duplication | fixed, strict current build, complete native/GCT probes, and focused ownership proof green | R's shallow/deep duplicators may walk a raw attribute pairlist or recurse through nested presentation metadata after an allocating preflight. A pending finalizer could splice in an overlong/cyclic graph and turn an outward copy into an unbounded walk or native-stack exhaustion. Public built-in leaves, table shells, and columns now use a package-owned copier capped at 64 recursive frames, 65,536 nodes, and 64 attributes per carrier; it selects every edge into scanned R storage before recursive allocation, installs standard/general attributes only through public setters in a fixed dependency-safe order, and ends with an allocation-free graph receipt. Setter-normalized raw spellings reject rather than disappear; closure and `DOTSXP` presentation nodes reject while semantic functions remain opaque. Qunif metadata and typed list-leaf compatibility use a separate bounded top-level shallow copier: nested values retain exact identity, but the caller-owned spine is never handed to R's duplicator, and its common attr-free path allocates nothing. Valid-cell cycles/overlength are bounded with `R_mapAttrib` on R >= 4.6 and the reviewed facade loop on older R. |
| M12 | high correctness on cold migration | dispatch-free class/environment/table admission | fixed, current clean build and both focused migration files green | candidate recognition and environment exclusion previously used inheritance/package helpers which can inspect malformed outward metadata through behavior broader than the migration contract. One native ordinary-class snapshot now serves Domain, Condition, node, and top-level admission. The crawler identifies package and namespace environments from non-forcing ordinary bindings and exact namespace identity, excludes recognized user databases, and never traverses `.GlobalEnv`. R rejects unsupported table columns before `lengths()` or materialization can dispatch. The registered routine and native coverage ledger agree. |
| M13 | high correctness on cold deep clone | clone target and enclosure freshness | fixed, current clean build and focused equality/Shadow/migration proof green | graph cloning previously called each child shell's live `$clone` binding and trusted the returned shell topology. It now snapshots the complete public/enclosure/private/super chain through non-forcing plain-binding reads, invokes the locked package implementation selected by built-in node kind, and requires every returned public, private, and enclosure environment to be internally canonical and disjoint from the complete source graph and every earlier clone before assigning `.core` or `assert_values`. Shared-DAG identity and Shadow origin memoization remain unchanged. |
| M14 | high correctness under adversarial finalizer mutation | migration per-node edge-generation coherence | fixed, strict compilation against every supported R header, focused current tests, and actual R-3.6.3 build/migration proof green | Graph discovery previously scheduled a node's attributes separately from its vector, pairlist, closure, or environment edges, with allocating path/carrier work between the two passes. A pending finalizer could therefore splice attributes from one generation to primary edges from another, traversing a graph which never existed. Every direct path now allocates all rooted carriers before one joint capture; the reviewed old-R closure facade captures formals, body, and environment without allocation once that carrier is rooted. The allocating old-R bytecode bridge and every environment still require two independently rooted complete snapshots to match exactly. Environment snapshots include attributes, parent, exact sorted binding inventory, kinds and promise/value/function edges, binding locks, and environment lock/object/S4 state without invoking an active binding or forcing a promise; package/import/user-database boundary policy is reapplied from the selected snapshot. Structural list/expression ALTREP rejects before attributes or its provider are observed. Finalizer regressions require vector and environment discovery to select exactly one whole old/new generation, the matching boundary decision, or fail closed. This is confined to cold migration. |
| M15 | high memory safety under adversarial finalizer mutation | migration search-boundary lifetime | fixed, strict current/all-supported-header compilation, analyzer, complete current native-probe inventory, and focused current/R-3.6.3 migration proof green | The crawler precomputed every attached search-path environment but retained those `SEXP` identities only in an unscanned `R_alloc()` array. A finalizer could detach a boundary after initialization; once no ordinary R root remained, collection and later address reuse could make discovery traverse the former boundary or falsely match an unrelated environment. One indexed ordinary `VECSXP` now owns every boundary identity for the complete allocating call, with the native array retained only for lookup. Growth roots the replacement carrier before a new raw entry is published. A registered cold test seam crosses the carrier's initial capacity, detaches the only external references, and collects after boundary initialization, proving the selected environment cannot finalize inside discovery and does finalize after the native root is released. |
| M16 | high lifetime safety under nested migration callbacks | legacy nested-Length operand roots | fixed, strict GCC/Clang C99 focused snapshot green | The M7 two-phase receipt initially retained nested source operands only through their mutable parent carriers. A stable ALTREP `Length` callback could replace its own Condition RHS or special/value cell, or replace a requirement's sibling `cond`, and collect before the selected pointer was used by the next receipt. Every callback-capable leaf now receives a direct source/snapshot root across dispatch; a requirement roots its selected `on`, sibling `cond`, and both detached counterparts together before either nested check. Self-detaching RHS and sibling-Condition regressions force collection; the latter retains a finalizable sentinel only through the old Condition and proves it remains live until the native row receipt releases it. The exact `release-fixes-preflight-20260731-r5` strict GCC/Clang focused snapshot passes. |
| P1 | high correctness | TuneToken value snapshots | fixed, current/R-3.6 strict compile, analyzer, registered native probes and focused tests green | Three distinct residues remained after the committed carrier work. (a) Search-space selection classified values, then allocated both result carriers and every per-token snapshot before revisiting the selected positions; an in-place class rewrite of an unselected value dropped that parameter from the search space with no diagnostic. Selection now uses one allocation-free exact classifier factored out of `exact_token_kind()` -- three-valued, so a value that merely claims `TuneToken` is still selected and still reaches its established structural error -- retains the exact kind per value, and ends with an allocation-free terminal reclassification of every retained value. (b) Token content was copied leaf by leaf with an allocation between copies, so an in-place write to a later bound could pair it with an earlier one; `to_tune(dt$lo, dt$hi)` retains the exact `data.table` columns, so `set()` alone reaches it. Canonical destination scalars are now preallocated and the payload copy is one allocation-free pass, eliminating the window instead of detecting it. (c) `SHALLOW_DUPLICATE_ATTRIB` copies only the attribute pairlist spine, so the detached snapshot shared its `class` and `names` vectors with the live token -- the same shape as O5 -- while four later readers use that class as the dispatch authority. The snapshot now owns both from the closed class table, and the terminal barrier compares the live token instead of a vector the caller still shares. The classifier also removes an ALTREP `Elt` dispatch from ordinary value admission, which the surrounding comment already forbade. |
| P2 | high correctness | Collection `$add()` terminal Shadow receipt | fixed, current/R-3.6 strict compile, analyzer, registered native probes and focused tests green | The hole was wider than ledgered: neither the add-topology snapshots nor the two flattened collection graphs retained any Shadow metadata, and a Shadow's `.core` stays pointer-identical while its `.paradox.shadow.snapshot.v1` carrier or one entry is replaced, so more than twenty allocations between selection and commit were unguarded. Both topology walks now retain the exact signature carrier and its entry snapshot per node, rooted in the existing graph root carrier, and the terminal scan compares them through the shared `paradox_shadow_signature_receipt_is_current()` -- no second Shadow validator. Both collection graphs switch to the receipted builder and the terminal wave adds `paradox_collection_graph_snapshot_is_intact()` beside the existing live-binding scan; the two are complementary, not redundant. The topology receipt is still required on its own: a Shadow reachable only by descending another Shadow's origin edge appears in no flattened graph, and a directly added BASE or SHADOW child gets no graph at all. A test-only registered reentry entry point evaluates a hook at the graph and topology snapshot boundaries, mirroring the subset reentry seam; the production entry passes `R_NilValue`. Direct, in-place-entry, nested-below-a-child-Collection, already-in-the-receiver, origin-edge-only, and shared-DAG mutations are permanent regressions, each asserting that the collection core, its `$sets`, and the child graph are unchanged. |
| P3 | medium public-kernel correctness | nested factor/logical Domain cargo | historical complete-row fix; superseded by the masked public-Domain amendment below | **Historical initial implementation:** `paradox_snapshot_builtin_domain()` is not usable at this boundary: it admits exactly one row of a bounded non-`ParamUty` Domain, so it rejects multi-row tables, zero-row tables, `ParamUty`, and unbounded numeric Domains -- all supported public operations -- and it changes the identity of what a read-only operation validates. The repair is therefore the shared admission-only adapter, with `paradox_admit_builtin_domain_row()` still the sole semantic owner: that owner is split into `paradox_admit_builtin_domain_schema_row()` -- identity, closed kind, grouping, tags, cargo, transformation, special values, bounds, levels -- plus the default/requirement/initialization remainder a constructor additionally owns, and the full-row entry point is now that schema call plus the remainder. The split is required, not cosmetic: a public `$domains` projection deliberately carries the stored TuneToken in `.init`, which the cold search-space converter detaches. `paradox_admit_public_domain_table()` validates only the outward column container, captures all sixteen columns in one allocation-free pass, reuses eight preallocated scalar carriers, and admits every row unconditionally; the four public kernels then read the admitted columns, rows, and numeric schema instead of reselecting them. Fifteen defects close together, all reachable with an ordinary malformed Domain: the special-value fast path no longer suppresses factor-level admission; duplicate, attributed, classed, named, S4, and structural-ALTREP factor levels are rejected by check, property, sanitize, and quantile alike; logical-level shells are admitted structurally rather than by value alone; special-value row shells and typed ALTREP special leaves are rejected before observation; `ParamUty` cargo is admitted canonically and non-`ParamUty` cargo is admitted at all; infinite and integer-inappropriate tolerances, non-integerish integer bounds, kind-inappropriate levels and bounds, an empty `id`, and a `grouping` unrelated to `cls` are rejected everywhere. Duplicated per-kernel validators are removed rather than kept beside the owner. Canonical zero-level `ParamFct`, the empty Domain, zero-row typed Domains, unbounded numeric Domains, `ParamUty` opacity, multi-row tables, and every pinned diagnostic are preserved by regression. |
| P4 | high adversarial correctness | direct Condition operand after callback-capable `Length` | fixed, exact r9 focused proof green | `condition_test()` now performs its sole ALTREP `Length` observation before a terminal hard-bounded re-admission of type, ALTREP state, object/S4 state, and the complete allowed attribute spine. Only that post-observation generation may supply raw names metadata or enter comparison, so a provider cannot splice malformed structure between an earlier proof and the selector. Ordinary input adds no R transition or fallback engine. |
| P5 | undefined behavior on hostile generation change | Domain empty-special names receipt | fixed, exact r9 focused proof green | every interpreted row's names-presence byte is initialized before special-value admission. A nonempty special-value shell no longer leaves indeterminate workspace which a later terminal generation path could inspect after the source premise changed; the accepted path gains no allocation or callback. |
| P6 | contract and memory-safety boundary | structural ALTREP name metadata | fixed, exact r9 current/R-3.6 focused proof green | a provisional broad exception for ALTREP names in built-in presentation metadata was rejected: structural class/name metadata remains ordinary and hostile providers reject before observation. The package instead materializes the deferred-string names it creates for categorical levels and emits compact ordinary data-frame row names directly, preserving ordinary base-R behavior without enlarging the native admission language or adding an O(n) facade materialization. |

The final migration slice used a clean staged source. Strict GCC 14/C99
installation on R 4.6.1 was warning-free; its DSO SHA-256 is
`4313b881180c6314363b18209b5363bd2c35aef48bfc92e4a0f2950de958cac3`.
Clang 22's analyzer emitted no diagnostic for `upgrade_graph.c`. Both focused
migration files passed on current R (apart from the configured missing external
MBO fixtures), and their executable R-3.6.3 coverage passed after the intended
old-runtime skips/warnings. The complete plain registered-native inventory has
215 passing records and zero failures; its independently verified TSV SHA-256
is `5846922ed1ad4aa9df78effde494de591262db1cfbf13d11944251c8fee90e5d`.
No compatibility, memory, or release matrix was run for this focused slice.

## Bounded wrap result

At the requested stop, one fresh isolated R 4.6.1 installation compiled and
loaded the complete worktree. The focused batch covered public ownership,
snapshot transactions, both migration suites, equality/deep clone, Shadow,
Collection construction, shared-write conflicts, and diagnostic encoding.
After restoring the specific invalid-name diagnostic and updating the
constructor-bundle expectation for its retained stable `sets` carrier, every
ordinary selected test passed. The diagnostic transient-lifetime regression
also passed with its non-CRAN forced-collection case enabled.

No complete package suite, compatibility matrix, memory analyzer, benchmark,
documentation, portability, or release run was started. P1--P3 above are
deliberately recorded as open rather than being hidden by the wrap boundary.

## Merged native lifetime cross-review evidence

The merged-worktree review traced callback-result observation, direct and
nested Shadow receipts, core-replacement transactions, value multi-target
commit waves, Collection add/reflatten, Shadow refresh, grid controls, Design
columns, upgrade traversal, and the old-R binding facade. Callback results are
observed once: constraints consume one logical element, custom checks retain
one logical/string diagnostic, extra-transform results enter one owned list
snapshot, and opaque utility/per-parameter transformation leaves retain
identity.

A clean staged R 4.6.1 install completed from the merged source. Clang 22's
static analyzer was then clean for `design_dependencies.c`,
`design_transpose.c`, `paramset_check.c`,
`paramset_collection_construct.c`, `paramset_collection_values.c`,
`paramset_mutate.c`, and `paramset_qunif.c`; logs are retained under
`.local/tmp/adversarial-crossreview-analyzer-final/`. Focused
`test-native-paramset-mutation.R`, `test-native-design-dependencies.R`, and
`test-native-generate-design-grid.R` runs were green. The new Design metadata
and Shadow-origin receipt cases also passed before two later, unrelated
expectation/fixture failures in their containing files; those two tests were
reported to the root review for independent correction. No complete package,
compatibility, memory, or release matrix was run during this lane.

The graph-epoch fixes deliberately do not reject a supported mutation after a
complete immutable graph snapshot has been selected. Their remaining
adversarial trigger is a pending finalizer at an allocation inside native
child admission; capsule structure intentionally excludes callback-capable
ALTREP there, so a deterministic package-level trigger would require a
test-only hook in the selector itself. The exact allocation/epoch proof and
the separate admitted-semantic-ALTREP reentry regression were retained instead
of adding production hook cost or broadening capsule admission.

A final merged snapshot was built concurrently from separate source trees on
R 4.6.1 and R 3.6.3, avoiding cross-runtime object-file reuse; both strict
header builds installed and loaded cleanly. The focused current-R selection
covered dependency and grid generation receipts, output-sensitive grid order,
operation snapshots, public-table admission, semantic ALTREP lifetimes,
forced-GC entry points, and both migration suites. The R-3.6 selection covered
the same executable receipt/grid/table/snapshot paths and both migration
suites, with only the documented unavailable-runtime and external-fixture
skips. Both selections passed. The complete plain registered-native inventory
then passed all 215 records, including its terminal summary, and its
independent verifier accepted the result. The current-R DSO, R-3.6 DSO, and
native-result SHA-256 values were respectively
`d5c9c1014f2388c72ca1b68129b79924e70d53bdcc952aacc644168b551ff8df`,
`43a7a2e132ea65c480092a5d396b01ca873f0acbc03251fcfbc26728e79230d8`,
and
`ddbcebe1554a032f34a769108b3a4316b3544400e57de1a5315620aa02c05fda`.
The final receipt scanner also passed strict current and R-3.6 compiler
diagnostics, Clang analysis, and cppcheck; all changed R files parsed and
`git diff --check` remained clean. The source-convergence review still did not
run the deferred full compatibility, memory, portability, or release matrices.

## P1--P3 convergence slice

All three deferred items are closed with permanent regressions. The evidence
for this slice, all from a clean staged install of the worktree:

- Strict GCC and Clang syntax sets and the Clang static analyzer are green for
  every changed translation unit -- `domain_kernels.c`, the new
  `domain_row_admission.c`, `domain_construct.c`, `paramset_check.c`,
  `paramset_collection_construct.c`, `test_altrep.c`, and `init.c` -- against
  both the current R 4.6.1 headers and the pinned R 3.6.0 header set. The
  analyzer emitted no diagnostic; logs are under
  `.local/tmp/analyzer-p1p3-final/`.
- The complete plain registered-native inventory passes: 220 records and zero
  failures, and its independent verifier accepts the result. Two ledger
  defects had to be repaired first and were both failing before this slice:
  `environment/native-routine-coverage.tsv` ledgered `upgrade_class_snapshot`
  with no `direct_upgrade_class_snapshot` probe, which made the whole probes
  mode refuse to start, and `direct_test_tune_token_gc_mutation_snapshot`
  asserted the opposite of what the fixture has always returned. Three
  routines are new: the collection-add reentry seam, the GC attribute mutator,
  and the fourth `phase` argument of the TuneToken fixture.
- A 29-file focused selection under `NOT_CRAN=true` passes 2809 assertions.
  The seven remaining results are identical on a `214fdf3` build and are
  therefore pre-existing: three in `test-core-state-contract.R`, an eager
  `repr` deparse of an ALTREP default and an unguarded initial-value warning in
  `test-native-domain-construction.R`, one `gctorture` finalizer fixture in
  `test-native-paramsetcollection-construction.R`, and one ALTREP row-name
  rearm in `test-native-snapshot-atomicity.R`. None is caused or masked by
  this slice; they are reported to the root review unchanged. Note that the
  last of these fails the gate on its own, because the focused runner treats an
  uncaught warning as a failure.
- Targeted Domain microbenchmarks, median of medians over paired alternating
  processes at 20,000 iterations per workload. `ParamSet` hot paths are
  unchanged -- `$check()` 22.3 to 22.8 microseconds (1.02x) and `$qunif()`
  262.9 to 258.7 microseconds (0.98x) -- confirming that no ParamSet operation
  enters these kernels. The public Domain operations pay the admission they
  previously skipped: on a one-row Domain, `domain_check()` moves from about
  4.5--5.1 to 8.4--9.8 microseconds (1.7--2.1x), `domain_nlevels()` and
  `domain_is_bounded()` from 3.0--3.4 to 7.4--8.1 (2.2--2.7x),
  `domain_qunif()` from 3.2--3.6 to 7.8--8.3 (2.2--2.6x), and
  `domain_sanitize()` from 4.1 to 8.3 (2.1x). A 512-row bound Domain costs
  4.5x on check and 8.7x on quantile. Two reductions are already applied: the
  owner answers duplicate detection directly below two elements instead of
  building a hash table, and the adapter selects only the twelve columns a
  Domain operation interprets. The residue is the per-row semantic admission
  itself, which is the repair. `paradox_snapshot_builtin_domain()` was not a
  cheaper alternative -- it allocates roughly forty objects per row and
  structurally rejects multi-row, zero-row, `ParamUty`, and unbounded numeric
  Domains, all of which are supported public operations. Whether about four
  microseconds per public Domain call is worth paying is a maintainer
  decision; no benchmark policy row covers these entry points today.

  **Release-convergence amendment (2026-07-31):** the twelve-column selection
  above describes this historical measurement, not the final structural
  boundary. The release source validates the exact complete sixteen-column
  outward shell on every public operation for a typed built-in Domain,
  including typed zero-row and empty-value exits; the canonical zero-column
  empty Domain passes its separate exact validator. The four constructor-owned
  columns remain semantically opaque there, but their presence, uniqueness,
  storage type, and row count are structural. The historical timings are
  therefore not final release benchmark evidence.

  A historical pre-mask complete-shell development A/B compared the exact
  pre-correction `e923c1a` archive with that intermediate source in separate
  GNU C17 installations under R 4.6.1/GCC 14.3. Twelve paired,
  alternating-order rounds on one pinned
  CPU measured a fixed-call cost of about 4.1% for one-row `domain_check()` and
  `domain_nlevels()` (approximately 280 ns and 176 ns respectively), 2.2% for
  one-row `domain_sanitize()`, and no material regression for 512-row
  `domain_check()` (1.004x). The other one-row ratios were 1.016--1.021x and
  bulk `domain_qunif()` was too noisy for a directional claim. The small fixed
  cost motivated the later single-owner masked refinement below. This
  historical comparison is not the final candidate-bound three-way timing
  evidence required by the receipt-compaction plan.
- Not run for this slice: the R 3.6.3 runtime behavior stage, GCT/Valgrind,
  and the deferred compatibility, memory, portability, and release matrices.

## Post-commit review wave (87c24ba + 214fdf3)

Both hunt commits were re-reviewed after landing: one adversarial self-review
of `87c24ba` and six independent review lanes over the `214fdf3` checkpoint
(paramset_check, design/condition, core/collection/shadow, domain/value,
R-side core, samplers/atomicity/environment), each verifying its findings
against the installed build. Confirmed defects, all fixed in this wave with
focused regressions:

- **Live capsule `.values` published to user callbacks** (introduced by
  214fdf3; the wave's most severe finding). The internal-tuning snapshot
  stored `node->values` -- the live capsule list -- in `owner_values`, which
  `convert_internal_search_space()` hands verbatim to every documented
  `in_tune_fn` cargo callback. A by-reference `data.table::setattr()` inside
  the callback renamed canonical package-owned state and permanently bricked
  the child ParamSet (both reads thereafter: "Corrupt ParamSet capsule").
  `paradox_param_set_internal_tuning_snapshot` now publishes the public
  accessor's outward form: fresh list and name carriers with
  `paradox_detach_stored_value_leaf()` applied per leaf (`ParamUty` leaves
  keep identity). This also detaches the `hidden_values` slice a flattened
  Shadow forwards into the same callbacks.
- **Collection constructor admitted children every later read rejects.** The
  three `test-core-state-contract.R` results the convergence slice reported
  as pre-existing were diagnosed: one implementation defect --
  `initialize_child()` validated only `.params`/`.tags`/`.trafos`, so a child
  whose `.postfix`/`.values`/`.deps` were corrupt constructed a dead-on-
  arrival collection -- and two test-fixture defects (`data.table::setattr()`
  materializes a referenced ALTREP fixture before installing it, and
  `seq_along()` row names are compact-sequence ALTREP the capsule validator
  rejects by design). Construction now routes every child through the
  reader's admission in its read-only form -- the new
  `paradox_collection_validate_single_node_readonly()` for BASE/SHADOW
  children, `paradox_collection_graph_build_readonly()` for COLLECTION
  children; see the exponential-construction finding below for why admission
  must not commit -- and the S4 pins were extended to `.values` and
  `.deps`.
- **Design masker crashed on an infeasible stored-value predicate.** The
  plain-fixed-value path in `parameter_condition_matches()` skipped the
  operand-type guard, so a bulk-installed dependency whose RHS type can never
  equal the parent's (`CondEqual$new(1)` over a `p_fct` parent with a stored
  value) aborted `generate_design_random()` with `STRING_ELT ... not a
  'double'` instead of masking the child inactive. Plain and non-plain leaves
  now take the same `paradox_builtin_condition_scalar_supported()` admission,
  matching the list-basis activity kernel's verdict.
- **`condition_test()` rejected ordinary base-R names.** The checkpoint's
  hardening rejected ALTREP names outright, but `names(x) <-
  as.character(...)` stores a deferred-string ALTREP, so ordinary input
  errored "Condition comparison names are malformed". The names shell now
  admits ALTREP; `paradox_snapshot_semantic_vector()` captures all names
  before the first semantic element (one observation each), and the
  comparison boundary attaches the caller's names to the fresh result by
  identity without reading an element.
- **Out-of-bounds factor read under a nonconforming user RNG.** The
  `Sampler1DUnif` fill loop indexed levels with `paradox_qunif_level_index()`
  without checking its `R_XLEN_T_MAX` sentinel; a user-supplied RNG returning
  `NaN` read out of bounds. It now degrades to `NA` like the numeric
  branches, because raising would longjmp between `GetRNGstate()` and
  `PutRNGstate()`.
- **1-row Designs unwrapped non-plain fixed values.** `data.table::set()`
  unwraps a bare list value of length one, so a wrapped special-value leaf
  (`list(a = 1)`) was stored as its content. The replacement is now always
  passed as a one-column list. Pre-existing before 214fdf3, kept by its
  rewrite of the same loop.
- **Classed cargo dispatched user methods inside flatten.** The checkpoint
  dropped the plain-cargo guard, so `cargo$in_tune_fn` on a hand-built
  capsule with a classed cargo dispatched `$.rogue` inside
  `flatten()`/`disable_internal_tuning()` before any plan receipt exists. The
  per-row guard is restored (`is.list(cargo) && !is.object(cargo)`).

Test-side repairs beyond the above: the eager-`deparse1` pin now drives the
native constructor directly (`R/Domain.R` legitimately deparses the default
for `repr` before native admission, and the sibling test documents that safe
rejection); the init-equals-default advisory warning is pinned with
`expect_warning`; the snapshot-atomicity rearm test installs its fixture
through the finalizer-armed attribute mutator (`data.table::setattr()`
duplicates a referenced value, so the exact fixture never arrived); the
`$data` state-read mock expects zero R-level capsule reads (the accessor
derives everything from one detached `$params` projection); the gctorture
finalizer fixture for construction -- which can never fire, because
per-allocation collections are young-generation only -- was replaced by a
registered construction reentry seam (`test_param_set_collection_construct_
reentry`, arity 5, ledgered and probed) mirroring the add seam, and its test
no longer needs `skip_on_cran()`; two vacuous `private_forced`/`super_forced`
assertions in the gateway test now superassign from the stub frame; a
checkpoint-era stale adversarial test in `test-native-altrep-lifetimes.R` was
rewritten to pin the P3 contract (typed kinds reject the hostile ALTREP
special before observation, `calls == 0`; ParamUty observes it and completes
from the admitted row). The unused `param_set_tags_from_state()` helper was
removed. Completing the full suite (possible only after the exponential fix
below) exposed three more checkpoint-era failures the convergence slice's
narrower selections had never run, all test defects, all reproduced
identically on clean `214fdf3` and `87c24ba` builds:
`native_get_values_copy_table()` installed `attr(table, "row.names")` --
the plain read expands compact row names into an ALTREP sequence the
capsule validators reject by design, which failed the mixed-encoding
`get_values()` test and masked the malformed-Condition test's intended
"Unsupported Condition class" diagnostic behind a blanket `.deps`
rejection (the helper now uses `.row_names_info(table, 0L)`, and the
encoding test proves the implementation handled equivalent mixed encodings
correctly all along); and the ids-engine classed-column test pinned a
"callback-free" wording that the checkpoint's unified column checks
replaced with "must use an ordinary character representation" (the
dispatch-count pin, `callbacks == 0`, was and stays the real assertion).
With these, every one of the eleven previously failing pre-existing
results -- the convergence slice's seven, the `$data` mock, and these
three -- passes.

**Exponential construction of shared shadow/collection graphs (pre-existing,
suite-blocking).** Running the complete unit suite for this wave exposed that
it has been unable to finish since the checkpoint landed:
`test-regression-barren-subtree-prune.R`'s second fixture -- eighteen levels
of `Collection(a = shared, b = shared)` over a shared
`Shadow(Collection(...))` -- constructs in Theta(4^depth) on a clean
`214fdf3` build (measured 0.47s at depth 7 doubling twice per level; depth
18 is days). The two "full focused-wave processes that ran more than two
hours without completing" in the convergence slice were this defect, not
machine load. The driver is mutual recursion with no terminating cache:
nothing at or above a SHADOW can ever hold a verification stamp (the
signature carrier is rewritable in place, deliberately), so the post-order
capsule-graph heal re-enters `paradox_shadow_refresh_authoritative` for
every shadow occurrence, whose committing origin-graph build re-runs
`paradox_core_refresh` -- the full heal -- for every contained shadow of a
subtree the outer walk had just finished healing. The fix keeps the stamp
semantics untouched and removes the redundant recursion instead: the heal
now enters the new `paradox_shadow_refresh_authoritative_prehealed()`,
which resolves the just-healed origin read-only (the read-only receipted
build still captures every signature the currency comparison needs) while
committing exactly as before. Construction admission in the collection
constructor is read-only for the same reason
(`paradox_collection_graph_build_readonly` plus the new
`paradox_collection_validate_single_node_readonly()`): a committing
admission installs a capsule per validated occurrence and invalidates the
neighboring refresh signatures. Depth-18 construction now takes 0.64
seconds, and `check(list())` on the result 0.06 seconds.

**Performance recovery.** Callgrind attributed the then-current admission
overhead to column-name selection: sixteen full name-vector scans per public
call (four in `domain_info()`, twelve in the historical adapter), each an
`Rf_getAttrib` plus a
strcmp sweep. The canonical column names are now interned once at package
load (`paradox_domain_intern_column_names()`), and one
`paradox_domain_select_columns()` pass selects all requested columns by
CHARSXP pointer identity with a byte-comparison fallback, preserving the
exact container diagnostics. `domain_info()` also self-interns the accepted
`cls`/`storage_type` per call, the adapter hoists per-row column lookups and
skips rewriting unchanged carrier strings. Interleaved three-round medians
against a `214fdf3` build on an idle machine: one-row public operations fall
from 2.2--2.9x to 1.25--1.8x of baseline (about +1--2 microseconds absolute;
`domain_check()` 4.0 to 5.0, `domain_sanitize()` 3.2 to 4.3,
`domain_is_bounded()` 2.5 to 3.8), `ParamSet` hot paths stay unchanged
(0.96--0.98x), and the 512-row Domain improves from 5.4x/11.5x to
4.7x/9.5x (check/quantile) -- the remaining cost is the per-row semantic
admission that is the P3 repair itself.

Evidence for this wave: the R-API compatibility gate passes (7 pinned R
releases x 39 translation units x 2 compilers); the plain registered-native
inventory passes 224 records with the verifier accepting the result
(including the new construction-reentry row); `scripts/native-check` with
strict GCC, strict Clang, the Clang static analyzer, and per-mode probes is
green after two corrections it demanded itself: the rewritten construction
test no longer skips on CRAN (restoring the analyzer corpus to its ten
reviewed skips), and a checkpoint-era dead store in
`paradox_param_set_construct()` -- the caller's name attribute was protected
and immediately superseded by the shared capture -- was removed with the
function's protection counts renumbered. The complete unit suite passes
under `NOT_CRAN=true` -- 111 files, 8784 assertions, zero failures, zero
errors, zero warnings -- for the first time since the checkpoint landed
(the exponential-construction defect below had blocked every complete run
in between). Still not run, as
before: the R 3.6.3 runtime behavior stage, GCT/Valgrind, and the deferred
compatibility, memory, portability, and release matrices.

## Masked public-Domain admission (maintainer decision, 2026-07-31)

The maintainer accepted a refinement of the P3 boundary contract to recover
the remaining public-Domain overhead: each public operation now declares the
fields it interprets, and the row owner expands that declaration to its
rule-dependency closure (`paradox_domain_interpretation_closure()` -- cargo
pulls in tags and the transformation, special values pull in the
transformation; the identity spine is always admitted). `domain_check()`
declares everything and remains the one operation that certifies the entire
row; `domain_sanitize()` declares the numeric schema, `domain_qunif()` and
the level-count property declare the numeric schema plus levels,
boundedness declares the numeric schema, and the class predicates declare
nothing beyond the spine. This is not a return to pre-P3 per-operation
checks: there is still exactly one implementation of every rule, a rule
outside the closure is skipped whole rather than restated, and mask
correctness is owned by the same translation unit as the rules and pinned
by a registered closure fixture (`test_domain_interpretation_closure`,
including idempotence over all 64 masks). The visible consequence is
deliberate: a corrupt field outside an operation's closure is diagnosed by
the first operation that interprets it -- `check` always does -- instead of
by every operation; the accept/reject matrix is pinned in
`test-native-domain-nested-admission.R`. The kind and storage names are
additionally interned at load, so canonical rows resolve their kind by
CHARSXP identity. Interleaved medians against the `214fdf3` baseline after
the change: one-row operations 1.23--1.74x (from 1.25--1.93x; the
remaining one-row cost is the per-call fixed part, which masks do not
touch), 512-row `domain_qunif()` 3.67x (from 8.75x, 292 to 183
microseconds), 512-row `domain_check()` unchanged at ~4.7x by design, and
`ParamSet` hot paths unchanged.

## Exact r9 focused-review closure

The final independent native review closed P4--P6 against the immutable local
snapshot
`.local/checks/integrated-selectors-r9-20260731T122010Z`. Its complete source
manifest SHA-256 is
`0a000dfb692332aed5eacd80119b9ae3fdf037e21a31fa7bf1cf60956584db0a`.
The five directly relevant package-source identities are:

- `src/r_utils.c`:
  `f9b81c1d3aca0f99bae5bfc3625ba252eb513eb177c2e1b6dbf8d760110104fb`;
- `src/domain_row_admission.c`:
  `68233a976348f6b09ff42f5157fc780a3451fb52c797a91c85cbf82cc71e38c5`;
- `src/builtin_condition.c`:
  `621ce8498691a7ff60ba39e8cff617024664738c505a295cfa310cfd854aceff`;
- `R/ParamFct.R`:
  `4ee3b9179ff78880969acedcaafefb2f0eb8356c87d8a35b3d5ee590e16a3d53`;
  and
- `R/ParamSet.R`:
  `e676006d826fc97089d56ff5852fa369b79de4f9250b44cb7bfba7b3a66d940d`.

Five retained focused logs ended with `DONE` and process exit zero:

- current-R `Condition.log`:
  `7e494791778fad9946d60883cf0d514b403dfd11b312c80af2bd3c550fd2a27d`;
- current-R `native-design-transpose.log`:
  `eb1940f1604ef0174515ed11db7ad6584f7b78ecb1c3e2389178da29221e91e0`;
- current-R `native-domain-construction.log`:
  `033caeaef2f31c11d05f2bcaefb584d47edc60bcfe3cf2737d61a9a980d24df3`;
- current-R `native-public-accessor-ownership.log`:
  `9c97379a6cb472533ee656ceb683e83f850c7c15f9ced6e507bb38f086e4d3d6`;
  and
- actual-R-3.6.3 `native-public-accessor-ownership.log`:
  `c95f6a004903c3bd1d4d0bdc70847254fcd2a8410438632ee770c5feec791b2f`.

The r9 C/R implementation is byte-identical to the preceding r8 build
snapshot whose source-manifest SHA-256 is
`6ed37e6c07ca17d8f487a949cfc2e4cf510eaded448e89fc1b11c7750a114634`.
That r8 snapshot supplies the retained strict GCC and Clang GNU99 package
installs and ordinary current-R and actual-R-3.6.3 installs. Their log
SHA-256 values are, respectively,
`4c98c7df3538b95b5f573266401756403cadf59d517ea2eeb4aa351d488fb1c8`,
`910585703807a07e07733de6f5340bc7a7c27ae7ddcff55f1ec0298c8ebd3d57`,
`f64d4878fed4eda8345edd476e95503b491d277482c9397f4e3eea9d445a1a81`,
and
`6d39e7da4ce4679dced128a440308d9dff21d6ee2543981ff3ca426128916775`.
This transfer is limited to the byte-identical package implementation and is
focused development evidence, not release acceptance.

No immutable release candidate exists yet. In particular, the exact complete
runtime selection remains mandatory: R 3.6.3, 4.0.5, 4.1.3, 4.2.3, 4.3.3,
4.4.3, and 4.5.2 each own a complete supported-runtime stage, while the full
native lane owns current R 4.6.1. The complete package, C23, API/header,
sanitizer, memory, differential, downstream, documentation, benchmark, and
hosted portability gates all remain pending.

## Completion criteria

- Every confirmed defect has a focused regression and a reviewed fix.
- Changed C translation units pass the strict C99 warning sets.
- Registered routines, headers, `src/init.c`, and the native coverage ledger
  agree.
- Directly affected tests, adversarial probes, and relevant GCT/sanitizer or
  Valgrind slices pass from a clean staged source.
- One complete Paradox unit suite passes after the focused fixes converge.
- The final diff receives an independent cross-review for GC safety,
  transaction semantics, compatibility, and accepted-path cost.
