# Final pre-release performance implementation plan

Status at creation (2026-07-26, package commit `b2e1649`): **approved for
implementation, not yet implemented**. Status after implementation:
**P1--P5 and two profile-led P1 follow-ups accepted at commits `81cbccf` and
`387c1cd`; focused development gates complete**. This document was written
before the first source change in this batch so that the measured
opportunities, compatibility obligations, and stop conditions could not drift
while the work was in progress. The original baselines and targets below are
therefore retained even where the result exceeded them. The full release
matrix remains intentionally deferred until the remaining source cleanup
converges.

This is the last planned performance batch before the replacement Paradox 2
release candidate is frozen.  It must preserve every behavioral and integrity
decision in `design/contract-first-2.0.0.md`; speed is not permission to add
persistent validation caches, trust caller-owned metadata, weaken capsule or
graph admission, change promise-forcing order, or change the detached-output
contract.  Development uses focused tests and paired benchmarks only.  The
full compatibility, memory, portability, and release benchmark matrices run
once after the remaining pre-release cleanup is complete, not during this
batch.

## 1. Baseline and measurement discipline

The immutable source baseline is commit
`b2e1649884fe1478fd8f248cfefcd071f8278559b`.  Preserve a separate installed
copy of that exact payload before implementation and compare each changed
installation against it in fresh R processes.  Every claimed gain must use:

- the same R runtime and dependency library;
- one pinned physical CPU with its SMT sibling left idle where possible;
- single-threaded BLAS, OpenMP, and data.table settings;
- warm-up iterations followed by repeated samples;
- both baseline-first and candidate-first orders, summarized by a balanced
  ratio rather than the favorable order;
- exact semantic result keys before timings are accepted;
- allocation measurements where the change is intended to remove copies; and
- workload sizes large enough to expose the predicted scaling behavior.

Small changes within approximately the observed one-percent order/thermal
drift are inconclusive.  A slice that does not deliver its predicted benefit,
or delivers it only in one order, is reverted unless it materially simplifies
the implementation.  Raw evidence belongs under the ignored
`.local/perf/final-performance-implementation-20260726/` tree.  Package-facing
source and normative documentation remain tracked.

## 2. Required implementation slices

### P1. Operation-local parameter-ID index for dependency work

Problem: dependency endpoint resolution in native `get_values()` performs two
linear parameter-ID scans per edge.  At 512 parameters this makes
`get_values(remove_dependencies = FALSE)` approximately 19--37 times slower
than Paradox 1 and gives current operations an avoidable near-quadratic slope.

Implementation:

1. Build one rooted, operation-local ID index after parameter admission.
2. Use pointer identity as the common CHARSXP fast path and preserve the
   existing encoding-aware string-equality owner for collisions/fallbacks.
3. Resolve both dependency endpoints in expected `O(P + D)` work while
   retaining source-row order and first-error order.
4. Reuse the index where the same operation currently repeats ID lookup.
5. When dependencies are not being removed and there are no required
   parameters, avoid activity calculation; this is an optimization only and
   must not skip condition/dependency validation.
6. If safe within the same rooted operation, consume the admitted value
   projection directly and detach only the outward result instead of copying
   and validating an intermediate list a second time.

Proof obligations:

- mixed-encoding IDs compare exactly as before;
- malformed, duplicate, missing, and reordered endpoints produce the same
  diagnostic family and deterministic first failure;
- required/default/dormant-value activity is unchanged;
- promise forcing and callback/reentry behavior are unchanged;
- final output remains detached and operation-entry capsule generations remain
  authenticated after allocation/callback-capable work.

Focused evidence:

- existing native `get_values`, dormant-values, dependency, required-value,
  encoding, corruption, finalizer/reentry, and gctorture-focused tests;
- chain and star schemas at 16, 64, 128, 256, and 512 parameters;
- raw checked, raw unchecked, active getter, dependency check, and checked
  assignment workloads;
- target: eliminate the v1 regression at 512 and restore approximately linear
  endpoint-resolution scaling without slowing ordinary small getters.

### P2. Single-use internal ownership handoff for `SamplerUnif`

Problem: `SamplerUnif` constructs fresh one-parameter subspaces and immediately
passes each to a public child-sampler constructor that defensively deep-clones
it.  Profiling attributes roughly 56% of 64-parameter construction time to
these redundant clones.

Implementation:

1. Add a package-private, unforgeable, single-use handoff path for freshly
   constructed subspaces owned solely by `SamplerUnif`.
2. Keep public `Sampler` and `Sampler1D` constructors' defensive-clone
   semantics unchanged.
3. Consume the handoff exactly once and reject malformed, reused, or
   externally fabricated carriers.
4. Do not alter the required clone of the complete input ParamSet, sampler
   class selection, child order, IDs, or serialized public topology.

Proof obligations:

- mutating the caller's ParamSet or any public constructor input cannot mutate
  a sampler;
- child samplers remain independent from one another;
- ordinary public construction, clone, serialization, and sampling results
  retain their existing structure and behavior;
- the internal path is not a user-visible constructor mode.

Focused evidence:

- sampler construction/clone/serialization/sampling tests plus targeted
  corruption tests for the carrier;
- exact classes/order/IDs and deterministic result keys;
- construction at 1, 8, 32, and 64 parameters;
- target: remove most child clone/copy time, with an expected 1.8--2.3x
  improvement at 64 parameters and no sampling regression.

### P3. Construct detached dependency data.table facades natively

Problem: the native BASE/SHADOW `$deps` accessor already creates a fresh,
detached dependency snapshot, after which R copies every column and invokes a
general defensive data.table finalizer.  At 64 parameters this makes BASE
`$deps` approximately 50 times slower than Paradox 1, although the new
detachment/integrity contract necessarily prevents v1's effectively free
return of internal state.

Implementation:

1. Complete the fresh BASE/SHADOW dependency result as a valid data.table
   facade in native code, following the already established collection
   accessor pattern.
2. Remove only the redundant R-level copy/finalization.
3. Preserve fresh `Condition` objects and detached columns on every call.

Proof obligations:

- valid data.table `class`, `row.names`, names, and `.internal.selfref`;
- mutation of the returned table, its columns, or a returned Condition cannot
  affect capsule state or another read;
- zero-row and mixed Condition/subclass-compatible representations remain as
  currently contracted;
- corruption and generation reauthentication are unchanged.

Focused evidence:

- native ParamSet and ParamSetCollection dependency tests, data.table
  characterization, detachment/Condition-isolation, corruption, and
  finalizer/reentry tests;
- 0, 1, 16, 64, and 256 dependency rows;
- target: remove the second facade copy and reduce the 64-row accessor from
  about 214 microseconds toward the native snapshot cost of 76--90
  microseconds.

### P4. Finish fresh Domain facades in the native constructor

Problem: `C_domain_construct()` owns a new sixteen-column shell, but R adds
attributes and crosses back into a general defensive native finalizer that
duplicates shell/metadata work.

Implementation:

1. Install the complete Domain data.table facade on the fresh object within
   the native constructor, reusing the canonical native attribute helper.
2. Remove the redundant R/C transition and defensive finalizer from the
   ordinary constructor path.
3. Do not weaken the public general finalizer used for caller-owned objects.

Proof obligations:

- byte/semantic equivalence of every public Domain column and attribute;
- correct zero/one-row encoding, row names, class, and self-reference on every
  supported data.table representation;
- constructor errors, callback admission, repr, srcref normalization, opaque
  leaves, and numeric edge cases are unchanged.

Focused evidence:

- native Domain construction and data.table characterization tests;
- representative `p_dbl`, `p_int`, `p_fct`, `p_lgl`, `p_uty`, logscale,
  special-value, callback, and keep-source cases;
- paired constructor benchmarks and allocations;
- target: a repeatable 5--15% improvement or a clearly material allocation
  reduction without an elapsed-time regression.

### P5. Low-risk collection-graph admission cleanup

Implementation candidates, in order:

1. preserve and reuse the first prior-node lookup instead of scanning the same
   prefix again in `initialize_node()`;
2. use inline graph/path/postorder capacity 16 rather than immediately growing
   the common 9--11-node fixtures from capacity 8;
3. select and validate `{private, core}` once per node when that can reuse the
   same semantic validator and rooted generation;
4. add a rooted immutable-capsule validator entry only if it shares one
   validation implementation with public ingress.

Only the first two are expected in this batch unless profiling makes the
others both obvious and compact.  Review stack consumption at every call site
that can hold two graphs before increasing inline storage.

Proof obligations:

- complete DAG admission, cycle/shared-DAG behavior, translation and permanent
  row validation, Shadow refresh, and final generation scans remain;
- outward values/dependencies remain detached and identically ordered;
- no persistent cache or trusted validation bit is introduced.

Focused evidence:

- collection values/dependencies/add/cycle/corruption/encoding/Shadow graph
  tests;
- plain, rich, nested, shared-DAG, and 16/64/256-child benchmarks;
- target: a reproducible 2--8% improvement or meaningful allocation reduction.
  Revert complexity that measures as neutral.

## 3. Opportunistic work and explicit exclusions

New improvements discovered while implementing P1--P5 may be included only
when profiling identifies a concrete owner, the change preserves the current
contract, focused A/B evidence is positive in both orders, and focused tests
cover its proof obligations.  Record each such change and its evidence in §5.

The following are intentionally excluded:

- persistent validation/translation caches, skipped graph checks, trusted
  data.table metadata, or weaker generation reauthentication;
- the previously neutral fused Shadow values reader;
- broad Design/activity-engine rewrites;
- LTO (the exact-source experiment was geometrically about 0.5% slower),
  `-O3`, `-Ofast`, fast-math, architecture-specific flags, PGO, manual SIMD,
  unity builds, or R-internal API use;
- a native srcref scanner unless all required slices are complete and a
  separately reviewed implementation is demonstrably simple.  Srcref handling
  is cold admission work, current Domain constructors still beat Paradox 1,
  and semantic risk is higher than P1--P4.

## 4. Implementation order and development gates

1. Freeze/install the `b2e1649` baseline and record machine/runtime metadata.
2. Implement P1; compile with the strict warning configuration; run its
   focused tests and A/B/scaling benchmarks.
3. Implement P2 independently; repeat its focused gates.
4. Implement P3 and P4 as separate reviewable changes and measure each.
5. Apply only positively measured P5 cleanup.
6. Run one combined focused test selection covering every touched subsystem,
   strict compiler/registration/static checks, and a small combined benchmark
   to detect interactions.
7. Update this document, `AGENTS.md`, architecture/release notes where the
   implementation boundary changed, and `NEWS.md` only for externally useful
   release facts.

Do **not** run the full reverse-dependency corpus, compatibility matrix,
Valgrind matrix, multi-R matrix, source-package checks, or sealed release
benchmark in this batch.  Those conclusions cannot transfer across the
remaining package changes and belong after final source convergence.

## 5. Results ledger

“Accepted” requires focused correctness plus balanced A/B evidence. Raw
results, scripts, exact installed libraries, source snapshots, logs, and DSO
hashes are retained below
`.local/perf/final-performance-implementation-20260726/`. Comparisons use the
order-balanced method in §1.

| Slice | Baseline | Target | Observed | Decision/evidence |
|---|---|---|---|---|
| P1 wide dependency lookup | chain-512 raw current/v1 19.3--36.7x slower | remove v1 regression; near-linear lookup | Versus `b2e1649`, chain-512 active/checked-raw/unchecked-raw moved from 5.070/5.101/5.019 ms to 0.309/0.283/0.276 ms (16.4/18.0/18.2x); star-512 moved 9.3--10.8x. The 12-case 128/512 chain/star geometric ratio is 0.135. Checked chain-512 is within about 6% of the retained v1 timing; unchecked remains about 2x v1 because v2 still performs full capsule, dependency, and Condition integrity admission. Scaling is approximately linear and small 1--64-parameter reads were neutral or faster. | **Accepted.** One operation-local pointer index with encoding-aware fallback resolves values, tags, and both edge endpoints; the admitted value projection is consumed directly; unnecessary activity is skipped without skipping admission. Chain-512 checked raw allocation fell 75,784 to 44,080 bytes. |
| P2 SamplerUnif handoff | 64-param construction about 158 ms | 1.8--2.3x faster | 64-parameter construction moved 143.76 to 59.31 ms (2.42x); allocation moved 493,888 to 66,880 bytes. Sizes 1/8/32/64 were faster in both orders. An isolated 12-process control of the ordinary public child constructor found baseline/candidate 0.9993 (95% interval 0.9955--1.0031), excluding a stable 1% regression. | **Accepted.** Fresh singleton states cross one sampler-specific process-local single-use carrier. Public constructors retain defensive cloning; malformed, reused, serialized, and generic carriers reject. |
| P3 native BASE/SHADOW deps facade | BASE 64/63 about 214 us | about 76--90 us | BASE 64 rows moved 157.5 to 80.7 us and SHADOW 204.3 to 105.9 us; both saved 2,568 bytes. At 256 rows both saved 9,480 bytes. All ten 0/1/16/64/256 cases were faster in both orders. | **Accepted.** Fresh detached snapshots are completed with the shared native facade helper; caller-owned tables still use the defensive finalizer. |
| P4 native Domain facade | constructor-specific | 5--15% or material allocation gain | All seven representative constructors were faster in both orders: common plain constructors about 9--11%, logscale integer 16%, factor 6%, callback-heavy cases 1--3%. Allocation was unchanged. | **Accepted.** The native constructor completes its fresh 16-column facade; R attaches only `repr`. |
| P5 collection cleanup | plain/rich/nested current baselines | 2--8% or allocation gain | Geometric timing ratio 0.968. All 16/64-node cases were faster in both orders; 256-node timing was mixed/noisy. Admission scratch fell by 2,232 bytes at 16 nodes and 7,008 bytes at 64/256 nodes for every workload. | **Accepted at `81cbccf`.** Inline capacity 16 and reuse of the first prior-node lookup provide a material fixed allocation reduction without another validator/cache path. |
| Opportunistic: dependency RHS fusion | P1 index implementation before fusion | profile-led and positive in both orders | All 12 128/512 chain/star getter cases improved in both orders; geometric ratio 0.638 (36% faster) with unchanged allocation. | **Accepted.** Exact dependency validation now returns its already admitted RHS array to the getter instead of validating every Condition twice. |
| Opportunistic: one-pass Condition attributes | RHS-fused implementation | profile-led and positive in both orders | All 12 cases improved in both orders; geometric ratio 0.885 (11.5% faster) with unchanged allocation. | **Accepted.** Exact Condition admission captures raw `names`/`class` in one versioned public-API traversal while retaining exact class, attribute, and shape checks. |

The chain-512 Callgrind workload fell from about 908.2 million instructions
before P1 to 51.8 million after the final two follow-ups, about 17.5x fewer.
The residual native profile is principally the one retained exact dependency/
Condition pass and the exact sixteen-column parameter-table admission. Removing
those would weaken v2 corrupt-state detection rather than remove duplicate
work, so this batch stops there.

Combined development verification used the exact `387c1cd` source: focused
getter/dormant/Condition/dependency, sampler, dependency-facade, Domain,
collection, Shadow, srcref, and data.table-characterization selections passed;
the exhaustive registered-routine probe ledger passed with 176 records and no
failure; and strict GCC 14 and Clang 22 C17 builds passed with warnings promoted
to errors. Retained compiler evidence is
`.local/checks/final-performance-batch-strict-20260726`. No full compatibility,
memory, multi-R, or release matrix was run in this batch, as required by §4.
