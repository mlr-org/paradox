# Completing operation-specific value and graph reads

## Plan recorded before implementation

This pass implements the follow-up to `validation-policy-20260905.md` and the
residual-performance review. The preceding uncommitted work is preserved.
Baseline source/install are `.local/validation-20260905/source` and
`.local/validation-20260905/library`; the baseline DSO SHA-256 is
`b11972e8cc9732a0ff716e1432545dc773ab53190711772788bd9c2678f2e66d`.
New development artifacts belong under `.local/graph-validation-20260905/`.

The public ownership contract is unchanged: outward names, interpreted
containers, and typed atomic leaves remain detached. This is not a lazy-output
redesign, a new public API, or permission to skip public argument validation.

1. Separate safe stored-value access from public/cold semantic admission. Do
   not recheck duplicate private names on each read. Share value-to-parameter
   mapping, recognize the ordinary matching-order case, and retain missing-row
   and bounds guards wherever native code indexes a selected carrier.
2. Use one graph traversal with explicit operation needs. Remove redundant
   parent/child schema-equality and name-reconstruction proofs from hot reads;
   keep the relevant column/edge shapes, lengths, owner bounds, cycle handling,
   and public refresh. Reuse resolved rows and fuse outward value collection
   with detachment rather than assembling, rematching, and copying twice.
   Preserve any genuinely cold/public semantic admission without creating a
   second graph or value execution engine.
3. Let an unchanged Shadow reuse its public-generation stamp. A stamp proves
   public freshness, never memory safety of user-reachable fields. Keep
   per-operation access checks and cold serialization/clone admission. Preserve
   the cheap schema-only epoch for Shadow-free Collections; do not introduce
   reverse-parent registries or a new serialized capsule layout. Move full
   Shadow-template reading to the branch that actually rebuilds it.
4. Review adjacent value/get_values/dependency readers for the same repeated
   private-semantic work and unexposed-intermediate replay. Prefer deletion,
   small read views, and shared kernels over multiple optimized variants.
5. Add focused public refresh, shared graph, serialization/clone, reordered
   values/encoding, detached-output, R-level malformed-field and callback/GC
   regressions. Adjust only expectations whose sole purpose was diagnosing
   unsupported private semantic edits. Audit all affected consumers of parsed
   views for type/length/index/lifetime assumptions.
6. Install stable staged source locally, reuse unchanged objects/dependencies,
   and benchmark against the installed baseline in warmed A/B/B/A processes.
   Include small/wide BASE, Collection, nested/shared Shadow reads and public
   mutation/refresh controls. Run focused current/old-R tests and strict
   C99/C23 builds, then the Paradox unit suite once after convergence and
   bounded native/GC/memory probes. Do not run the full release/downstream
   compatibility matrix.

## Implementation and verification

### Implementation

- The stored-value view admits list/name shapes without a second duplicate or
  missing-name scan. One mapper recognizes schema-order/prefix stores by
  interned name identity and otherwise uses R's encoding-correct matcher.
  Unknown rows still error before indexing. Classed names cannot enter that
  fallback: R's matcher can dispatch `as.character()` for POSIXlt, so trusting
  its result as an unexposed fixed-length row map would otherwise be unsound.
- A three-bit needs mask selects value, dependency, and full-schema views in
  the existing graph walker. Value/get_values/dependency/Domain/params reads
  no longer parse unused translation fields or prove parent/child semantic
  equality. Edge extents, consumed-column lengths, owner-index bounds where
  used, cycle handling, generation selection and epoch barriers remain.
- The graph retains its resolved row maps and emits public values with typed
  detachment in one pass. There is no intermediate list followed by another
  matcher/copy. Selected value/class carriers and root IDs have separate roots
  across leaf-copy callbacks; the ordinary internal collector avoids that
  additional root carrier. Opaque ParamUty leaves retain identity.
- A small dependency row view separates shape access from existing full
  Condition admission. `has_deps` does not interpret Conditions. Evaluation
  still admits selected Conditions/RHSs. Public writers are unchanged.
- The final adjacent-reader review uses topology-only graph views for
  Collection callback flags and the built-in uniform sampler. The sampler
  admits the same four root columns as its BASE path, retaining the existing
  specification capture, row bounds and RNG kernel. It does not inspect
  unused value stores, dependencies or translation tables.
- Shadow stamps now observe public state epochs, with full template reading
  deferred until an actual rebuild. Shadow-free Collections retain their
  schema-only stamps; Shadow-bearing Collections remain unstamped. This
  avoids a reverse-parent registry and changes no serialized capsule layout.
- Collection construction no longer repeats whole-graph admission after
  reading its selected child fields. Two now-unused read-only wrappers were
  deleted. Cold callback/clone/migration receipt paths continue to share the
  existing walker; no second graph execution engine was introduced.

Tests now distinguish required storage guards from obsolete expectations of
automatic private-signature repair or rejection of unused private fields.
New cases cover shared/nested public refresh, clone/serialization, private
name order and encoding, unknown owners, classed-name non-dispatch, and an
ALTREP leaf callback that changes public child values/schema and runs GC while
the outward reader retains its previously selected generation.

### Measurements and checks

The 42-case first paired run is retained under `benchmark/`. It measures the
main implementation before the last two topology-only consumers were changed:
wide raw values improve 1.58--1.69x, the live Shadow 1.90x, rich Collection
values 2.78x, and nested Collection values 3.55x. No case regresses by 10%.
The final paired run adds nine cases for those two consumers and owns the
terminal conclusions below; the first run is not relabeled as that payload.

Historical release evidence does not transfer to this working source; no
complete release or downstream matrix is requested.

Verification is staged to avoid replaying the whole inventory for each small
review correction:

- The 117-file, eight-worker unit run on intermediate DSO `d67e71d...` reached
  1,148 blocks and 11,096 passes with zero assertion failures or warnings.
  Its nonzero result is retained: twenty ConfigSpace errors came from missing
  fixture configuration/relative worker HOME, and five migration skips came
  from an unset fixture directory (plus one expected current-R capability
  skip). The relative-output bug in `run-native-tests.R` is fixed and covered
  by `test-native-test-batch`. No full run is relabeled green.
- On the subsequent build, the exact two ConfigSpace files pass 47
  expectations using the existing pinned current/old Python environments;
  the two migration files pass 847 with the retained mbo_config fixtures.
  The harness batch self-test passes, including the new relative-output HOME
  assertion and its existing worker/timeout/ledger checks.
- Current-R focused views/Shadow/Collection tests pass 1,168 expectations and
  the one expected old-R-only skip after moving the classed-name guard to the
  common stored-value reader. R 3.6.3 passes 1,173 expectations before that
  guard move, then all 476 operation-view expectations on the moved guard.
- Strict warnings-as-errors syntax checks pass for all C units under Clang
  GNU C99 and C23 and GCC C23. The main implementation passes all 112 native
  routines and four hazard probes under GCT (239 ledger rows, zero failures).
- The instrumented-R Valgrind run on `d67e71d...` completes the operation-view,
  Collection-value and ALTREP-lifetime files with zero memory errors, definite,
  indirect or possible losses, and zero suppressions. A missing local debug
  search path stopped the first attempt before R; its corrected rerun uses
  `.local/debug/libc6-dbg-2.31-0ubuntu9.18` and is retained separately. Old-R
  ad-hoc tests likewise unset `LC_ALL` before testthat changes language; the
  initial locale-only failure is retained, not counted as a package failure.
- The final two topology-only consumers pass their 1,243-expectation focused
  current-R set (one expected skip). Their terminal old-R, native, memory and
  paired measurement follow-ups are recorded below.

All artifacts are under `.local/graph-validation-20260905/`. The initial
`library` and `source` retain the broad-run implementation; `library-final`
and `source-final` select the terminal implementation. This is development
verification, not a new immutable release candidate or downstream acceptance.

### Terminal result

The terminal installed DSO is
`3ea9517e6773b919775f2d25a981ee3d957c4f6a7ae3668ac0f841c0a5dd764e`.
The final operation-view suite passes 486 expectations, including seeded
BASE/Collection sampling identity. Its R 3.6.3 follow-up with the sampler suite
passes 682. The terminal native inventory passes every routine and all four
hazards (237 plain-mode ledger rows). Strict Clang GNU C99/C23 and GCC C23
checks remain clean. The separate terminal instrumented-R Valgrind smoke
exercises 120 paired sampling/RNG cases at 0/1/8/64 parameters, callback flags,
irrelevant private stores and classed-name rejection: zero errors, all loss
categories zero, and no suppressions. This focused delta complements, rather
than relabels, the earlier broader GCT/Valgrind run.

`benchmark-final-r2/` contains the terminal 51-case A/B/B/A run: three
300-sample blocks per case/process, ten-second warmups, CPU 6, one numeric
thread, and no concurrent builds/tests. Every result key agrees across the
four processes. Sampling keys check shape, names and bounds; exact seeded
sample identity is tested separately as above. All 51 paired median point
estimates improve and none increases bench-observed allocation. Small control
differences are not claimed as genuine algorithmic improvements; ordinary
timing/power-state noise still applies. The preceding `benchmark-final/`
attempt failed on the baseline because its new sampling validator captured a
loop variable instead of the fixed parameter count; it is diagnostic only.

Representative balanced medians in microseconds, versus the **preceding
Paradox-2 development build**, not Paradox 1:

| Workload | Before | After | Speedup |
| --- | ---: | ---: | ---: |
| Numeric raw values, 500 parameters | 124.09 | 76.04 | 1.63x |
| Categorical raw values, 500 parameters | 119.94 | 69.25 | 1.73x |
| Live Shadow values, 64 origin parameters | 50.60 | 26.57 | 1.90x |
| Rich Collection values, 64 parameters | 109.79 | 39.74 | 2.76x |
| Nested Collection values, 64 parameters | 147.64 | 42.57 | 3.47x |
| Collection has_deps, small shared origin | 17.67 | 8.62 | 2.05x |
| Collection has_trafo, 500 parameters | 228.42 | 9.43 | 24.23x |
| Collection has_constraint, 500 parameters | 233.76 | 13.37 | 17.49x |
| Built-in Collection sampling, 500 parameters / 8 rows | 345.56 | 128.11 | 2.70x |
| Built-in Collection sampling, 5 parameters / 8 rows | 17.74 | 9.46 | 1.87x |

Public value refresh through a nested shared Shadow improves 1.26x; the
schema-only Collection control after a value write remains effectively
unchanged. Bench-observed allocations for the 500-value reads fall from
26,528 to 8,096 bytes, and nested Collection values from 10,392 to 1,352.
These profiler figures do not count every small R heap allocation.

The main Collection reader shrinks from 1,234 to 808 lines despite adding
operation needs and fused detachment. No API, serialized layout, ownership
rule, third-party fallback or persistent graph cache is added. The remaining
O(n) cost of detached output is intentional; this does not turn wide `$values`
or `$levels` into Paradox-1-style borrowed/lazy O(1) views.
