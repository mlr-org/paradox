# Static getter performance, September 2026

## Plan recorded before implementation

The development baseline is commit `be2de62a` (the preceding review is now
committed). This is a new, bounded performance pass, not acceptance of the
older release candidate or a reason to restart the full release matrix.

The read-only `/home/mewse/results_lrz.db` compares Paradox 1.0.1
(`snapshot_2024_07_09`) with 2.0.0 (`snapshot_2026_08_07`). At 500 parameters,
`class`, `lower`, and `upper` are about 3--7 times slower in the latter;
`levels` also regresses. The database does not identify exact package commits
or compiler flags, so its ratios motivate workloads rather than certify the
current source. Absolute units must not be inferred from column names alone.

1. Retain a separate installed baseline and measure numeric, categorical, and
   mixed BASE schemas at 0, 5, 50, and 500 parameters. Include COLLECTION,
   nested COLLECTION, SHADOW, and collections containing a SHADOW. Time related
   static flags and a few unchanged read controls, and record allocations.
2. Profile the native property kernel and the R6/native entry path separately.
   Remove R wrapper/round-trip work and redundant per-operation work where the
   measurements justify it. Preserve a single implementation of each property
   rule and the existing raw-table kernel used by table projections/tests.
3. Optimize shared admission only where all existing checks still execute with
   equivalent meaning. In particular, investigate repeated closed-kind
   classification and allocation-heavy uniqueness checks for small level sets.
   Do not cache caller-owned or capsule validity across calls, lend outward
   names/columns, or make a property silently accept formerly corrupt state.
4. Extend focused regressions for properties, ownership of result names and
   typed leaves, mixed encodings, invalid storage, derived-schema refresh,
   clone/serialization, and forced collection. Keep R 3.6 and strict C99/C23
   source compatibility. Update registration/probes if an entry interface
   changes.
5. Benchmark each implementation slice against the baseline. Use warmed,
   fresh processes in both orders on one allowed CPU with nested threading
   disabled; retain timing distributions and allocations. Treat small effects
   comparable to order/thermal variation as inconclusive. Check adjacent
   workloads when shared code changes.
6. Run strict compilation, focused tests on current R and R 3.6, then the
   complete Paradox unit suite once after convergence. Do not run full
   downstream, hosted, runtime, or memory-analysis matrices during this pass.

Artifacts belong below `.local/getters-20260905/`. No installation outside the
repository, remote write, new dependency, or CPU-governor change is needed.
The detached-output and capsule/graph contracts remain normative.

## Implemented decisions

The subsequent [operation-specific validation pass](validation-policy-20260905.md)
supersedes the whole-schema admission and continued scalar-reduction validation
described in this historical implementation/measurement record. Its baseline
is this pass's final installed library; the measurements below are retained,
not retroactively relabeled as results of the newer policy.

- R6 static bindings enter `C_param_set_get_property` directly. Its only job
  before the existing table kernel is the authoritative capsule/derived-graph
  refresh and retention of that generation's parameter table itself. The
  table, not merely its replaceable parent field, is rooted before the first
  kernel allocation. Removing the old
  `.state()`/R `$`/second-native-call round trip saves fixed overhead without
  duplicating the property rules or changing the raw-table interface.
- Scalar `all_numeric`, `all_categorical`, and `all_bounded` share the existing
  vector-flag loop but allocate only the scalar answer. A false result does
  not skip validation of subsequent rows. Computed properties select their
  five input columns in one shared lookup instead of five complete scans;
  numeric/category flags no longer read unused per-row bounds or levels.
- `length` and `is_empty` use the shared exact canonical table-shape check,
  without an R `nrow()` round trip or per-parameter interpretation. These
  dimension queries did not previously interpret each Domain either.
- The parameter validator and computed properties reuse the constructor's
  existing interned closed-kind classifier, removing two duplicate string-
  classification implementations. The eleven fixed capsule labels likewise
  use interned identity with the exact byte-comparison fallback on every call;
  no capsule or table is remembered as permanently valid.
- Tiny uniqueness queries use at most 28 comparisons over at most eight
  native-ASCII strings of at most 64 bytes. Missing, longer, encoded, or larger
  inputs use `Rf_any_duplicated()` exactly as before, including the errors
  older R versions raise for some mixed bytes/text vectors. This is one shared
  native predicate for IDs, tags, and levels, not a second R implementation.
- Ordinary attribute-free semantic vectors up to one interrupt interval reuse
  the existing owned-payload copier, including its post-allocation shape and
  metadata receipt. Numeric payloads use its bulk copy. The general named,
  attributed, ALTREP, and long-vector paths keep their capture/interrupt rules.
  Already attribute-free level vectors no longer set an absent names attribute
  to `NULL` after copying. Selected outward property columns, IDs, and classes
  are independently rooted through allocation; retaining their parent table
  alone would not retain a child replaced by a pending finalizer.

No property cache, borrowed outward column, serialized field, public argument,
fallback engine, dependency, or downstream adaptation is added. Current
registration/probe coverage includes the new three-argument entry; historical
111-routine memory evidence still belongs only to the historical candidate.

## Profile and measurement method

Callgrind separates the whole R6 read, raw table kernel, and capsule refresh.
On the baseline's 500-factor `$lower` workload, complete parameter validation
accounts for about 85% of instructions; duplicate checking alone accounts for
38%, and repeated class/storage string classification for 18%. The final
output copy is not the principal cost. The first fused-entry slice saves
roughly 3--4 microseconds on small sets; the shared validation slice adds a
material wide-set improvement. Each installed slice and its raw measurements
is retained below `.local/getters-20260905/`.

The complete comparison uses `benchmarks/getters-20260905.R` and its separate
summary script. It covers 257 workloads on BASE schemas of 0, 5, 50, and 500
parameters, numeric/categorical/mixed kinds, COLLECTION, nested COLLECTION,
SHADOW, collections with a SHADOW, and adjacent constructors/reads. Each
workload has three blocks of 1,500 iterations, with result-key and allocation
checks. Fresh baseline/candidate/candidate/baseline processes share local
R 4.6.1, strict GNU C99 GCC 14.3 flags, dependencies, CPU affinity 6, and
single-threaded numeric libraries. No test, compilation, or profiler runs
concurrently. Reported times combine both orders geometrically. Recorded
zero-byte allocations do not include every small R pool or native stack slot.

A separate installed Paradox 1.0.1 uses the same current dependency library
and the same 50 column workloads. It is a local old-package comparison, not a
reconstruction of the machine/compiler behind `results_lrz.db`. All ordinary
result keys must agree across the three installations before timing is used.

The first complete paired run suggested small (0.3--0.7 microsecond) control-
read regressions. Fresh instruction profiles found no added control work;
the follow-up also identified the fixed capsule-name comparisons optimized
above. Retained `core-controls/` paired results show those costs recovered:
ordinary `has_deps`/small `ids` are flat or faster, while raw/get-values and
constructors are approximately unchanged. The final complete comparison is
separate from these diagnostic attempts; no earlier result is overwritten.

## Final results and verification

Final complete raw process blocks and summaries are in
`.local/getters-20260905/final/`. Both order comparisons agree on the ordinary
result keys, including the separate Paradox-1 runs. Representative mixed-set
results against `be2de62a` are:

| Property/workload | Parameters | Before, microseconds | After, microseconds | Speedup |
| --- | ---: | ---: | ---: | ---: |
| BASE `$class` | 5 | 11.14 | 7.72 | 1.44x |
| BASE `$lower` | 5 | 11.21 | 7.86 | 1.43x |
| BASE `$lower` | 50 | 19.21 | 14.18 | 1.35x |
| BASE `$class` | 500 | 92.09 | 72.04 | 1.28x |
| BASE `$lower` | 500 | 90.90 | 69.21 | 1.31x |
| BASE `$upper` | 500 | 91.32 | 70.05 | 1.30x |
| BASE `$levels` | 500 | 137.56 | 109.64 | 1.25x |
| BASE `$all_numeric` | 500 | 40.09 | 11.59 | 3.46x |
| BASE `$length` | 500 | 12.82 | 5.59 | 2.29x |
| COLLECTION `$lower`, two 500-parameter children | 1,000 | 167.83 | 129.80 | 1.29x |
| SHADOW `$lower`, 500-parameter origin, one hidden ID | 499 | 180.16 | 140.77 | 1.28x |

Across numeric/categorical/mixed schemas, the three requested getters improve
1.43--1.45x at five parameters, 1.32--1.37x at fifty, and 1.19--1.35x at five
hundred. Related derived reads improve in both orders. No measured workload
increases recorded allocation; the 500-row scalar `all_numeric` removes the
6,096-byte intermediate vector/names allocation reported by `bench`. No case
is more than 3% slower in both orders. The smallest combined speedup is 0.983
for five-parameter mixed `$ids()`, with one order exactly flat; the dedicated
control comparison and unchanged/lower instruction counts do not support a
material regression. Raw/get-values and constructors remain approximately
flat, not claimed to have mathematically identical timing.

For the local Paradox-1 comparison, five-parameter mixed primary getters now
take 7.7--7.9 microseconds versus 14.4--14.5 for Paradox 1; at fifty parameters
they are about 7% faster. At five hundred parameters they still take 69--72
microseconds versus 14.5 for Paradox 1, about five times slower. The level-list
getter is about 7.6 times slower there. These remaining costs are not hidden
by reporting only speedups against the current development baseline.

The complete comparison's baseline and optimized DSO SHA-256 values are
respectively
`58dae05c8b618d3e1fa85dd0ee3e309545f86a95ec3489d1b54a455361726309`
and `3537752893d8a2f39d9347cf7b74e50fa183391f60dc684ac6e4f2c5b1440b38`.
The `comparison.tsv` and `comparison-v1.tsv` SHA-256 values are respectively
`4d764ca35c243608513e6aa6c15edf9e0c517eb3db3db11287c370cc1d229787`
and `344b236ea9963c33c269e597e2e96e98885e3b58cb94a023ff18dba3d3976985`.
These hashes locate development measurements; they are not a release seal.

The final code-review correction roots the selected `.params` table rather
than its parent payload in the new native entry. The table is the kernel's
only input from that generation, and a finalizer could replace the parent's
field. This changes which object occupies the existing protection slot, not
the number of protections, allocations, validations, or copies. The compiled
wrapper has the same instructions and calls with `VECTOR_ELT` and
`Rf_protect` in the corrected order. Final DSO SHA-256 is
`38951c6d9b4fa5456f0d2e4766873cb74d3bd071e5dee769464838d3fb9d1f71`.

The twelve-case `root-followup/` comparison initially hit a broad power-state
shift within its last baseline process: one order was near flat while the
other moved almost every workload together by 25%. It is retained as noisy
diagnostic evidence. Exactly that small comparison was repeated with a
ten-second CPU warmup (`PARADOX_BENCH_WARMUP_SECONDS=10`) and no other active
test/build work. `root-followup-warm/comparison.tsv`, SHA-256
`98e24d5adc0a987e661fd81ce16d6307f4f8f2fa56be8b8b171b8040366b806e`,
has unchanged results/allocations and combined speedups 1.001--1.083 over the
pre-correction DSO. The largest cases are approximately flat. This bounds the
last ownership correction's cost; the conservative complete-comparison table
above is not inflated by multiplying in these small follow-up gains.

Verification:

- Strict GCC 14.3 GNU C99 installation, Clang 22 GNU C99 syntax checks, and
  explicit C23 syntax checks under GCC 15.2 and Clang 22 pass without warnings,
  including the final wrapper correction. No R API exception was added; the
  final 56-file public-API token audit passes.
- The complete integrated R 4.6.1 unit suite passes 10,646 expectations in
  116 files and 1,136 blocks, with one expected old-R-only capability skip and
  no failures, errors, or warnings. Eight isolated workers run 115 tasks with
  `NOT_CRAN=true`, the retained historical migration objects, and both cached
  ConfigSpace fixtures. `full-tests.tsv` has SHA-256
  `c7ba2fb60922441b53fe53d4458a319540e3f9eea5579bb8e860d1b4c46e40c0`.
  It explicitly identifies the pre-final-wrapper DSO `353775...` above.
- After that root-only correction, all ten affected test files pass again
  under both actual R 4.6.1 and R 3.6.3, including the direct new entry's forced
  collection tests for computed properties, numeric columns, level lists, and
  SHADOW output. Current R retains its one old-R capability skip; R 3.6 retains
  two unavailable list-ALTREP-fixture skips. No warning/error is accepted.
  Logs are `logs/focused-root.log` and `logs/focused-r36-root.log`. This focused
  final-delta check avoids repeating the just-passed complete suite for a
  change to the one existing protection slot.
- Final plain native probes pass all 236 records and four included hazards;
  `native-probes-root.tsv` has SHA-256
  `aeaf3fe97cec05c541e05f9ee6546f28cae15e2b48e2d667e5ae06aed2ca9b3c`.
  New regressions also cover all property selectors, scalar reductions on
  empty/mixed sets, validation of a bad trailing row after a false aggregate,
  refreshed nested/Shadow schemas, clone/serialization, every capsule label,
  detached ownership, and short/long/mixed-encoding uniqueness boundaries.

The full release/reverse-dependency/runtime/memory-analysis matrices were not
rerun. The older immutable candidate's acceptance is unchanged and is not
transferred to these new package bytes.

## Remaining cost and possible next decision

Complete parameter admission remains deliberately unchanged for detached
`class`, bound, storage, level, special-value, and default properties. Thus a
`$lower` read still checks other columns, every factor's levels, and special-
value structure. The 500-parameter getters remain slower than Paradox 1's
cheap internal-column access despite this pass's gains. Lending internal
mutable columns would violate the ownership contract and is not proposed.

The next substantial option is operation-specific read admission: check the
table shell, selected columns and their semantic meaning, rather than reject
unrelated private-field corruption during every read. This can retain safe
indexing, detached output, graph refresh, and complete checking/mutation
admission. It would nevertheless change when unrelated malformed private
state is diagnosed, so it is a separate explicit design choice, not a hidden
shortcut in this pass. A nonblocking question was sent to the user; absent
approval, the existing validation remains.

Two other visible remaining R loops are `$default`'s `is_nodefault` filtering
and `$is_logscale`'s cargo traversal. Their 500-row costs are substantially
larger than the column copies. Native versions should share the existing
marker/cargo rules and preserve opaque default identity, partial-name behavior
where applicable, and detached names. They are not silently bundled into this
bounded column/admission pass.
