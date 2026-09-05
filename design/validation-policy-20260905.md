# Operation-specific validation, September 2026

## Authority and plan recorded before implementation

The user's September 2026 policy supersedes earlier requirements to diagnose
every form of private-state corruption or arbitrary native-generated malformed
representation. This is a new development pass after
`getter-performance-20260905.md`; the previous uncommitted work is retained.
Baseline installed DSO SHA-256 is
`38951c6d9b4fa5456f0d2e4766873cb74d3bd071e5dee769464838d3fb9d1f71`.
No old candidate's release acceptance transfers to new source bytes.

### Validation policy

1. Public arguments receive their documented validation and useful errors.
   C must remain safe for well-formed R objects with invalid values, types,
   lengths, attributes, or private members. Supported R operations, including
   ordinary by-reference R APIs, count as R-level mutation even when their
   implementation happens to be native. Native memory damage, invalid R object
   headers/spines, and ALTREP implementations violating R's rules are outside
   the supported input model.
2. Private state has no semantic validity guarantee after outside mutation.
   Revalidate only what the operation needs for safe access and its public
   behavior on correctly constructed objects. Do not scan unrelated private
   semantics solely to make an unrelated getter diagnose misuse sooner.
3. A package-owned value which user code has had no opportunity to alter is
   trusted internally. Share already-admitted views and internal kernels;
   do not replay public argument/schema validation between native producers
   and consumers. This is an operation-local ownership fact, not a persistent
   bit declaring an externally reachable object permanently safe.
4. Retain GC roots, safe size/index arithmetic, cycle handling where R can
   create cycles, and callback/reentrancy boundaries. Allocation through a
   callback-capable helper can run R, including its finalizers. Independently
   root retained selected children of user-reachable
   containers; retain snapshots and commit-conflict checks for public
   reentrant mutations. Never replace necessary lifetime protection with a
   claim that the parent was once valid.

### Implementation sequence

1. Preserve an installed/source baseline, record resource admission, and map
   validation ownership/callers across all production translation units and
   R wrappers. Classify checks as public arguments, R-level memory safety,
   private semantic diagnostics, or repeated checks on unexposed owned data.
2. Replace broad static-property admission with selected-column admission and
   one property kernel. Keep the capsule/derived refresh, safe source/name
   access, detached outward results, and opaque identity rules. Remove
   unrelated row/level/special-value validation and redundant column scans.
3. Follow the same principle through remaining hot readers, parameter/Domain
   admission, mutation/collection planners, and outward snapshot helpers.
   Prefer shared internal views and existing kernels over parallel fast/slow
   semantic engines. Document retained boundaries and any reviewed no-change
   decisions; do not remove a check merely because it contains `validate`.
4. Update contract/architecture/compatibility/maintainer notes and regression
   expectations. Add R-reachable malformed-input/private-edit cases, output
   ownership, supported ALTREP, callback mutation, and GC tests for each
   changed boundary. Native-only fixtures may still exercise cheap guards,
   but their arbitrary behavior does not prescribe expensive production work.
5. Benchmark each coherent change with already-installed libraries, identical
   compiler/dependencies, fresh A/B/B/A processes, warmed CPU and one affinity.
   Cover static and derived getters plus adjacent constructors, checks, values,
   designs and samplers where their shared implementation changes. Investigate
   order-sensitive results rather than tuning against a single noisy median.
6. Batch focused tests and strict C99/C23 builds; test actual R 3.6 as well as
   current R. After convergence run the complete Paradox unit suite once and
   bounded memory checks appropriate to changed ownership/access paths. Do not
   restart the full release, downstream, hosted, or supported-runtime matrix.

Reuse the local toolchains, dependency libraries, compiled unchanged units,
and test fixtures. New artifacts belong under `.local/validation-20260905/`.
No remote writes, global installations, or cache deletion are authorized or
needed. Record findings, changes, benchmarks, and verification below.

## Review and implementation ledger

### Changed boundaries

- `properties.c`: selected-column admission for detached properties; class
  flags read no numeric/level columns; scalar reductions short-circuit;
  dimensions read only IDs. Selected children remain individually protected,
  and outward copies use the existing ownership kernel. The common column
  selector recognizes both the 11-column private layout and 16-column Domain
  layout without falling back to a name search.
- `paramset_domain_common.c`: replace the complete parameter-schema validator
  with `paradox_domain_read_params()`. It checks the fixed carrier width,
  requested column shells and lengths, and the integer row-count ceiling.
  It neither allocates nor classifies rows. Remove the old duplicate
  class/storage, ID-uniqueness, logical/factor-level and special-list scan and
  its unused selected-row mode. Remove the unused plain-table snapshot helper.
- Reader masks: ID-only for dependency design mapping and mutation/value-write
  ownership; IDs/classes/defaults for `get_values`; IDs/defaults for trafo;
  IDs/classes for raw values; classes/bounds/levels for uniform sampling;
  classes/bounds/tolerance/levels/storage for quantiles. Whole-table copiers
  (`params`, `domains`, subset, collection flatten, Shadow refresh) still
  request every column shell because they copy every column.
- `paramset_check.c`: interpret factor/special-value carrier types when
  building check specs. Drop the preceding all-row numeric/cargo semantic
  replay. Value comparisons do not need integer-domain admission; quantile
  and sampling arithmetic still guard the bounds they narrow or index.
- `paramset_params.c` and collection construction: no private parameter-row
  name validation, since row counts come from admitted columns and outward
  headers are freshly built. Delete the duplicate row-name validator.
- Raw value detachment accepts an already admitted view (or an unexposed
  collector result), not a list which it revalidates. Trust the shape and
  integer indices of R's own `match()` result; keep the unknown-owner guard
  before indexing. R 4.6.1's retained `src/main/unique.c:HashLookup` and the
  bounded table size confirm the integer-result contract. Retain names,
  classes, IDs, and values independently during outward allocations.
- `get_values` retains its selected IDs, classes, defaults, value names and
  tag columns in its existing root plan. It uses that defaults carrier rather
  than reselecting the private table after allocations.
- `core_state.c`: ordinary private lookup checks the closed tag and ordinary
  eleven-slot payload, not its printable labels/extra attributes. Explicit
  capsule construction, cold cloning/migration admission, and derived graph
  validation retain their relevant checks. `new_core()` trusts the payload
  just constructed by its C callers; edge fields can remain externally
  reachable and are still guarded.
- `domain_construct.c`: logscale conversion records its known generated
  double kind directly, without rechecking its own kind/cargo intermediate.
  Final public-argument admission still checks defaults, init, requirements,
  callbacks, and cross-field rules.
- `r_utils.c`: freshly allocated unnamed sample/quantile/grid columns skip
  the named-column metadata normalizer. The registered public table finalizer
  also skips that scan after its own complete column copier has removed names.
  This uses one self-reference builder, not another data.table implementation.

### Review of retained boundaries

| Family | Why its remaining validation is retained |
|---|---|
| Public Domain constructors, `domain_row_admission`, Domain kernels, Conditions, TuneTokens | Externally supplied arguments and publicly mutable Domain/Condition data need semantic admission. The existing masked Domain adapter already has one semantic owner. Defaults/init and callback-dependent cross-field rules are not merely internal assertions. |
| IDs, matching and suggestions | Public filters and names need argument diagnostics and encoding-correct matching. Native allocation/index bounds remain necessary. Failure-only suggestion work does not affect successful calls. |
| Collection/Shadow topology, subset, mutation, generation receipts | Public add/subset/value/tag operations can change a shared graph, including during callbacks. Refresh, owner-index guards, traversal cycle detection, and transaction conflict checks remain substantive public behavior. A snapshot's existence is not permission to trust a later live generation. |
| Value checking, activity, constraints, transformations and callback detachment | Public values and callback results are untrusted inputs. Capture/evaluation order, typed vs opaque ownership, dependency index bounds, and commit conflicts stay intact. Parameter-schema semantics no longer run as a preliminary reader gate. |
| Grid, sampling, dependency planning, transpose | Public dimensions, products, sample counts, probability inputs, integer casts and array indices need guards. Newly built outputs and internal match results do not need another general ingress pass. |
| Metadata/ALTREP helpers, R API facade | Ordinary R can create by-reference edits and cyclic nested attribute/value graphs. Output ownership and GC rooting are not optional validation. Public supported ALTREP inputs still materialize through the existing adapter; no parallel R path is introduced. Cheap existing native-fixture guards may remain, but native-only pathologies cannot justify new hot-path scans. |
| Recursive migration and R6 shell admission | Migration is an explicit public/cold boundary over serialized caller graphs and arbitrary nested R objects. Environment/closure/attribute traversal, visit identity, legacy shape checks, and atomic conversion remain needed. This is not a hot getter's validation policy. |
| R wrappers | No new R type-checking engine or fallback was introduced. Language capture, public callback orchestration and established method forwarding remain as before. |

This pass is not a claim that every cheap assertion has been deleted. Its
principle is to remove measurable repeated admission and irrelevant semantic
scans without replacing the public argument/ownership architecture.

A sampling follow-up deliberately tested the difference between an allocation
and an R-evaluation boundary. Current and old R defer R finalizers until safe
evaluation points (or explicit `gc()`); allocating workspace alone did not
execute the armed R finalizer in the eight tested scheduling positions. The
retained R 4.6.1 `memory.c` confirms that immediate allocation-time finalization
is behind the non-default `IMMEDIATE_FINALIZERS` build macro. The proposed
extra sampler levels snapshot was therefore withdrawn rather than charging
ordinary calls for an unconfirmed R-level path. Its temporary build and failed
timing-of-finalization assertion remain diagnostic artifacts in `final-library`
and the `*-r3` sampling logs. The final regression explicitly runs the R
finalizer in the public count's stable ALTREP callback and proves that the
changed column is admitted afterwards. The fixture changes neither ALTREP
length nor values. Existing roots and public callback/transaction checks are
retained; this experiment does not authorize dropping them elsewhere.

### Tests and measurements

The first getter-only A/B/B/A run is retained at
`.local/validation-20260905/getters-benchmark/`. It compares 25 cases against
the installed pre-policy baseline, using one CPU, 10-second warmups and three
1,500-sample blocks per fresh process. On the categorical 500-parameter cases,
class access improves 6.1x and lower/upper access 7.4x; both execution orders
agree on the gain. These are development measurements, not release evidence.
Final integrated benchmarks and verification are recorded below.

#### Verification

The final installed development DSO is
`b11972e8cc9732a0ff716e1432545dc773ab53190711772788bd9c2678f2e66d`.
After the withdrawn sampling experiment, reinstalling the final source
reproduces those exact bytes. Its complete Paradox unit run uses eight isolated
workers: 117 files, 1,141 blocks, 10,907 passes, eleven obsolete private-semantic
error expectations, one expected skip, and no errors or warnings. Those eleven
expectations concern copied private class/storage labels and unused private
table headers in four files. After reviewing and updating them, all four files
pass on both R 4.6.1 and R 3.6.3. Do not relabel the original nonzero run green;
the unchanged DSO plus these focused reruns cover the test-only correction.

New `test-operation-validation.R` supplies 398 expectations, including
selected/ignored columns, wrong-type and wrong-length R values across twelve
native readers, nested factor/special-value carriers, unchanged public
argument errors, and the R callback/finalizer case. The final three-file
operation/sampler/GCT run passes all 654 expectations without skips or
warnings; the corresponding operation/sampler files also pass on actual
R 3.6.3. Earlier ten-file R 3.6.3 coverage includes values, transformations,
properties, core state, public ownership, Shadow and GC tests, with only the
three existing unavailable-list-ALTREP skips.

Strict GCC 14 GNU C99 installation and all-source Clang GNU C99, Clang 22 C23,
and GCC 15.2 C23 warning-as-error checks pass. The R C API audit passes. All
112 registered routines and four dynamic lifetime/callback scenarios pass the
236-record native probe run and its separate validator. Logs, full-suite
ledger, focused results, and compiler commands are retained below
`.local/validation-20260905/`. No complete release, reverse-dependency, hosted,
supported-runtime, or rchk matrix was rerun in this development pass.

Instrumented R 4.6.1 under Valgrind 3.27.1 completes the final operation and
sampler files with all 594 expectations passing, no skips, and zero memory
errors, definite/indirect/possible losses or suppressions. The preceding
four-file memory run also reports zero errors/losses/suppressions across
properties, output ownership, malformed storage and the new operation tests;
its only test failure was an overstrict new expectation that an empty private
ID column must make an empty tag getter error. That expectation was removed
from the consumed-column matrix, not enforced in production. Both actual-R
memory logs identify the instrumented `lib/R/bin/exec/R` executable and the
exact candidate DSO. Initial missing-loader-symbol and shell-launcher attempts
are retained as setup diagnostics, not memory-check acceptance. The final
focused logs are `valgrind-tests-r4.log` and `valgrind-memory-r4.2.log`.

#### Integrated performance results

The completed comparison covers 257 getter/control workloads and 43
reader/design workloads. Both complete A/B/B/A runs use the already installed
pre-policy and final libraries, CPU 6, one numeric-library thread, ten-second
warmups, three blocks per workload, and respectively 500 and 200 samples per
block. There are no concurrent tests, builds or profilers. All 300 result keys
agree across the installations, and no workload increases recorded allocation.
The final directories are `final-getters-benchmark-r2/` and
`final-readers-benchmark/` below `.local/validation-20260905/`.

| Workload | Parameters | Before (microseconds) | After (microseconds) | Speedup |
| --- | ---: | ---: | ---: | ---: |
| Mixed `$lower` | 5 | 8.78 | 6.52 | 1.35x |
| Categorical `$class` | 500 | 92.26 | 18.96 | 4.87x |
| Mixed `$lower` | 500 | 73.72 | 15.68 | 4.70x |
| Categorical `$upper` | 500 | 88.89 | 15.57 | 5.71x |
| Mixed collection `$lower` | 1,000 | 133.52 | 23.81 | 5.61x |
| Mixed Shadow `$lower` | 499 visible | 148.45 | 32.89 | 4.51x |
| Categorical `$values` | 500 | 199.06 | 120.41 | 1.65x |
| Categorical `$get_values()` | 500 | 196.31 | 121.16 | 1.62x |
| Categorical `$trafo()` | 500 | 113.77 | 42.48 | 2.68x |
| Categorical `$tags` | 500 | 114.53 | 38.09 | 3.01x |
| Categorical `$qunif()` | 500 | 225.53 | 152.07 | 1.48x |
| Categorical random design | 500 | 2,042.53 | 1,803.61 | 1.13x |

Across the three built-in schema mixes, the requested class/lower/upper reads
improve 3.38--5.71x at 500 parameters. Their recorded allocation falls from
12,240 to 8,096 bytes per read. Raw values at that size fall from 34,816 to
26,528 bytes. Full Domain/parameter snapshots and complete 500-value checking
gain only about 1--6%, since their actual construction/check work remains.
The five-parameter numeric/categorical grids improve 1.08/1.10x; logscale
construction improves 1.06x. Random sampling uses an unpopulated independent
space so it measures free-dimension sampling, not only fixed-value expansion.

The initial broad run shows four `$is_logscale` rows 4--9% slower in both
orders, and some constructor controls have slightly negative combined ratios.
The separate nine-case `control-followup/` reruns these with 1,500 samples per
block and both process orders. Logscale medians then range from 1.3% faster to
2.1% slower; numeric/categorical wide-case direction reverses between orders.
The constructor controls remain about 0.7--2.6% slower, with unchanged
allocation. No follow-up case is over 3% slower in both orders. The R method
source is byte-identical to the baseline, `$is_logscale` still spends its work
in its existing R `map_lgl`, and its native lookup now does less validation.
No added work explaining the original large timing difference was identified.
The smaller differences are retained as measurement uncertainty, not described
as universal speedups or erased by the follow-up.

Two extra fresh Paradox-1.0.1 runs use the same current runtime/dependencies
and 18 matching class/lower/upper cases. Their result keys also agree. The
five-parameter getters are 2.47--2.69x faster than Paradox 1. At 500 parameters,
lower/upper are now 1.13--1.14x faster, while class remains approximately 7%
slower (about 19 versus 17.8 microseconds). The detached class-vector/name
ownership guarantee is preserved; whole-schema validation no longer explains
that remaining difference. This local comparison is not a reproduction of
the original machine/runtime behind `results_lrz.db`.

The first `final-getters-benchmark/` attempt is diagnostic only. Its baseline
TSV was written, but editing the benchmark's reader fixtures while Rscript
still had the source open caused a trailing parse error. Neither that attempt
nor the withdrawn sampler build supplies the final comparison. Both libraries
passed a 43-case fixture preflight before the fresh complete runs above; their
runner files then stayed unchanged. Final runner/summary SHA-256 values are
`4db9de3241df49bcdcf1e57adf9cf19d562ba53c122a72bb9aa37ffb89b1ff00`
and `c24c9652c381edaeae19bf04c45cb114d25f991662659dac5a67b208aad6e61b`.
All results here are development measurements, not a newly sealed release.
