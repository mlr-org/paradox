# Paired performance benchmarks

## Final pre-release development batch

The bounded implementation plan recorded before source changes is
`design/final-performance-implementation-plan.md`. Its immutable performance
baseline is `b2e1649884fe1478fd8f248cfefcd071f8278559b`; collection scratch
cleanup landed at `81cbccf` and the remaining implementation/tests/probes at
`387c1cd`. The ordinary integrated GCC DSO used for the final focused A/B pass
has SHA-256
`b664b29bca6e9ed6247fa17abe5f8a5942bd4d752e3d8f215f2482f353d17b9c`.
This is development evidence, not the eventual sealed release benchmark.

Every retained comparison used fresh R processes, one pinned CPU,
single-threaded numeric libraries, warmups, both baseline-first and
candidate-first orders, semantic result keys, and the geometric
forward/reverse ratio. Exact scripts, CSVs, logs, installed libraries, source
snapshots, and Callgrind outputs are below
`.local/perf/final-performance-implementation-20260726/`. Principal results
are:

- 512-parameter chain active/checked-raw/unchecked-raw getters improved
  16.4/18.0/18.2x versus `b2e1649`; the 128/512 chain/star set has geometric
  ratio 0.135. Checked-raw allocation fell from 75,784 to 44,080 bytes.
- `SamplerUnif$new()` at 64 parameters improved 2.42x and allocation fell from
  493,888 to 66,880 bytes. An isolated control of the unchanged public
  `Sampler1DUnif$new(input)` path found a balanced baseline/candidate ratio of
  0.9993 (95% interval 0.9955--1.0031), excluding a stable 1% regression.
- detached BASE/SHADOW dependency facades at 64 rows improved about 1.9x and
  saved 2,568 bytes; fresh Domain construction improved 1--16% across all
  representative kinds.
- collection graph timing has geometric ratio 0.968, with every 16/64-node
  case faster in both orders; fixed admission scratch fell 2,232 bytes at 16
  nodes and 7,008 bytes at 64/256 nodes.
- fusing dependency validation with RHS capture improved the already indexed
  getter another 36%, and one-pass exact Condition attributes another 11.5%;
  both were faster in every balanced target case without changing allocation.

The equivalent chain-512 Callgrind workload fell from about 908.2 million to
51.8 million instructions. The residual native cost is the single exact
dependency/Condition pass and exact parameter-table admission required by the
Paradox-2 integrity contract, not duplicated R/C logic. Strict GCC 14 and
Clang 22 C17 builds are retained at
`.local/checks/final-performance-batch-strict-20260726`; both promoted warnings
to errors. The 176-record exhaustive direct native probe ledger has no
failure.

`benchmarks/run` compares a pinned upstream installation with the candidate in
two separate, fresh R processes. It loads the supplied immutable installations
and does not rebuild either package for each process or workload. It never
changes the CPU governor or writes to a global R library. The default inputs
exercise calls repeatedly observed in
the mlr-org compatibility corpus: `p_*()` and `ps()` construction,
`ParamSet$new()`, filtered `ids()`, static properties, scalar and tabular
checking, sanitization, `qunif()`, `subset()`, `get_domain()`, and `trafo()`.
The retained `SamplerUnif$sample()` workload constructs its sampler once and
times repeated sampling, matching the optimizer-loop use in bbotk and
mlr3hyperband instead of conflating sampling with constructor cost.
Separate plain and dependency-bearing workloads time `ParamSet$subspaces()`
and `SamplerUnif$new()` themselves, preserving visibility into their one-time
construction and ownership costs.
Grid generation retains the ordinary four- and eight-dimensional mixed
Cartesian workloads and adds three output-sensitive cases. Five binary integer
axes at resolution 12 cover deduplication of realized values before expansion;
five fixed numeric axes cover singleton-axis collapse; and a seven-parameter
dependency chain covers branch pruning. Their validators compare the complete
small realized tables, including column types and established row order, rather
than accepting only the expected row count.
Two fixed 64-parameter dependency-chain workloads cover the dormant-value
contract directly. `set_values_deep_dependencies` performs a checked,
complete, all-active replacement, while `get_values_deep_dependencies`
filters a mixture of active and dormant payload values whose intervening
parents are supplied by defaults in Paradox 2. The pinned Paradox 1 fixture
materializes those control defaults explicitly and the timed tag filter omits
them, so both sides validate the same outward result while Paradox 2 still
pays its intended default-aware activity cost.
Five `ParamSetShadow` workloads retain the budget-view shape used by
miesmuschel: construction over a mixed 64-parameter origin, a live `$values`
read, the `shadow_constraint_live` hidden-value merge, live `$domains`
reconstruction, and a write-through `$values <-` assignment that must preserve
the hidden budget value. The read fixture is
changed through its origin after construction, while the write fixture is
independent and idempotent across warmup and timing. For the pinned Paradox 1
baseline, the constructor resolves miesmuschel's version-gated legacy
implementation; Paradox 2 resolves its package-owned generator. This compares
the two real migration endpoints rather than a benchmark-only proxy.
The scalar validation group separately times complete `$check()` and the
`check_dependencies` dependency-only workload, so removing the latter's former
R-side dependency walk remains visible without conflating it with Domain
validation. `has_deps_flags` measures the common dependency-presence gate over
BASE, COLLECTION, and live SHADOW nodes without conflating it with detached
dependency-table construction. `condition_equal_vector` isolates the vector
comparison used by dependency masking in `Design`, including its public native
admission cost. A separate `test_constraint_dt` workload times an
already-admitted batch and a tiny real callback. It therefore exposes
row-adapter and callback snapshot overhead instead of hiding the native batch
evaluator behind repeated Domain validation.

The retained focused `$has_deps` comparison is
`.local/benchmarks/has-deps-scalar-ab-final-20260719`. It used R 4.6.1,
single-threaded BLAS/OpenMP settings, separate fresh baseline and candidate R
processes, 64 parameters, 16 rows, seed 20260713, three untimed warmups, and 50
timed evaluations of the complete three-node expression. The exact libraries
were `.local/final-rc-focus-20260719` and
`.local/tmp/has-deps-focused-20260719/library`; their installation-manifest
MD5 fingerprints were `e2c1bd70d424230108c9a85ff73b49b2` and
`6f395ff7002b4c73066bc81d6f3698f4`. The corresponding native DSO SHA-256
values were `6311d2b661e50e61e6b85fb8c27b7f8c71fa33c6c13ab435c7e9b268044bf0f4`
and `96ac522fca06e4060944f79da0925dafad90a03a520ba5e69caa36962ab53e7f`.
For the candidate, the ordered SHA-256 manifest of `R/ParamSet.R`,
`src/paramset_collection_deps.c`, `src/paramset_domain_common.c`, `src/init.c`,
`src/paradox.h`, and `benchmarks/workloads.R` hashes to
`98e80aa6cf275cbb9a35905509483154fa9c1674cee51fb1214358f5b6eab62c`.
The median moved from 498.575 to 156.065 microseconds (3.195x), with no timed
GCs. The one-evaluation `Rprofmem` profile reported 12,688 bytes in 14 records
on both sides, so this evidence demonstrates a latency win, not an allocation
reduction. `metadata.json`, the raw samples, and both allocation traces retain
the complete audit trail.

The final public-table metadata hardening received a separate bounded
development audit before the replacement candidate was frozen. Four balanced,
pinned-core fresh-process pairs compared the superseded `4f28327` DSO
(`2ebe2483051ad41d4ce985b89c966de373afdb1b0e333400b47b4b4a09eb2a5e`)
with the hardened DSO
(`ce99a2896f9f4f69a6387ed3b2c9da040ef6f011999f7ae6c028c13d56cda8a2`).
Representative `check_dt`, `qunif`, `trafo`, Design transpose/dependency, wide
base-wrapper, and zero-column medians were flat to 2.8% faster. On deliberately
minimal one-row inputs, the strict classifier/count checks cost only about
0.6--1.1 microseconds for keyed/indexed data.tables. Ordinary paths retained
the same measured R-heap allocations; owning metadata for a wide lazy wrapper
added 568 bytes. A direct diagnostic put all three repeated `Rf_install()`
lookups below 79 ns per table and one complete classifier below 0.47
microseconds even for a keyed/indexed data.table. Caching those symbols or
threading a second classifier result through six operations would therefore
save less than half a microsecond while adding release-risk plumbing. No such
optimization was retained. The raw development evidence is
`.local/tmp/public-table-perf-robust.tsv` and
`.local/tmp/public-table-cache-perf.tsv`; it justifies the engineering decision
but does not replace the exact-candidate release benchmark.

The value group separates the default dependency-aware `$get_values()` call,
the callback-free `remove_dependencies = FALSE` case, and the ubiquitous
`tags = "train"` filter. It also measures default filtered getters on rich and
nested collections, rather than treating the cheaper `$values` binding as a
proxy for that work.
The domain extraction workloads cover first, middle, and last IDs as well as
the public `$domains` batch accessor, so a fast first-row lookup cannot hide
linear-scan or repeated-reconstruction costs.
The `$params` group includes an ordinary ParamSet plus plain, metadata-rich,
and nested exact ParamSetCollections. Collection fixtures use eight-parameter
children and retain live values and dependencies, matching shapes repeatedly
seen in the compatibility corpus.
The same fixtures measure collection construction, delegated values, filtered
getters, dependencies, whole-batch Domains, subset, and flatten operations.
The permanent write-path cases use an ordinary base `$set_values()` insertion
and an exact collection `$values <-` assignment; both verify stored value order
and contents after warmup and after the complete timed sample.
Dedicated callback-bearing subset and flatten variants retain live child
constraints and extra transformations, so the common callback-free case cannot
hide detachment overhead or a regression that reintroduces R-side callback
orchestration. Together with the Shadow read/write workloads, these cases keep
the temporary native ID index and the shared live/detached collection evaluator
visible in the normal paired report instead of relying only on one-off
profiling scripts. The small direct `to_tune(ParamSet)` wrapper probe remains a
development diagnostic; release claims come from these end-to-end workloads
and the complete paired gate.

For dependency aggregation specifically, retain both rich and nested shapes so
the result covers local rows, per-edge affixing, and recursive postorder
assembly:

```sh
benchmarks/run \
  --workloads collection_deps_rich,collection_deps_nested \
  --baseline-library .local/baseline-library \
  --candidate-library .local/R/library-dev
```

For a focused `ParamSetCollection$domains` comparison, retain all three exact
collection shapes rather than timing only a flat toy fixture:

```sh
benchmarks/run \
  --workloads collection_domains_plain,collection_domains_rich,collection_domains_nested \
  --baseline-library .local/baseline-library \
  --candidate-library .local/R/library-dev
```

The rich and nested validations cover names, order, facade classes, dependency
owners, tags, transformations, and initial values before timing. Run
`benchmarks/paramsetcollection-consumers.R` alongside them for the retained
miesmuschel and mlr3pipelines shapes; a synthetic collection speedup alone is
not sufficient evidence for the native batch path.

Bootstrap the local toolchain first. A normal paired run uses the baseline and
candidate installations produced by the compatibility setup:

```sh
benchmarks/run
```

The default candidate is the disposable development install at
`.local/R/library-dev`; final release measurements should explicitly select the
run-specific immutable candidate library produced by `compat/install-candidate`.
Run `--help` for all input-size and sampling controls, or `--list-workloads` to
select a smaller set:

```sh
benchmarks/run \
  --baseline-library .local/baseline-library \
  --candidate-library .local/compat/runs/RUN/library-candidate \
  --dependency-library .local/compat/R/library-dependencies \
  --dependency-library .local/R/library \
  --baseline-ref 06091b5b64a78807d332ec95c5cdc1aaac5899b9 \
  --params 64 --rows 128 --warmups 5 --iterations 100
```

## Sealed release gate

`benchmarks/run` remains the convenient development driver. Final release
evidence is instead produced by `benchmarks/release`, which has no workload
selection option and fails unless the validation repository is one clean,
committed tooling tree. That tree need not equal the frozen package candidate:
the candidate is selected by an exact profile/axis registry row and authenticated
through its managed detached source worktree. The gate derives
`library-baseline` from a sealed, passing,
full-inventory differential run, authenticates the candidate installed by the
current `compat/install-candidate`, requires that differential evidence to bind
the exact current `.local/R/library` content and fingerprint helper, runs every
ordinary workload, and runs the focused ParamSetCollection consumer probe once
per installation:

```sh
. scripts/activate

benchmarks/release \
  --baseline-evidence .local/compat/differential/runs/DIFFERENTIAL_RUN \
  --candidate-source .local/compat/candidate-snapshots/CANDIDATE_COMMIT \
  --candidate-library .local/compat/runs/CANDIDATE_RUN/library-candidate \
  --dependency-library .local/compat/R/library-dependencies \
  --evidence-profile release-refresh-20260720 \
  --paradox-axis paradox2 \
  --output .local/benchmarks/release-YYYYMMDDTHHMMSSZ
```

The five `PARADOX_CANDIDATE_*` variables written for the immutable candidate
workflow must still be exported: `RUN_ID`, `REF`, `COMMIT`, `TREE`, and
`CONTENT_SHA256`. `--dependency-library` is repeatable and the first occurrence
must be the canonical dependency library and content recorded when the
candidate was installed. Use repeatable
`--protected-library` arguments for any additional read-only libraries reachable
by package loading. `--candidate-source` must be the clean detached linked
worktree at `.local/compat/candidate-snapshots/<candidate-commit>`; candidate
differential helpers and the reproduced source archive are read from that exact
tree. The focused legacy worker also names `.local/R/library`
directly; the release gate therefore protects and fingerprints that library
automatically. All baseline, candidate, dependency, miesmuschel, additional,
ordinary project, and repository-local R base libraries are fingerprinted
before and after the gate. All user-supplied library roots must be distinct,
disjoint, plain directories below this repository's `.local/` tree.
The named evidence profile and axis derive the exact suffixed bridge library and
evidence paths; there is no caller-selected `--mies-library` seam. The profile
overlay supplies both the reviewed miesmuschel and mlr3pipelines builds to the
focused worker and remains ahead of mlr3verse or documentation extras in the
effective library order. Freeze the final validation-tooling commit first, then
construct one fresh named profile overlay with that exact commit and reuse it
read-only for documentation, full checks, and this benchmark. Normal verification
requires the overlay completion and retained inputs to match the current tooling.
It does not rebuild or substitute the default unsuffixed overlay.

The sealed benchmark records the current tooling commit, tree, and clean status
separately from the candidate ref/commit/tree. It retains the profile and axis
registries, the selected manifests, and the overlay's completion, package,
manifest, and seal records. This separation is required after freezing: the
registry row that names a candidate hash cannot already be part of that same
candidate commit, while the overlay is built only after the validation tooling
identity is final.

`--plan-only` performs the same provenance, Git, evidence, candidate, library,
workload-inventory, and regression-policy authentication and prints the exact
three commands plus the policy manifest hash without reserving a retained
output directory. A real run requires an absent output path below
`.local/benchmarks/`. It records separate stdout/stderr logs, raw paired and
consumer results, command arguments and environment, before/after content
fingerprints, the candidate and differential provenance, completion metadata,
the reviewed policy inputs, a per-case decision ledger, and copies of every
helper. Success seals the complete output with the common repository-evidence
format and verifies it again with the retained verifier:

```sh
Rscript --vanilla compat/verify-repository-evidence.R \
  .local/benchmarks/release-YYYYMMDDTHHMMSSZ
```

A failed command, material regression, or postcondition leaves an explicitly
failed, deliberately unsealed directory for diagnosis. Because the focused
consumer script has a fixed 100-iteration contract, `--iterations` controls the
all-workload paired run only. The release policy requires at least 50 paired
samples per workload; the default remains 100.

### Reviewed regression policy

[`regression-policy.tsv`](regression-policy.tsv) assigns every registered
paired workload and every focused consumer operation to exactly one reviewed
tier and records why it is protected. The gate rejects missing, extra,
duplicated, or reordered policy rows before starting a benchmark. Adding or
renaming a workload therefore requires an explicit policy review. The policy
algorithm and constants live in
[`regression-policy.R`](regression-policy.R); both files are copied into the
evidence and their individual SHA-256 values and combined policy-manifest hash
are recorded in completion metadata.

The thresholds are relative to the authenticated upstream baseline measured on
the same host:

| Tier | Median limit | Upper-quartile limit | Required probability candidate is slower | Allocation ratio | Minimum allocation increase |
|---|---:|---:|---:|---:|---:|
| `hot` | 1.20 | 1.35 | 0.75 | 1.25 | 16 KiB |
| `standard` | 1.35 | 1.60 | 0.80 | 1.50 | 64 KiB |
| `integrity-shadow-read` | 3.25 | 3.50 | 0.80 | 1.25 | 16 KiB |
| `integrity-collection-read` | 2.75 | 3.00 | 0.80 | 1.25 | 16 KiB |

Common constructors, direct ID/value/domain access, validation, design
generation, mutation, and maintained consumer hot paths use the stricter `hot`
tier. Structural stress, nested graph traversal, and callback-bearing cases use
`standard`. The retained consumers' integrity-validated live collection
`$values` reads use `integrity-collection-read`; their direct `$params` and
`$get_values(check_required = FALSE)` paths remain `hot`.
The two integrity tiers are deliberately narrower than a generic compatibility
waiver. `integrity-shadow-read` applies only to `shadow_values_live`.
`integrity-collection-read` applies to exactly six full collection-graph reads:
the three synthetic direct `collection_values_*` rows and the three exact
miesmuschel/mlr3pipelines consumer `$values` rows. A live Shadow read validates
its origin generation, signature, hidden-value merge, and visible schema; a
direct collection read admits the complete authoritative capsule DAG before
assembling its detached result. Paradox 1 did not provide those integrity
contracts, so its cached R surfaces are not an honest strict-latency budget for
the additional work.

The finite ceilings retain that major-version contract-reset cost in every
decision ledger, turn a material fraction of it into a visible `marginal`
review, and still reject a materially slower implementation.
They cover the measured optimized ratios with reviewable headroom and would
reject the retained pre-optimization nested collection stage. Shadow
construction, constraints, domains, and writes remain `hot` or `standard` as
listed. The three real miesmuschel/mlr3pipelines structural `$values` reads use
the existing `integrity-collection-read` ceiling because they expose the same
integrity-validated full collection-graph traversal as the three synthetic
reads; their `$params` and unchecked filtered getters, and every other retained
consumer operation, remain `hot`. Thus a synthetic safety boundary cannot hide
an end-to-end consumer regression, while every full graph read is held to the
same finite reviewed tier. These are portable same-host relative budgets: the
policy assumes no processor model, instruction set, or absolute nanosecond
target.
The exception is timing-only; both integrity tiers retain the strict `hot`
allocation ratio and minimum-byte thresholds.
After Paradox 2 becomes the authenticated baseline, review and normally retire
the contract-reset tiers instead of carrying their wider ratios forward.

Timing decisions use all retained samples, not only the summary median. For
both the median ratio and the 75th-percentile ratio, the evaluator computes
2,000 deterministic independent-sample bootstrap replicates. A timing metric
fails only when its observed ratio and its one-sided 95% lower bootstrap bound
both reach the tier limit and the empirical all-pairs probability that the
candidate is slower reaches the tier's probability limit. This conjunction
protects against material, distribution-wide slowdowns while avoiding failures
from a single timer outlier or a noisy median. The evaluator restores the
caller's random-number-generator state.

A case is `marginal` when a timing distribution consumes at least half of its
relative budget, its upper confidence bound crosses the full limit, or its
allocation increase consumes at least half of the allocation budget, without
meeting all failure criteria. Marginal cases do not fail the gate, but they are
printed and must be reviewed with the raw distributions before making a release
claim. This prevents normal host noise from creating brittle gates without
hiding an emerging regression.

Allocation profiles are independent of timed samples. An allocation failure
requires both the tier's relative increase and minimum byte increase. If the
baseline allocates zero bytes, the relative ratio is infinite but the minimum
byte increase still applies; zero-to-zero is recorded as ratio 1, and a tiny
zero-to-nonzero change is not automatically a failure. This handles native
zero-allocation paths without dividing by zero or treating a profiling record
as a material regression.

`metadata/regression-decisions.tsv` retains one explicit decision per policy
row, including sample counts, medians, upper quartiles, bootstrap bounds,
empirical slower probability, allocations, applicable thresholds, and reasons.
`metadata/completion.tsv` records pass/marginal/fail counts and the worst median,
upper-quartile, and allocation regressions. "Worst" is selected by the fraction
of the applicable tier budget consumed, so a tiny allocation above a zero-byte
baseline cannot hide a larger material increase. The raw ratio, byte delta, and
budget fraction are all retained. Any `fail` row prevents the evidence seal.
The deterministic policy fixtures cover stable passes, a noisy marginal, a
clear timing failure, both integrity-tier margins and a limit breach, and both
harmless and material zero-baseline allocation changes:

```sh
Rscript --vanilla benchmarks/tests/test-regression-policy.R
Rscript --vanilla benchmarks/tests/test-release-path-validation.R
Rscript --vanilla benchmarks/tests/test-worker-validation-diagnostics.R
```

For the development runner, supply `--baseline-ref` whenever the source
revision is known. The default is deliberately `unrecorded`, because Git `HEAD`
cannot prove which source created an older installed baseline. The installation
fingerprint in `metadata.json` identifies the exact package tree that was
measured.

The default output is a unique ignored directory below `.local/benchmarks/`.
An explicit `--output` directory must be empty. Each run contains:

- `metadata.json`: revisions, installation fingerprints and paths, package/R/
  compiler/platform details, session information, CPU model and governor,
  process-start/timing-start/after load averages, seed, inputs, warmups, and
  iterations;
- `samples-{baseline,candidate}.csv`: every elapsed-time and per-sample GC
  observation;
- `allocations-{baseline,candidate}.csv`: the raw `Rprofmem` allocation trace
  collected by `bench` for one independent evaluation of each workload;
- `summary-{baseline,candidate}.csv`: distribution quantiles, allocation
  totals, and GC totals; and
- `comparison.csv`: median speedup and allocation ratio. Ratios greater than
  one favor the candidate.

The focused consumer directory likewise retains
`{baseline,candidate}-samples.csv` beside its summaries. In the regression
decision ledger, ratios use the more natural regression orientation instead:
values greater than one mean that the candidate is slower or allocates more.

Every workload is evaluated and checked before timing, warmed up outside the
timed region, then checked again for fixture mutation. The driver also requires
the baseline and candidate validation keys to agree. Semantic bug regressions
remain in the differential/unit suites instead of being mixed into performance
measurements.

Performance runs are only interpretable on an otherwise idle machine. Keep the
raw distributions and metadata with any claimed result; do not report only the
compact ratios. The allocation profile is deliberately separate from the timed
distribution, because allocation instrumentation itself changes timings.

For the three retained consumer constructors used by the collection migration,
run the dedicated worker once for each immutable package installation. For
release evidence, `BRIDGE_LIBRARY` is the verified named-profile overlay (for
example
`.local/compat/runs/CANDIDATE_RUN/library-downstream-bridges-release-refresh-20260720-paradox2`),
not the historical unsuffixed default overlay:

```sh
Rscript benchmarks/paramsetcollection-consumers.R \
  candidate .local/compat/runs/CANDIDATE_RUN/library-candidate \
  consumer-candidate.csv \
  "$BRIDGE_LIBRARY" \
  .local/compat/R/library-dependencies
```

It measures `miesmuschel`'s `MutatorMaybe(MutatorGauss)` and `OptimizerMies`
parameter sets plus a two-node `mlr3pipelines` graph. Each object measures
`$params`, `$values`, and the common
`$get_values(check_required = FALSE)` read path. The fourth and fifth arguments
are mandatory: use the exact candidate-specific downstream bridge and protected
shared dependency libraries, respectively. An optional sixth argument selects
the raw-sample CSV; otherwise it is written beside the summary as
`<summary-name>-samples.csv`.

For the collection initializer itself, the focused worker adds plain and
metadata-rich 8-by-8 and 32-by-8 synthetic collections to those three retained
consumer shapes. Run it in separate processes against the pinned baseline and
candidate, then compare the emitted CSV files:

```sh
Rscript benchmarks/paramsetcollection-construction.R \
  baseline .local/checks/psc-constructor/baseline-library baseline.csv \
  "$BRIDGE_LIBRARY" \
  .local/compat/R/library-dependencies
Rscript benchmarks/paramsetcollection-construction.R \
  candidate .local/checks/psc-constructor/candidate-library candidate.csv \
  "$BRIDGE_LIBRARY" \
  .local/compat/R/library-dependencies
```

Child sets are constructed before timing. Each workload verifies public IDs,
tags, and child-reference identity before and after measurement; the candidate
CSV also records whether the exact fixture was admitted by the native helper.

For public value mutation, the focused worker measures `$set_values()` and
`$values <-` on small and 64-parameter shapes, then repeats replacement through
miesmuschel's maybe mutator and optimizer and an mlr3pipelines graph. Each
workload verifies the stored values before and after timing:

```sh
Rscript benchmarks/value-mutation.R \
  baseline .local/checks/value-mutation/baseline-library baseline.csv \
  "$BRIDGE_LIBRARY" \
  .local/compat/R/library-dependencies
Rscript benchmarks/value-mutation.R \
  candidate .local/checks/value-mutation/candidate-library candidate.csv \
  "$BRIDGE_LIBRARY" \
  .local/compat/R/library-dependencies
```
