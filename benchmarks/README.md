# Paired performance benchmarks

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
validation. `condition_equal_vector` isolates the vector comparison used by
dependency masking in `Design`, including its public native admission cost. A
separate `test_constraint_dt` workload times an already-admitted
batch and a tiny real callback. It therefore exposes row-adapter and callback
snapshot overhead instead of hiding the native batch evaluator behind repeated
Domain validation.
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
selection option and fails unless the repository is clean at the exported
candidate commit. It derives `library-baseline` from a sealed, passing,
full-inventory differential run, authenticates the candidate installed by the
current `compat/install-candidate`, requires that differential evidence to bind
the exact current `.local/R/library` content and fingerprint helper, runs every
ordinary workload, and runs the focused ParamSetCollection consumer probe once
per installation:

```sh
. scripts/activate

benchmarks/release \
  --baseline-evidence .local/compat/differential/runs/DIFFERENTIAL_RUN \
  --candidate-library .local/compat/runs/CANDIDATE_RUN/library-candidate \
  --dependency-library .local/compat/R/library-dependencies \
  --mies-library .local/compat/R/library-mies-diagnose \
  --output .local/benchmarks/release-YYYYMMDDTHHMMSSZ
```

The five `PARADOX_CANDIDATE_*` variables written for the immutable candidate
workflow must still be exported: `RUN_ID`, `REF`, `COMMIT`, `TREE`, and
`CONTENT_SHA256`. `--dependency-library` is repeatable and the first occurrence
must be the canonical dependency library and content recorded when the
candidate was installed, as well as provide `mlr3pipelines` for the focused
worker. Use repeatable
`--protected-library` arguments for any additional read-only libraries reachable
by package loading. The focused legacy worker also names `.local/R/library`
directly; the release gate therefore protects and fingerprints that library
automatically. All baseline, candidate, dependency, miesmuschel, additional,
ordinary project, and repository-local R base libraries are fingerprinted
before and after the gate. All user-supplied library roots must be distinct,
disjoint, plain directories below this repository's `.local/` tree.

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

Common constructors, ID/value/domain access, validation, design generation,
mutation, and maintained consumer paths use the stricter `hot` tier. Structural
stress, nested graph traversal, and callback-bearing cases use `standard`.
The Shadow constructor and value read/write paths are `hot`; complete live
Domain reconstruction is `standard` because it is a structural materialization.
Direct `ParamSetCollection$values` reads now traverse only the authoritative
capsule graph; they no longer receive a special budget for authenticating R6
wrappers or private tables. These are portable relative budgets; the policy
assumes no processor model, instruction set, or absolute nanosecond target.

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
clear timing failure, and both harmless and material zero-baseline allocation
changes:

```sh
Rscript --vanilla benchmarks/tests/test-regression-policy.R
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
run the dedicated worker once for each immutable package installation:

```sh
Rscript benchmarks/paramsetcollection-consumers.R \
  candidate .local/compat/runs/RUN/library-candidate consumer-candidate.csv
```

It measures `miesmuschel`'s `MutatorMaybe(MutatorGauss)` and `OptimizerMies`
parameter sets plus a two-node `mlr3pipelines` graph. Each object measures
`$params`, `$values`, and the common
`$get_values(check_required = FALSE)` read path. Optional fourth and fifth
arguments select the miesmuschel and shared dependency libraries when their
compatibility-run locations differ from the defaults. An optional sixth
argument selects the raw-sample CSV; otherwise it is written beside the summary
as `<summary-name>-samples.csv`.

For the collection initializer itself, the focused worker adds plain and
metadata-rich 8-by-8 and 32-by-8 synthetic collections to those three retained
consumer shapes. Run it in separate processes against the pinned baseline and
candidate, then compare the emitted CSV files:

```sh
Rscript benchmarks/paramsetcollection-construction.R \
  baseline .local/checks/psc-constructor/baseline-library baseline.csv
Rscript benchmarks/paramsetcollection-construction.R \
  candidate .local/checks/psc-constructor/candidate-library candidate.csv
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
  baseline .local/checks/value-mutation/baseline-library baseline.csv
Rscript benchmarks/value-mutation.R \
  candidate .local/checks/value-mutation/candidate-library candidate.csv
```
