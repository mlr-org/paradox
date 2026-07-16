# paradox 2.0.0 release ledger

This is the internal, append-only handoff for the native 2.0.0 release. It is
excluded from the package tarball. A row is release evidence only when its
retained verifier passes against the immutable candidate identity below; an
old diagnostic run, an unsealed run, or a successful command without retained
provenance is not equivalent evidence.

## Frozen package identity

- Full ref: `refs/paradox-release/candidate-20260716T180137Z`
- Commit: `afa56689e4037ee14a75b32811686f563f95effe`
- Tree: `06a6a8eccaca02d30baf950aca7bc352a3aeaf5a`
- Installed package-content SHA-256:
  `b628565f839e2743e064fd42c042aec960b159919964b2469a2e66ec9869ee6a`
- Version: `2.0.0`
- Language/toolchain contract: portable C17 through the public R C API,
  R 4.3 or newer; no architecture-specific intrinsics.
- Detached source:
  `.local/compat/candidate-snapshots/afa56689e4037ee14a75b32811686f563f95effe`
- Run-local installation:
  `.local/compat/runs/release-consumers-p1-afa5668-20260716/library-candidate`

The native/API evidence source commit
`aa07cce11e4c99ff777fe374700ba565cfa6b345`, the runtime/differential source
commit `c2847df4d4af989d5b1329ff0187f082280e17b9`, and the frozen candidate have
identical package payloads. Their intervening changes are confined to
`.Rbuildignore`-excluded `scripts/`. Later primary-checkout changes are confined
to excluded `scripts/`, `compat/`, `design/`, and `AGENTS.md`. Those changes
invalidate only a harness whose authenticated bytes changed; they do not
justify rebuilding the installed candidate or rerunning package-byte-dependent
native, runtime, differential, or memory analyzers.

## Release evidence

| Gate | Retained ID | Status |
|---|---|---|
| Native compilers, analyzers, sanitizers, symbols, full tests, CRAN and depends-only checks | `release-native-20260716T165635Z` | Passed and independently replayed |
| Public R API, four header releases, 35 translation units, GCC and Clang | `release-r-api-20260716T171343Z` | Passed and independently replayed |
| Windows/Rtools and macOS ARM64 CI | Exact frozen-candidate remote run not yet available | Required external handoff before publication; the frozen ref is local only |
| Real R 4.3.3 and 4.5.2 runtimes | `release-runtime-matrix-20260716T172036Z` | Passed; verifier replayed with restricted `PATH` |
| Behavioral differential, 17 cases | `20260716T172358Z-922316` | 10 equal, 7 exact reviewed differences, 0 unexpected |
| GCT, Valgrind, and bounded rchk | `release-memory-20260716T172507Z` | Passed and independently replayed |
| CRAN/Bioconductor P0/P1 hard dependency preparation | `release-reverse-dependencies-p1-afa5668-20260716` | Passed and sealed |
| GitHub P0/P1 hard dependency preparation | `release-consumers-p1-afa5668-20260716` | Passed and sealed |
| GitHub P0/P1 source tests | `release-consumers-p1-afa5668-20260716` | Sealed: 20 passed, 7 classified non-candidate failures, 1 bounded timeout; both verifiers passed |
| CRAN/Bioconductor P0/P1 source checks | `release-reverse-p1-afa5668-20260716` | Pending final execution |
| Documentation, books, galleries, and serialized migration workloads | `release-documentation-afa5668-20260716` | Pending final execution |
| Full release benchmark inventory | `release-afa5668-20260716` | Pending final execution |

The differential differences are not broad allowlists: both complete result
fingerprints and their reasons are fixed in
`compat/differential/expected-differences.tsv`. They cover corrected empty
presence handling, repeated-ID subsetting, grouped sanitization, infinite
bounds, collection callbacks, and ID-filter order/type behavior.

### Classified GitHub consumer limitations

The source-checkout gate retains ordinary upstream failures as factual rows;
they are not relabeled as passes. The following observed non-green results have
independent evidence that they are outside Paradox 2.0.0:

- `mlr3tuningspaces`: 430 passes and one `expect_learner()` helper-scope error.
  The exact test and helper bytes fail identically with paradox 1.0.1.
- `mlr3cluster`: 3,669 passes; the one failure and two errors all require the
  Weka Package Manager `XMeans` plug-in absent from isolated `WEKA_HOME`. The
  exact error reproduces with paradox 1.0.1; its ClusterR warning does too.
- `mlr3filters`: 502 passes and three errors caused by installed CRAN
  mlr3pipelines 0.11.0 registering `FilterEnsemble` without prototype
  arguments. Candidate/baseline probes are identical; the pinned GitHub
  mlr3pipelines source contains the upstream fix and passed 76,848 expectations.
- `mlr3torch`: all 85 Paradox-facing expectations passed. The wider row is
  non-green because Lantern/libtorch is not provisioned and two external CIFAR
  mirrors timed out; its independent clone-hash failure reproduces byte-for-byte
  with paradox 1.0.1.
- `xplainfi`: 1,838 passes and 15 errors caused by unqualified `tgen()` in
  helpers whose source-load lexical environment does not import it. A focused
  paradox 1.0.1 probe reproduces the same error; all 15 sites share that helper.
- `mlr3forecast`: 981 passes. Its 41 help-index errors require an installed
  package although the checkout is source-loaded; its one source-loaded R6
  callback failure reproduces with paradox 1.0.1.
- `mlr3extralearners`: the huge optional-backend inventory reached the RWeka
  section before the fixed 60-minute deadline. Its factual timeout is retained;
  the runner was not restarted or granted an unbounded exception.
- `mlr3resampling`: 117 passes and 16 errors. Fifteen errors and all 658
  warnings are one `future` multisession cascade because the source-loaded
  package is unavailable to spawned workers; the remaining error requires
  `sbatch` on a non-SLURM host. There are no failed expectations and no Paradox
  failure signature.

These are bounded consumer/environment characterizations, not expected Paradox
differences and not permissions to ignore a future failure with another
signature.

## Engineering decisions and compatibility boundary

The detailed implementation contract is in `design/architecture.md`; the
observable compatibility decisions are in `design/compatibility.md`; ownership
and adversarial teardown requirements are in `design/validation.md`. The
release boundary is:

- Exact canonical built-in `Domain`, `ParamSet`, and `ParamSetCollection`
  surfaces enter registered C immediately. The public R6 classes, ordinary R
  values, error sequencing, callback frames, and data.table-compatible outward
  shapes remain the compatibility boundary.
- C constructs and traverses those canonical shapes without calling checkmate
  or data.table on hot paths. It authenticates callback-capable or replaceable
  R surfaces before using fast lanes and declines to the established R/S3 path
  when semantics cannot be preserved.
- Unknown third-party Domain classes retain the slow S3 fallback. There is no
  native third-party plug-in ABI. Adding another package-owned built-in type is
  supported as a deliberate cross-kernel maintainer change, not as one unsafe
  table entry.
- Every R object retained beyond a call has explicit ownership. Preserved roots
  have deterministic teardown; borrowed values are protected across allocation;
  callbacks never run from an allocation-free region that assumes no callback.
- The reviewed behavior fixes produce seven normalized differential deltas;
  they repair likely bugs rather than emulating accidental upstream failures.
  Everything else is expected to remain compatible, including internal object
  shapes demonstrably used by current consumers.

## Verification and performance policy

The repository/reverse/documentation runners reuse the one authenticated
candidate installation and content-addressed dependency/install caches. Their
outer scheduler admits at most four 8-GiB consumer rows on this host and
rechecks live CPU/memory headroom before every wave. Nested work is normally
single-threaded. The GitHub `mlr3` row alone receives a receipt-bound two-CPU
exception for upstream tests that explicitly assert its worker contract. The
reverse runner applies the same five-field projection only to the CRAN `mlr3`
R CMD check child; its installation and outer worker remain at one. In both
cases make, CMake, testthat, BLAS, and Rcpp stay at one. All consumer and
documentation children force `LC_ALL=C.UTF-8`, `LANG=C.UTF-8`, `LANGUAGE=C`,
and `TZ=UTC`. Documentation gates authenticate the same managed detached
candidate source at every workload boundary, so excluded primary-checkout work
cannot replace the package being documented.

Discretionary performance work is frozen. Reopen native code only if the final
full benchmark exposes a clear release-relevant regression with a low-risk,
measured fix. A source edit requires a new candidate and proportionate replay
of every gate that consumes its package bytes; a verifier-only repair does not.

## Release completion rule

Replace every pending row above with its sealed result, record benchmark ratios
and any accepted consumer-specific environmental limitation, replay the final
verifiers, confirm package-payload equivalence and a clean primary worktree,
and obtain exact-candidate Windows/Rtools and macOS ARM64 CI results before
declaring 2.0.0 publishable. Do not turn a consumer failure into an allowlist:
first reproduce it against upstream paradox under the identical environment,
fix a harness artifact when proven, and add a package regression test for every
genuine candidate defect.
