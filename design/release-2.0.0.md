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
| Real R 4.3.3 and 4.5.2 runtimes | `release-runtime-matrix-20260716T172036Z` | Passed; verifier replayed with restricted `PATH` |
| Behavioral differential, 17 cases | `20260716T172358Z-922316` | 10 equal, 7 exact reviewed differences, 0 unexpected |
| GCT, Valgrind, and bounded rchk | `release-memory-20260716T172507Z` | Passed and independently replayed |
| CRAN/Bioconductor P0/P1 hard dependency preparation | `release-reverse-dependencies-p1-afa5668-20260716` | Passed and sealed |
| GitHub P0/P1 hard dependency preparation | `release-consumers-p1-afa5668-20260716` | Passed and sealed |
| GitHub P0/P1 source tests | `release-consumers-p1-afa5668-20260716` | Pending final execution |
| CRAN/Bioconductor P0/P1 source checks | `release-reverse-p1-afa5668-20260716` | Pending final execution |
| Documentation, books, galleries, and serialized migration workloads | `release-documentation-afa5668-20260716` | Pending final execution |
| Full release benchmark inventory | `release-afa5668-20260716` | Pending final execution |

The differential differences are not broad allowlists: both complete result
fingerprints and their reasons are fixed in
`compat/differential/expected-differences.tsv`. They cover corrected empty
presence handling, repeated-ID subsetting, grouped sanitization, infinite
bounds, collection callbacks, and ID-filter order/type behavior.

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
- The seven reviewed behavior changes fix likely bugs rather than emulating
  accidental upstream failures. Everything else is expected to remain
  compatible, including internal object shapes demonstrably used by current
  consumers.

## Verification and performance policy

The repository/reverse/documentation runners reuse the one authenticated
candidate installation and content-addressed dependency/install caches. Their
outer scheduler admits at most four 8-GiB consumer rows on this host and
rechecks live CPU/memory headroom before every wave. Nested work is normally
single-threaded. The GitHub `mlr3` row alone receives a receipt-bound two-CPU
exception for upstream tests that explicitly assert its worker contract;
make, CMake, testthat, BLAS, and Rcpp stay at one. All consumer and documentation
children force `LC_ALL=C.UTF-8`, `LANG=C.UTF-8`, `LANGUAGE=C`, and `TZ=UTC`.

Discretionary performance work is frozen. Reopen native code only if the final
full benchmark exposes a clear release-relevant regression with a low-risk,
measured fix. A source edit requires a new candidate and proportionate replay
of every gate that consumes its package bytes; a verifier-only repair does not.

## Release completion rule

Replace every pending row above with its sealed result, record benchmark ratios
and any accepted consumer-specific environmental limitation, replay the final
verifiers, confirm package-payload equivalence and a clean primary worktree,
then declare 2.0.0 complete. Do not turn a consumer failure into an allowlist:
first reproduce it against upstream paradox under the identical environment,
fix a harness artifact when proven, and add a package regression test for every
genuine candidate defect.
