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
- Sealed source tarball:
  `.local/checks/release-native-20260716T165635Z/source-package/build/paradox_2.0.0.tar.gz`
- Source-tarball SHA-256:
  `60cb9cc2f9ccd1b75d1cbf72b4f935d9f5c510c83166dc67ea9dbf40c9ee074f`
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
to excluded `scripts/`, `compat/`, `benchmarks/`, `design/`, and `AGENTS.md`.
Those changes invalidate only a harness whose authenticated bytes changed; they
do not justify rerunning package-byte-dependent native, runtime, memory,
consumer, reverse-dependency, or documentation analyzers.

The final benchmark uses the authenticated harness companion
`refs/paradox-release/candidate-20260717T035858Z`, commit
`fe9bd8cea27e3418c16437fe62c1994f70476533`, tree
`eea6098d93896721afe15f87604f42772c1e4b8a`, and run-local installation
`.local/compat/runs/release-benchmark-final3-fe9bd8c-20260717/library-candidate`.
Its installed-tree fingerprint is
`4d3699199bf7e09eec4472aefd622ab34fba82323d55553dcafefcf4beecbb13`;
this is expected to differ because R installation embeds build time and source
location metadata. No package-bearing path differs from the frozen candidate,
their canonical 207-file `R CMD build` payload manifests match at
`cfea9374a31002a7fd04f3f00b4d34e9f36e584010e43674d10505d77bcc635c`,
and their native shared-library disassemblies are identical. The companion is
therefore evidence for the same distributable package source, not a second
package implementation.

## Release evidence

| Gate | Retained ID | Status |
|---|---|---|
| Native compilers, analyzers, sanitizers, symbols, full tests, CRAN and depends-only checks | `release-native-20260716T165635Z` | Passed and independently replayed |
| Public R API, four header releases, 35 translation units, GCC and Clang | `release-r-api-20260716T171343Z` | Passed and independently replayed |
| Windows/Rtools and macOS ARM64 CI | Exact frozen-candidate remote run not yet available | Required external handoff before publication; publish only CI tag `paradox-2.0.0-ci-afa5668`, then follow `design/portability-ci.md` |
| Real R 4.3.3 and 4.5.2 runtimes | `release-runtime-matrix-20260716T172036Z` | Passed; verifier replayed with restricted `PATH` |
| Behavioral differential, 17 cases | `20260717T030312Z-2192945` | Exact frozen candidate: 10 equal, 7 exact reviewed differences, 0 unexpected; verifier passed over 689 files |
| GCT, Valgrind, and bounded rchk | `release-memory-20260716T172507Z` | Passed and independently replayed |
| CRAN/Bioconductor P0/P1 hard dependency preparation | `release-reverse-dependencies-p1-afa5668-20260716` | Passed and sealed |
| GitHub P0/P1 hard dependency preparation | `release-consumers-p1-afa5668-20260716` | Passed and sealed |
| GitHub P0/P1 source tests | `release-consumers-p1-afa5668-20260716` | Sealed: 20 passed, 7 classified non-candidate failures, 1 bounded timeout; both verifiers passed |
| CRAN/Bioconductor P0/P1 source checks | `release-reverse-p1-afa5668-20260717-v3` | Sealed: all 21 completed, 17 passed, 4 factual non-candidate failures, 0 timeouts; verifier passed |
| Documentation, books, galleries, and serialized migration workloads | `release-documentation-afa5668-20260717-v2` | Sealed: 7/7 mandatory passed; 7/10 advisory passed with 3 factual external failures; verifier passed over 2,020 files |
| Full release benchmark inventory | `release-final3-fe9bd8c-20260717` | Sealed companion run: 59 paired workloads plus 9 focused rows; 64 passed policy, 4 marginal, 0 failed; verifier passed over 55 files |

The final differential manifest/seal SHA-256 pair is
`a1a678b778926b3efddbe0f9380e4dd0c8756595ddfd651736247f34bdcf8cf8` /
`0a9bfc7e4cc2673aa28dbd67f79374de61f20ab2b32c3547790c2dba2192ec0b`.
The reverse pair is
`5acb8201ac01721b7b976b75951764b6a2e8297cec7badb11b7bf46bfb74ac88` /
`e9d44a8bc98a35861ac1e706fe3dc983f50867ff5f3c2839a54d6d094ca1d9f6`.
The documentation pair is
`59c85298eae17b7ef438f2514d8abb53ee4610076e07c88be1f72d880e673d61` /
`76ab9be9de4d4f81e2f3b8ccf5fe7d3d004ad2d4b6dedd1a95c6b3215f6b1ee6`.
The benchmark pair is
`c1074c6aebeeb87da93926d6102d42405cc3ad4e9d0d82588f5dd9277611ce17` /
`f0c69e95d1e5e97f199794ad6be495770eb15ae3c36ea96c2a2b3b2ec5ceaaf8`.
Each pair is the evidence-manifest content hash followed by the retained
completion-seal file hash.

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

### Classified reverse-dependency limitations

The final reverse gate ran six resource-bounded waves. Eight installation
caches were reused, thirteen packages were built once, no package was rebuilt,
and the complete protected-library fingerprint passed twice. Its four
non-green rows are retained as failures with these factual classifications:

- `mlr3cluster` requires the Weka `XMeans` plug-in absent from the isolated
  environment; the exact failure reproduces with paradox 1.0.1.
- `mlr3filters` encounters CRAN mlr3pipelines 0.11.0's incompatible
  `FilterEnsemble` prototype. The baseline probe is identical and the pinned
  GitHub mlr3pipelines source contains the fix and passes.
- CRAN `mlr3spatiotempcv` 2.3.4 creates two vdiffr snapshots whose titles
  normalize to the same filename: 1 failure follows 1,325 passes. The pinned
  GitHub source removed the duplicate and passed 1,315 expectations with three
  skips.
- `mlrintermbo` 0.5.1-1 successfully traverses the Paradox accessors before
  its examples and tests require the unavailable Suggested package
  `ParamHelpers`. This is an environmental dependency failure, not a Paradox
  assertion or crash.

### Documentation outcomes

All mandatory documentation contracts passed: both mlr3book install workloads
and the Paradox chapter, the website install helper and 67-chunk benchmark, the
Paradox cheatsheets, and the 128-row serialized `mbo_config` spaces. Seven
advisory workloads also passed, including the full website, all four
cheatsheets, both mlr3benchmark nested-values contracts, the 2,048-row
`mbo_config` workload, and both mlr3-targets contracts. The three factual
advisory failures were the full book, where an unrelated mlr3fairness
`MeasureFairness` prototype lacks `base_measure`, and both archived gallery
renders, which require the obsolete unavailable package `distill`.

Every one of the 17 workloads passed both protected pre/post boundaries: 34/34
checks over nine roots and 131,709 inventory rows retained the identical
metadata hash
`395411555e08e1bb13c1d1d418c11a2444976955c1ddd0275fcef724585888bf`.
The final full content boundaries also matched for the candidate, package
libraries, local R base, both overlays, toolchain, TinyTeX, and Quarto.

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

The documentation gate receives two explicit protected overlays. The
mlr3verse-core library supplies the reviewed hard-import closure. The second,
`library-documentation-extra-final3`, supplies `gt` 1.3.0, `V8` 8.2.0,
`bigD` 0.3.1, and `juicyjuice` 0.1.0 from
`environment/r-packages-linux-64.lock`; its pre-release content SHA-256 is
`169286c9080e0c5a3b58a3ffd51a24e258d817030cd1f8c991411453a84c99d6`.
The schema-8 gate binds both overlay paths and content hashes, checks their
metadata at every workload boundary, and rehashes them at postflight. This
release reuses the second overlay from sealed documentation evidence; a future
release should give that small overlay its own reproducible preparation
receipt.

Discretionary performance work is frozen. Reopen native code only if the final
full benchmark exposes a clear release-relevant regression with a low-risk,
measured fix. A source edit requires a new candidate and proportionate replay
of every gate that consumes its package bytes; a verifier-only repair does not.

### Reviewed direct-values performance tradeoff

The first complete diagnostic benchmark classified two direct
`ParamSetCollection$values` reads as regressions under the generic tiers. The
flat rich read measured 120.1 microseconds versus 80.0 upstream and the nested
read 183.7 versus 133.7; the plain read was 110.2 versus 107.3 and allocated
10,600 bytes from an upstream zero. Triage found no unsafe C loop or
release-worthy low-hanging fix. A flat-only R fallback measured about 82
microseconds but nested fallback was about 170 versus 148--161 native; selecting
only the flat shortcut would add another dispatch boundary and abandon native
admission for about 40 microseconds.

The cost buys per-read authentication of nine flat or eleven nested frames:
the public and private R6 wrappers, child ParamSet tables, value snapshots,
IDs and types, translation ownership, graph edges, affixed output IDs, and
fallback safety are all checked against live mutable R state. The reviewed
`authenticated-read` tier is therefore restricted to the three direct active
binding workloads. Its median, upper-quartile, slower-probability, allocation
ratio, and allocation-delta limits are 1.60, 1.70, 0.80, 1.50, and 16 KiB.
It does not cover `get_values()`, assignments, or any other path.

The fresh sealed run kept all three reads visible as marginal allocation rows:
plain median ratio 1.236, rich 1.266, and nested 1.166. The only additional
marginal was `mlr3pipelines_graph/values`, whose median ratio was 1.080 and
upper-quartile ratio 1.189 under the stricter hot tier. There were no failed
rows. Related collection paths materially improve: construction is 3.1--3.5x,
assignment 14.8x, filtered reads 10.9--11.0x, dependency aggregation
22.8--25.1x, and domain aggregation 744--982x faster. The remaining root-carrier
sizing idea had already measured only about 4--9%, so native source was not
reopened.

### Reverse-gate harness incidents

Two pre-release reverse runs were deliberately rejected rather than cited as
evidence. `release-reverse-p1-afa5668-20260716` compared the raw legal package
version `0.0.4-3` with R's canonical `0.0.4.3` spelling and stopped before
checking `miesmuschel`. The validator now keeps the exact raw version in the
receipt but compares installed versions as parsed `package_version` objects;
the focused synthetic cache test covers the incident and genuine mismatches.

`release-reverse-p1-afa5668-20260717` completed its first wave and all four
checks in its second wave, but failed to seal two factual non-green rows because
raw log excerpts were marked with `Encoding = "bytes"` before character-based
diagnostic truncation. The shared compactor now converts valid UTF-8, escapes
invalid external bytes as ASCII `<xx>`, normalizes newlines and tabs,
and truncates to an exact character bound. Synthetic Unicode, invalid-byte,
head/tail, and representative failed-row tests cover that path. Both changes
alter authenticated runner bytes, so each required a fresh run ID; package
installations remained in their content-addressed caches and the frozen
candidate package was neither rebuilt nor changed.

The final verifier initially rejected the aggregate reverse slice solely
because a one-row aggregate retained positional `row.names = "2"` while the
corresponding per-package file used `"1"`. The verifier now removes only the
positional row-name attribute before `identical()`. Multirow fixtures prove
that schema, order, and values remain exact and that any substantive change is
still fatal. This verifier-only repair did not change the already completed
runner evidence and did not warrant another reverse execution.

### Documentation-gate harness incident

The rejected `release-documentation-afa5668-20260716` stage died after workload
10 inside fs 2.1.0's recursive `dir_map` while repeatedly scanning 131,709
entries. All nine roots were individually valid; a cumulative reproduction
corrupted state after 27 successful mixed scans. The replacement collector is
an explicit nonrecursive base-R queue. It records symlinks without descending
through them and retains `fs::file_info(follow = FALSE)` metadata. Hidden and
nested paths, hardlinks, directory and dangling symlinks, repeated GC, and the
real protected tree are covered by focused tests. The crashed stage is unsealed
forensic material only; the v2 stage above is the sole documentation evidence.

### Benchmark-gate harness incidents

Four benchmark-only defects were exposed before or after measurement and fixed
without changing package bytes:

- TRE interpreted `"[\\r\\n\\t]"` as the literal letters `r`, `n`, and `t`,
  so ordinary release paths were rejected. `[[:cntrl:]]` now performs the
  intended byte classification.
- `paste0()` with a zero-length optional protected-library index produced one
  spurious role. `sprintf()` now preserves zero length, with 0/1/many fixtures.
- `vapply()` retained path names in the expected support-library vector while
  JSON correctly returned the same values unnamed. Both optional path vectors
  now use `USE.NAMES = FALSE`.
- The completion error compactor repeated the TRE mistake and removed literal
  letters. One shared control-character compactor now preserves ordinary text.

The fully measured `release-final2-97b5fe0-20260717` stage has three successful
commands, complete 59-by-100 paired samples and 9-by-100 focused samples,
matching protected boundaries, and empty stderr, but it failed after timing on
the names-attribute check. It remains deliberately unsealed and is not release
evidence. Rather than inventing a post-hoc recovery schema, every downstream
shape, quantile, policy, provenance, and sealing check was replayed read-only,
the harness was fixed and tested, and the final gate was rerun once from
scratch. The successful final stage is the sealed row above.

## Release completion rule

All local package, compatibility, memory, documentation, and performance gates
now have final factual outcomes; every successful evidence stage above has
passed its independent verifier. Package-bearing paths remain identical to the
frozen candidate, the benchmark companion's canonical distribution manifest
and native disassembly match it, and the primary worktree must remain clean at
handoff. The classified failures and marginal benchmark rows above are explicit
reviewed limitations, not open-ended allowlists.

The release is not yet publishable. Exact frozen-candidate Windows/Rtools and
Apple ARM64 CI must still pass after the local ref is made available remotely;
the ref currently exists only in this repository and no push is authorized.
The minimal handoff is the single CI-only tag and manual dispatch specified in
`design/portability-ci.md`; do not push local `main`, the benchmark companion,
all tags, or the custom release-ref namespace. After the run, this ledger must
retain its run ID and URL, exact `headSha`, both named platform conclusions,
and hashes of the metadata, complete log, and workflow before publication.
Do not turn any future consumer failure into an allowlist: first reproduce it
against upstream paradox under the identical environment, fix a harness
artifact when proven, and add a package regression test for every genuine
candidate defect.
