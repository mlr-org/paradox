# paradox 2.0.0 release ledger

This is the internal, append-only handoff for the native 2.0.0 release. It is
excluded from the package tarball. A retained row is evidence for the immutable
identity that it names. Evidence for an older payload may support the final
release only through the explicit bounded carry-forward decision below and
exact evidence for the changed delta; it must never be silently relabeled as a
run of newer package bytes. An old diagnostic run, an unsealed run, or a
successful command without retained provenance is not equivalent evidence.

## Current release state

Candidate `afa56689e4037ee14a75b32811686f563f95effe` and its portability
companion `b840d9c4a4d118c70595f0ce00d38ed7951761ee` are immutable rejected
historical evidence. GitHub Actions run `29561742772` exposed an ARM64
floating-point compatibility defect in the candidate and temporary-directory
detritus in its source tests. The corrected final package candidate is frozen
at full ref `refs/paradox-release/candidate-20260717T083921Z`, commit
`2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa`, and tree
`91eea910212ea15f4ccba598929f8a61844bcc35`. Its source archive is
`.local/compat/candidate-freeze-2f40e3e/build/paradox_2.0.0.tar.gz`, SHA-256
`508a596b435f0e017c60cb54143656a8b85ad0f78f37476730d282334102aeb8`.
The direct-child two-platform companion is frozen locally at
`refs/paradox-release/portability-harness-ede67fc`, commit
`ede67fc5780c9b1f6f91325189f8f7560376060c`, and tree
`578abec87a84c706f3a77803f9c645c933619aac`. Its parent is the exact corrected
candidate, its sole changed path is `.github/workflows/r-cmd-check.yml`, and
that workflow's SHA-256 is
`3a08de120b85707611e8f1f94e73f88aa92e926e85352b8303b1d81a772d4f4a`.
The intended new tags are `paradox-2.0.0-ci-2f40e3e` and
`paradox-2.0.0-ci-2f40e3e-harness-ede67fc`; neither the user-controlled remote
write nor the dispatch has occurred. No old tag may be moved or reused.

The complete corrected-candidate delta from `afa56689` is deliberately narrow:

- `.Rbuildignore` adds `^\.git$`, preventing a linked-worktree Git metadata
  file from entering `R CMD build`. This is build control only and does not
  alter the installed interface or native runtime.

- `src/domain_kernels.c` evaluates the existing affine quantile formula through
  three automatic `volatile double` rounding barriers. The helper has no
  `SEXP`, R C API, pointer, allocation, ownership, callback, or shared-state
  operation.
- `tests/testthat/test-native-domain-kernels.R` characterizes the ARM64 double
  difference and an adversarial integer `floor()` boundary.
- `tests/testthat/teardown.R` removes only a
  `uv-setuptools-[0-9A-Fa-f]{16}.lock` file from Python's temporary directory,
  and only when reticulate has already initialized Python. It is test-only and
  does not initialize Python, alter installed runtime behavior, or delete a
  broader class of files.

No `NEWS.md` entry was added. This correction was found before Paradox 2.0.0
was released, so it is not a behavior change from any released 2.0.0. Adding a
candidate-internal fix entry would be misleading and would unnecessarily
change the already validated package payload.

The amended payload has exact local delta evidence at
`.local/checks/release-fma-delta-20260717`. Its retained source archive SHA-256
is `e6955eedec5ff982acb186237f8fdd27d59281a0bb581c0e25de74f369a1f07f`;
the strict-GCC installed-content hash is
`b286ca087629577d3316c733da6a488bd575b3812eb12431dd8aaa4d61d19c59`
and its DSO hash is
`1bc34e7c95eb484cfb0c1c77b05048bccc69e644f508d625b626e2660147a2bd`.
The source-manifest, source-tree, and modes-tree SHA-256 values are respectively
`dd11fd7e188e49598fec94d46a2e369c5f90650fe43dafb5169871f75803c293`,
`d7929789f820e39df7311045909fab36ba2847ec63489f246f7f19750d63a0fc`,
and `a0e125fbdd3139e9ad04ea9f542ffaee19a7680f068f4b79ce80038788e251ff`;
their retained receipts verify 485 source and 996 mode entries. The completion
record SHA-256 is
`54f9f8968c05ec66cb2d59018360dfb5d21215026f4e11b55912358a0e55ff04`;
the `completion.sha256` file itself has SHA-256
`edc768d4fc0fd055f92f17cc26b413ef5a05be3b6c5525a8967b7b0053431cd1`.

Strict GCC ran 38,014 expectations: 38,010 passed, four were reviewed skips,
and none failed, errored, or warned. Its CRAN check ran 18,832 passes and 31
skips with no failure or warning and ended in its sole `Status: OK`; the
depends-only check also ended in its sole `Status: OK`. Strict Clang, ASan, and
UBSan each retained 135 successful native-probe records. The final C, test, and
teardown source hashes in the run match the current package-bearing files. The
frozen candidate archive contains 211 files and no `.git` member. Of those,
209 are byte-identical to the delta-gate archive; the only differences are the
expected `DESCRIPTION` `Packaged` timestamp and the stochastically rendered
`inst/doc/indepth.html`. This comparison binds the run to the corrected
candidate without relabeling the two archives as byte-identical.

The checksum-bound freeze summary
`.local/compat/candidate-freeze-2f40e3e/README.txt` has SHA-256
`dc77d1a5608bf9af0702ec98e173320ed6fca3db0abdf13f86de8583b59562a2`.
Its complete 455-entry `SHA256SUMS` verifies and has SHA-256
`03330fbd13cfed4554001f3c9766e420780bbfcc864c5e00308a362c76618c3e`.
The candidate and gate 211-file payload manifests have SHA-256
`690fd4d0bb95939294c0cee835977c5d9ccdff748c7a33109f7a46bf6db0ac0b`
and `ed2bf09658dee2bc3a4466f007ce52937c2d3554ca3643156f78d0872fdf07b0`.
The candidate installed-content fingerprint is
`5ba4d1842a1f80811299632d05ee1238178587544fc7262c11aa8311a4d83be2`;
its raw DSO SHA-256 is
`30e4e169061d035c953b2cca8a6f5c0fd0c21073f86436dc290d52a962fc93df`.
The candidate and gate DSOs differ only in debug paths and build IDs and are
byte-identical after removing those sections, both with SHA-256
`3099a2c88bf26a7639cec39ead58a575f1f9654e56ccbbce5bab35fab8a0a865`.
The installed candidate's exact affine-boundary probe passes.

Focused performance and code-generation evidence is retained at
`.local/benchmarks/release-fma-affine-20260717`. Its 104-row `SHA256SUMS`
authenticates every other retained file and has SHA-256
`7bf4bdadd1fced161bf49392cb0ad7b37966c34879aacbe525ad35cdb8c115fb`.
Six AB/BA repetitions for each of two public
`ParamSet$qunif()` workloads give identity-pooled costs of 1.205% at 64 mixed
parameters by 128 rows and 2.428% at 64 by 4,096. Allocations were byte-identical
at 126,800 and 3,682,128 bytes. Applying the representative 1.205% cost to the
sealed pre-barrier 9.801104x release speedup gives approximately 9.684x, reported
as about 9.7x rather than preserving the stale 9.8x claim. Retained
Apple-M1-target Clang assembly at `-O2` and `-O3 -ffp-contract=fast` contains
separate multiply/subtract operations and no fused instruction in the
registered affine paths.

## Superseded package identity: `afa56689`

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

At the time this candidate was frozen, the native/API evidence source commit
`aa07cce11e4c99ff777fe374700ba565cfa6b345`, the runtime/differential source
commit `c2847df4d4af989d5b1329ff0187f082280e17b9`, and this candidate had
identical package payloads. Their intervening changes are confined to
`.Rbuildignore`-excluded `scripts/`. This remains historical evidence for
`afa56689`; it does not assert that the later affine/test delta is identical.

The superseded full benchmark used the authenticated harness companion
`refs/paradox-release/candidate-20260717T035858Z`, commit
`fe9bd8cea27e3418c16437fe62c1994f70476533`, tree
`eea6098d93896721afe15f87604f42772c1e4b8a`, and run-local installation
`.local/compat/runs/release-benchmark-final3-fe9bd8c-20260717/library-candidate`.
Its installed-tree fingerprint is
`4d3699199bf7e09eec4472aefd622ab34fba82323d55553dcafefcf4beecbb13`;
this is expected to differ because R installation embeds build time and source
location metadata. No package-bearing path differed from candidate `afa56689`,
their canonical 207-file `R CMD build` payload manifests match at
`cfea9374a31002a7fd04f3f00b4d34e9f36e584010e43674d10505d77bcc635c`,
and their native shared-library disassemblies are identical. The companion is
therefore evidence for that same superseded distributable package source, not a
second package implementation.

The rejected portability replacement harness is the detached local ref
`refs/paradox-release/portability-harness-b840d9c`, commit
`b840d9c4a4d118c70595f0ce00d38ed7951761ee`, tree
`f624cd5bf8b8faeedf2e77ced6dc8f4904504689`. Its direct parent is the frozen
candidate and its sole changed path is `.github/workflows/r-cmd-check.yml`,
which `.Rbuildignore` excludes. The workflow SHA-256 is
`54b1265d48ca60fc6ccd8e1f42d9798b8af5e7f2d2dfb12e593a4d079cefe392`.
Each replacement job loaded this workflow but explicitly checked out and
authenticated remote candidate tag `paradox-2.0.0-ci-afa5668` at the full
frozen SHA before compiling or checking package code. Both this tag and
`paradox-2.0.0-ci-afa5668-harness-b840d9c` are immutable historical tags and
must never be moved or reused.

## Release evidence

| Gate | Retained ID | Release use |
|---|---|---|
| Full native compilers, static analyzers, sanitizers, symbols, tests, CRAN and depends-only checks | `release-native-20260716T165635Z` | Passed and independently replayed for `afa56689`; full unchanged-core evidence carried forward under the delta analysis below |
| Exact affine/test delta: strict GCC/Clang, full suite, CRAN, depends-only, ASan and UBSan | `release-fma-delta-20260717` | Passed on the amended built payload; exact final-delta evidence after frozen-payload equivalence is recorded |
| Public R API, four header releases, 35 translation units, GCC and Clang | `release-r-api-20260716T171343Z` | Passed for `afa56689`; carried forward because the delta adds no R API use or declaration |
| Windows/Rtools and macOS ARM64 CI | Runs `29559803987` and `29561742772` rejected; corrected candidate `2f40e3e5` and companion `ede67fc5` frozen locally, remote run pending | Blocking: both rows must pass against the exact candidate |
| Real R 4.3.3 and 4.5.2 runtimes | `release-runtime-matrix-20260716T172036Z` | Passed for `afa56689`; carried forward with exact delta source tests and the current-R full check |
| Behavioral differential, 17 cases | `20260717T030312Z-2192945` | `afa56689`: 10 equal, 7 reviewed exact differences, 0 unexpected; carried forward except for affine bytes now covered by exact R-method regression tests |
| GCT, Valgrind, and bounded rchk | `release-memory-20260716T172507Z` | Passed for `afa56689`; carried forward because the C delta has no pointer, R object, allocation, callback, or ownership operation, with exact ASan/UBSan probes on the amended DSO |
| CRAN/Bioconductor P0/P1 hard dependency preparation | `release-reverse-dependencies-p1-afa5668-20260716` | Passed and sealed; dependency closure unchanged |
| GitHub P0/P1 hard dependency preparation | `release-consumers-p1-afa5668-20260716` | Passed and sealed; dependency closure unchanged |
| GitHub P0/P1 source tests | `release-consumers-p1-afa5668-20260716` | Full `afa56689` evidence carried forward: 20 passed, 7 classified non-candidate failures, 1 bounded timeout |
| CRAN/Bioconductor P0/P1 source checks | `release-reverse-p1-afa5668-20260717-v3` | Full `afa56689` evidence carried forward: all 21 completed, 17 passed, 4 factual non-candidate failures |
| Documentation, books, galleries, and serialized migration workloads | `release-documentation-afa5668-20260717-v2` | Full `afa56689` evidence carried forward: no exported interface, object shape, help, or vignette input changed |
| Full release benchmark inventory | `release-final3-fe9bd8c-20260717` | Full unaffected-path inventory carried forward: 59 paired plus 9 focused rows, 64 passed, 4 marginal, 0 failed |
| Focused affine performance and Apple-M1 code generation | `release-fma-affine-20260717` | Exact C delta: 12 AB/BA runs, unchanged allocations, 1.205%/2.428% identity-pooled cost, and no fused instruction under forced contraction |

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

### Evidence carry-forward boundary

Repeating the full P0/P1 consumer, reverse-dependency, documentation, and rchk
corpora would have very low information value for this delta. The only shipped
C change is a scalar arithmetic helper called from the same admitted quantile
paths. It introduces no R API, object access, pointer, allocation, preservation,
callback, dispatch, class, outward shape, dependency, or documentation change.
The teardown change is confined to source-test cleanup after Python is already
initialized. The `.Rbuildignore` addition is build control that can only exclude
the linked-worktree `.git` metadata file; the frozen archive confirms that the
member is absent and that the remaining payload comparison has only the two
recorded generated-file differences. Consequently the old full evidence remains
the broad compatibility and lifetime corpus, while the amended bytes are
covered by the full package suite, both CRAN check profiles, strict GCC and
Clang, ASan, UBSan, exact double/integer R-method comparisons, focused
public-path performance, forced-contraction assembly, and the still-required
real Apple ARM64 and Windows checks. This is a bounded source-impact decision,
not permission to carry a future native edit forward.

The old real-runtime and public-API gates are also retained rather than
repeated: the helper uses only standard C17 scalar arithmetic and the current
delta gate rebuilt every translation unit. The behavioral differential remains
valid for every reviewed case outside affine rounding; the new regression
compares the changed path directly with the historical R methods, including a
bucket-changing integer boundary. Any further package-bearing change reopens
this decision and requires a new impact review and proportionate evidence.

### Rejected portability runs

GitHub Actions run
[`29559803987`](https://github.com/mlr-org/paradox/actions/runs/29559803987)
is immutable diagnostic evidence for `afa56689`, not a platform pass. Four ordinary rows
(Linux release, Linux devel, Windows release, and macOS ARM64) installed the
package but then passed a present empty `_R_CHECK_DEPENDS_ONLY_` value to R.
R converted it to `NA`, aborted at `if (R_cdo_tests)`, and logged
`Execution halted`. `rcmdcheck` 1.4.0 accepted that child status because no
formal check finding was parsed, so all four job labels were false green. The
Linux release no-Suggests row alone completed with `Status: OK`. The R 4.3 row
correctly failed after 9,602 passes because all 832 failures belonged to 20 of
the 22 already authenticated pre-4.6 direct-native exclusion contexts.

The incident is retained under
`.local/ci/r-cmd-check-29559803987-failed`. The run-metadata SHA-256 is
`e9f05278069f9e9222d2ddfc3870233fbdddfc208948d4cda6d9d1f82cba899a`,
the six individual job-log manifest is
`8380c769e779ce1217d0b0748b90738a050f4882fdfe68c7b7603d6b4af34ad0`,
and the 687-file downloaded-artifact manifest is
`5630de7d50535b7208f4a66afb26a1fb4c0198dfaaab6ebf2c9eaf2d6c6070a5`.
The executed workflow, artifact metadata, and top-manifest hashes are
`21d2ac45d6d25207b374969f41c013bbe11bd4cce58fb8d87b47e215d734b0e0`,
`861605ab59119669bfadacf796a912c59367067e15338a65fba9ee46b25cf65d`,
and `8c0c38376f2e014a2866a6f5c4477251a6721c043036c70c9c644968fabe7c5f`.
The six job ID/log-hash pairs were:

- `87819643660` / `00a41857b7df5d733d12cd8b0f8d3ebec14b95eeeb02a966c90717d4d46859d0`
  (no-Suggests, genuine pass);
- `87819643667` / `77b50f27e28ba2a3a46db684080bb36ec1a111a9bb4d3d07b38f6aaba8a7b228`
  (Linux release false green);
- `87819643668` / `0f8cd01243c78f59408076701d7100b9382cd913516c0c3f3a32ccf05cfc9305`
  (Windows false green);
- `87819643686` / `4628bd7276861e88a9da0c3f54c681f6c0c018288a23b9304a9ee2d515baa1fd`
  (R 4.3 genuine failure);
- `87819643694` / `11385ea89c9d208db257194ddc4823620512ea8c09a154055ba5330c1471dd67`
  (Linux devel false green);
- `87819643708` / `e7f4b815b69b634eb121c2ff84295e0a5eb5fded08d7e654994751f734bd86ab`
  (ARM64 false green).

Its sole artifact was ID `8398884805`, name `Linux-X64-r4.3-5-results`, size
7,388,076 bytes, and REST digest
`5b1ca972d9eeea4a6adea745b3c39880456422bc2e6bb6f917b1ed1cbf568fb2`.

The hardened companion run
[`29561742772`](https://github.com/mlr-org/paradox/actions/runs/29561742772)
is also immutable rejected evidence. It executed companion
`b840d9c4a4d118c70595f0ce00d38ed7951761ee` and checked out `afa56689`.
Windows job `87825499474` genuinely passed with final `Status: OK`; macOS ARM64
job `87825499523` failed after 19,008 passes because fused affine rounding
changed the `.499` double result, then reported the uv lock as detritus.
Their log hashes are
`625b19371ae2164ef18207ae72755c746aee33df0e1dbf820e71cdd0b2d8b2c2`
and `1c87983826ca74e5bda0727c7f47fb59085139f28f17611044f730d987d403b0`.
Artifact `8400111188` is the 2,997,115-byte Windows archive with digest
`d991c5abf562c35ab7a7f491f13168c2dbce77a49983b353dcb35587442eb8d3`;
artifact `8399808867` is the 4,185,166-byte macOS archive with digest
`00bff571f283cbd3abcc047fece0437617819a5a07596d7830104c7dcf507688`.

The retained directory is `.local/ci/r-cmd-check-29561742772`. Its run, jobs,
artifacts, and workflow hashes are respectively
`6cb32298ec0b142d61ee691772955a1ae2f0d50dac89b9c13698bb94b44a1b53`,
`e54eaf77e6fb70ee6b10d5cb003cc2bdb97e57d59ba509454044c572957529c3`,
`b81afa50589c4d2901b0c5bf5b57d7e66d21954bd6dd52378c18c49d2a127af4`,
and `54b1265d48ca60fc6ccd8e1f42d9798b8af5e7f2d2dfb12e593a4d079cefe392`.
The archive, artifact, job-log, metadata, verifier, and enclosing evidence
manifest hashes are
`591a501d68e85a26a710e9d3a1906b30a2adfe70c1287229bf3de405362c750a`,
`e484378567d711e8a8b910f01a6ed57904a296bcb75a8a6feaba9ff47a2e71ff`,
`369bb35828866aa8d4b48a97b5e579f01b7c3c8e92db42cd93aec3470044bb95`,
`faa6ace38dc4539a447ecdff7e9dabd577548a5f41f4f68d5892a23ad8d8c091`,
`e61285926dde11028e7083b5e9abbb83c5df51fd9bd264190cf796fca969801d`,
and `18ce43ffb4c85d3863a85c6736f9981cc646c49ab31f0d58c35d92dbbeb43fa1`.
The retained verifier rejected exactly on the failed run conclusion. The full
step, provenance, and manifest inventory is in `design/portability-ci.md`.
Neither rejected tag is a valid base for the corrected handoff.

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

Discretionary performance work is frozen for 2.0.0. Native source was reopened
once after the second external portability run exposed the Apple ARM64 FMA
correctness defect, not for a performance experiment. That bounded scalar
change is covered by the exact delta gate and focused benchmark recorded above;
the earlier full evidence is carried forward only within the documented impact
boundary. Any further package-source edit requires a new candidate, a fresh
impact analysis, and proportionate replay of the gates whose conclusions it can
affect. A verifier- or report-only repair does not invalidate package-byte
evidence whose authenticated inputs exclude that repair.

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

The broad local package, compatibility, memory, consumer, documentation, and
performance rows above have final factual outcomes for the superseded frozen
payload and have passed their independent verifiers. They are carried forward
only within the explicit impact boundary above. The exact delta gate and
focused affine benchmark separately validate the corrected delta. The 211-file
archive comparison binds that evidence to frozen candidate `2f40e3e5` with only
the recorded timestamp and stochastic-vignette differences. The primary
worktree must be clean at handoff, and the classified failures and marginal
benchmark rows above remain explicit reviewed limitations rather than
open-ended allowlists.

The release is not yet publishable. Its remaining handoff is:

1. Recheck both frozen local refs, then manually publish only candidate tag
   `paradox-2.0.0-ci-2f40e3e` at
   `2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa` and companion tag
   `paradox-2.0.0-ci-2f40e3e-harness-ede67fc` at
   `ede67fc5780c9b1f6f91325189f8f7560376060c`. Do not move or reuse either
   rejected tag, and do not push local `main`, the benchmark companion, all
   tags, or the custom release-ref namespace.
2. Manually dispatch the new companion and require both Windows/Rtools
   and Apple ARM64 jobs to pass with sole final `Status: OK` conclusions. Retain
   and verify the complete run/job metadata, individual logs, workflow,
   artifacts, manifests, and provenance before publication.
3. Add the new run ID and URL, companion `headSha`, candidate checkout
   identities, named platform conclusions, artifact provenance, check-log
   conclusions, and retained-file hashes to this ledger.

The `afa56689` and `b840d9c4` refs and runs `29559803987` and `29561742772`
remain immutable rejected historical evidence. The scalar helper and test-only
reticulate teardown do not require another full consumer, documentation, or
rchk campaign for the reasons recorded in the carry-forward boundary. Any
further package-source edit beyond that audited delta reopens impact analysis
and proportionate validation.

Do not turn any future consumer failure into an allowlist: first reproduce it
against upstream paradox under the identical environment, fix a harness
artifact when proven, and add a package regression test for every genuine
candidate defect.
