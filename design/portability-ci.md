# Native portability CI

The normal package check matrix treats portability as a release requirement,
not as a best-effort downstream check. It covers:

- release and development R on Linux x86-64;
- release R without Suggests on Linux x86-64;
- release R on Windows x86-64, using the suitable Rtools version selected by
  `r-lib/actions/setup-r`; and
- release R on the standard `macos-15` GitHub-hosted runner. GitHub documents
  that label as an Apple silicon ARM64 image; it is pinned instead of relying
  on a moving `macos-latest` architecture.

The R 4.3 depends-only row builds, installs, checks examples and vignettes, but
uses `--no-tests`. The sealed local old-runtime matrix owns the reviewed public
test scope: it runs 57 of 79 files and authenticates the 22 direct-native
admission contexts that require R 4.6 inspection APIs. Running those 22
implementation tests indiscriminately on R 4.3 creates hundreds of expected
fallback differences without adding compatibility evidence.

The matrix checks both GitHub's runner architecture and R's reported
architecture. Each entry then performs a clean source installation, loads the
resulting shared library, and verifies that registered `.Call` routines are
present before running the ordinary package check. This makes a missing native
build visible even if a future check configuration happens to reuse an
installed package.

The check runs through a direct `rcmdcheck` call, rejects its retained child
status explicitly, and then applies an independent top-level completion check.
That check requires exactly one expected `00check.log`, no `Execution halted`,
one status line, and a final nonempty line equal to `Status: OK`. The raw-log
boundary is mandatory because wrapper policies can accept a nonzero child
status when an internal R error does not form a conventional check section.

The workflow follows the current major-version recommendations from the
[`r-lib/actions` examples](https://github.com/r-lib/actions/tree/v2/examples).
The runner labels and architectures are defined in GitHub's
[`actions/runner-images` inventory](https://github.com/actions/runner-images#available-images).
No compiler path or platform-specific compilation flags are overridden: R,
Apple Clang, and Rtools remain responsible for selecting their supported C17
toolchains.

## Rejected first 2.0.0 run

Manual run
[`29559803987`](https://github.com/mlr-org/paradox/actions/runs/29559803987)
checked out immutable candidate tag `paradox-2.0.0-ci-afa5668` at
`afa56689e4037ee14a75b32811686f563f95effe`. It is diagnostic evidence only.
The run was attempt 1, event `workflow_dispatch`, and concluded `failure`.
Four ordinary rows—Linux release, Linux devel, Windows release, and macOS
ARM64—exported `_R_CHECK_DEPENDS_ONLY_` as the present-but-empty string. R
converted it to `NA`, aborted at `if (R_cdo_tests)` after package installation,
and emitted `Execution halted`. `rcmdcheck` 1.4.0 accepted that child status
because the abort did not form a conventional ERROR/WARNING/NOTE section, so
GitHub labeled all four jobs green. The Linux release no-Suggests row completed
with `Status: OK`. The R 4.3 row genuinely ran tests and its 832 failures were
confined to 20 of the 22 authenticated pre-4.6 exclusion contexts.

The exact job inventory is:

| Job ID | Matrix row | GitHub conclusion | Retained log SHA-256 |
|---|---|---|---|
| `87819643660` | `ubuntu-latest / x86_64 (release) – noSuggests` | `success`; genuine final `Status: OK` | `00a41857b7df5d733d12cd8b0f8d3ebec14b95eeeb02a966c90717d4d46859d0` |
| `87819643667` | `ubuntu-latest / x86_64 (release)` | `success`; false green after `Execution halted` | `77b50f27e28ba2a3a46db684080bb36ec1a111a9bb4d3d07b38f6aaba8a7b228` |
| `87819643668` | `windows-latest / x86_64 (release)` | `success`; false green after `Execution halted` | `0f8cd01243c78f59408076701d7100b9382cd913516c0c3f3a32ccf05cfc9305` |
| `87819643686` | `ubuntu-latest / x86_64 (4.3) – noSuggests` | `failure`; 9,602 passes, 832 reviewed pre-4.6 failures | `4628bd7276861e88a9da0c3f54c681f6c0c018288a23b9304a9ee2d515baa1fd` |
| `87819643694` | `ubuntu-latest / x86_64 (devel)` | `success`; false green after `Execution halted` | `11385ea89c9d208db257194ddc4823620512ea8c09a154055ba5330c1471dd67` |
| `87819643708` | `macos-15 / arm64 (release)` | `success`; false green after `Execution halted` | `e7f4b815b69b634eb121c2ff84295e0a5eb5fded08d7e654994751f734bd86ab` |

The sole uploaded artifact was ID `8398884805`, name
`Linux-X64-r4.3-5-results`, size 7,388,076 bytes, and REST digest
`sha256:5b1ca972d9eeea4a6adea745b3c39880456422bc2e6bb6f917b1ed1cbf568fb2`.
Its 687 extracted files are authenticated by the artifact manifest below.

The completed run is retained at
`.local/ci/r-cmd-check-29559803987-failed`. Its run metadata SHA-256 is
`e9f05278069f9e9222d2ddfc3870233fbdddfc208948d4cda6d9d1f82cba899a`;
the six-job log-manifest SHA-256 is
`8380c769e779ce1217d0b0748b90738a050f4882fdfe68c7b7603d6b4af34ad0`;
the workflow SHA-256 is
`21d2ac45d6d25207b374969f41c013bbe11bd4cce58fb8d87b47e215d734b0e0`;
the artifact metadata SHA-256 is
`861605ab59119669bfadacf796a912c59367067e15338a65fba9ee46b25cf65d`;
the downloaded R 4.3 artifact-manifest SHA-256 is
`5630de7d50535b7208f4a66afb26a1fb4c0198dfaaab6ebf2c9eaf2d6c6070a5`;
and the top-level evidence-manifest SHA-256 is
`8c0c38376f2e014a2866a6f5c4477251a6721c043036c70c9c644968fabe7c5f`.
Never infer check completion from its four green labels, and never move or
reuse its candidate tag.

## Rejected second 2.0.0 run

The hardened detached companion was:

- local ref: `refs/paradox-release/portability-harness-b840d9c`;
- commit: `b840d9c4a4d118c70595f0ce00d38ed7951761ee`;
- tree: `f624cd5bf8b8faeedf2e77ced6dc8f4904504689`;
- direct parent/package candidate:
  `afa56689e4037ee14a75b32811686f563f95effe`;
- workflow SHA-256:
  `54b1265d48ca60fc6ccd8e1f42d9798b8af5e7f2d2dfb12e593a4d079cefe392`;
- immutable remote tag:
  `paradox-2.0.0-ci-afa5668-harness-b840d9c`.

Manual run
[`29561742772`](https://github.com/mlr-org/paradox/actions/runs/29561742772)
was attempt 1, event `workflow_dispatch`, branch the immutable harness tag,
head SHA the full companion commit, and conclusion `failure`. Both artifacts
bind the workflow to `b840d9c4a4d118c70595f0ce00d38ed7951761ee` and the checked-out package to
`afa56689e4037ee14a75b32811686f563f95effe`.

| Job ID | Matrix row | Result | Retained log SHA-256 |
|---|---|---|---|
| `87825499474` | `windows-latest / x86_64 (release)` | `success`; every required step passed and the sole check status was final `Status: OK` | `625b19371ae2164ef18207ae72755c746aee33df0e1dbf820e71cdd0b2d8b2c2` |
| `87825499523` | `macos-15 / arm64 (release)` | `failure`; checkout, architecture, compilation, provenance, and upload passed, while direct check and independent completion verification failed | `1c87983826ca74e5bda0727c7f47fb59085139f28f17611044f730d987d403b0` |

The macOS check ran 19,008 successful expectations before one genuine failure
at `test-native-paramset-qunif.R:58`: native `.499` mapping returned
`-0.01999999999999980`, while the historical R primitive sequence returned
`-0.01999999999999957`. Apple Clang had contracted the affine expression into
an ARM64 fused operation. The check therefore emitted `Execution halted` and
ended `Status: 1 ERROR, 1 NOTE`. The NOTE separately reported the reticulate/uv
coordination file `uv-setuptools-d489c7a91649635e.lock` as temporary-directory
detritus. Both are candidate defects; the Windows pass does not transfer to
package bytes that contain their corrections.

| Artifact ID | Name | Size | REST SHA-256 digest |
|---|---|---|---|
| `8400111188` | `paradox-2.0.0-portability-windows-latest-x86_64` | 2,997,115 bytes | `d991c5abf562c35ab7a7f491f13168c2dbce77a49983b353dcb35587442eb8d3` |
| `8399808867` | `paradox-2.0.0-portability-macos-15-arm64` | 4,185,166 bytes | `00bff571f283cbd3abcc047fece0437617819a5a07596d7830104c7dcf507688` |

The rejected run is retained at `.local/ci/r-cmd-check-29561742772`. Its exact
metadata hashes are `run.json`
`6cb32298ec0b142d61ee691772955a1ae2f0d50dac89b9c13698bb94b44a1b53`,
`jobs.json`
`e54eaf77e6fb70ee6b10d5cb003cc2bdb97e57d59ba509454044c572957529c3`,
`artifacts.json`
`b81afa50589c4d2901b0c5bf5b57d7e66d21954bd6dd52378c18c49d2a127af4`,
and executed workflow
`54b1265d48ca60fc6ccd8e1f42d9798b8af5e7f2d2dfb12e593a4d079cefe392`.
The manifest-file SHA-256 values are:

- `ARCHIVE-SHA256SUMS`:
  `591a501d68e85a26a710e9d3a1906b30a2adfe70c1287229bf3de405362c750a`;
- `ARTIFACT-SHA256SUMS` (754 extracted files):
  `e484378567d711e8a8b910f01a6ed57904a296bcb75a8a6feaba9ff47a2e71ff`;
- `JOB-LOG-SHA256SUMS`:
  `369bb35828866aa8d4b48a97b5e579f01b7c3c8e92db42cd93aec3470044bb95`;
- `METADATA-SHA256SUMS`:
  `faa6ace38dc4539a447ecdff7e9dabd577548a5f41f4f68d5892a23ad8d8c091`;
- `VERIFIER-SHA256SUMS`:
  `e61285926dde11028e7083b5e9abbb83c5df51fd9bd264190cf796fca969801d`;
- `EVIDENCE-SHA256SUMS`, which binds the five subordinate manifests:
  `18ce43ffb4c85d3863a85c6736f9981cc646c49ab31f0d58c35d92dbbeb43fa1`.

The retained verifier has SHA-256
`d6541a76e8eaec5c4a2489c4bb8055d515d751eaeaa7dc7692229603be98adb7`;
its rejection log has SHA-256
`d1e2a722c341a6d8f47983c69d4a92a9c784c76ef30881e2baec8824c2abef10`
and rejects exactly because the run conclusion is `failure`. This is evidence
that the hardened workflow exposed a real defect, not release portability
evidence. Never move, delete, or reuse either the candidate or companion tag.

## Accepted final 2.0.0 run

The corrected package candidate is frozen at
`refs/paradox-release/candidate-20260717T083921Z`, commit
`2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa`, tree
`91eea910212ea15f4ccba598929f8a61844bcc35`. Its source archive is
`.local/compat/candidate-freeze-2f40e3e/build/paradox_2.0.0.tar.gz`, SHA-256
`508a596b435f0e017c60cb54143656a8b85ad0f78f37476730d282334102aeb8`.
It contains 211 files and no `.git` member. A complete comparison with the
delta-gate archive found 209 byte-identical files; only the expected
`DESCRIPTION` `Packaged` timestamp and stochastically rendered
`inst/doc/indepth.html` differ. The candidate's final `.Rbuildignore` addition
`^\.git$` is the build-control fix that prevents linked-worktree Git metadata
from entering the archive.

The direct-child portability companion is frozen locally at
`refs/paradox-release/portability-harness-ede67fc`, commit
`ede67fc5780c9b1f6f91325189f8f7560376060c`, tree
`578abec87a84c706f3a77803f9c645c933619aac`. Its direct parent is exact
candidate `2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa`, and its only changed path is
`.github/workflows/r-cmd-check.yml`. The executed workflow must have SHA-256
`3a08de120b85707611e8f1f94e73f88aa92e926e85352b8303b1d81a772d4f4a`.
The static release-workflow regression test and repository-local `actionlint`
both pass on this companion.

The immutable tags are `paradox-2.0.0-ci-2f40e3e` for the exact candidate and
`paradox-2.0.0-ci-2f40e3e-harness-ede67fc` for the exact companion. They were
published by the user without pushing `main`, the benchmark companion, or the
local `refs/paradox-release/` namespace. The rejected `afa5668` and `b840d9c`
identities remain immutable historical evidence.

Manual run
[`29573168344`](https://github.com/mlr-org/paradox/actions/runs/29573168344)
was attempt 1, event `workflow_dispatch`, head branch
`paradox-2.0.0-ci-2f40e3e-harness-ede67fc`, head SHA
`ede67fc5780c9b1f6f91325189f8f7560376060c`, and conclusion `success`. Both
jobs authenticated checkout
`2f40e3e567c6d4fa568384622cb4e2d81c3fb2fa`; the executed workflow SHA-256 was
`3a08de120b85707611e8f1f94e73f88aa92e926e85352b8303b1d81a772d4f4a`.

| Job ID | Matrix row and interval | Runtime | Result and retained hashes |
|---|---|---|---|
| `87861491934` | `windows-latest / x86_64 (release)`, `10:21:11Z`–`10:53:20Z` | `x86_64-w64-mingw32`, R 4.6.1 UCRT | `success`; 19,011 pass, 2 skip, 0 fail/warn; sole final `Status: OK`; job log `3112a7d5804191751b5652e5c62b243925c00de0b4b91f3d5429ba21d488a228`; check log `c47f8c8bb33b514e800957508532f80e703eaecb8df087b845e494d441543418`; provenance `1b57eceeacf7d2998415dae1f957937715e74348f65364dfb841d3b6b43fd6e8` |
| `87861491957` | `macos-15 / arm64 (release)`, `10:21:09Z`–`10:36:42Z` | `aarch64-apple-darwin23`, R 4.6.1, Apple Clang 17 | `success`; 19,013 pass, 2 skip, 0 fail/warn; sole final `Status: OK`, including clean temporary-detritus check; job log `2f5c11fc5ef5d1d86e54610a66e20ab90e80960aa9f9ca0d018684b9e00fb338`; check log `edf3c4fbbd9abfb39309bccc18a611fb8c4ada55bf4935ef4d8cb27fe9fd0bf9`; provenance `6bf22f031a8f5c885e9e503f5243b9c72178e2446ea89e20cda9452024ca0035` |

| Artifact ID | Platform | Size | REST digest / raw ZIP SHA-256 | Extracted files |
|---|---|---:|---|---:|
| `8404629035` | Windows x86-64 | 3,000,388 bytes | `df9a1e7b334d76c0748383cb1808c744df22ecfbfaf0e20f4b8ba6d7ccd277fc` | 375 |
| `8404256296` | macOS ARM64 | 4,146,086 bytes | `228bccc9d96fac3b6f41a9b9f248809d4bff8f09e88c3e14f424d0157419af53` | 377 |

The accepted evidence is retained exactly once at
`.local/ci/r-cmd-check-29573168344`. The REST metadata hashes are `run.json`
`069181abcfc38e4f29d725c2d647b3622399a3b6290af71e07217c1bb9aed307`,
`jobs.json`
`fe0740f581fa08e4a577163e1e3fa73065259faa25e47d881d22a2cb0dfbe7f7`,
and `artifacts.json`
`0dc36716c03f6000ab0718b85070910b0076046b07b02dade423dbf6020cad02`.
The exact six-manifest hierarchy is:

- `ARCHIVE-SHA256SUMS` (2 rows):
  `ad02591bc69faadeeec988cad28e1723dd2120442c7eafe656c9b0927adb7f80`;
- `ARTIFACT-SHA256SUMS` (752 rows):
  `36cad6980df7af4b5d9a4987c402b1979b8087404eb23e484c15e3513d24d649`;
- `EVIDENCE-SHA256SUMS` (5 rows):
  `7e8eb7097d3b403b5b4e712f5da65d1a8018c6d6cd0eb0efc533abd9b4b79484`;
- `JOB-LOG-SHA256SUMS` (2 rows):
  `9fc6cc750d1b0d3699e206ddccff3e245d35834acb920777e9cbf0e7f18e668d`;
- `METADATA-SHA256SUMS` (4 rows):
  `3227de2dc55f56797586f3236e10858a05230cc3119ffb382122c30301955b59`;
- `VERIFIER-SHA256SUMS` (2 rows):
  `34696b48062cd3bed7a6b87d2f79a89b99aea5e6116d6580c32b8d76fa126674`.

Those manifests cover exactly 767 rows. The retained verifier SHA-256 is
`7a6a88fe06882bc7aa443f99e29e6c1b8aec8e0fca2389b131405e060487c670`,
and its deterministic acceptance receipt has SHA-256
`d4872821afd51fa1101456dfe72136db6b5fad1dbbb67010a3a613dcc9ec60d5`.
The verifier accepted the evidence once after checking the REST identities,
raw archives, extracted trees, job logs, workflow, candidate provenance, sole
final check statuses, and complete manifest coverage. This closes the final
portability gate; it does not assert that a CRAN upload or publication has
occurred.
