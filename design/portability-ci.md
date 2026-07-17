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
checked out the exact candidate but is diagnostic evidence only. Four ordinary
rows—Linux release, Linux devel, Windows release, and macOS ARM64—exported
`_R_CHECK_DEPENDS_ONLY_` as the present-but-empty string. R converted it to
`NA`, aborted at `if (R_cdo_tests)` after package installation, and emitted
`Execution halted`. `rcmdcheck` 1.4.0 accepted that child status because the
abort did not form a conventional ERROR/WARNING/NOTE section, so GitHub labeled
all four jobs green. The Linux release no-Suggests row completed with
`Status: OK`. The R 4.3 row genuinely ran tests and its 832 failures were
confined to 20 of the 22 authenticated pre-4.6 exclusion contexts.

The completed run is retained at
`.local/ci/r-cmd-check-29559803987-failed`. Its run metadata SHA-256 is
`e9f05278069f9e9222d2ddfc3870233fbdddfc208948d4cda6d9d1f82cba899a`;
the six-job log-manifest SHA-256 is
`8380c769e779ce1217d0b0748b90738a050f4882fdfe68c7b7603d6b4af34ad0`;
and the downloaded R 4.3 artifact-manifest SHA-256 is
`5630de7d50535b7208f4a66afb26a1fb4c0198dfaaab6ebf2c9eaf2d6c6070a5`.
Never infer check completion from its four green labels.

## Frozen 2.0.0 replacement handoff

The replacement is a detached harness companion whose direct parent is the
frozen candidate and whose only changed path is the package-excluded workflow:

- local ref: `refs/paradox-release/portability-harness-b840d9c`;
- commit: `b840d9c4a4d118c70595f0ce00d38ed7951761ee`;
- tree: `f624cd5bf8b8faeedf2e77ced6dc8f4904504689`;
- parent/package candidate:
  `afa56689e4037ee14a75b32811686f563f95effe`;
- workflow SHA-256:
  `54b1265d48ca60fc6ccd8e1f42d9798b8af5e7f2d2dfb12e593a4d079cefe392`.

The companion runs only the two missing platform checks. Its pinned checkout
explicitly selects remote tag `paradox-2.0.0-ci-afa5668`, asserts the resulting
HEAD is the full frozen SHA and clean, passes a legal `FALSE` logical, checks
the retained child status, verifies the raw completion footer independently,
and uploads provenance plus the full check directory even on failure. Run the
following only from the repository root and only in a user-controlled shell.
It pushes neither `main` nor any package commit.

```sh
candidate_commit=afa56689e4037ee14a75b32811686f563f95effe
candidate_tag=paradox-2.0.0-ci-afa5668
harness_ref=refs/paradox-release/portability-harness-b840d9c
harness_commit=b840d9c4a4d118c70595f0ce00d38ed7951761ee
harness_tree=f624cd5bf8b8faeedf2e77ced6dc8f4904504689
harness_tag=paradox-2.0.0-ci-afa5668-harness-b840d9c

set -eu
test "$(git remote get-url origin)" = \
  "https://github.com/mlr-org/paradox.git"
test "$(git rev-parse "$harness_ref^{commit}")" = "$harness_commit"
test "$(git rev-parse "$harness_commit^{tree}")" = "$harness_tree"
test "$(git rev-parse "$harness_commit^1")" = "$candidate_commit"
test "$(git diff --name-only "$candidate_commit" "$harness_commit")" = \
  ".github/workflows/r-cmd-check.yml"
test "$(git ls-remote --refs origin "refs/tags/$candidate_tag" | cut -f 1)" = \
  "$candidate_commit"
test -z "$(git ls-remote --refs origin "refs/tags/$harness_tag")"

git push --porcelain origin \
  "$harness_ref:refs/tags/$harness_tag"
test "$(git ls-remote --refs origin "refs/tags/$harness_tag" | cut -f 1)" = \
  "$harness_commit"

gh workflow run r-cmd-check.yml \
  --repo mlr-org/paradox \
  --ref "$harness_tag"
```

The resulting run must have event `workflow_dispatch`, head SHA equal to the
companion commit, and exactly two successful jobs named
`macos-15 / arm64 (release)` and `windows-latest / x86_64 (release)`. Each job
must pass `Verify frozen candidate checkout`, native compilation, direct
`R CMD check`, the independent completion verifier, provenance retention, and
artifact upload. The two downloaded provenance files must bind workflow SHA to
the companion and checked-out SHA to the candidate; both check logs must lack
`Execution halted` and end with their sole `Status: OK`. Retain the REST run and
job metadata, individual job logs, executed companion workflow, artifact
metadata and downloads, and a SHA-256 inventory before updating the release
ledger. Never move either CI tag.
