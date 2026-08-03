# Downstream Paradox 2 pull-request handoff

Agentic processes must not push these branches, update remote pull requests, or
close them. Eight repository-local branches remain useful migrations. The mlr3
and mlr3fselect diagnostic-only branches are wholly redundant; no corresponding
PR is open and neither branch should be published.

## Current handoff heads and manual state

The user has published all eight reviewed migration heads and opened their
pull requests. No agent made a remote write. The exact heads remain the sealed
compatibility inputs; do not update or force-push them without rerunning the
corresponding evidence.

| Repository | Exact head | Current remote state | Remaining manual action |
|---|---|---|---|
| bbotk | `09dafa6c3048f9be5b6961739787d201f6600a6f` | [PR #356](https://github.com/mlr-org/bbotk/pull/356), open/mergeable draft; all hosted checks green | mark ready, review, merge, and release before Paradox 2 |
| mlr3tuning | `0ec4f40033a393d41c7842541c2d5f8173dfb6bd` | [PR #565](https://github.com/mlr-org/mlr3tuning/pull/565), open/mergeable draft; all hosted checks green | mark ready, review, merge, and release before Paradox 2 |
| miesmuschel | `3c4bf94788b9259878b1fa067d216823d0771681` | [PR #100](https://github.com/mlr-org/miesmuschel/pull/100), open/mergeable draft; repository workflow disabled for inactivity | mark ready and review/merge; rely on the exact local dual-axis source checks unless CI is re-enabled |
| mlr3mbo | `85dd8a5ada86aacafe93637711e3b1f2e91ba219` | [PR #284](https://github.com/mlr-org/mlr3mbo/pull/284), open/mergeable draft; ordinary checks green, one dev-bbotk row lacks Rush development API | mark ready, review, merge, and release as at least 1.2.2 |
| celecx | `5a094a391ae11a8ae23ce4abf98eaf63e36bb3f1` | [PR #8](https://github.com/mlr-org/celecx/pull/8), open/mergeable draft; hosted dependency solving cannot obtain unpublished mlr3mbo >= 1.2.1.9000 | mark ready and merge only after the compatible mlr3mbo release; exact local source check passes |
| mlr3pipelines | `a7954067061f20a45dd9e6c03129dca0ba0f1753` | [PR #1016](https://github.com/mlr-org/mlr3pipelines/pull/1016), open/mergeable draft; pkgdown green, check rows fail only on removed mlbench Pima data | mark ready, review, merge, and release before Paradox 2; do not retry unchanged Pima failures |
| mlr3fda | `0df56f51b5d7fd751e16575fbd897b1c7f449c5e` | [PR #171](https://github.com/mlr-org/mlr3fda/pull/171), open/mergeable draft; hosted checks green | mark ready, review, merge, and release before Paradox 2 |
| mlr3forecast | `35e4bdc914a913508450866e629309f8364077ec` | [PR #53](https://github.com/mlr-org/mlr3forecast/pull/53), open/mergeable draft; hosted checks green | mark ready, review, merge, and release before Paradox 2 |

No mlr3 or mlr3fselect diagnostic-only PR remains open.

## Current source and candidate

Package-facing source is frozen at
`refs/paradox-release/candidate-20260803T131049Z`, commit
`a0a9ff3e05b535068392e0c20442ad9794f3b824`, tree
`49079e12a816542fe8d8d6a0a2290a757a558b41`. The checked-in
`release-refresh-20260720`/`paradox2` compatibility axis now binds that exact
immutable identity. Final compatibility evidence is owned by clean,
package-facing-source-identical tooling ref
`refs/paradox-release/validation-tooling-20260803T151943Z`, commit
`f7b3eff651ad747015a4d3372304f296eac661f7`, tree
`7749375e12300c6aa4258a76206ce082b96bd09f`. Both axes bind the eight
published PR heads above without changing them.

## Active compatibility conclusion

Fresh Paradox-1 and Paradox-2 preparation reproduced exact dependency endpoint
`3828cdaf0c767d89fa30ea595845d3eab9695dfdb80bee909a5b3fabcc31919e`;
both ten-package overlays pass their current and retained validators. All eight
prepared heads build and complete source-package checks on both axes. Seven
finish with `Status: OK`; mlr3fda has only the reviewed environmental `fdasrvf`
cross-reference NOTE.

Paradox-1 coordinator
`release-candidate-a0a9ff3-final-p1-focused-f7b3eff-r1` passes all five tasks.
Paradox-2 coordinator
`release-candidate-a0a9ff3-final-p2-compat-f7b3eff-r1` passes ten of twelve;
its overall nonzero status is the intentional factual aggregate of the broad
repository and CRAN reverse-dependency rows, not a harness or candidate
failure. The broad corpus completes 20 of 28 exact repositories, with the
eight non-green rows retaining reviewed upstream or environmental
classifications. The reverse run completes all 22 rows without timeout, OOM,
missing worker, or transient failure: ten pass and twelve retain reviewed
non-green outcomes. All seven mandatory documentation rows pass, and 14 of 17
rows pass overall. Current and retained verifiers accept the exact-head,
corpus, reverse, and documentation evidence.

Seven reverse-dependency failures are unadapted released versions of bbotk,
mlr3tuning, miesmuschel, mlr3pipelines, mlr3mbo, mlr3fda, and mlr3forecast.
Each corresponding prepared PR head in the table above passes the exact dual-
axis source-package gate. Hosted reverse-dependency run `30807900759` likewise
contains no transient infrastructure failure and matches those prepared fixes;
do not retry unchanged released versions as though they were candidate bugs.
The five remaining non-green reverse rows are reviewed optional/external-
dependency or offline-network outcomes.

Exact hashes and row classifications are recorded in `AGENTS.md` and
`design/release-2.0.0.md`. No `f27776e` package-bound result was transferred to
`a0a9ff3`; the new executions own these conclusions even though the downstream
heads are unchanged.

## Historical `f27776e` retained evidence

Fresh Paradox-1 and Paradox-2 preparations reproduce exact dependency endpoint
`3828cdaf0c767d89fa30ea595845d3eab9695dfdb80bee909a5b3fabcc31919e`;
both ten-package overlays and both current/retained verifiers pass. All eight
exact heads above build and complete source-package checks successfully on
both axes. Seven end `Status: OK`; mlr3fda retains only the unrelated
`fdasrvf` cross-reference NOTE. Paradox-1 completion/results/manifest/seal
SHA-256 values are
`da3ea0f6ce24639cd9af6d1d83d1f32f3eeed138ebeedd5c9f4851d3e90865ca`,
`2e8595a74aa9df7adbd1cb0998b0a25aa0bcc220dfea91189fc49fe0a29cc83a`,
`d909980ec907143229a8acced261a6f357fbcf55e059ce03e7a4d20d1ced0e1d`,
and
`c45d078095689941c41ca5e73cb532d85b84a14e7904e73f67cd96bbfb13c5c6`.
Paradox-2 values are
`f3a000732a1ff786d0a2bcf4191015d2b065f1fe0dd2b0037fc3dc5e30f38159`,
`c5fe3fdc8a2d0bed4d7bd4eafcef3a1af28af1ad2e81708c90437ff72aa5299a`,
`e7c356cfd18cce1c6892c22f0c3c4c278dcf4d0e32fcda18d6f1be0ed22f95af`,
and
`6395aac9a706507851191b80227e7063306ce686548ac9cf32f93764aafd5cb8`.

The final broad corpus passes 20 of 28 exact repositories; all eight retained
non-green rows are reviewed upstream/environmental outcomes. The reverse run
passes 10 of 22 and completes every row without timeout/OOM; every non-green
row is either an unadapted release covered by a passing PR head above or an
external/optional dependency failure. All mandatory documentation rows pass.
Current and retained evidence verifiers accept the exact-head, broad, reverse,
and documentation stages. The final sealed benchmark has 79 passes, three
bounded marginals, and zero failures. Exact hashes and row classifications are
recorded in `AGENTS.md` and `design/release-2.0.0.md`. Older evidence below is
history and must not be presented as active-candidate proof.

## Active hosted-portability handoff

The replacement immutable direct child is
`refs/paradox-release/portability-harness-00a24cb`, commit
`00a24cb3a094f08e486e4271d673a13f18df0a90`, tree
`96fc9ad4fc7d9607f9222bfa2f50b3a19a0ff0b0`, with local tag
`paradox-2.0.0-ci-a0a9ff3-harness-00a24cb`. Its sole parent is `a0a9ff3`; its
sole changed path is `.github/workflows/r-cmd-check.yml`, whose SHA-256 is
`bb17199b4c6621bcc427961e499e9fc88e00407e48b73789de0447c9c57f1e40`.
It inherits the authenticated installer as exact `100644` blob
`3b420d542dc5a1ae5506380543159cdea84517fa` and the reviewed R 3.6 source
lock as exact `100644` blob `5e9fb484b63cff6ee51ab2101dcaa37defd0e603`.
Structural, release-mode, deterministic-renderer, exact-lock-materialization,
evidence-verifier, documentation-economy, and actionlint validation pass
locally.

The candidate tag and superseded `da5a500` companion tag are already remote,
and the last observed remote `paradox_c` head is exactly
`8f1654e3d1c91e395823c912a1727a91b7f6fb4f`. Agents must not run any command
below. After reviewing the local refs, the user may publish the current branch
and only the new `00a24cb` tag atomically, then dispatch that immutable tag.
The lease and remote-tag assertions deliberately fail if remote state has
moved; do not weaken or delete them.

```sh
repo=/home/mewse/paradox_neo
candidate=a0a9ff3e05b535068392e0c20442ad9794f3b824
old_companion=da5a500936d00f7bc4c44989258d5bc385082252
companion=00a24cb3a094f08e486e4271d673a13f18df0a90
remote_head=8f1654e3d1c91e395823c912a1727a91b7f6fb4f
candidate_tag=paradox-2.0.0-ci-a0a9ff3
old_companion_tag=paradox-2.0.0-ci-a0a9ff3-harness-da5a500
companion_tag=paradox-2.0.0-ci-a0a9ff3-harness-00a24cb

test -z "$(git -C "$repo" status --porcelain=v1 --untracked-files=all)"
test "$(git -C "$repo" symbolic-ref --short HEAD)" = paradox_c
test "$(git -C "$repo" rev-parse refs/paradox-release/candidate-20260803T131049Z)" = "$candidate"
test "$(git -C "$repo" rev-parse "refs/tags/$candidate_tag")" = "$candidate"
test "$(git -C "$repo" rev-parse refs/paradox-release/portability-harness-da5a500)" = "$old_companion"
test "$(git -C "$repo" rev-parse "refs/tags/$old_companion_tag")" = "$old_companion"
test "$(git -C "$repo" rev-parse refs/paradox-release/portability-harness-00a24cb)" = "$companion"
test "$(git -C "$repo" rev-parse "refs/tags/$companion_tag")" = "$companion"
test "$(git -C "$repo" rev-list --parents -n 1 "$companion")" = \
  "$companion $candidate"
test "$(git -C "$repo" rev-parse "$companion^{tree}")" = \
  96fc9ad4fc7d9607f9222bfa2f50b3a19a0ff0b0
test "$(git -C "$repo" diff --name-only "$candidate" "$companion")" = \
  .github/workflows/r-cmd-check.yml
test "$(git -C "$repo" show "$companion":.github/workflows/r-cmd-check.yml | \
  sha256sum | cut -d' ' -f1)" = \
  bb17199b4c6621bcc427961e499e9fc88e00407e48b73789de0447c9c57f1e40
test "$(git -C "$repo" ls-tree "$companion" -- \
  scripts/environment/install-hosted-r36-windows.ps1)" = \
  "$(printf '100644 blob %s\t%s' \
    3b420d542dc5a1ae5506380543159cdea84517fa \
    scripts/environment/install-hosted-r36-windows.ps1)"
test "$(git -C "$repo" show \
  "$companion":scripts/environment/install-hosted-r36-windows.ps1 | \
  sha256sum | cut -d' ' -f1)" = \
  283e3450e47337f3fc121d6e103c1c0a0ad66ab4cab63b8c42caa57ab174381b
test "$(git -C "$repo" ls-tree "$companion" -- \
  environment/runtime-r-3.6.3-packages.lock)" = \
  "$(printf '100644 blob %s\t%s' \
    5e9fb484b63cff6ee51ab2101dcaa37defd0e603 \
    environment/runtime-r-3.6.3-packages.lock)"
test "$(git -C "$repo" show \
  "$companion":environment/runtime-r-3.6.3-packages.lock | \
  sha256sum | cut -d' ' -f1)" = \
  9007e3a2d7eecb1057bf9610a2f2ffacf617c224b9aeb9b91bd1ef5ae85f59c5
git -C "$repo" merge-base --is-ancestor "$candidate" refs/heads/paradox_c
git -C "$repo" merge-base --is-ancestor "$remote_head" refs/heads/paradox_c
test -z "$(git -C "$repo" diff --name-only "$candidate" refs/heads/paradox_c | \
  grep -Ev '^(AGENTS\.md|compat/|design/|scripts/)' || true)"
test "$(git -C "$repo" ls-remote --heads origin refs/heads/paradox_c | cut -f1)" = \
  "$remote_head"
test "$(git -C "$repo" ls-remote --tags origin \
  "refs/tags/$candidate_tag" | cut -f1)" = "$candidate"
test "$(git -C "$repo" ls-remote --tags origin \
  "refs/tags/$old_companion_tag" | cut -f1)" = "$old_companion"
test -z "$(git -C "$repo" ls-remote --tags origin \
  "refs/tags/$companion_tag")"

git -C "$repo" push --atomic \
  --force-with-lease="refs/heads/paradox_c:$remote_head" origin \
  refs/heads/paradox_c:refs/heads/paradox_c \
  "refs/tags/$companion_tag:refs/tags/$companion_tag"
gh workflow run r-cmd-check.yml --repo mlr-org/paradox \
  --ref "$companion_tag"
```

Then locate the fresh run without polling partial logs:

```sh
gh run list --repo mlr-org/paradox --workflow r-cmd-check.yml \
  --event workflow_dispatch --limit 5 \
  --json databaseId,headSha,status,conclusion,url
```

Select only the fresh run whose `headSha` is
`00a24cb3a094f08e486e4271d673a13f18df0a90`. Retain and independently verify
its platform artifacts and REST job inventory before accepting hosted
portability.

### Archived `da5a500` hosted diagnostic

Candidate tag `paradox-2.0.0-ci-a0a9ff3` and companion tag
`paradox-2.0.0-ci-a0a9ff3-harness-da5a500` are already remote at the exact
commits asserted above. Hosted run `30853585319` at head `da5a500` reported all
four required jobs green, including current Windows, macOS ARM64, exact Windows
R 3.6.3/Rtools35, and the completion gate. The complete REST metadata, job
logs, raw artifact archives, extracted artifacts, and manifests are retained
under `.local/ci/r-cmd-check-30853585319-r1`.

That run is diagnostic, not portability acceptance. Windows checkout converted
the reviewed LF source lock to CRLF before the installer copied and hashed it:
the artifact retained SHA-256
`ff1fa9d5b65a52fc843e25d1de1e2429128cc114a9541f20e92c772f07ae4d4b`
instead of reviewed SHA-256
`9007e3a2d7eecb1057bf9610a2f2ffacf617c224b9aeb9b91bd1ef5ae85f59c5`.
The current and retained offline verifiers therefore reject it even though the
same package rows installed successfully. This is deterministic harness
evidence; do not retry `da5a500` unchanged and do not normalize its retained
artifact after the fact.

## Historical `f27776e` hosted-portability handoff

Candidate tag `paradox-2.0.0-ci-f27776e` and failed companion tags
`paradox-2.0.0-ci-f27776e-harness-198e838` and
`paradox-2.0.0-ci-f27776e-harness-ff3b510` are already remote. Run
`30793059118` stopped old Windows at the R 3.6 top-level `Rfe.exe` multiline
argument boundary. Run `30803703541` passed the complete current-Windows and
macOS jobs but stopped old-Windows job `91654062556` at the harness's
`Get-Command`/PATH assertion before launching R. Do not restart either run.
The final historical harness tag points to direct child
`582eba86e7a05428f63608272c1c6c6e11a894f4`, tree
`e5ba13f476b4997fea4dbaf5d60369a058ad024c`. Its two changed paths are excluded
from package builds. The workflow checks out that immutable companion, proves
its sole parent is candidate `f27776e`, admits only the two reviewed changes,
and authenticates the helper's exact `100644` tree entry before use. Do not
dispatch from the mutable development branch. Its old-Windows check failure is
deterministic failed-harness history. Do not execute any obsolete `f27776e`,
`582eba8`, `198e838`, or `ff3b510` publication, dispatch, or retry command from
an older revision of this document; only the `a0a9ff3`/`00a24cb` handoff above
is current.

### Exact Paradox 1 owner conclusion

The Paradox-1 source on both retained owner runs is exactly tag `v1.0.1`,
commit `cdcc8e616afe243cd7c54e15713c86f818a52626`, tree
`09815307df797edab1c3241ff75f476289e2cb6c`. The final owner conclusion is
composed from two sealed runs:

- bbotk `29f18061b03fe1d31bfd2d1955e3fe6be5cec0c0` passed its full source-package
  check in
  `.local/compat/runs/migration-release-final-p1-cdcc8e6-221c95e-r2/repository-checks-release-refresh-20260720-paradox1/`.
  Its `results.tsv` SHA-256 is
  `e7780ffd8498f3388179c2283ac76342aef4624b2b36c6e60ad25f93c3bb61e1`.
  Do not call that complete five-row stage green: it also contained the
  superseded miesmuschel `7cca4ed` head, whose row failed.
- The corrected miesmuschel
  `2734db0d896745926dbe0c14c2ede272affa9495` then passed its focused
  source-package check in
  `.local/compat/runs/migration-release-final-p1-cdcc8e6-2771f5d-r3/repository-checks-release-refresh-20260720-paradox1/`.
  The result, evidence-manifest, and completion-seal SHA-256 values are
  `2fc38d36c3691fbcde54fa8b50aafc34506c30d8e4e286cdbb7b8cfecabdb3c2`,
  `4f56dc540e92e82474a2341774533963614ea11f43dfc4c7d11b7e89a7d41d57`,
  and `b21a13cd65f08ba2094deddd5fd70876ca5573c92722f0fb972868107454860d`.

These two passing rows establish the final dual-version owner claim. Rebuilt
installed trees and content fingerprints may differ even for the same exact
Paradox source because installation timestamps and lazy-load databases are not
reproducible package payloads. The claim is exact source identity and sealed
row behavior, not byte identity between independently installed libraries.

### Historical exact Paradox 2 source-package checks

The last sealed Paradox-2 candidate before the dormant-value change is
`refs/paradox-release/candidate-20260724T105215Z`, commit
`8797f1163fe612cb01d1facf517834d3f516a697`, tree
`81e6f901266754b97a0906f88a472bf04795f13c`. The authoritative final
source-package row set is
`.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1/repository-checks-release-refresh-20260720-paradox2/`.
Under tooling commit `fc92edd7f1ab612468066fe06bd3d9fc7afea41c`,
all five exact heads passed with `Status: OK` and zero failed rows:
bbotk `29f18061`, miesmuschel `2734db0`, mlr3mbo `1a1c0ab`, celecx
`6da5102`, and mlr3fda `c1cdad5`. The completion TSV, results, evidence
manifest, and completion-seal file SHA-256 values are
`a43064cd48b78fd433b4a4995a2da9cfad422da7b3b66f53391de05fbd289179`,
`f443e9ace27450057e4c8fcb6f9d93da85d595028abfff133272cc32f11a364c`,
`aa6d0ad380f0453db8c87888bdd5b7d18d54bc2698d21b9a4e67ce1040bcd731`,
and `7de4719e3fa0016fb05d804aa240dea34a0e5de76763c7f1d5d885e2e581acf1`.
Both the strong live verifier and the retained generic inventory verifier
passed against this completed stage.

The earlier exact bbotk `29f18061` and miesmuschel `2734db0` owner checks also
passed in
`.local/compat/runs/migration-release-final-p2-8797f11-a05cd51-r1/repository-checks-release-refresh-20260720-paradox2/`.
The result, evidence-manifest, and completion-seal SHA-256 values are
`398ac3b6dba21ff0e2339a53d7e55b66551b9cb6c3c0c82290cbd4957a04733c`,
`109cb91c994b6804fd8b6fefd959036659ec3033e5c5da425ad4d4a1efddab40`,
and `3b2221874fcbd4f5d11be4b3eb63bd3b32f1086930645ae1311280b617e83b55`.
Tooling commit `a05cd51` changes only release-policy and evidence paths relative
to the candidate: it is package-facing-source-identical, not package-identical.
This narrower run remains corroborating owner evidence; it is not that
historical candidate's five-row final source-package set.

### Historical exact-candidate broad corpus (`8797f11`)

The complete priority-zero/one repository corpus was retained under
`.local/compat/runs/migration-release-final-p2-8797f11-fc92edd-bench-r1/repository-tests-priority-1-release-refresh-20260720-paradox2/`.
It uses the exact `8797f11` candidate above through tooling commit `fc92edd`,
which is likewise package-facing-source-identical to that candidate. The
completion records 28 rows: 20 passed and eight remained failed or timed out.
The ordered rows, accepted completion's repository-runner seal, and top-level
evidence completion-seal SHA-256 values are
`a1f487a5e11a120015d0e099a92d950bdbe0f387bf81bc077cfc353dffba3ddd`,
`d03fe66c95bcd50e34b29305e06296563d49cc9df43ea33899eb27632a002627`,
and `611ed1b2745143b34910f56ed1d6a3ce4f14031728955e1d4fe9d6cf389d4b9c`.

The passing rows include bbotk, mlr3tuning, miesmuschel, celecx, mlr3,
mlr3pipelines, mlr3learners, mlr3fselect, mlr3hyperband, mlr3mbo, mlr3verse,
mlr3fairness, mlr3fda, mlr3inferr, mlr3oml, mlr3spatiotempcv, mlr3proba,
mlr3automl, mlr3cmprsk, and mlr3batchmark. The other eight remain honestly
failed/timed-out evidence rows; retained logs scope them away from Paradox
rather than relabelling the corpus green:

- mlr3tuningspaces has one downstream helper-environment error after all
  430 Paradox-facing expectations pass: `expect_learner` was sourced into the
  wrong test environment.
- mlr3cluster lacks Weka's separately managed `XMeans` component; its other
  RWeka and Paradox work proceeds.
- mlr3filters tries to instantiate mlr3pipelines' argument-requiring
  `FilterEnsemble` through a zero-argument all-dictionary test.
- mlr3torch lacks the external Lantern runtime and also records external data
  download timeouts.
- xplainfi has a downstream test-scope/import failure in which `tgen` is not
  visible.
- mlr3extralearners reaches the fixed 3,600-second row timeout while continuing
  through its very broad learner corpus; it records no Paradox failure.
- mlr3forecast's source-only test process asks `help.search()` for an installed
  `mlr3forecast` package that is not in that process library.
- mlr3resampling's future workers likewise cannot attach the source-only
  `mlr3resampling` package from the parent process.

### Historical bounded evidence

The older 2,022/2,022 focused-axis runs under
`.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`,
and the focused upgrader runs under
`.local/checks/downstream-upgrader-latest-20260724/` and
`.local/tmp/assert-values-final.XDyV3e/`, remain useful development history.
They do not replace the exact final owner and broad-corpus evidence above.
Likewise, the old `a4617ca` to `10c6a0e` package-payload proof and the
`bf64490` to `9e87556` post-freeze exception are historical proof paths, not
evidence transfers to candidate `8797f11`.

## Dormant-value downstream notes

- mlr3 currently includes raw `$values` in learner/graph-related hashes. That
  is correct when dormant settings are part of object identity. If a caller
  instead wants canonical identity over only the currently effective
  configuration, it should deliberately hash `$get_values()`; Paradox does not
  silently redefine raw-store identity.
- The checked assignment in `mlr3tuning::AutoTuner` refit no longer fails merely
  because a tuned child became inactive in the selected branch. No downstream
  workaround should disable Domain validation to solve that failure class.
  The `0ec4f40` test-only branch removes an accidental helper default so two
  strict store-blind assertions remain meaningful on both majors, and
  version-gates only the TuneToken-child expectation whose semantics changed.
- `lrn(...)` and `$configure()` may now retain a dependency-inactive setting
  instead of erroring. Default-aware activity also fixes common cases such as
  an SVM option depending on the learner type's satisfying default. Downstream
  code should consume the active configuration through `$get_values()` as it
  already does for train/predict.
- A cross-child dependency owned by `ParamSetCollection` filters the
  collection-level read only. Direct child/PipeOp reads still apply the child's
  own dependency rows. This is the intended boundary for the planned
  mlr3pipelines automatic branch dependencies. The `a795406` tests record the
  simpler existing spline contract: Paradox 2 stores inactive `degree`, filters
  it from the default read, and reveals it when `type` becomes polynomial,
  while Paradox 1 retains its historical assignment error.
- The Paradox-1 miesmuschel Shadow explicitly asserts the complete candidate
  before writing and therefore remains strict. The Paradox-2 branch must use
  `paradox::ParamSetShadow`, whose native assignment stores dormant values and
  whose filtered reads reactivate them. Dual-version tests should assert the
  version-appropriate assignment result rather than recreating a second Shadow
  activity implementation. Head `3c4bf94` now carries that exact regression
  together with exact, dual-major diagnostic expectations.

The eight retained PRs are published as drafts; mark them ready, review, and
merge them first. Release the seven CRAN reverse-
dependency adaptations—bbotk, mlr3tuning, miesmuschel, mlr3mbo,
mlr3pipelines, mlr3fda, and mlr3forecast—before Paradox 2, so CRAN and ordinary dependency
resolution select compatible versions. Release mlr3mbo as at least 1.2.2, then
merge the GitHub-only celecx bridge. The prepared celecx branch requires the
exact bridge development line `mlr3mbo >= 1.2.1.9000`, which admits that
release but excludes released 1.2.0/1.2.1 without the bridge. The other
retained PRs are independent apart from their shared Paradox-2 boundary. The
commands below are retained as an exact publication/provenance record; they
have already been executed for these heads and should not be rerun unchanged.

## bbotk

- base: `74515a792243a0f62a95f8ba3452be6290278c8c`
- head: `09dafa6c3048f9be5b6961739787d201f6600a6f`
- branch: `codex/public-paramsetcollection-sets`
- target branch: `main`
- proposed title: `Use Paradox 2 public ParamSet state safely`

Proposed body:

> Paradox 2 makes the private layout of `ParamSetCollection` opaque. Read the
> optimizer-chain child values through the existing public `$sets` binding and
> add a regression for that public contract.
>
> The public Paradox 2 `$data` and `$deps` accessors deliberately return
> detached snapshots. Root those two owner objects for the complete native
> local-search lifetime instead of retaining unrooted pointers into their
> columns and Conditions. The added forced-allocation regression covers the
> lifetime boundary. This also fixes a latent C ownership bug with no API
> change on either Paradox version.
>
> Register the exact legacy `Codomain` class as Paradox 2's maintained additive
> owner migration. Preserve all eight historical leanified Codomain targets as
> cold gateways: they give an actionable recursive-upgrade error by default,
> or migrate the shell in place and replay the requested operation when the
> user explicitly enables first-use upgrading. Current objects bypass this
> migration path.
>
> Declare `rush >= 1.2.1.9000`, the first development line that exports
> `assert_profiles`, because the current compute-profile path calls that API
> directly. This makes the provider requirement explicit instead of depending
> on an ambient development checkout.

Original manual publication commands (already completed for this exact head):

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/bbotk push --set-upstream origin codex/public-paramsetcollection-sets
gh pr create --web --repo mlr-org/bbotk --base main --head codex/public-paramsetcollection-sets --title 'Use Paradox 2 public ParamSet state safely'
```

## miesmuschel

- base: `7aaca22d2fc61d8d86291b681a8ecbde21f649c5`
- head: `3c4bf94788b9259878b1fa067d216823d0771681`
- branch: `codex/paradox-paramsetshadow-bridge`
- target branch: `master`
- proposed title: `Use Paradox's ParamSetShadow on Paradox 2`

Proposed body:

> Paradox 2 now owns and exports the live `ParamSetShadow` abstraction, so
> miesmuschel no longer needs to depend on Paradox's private ParamSet layout on
> that version. Select and re-export the official generator at load time while
> retaining the legacy implementation for Paradox 1. Update fidelity tests to
> compare documented public operator representation and ParamSet values instead
> of recursively comparing opaque R6 environments. The downstream tests cover
> bridge selection, live values and dependencies, transformations, and
> deep-clone integration. Paradox's own `ParamSetShadow` contract suite owns
> direct constraint and serialization coverage rather than duplicating it here.
> The legacy Paradox-1 Shadow remains strict because it explicitly asserts the
> candidate before writing. On Paradox 2, test the official class's dormant-
> value storage and filtered reactivation contract instead of expecting that
> assignment-time dependency error or adding another downstream activity
> engine.
> This is intentionally a dual-version bridge; existing Paradox 1 installations
> continue to construct the legacy class. Keep the class documentation link
> valid when the package is checked with either Paradox major version.
>
> On Paradox 2, register the exact legacy miesmuschel Shadow as a replacement
> owner migration with its single `origin` dependency. Preserve every
> historical leanified target as a cold default-error/opt-in-replay gateway,
> migrate to Paradox's canonical current Shadow capsule without changing the
> serialized shell identity, and give explicit retired-API diagnostics for the
> former `params_unid` and `set_id` fields.
>
> Keep the load-time namespace rebinding deliberately narrow: only the exported
> generator and the eleven package-owned historical leanification targets may
> be unlocked, and cleanup is registered before the first target changes so an
> exceptional partial operation cannot leave those bindings mutable. Dynamic
> lookup avoids a misleading static unsafe-call NOTE; it does not make the
> rebinding mechanism part of the public API.
>
> Keep the dual-major failure tests precise: Paradox 1 retains its historical
> diagnostics, while Paradox 2 asserts the corresponding native messages.

Original manual publication commands (already completed for this exact head):

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/miesmuschel push --set-upstream origin codex/paradox-paramsetshadow-bridge
gh pr create --web --repo mlr-org/miesmuschel --base master --head codex/paradox-paramsetshadow-bridge --title "Use Paradox's ParamSetShadow on Paradox 2"
```

## mlr3mbo

- base: `4471f6fc4a8aa217fffb6ce5a45d3e525e96dc44`
- runtime change: `185b2298216eef47b0f976667c4e8949c069dff4`
- head: `85dd8a5ada86aacafe93637711e3b1f2e91ba219`
- branch: `codex/paradox2-transformless-subset`
- target branch: `main`
- proposed title: `Use the public transform-free subset API on Paradox 2`

Proposed body:

> Paradox 2 deliberately makes detached Domain internals non-authoritative and
> provides `ParamSet$subset(..., keep_trafo = FALSE)` for constructing a
> transformation-free acquisition domain. Use that public API on Paradox 2
> instead of deleting the private `.trafo` column returned by `$domains`.
> Retain both historical implementations for Paradox 1, so the package remains
> dual-version compatible. Extend the existing acquisition-domain regression to
> verify that producing the detached domain does not remove transformations
> from the source search space, and record the migration in NEWS.

Original manual publication commands (already completed for this exact head):

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3mbo push --set-upstream origin codex/paradox2-transformless-subset
gh pr create --web --repo mlr-org/mlr3mbo --base main --head codex/paradox2-transformless-subset --title 'Use the public transform-free subset API on Paradox 2'
```

## celecx

- base: `8fc8a8dbaf15e72010b0e721a2167c5db9984810`
- head: `5a094a391ae11a8ae23ce4abf98eaf63e36bb3f1`
- branch: `codex/paradox2-diagnostics`
- target branch: `master`
- proposed title: `Use the Paradox 2 mlr3mbo bridge`

Proposed body:

> Require mlr3mbo >= 1.2.1.9000, the compatible development bridge and
> predecessor of the intended >= 1.2.2 release, so released 1.2.0/1.2.1 cannot
> select their unsupported private-Domain path on Paradox 2. Keep the
> design-grid comparison meaningful on both Paradox generations: Paradox 2's
> native dependency planner and celecx reject the same cycle at different
> boundaries, so assert each package-owned cycle diagnostic instead of
> requiring the two implementation messages to be identical. Ordinary value
> diagnostics remain on their original precise assertions.
>
> Stop assigning `NULL` to obsolete active bindings in the test surrogate and
> assert the public data.table prediction result instead.

Original manual publication commands (already completed for this exact head):

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/celecx push --set-upstream origin codex/paradox2-diagnostics
gh pr create --web --repo mlr-org/celecx --base master --head codex/paradox2-diagnostics --title 'Use the Paradox 2 mlr3mbo bridge'
```

## mlr3tuning

- base: `5ac566dc53480e2fd3fa0497f70f2cb038412863`
- head: `0ec4f40033a393d41c7842541c2d5f8173dfb6bd`
- branch: `codex/paradox2-dormant-values-current`
- target branch: `main`
- proposed title: `Test Paradox 2 dormant dependency values`

Proposed body:

> Preserve the two strict point-validation tests as genuine store-blind,
> no-default checks by removing the accidental default from their test helper.
> Version-gate only the TuneToken-child case whose documented behavior differs:
> Paradox 1 retains its historical assertion error, while Paradox 2 skips the
> incoming dependency edge and accepts later domain-valid dormant values.
>
> This is a test-only compatibility adaptation. Complete mlr3tuning suites pass
> against both Paradox 1 and 2, and no runtime validation is disabled.

Original manual publication commands (already completed for this exact head):

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3tuning push --set-upstream origin codex/paradox2-dormant-values-current
gh pr create --web --repo mlr-org/mlr3tuning --base main --head codex/paradox2-dormant-values-current --title 'Test Paradox 2 dormant dependency values'
```

## Redundant diagnostic-only PRs

Close the mlr3 branch `codex/paradox2-diagnostics` at `35e30a9` and the
mlr3fselect branch `codex/paradox2-diagnostics` at `ae8e1d1` if their PRs are
open. After removing their temporary diagnostic gates, each effective
package-facing source tree is identical to its target base (`f70c001` and
`cd12d77`, respectively). There is therefore no commit to publish and no
replacement PR to create.

The final live-head review does not create a new mlr3fselect branch or probe.
Current main `29fa095` only removes its obsolete Paradox `set_id` compatibility
test in favor of the already-covered public `p_dbl()` path; its other changes
are unrelated and its updated tests require mlr3's separate diabetes-task
refresh. The exact pinned Paradox-facing path therefore remains the smaller,
more informative compatibility subject.

The same read-only review compared the pinned mlr3 support head `f70c001`
against current main `7cb6a08` on 2026-07-31. The two intervening commits cache
class frequencies in `TaskClassif` and replace the removed `pima` data task
with the synthetic `diabetes` task across examples, snapshots, and tests; none
changes a Paradox import, ParamSet path, parameter value path, or Paradox-facing
test. Keeping `f70c001` paired with the reviewed mlr3fselect snapshot therefore
avoids importing a 67-file unrelated corpus refresh without omitting a known
Paradox contract.

## mlr3pipelines

- base: `bef040ae5c886bf5b09863b956d341eb3cbd772c`
- head: `a7954067061f20a45dd9e6c03129dca0ba0f1753`
- branch: `codex/paradox-diagnostic-compat-current`
- target branch: `master`
- proposed title: `Fix GraphLearner cloning and test dormant spline values`

Proposed body:

> Deep-clone mutable R6 values stored in `GraphLearner$state$param_vals` so a
> cloned learner cannot mutate the original learner through shared proxy
> content. The previous test happened to pass with Paradox 1 because its
> traversal encountered an alias through private ParamSet storage first;
> Paradox 2's detached capsule layout exposed the existing ownership bug. Add
> an explicit identity and mutation-isolation regression that catches it under
> both Paradox versions.
>
> Also make the existing spline dependency assertion dual-version: Paradox 1
> keeps its historical inactive-assignment error, while Paradox 2 retains the
> dormant degree in raw values, filters it for a natural spline, and reveals it
> when the type becomes polynomial.
>
> Inflate the expected dictionary object as well as the observed object before
> comparing them. This makes the assertion symmetric and valid with both the
> Paradox 1 and Paradox 2 public shells.

Original manual publication commands (already completed for this exact head):

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3pipelines push --set-upstream origin codex/paradox-diagnostic-compat-current
gh pr create --web --repo mlr-org/mlr3pipelines --base master --head codex/paradox-diagnostic-compat-current --title 'Fix GraphLearner cloning and test dormant spline values'
```

## mlr3fda

- base: `8960c9292221e7065e5175762e12354c6eb08607`
- head: `0df56f51b5d7fd751e16575fbd897b1c7f449c5e`
- branch: `paradox2-snapshots`
- target branch: `main`
- proposed title: `Support Paradox 2 diagnostics in FDA snapshots`

Proposed body:

> Keep the existing Paradox 1 wavelet-pipeline error snapshots byte-for-byte
> unchanged and select a Paradox-2 snapshot variant only when that major is
> installed. The informative diagnostic content and punctuation are now the
> same on both axes; the variant remains necessary only because Paradox 2's
> native assignment reports the current versioned
> `.__paradox2_ParamSet__values()` call header instead of Paradox 1's
> `self$assert()` header. Pipeline construction and FDA behavior are unchanged.

Original manual publication commands (already completed for this exact head):

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3fda push --set-upstream origin paradox2-snapshots
gh pr create --web --repo mlr-org/mlr3fda --base main --head paradox2-snapshots --title 'Support Paradox 2 diagnostics in FDA snapshots'
```

## mlr3forecast

- base: `8e352550f2334e42c8c81ee80d3e237b807abebe`
- head: `35e4bdc914a913508450866e629309f8364077ec`
- tree: `195e95a01bdc79e3027a8cd6685e5e3b18a4733a`
- branch: `paradox2-snapshots-20260801`
- target branch: `main`
- proposed title: `Support Paradox 2 diagnostics in forecasting snapshots`

The 2026-08-02 read-only publication audit found remote `main` at
`58868a8cdeee74f3793a190308c635b48900e804`, twelve commits beyond the recorded
base. Those upstream commits and this branch's four test/snapshot paths are
disjoint, so the exact tested head remains a clean PR input and was not rebased
after the final two-axis checks. If a later upstream change touches one of
those four paths before publication, rebase and repeat the focused dual-axis
check instead of claiming the retained head evidence.

Proposed body:

> Keep the existing Paradox 1 forecasting snapshots byte-for-byte unchanged
> and select Paradox-2 variants for the three affected validation-call
> headers. The diagnostic bodies and production behavior are unchanged;
> Paradox 2 only reports its native
> `.__paradox2_ParamSet__values()` gateway where Paradox 1 reports
> `self$assert()`.

Original manual publication commands (already completed for this exact head):

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3forecast push --set-upstream origin paradox2-snapshots-20260801
gh pr create --web --repo mlr-org/mlr3forecast --base main --head paradox2-snapshots-20260801 --title 'Support Paradox 2 diagnostics in forecasting snapshots'
```
