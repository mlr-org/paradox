# Downstream Paradox 2 pull-request handoff

Agentic processes must not push these branches, update remote pull requests, or
close them. Seven repository-local branches remain useful migrations. The mlr3
and mlr3fselect diagnostic-only PRs are now wholly redundant and should be
closed without replacement.

## Current handoff heads and manual state

No remote write was made while preparing this handoff. The exact local heads
below include the reviewed dormant-value adaptations. They remain local until
the user performs the listed manual actions:

| Repository | Exact head | Current remote state | Remaining manual action |
|---|---|---|---|
| bbotk | `b9925122e444015b65c4d548150764300c9c0637` | migration branch is absent remotely | push, create and merge the PR, then release before Paradox 2 |
| mlr3tuning | `0ec4f40033a393d41c7842541c2d5f8173dfb6bd` | migration branch is absent remotely | push, create and merge the PR, then release before Paradox 2 |
| miesmuschel | `ecd7c69e22b5fd73670393155781bdcd638445fd` | migration branch is absent remotely | push, create and merge the PR, then release before Paradox 2 |
| mlr3mbo | `85dd8a5ada86aacafe93637711e3b1f2e91ba219` | migration branch is absent remotely | push, create and merge the PR, then release as at least 1.2.2 |
| celecx | `3a8291a9e2058323f4af93452141f6f1e46b5295` | migration branch is absent remotely | push after the mlr3mbo release, then create and merge the PR |
| mlr3pipelines | `13610d39e06639ce96f0b76862f76acd794c0dc8` | migration branch is absent remotely | push, create and merge the PR, then release before Paradox 2 |
| mlr3fda | `0df56f51b5d7fd751e16575fbd897b1c7f449c5e` | migration branch is absent remotely | push, create and merge the PR, then release before Paradox 2 |

Closing any open mlr3 and mlr3fselect diagnostic-only PRs also remains manual.

## Current source and next candidate

There is no current frozen Paradox candidate while the final C17/C23,
complete-minor-runtime, and focused admission corrections converge. The
checked-in `paradox2` compatibility axis still pins the historical placeholder
`refs/paradox-release/candidate-20260727T152133Z`, commit
`dbbdcc156cb52793e84e8767f0ce84b6ecbb85ea`, tree
`b60b75e3923cdcb49f1ef2fb0f9b386d5cac291d`; it is not authority for the
reopened source. The branch heads above are the current reviewed migration
inputs. After source convergence, freeze one new immutable candidate, repoint
the `release-refresh-20260720`/`paradox2` axis to it, and make the prepared
compatibility gates bind these exact heads before the user publishes them.

## Historical retained evidence to refresh

The dormant-value/default-aware dependency change reopened package-facing
Paradox source after candidate `8797f11`. The exact runs below remain useful
baselines for the recorded downstream heads, but they are not release evidence
for the next candidate. Source is still converging for the final C17/C23,
all-minor runtime, and focused admission corrections. After that source is
frozen, build a fresh Paradox-2 bridge overlay, rerun the affected
dependency/value rows, and then run the normal broad downstream wave. Do not
relabel the historical hashes or pass counts.

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
This narrower run remains corroborating owner evidence; it is not the current
five-row final source-package set.

### Exact-candidate broad corpus

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
  mlr3pipelines automatic branch dependencies. The `13610d3` test records the
  simpler existing spline contract: Paradox 2 stores inactive `degree`, filters
  it from the default read, and reveals it when `type` becomes polynomial,
  while Paradox 1 retains its historical assignment error.
- The Paradox-1 miesmuschel Shadow explicitly asserts the complete candidate
  before writing and therefore remains strict. The Paradox-2 branch must use
  `paradox::ParamSetShadow`, whose native assignment stores dormant values and
  whose filtered reads reactivate them. Dual-version tests should assert the
  version-appropriate assignment result rather than recreating a second Shadow
  activity implementation. Head `ecd7c69` now carries that exact regression.

Publish and merge all retained PRs first. Release the six CRAN reverse-
dependency adaptations—bbotk, mlr3tuning, miesmuschel, mlr3mbo,
mlr3pipelines, and mlr3fda—before Paradox 2, so CRAN and ordinary dependency
resolution select compatible versions. Release mlr3mbo as at least 1.2.2, then
publish the GitHub-only celecx bridge. The prepared celecx branch requires the
exact bridge development line `mlr3mbo >= 1.2.1.9000`, which admits that
release but excludes released 1.2.0/1.2.1 without the bridge. The other
retained PRs are independent apart from their shared Paradox-2 boundary.
After each push, the `gh pr create --web` command
below opens the exact comparison when a PR is not already open; otherwise push
the recorded branch and update the existing PR. Copy the immediately preceding
proposed body into the form when creating one.

## bbotk

- base: `74515a792243a0f62a95f8ba3452be6290278c8c`
- head: `b9925122e444015b65c4d548150764300c9c0637`
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

Publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/bbotk push --set-upstream origin codex/public-paramsetcollection-sets
gh pr create --web --repo mlr-org/bbotk --base main --head codex/public-paramsetcollection-sets --title 'Use Paradox 2 public ParamSet state safely'
```

## miesmuschel

- base: `7aaca22d2fc61d8d86291b681a8ecbde21f649c5`
- head: `ecd7c69e22b5fd73670393155781bdcd638445fd`
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

Publish it manually with:

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

Publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3mbo push --set-upstream origin codex/paradox2-transformless-subset
gh pr create --web --repo mlr-org/mlr3mbo --base main --head codex/paradox2-transformless-subset --title 'Use the public transform-free subset API on Paradox 2'
```

## celecx

- base: `8fc8a8dbaf15e72010b0e721a2167c5db9984810`
- head: `3a8291a9e2058323f4af93452141f6f1e46b5295`
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

Publish it manually with:

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

Publish it manually with:

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
- head: `13610d39e06639ce96f0b76862f76acd794c0dc8`
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

Publish it manually with:

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

Publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3fda push --set-upstream origin paradox2-snapshots
gh pr create --web --repo mlr-org/mlr3fda --base main --head paradox2-snapshots --title 'Support Paradox 2 diagnostics in FDA snapshots'
```
