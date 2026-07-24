# Downstream Paradox 2 pull-request handoff

Agentic processes must not push these branches, update remote pull requests, or
close them. Six repository-local branches remain useful migrations. The mlr3
and mlr3fselect diagnostic-only PRs are now wholly redundant and should be
closed without replacement.

Before the serialized-object migration work reopened Paradox and the two
affected bridge packages, the retained runtime changes passed the complete
named release-refresh gate on both pinned Paradox axes. The pruned working
trees then passed 2,022/2,022 focused expectations per axis, with zero
failures, errors, warnings, or skips. That pre-migration evidence is retained
under
`.local/checks/informative-diagnostics-downstream-paradox1-focused-20260723T131726Z/`
and `.local/checks/informative-diagnostics-downstream-final-20260723T130129Z/`.

The current bbotk and miesmuschel trees additionally implement the exact
serialized-owner bridges required by Paradox 2. Against a fresh Paradox 2.0.0
installation, bbotk's focused upgrader suite passed 8/8 and miesmuschel's
ParamSetShadow suite passed 62/62. Authentic Paradox-1 Codomain and Shadow
fixtures passed default diagnostics, opt-in replay for every historical target,
explicit recursive migration, cloning, and RDS round-trips. Tightened additive,
replacement, retired-binding, and canonical-Shadow contracts also passed.
Evidence is retained under
`.local/checks/downstream-upgrader-latest-20260724/`, with the final
`assert_values = FALSE` fixture replay under
`.local/tmp/assert-values-final.XDyV3e/`. The exact commits below contain those
tested source trees.

Publish and merge the dual-version bridge PRs before Paradox 2. Land and
release mlr3mbo as 1.1.2 before celecx, whose DESCRIPTION intentionally
requires that bridge. The other retained PRs are independent apart from their
shared Paradox-2 boundary. After each push, the `gh pr create --web` command
below opens the exact comparison when a PR is not already open; otherwise push
the recorded branch and update the existing PR. Copy the immediately preceding
proposed body into the form when creating one.

## bbotk

- base: `905901b45d4dd9445efc0ffa49e663ab5ae534cb`
- head: `29f18061b03fe1d31bfd2d1955e3fe6be5cec0c0`
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
- head: `2734db0d896745926dbe0c14c2ede272affa9495`
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
> Keep dependency-error expectations useful on both major versions by selecting
> the official Paradox 2 diagnostic only when that implementation is active.
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

- base: `d1ce6189b637dd552fac95d56c53a39503bae889`
- runtime change: `a8a988a64b66e651043b75f63dfdfb4604185e3f`
- head: `1a1c0abe95f59cd314f1fbc19c596cb6ac15f067`
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
- head: `6da5102ca948b8182aae13575c48a932812b05c6`
- branch: `codex/paradox2-diagnostics`
- target branch: `master`
- proposed title: `Use the Paradox 2 mlr3mbo bridge`

Proposed body:

> Require the compatible mlr3mbo development bridge so CRAN mlr3mbo 1.1.1
> cannot select its unsupported private-Domain path on Paradox 2. Keep the
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

## Redundant diagnostic-only PRs

Close the mlr3 branch `codex/paradox2-diagnostics` at `35e30a9` and the
mlr3fselect branch `codex/paradox2-diagnostics` at `ae8e1d1` if their PRs are
open. After removing their temporary diagnostic gates, each effective tree is
byte-identical to its target base (`f70c001` and `cd12d77`, respectively).
There is therefore no commit to publish and no replacement PR to create.

## mlr3pipelines

- base: `daebff3cc15257cdecde898fd3f0ceff5f320c53`
- head: `c85b2f4165e056934f892c5db37391869cd40e38`
- branch: `codex/paradox-diagnostic-compat`
- target branch: `master`
- proposed title: `Fix GraphLearner state deep cloning across Paradox versions`

Proposed body:

> Deep-clone mutable R6 values stored in `GraphLearner$state$param_vals` so a
> cloned learner cannot mutate the original learner through shared proxy
> content. The previous test happened to pass with Paradox 1 because its
> traversal encountered an alias through private ParamSet storage first;
> Paradox 2's detached capsule layout exposed the existing ownership bug. Add
> an explicit identity and mutation-isolation regression that catches it under
> both Paradox versions.

Publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3pipelines push --set-upstream origin codex/paradox-diagnostic-compat
gh pr create --web --repo mlr-org/mlr3pipelines --base master --head codex/paradox-diagnostic-compat --title 'Fix GraphLearner state deep cloning across Paradox versions'
```

## mlr3fda

- base: `5e6204d0d3a3c21325a71eda4402c30b31209eef`
- head: `8f5a3dfa297ad236812cda57fab02de75fec375a`
- branch: `paradox2-snapshots`
- target branch: `main`
- proposed title: `Support Paradox 2 diagnostics in FDA snapshots`

Proposed body:

> Keep the existing Paradox 1 wavelet-pipeline error snapshots byte-for-byte
> unchanged and select a Paradox-2 snapshot variant only when that major is
> installed. The informative diagnostic content and punctuation are now the
> same on both axes; the variant remains necessary only because Paradox 2's
> native assignment reports the internal `.__ParamSet__values()` call header
> instead of Paradox 1's `self$assert()` header. Pipeline construction and FDA
> behavior are unchanged.

Publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github-release-refresh-20260720/mlr3fda push --set-upstream origin paradox2-snapshots
gh pr create --web --repo mlr-org/mlr3fda --base main --head paradox2-snapshots --title 'Support Paradox 2 diagnostics in FDA snapshots'
```
