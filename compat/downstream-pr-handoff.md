# Downstream Paradox 2 pull-request handoff

Agentic processes must not push these branches or open remote pull requests.
The eight repository-local branches below are the complete reviewed migrations.
Before publication, the release gate must test these exact heads against the
frozen Paradox candidate and `compat/verify-mlr-org-review` must authenticate
their clean trees.

## bbotk

- base: `905901b45d4dd9445efc0ffa49e663ab5ae534cb`
- head: `6cae9559cfa2133b02b19e9762211aa49ec4c1c7`
- branch: `codex/public-paramsetcollection-sets`
- target branch: `main`
- proposed title: `Use Paradox 2 public ParamSet state safely`

Proposed body:

> Paradox 2 makes the private layout of `ParamSetCollection` opaque. Read the
> optimizer-chain child values through the existing public `$sets` binding and
> add a regression for that public contract. Keep the same tests useful with
> Paradox 1 and 2 by selecting the expected validation diagnostic by installed
> Paradox major version; no runtime behavior is version-branched.
>
> The public Paradox 2 `$data` and `$deps` accessors deliberately return
> detached snapshots. Root those two owner objects for the complete native
> local-search lifetime instead of retaining unrooted pointers into their
> columns and Conditions. The added forced-allocation regression covers the
> lifetime boundary. This also fixes a latent C ownership bug with no API
> change on either Paradox version.

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github/bbotk push --set-upstream origin codex/public-paramsetcollection-sets
```

## miesmuschel

- base: `7aaca22d2fc61d8d86291b681a8ecbde21f649c5`
- head: `6255050a4d1d1a3555ca70707e4f1588c86006be`
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

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github/miesmuschel push --set-upstream origin codex/paradox-paramsetshadow-bridge
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

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/downstream-pr-worktrees/mlr3mbo-paradox2 push --set-upstream origin codex/paradox2-transformless-subset
```

## celecx

- base: `8fc8a8dbaf15e72010b0e721a2167c5db9984810`
- head: `a2975550c14f824c6abc86db9db32e982908c3ea`
- branch: `codex/paradox2-diagnostics`
- target branch: `master`
- proposed title: `Accept Paradox 2 validation diagnostics in tests`

Proposed body:

> Keep celecx runtime behavior unchanged while making its diagnostic
> expectations work with both supported Paradox generations. Paradox 1 retains
> the exact checkmate-era fragments; Paradox 2 expectations select its native
> Domain-bound, initial-value, and dependency-cycle diagnostics. This is a
> test-only bridge. Together with mlr3mbo's public transform-free subset change,
> the affected acquisition, grid, bootstrap, and quantile tests pass without
> relying on mutable Domain internals. Require the compatible mlr3mbo
> development version so CRAN mlr3mbo 1.1.1 cannot select the unsupported path.

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/downstream-pr-worktrees/celecx-paradox2 push --set-upstream origin codex/paradox2-diagnostics
```

## mlr3

- base: `f70c001d526213d80059744b81fcbc420bb83017`
- head: `35e30a91e305936e57328b65e15b60f3ab00eef3`
- branch: `codex/paradox2-diagnostics`
- target branch: `main`
- proposed title: `Accept Paradox 2 numeric Domain diagnostics in tests`

Proposed body:

> Preserve the existing Paradox 1 assertions for invalid pinball and RQR
> `alpha` values while selecting Paradox 2's native numeric Domain-bound
> fragment on that major version. This changes test expectations only; measure
> construction and scoring behavior are unchanged.

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/downstream-pr-worktrees/mlr3-paradox2 push --set-upstream origin codex/paradox2-diagnostics
```

## mlr3fselect

- base: `cd12d7717f31701edc6ebcf7f2cbe1168cae1ecb`
- head: `ae8e1d163bc7d8a2dd9f12e61d704e5b0d8430d7`
- branch: `codex/paradox2-diagnostics`
- target branch: `main`
- proposed title: `Accept the Paradox 2 feature-fraction diagnostic in tests`

Proposed body:

> Select Paradox 2's native numeric Domain-bound fragment for the invalid RFE
> `feature_fraction` test while preserving the exact Paradox 1 expectation.
> The package-owned `subset_sizes` checkmate assertions remain unchanged because
> they are not Paradox diagnostics and already pass on both versions. This is a
> one-file, test-only compatibility change with no feature-selection runtime
> branch.

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/downstream-pr-worktrees/mlr3fselect-paradox2 push --set-upstream origin codex/paradox2-diagnostics
```

## mlr3pipelines

- base: `daebff3cc15257cdecde898fd3f0ceff5f320c53`
- head: `1c4bc6e52005d40d61fdba27b047f09fd6a6d29a`
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
>
> Keep the two PICV invalid-value assertions while removing their dependency on
> exact Paradox 1/checkmate wording. Focused PICV and Proxy tests pass with both
> Paradox 1.0.1 and the Paradox 2 candidate; runtime validation behavior is not
> version-branched.

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github/mlr3pipelines push --set-upstream origin codex/paradox-diagnostic-compat
```

## mlr3fda

- base: `5e6204d0d3a3c21325a71eda4402c30b31209eef`
- head: `035da5bb8d1c2ae22f04898718355e9653c382b2`
- branch: `paradox2-snapshots`
- target branch: `main`
- proposed title: `Support Paradox 2 diagnostics in FDA snapshots`

Proposed body:

> Keep the existing Paradox 1 wavelet-pipeline error snapshots byte-for-byte
> unchanged and select a Paradox-2 snapshot variant only when that major is
> installed. This is a test-only dual-version adaptation; pipeline construction
> and FDA behavior are unchanged. The Paradox-1 gate executes the legacy
> snapshots, while the Paradox-2 gate covers the new native diagnostics.

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/downstream-pr-worktrees/mlr3fda-paradox2 push --set-upstream origin paradox2-snapshots
```
