# Downstream Paradox 2 pull-request handoff

Agentic processes must not push these branches or open remote pull requests.
The two repository-local branches below are the complete reviewed migrations.
Before publication, the release gate must test these exact heads against the
frozen Paradox candidate and `compat/verify-mlr-org-review` must authenticate
their clean trees.

## bbotk

- base: `905901b45d4dd9445efc0ffa49e663ab5ae534cb`
- head: `94e4c223bce08022dbe269c79e709dd24b90714f`
- branch: `codex/public-paramsetcollection-sets`
- target branch: `main`
- proposed title: `Use public ParamSetCollection state with Paradox 2`

Proposed body:

> Paradox 2 makes the private layout of `ParamSetCollection` opaque. Read the
> optimizer-chain child values through the existing public `$sets` binding and
> add a regression for that public contract. Keep the same tests useful with
> Paradox 1 and 2 by selecting the expected validation diagnostic by installed
> Paradox major version; no runtime behavior is version-branched.

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github/bbotk push --set-upstream origin codex/public-paramsetcollection-sets
```

## miesmuschel

- base: `7aaca22d2fc61d8d86291b681a8ecbde21f649c5`
- head: `d9d5c01ab069839abf4f82521826ac61e31191de`
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
> continue to construct the legacy class.

After the exact-head release test passes, publish it manually with:

```sh
git -C /home/mewse/paradox_neo/.local/compat/github/miesmuschel push --set-upstream origin codex/paradox-paramsetshadow-bridge
```
