# mlr-org organization census

`mlr-org-review.tsv` is the complete, row-by-row review of the 91 public
repositories returned by the cached GitHub organization inventory collected on
2026-07-13. The exact cached JSON input had SHA-256
`e586fa4b48e1822f300ecbedaf31edd5461478c2801723fbfe7f4af05d2ec162`.
That JSON is retained below `.cache/` as collection evidence, while the tracked
ledger freezes the source identities and decisions needed by later work.

The 44 executable-corpus repositories use the exact clean heads pinned by
`github-snapshot.tsv`. Forty-three are Paradox consumers: unchanged consumers
remain at their reviewed upstream heads, while every prepared Paradox-2
downstream branch sits at the exact local descendant recorded in
`github-bridge-provenance.tsv`. The remaining executable source is the exact
`rush` transitive dependency provider required by the prepared bbotk and
mlr3tuning heads; it is not a Paradox consumer or gate. Its complete
five-commit refresh from the original organization-scan source was reviewed,
introduces no Paradox use, and is authenticated directly by its active ledger
row rather than being mislabeled as a local bridge. The reviewed upstream
census bases for actual downstream bridges remain the rows in
`mlr-org-review.tsv`; bridge provenance binds each base identity to its
executable head and branch. The other 47 sources were downloaded separately
below `.local/compat/github-org-scan/checkouts/`; the existing executable
checkouts were not fetched, reset, cleaned, or otherwise changed. Ninety
repositories have an exact reviewed source commit, tree, committer date, and
deterministic `git archive` SHA-256. `mlr-org/docker` is a real empty
repository, so its absent commit, tree, and date are represented by `-`, its
status is `empty-no-head`, and its content receipt is the SHA-256 of the empty
byte sequence. No provenance value is invented for it.

## Scan and review method

The primary scan covers tracked `DESCRIPTION` and `NAMESPACE` files, every
tracked `.R`, `.Rmd`, and `.qmd` file, and files below any `scripts/`
directory. `scope_receipt_sha256` authenticates the ordered `git ls-tree`
records for exactly that set. The two primary match counts cover case-insensitive
`paradox` mentions and a deliberately broad set of API-shaped tokens, including
`ParamSet`, `ParamSetCollection`, old `Param*` constructors, `Domain`, `ps()`,
`p_*()`, `to_tune()`, `param_set`, `search_space`, design generation, and
`qunif`. The `all_text_*` columns are a supplemental scan of every tracked text
file, which catches lock files, READMEs, shared authoring templates, and other
mentions outside the primary scope.

Every positive result was reviewed in context. This is necessary because names
such as `ps`, `qunif`, `Domain`, `map_dbl`, `param_set`, and legacy
`ParamHelpers::makeParamSet()` are not automatically paradox use. The
`use_class`, `review_decision`, `evidence`, and `rationale` columns distinguish
direct consumers, implicit object-shape consumers, transitive templates,
predecessor systems, stale material, infrastructure, and genuine non-use.

The census found three omissions that are integrated into the separate
consumer manifest:

- `mlr3batchmark`: priority 1 package gate. Its production worker calls
  `learner$param_set$set_values()` and its tests read `$values`, despite no
  direct DESCRIPTION dependency on paradox.
- `mlr3benchmark`: priority 2 advisory documentation gate. Maintained package
  documentation assigns through `learner$param_set$values`.
- `mlr3-targets`: priority 2 advisory workload. It loads paradox and constructs
  legacy `ParamSet`/`Param*` objects.

`binder`, `mlr-outreach`, `opengeohub-summer-school-2022`, and the archived
`mlr3-learndrake` remain useful characterization evidence, but are too old or
too narrowly documentary to become release gates. The mlr-org
`mlr3resampling` fork directly consumes paradox but is superseded by the newer
`tdhock/mlr3resampling` source already pinned at priority 1. Existing reviewed
priority-3 exclusions remain exclusions.

## Reproduce the evidence

Start with the repository-local environment and the ordinary pinned consumer
corpus:

```sh
. scripts/activate
compat/fetch-github-repositories
compat/verify-mlr-org-review --fetch
```

`--fetch` creates only absent `org-scan` checkouts at their exact ledger
commits. It refuses to alter an existing checkout and refuses to populate a
missing `consumer-corpus` source. Until the user publishes the prepared
downstream branches, the already authenticated local executable checkouts,
including the exact `rush` provider, are therefore required. Once all sources
exist, the offline form is:

```sh
compat/verify-mlr-org-review
```

The verifier checks the 91-row schema and uniqueness, exact origin, clean
status, commit, tree, commit date, full source-archive SHA-256, scan-scope
SHA-256, and all four match counts. For every provenance-ledgered downstream
branch it additionally authenticates the executable snapshot
head/tree/date/branch, the complete base identity, and base-to-head ancestry
while calculating census receipts from the base commit. It also checks the
fixed partition of 44 executable-corpus sources (43 consumers and one exact
dependency provider) and 47 separately downloaded census-only sources. The
ignored raw match files are convenient review artifacts, not an input to the
verification.
