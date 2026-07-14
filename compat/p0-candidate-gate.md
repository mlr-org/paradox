# Priority-zero candidate compatibility gate

Gate date: 2026-07-13 (Europe/Berlin)

This pass was intended to test one immutable, Git-visible package source
snapshot. A final fingerprint audit found that a coordinated development
rebuild accidentally overwrote the shared candidate library at 23:03 while
the pass was active. The frozen ref and detached source worktree remained
immutable, but only the rows completed before that transition are valid results
for the frozen installation. Consumer checkout sources were never modified.

## Frozen candidate

- Git ref: `refs/paradox-compat/candidate-p0-20260713T200048Z`
- Commit: `59cec9a2b3cac5da9b777f118ed1076da2462bec`
- Tree: `11913378f80a7ab687510a434ff9dadf85f8eaae`
- Upstream parent: `06091b5b64a78807d332ec95c5cdc1aaac5899b9`
- Detached source worktree:
  `.local/compat/candidate-snapshots/59cec9a2b3cac5da9b777f118ed1076da2462bec`
- Real-index tree before and after freezing the ref:
  `abb3de86829a7e3e79aca9680d1e50da50c09dcb`

The detached worktree was clean before and after installation.  It reports
paradox `2.0.0.9000` after installation (the DESCRIPTION spelling is
`2.0.0-9000`).

The candidate was installed with:

```sh
. scripts/activate
R_LIBS="$PARADOX_ROOT/.local/compat/R/library-dependencies${R_LIBS:+:$R_LIBS}" \
  R CMD INSTALL --preclean --clean --no-multiarch --with-keep.source \
  --library="$PARADOX_ROOT/.local/compat/R/library-candidate" \
  "$PARADOX_ROOT/.local/compat/candidate-snapshots/59cec9a2b3cac5da9b777f118ed1076da2462bec"
```

Fingerprints in this ledger hash the sorted relative-path, byte-size, and
nanosecond-mtime manifest produced by:

```sh
find "$library" -type f -printf '%P\t%s\t%T@\n' | LC_ALL=C sort | sha256sum
```

The declared frozen candidate-library fingerprint recorded for every result
row is
`23a2bcc1fa04e58af0f5bd8cb4edadeda9a6a848b3f61192ea7fb02802fabd3d`.
The original child-process guard verified the normalized package path and
version, but did not recompute this fingerprint. That omission allowed a
same-version replacement to pass the guard and is corrected in the hardened
harness.

## Library isolation and fingerprints

The gate's child `.libPaths()` is, in order:

1. `.local/compat/R/library-candidate`
2. `.local/compat/R/library-mlr3verse-core`
3. `.local/compat/R/library-dependencies`
4. the activated R 4.6.1 base library

The ordinary project library is deliberately absent.  The protected library
fingerprints immediately before and after candidate installation were
identical:

| Library | Before | After |
| --- | --- | --- |
| `.local/R/library` | `d5dbb8155f22bb8f1d7949a1fa0ef9d184110cce6d4665cf7a68d66dabc0fa6d` | `d5dbb8155f22bb8f1d7949a1fa0ef9d184110cce6d4665cf7a68d66dabc0fa6d` |
| `.local/baseline-library` | `9f2f9960dcceedaa425d97509f54c280394ccc48cfe223bc000def27af55047a` | `9f2f9960dcceedaa425d97509f54c280394ccc48cfe223bc000def27af55047a` |
| `.local/compat/R/library-dependencies` | `3b435f27cc18a8bc49411b11fb703db0efb15f84c8e0ed39448f56d8794d7d05` | `3b435f27cc18a8bc49411b11fb703db0efb15f84c8e0ed39448f56d8794d7d05` |

The baseline, dependency, and overlay fingerprints remained unchanged through
the pass. The candidate fingerprint changed from the frozen value above to
`4ba0205a7ab9e2f23a66c47996c2ddfcb5c6ef43bbf8bb02a5dc4d32028e317d`;
installed files show rebuild mtimes from 23:03:28 through 23:03:35. The
replacement's portable content fingerprint is
`29aa4bb12b80fe3660bf75574c6119ac3fe1811d3e55389fb01c8b841d1cf4c1`.
The ordinary project library was also modified by separate development work. It
was excluded from every consumer child path and was proven unchanged across
this gate's own initial installation.

Using the final ledger timestamp and per-repository elapsed times gives the
following conservative provenance boundary:

- bbotk, mlr3tuning, miesmuschel, and mlr3 completed against the frozen
  `23a2...` installation.
- mlr3pipelines and mlr3learners overlap the 23:03 rebuild boundary and are
  provenance-invalid, even though their failures were independently
  reproduced and classified.
- mlr3fselect, mlr3hyperband, mlr3mbo, mlr3tuningspaces, and mlr3verse ran
  after the replacement and are not results for the frozen snapshot.

The separate `mlr3verse` hard-import overlay has fingerprint
`83da65734d43187fbe5caf51854d42070e88ed66191da9d2d0fb6945d334b12f`.
It contains only the missing hard imports needed by the meta-package gate:
gridExtra 2.3.1, mlr3cluster 0.4.1, mlr3fselect 1.6.0, mlr3hyperband
1.1.0, mlr3inferr 0.2.1, mlr3mbo 1.1.1, mlr3tuningspaces 0.6.0,
mlr3viz 0.11.0, and viridis 0.6.5.
The optional geospatial Suggests branch is excluded because `terra` cannot be
built on this host without `gdal-config`; the usable `mlr3verse` core remains
in scope.

## Bulk command

Exactly one bulk priority-zero pass was started.  `NOT_CRAN=true` is set in the
parent and reasserted in every isolated child process.

```sh
. scripts/activate
export NOT_CRAN=true
export PARADOX_CONSUMER_EXTRA_LIBS="$PARADOX_ROOT/.local/compat/R/library-mlr3verse-core"
export PARADOX_CANDIDATE_COMMIT=59cec9a2b3cac5da9b777f118ed1076da2462bec
export PARADOX_CANDIDATE_TREE=11913378f80a7ab687510a434ff9dadf85f8eaae
export PARADOX_CANDIDATE_LIBRARY_SHA256=23a2bcc1fa04e58af0f5bd8cb4edadeda9a6a848b3f61192ea7fb02802fabd3d
Rscript compat/test-repositories.R "$PARADOX_ROOT" 0 \
  "$PARADOX_ROOT/.local/compat/R/library-candidate" \
  "$PARADOX_ROOT/.local/compat/R/library-dependencies"
```

The canonical machine-readable result of this historical pass is the checked-in
fixture `compat/test-results-priority-0.tsv`. Current harnesses never overwrite
it; new results are retained below a unique `.local/compat/runs/<run-id>/`
directory.
It records each checkout commit, framework, declared and observed candidate
fingerprints, provenance, `NOT_CRAN`, elapsed time, status, classification, and
decisive error text. Passed-test output is streamed by the harness; failures
are also retained in the TSV.

## Miesmuschel secondary-index finding

The sole miesmuschel failure in the original pass is deterministic but is not
a candidate-only regression.  In natural tinytest order:

1. `test_TunerMies.R` line 6 assigns `to_tune()` values to an mlr3 learner's
   `ParamSet` and causes data.table to attach the secondary index `on__id` to a
   shared empty `.deps` table.
2. The index persists through the R6 class default.
3. `test_operatorcombination.R` line 436 compares deeply cloned operators and
   finds one nested `primed_ps$deps` with `on__id` and one without it.

The same-process upstream sequence using paradox `1.0.1.9000` reproduces the
same index and the same line-436 difference.  An isolated
`test_operatorcombination.R` passes for both versions until the shared index is
seeded.  This is therefore classified as inherited, order-dependent internal
attribute behavior.  Giving every `ParamSet` its own empty `.deps` table is a
useful rewrite hardening even though the observed failure is also present
upstream.

Exact pre-comparison objects and complete dependency-table attributes were
captured after evaluating the consumer test through line 435:

| Artifact | SHA-256 |
| --- | --- |
| `.local/compat/artifacts/p0-miesmuschel-index/candidate-seeded.rds` | `e6b7f24146c30057b25bcbe7b8d9d62c0266626611ef7c1176aae9f62be2780f` |
| `.local/compat/artifacts/p0-miesmuschel-index/candidate-seeded-attributes.R` | `6f8712503719f3eced2a55d669f92a04ae5ea3d038cc1580e55dbfdecf3a4567` |
| `.local/compat/artifacts/p0-miesmuschel-index/baseline-seeded.rds` | `c4be42297f9087f1c7f2cac0758b015b34f8f93eaf90aa301ca75cd0bac65005` |
| `.local/compat/artifacts/p0-miesmuschel-index/baseline-seeded-attributes.R` | `43971af845f7cf4ffebdd06e1ea873f91d042d516ad9484120fa814bf68eb50d` |

Both attribute dumps record an unindexed actual nested dependency table, an
`on__id`-indexed expected table, and a failing `all.equal` result.

## Harness-only consumer findings

The original pass exposed two consumer-test orchestration limitations.  Both
were reproduced independently of paradox and are classified separately from
candidate behavior.

### mlr3pipelines development help

All functional Graph, CNF, and PipeOp tests ran.  The failures in
`test_dictionary.R:176` were label checks for approximately 80 PipeOps.  A
parallel testthat worker uses `utils::help()` while pkgload has registered the
source checkout as the package path; that source directory has no installed
help database.  The lookup therefore returns an empty `help_files_with_topic`
and the PipeOp label caches `LABEL COULD NOT BE RETRIEVED`.

The isolated dictionary file passes with both candidate and upstream paradox,
because the serial pkgload process supplies its `dev_topic` help shim.  Forcing
base help gives the exact failure with both versions:

```r
pkgload::load_all(".local/compat/github/mlr3pipelines")
x = mlr3pipelines::po("adas")
help = utils::help
length(x$help()) # 0
x$label          # "LABEL COULD NOT BE RETRIEVED"
```

This is classified `harness_parallel_source_help_db`, not a paradox failure.
A future source-checkout pass should set `TESTTHAT_PARALLEL=false` so pkgload's
development-help shim remains available; this does not disable parallelism
that consumer tests explicitly exercise themselves.

### mlr3learners encapsulation libraries

The reported mlr3learners failures all said that an optional learner backend
(`glmnet`, `kknn`, `ranger`, `xgboost`, or `DiceKriging`) was not installed.
All of those packages are present in the protected dependency library.

The exact harness structure reproduces the problem: `callr::r(libpath=...)`
sets the immediate child's `.libPaths()`, but leaves `R_LIBS` empty.  The mirai
encapsulation daemon started by the consumer tests does not inherit the
immediate R process's in-memory path vector.  Exporting the same ordered paths
through both `R_LIBS` and `R_LIBS_USER` makes the candidate's complete
`test_classif_glmnet.R` pass.  This is classified
`harness_nested_process_library_env`, not a missing dependency or paradox
regression.

## Results

The single bulk pass consumed 4,277.849 repository-test seconds (71.30
minutes) and exited nonzero because it faithfully retained four classified
failures. No consumer was rerun in bulk to hide or replace a failure. Results
at or after the rebuild boundary are retained as useful provisional signals,
but are explicitly invalidated as results for the frozen snapshot.

| Repository | Status | Classification | Provenance | Seconds |
| --- | --- | --- | --- | ---: |
| bbotk | passed | passed | frozen verified | 167.187 |
| mlr3tuning | passed | passed | frozen verified | 330.932 |
| miesmuschel | failed | `upstream_inherited_order_dependent_index` | frozen verified | 191.251 |
| mlr3 | passed | passed | frozen verified | 1125.956 |
| mlr3pipelines | failed | `harness_parallel_source_help_db` | invalid: rebuild overlap | 1305.894 |
| mlr3learners | failed | `harness_nested_process_library_env` | invalid: rebuild overlap | 134.015 |
| mlr3fselect | passed | passed | invalid: post-rebuild | 280.292 |
| mlr3hyperband | passed | passed | invalid: post-rebuild | 149.643 |
| mlr3mbo | passed | passed | invalid: post-rebuild | 568.401 |
| mlr3tuningspaces | failed | `upstream_consumer_test_helper_scope` | invalid: post-rebuild | 19.098 |
| mlr3verse | passed | passed | invalid: post-rebuild | 5.180 |

Seven of 11 executions passed directly, and all four failed statuses were
reproduced without the candidate or traced to the original harness. However,
only the first four rows constitute a valid frozen-snapshot prefix. No final
claim about the complete priority-zero gate can be made from this mixed pass.
The inherited miesmuschel result still motivated a useful per-instance empty
dependency-table hardening.

The successful repositories include the complete functional mlr3 core,
feature-selection, hyperband, and MBO suites. Expected environmental skips
were retained rather than hidden: Redis/async tests in bbotk, mlr3tuning,
mlr3fselect, mlr3hyperband, and mlr3mbo; two mlr3 tests; and four
mlr3pipelines tests. The `mlr3verse` base-R hard-import core passed; only its
previously documented optional geospatial branch was excluded.

The remaining upstream mlr3tuningspaces error is a helper-scope issue. Its
`helper.R` sources mlr3 test helpers without a local environment; the
helper-defined `test_tuning_space()` later cannot resolve `expect_learner()`.
The full suite against upstream paradox `1.0.1.9000` produces the identical
sole error at `test_tuning_spaces.R:3`.

The provenance-annotated result ledger SHA-256 is
`4a745b57bb537e0793ffe57fd328e72d6cdb5f19896da1437e2a03df200644c3`.

The complete priority-zero suite must be rerun after the now-current candidate
is frozen, using a run-specific candidate library that no development command
shares. The hardened harness fingerprints package contents before every child
and after every repository, and aborts on any replacement. This mandatory
rerun is deferred until the pending native slices are incorporated; rerunning
the obsolete frozen snapshot would spend an hour without advancing the
release candidate.

Use the following inputs and command shape for that mandatory rerun, replacing
`NEW_FULL_REF` only after all intended native slices are frozen into a
Git-visible commit. The ref must remain fixed for the whole run. The recipe
prepares dependencies first, creates or verifies the detached source worktree,
and refuses to share or replace a candidate library:

```sh
. scripts/activate

candidate_ref=NEW_FULL_REF
git check-ref-format "$candidate_ref"
candidate_commit="$(git rev-parse --verify "$candidate_ref^{commit}")"
candidate_tree="$(git rev-parse --verify "$candidate_ref^{tree}")"
test "$(git rev-parse --verify 'HEAD^{commit}')" = "$candidate_commit"
test "$(git rev-parse --verify 'HEAD^{tree}')" = "$candidate_tree"
test -z "$(git status --porcelain=v1 --untracked-files=all)"
candidate_short="$(git rev-parse --short=12 "$candidate_commit")"
candidate_source="$PARADOX_ROOT/.local/compat/candidate-snapshots/$candidate_commit"
run_id="$(date -u +%Y%m%dT%H%M%SZ)-$candidate_short"
candidate_library="$PARADOX_ROOT/.local/compat/runs/$run_id/library-candidate"
dependency_library="$PARADOX_ROOT/.local/compat/R/library-dependencies"
mlr3verse_library="$PARADOX_ROOT/.local/compat/R/library-mlr3verse-core"
test ! -e "$PARADOX_ROOT/.local/compat/runs/$run_id"

Rscript compat/install-repository-test-dependencies.R \
  "$PARADOX_ROOT" 0 "$dependency_library" --run-id "$run_id"
test -d "$mlr3verse_library"

mkdir -p "$PARADOX_ROOT/.local/compat/candidate-snapshots"
if [ ! -e "$candidate_source" ]; then
  git worktree add --detach "$candidate_source" "$candidate_commit"
fi
test "$(git -C "$candidate_source" rev-parse --verify 'HEAD^{commit}')" = \
  "$candidate_commit"
test "$(git -C "$candidate_source" rev-parse --verify 'HEAD^{tree}')" = \
  "$candidate_tree"
test -z "$(git -C "$candidate_source" status --porcelain=v1 --untracked-files=all)"
test ! -e "$candidate_library"

export PARADOX_CANDIDATE_REF="$candidate_ref"
export PARADOX_CANDIDATE_COMMIT="$candidate_commit"
export PARADOX_CANDIDATE_TREE="$candidate_tree"
export PARADOX_CANDIDATE_RUN_ID="$run_id"
compat/install-candidate \
  "$candidate_library" "$dependency_library" "$candidate_source"

candidate_content="$(
  tr -d '\r\n' < "$candidate_library/.paradox-candidate-content-sha256"
)"
case "$candidate_content" in
  ''|*[!0-9a-f]*) echo "invalid candidate content sentinel" >&2; exit 1 ;;
esac
test "${#candidate_content}" -eq 64

export NOT_CRAN=true
export PARADOX_CANDIDATE_CONTENT_SHA256="$candidate_content"
export PARADOX_CONSUMER_EXTRA_LIBS="$mlr3verse_library"

Rscript compat/test-repositories.R "$PARADOX_ROOT" 0 \
  "$candidate_library" "$dependency_library" --run-id "$run_id"
```

`compat/install-candidate` writes the required portable content fingerprint
sentinel plus the ordered provenance receipt and its seal. The ref, commit, and
tree are deliberately exported before installation; the portable content hash
is read from the sentinel only after installation. The hardened harness
authenticates those values, the candidate installation run and canonical
candidate/dependency paths, the dependency-library content, the current
installer and Git-state helper, and a reproduced Git archive
before testing, and checks candidate and dependency contents around every
repository. It also requires a clean primary checkout at the candidate HEAD and
rejects Git replacements, grafts, and inherited object/config overrides. Before
making a release-gate claim, also verify that the baseline
and mlr3verse-overlay fingerprints remain unchanged and review every non-pass
row rather than merely the process exit status.
The dependency-install and checkout-test ledgers for this rerun are retained at
`.local/compat/runs/<run-id>/repository-dependencies-priority-0/` and
`.local/compat/runs/<run-id>/repository-tests-priority-0/`, respectively.
Both stages refuse an existing or symbolic evidence path before mutable work;
their `metadata/` directories bind the explicit run ID to the exact harnesses,
manifests, libraries, candidate provenance, and result hashes.
