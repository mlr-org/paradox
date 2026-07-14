# Compatibility laboratory

`reverse-dependencies.tsv` is the reviewed inventory from the CRAN package
page plus current CRAN source metadata. The latter deliberately retains direct
relationships from newly published `FoRecoML`, `interflex`, and `ggmlR` source
packages that were not yet displayed on paradox's generated CRAN page when it
was rechecked on 2026-07-14. It is not a claim that every listed package can
run fully on this host.
Priority 0 and 1 packages form the release gate; priority 2 packages are broad
compatibility probes; priority 3 packages are optional consumers whose relevant
tests are retained when their full stacks are impractical.

## Reproducible Linux system overlay

The priority-zero and priority-one Linux corpus needs geospatial libraries,
Rust, protobuf, OpenMPI, a JVM/JNI toolchain, and font libraries beyond the
ordinary package-development toolchain. These are a separate opt-in layer; the
ordinary `scripts/bootstrap` remains unchanged:

```sh
. scripts/activate
scripts/bootstrap-compat-system          # provision, then verify
# or, once the conda artifacts are cached:
scripts/bootstrap-compat-system --offline
. scripts/activate-compat-system
scripts/bootstrap-compat-system --verify
```

The authoritative inputs are
`environment/compat-system-geo-linux-64.lock` (65 artifacts) and
`environment/compat-system-p1-linux-64.lock` (95 artifacts). Both are
prefix-free explicit conda locks with an artifact SHA-256 on every row.
Provisioning writes only below `.local/compat/system/`, generates a
checkout-local `Makevars`, and seals a deterministic receipt below
`.local/receipts/compat-system/`. `--verify` is entirely read-only and
fails if either complete installed package set, a required capability, the
generated Makevars, any tracked input hash, or the receipt seal differs.

Source the overlay only for dependency preparation and consumer/documentation
workloads that require it. Native checks and differential upstream baselines
must continue from ordinary activation without the overlay. Every retained
compatibility stage records an inactive cross-platform status when the overlay
is absent. When it is active, the stage verifies it before and after the
workload and retains the receipt, seal, locks, bootstrap, activation helper,
generated Makevars, and evidence helper under the stage's authenticated
`metadata/` tree.

After activating the local environment, fetch or verify the exact checked-in
CRAN source snapshot with:

```sh
Rscript compat/fetch-cran-sources.R "$PARADOX_ROOT"
```

This writes archives below `.local/compat/cran-sources/` only after both their
MD5 and SHA-256 values and package metadata match `compat/cran-snapshot.tsv`.
It never queries current CRAN metadata or rewrites the reviewed snapshot; use
`--offline` to require every pinned archive to be present already. A missing
CRAN version is tried first at the reviewed current-source URL and then at its
deterministic per-package Archive URL; either result must have the checked-in
content hashes. GitHub
checkout and test manifests are maintained separately because reviewed
mlr-org development heads, including non-CRAN books and galleries, are part of
the compatibility surface.

The Bioconductor reverse importer is pinned separately in
`bioconductor-snapshot.tsv`; fetch it with
`compat/fetch-bioconductor-sources`.
This command has the same fail-closed and `--offline` semantics and verifies
the pinned SHA-256 value before selecting an archive. Its repository URL names
the exact Bioconductor 3.23 release rather than the mutable `release` alias.

## Prepare reverse-package hard dependencies

Prepare the hard `Depends`, `Imports`, and `LinkingTo` closure before freezing
the candidate. This is a separate stage from the GitHub checkout dependency
installer because it is derived only from the authenticated CRAN and
Bioconductor source archives:

```sh
reverse_dependency_run_id="$(date -u +%Y%m%dT%H%M%SZ)-reverse-deps-p2"
dependency_library="$PARADOX_ROOT/.local/compat/R/library-dependencies"
test ! -e "$PARADOX_ROOT/.local/compat/runs/$reverse_dependency_run_id"

Rscript --vanilla compat/install-reverse-dependency-dependencies.R \
  --root "$PARADOX_ROOT" \
  --max-priority 2 \
  --dependency-library "$dependency_library" \
  --run-id "$reverse_dependency_run_id"
```

The harness first authenticates both manifests and every selected target
archive, including CRAN MD5 values, SHA-256 values, package names, versions,
and safe archive structure. It extracts only each target's `DESCRIPTION` and
constructs a retained synthetic `deps::` resolver root containing its hard
fields. The target archive is never supplied as an installation root,
`Suggests` and `Enhances` never enter the resolver, and direct `paradox`
requirements are removed. A transitive dependency may itself be another
selected reverse package; that package is still a genuine hard dependency,
not the target being checked. An already installed bootstrap `paradox` may
satisfy those transitive package installations, but the authenticated pak lock
may reference it only as `installed`; a source/update plan fails, and its
complete package content must remain byte-identical.

Resolution produces a retained pak lock with exact versions, source URLs, and
SHA-256 values. Some CRAN-like repositories, including Bioconductor, do not
give pak a SHA-256 that it can serialize into the lock. Every checksum-less
standard source row is therefore required to expose an exact HTTPS
`PACKAGE_VERSION.tar.gz` URL. The stage downloads that archive, checks its
safe package tree and DESCRIPTION identity, retains it under
`resolved-sources/`, hashes it with MD5 and SHA-256, and inserts the retained
file as the first authenticated lock source. This policy is based on the
verifiable archive contract, not pak's repository-type label.
Installed-package references must resolve directly below the specified
repository-local dependency library; source rows without a SHA-256
or HTTPS source fail. Installation replays that lock with `update = FALSE`,
then verifies every locked version, the protected paradox tree, all target
archives, all live and retained input hashes, and the optional system overlay.
The source plan, original and synthetic descriptions, direct-dependency
ledger, lock, resolved plan, package-library endpoints, logs, and completion
metadata are sealed below
`.local/compat/runs/<ID>/reverse-dependency-dependencies-priority-<N>/`.
Verify the stage with `compat/verify-repository-evidence.R`.

Repeated `--package NAME` selects a bounded subset. `--plan-only` resolves and
seals the exact lock while requiring a pre-existing dependency library to
remain byte-identical; it does not create the library or install anything. Run
`scripts/environment/test-reverse-dependency-preparation.R` after ordinary
activation for deterministic parser, archive, lock-policy, path-escape, and
evidence-tamper fixtures, plus real plan-only success and injected post-lock
failure/retry runs. Those fixtures use a disposable library below `.local/tmp`,
verify function-scoped lock cleanup, and never inspect or mutate the shared
compatibility library.

## Freeze and install one candidate

All release compatibility commands must use one newly installed, immutable
candidate. Start from the repository root, activate the exact project R, and
populate the shared dependency library before installing the candidate. Replace
the example ref with a full, Git-visible ref that will not be moved during the
run:

```sh
. scripts/activate

candidate_ref=refs/paradox-compat/release-candidate
git check-ref-format "$candidate_ref"
candidate_commit="$(git rev-parse --verify "$candidate_ref^{commit}")"
candidate_tree="$(git rev-parse --verify "$candidate_ref^{tree}")"
candidate_short="$(git rev-parse --short=12 "$candidate_commit")"
run_id="$(date -u +%Y%m%dT%H%M%SZ)-$candidate_short"
candidate_source="$PARADOX_ROOT/.local/compat/candidate-snapshots/$candidate_commit"
candidate_library="$PARADOX_ROOT/.local/compat/runs/$run_id/library-candidate"
dependency_library="$PARADOX_ROOT/.local/compat/R/library-dependencies"
test ! -e "$PARADOX_ROOT/.local/compat/runs/$run_id"

# Reconcile P0 GitHub consumer dependencies before the candidate and
# protected-library hashes are taken.
Rscript compat/install-repository-test-dependencies.R \
  "$PARADOX_ROOT" 0 "$dependency_library" --run-id "$run_id"

mkdir -p "$PARADOX_ROOT/.local/compat/candidate-snapshots"
if test ! -e "$candidate_source"; then
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
export PARADOX_CANDIDATE_CONTENT_SHA256="$candidate_content"
```

`compat/install-candidate` accepts candidate-library, dependency-library, and
source-worktree arguments in that order. It requires the absent, run-specific
candidate path selected by `PARADOX_CANDIDATE_RUN_ID` and installs an exact
`git archive` of the declared commit, so modified
or ignored worktree bytes cannot enter the package. The three Git declarations
and the run ID must be exported before installation. The installer then writes
the portable package-content sentinel shown above, an ordered provenance
receipt, and its SHA-256 seal beside the candidate library. The sentinel is the
package-tree content hash produced by `compat/fingerprint.R`; it is not a GNU `find`/mtime
fingerprint. The installer verifies the exact local R home and proves the
dependency-library content hash unchanged across installation. Every
compatibility and documentation release gate authenticates
the receipt against the full ref, commit, tree, installed version, content
hash, current installer, and a freshly reproduced source archive.

Never reuse `.local/compat/R/library-candidate` or another development library
for release evidence. The candidate path must be new and run-specific, and no
development command may install into it while a gate or benchmark is running.

## Source-package reverse-dependency gate

Run the pinned CRAN and Bioconductor source-package gate against that immutable
candidate. A new reverse-run ID is required because retained evidence is never
overwritten. The protected dependency library must already contain the hard
dependency closure for the selected source packages; this harness deliberately
does not mutate that library, and classifies missing dependencies explicitly:

```sh
reverse_run_id="$run_id-reverse-p1"
test ! -e "$PARADOX_ROOT/.local/compat/reverse-runs/$reverse_run_id"

Rscript --vanilla compat/test-reverse-dependencies.R \
  --root "$PARADOX_ROOT" \
  --max-priority 1 \
  --candidate-library "$candidate_library" \
  --dependency-library "$dependency_library" \
  --candidate-ref "$candidate_ref" \
  --candidate-commit "$candidate_commit" \
  --candidate-tree "$candidate_tree" \
  --candidate-content "$candidate_content" \
  --run-id "$reverse_run_id"
```

The gate verifies the manifest, every pinned CRAN MD5 and SHA-256, and every
pinned Bioconductor SHA-256 before running isolated `R CMD check` processes.
Each process gets a
writable run-local check library, `NOT_CRAN=true`, and the candidate before the
shared dependency library. Candidate package, complete candidate-library, and
complete dependency-library content hashes are checked before and after every
package. Plans, copied manifests, commands, full logs, results, and completion
metadata are retained below `.local/compat/reverse-runs/`. A deterministic
whole-stage manifest and `metadata/completion.seal` cover those retained files
and can be checked with `compat/verify-repository-evidence.R`. Use repeated
`--package NAME` selections and `--plan-only` for a bounded checksum/provenance
preflight. Missing R dependencies and missing system dependencies have explicit
result classifications; other check failures remain candidate-or-consumer
failures rather than being silently waived.

`github-repositories.tsv` records the executable subset selected from the
organization-wide scan. `mlr-org-review.tsv` and its companion review explain
the decision for all 91 repositories, including the separately authenticated
source-only corpus and the verified empty `mlr-org/docker` repository. Recheck
that census against the local archives with `compat/verify-mlr-org-review`.
Fetch all non-obsolete executable or focused-workload checkouts with
`compat/fetch-github-repositories`. Rows marked `skip` remain in the inventory
to document why they are not release gates.

`differential/` contains the isolated upstream-versus-worktree behavioral
harness. It builds and installs both packages into separate run-local libraries,
captures deterministic observations in separate R processes, and writes an
auditable comparison below `.local/compat/differential/`. See
`compat/differential/README.md` for usage and normalization rules.
After fetching, run `compat/snapshot-github-repositories` to record the exact
commits used by the compatibility baseline.

## GitHub source-checkout gate

Consumer dependencies and the candidate package use separate local libraries,
so pak cannot replace the package under test with CRAN paradox. Run the
priority-zero checkout gate with the reviewed hard-import-only `mlr3verse`
overlay explicitly present in the child library path:

```sh
mlr3verse_library="$PARADOX_ROOT/.local/compat/R/library-mlr3verse-core"
test -d "$mlr3verse_library"
export NOT_CRAN=true
export PARADOX_CONSUMER_EXTRA_LIBS="$mlr3verse_library"

Rscript compat/test-repositories.R "$PARADOX_ROOT" 0 \
  "$candidate_library" "$dependency_library" --run-id "$run_id"
```

The dependency library is `.local/compat/R/library-dependencies`; the candidate
is installed into a run-specific library, which is first on every isolated
consumer test process's library path. The installer writes an ordered
provenance receipt and SHA-256 seal beside that library. Both files must be
regular, non-symbolic files. The harness requires their exact schema, checks
the receipt against the full candidate ref, commit, tree, installed version,
and installed package-content hash, authenticates the current installer, and
reproduces the source tar byte-for-byte with `git archive`. It then verifies
every selected checkout against
`github-snapshot.tsv`, including its origin, commit, and clean worktree; and
requires the successful, sealed sibling dependency-preparation stage for the
same run ID and priority. The dependency stage's reviewed manifests, selected
repositories, local R, harnesses, result ledger, and final dependency-library
content hash must all match the checkout-test preflight. Every cloned checkout
that exposes a package `DESCRIPTION` and is therefore eligible for the local
resolver fallback has a separate pinned preflight and postflight receipt. Both
receipts bind its package, repository, origin, commit, and clean worktree. The
test harness authenticates those provider receipts while retaining the selected
consumer rows as a separate exact binding, then
rechecks all protected content around every repository. It checkpoints the
result ledger after each row and aborts on a provenance violation. Optional
third and fourth script arguments select the candidate and dependency
locations; `--run-id ID` is mandatory, and `--plan-only` validates the
complete preflight without creating the run directory, test stage, or result
ledger. Plan-only behavior is controlled only by that command-line flag;
inherited environment variables cannot silently turn the release command into
a successful plan.
Results, elapsed times, pinned
commits, frameworks, candidate versions, candidate source and library
fingerprints, `NOT_CRAN`, classifications, and unrelated blockers are written
to
`.local/compat/runs/<run-id>/repository-tests-priority-<N>/test-results-priority-<N>.tsv`.
The sibling `metadata/` directory retains the exact manifests, harness,
fingerprint implementation, candidate provenance receipt and seal, installer,
ordered run metadata, and completion hashes. A deterministic manifest covers
every regular file in the completed stage, and `metadata/completion.seal`
authenticates that manifest. Verify either dependency or test evidence with:

```sh
Rscript --vanilla compat/verify-repository-evidence.R \
  ".local/compat/runs/$run_id/repository-tests-priority-0"
```

The priority-specific evidence
directory is reserved before testing and any collision, symlink, or path
escape fails closed. Each child verifies that
paradox resolves from the dedicated candidate library before it runs any
consumer code. Additional isolated overlay libraries, such as the reviewed
hard-import-only `mlr3verse` overlay, can be supplied through the
platform-separated `PARADOX_CONSUMER_EXTRA_LIBS` environment variable.

The same ordered library list is exported through `R_LIBS` and
`R_LIBS_USER`, not merely assigned to the immediate callr process. This is
required for consumer-created subprocesses such as mlr3's mirai learner
encapsulation daemons. Source-checkout testthat files run serially so every
file retains pkgload's development-help shim; this only disables testthat's
file scheduler, not parallel behavior explicitly exercised by consumer tests.

The harness runs testthat and tinytest packages with their native frameworks.
For meta-packages whose tests are ordinary `tests/*.R` scripts, it loads the
checkout and sources those scripts in fresh environments instead of silently
classifying the repository as having no tests.

Some current consumers depend on packages that are not discoverable from CRAN
or their declared additional repositories. If pak reports such a package and a
reviewed checkout in `github-repositories.tsv` provides it, the dependency
installer records the checkout commit in `local_sources`, installs that
package with hard dependencies only, and retries the consumer dependency
solve. The package's own optional development dependencies remain the
responsibility of its priority-level run.
The dependency installer also requires `--run-id ID`; its ledger and retained
inputs are written to
`.local/compat/runs/<run-id>/repository-dependencies-priority-<N>/` before the
candidate is installed. Reusing that stage name fails before the shared
dependency library can be changed. Before changing that library, the installer
strictly matches every selected checkout's origin and commit to the pinned
snapshot and requires a clean worktree. Its completion seal covers the exact
preflight and postflight checkout observations, inputs, result ledger, and
library endpoint. The same rule applies before an unselected checkout can enter
the resolver's local package-provider map, so a dirty or wrong-commit fallback
cannot modify the dependency library. The checkout-test stage refuses an
absent, failed, unsealed,
or mismatched dependency stage rather than accepting an unrelated prepopulated
library.
Absent values in result ledgers are written as `-`, keeping every TSV row at
the declared schema width and free of ambiguous trailing empty fields.

The checked-in `dependency-install-priority-0.tsv` and
`test-results-priority-0.tsv` are historical fixtures documenting the earlier
priority-zero investigation. Harnesses never update them; no checked-in ledger
is release evidence for a new candidate.

## Documentation compatibility gate

The documentation and corpus-workload harness uses the same sealed candidate
and the pinned, clean `mlr3book`, `mlr3website`, `mlr3gallery`,
`mlr3cheatsheets`, `mlr3benchmark`, `mbo_config`, `mlr3-targets`, and
`mlr3verse` checkouts.
Give it a new evidence ID and supply the reviewed `mlr3verse` hard-import
overlay:

```sh
documentation_run_id="$run_id-documentation"
test ! -e "$PARADOX_ROOT/.local/compat/runs/$documentation_run_id"
unset PARADOX_CONSUMER_EXTRA_LIBS

Rscript compat/test-documentation \
  --root "$PARADOX_ROOT" \
  --candidate-library "$candidate_library" \
  --dependency-library "$dependency_library" \
  --extra-library "$mlr3verse_library" \
  --run-id "$documentation_run_id" \
  --scope all \
  --timeout 7200
```

`--scope all` runs `full` followed by `essential`. The full mlr3book render,
full mlr3website render, all four current cheatsheets, mlr3gallery's legacy
14-post corpus, the maintained mlr3benchmark nested-values example, and a
2,048-row real-space workload are retained advisory probes. The essential
scope makes the book's advanced paradox chapter, the website's paradox
benchmark, the current tuning and pipelines cheatsheets, and 128-row design,
quantile, subset, transpose, and serialization operations on both pinned
`mbo_config` ParamSets mandatory. It also runs the three reviewed gallery posts,
the mlr3benchmark nested active-binding contract, and the modern equivalent of
mlr3-targets' pre-`ps()` legacy constructor surface as advisory evidence. A
standalone `--scope full` or `--scope essential` runs only that half. Installing
the pinned mlr3verse and the book/website helper packages into fresh run-local
overlays is itself mandatory. The removed legacy names in `mlr3-targets` and
the GPU-heavy `mlr3torch-course` remain authenticated source characterization,
not mandatory executable release gates.

The harness authenticates the candidate receipt and seal, reproduces its Git
archive, verifies each documentation/workload checkout's pinned origin,
commit, relation, and clean tree, and executes only archived run-local source
copies with the exact
repository-local R, Quarto 1.9.38, and TinyTeX. It authenticates the ordinary
toolchain's installed package set byte for byte against the explicit lock,
retains a complete toolchain-tree receipt, and fingerprints R's base library.
The package set and base library are checked before and after every command;
the complete toolchain tree is checked again at completion. The harness also
fingerprints the candidate library, dependency library, and every explicit
extra library before and after every command. It reauthenticates Quarto's
archive/installed-tree receipt and the pinned, read-only TinyTeX
archive/complete-tree receipt before and after each workload. TeX
configuration, caches, and generated fonts live only inside that retained run.
Each workload starts through an empty environment with explicit local tool and
library paths plus run-local home, temporary, and cache directories. No
ordinary development library is added implicitly. Commands, results, logs,
archived sources, run-local overlays, protected-library hashes, candidate and
source metadata, and their deterministic evidence manifest are retained below
`.local/compat/runs/<ID>/documentation/`;
`metadata/completion.seal` authenticates that complete stage. Verify it with
`compat/verify-repository-evidence.R` and review advisory rows as well as the
process status before making a documentation-compatibility claim.

## Final sealed benchmark

Performance evidence uses the already authenticated candidate and the baseline
installation inside the successful, sealed full differential run. The release
wrapper has no workload selector: it always runs every registered workload and
both focused ParamSetCollection consumer processes. Select a new output and
the library that directly provides miesmuschel:

```sh
benchmark_output="$PARADOX_ROOT/.local/benchmarks/$run_id-release"
differential_run="$PARADOX_ROOT/.local/compat/differential/runs/DIFFERENTIAL_RUN"
mies_library="$PARADOX_ROOT/.local/compat/R/library-mies-diagnose"
test ! -e "$benchmark_output"

benchmarks/release \
  --baseline-evidence "$differential_run" \
  --candidate-library "$candidate_library" \
  --dependency-library "$dependency_library" \
  --mies-library "$mies_library" \
  --output "$benchmark_output" \
  --params 64 \
  --rows 128 \
  --warmups 5 \
  --iterations 100
```

The wrapper reauthenticates the frozen Git ref, candidate installation,
differential evidence, complete dependency-library contents, workload
inventory, reviewed regression-policy inputs, and helper bytes before and after
measurement. It retains raw samples, allocations, summaries, consumer
comparisons, command environments, provenance, and one decision row for every
registered workload and focused consumer operation. Missing policy coverage or
any material regression leaves the stage failed and unsealed. The completion
metadata binds the policy hashes and reports pass/marginal/fail counts and the
worst timing and allocation ratios; inspect
`metadata/regression-decisions.tsv` for the distribution-aware bootstrap bounds
and reasons. Marginal rows are deliberately non-fatal timer-noise/review cases,
not an automatic performance claim. Compact ratios are not sufficient on their
own. Run this gate on an otherwise idle host and review every marginal row and
the raw distributions before accepting the candidate. The full threshold and
zero-allocation policy is documented in `benchmarks/README.md`.
