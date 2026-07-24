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

## Local downstream branches

The Paradox-2 migrations and dual-version test adaptations are prepared in the
repository-local worktrees recorded in
[`design/release-2.0.0.md`](../design/release-2.0.0.md). They include the
official miesmuschel `ParamSetShadow` bridge, bbotk public-state and native
owner-root repairs, mlr3mbo's public transform-stripping subset call, and small
diagnostic-test adaptations in affected maintained packages, together with the
independent mlr3pipelines clone-ownership repair that Paradox 2 exposed. Release
compatibility must retest every exact branch head against the exact frozen
candidate; focused development tests are not release evidence.

The executable [`github-snapshot.tsv`](github-snapshot.tsv) pins every exact
reviewed downstream head and branch selected for the consumer run. The
organization census remains a review of the upstream sources from which those
branches started;
[`github-bridge-provenance.tsv`](github-bridge-provenance.tsv) binds each
upstream commit/tree/date/branch to its bridge commit/tree/date/branch.
`compat/verify-mlr-org-review` authenticates both endpoints, requires the base
to be an ancestor of the bridge, and performs the census scan against the base
bytes. The ordinary dependency and repository runners execute only the bridge
heads selected by the snapshot.

The frozen release candidate also has one downstream-only refresh profile,
`release-refresh-20260720`, declared in
[`downstream-evidence-profiles.tsv`](downstream-evidence-profiles.tsv). Its
separate snapshot/provenance files bind all eight exact reviewed heads. The
repository stage runs the complete priority-zero/one corpus once; the
additional exact source-package checks select bbotk, miesmuschel, mlr3mbo,
celecx, and mlr3fda. The reviewed mlr3, mlr3pipelines, and mlr3fselect heads
remain authenticated support packages in the ordered install overlay and are
covered by the broad repository stage. Named profiles select a separate
authenticated primary-checkout namespace. Their dependency receipt is
profile-specific, axis-neutral, and run-local because it prepares only the
unchanged external dependency closure; refreshed mlr3mbo is supplied by the
ordered bridge overlay. The refresh dependency manifest is deliberately
limited to the eight overlay packages rather than replaying unrelated
consumers. The `paradox2`/`paradox1` axis registry pins exact candidate
ref/commit/tree/version tuples and creates distinct overlay, lock,
repository-test, full-check, and completion paths. These paths never overwrite
or relabel default full-corpus evidence. A non-default profile is post-freeze
validation tooling: it must be one clean tracked commit, and its Paradox-2 path
proves that package bytes still equal the frozen candidate.

Agents may commit and test these local branches but must not push them or open
remote PRs. The final handoff gives the user exact manual push commands and PR
text after the branches pass against the frozen candidate. The reviewed titles,
bodies, exact heads, and commands are retained in
[`downstream-pr-handoff.md`](downstream-pr-handoff.md).

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
`metadata/` tree. Overlay activation preserves the ordinary documentation and
runtime contract: its exact managed `PATH` prefix is TinyTeX, Quarto, the
top-level toolchain, `.local/bin`, P1, then GEO. It therefore must not replace
the activated `.local/toolchain/bin/R` or `Rscript`, nor let conda TeX programs
shadow the authenticated TinyTeX tools. On an installed Linux checkout, audit
that contract with:

```sh
PARADOX_COMPAT_TEST_INSTALLED_ROOT="$PARADOX_ROOT" \
  scripts/environment/test-reverse-activation-contract
```

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
reverse_dependency_run_id="$(date -u +%Y%m%dT%H%M%SZ)-reverse-deps-p1"
dependency_library="$PARADOX_ROOT/.local/compat/R/library-dependencies"
test ! -e "$PARADOX_ROOT/.local/compat/runs/$reverse_dependency_run_id"

Rscript --vanilla compat/install-reverse-dependency-dependencies.R \
  --root "$PARADOX_ROOT" \
  --max-priority 1 \
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
test "$(git rev-parse --verify 'HEAD^{commit}')" = "$candidate_commit"
test "$(git rev-parse --verify 'HEAD^{tree}')" = "$candidate_tree"
test -z "$(git status --porcelain=v1 --untracked-files=all)"
candidate_short="$(git rev-parse --short=12 "$candidate_commit")"
run_id="$(date -u +%Y%m%dT%H%M%SZ)-$candidate_short"
candidate_source="$PARADOX_ROOT/.local/compat/candidate-snapshots/$candidate_commit"
candidate_library="$PARADOX_ROOT/.local/compat/runs/$run_id/library-candidate"
dependency_library="$PARADOX_ROOT/.local/compat/R/library-dependencies"
test ! -e "$PARADOX_ROOT/.local/compat/runs/$run_id"

# Reconcile priority-zero/one GitHub consumer dependencies before the candidate
# and protected-library hashes are taken. The dependency stage is immutable;
# preparing only P0 here cannot later be extended for the mandatory P1 gate.
Rscript compat/install-repository-test-dependencies.R \
  "$PARADOX_ROOT" 1 "$dependency_library" --run-id "$run_id"

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
compile_jobs="$(scripts/environment/resource-jobs compile)"
MAKEFLAGS="-j$compile_jobs" compat/install-candidate \
  "$candidate_library" "$dependency_library" "$candidate_source"

candidate_content="$(
  tr -d '\r\n' < "$candidate_library/.paradox-candidate-content-sha256"
)"
case "$candidate_content" in
  ''|*[!0-9a-f]*) echo "invalid candidate content sentinel" >&2; exit 1 ;;
esac
test "${#candidate_content}" -eq 64
export PARADOX_CANDIDATE_CONTENT_SHA256="$candidate_content"

# Build each reviewed bridge head once from its immutable Git object. The
# candidate, dependency endpoint, exact install order, installed package bytes,
# and resource decision are sealed beside the read-only overlay.
compat/install-downstream-bridges --candidate-source "$candidate_source"
compat/install-downstream-bridges --candidate-source "$candidate_source" --verify
bridge_library="$PARADOX_ROOT/.local/compat/runs/$run_id/library-downstream-bridges"
test -d "$bridge_library"
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
dependency-library content hash unchanged across installation. Receipt schema
2 binds the candidate run ID, canonical candidate and dependency-library
paths, dependency-library content, installer, and shared Git authenticator in
addition to the source and installed package identities. The dedicated
candidate library may contain only the package, sentinel, receipt, and seal.
Installation and the compatibility/documentation gates authenticate the clean
detached candidate source at the declared commit and tree. They reject
replacement refs, grafts, alternate object stores, external archive
attributes, hidden index flags, filesystem-monitor/untracked-cache shortcuts,
and inherited repository-altering `GIT_*` variables. Unrelated work in the
primary checkout is allowed after the candidate is frozen. The differential
driver remains an exception when it snapshots the primary checkout. The sealed
benchmark instead requires a clean committed validation-tooling checkout and a
separate exact managed candidate worktree; it records both identities rather
than requiring tooling `HEAD` to equal the candidate. Every
compatibility and documentation release gate authenticates
the receipt against the full ref, commit, tree, installed version, content
hash, current installer, and a freshly reproduced source archive.

Never reuse `.local/compat/R/library-candidate` or another development library
for release evidence. The candidate path must be new and run-specific, and no
development command may install into it while a gate or benchmark is running.
The downstream bridge overlay follows the same rule. It is built once in the
candidate run by `compat/install-downstream-bridges`, in the fixed dependency
order bbotk, mlr3, miesmuschel, mlr3pipelines, mlr3fselect, mlr3mbo, and
celecx, and mlr3fda. Its read-only verifier reauthenticates the candidate and
dependency receipts, priority-one dependency preparation, reviewed Git
objects, sealed package ledger, complete installed-library fingerprint,
evidence verifier, and resource-scheduler bytes without loading a bridge
package. One run-local owner serializes construction; atomic no-clobber
publication and owner plus device/inode-gated cleanup prevent a losing process
from deleting a raced replacement. Repository, documentation, and
release-benchmark entrypoints fail unless their exact overlay verifies. The
active named refresh uses schema 3 and a profile/axis suffix; schema 2
describes only the historical default overlay.

For a named downstream-only refresh, choose a new candidate-bound run, prepare
the profile dependency receipt before installing the candidate, and pass the
same profile and axis to every later command:

Before the run, populate the profile's `checkout_namespace` as standalone
primary Git checkouts at the exact snapshot heads. Do not switch the default
`.local/compat/github` corpus: that corpus remains the dependency-input and
historical-default source. The bridge installer audits each profile checkout's
origin, primary `.git` store, dangerous config, graft/alternate/attribute
inputs, hidden index flags, clean branch/HEAD/date/tree, and declared ancestry
before producing any archive.

```sh
profile=release-refresh-20260720
axis=paradox2                    # broad current-candidate gate
Rscript compat/install-repository-test-dependencies.R \
  "$PARADOX_ROOT" 1 "$dependency_library" --run-id "$run_id" \
  --evidence-profile "$profile"
# install-candidate exactly as above, then export its content sentinel
compat/install-downstream-bridges --candidate-source "$candidate_source" \
  --evidence-profile "$profile" --paradox-axis "$axis"
bridge_library="$PARADOX_ROOT/.local/compat/runs/$run_id/library-downstream-bridges-$profile-$axis"
export PARADOX_CONSUMER_EXTRA_LIBS="$bridge_library:$mlr3verse_library"
Rscript compat/test-repositories.R "$PARADOX_ROOT" 1 \
  "$candidate_library" "$dependency_library" --run-id "$run_id" \
  --candidate-source "$candidate_source" --evidence-profile "$profile" \
  --paradox-axis "$axis" --jobs 1
compat/check-downstream-profile --candidate-source "$candidate_source" \
  --evidence-profile "$profile" --paradox-axis "$axis" \
  --repositories bbotk,miesmuschel,mlr3mbo,celecx,mlr3fda
```

Use a distinct run/library/overlay with `axis=paradox1` for the released
Paradox-1 compatibility axis. On that axis, add
`--repositories bbotk,miesmuschel,mlr3mbo,celecx,mlr3fda` to the
`test-repositories.R` command and retain the same five-package
`check-downstream-profile` selection. Do not repeat the complete consumer
corpus, reverse-dependency, documentation, differential, or benchmark gates on
Paradox 1.

The full-check stage records and reauthenticates an ordered content manifest
for the candidate package, every configured extra library (including the
bridge and `mlr3verse` libraries), and the dependency library. A retained
`00check.log` with an `ERROR` or `WARNING` is a failed row even if that R
version returns process status zero; NOTE-only checks remain separately visible
and are accepted by this focused harness. Each exact Git archive is first
processed by repository-local `R CMD build`; the retained build log and package
tarball hash are authenticated before that tarball, rather than the raw source
directory, is passed to `R CMD check`. Besides matching CRAN's source-package
boundary, this ensures that `Authors@R`-only metadata is expanded before R 4.6
validates the package.

The P1 run uses an exact Paradox-1 Git ref with the unchanged candidate
installer, so its normal receipt still binds source, archive, installed bytes,
dependency endpoint, and run-local library. Profile/axis metadata in the
overlay and repository stage states explicitly that the subject is Paradox 1;
the independent clean tooling commit supplies only the post-freeze harness and
review manifests. The dependency stage can be shared only within that one run;
the actual P1 and P2 release evidence uses distinct run IDs. Never reuse the
Paradox-2 candidate library for that axis.

## Source-package reverse-dependency gate

Run the pinned CRAN and Bioconductor source-package gate against that immutable
candidate. A new reverse-run ID is required because retained evidence is never
overwritten. The protected dependency library must already contain the hard
dependency closure for the selected source packages; this harness deliberately
does not mutate that library, and classifies missing dependencies explicitly.
Start from ordinary activation and source the verified system overlay when the
selected Linux packages require it; do not edit `PATH` manually. The harness
requires the exact top-level `.local/toolchain/bin/R` and `Rscript`, the
authenticated TinyTeX tools, and the local `texi2dvi`:

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
  --candidate-source "$candidate_source" \
  --candidate-content "$candidate_content" \
  --run-id "$reverse_run_id"
```

The gate verifies the manifest, every pinned CRAN MD5 and SHA-256, and every
pinned Bioconductor SHA-256 before running isolated `R CMD check` processes.
The fixed candidate is authenticated through its clean detached source
worktree, so unrelated development in the primary checkout does not invalidate
an in-flight run. Each process gets a writable row-local home, temporary tree,
Python/reticulate/R/compiler caches, `NOT_CRAN=true`, and the candidate before
the shared dependency library.

The complete candidate and dependency libraries are content-hashed exactly
twice in an actual stage: once at stage start and once at final postflight.
Every execution-wave boundary instead compares a non-following, path-bound metadata
fingerprint containing type, mode, size, mtime, ctime, device, inode, link
count, hard-link identity, and symbolic-link text. `--plan-only` and the
synthetic self-test perform zero protected-library content hashes. TinyTeX's
complete archive/tree authentication remains owned by its bootstrap receipt;
this consumer gate rechecks only the exact tools it executes.

Consumer installation is separate from checking and content-addressed below
`.local/compat/reverse-cache/install/`. Its key contains only inputs that can
affect installed bytes: authenticated archive and candidate/dependency
content, the exact install worker/command/environment, R configuration,
compiler/build-tool bytes, platform, Makevars, locks, and active overlay
receipts. Runner, verifier, report, plan, and row-order changes are excluded.
Every cache is fully sealed and semantically reauthenticated immediately before
the check child starts; an invalid cache is quarantined and rebuilt once.
Concurrent same-key builders stage privately; exactly one atomically promotes
the cache and every loser authenticates and reuses that completed target.

Independent rows run in conservative bounded waves. Immediately before every
wave, `scripts/environment/resource-jobs consumer --report` recomputes the
live CPU-affinity, cgroup, and available-memory ceiling; its exact report is
retained with that wave. The automatic ceiling is at most four heavyweight
consumers and fails closed if even one would invade the memory reserve.
`PARADOX_REVERSE_JOBS=N` may only lower the current automatic and retained
limits. Every child has a separate process, install library, home, work tree,
and caches. Nested make/CMake, testthat, `parallel`/`future`, OpenMP, BLAS, and
related thread pools are normally fixed at one and recorded in command
evidence and the install-cache key. The `mlr3` R CMD check child alone exposes
its allotted two logical CPUs for the upstream worker-contract assertions:
`MC_CORES`, the future and parallelly available-core fallbacks, and the two
OpenMP limits are 2. Installation, the outer worker, make/CMake, testthat,
BLAS, Rcpp, and every non-`mlr3` check remain at one; the sequential future
plan and disabled-fork policy are unchanged.

The parent waits for every process in a wave before checking protected inputs.
It alone writes the deterministic wave and acceptance ledgers, seals successful
worker outputs, and promotes rows in plan order. Ordinary consumer failures are
complete accepted rows; an unexpected worker failure remains unsealed with its
diagnostics while successful siblings from that completed wave are retained.
Every external task atomically publishes a launch receipt before invoking R and
a completion receipt only after its identity-bound result exists; resume uses
those receipts to distinguish completed siblings, failed workers, and tasks
that were never started. Process-group and token cleanup bound interruption
even when nested tools create new sessions.
`--no-stop-on-test-error` collects each package's complete bounded test-script
batch, and later packages continue. Counts are `exact`, `partial`, or
`unavailable` rather than manufactured zeros. If interrupted, resume the same
selection and identity with:

```sh
Rscript --vanilla compat/test-reverse-dependencies.R \
  --root "$PARADOX_ROOT" --max-priority 1 \
  --candidate-library "$candidate_library" \
  --dependency-library "$dependency_library" \
  --candidate-ref "$candidate_ref" --candidate-commit "$candidate_commit" \
  --candidate-tree "$candidate_tree" --candidate-source "$candidate_source" \
  --candidate-content "$candidate_content" --run-id "$reverse_run_id" \
  --resume
```

Every semantically verified accepted row is skipped, whether it records a pass
or a complete bounded failure; only incomplete attempts are moved to sealed
interrupted diagnostics and rerun. The run-level composite seal binds
row manifests and seals instead of rehashing every retained `Rcheck` artifact.
Verify a completed run with:

```sh
Rscript --vanilla compat/verify-reverse-dependency-evidence.R \
  "$PARADOX_ROOT/.local/compat/reverse-runs/$reverse_run_id"
```

Use repeated `--package NAME` selections and `--plan-only` for a bounded
checksum/provenance preflight. Missing R dependencies and missing system
dependencies have explicit result classifications; other failures remain
candidate-or-consumer failures rather than being silently waived. Exercise the
economy, bounded-concurrency, cache-promotion race, complete-wave failure,
parent-only acceptance, tamper, count, and resume contracts without real
consumers or large hashes with `Rscript --vanilla
compat/test-reverse-dependencies-self-test.R`.

Before the full run, repeat the command above with a fresh preflight run ID and
append `--package miesmuschel --plan-only`. This is the required practical
one-package smoke after a candidate is refrozen: it exercises the real reverse
harness and command-resolution preflight without creating a check stage or
mutating either protected library.

`github-repositories.tsv` records the executable subset selected from the
organization-wide scan plus any explicitly reviewed external dependency
providers. `mlr-org-review.tsv` and its companion review explain
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
mandatory priority-zero/one checkout gate with the exact candidate-specific
downstream bridge library and reviewed hard-import-only `mlr3verse` overlay
explicitly present in the child library path, in that order after the candidate
library:

```sh
mlr3verse_library="$PARADOX_ROOT/.local/compat/R/library-mlr3verse-core"
test -d "$bridge_library"
test -d "$mlr3verse_library"
export NOT_CRAN=true
export PARADOX_CONSUMER_EXTRA_LIBS="$bridge_library:$mlr3verse_library"
export PARADOX_CANDIDATE_SOURCE="$candidate_source"

Rscript compat/test-repositories.R "$PARADOX_ROOT" 1 \
  "$candidate_library" "$dependency_library" --run-id "$run_id"
```

The dependency library is `.local/compat/R/library-dependencies`; the candidate
is installed into a run-specific library, which is first on every isolated
consumer test process's library path. The installer writes an ordered
provenance receipt and SHA-256 seal beside that library. Both files must be
regular, non-symbolic files. The harness requires their exact schema, checks
the receipt against the candidate installation run, canonical candidate and
dependency paths, dependency content, full candidate ref, commit, tree,
installed version, and installed package-content hash, authenticates the
current installer and Git-state helper, and reproduces the source tar
byte-for-byte with `git archive`. `PARADOX_CANDIDATE_SOURCE` (or
`--candidate-source`) must name a clean, detached linked worktree at that exact
ref, commit, and tree. The primary checkout may contain unrelated development
work; it is never substituted for the frozen source. Replacement refs, grafts,
alternate object stores, redirected attributes, hidden index state, partial
clone inputs, symbolic tree members, and source dirt including ignored files
fail closed. It then verifies every selected checkout against
`github-snapshot.tsv`, including its origin, commit, and tree; and
requires the successful, sealed sibling dependency-preparation stage for the
same run ID and priority. The dependency stage's reviewed manifests, selected
repositories, local R, harnesses, result ledger, and final dependency-library
content hash must all match the checkout-test preflight. Every cloned checkout
that exposes a package `DESCRIPTION` and is therefore eligible for the local
resolver fallback has a separate pinned preflight and postflight receipt. Both
receipts bind its package, repository, origin, commit, and clean worktree. The
test harness authenticates those provider receipts while retaining the selected
consumer rows as a separate exact binding. Tests execute from two freshly
extracted copies of a deterministic archive of the pinned commit, so local
consumer dirt is neither executed nor silently incorporated. Optional
third and fourth script arguments select the candidate and dependency
locations; `--run-id ID` is mandatory. The resumable interface also accepts
`--candidate-source`, `--candidate-origin`, `--repositories`,
`--timeout-seconds`, and lowering-only `--jobs` controls. `--plan-only`
validates source,
candidate, dependency-evidence, and dynamic row selection without creating the
test stage and without computing any protected candidate/dependency/overlay
tree fingerprint. `--verify` reopens a completed stage and performs its
semantic and sealed-artifact verification without rerunning tests. Plan-only
behavior is controlled only by that command-line flag;
inherited environment variables cannot silently turn the release command into
a successful plan.

The general runner has no fixed priority split or row count. Each row has
append-only `attempt-NNNNNN/` evidence and is promoted only after archive,
environment, structured-count, mutation-delta, provenance, and status
semantics pass. Its small `accepted.tsv` pointer makes a later invocation skip
that row; an interrupted or invalid attempt is retained or quarantined and a
new attempt is used. Ordinary consumer failures are accepted as factual row
results and the remaining consumers still run. Only a provenance/infrastructure
failure stops promotion. Testthat runs with `stop_on_failure = FALSE`, so a row
reports all reachable failures rather than only the first one.

Independent rows run in bounded external-`Rscript` waves. Immediately before
each wave the runner retains a fresh `resource-jobs consumer` decision;
`--jobs N` may only lower that live and initially retained ceiling. Every
worker has isolated mutable state and normally forces nested make, CMake,
testthat, `parallel`/`future`, BLAS, and OpenMP work to one thread. The `mlr3`
row alone receives a receipt-bound two-CPU exception for its explicit worker
contract tests; make, CMake, testthat, BLAS, Rcpp, and every other repository
remain capped at one within the scheduler's two-CPU row allocation. The parent
waits for every sibling, seals the complete wave, and promotes accepted rows in
deterministic plan order. Restarting the same unfinished run reuses every
semantically verified accepted row, including successful siblings retained
before a later row failed, instead of executing it again.

Candidate package, complete candidate library, dependency library, and every
ordered extra library receive a full content fingerprint exactly at the stage
start and final boundary. Between boundaries the runner uses exhaustive
non-following metadata checks and small authenticated sentinels; it does not
rehash every package tree around every row. Every row receives disposable
HOME, temporary, XDG, R cache, Python bytecode/user, pip/uv, reticulate,
matplotlib, ML-framework, CUDA, WEKA, and ccache roots. Those roots and both
writable source extractions are deleted before the attempt is sealed, so a
later row cannot reuse consumer compilation or cache state accidentally.

Results, elapsed times, pinned commits/trees, frameworks, candidate versions,
the two boundary fingerprints, semantic test counts, `NOT_CRAN`,
classifications, mutation deltas, logs, and exact archived source proofs live
below `.local/compat/runs/<run-id>/repository-tests-priority-<N>/rows/` and
`completions/`. `inputs/` retains the dynamic selection, exact harnesses,
fingerprint implementation, detached candidate proof, environment, and start
boundary. A deterministic manifest covers every regular file in the completed
stage, and `metadata/completion.seal` authenticates that manifest. Perform the
full repository-specific verifier with:

```sh
Rscript --vanilla compat/test-repositories.R "$PARADOX_ROOT" 1 \
  "$candidate_library" "$dependency_library" --run-id "$run_id" \
  --candidate-source "$PARADOX_CANDIDATE_SOURCE" --verify
```

`compat/verify-repository-evidence.R` remains the generic outer manifest/seal
check. The repository-specific verifier additionally reconstructs every
accepted row's semantics and proves its archive against the pinned Git tree.
Before a release run, exercise the reusable row scheduler, detached-candidate
authentication, bounded parallelism, complete-batch failure collection, and
resume semantics without touching real consumers:

```sh
Rscript --vanilla compat/test-repository-runner.R
```

This is a synthetic harness regression only; it is not consumer evidence.
The prior monolithic schema-3 body remains in `test-repositories.R` only to
interpret historical local evidence when
`PARADOX_REPOSITORY_LEGACY_SCHEMA3=true`; never set that switch for a new
candidate or relabel its evidence as the resumable schema.

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
file retains pkgload's development-help shim. Default nested `parallel` and
`future` plans are also forced sequential inside each bounded outer worker.
The sealed base child environment also carries the repository-local build
`PATH` and activated `R_MAKEVARS_USER`. When the compatibility-system overlay
is active, its declared restricted child `PATH` is used; the interactive
activation `PATH` may retain additional host entries. Both paths are validated
to contain the local toolchain. This is required because an explicit
`processx` environment replaces, rather than augments, the parent environment
on Unix; without these entries a source-checkout test that invokes `pkgload`
cannot find the selected compiler.
The receipt-bound `mlr3` row advertises its allotted two logical CPUs because
its tests assert that worker contract; it still starts from a sequential
future plan. Other consumer tests may explicitly install their own reviewed
plan when parallel behavior is itself the subject of the test.

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
responsibility of its priority-level run. Rows whose relation is `Dependency`
are eligible only for this pinned fallback map: they are not paradox consumer
test or documentation targets. This currently retains `fastshap` for
`mlr3summary`, because CRAN archived `fastshap` after the consumer snapshot was
reviewed.
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
Give it a new evidence ID and supply the candidate-specific exact downstream
bridge first, followed by the `mlr3verse` hard-import library and the locked
documentation-only library containing `gt`, `V8`, `bigD`, and `juicyjuice`:

```sh
documentation_run_id="$run_id-documentation"
documentation_extra_library="$PARADOX_ROOT/.local/compat/R/library-documentation-extra-final3"
test ! -e "$PARADOX_ROOT/.local/compat/runs/$documentation_run_id"
test -d "$bridge_library"
test -d "$mlr3verse_library"
test -d "$documentation_extra_library"
unset PARADOX_CONSUMER_EXTRA_LIBS

Rscript compat/test-documentation \
  --root "$PARADOX_ROOT" \
  --candidate-library "$candidate_library" \
  --candidate-source "$candidate_source" \
  --dependency-library "$dependency_library" \
  --evidence-profile "$profile" \
  --paradox-axis "$axis" \
  --extra-library "$bridge_library" \
  --extra-library "$mlr3verse_library" \
  --extra-library "$documentation_extra_library" \
  --run-id "$documentation_run_id" \
  --scope all \
  --timeout 7200
```

`--timeout` is expressed in seconds. The harness passes that value unchanged
to `processx` for every retained command and records the same value as
`timeout_seconds` in the sealed metadata; it is never converted to
milliseconds by the harness.

`--scope all` runs `full` followed by `essential`. The full mlr3book render,
full mlr3website render, all four current cheatsheets, mlr3gallery's legacy
14-post corpus, the maintained mlr3benchmark nested-values example, and a
2,048-row real-space workload are retained advisory probes. The essential
scope makes the book's advanced paradox chapter, the website's paradox
benchmark, the current tuning and pipelines cheatsheets, and an explicit legacy
upgrade followed by 128-row design, quantile, subset, transpose, and current
serialization operations on both pinned `mbo_config` ParamSets mandatory. The
workload verifies that upgrading does not mutate either legacy input object. It
also runs the three reviewed gallery posts,
the mlr3benchmark nested active-binding contract, and the modern equivalent of
mlr3-targets' pre-`ps()` legacy constructor surface as advisory evidence. A
standalone `--scope full` or `--scope essential` runs only that half. Installing
the pinned mlr3verse and the book/website helper packages into fresh run-local
overlays is itself mandatory. The removed legacy names in `mlr3-targets` and
the GPU-heavy `mlr3torch-course` remain authenticated source characterization,
not mandatory executable release gates.

The harness authenticates the candidate receipt and seal through the required
clean detached `--candidate-source`, reproduces its Git archive, verifies each
documentation/workload checkout's pinned origin,
commit, relation, and clean tree, and executes only archived run-local source
copies with the exact
repository-local R, Quarto 1.9.38, and TinyTeX. It authenticates the ordinary
toolchain's installed package set byte for byte against the explicit lock,
retains a complete toolchain-tree receipt, and fingerprints R's base library.
The candidate, dependency, extra, package-set, base-library, Quarto, TinyTeX,
and complete-toolchain content is authenticated at the full pre-workload and
post-workload boundaries. Between individual workloads the harness checks the
retained small-input hashes and filesystem metadata that would reveal mutation;
it deliberately does not reacquire every protected tree for every command. TeX
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

The cheap static economy regression verifies that per-workload boundaries do
not reacquire heavyweight whole-tree evidence and that failure classification
reads only bounded log samples:

```sh
Rscript --vanilla scripts/environment/test-documentation-economy.R
```

It is a harness self-test, not a substitute for `compat/test-documentation`.

## Final sealed benchmark

Performance evidence uses the already authenticated candidate and the baseline
installation inside the successful, sealed full differential run. The release
wrapper has no workload selector: it always runs every registered workload and
both focused ParamSetCollection consumer processes. Select a new output and the
same named profile/axis whose suffixed overlay directly provides the reviewed
miesmuschel and mlr3pipelines builds:

```sh
benchmark_output="$PARADOX_ROOT/.local/benchmarks/$run_id-release"
differential_run="$PARADOX_ROOT/.local/compat/differential/runs/DIFFERENTIAL_RUN"
test ! -e "$benchmark_output"
test -d "$candidate_source"

benchmarks/release \
  --baseline-evidence "$differential_run" \
  --candidate-source "$candidate_source" \
  --candidate-library "$candidate_library" \
  --dependency-library "$dependency_library" \
  --evidence-profile "$profile" \
  --paradox-axis paradox2 \
  --output "$benchmark_output" \
  --params 64 \
  --rows 128 \
  --warmups 5 \
  --iterations 100
```

The wrapper reauthenticates the managed detached candidate source, frozen Git
ref, candidate installation, differential evidence, complete dependency-library
contents, exact profile/axis overlay, workload inventory, reviewed
regression-policy inputs, and helper bytes before and after measurement. The
validation checkout is separately required to be clean at one recorded tooling
commit; it no longer pretends that the post-freeze registry and benchmark driver
can belong to the candidate they name. Freeze that tooling commit first, build
one fresh named profile overlay with the exact same tooling identity, and reuse
it read-only for documentation, full checks, and the benchmark. The normal
verifier requires the completion and retained inputs to match current tooling;
the default unsuffixed overlay is neither selected nor rebuilt. The wrapper
retains raw samples, allocations, summaries, consumer
comparisons, command environments, candidate/differential, profile, tooling, and
downstream-bridge provenance, and one decision row for every
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
