# Paradox C rewrite working notes

## Isolation contract

The host R installation is deliberately unsupported for development. It is R
3.6.3 and must not be upgraded or modified. Do not install packages into a user
or system library outside this repository, do not edit shell startup files, and
do not use `sudo`.

Run `scripts/bootstrap` once, then start every development shell from the
repository root with:

```sh
. scripts/activate
```

For retained release work, fail closed if that activation did not select the
exact repository-local runtime:

```sh
test "$PARADOX_ACTIVE_ROOT" = "$(pwd -P)"
test "$(command -v R)" = "$PARADOX_ROOT/.local/toolchain/bin/R"
test "$(command -v Rscript)" = "$PARADOX_ROOT/.local/toolchain/bin/Rscript"
test "$(R RHOME)" = "$PARADOX_ROOT/.local/toolchain/lib/R"
```

`scripts/bootstrap --help` is read-only; the bootstrap itself accepts no
arguments and is safe to rerun to verify or complete the pinned local state.

This selects the project-contained R 4.6.1 toolchain, sets `R_LIBS_USER` to
`.local/R/library`, and clears inherited site-library selection so `.libPaths()`
contains only that repository-local project library and the toolchain's base
library.
Activation is reparative and idempotent: every source clears inherited
R startup/library variables and compiler, linker, pkg-config, CMake, and make
search inputs before rebuilding them from the pinned prefix. It rejects
symbolic managed roots before creating cache or library directories. The
toolchain and all bulky downloaded material live below `.local/` or `.cache/`
and are ignored by Git. The prefix-free explicit conda
input `environment/toolchain-linux-64.lock` is authoritative and carries a
SHA-256 for every package artifact: bootstrap creates the toolchain from it and
compares the installed explicit package set and hashes byte for byte on every
rerun. Bootstrap also authenticates the selected micromamba executable against
the sole `bin/micromamba` member of its SHA-256-pinned archive and checks its
exact version before invoking it, including on reruns. `environment/toolchain.yml` records human-readable solver intent only
and is never solved by an ordinary bootstrap. A mismatch fails closed; remove
only `.local/toolchain` and rerun bootstrap to rebuild it from the reviewed
lock. Deliberate toolchain updates require a separately reviewed lock refresh,
not an automatic bootstrap rewrite. Apple silicon is covered by code/CI
portability work, not by this Linux bootstrap. Activation sources the
environment's compiler hooks explicitly but does not install a global
micromamba hook.

The mandatory Linux compatibility corpus has a separate, opt-in native
dependency overlay. Its prefix-free explicit inputs are
`environment/compat-system-geo-linux-64.lock` and
`environment/compat-system-p1-linux-64.lock`; every artifact URL carries a
SHA-256. The ordinary bootstrap deliberately does not install these large
stacks. After ordinary activation, provision or verify them with:

```sh
scripts/bootstrap-compat-system
scripts/bootstrap-compat-system --verify
. scripts/activate-compat-system
```

`--offline` creates an absent prefix only from the authenticated local conda
cache. `--verify` is read-only and compares the complete installed explicit
package sets with both locks, checks their direct compiler/geospatial/JVM
capabilities, and authenticates the generated checkout-local Makevars and
sealed receipt. `--verify-locks` and `--verify-receipt` are narrower
read-only audits. Activation refuses an unverified overlay and exports only
repository-local search paths. Do not use this overlay for native package
validation or upstream differential baselines: it exists to reproduce the
system dependencies of the P0/P1 Linux consumer corpus. Compatibility,
reverse-dependency, and documentation evidence records whether it was active;
active runs retain and protect the exact locks, receipt, helpers, and generated
Makevars. Apple silicon and other platforms leave the overlay inactive and
continue through their native compatibility gates.

`environment/r-packages-linux-64.lock` is likewise an input, not a snapshot of
the mutable installed library. It records the complete non-base source closure
as exact package/version/SHA-256 rows. Bootstrap downloads only those named
archives (using CRAN's current location and then its versioned Archive
location), verifies every checksum and DESCRIPTION, resolves no dependencies
from live repository metadata, and installs only missing or mismatched locked
rows. It parses every hard `Depends`, `Imports`, and `LinkingTo` version
constraint and verifies it against the selected R, base packages, and locked
package versions. Unrelated extra packages are outside this narrow lock. The
CRAN paradox 1.0.1 row is an authenticated bootstrap-only archive input: it is
installed to seed a fresh library, but an already installed development paradox
is preserved only when its version satisfies every locked constraint and is
never overwritten to repair an incompatible version.
The specification also carries the libgit2, libxml2, and GLPK development
files needed by compiled optional dependencies in the mlr3 compatibility
corpus. Do not satisfy those builds from `/usr`; that would make consumer-test
results depend on the host image.
PDF manuals and vignettes use the checksummed full TinyTeX 2026.07 distribution
installed at `.local/tinytex`; activation places its `pdflatex`, `kpsewhich`,
and `makeindex` before conda and host binaries. The complete distribution is
authenticated by a tree receipt and made read-only. Mutable TeX state is
redirected below `.local/texmf` interactively and into each retained check run.
HTML checks use the pinned local HTML Tidy. Neither tool reads from or installs
into the host TeX tree.
The same bootstrap installs Quarto 1.9.38 below `.local/quarto`. Its official
archive SHA-256 and complete extracted-tree manifest digest are pinned in
`environment/quarto-linux-x86_64.tsv`; `scripts/bootstrap-quarto --verify`
rechecks the archive, input receipt, seal, executable version, and every tree
member without network access. Activation exposes that exact executable, and
documentation gates call it by its repository-local absolute path. Do not use
a Quarto binary from HOME, `/usr`, an editor, or a mutable container tag.
Temporary files and caches for R, pak, pip/uv, ccache, and reticulate are also
redirected below the repository. `HOME` is intentionally left unchanged so Git
credentials continue to work; do not allow a tool to install into HOME.
Ordinary activation also clears inherited `XDG_RUNTIME_DIR` and
`CCACHE_TEMPDIR`, selects `.local/runtime` and `.local/tmp/ccache`, and
repairs the runtime directory to mode 0700. This prevents ccache's runtime
temporary files from escaping into a user-wide XDG directory.
Containerized CRAN auxiliary checks must be invoked through
`scripts/podman-local`. Bootstrap installs the SHA-256-pinned Podman 5.8.2
static bundle, including crun, runc, conmon, pasta, netavark, aardvark-dns,
rootlessport, and catatonit, below `.local/podman`. The wrapper verifies the
whole extracted tree, invokes every runtime/helper by a project-local path, and
isolates HOME, XDG state, configuration, authentication, the vfs image store,
run root, and temporary files below `.local/containers`; invoking plain
`podman` or `docker` bypasses these guarantees. `/usr/bin/newuidmap`,
`newgidmap`, and `nsenter` are explicit read-only host bridges: the setuid and
kernel namespace behavior of the first two cannot safely be reproduced by
copying them into the repository.
The two reference images are content-addressed in
`environment/auxiliary-images.tsv`; never substitute a mutable `latest` tag in
a retained validation result. `scripts/fetch-auxiliary-images` pulls those
exact images into the repository-local store.

The ordinary profile deliberately matches normal user warning behavior for
reverse-dependency fidelity. Paradox-only adversarial jobs set
`R_PROFILE_USER=environment/Rprofile-strict.R`; do not use that strict profile
to interpret consumer warnings.

Run `scripts/fetch-reference-sources` to download the checksum-pinned R source
releases in `environment/r-api-sources.tsv`, the Writing R Extensions, R
Internals, and R Installation and Administration manuals, a reference checkout
of data.table, and pinned rchk/R-hub container sources used by the auxiliary
memory gates. Each R extraction is checked against the complete tree digest in
`environment/r-api-source-trees.tsv`. The three Git worktrees must have their
reviewed origins and be exactly clean, including untracked and ignored files,
before the command may move them to their exact detached commits. The command
never discards checkout contents; move local material aside or recreate a
dirty checkout before rerunning it. Manual PDFs are generated offline from the pinned R
4.6.1 source with the authenticated repository-local R and TinyTeX, never
downloaded from CRAN's mutable `r-release` paths. `--offline` reauthenticates
and repairs the local material without fetching, but fails if an archive,
checkout, or commit is absent. The PDF build takes its Texinfo macro, language,
and index inputs only from the source archive pinned in
`environment/texinfo-source.tsv`; it never mutates the authenticated TinyTeX
tree. The same manifest authenticates the small static BusyBox awk used by
Texinfo, so the manual build never falls back to the host's awk. The R set
includes the declared minimum (4.3.0),
the last release
before the public attribute-API transition (4.4.0), the staggered transition
release where `NO_ATTRIB` exists but `R_hasAttrib()` and `R_getAttribCount()` do
not (4.5.2), and the development runtime (4.6.1). The C interface rules in
those local sources and manuals take
precedence over remembered behavior.

## Development invariants

- Preserve the exported R API and the widely observed R6 object shape unless a
  compatibility break is explicitly documented and covered by a regression
  test.
- Add characterization tests before replacing behavior with C. Consumer tests
  belong in the package when they describe generally useful implicit behavior.
- Native entry points are registered; dynamic symbol lookup stays disabled.
- Every allocated R object is protected across any call that may allocate.
- Do not cache a `SEXP` across garbage collections unless it is preserved and
  released deliberately. Do not mutate shared R objects.
- Use the public R C API only in shipped code. Reference R and data.table source
  may inform compatible object construction, but do not copy internal APIs.
- Keep C portable C17. R 4.6 exposes explicit C17 and C23 toolchain modes but
  no longer exposes a C11 mode. The package selects it through
  `SystemRequirements: USE_C17` and therefore depends on R 4.3 or newer; a
  `C_STD` assignment in `src/Makevars` does not select the standard on current
  R. No x86-only intrinsics; Apple silicon is a first-class target.
- Treat warnings under both GCC and Clang as defects. Tests, `R CMD check`,
  sanitizer runs, and Valgrind runs must be clean before release claims.
- Never optimize from synthetic timings alone: retain representative benchmarks
  derived from real reverse dependencies and record their inputs and results.

## Repository layout used by the migration

- `src/`: shipped C implementation and registration.
- `tests/testthat/`: unit, compatibility, regression, and adversarial tests.
- `benchmarks/`: reproducible performance cases and recorded baselines.
- `compat/`: checked-in manifests and test orchestration; cloned consumer repos
  themselves stay under `.local/compat/`.
- `design/`: architecture and compatibility decisions.
- `.local/sources/`: ignored R, data.table, rchk, and check-container reference
  source trees.
- `.local/compat/`: ignored CRAN and GitHub consumer checkouts.
- `.local/compat/system/`: ignored, lock-reproducible Linux-only native
  dependency overlay for the mandatory compatibility corpus.

## Native validation commands

After activation, `scripts/native-check --help` lists the isolated native
profiles. There is intentionally no default mode. A typical development gate
is:

```sh
scripts/native-check --mode strict-gcc --mode strict-clang --tests focused
```

`--mode static` additionally runs a GCC `-fanalyzer` package build, Clang 22's
static analyzer over every C translation unit, exhaustive cppcheck, and the
registration/ELF export audit. The Clang analyzer retains one plist and log per
source file and rejects either textual warnings or nonempty diagnostics.
`--mode all` adds separate Clang ASan and UBSan package-DSO builds; their
retained scope files explicitly mark them as non-final because the local
release R executable is not sanitizer-built. Every run operates on a
hash-verified Git-visible snapshot and retains its source, commands, compilers,
libraries, and logs below `.local/checks/`. The separate instrumented-R,
`gctorture`, Valgrind, and rchk release gates remain mandatory.
Use `--source-run <prior-id>` to validate an already retained snapshot in a
fresh run while unrelated worktree edits are in flight; replay revalidates the
complete manifest and carries its original Git provenance forward.
Generated per-mode test caches and temporary directories are logged and pruned
before the modes-tree receipt is created, because reticulate/uv places absolute
interpreter symlinks there. They are not evidence; installed libraries,
artifacts, tool profiles, commands, and full logs are retained.

`scripts/check-r-api-compatibility` is the offline source-compatibility gate.
It snapshots the Git-visible worktree, extracts every exact R source archive
from `environment/r-api-sources.tsv` into a retained run, generates that
release's `Rconfig.h` without installing R, and compiles every shipped C file
with both strict GCC and strict Clang warnings. This catches accidental use of
new public C APIs despite successful builds on the development R. Its artifacts
remain below `.local/checks/` and it never reads or changes the host R. Before
compilation, Clang's raw lexer audits every shipped C source and header,
including inactive preprocessor branches, and rejects the legacy object-layout
and binding identifiers forbidden by Writing R Extensions, as well as either
spelling of the C token-pasting operator. The audit is
implemented by `scripts/environment/audit-public-api-tokens` and its report is
part of the retained run. The gate authenticates its snapshot manifest before
using it, receipts the complete source and generated-artifact trees, reverifies
both trees at completion, and writes a checksum-protected completion record
binding those receipts, the retained harness and helper, the audit, generated
headers, compilers, and release count. A failed or interrupted run has no valid
completion record and records `status=failed`.

`scripts/build-valgrind-r` builds a second, unoptimized R 4.6.1 below
`.local/r-valgrind/4.6.1` from the pinned source, with reference BLAS,
Valgrind instrumentation level 2, memory profiling, and only local compilers
and libraries. It is deliberately not selected by normal activation. Set
`PARADOX_BUILD_JOBS` to control its build parallelism; a configuration mismatch
fails instead of silently reusing a stale build tree.
`scripts/bootstrap-valgrind-r-packages` then installs the exact, checksummed
dependency closure in `environment/valgrind-r-packages.tsv` from source into
`.local/r-valgrind/library`; it must not reuse the ordinary compiled library.
The R build receipt is `.local/r-valgrind/receipts/R-4.6.1/`, and the separate
package-library receipt is `.local/r-valgrind/receipts/library/`. Each contains
authenticated input, package or source, installed-tree, and seal records. After
normal activation, both can be checked without rebuilding or installing:

```sh
scripts/environment/valgrind-receipts verify-r "$PARADOX_ROOT"
scripts/environment/valgrind-receipts verify-library \
  "$PARADOX_ROOT" "$PARADOX_ROOT/environment/valgrind-r-packages.tsv"
```

`scripts/bootstrap-valgrind-debug-symbols` independently extracts the
SHA-256-pinned Ubuntu loader debug package in
`environment/valgrind-debug-symbols.tsv` below `.local/debug`, creates the exact
debuglink mirror required by Valgrind, and receipts the full tree below
`.local/receipts/valgrind-debug-symbols`. Verify it offline with
`scripts/bootstrap-valgrind-debug-symbols --verify`. It never invokes apt/dpkg
or writes to `/usr`.

Normal `. scripts/activate` must continue to select the release R at
`.local/toolchain/bin/R`. Do not prepend the instrumented R or its library to
`PATH`, `R_HOME`, `R_LIBS`, or `R_LIBS_USER`; `scripts/memory-check` validates
the receipts and invokes `.local/r-valgrind/4.6.1/bin/R` with the dedicated
library directly.

The release memory gates are orchestrated separately and never have a default
heavy mode:

```sh
scripts/memory-check --source-run <passed-native-run> --mode gct
scripts/memory-check --source-run <passed-native-run> --mode valgrind
scripts/memory-check --source-run <passed-native-run> --mode rchk
```

`--source-run` is mandatory. The command verifies that run's source manifest
against its source tree, built archive, completion seal, ordered mode statuses,
and complete modes-tree receipt before copying it into a new retained run. The
retained native harness and receipt helper must equal trusted current copies,
so changing worktrees or transplanted evidence cannot alter the input. `gct`
runs `R CMD check --use-gct` with
`_R_CHECK_GCT_N_=10`. `valgrind` refuses to build prerequisites implicitly;
it validates the dedicated R, exact package closure, level-2 instrumentation,
and project-local reference BLAS before running focused tests and
`R CMD check --use-valgrind` with full actionable-leak and origin diagnostics.
Retained stage wrappers force exact command-line-only options, empty
`VALGRIND_OPTS`, the receipted loader debug object, and complete
suppressed-zero logs. `rchk`
refuses implicit image pulls and runs bcheck, maacheck, and fficheck offline in
the exact manifest digest, through `scripts/podman-local`, against only a
writable run-local source copy. All commands, source, metadata, tool versions,
logs, and diagnostic reports remain below `.local/checks/<run-id>`.

## Frozen release workflow

Release evidence starts only after all intended files are committed and a full
Git ref is fixed on that commit. Keep the repository on that exact clean commit
while commands that snapshot the current worktree run. A final native, public-R
API, and memory sequence uses new run IDs:

```sh
. scripts/activate

native_run=release-native-YYYYMMDDTHHMMSSZ
r_api_run=release-r-api-YYYYMMDDTHHMMSSZ
memory_run=release-memory-YYYYMMDDTHHMMSSZ

scripts/native-check --mode all --tests full --run-id "$native_run"
scripts/check-r-api-compatibility --run-id "$r_api_run"
scripts/memory-check --source-run "$native_run" --mode all \
  --run-id "$memory_run"
```

`--mode all` on `native-check` means all strict compiler, static-analysis,
symbol, ASan, and UBSan modes. With `--tests full`, its strict-GCC mode also
runs a clean `R CMD check --as-cran`, a second check with all skip-on-CRAN
tests enabled, and a depends-only check with forced Suggests disabled. All
three must end at exact `Status: OK` and are included in the sealed mode tree.
`--mode all` on `memory-check` means gctorture,
the dedicated instrumented-R Valgrind gate, and rchk. The memory gate must use
the passed native run that retained the same frozen source; an API-only run is
not a valid `--source-run`. Any later package-source change requires a new
commit/ref and new native, API, memory, compatibility, documentation, and
performance evidence.

Install the consumer-test candidate only after its run ID and full ref, commit,
and tree are exported, and only into the absent run-specific library reserved
beside the dependency-preparation stage. The authoritative command shape and
portable content-sentinel handling are in
`compat/README.md`; `compat/install-candidate` takes candidate library,
dependency library, and source worktree in that order. That same authenticated
candidate is then used for:

- `compat/install-repository-test-dependencies.R`, with positional root,
  maximum priority, and dependency library plus the mandatory named
  `--run-id`; its retained ledger is written below the matching
  `.local/compat/runs/<run-id>/repository-dependencies-priority-<N>/` stage;
- `compat/test-reverse-dependencies.R`, with named options for the root,
  priority, candidate/dependency libraries, full ref/commit/tree, portable
  candidate content hash, and a new reverse-run ID;
- `compat/test-repositories.R`, whose positional interface is root, maximum
  priority, candidate library, and dependency library, plus mandatory named
  `--run-id` and optional `--plan-only`; priority zero also receives the reviewed
  `library-mlr3verse-core` through `PARADOX_CONSUMER_EXTRA_LIBS`;
- `compat/test-documentation`, with the same candidate/dependency libraries,
  the mlr3verse core as `--extra-library`, a new run ID, and `--scope all`;
  this also runs the pinned mbo_config and reviewed documentation migration
  workloads; and
- `benchmarks/release`, with the sealed full-inventory differential run,
  authenticated candidate library, repeated dependency-library options,
  miesmuschel library, and a new output directory. This release wrapper always
  runs the complete workload inventory and both focused consumer processes.

Repository dependency and test ledgers are retained only below their unique
run directories. The checked-in priority-zero ledgers are historical fixtures;
release harnesses must never update files under `compat/`. Repository,
reverse-dependency, and documentation stages use deterministic whole-stage
manifests authenticated by `metadata/completion.seal`; verify them with
`compat/verify-repository-evidence.R` before citing the evidence.

The documentation gate's essential book chapter, website paradox benchmark,
tuning/pipeline cheatsheets, and both serialized mbo_config ParamSet workloads
are mandatory. Full book/website/cheatsheet renders, both gallery probes, the
mlr3benchmark nested-values contract, and mlr3-targets legacy migration remain
recorded advisory workloads. `compat/verify-mlr-org-review` separately
reauthenticates the complete 91-repository organization census; keep its
consumer/source partition synchronized whenever a census repository is
promoted into `github-snapshot.tsv`. The gate verifies pinned clean source
checkouts, all explicitly selected protected libraries, the candidate
provenance, and the pinned Quarto receipt. It seals commands, results, logs,
archived sources, overlays, and metadata below
`.local/compat/runs/<run-id>/documentation/`. Review advisory results and raw
benchmark distributions; a zero exit status or a ratio table alone is not a
release claim.
