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

Actual execution on older supported public R APIs is an independent, opt-in
matrix. Its prefix-free conda explicit inputs are
`environment/runtime-r-4.3.3-linux-64.lock` and
`environment/runtime-r-4.5.2-linux-64.lock`; every artifact has an exact
SHA-256. R 4.3.3 exercises all pre-4.5 compatibility branches and R 4.5.2
exercises the staggered direct closure/environment API without the new R 4.6
binding and attribute inspection APIs. Provision and verify both isolated
prefixes after ordinary bootstrap with:

```sh
scripts/bootstrap-runtime-matrix
scripts/bootstrap-runtime-matrix --verify
scripts/environment/test-runtime-matrix-installed all
```

The prefixes, conda cache, mutable development libraries, temporary files,
and receipts live below `.local/runtime-matrix` (with download/cache material
below `.cache`) and are independent of `.local/R/library`, the geospatial/P1
consumer overlay, and the instrumented Valgrind R. Bootstrap authenticates
micromamba from the same pinned archive as ordinary bootstrap, compares the
complete installed URL/SHA-256 inventory with the selected lock, runs an
actual C17 extension probe, and seals the complete runtime-prefix tree.
`--verify` is read-only and rehashes that tree. `--offline` can create an
absent prefix only from the authenticated local conda cache.

For interactive diagnosis, source exactly one verified runtime with
`. scripts/activate-runtime-matrix 4.3.3` (or `4.5.2`). This clears inherited
R/compiler/library state and selects a runtime-specific mutable library and
caches inside the repository. Source ordinary `scripts/activate` again to
return to development R 4.6.1. Never use a matrix prefix as a dependency
library for another R version.

The old-runtime source-test scope is explicit rather than inferred from a
testthat filter. Both interpreters lack the R 4.6 binding inspection APIs, so
the 22 direct native-admission implementation contexts listed in
`environment/runtime-matrix-pre46-exclusions.tsv` are not meaningful there;
their public behavior remains covered by characterization/regression tests and
the focused public-API probe. The runner stages the other 57 of the current 79
test files, all seven helper/setup inputs, and retains the complete
executed/excluded ledger. `environment/runtime-matrix-whole-file-skips.tsv`
separately authenticates the exact parsed leading-guard sequences of the two
staged ConfigSpace files (including the old file's preceding available `callr`
guard) before accepting their absent-reticulate whole-file skips.
`environment/runtime-matrix-result-skips.tsv` authenticates every reported
skip title and reason: six `NOT_CRAN=false` blocks on R 4.3.3 and those same
six plus the inactive legacy-data.table bridge on R 4.5.2. Unknown, duplicate,
symbolic, stale, reordered, or changed scope/skip rows fail closed.

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
repository-local search paths. On Linux it reconstructs the managed executable
prefix in this exact order: TinyTeX, Quarto, the ordinary toolchain,
`.local/bin`, P1, and GEO. Thus `R` and `Rscript` continue to resolve to the
top-level `.local/toolchain/bin` launchers while the repository TinyTeX tools
remain ahead of conda-provided TeX programs. The reverse-dependency gate rejects
any other resolution. Exercise hostile ordering, repeated activation, and the
R-level command predicates with:

```sh
PARADOX_COMPAT_TEST_INSTALLED_ROOT="$PARADOX_ROOT" \
  scripts/environment/test-reverse-activation-contract
```

Do not use this overlay for native package
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
of data.table, the upstream rchk source, the bounded-state rchk source used for
the release analyzer, and pinned R-hub container sources used by the auxiliary
memory gates. Each R extraction is checked against the complete tree digest in
`environment/r-api-source-trees.tsv`. The four Git worktrees must have their
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

Prepare the bounded bcheck executable once, without running an analysis, with:

```sh
scripts/prepare-bounded-rchk-bcheck
scripts/prepare-bounded-rchk-bcheck --verify
```

This uses `gaborcsardi/rchk` commit
`56b621a4e7112246d7b640bee6219ee9c6eb4bf8` and tree
`d08d5b88ab6c469ac1b6d9c4beeece322e223ac5`, builds inside the exact local
rchk image with networking and image pulls disabled, selects
`/usr/bin/clang++`, `LLVM=/usr/lib/llvm-14`, `BCHECK_MAX_STATES=800000`, and
`CALLOCATORS_MAX_STATES=1000000`, and atomically publishes a sealed cache below
`.local/rchk-bounded-bcheck/<input-key>`. The key excludes the preparation
verifier but includes every byte-producing input: the complete immutable source
receipt, image identity, compiler/LLVM/make identity, exact build command and
environment, macros, and container build driver. A cache hit and `--verify`
never compile or execute bcheck, never replace an invalid entry, and require a
mode-0555 final key directory with immutable, completely receipted descendants.
Run `scripts/environment/test-bounded-rchk-bcheck-cache` after changing this
layer; it proves publication and rejects stale and independently re-receipted
command, source, and inventory tampering without compiling rchk.

The release rchk gate does not invoke the image's `rchk.sh`, its unsafe
high-state `bcheck`, or upstream `check_package.sh`. Inside the pinned image it
performs a direct WLLVM libraries-only install of the frozen candidate,
extracts the package bitcode, and runs the cached bounded bcheck plus the image-pinned
maacheck and fficheck against the same R/package bitcode. Each analyzer receives
the exact soft and hard address-space limit `RLIMIT_AS=21474836480` bytes
(20 GiB). The
serial `resource-jobs rchk` admission additionally budgets 20480 MiB and keeps
at least 16384 MiB available for the host. Rootless Podman on this cgroup-v1
host does not enforce `--memory`, so container memory flags are deliberately
not part of the safety contract.

The source snapshot carries `environment/rchk-bcheck-policy/`. Its policy binds
the analyzer identity and complete bcheck, empty maacheck, and fficheck report
hashes, plus every ordered bcheck Function block, exact UP/PB counts, and a
reviewed rationale for each block. It accepts only those source-bound analyzer
model limitations; it is not a general UP/PB suppression. Every package-local
`ERROR:` remains fatal, maacheck must be byte-empty, and fficheck must report
exactly 61 functions and one `R_registerRoutines` call. The prefreeze review
after splitting the literal `ps()` constructor analyzed 854 functions and
41,293 states without package-local state exhaustion. That measurement explains
the policy and refactor; only a run consuming the frozen release candidate is
release evidence.

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
- Discretionary performance work is frozen for 2.0.0. Reopen it before release
  only for a clear release-relevant bottleneck whose measured gain justifies
  invalidating the frozen candidate and its source-dependent evidence.

## Verification economy and evidence

Release validation must maximize information gained per unit of wall time and
I/O without weakening provenance. Use this order and stop at the first failing
gate: parse/static harness checks, directly affected tests, one strict compiler
build, the complete paradox unit suite, the remaining compiler/runtime/API
gates, the full differential gate, focused then full priority-zero/one consumer
rows, memory analyzers, documentation, and finally benchmarks on an otherwise
idle host. Do not use a full `R CMD check` as an inner development loop.
Examples, vignettes, manuals, CRAN policy, native diagnostics, and consumer
tests are separate gates and must not be repeated merely to exercise one
another.

Freeze package source at a full Git ref and validate it from a clean detached
worktree. Continuing development in the primary checkout must not invalidate an
already fixed candidate. A source change invalidates evidence that actually
depends on those package bytes; a harness or report-only change invalidates the
affected harness evidence, not an authenticated compiled cache whose key omits
that input. Never cite evidence from an older package-content hash as evidence
for a newer candidate.

Install each ordinary candidate once per frozen stage. Cache expensive
compiler-instrumented variants and consumer installations by content-addressed
keys containing only inputs that can affect their bytes: source, install worker
and command, relevant environment, R/configuration, compilers/build tools,
platform, and dependency state. Authenticate the complete cache receipt
immediately before starting a test child. Reporting, verifier, plan, or
row-order changes must not cause a package rebuild. An incompatible or
incompletely authenticated cache is quarantined and rebuilt once; it is never
silently trusted. A fresh ordinary Paradox DSO currently compiles in roughly
15--25 seconds; it may be rebuilt when doing so is cheaper and easier to audit
than authenticating a cross-run binary cache. Never spend minutes to avoid
seconds of deterministic compilation.

For a protected multi-gigabyte library, compute a complete content hash once at
stage start and once at final postflight. At each row boundary compare a cheap,
non-following metadata fingerprint containing path, type, mode, size, mtime,
ctime, device, inode, link count, and hard-link identity. Redirect Python
bytecode, reticulate, XDG, R, compiler, and package caches into the disposable
row directory so consumers cannot dirty shared libraries. Plan-only and
self-test modes perform no full protected-library hashes.

The memory gate follows the same two-boundary rule. Its initial ordinary
toolchain receipt must be byte-identical to the toolchain receipt that built
the instrumented R, so Valgrind receipt validation must not traverse that tree
again. While holding the shared instrumented-R state lock, verify the complete
R source, installed R prefix, and dedicated dependency library once before and
once after execution; bind the interval with metadata fingerprints and sealed
receipt/runtime checks. Package and R archives are authenticated when a build
or install consumes them, but are not reread on a no-op cache verification or
inside memory-check. The receipts retain their pinned digests; `verify-r` and
`verify-library` traverse every runtime tree exactly once. A verifier-only
change migrates an authenticated schema-1 receipt with `refresh-r` and
`refresh-library`; it must never rebuild R or reinstall packages.

Consumer validation is append-only and resumable per package. A row is accepted
only after its tests, exact counts where available, provenance checks, protected
input checks, and row seal all succeed; an interrupted or partially promoted row
is never accepted. A verified sealed row is not rerun because a later package
failed. For external datasets, services, credentials, or optional runtimes,
retain one exact failure, classify it, and stop that external work at the row
boundary. Collect all failures from a bounded targeted run and fix a coherent
batch before rerunning only the affected target.
Synthetic tamper suites should build one authenticated valid fixture per schema
or scope and restore independent copies for mutations. Do not regenerate and
rehash an identical valid fixture before every negative assertion.

Reuse a build only across ABI-compatible gates: GCT copies and verifies the
sealed strict-GCC installation, Valgrind compiles one instrumented-R-specific
DSO, ASan and UBSan keep separate DSOs, and rchk uses its pinned analyzer
environment. Prefer direct coverage of every registered native routine and
known allocation/callback hazard over another complete functional package
check. Static-analyzer reports are retained verbatim and audited by source line
and root reachability; a green runtime torture probe is not presented as a
deterministic regression for a static lifetime defect unless the probe
guarantees an allocation in the unsafe window. Benchmarks run only after
correctness evidence is complete and alone on an idle host.

Use `scripts/environment/resource-jobs` instead of a raw CPU count for new
parallel gates. It intersects online CPUs, process affinity, cgroup v1/v2 CPU
quotas, Linux `MemAvailable` (or Darwin `vm_stat`), and the tightest cgroup
memory headroom. Every profile keeps at least 12 GiB and normally 25% of
currently available RAM free; heavyweight consumers are additionally capped at
four processes with 8 GiB budgeted per process. `--max-jobs` and gate-level
environment variables may only lower the detected ceiling. Retain the
`--report` TSV with release evidence. If one job would consume the safety
reserve, scheduling fails closed instead of forcing an unsafe serial process.
The current conservative admission budgets are: `compile`, one CPU and 1024
MiB per job with a maximum of 16; `api-compile`, one CPU and 768 MiB per job;
`light-test`, one CPU and 2048 MiB per job with a maximum of 16; and `consumer`,
two CPUs and 8192 MiB per job with a maximum of four and a 16384 MiB minimum
reserve. The serial `rchk` profile budgets 20480 MiB for one process and keeps
a 16384 MiB minimum reserve. These are scheduling budgets, not claims of
observed peak usage.
Heavyweight consumer checks may use only the repository and reverse runners'
bounded external-`Rscript` waves. They recompute the live ceiling before every
wave and normally force nested make, CMake, testthat, `parallel`, `future`,
BLAS, and OpenMP work to one thread. The repository runner's `mlr3` row and the
reverse runner's `mlr3` R CMD check child alone receive their receipted two-CPU
worker-contract exceptions; reverse installation and outer workers remain at
one, and build, test, BLAS, and Rcpp controls stay at one. The runners collect
every sibling, seal the complete wave before deterministic promotion, and
terminate worker descendants on interruption.
Ordinary focused/full native tests use the same nested-thread caps inside
file-isolated workers, enforce a 30-minute per-task deadline, and keep the two
ConfigSpace files in one exclusive worker after the ordinary wave.
Do not overlap heavyweight top-level gates: point-in-time admission reports are
not a cross-gate resource lease, and this host has no swap. Do not parallelize
Valgrind, GCT, rchk, ASan/UBSan execution, an unisolated functional-test
process, or other jobs whose isolation has not been proved merely because CPUs
are idle.

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
Functional tests run once, preferring strict GCC. Strict Clang, ASan, and UBSan
use the tracked native probe inventory, which dynamically calls every
registered routine and the reviewed allocation/callback hazards, followed by
the exact six-file analyzer-sensitive subset with `NOT_CRAN=false`. `--tests
probes` selects that bounded pair explicitly for fast development validation.
The ordinary 2.0.0 DSO currently contains 61 registered `.Call` routines; the
coverage manifest must match their names and arities exactly. The conditional
row-name-rooting fixture is confined to its dedicated instrumented build and
is not part of that ordinary inventory.
Run `scripts/environment/test-native-test-batch` after changing the functional
runner: it proves that one bounded invocation retains a complete two-failure
batch before returning nonzero, instead of exposing one expectation per rerun,
then passes a synthetic corpus at all reviewed focused-ledger minima through the
real writer and trusted verifier.

`scripts/check-r-api-compatibility` is the offline source-compatibility gate.
It snapshots the Git-visible worktree and compiles every shipped C file with
both strict GCC and strict Clang warnings against the releases in
`environment/r-api-sources.tsv`. This catches accidental use of new public C
APIs despite successful builds on the development R. Full archive extraction,
configuration, and generation of `Rconfig.h`/`Rversion.h` happen only on a
content-addressed cache miss below `.cache/r-api-headers`; those private trees
are deleted before cache publication. Header-cache schema 3 binds its release
and archive SHA-256, exact configure inputs and fixed shell, platform, pinned
toolchain lock, the executing cache and tree-receipt helpers, and the complete
reviewed configure/make command inventory. That inventory records present and
absent commands, selected paths, bounded link chains, executable bytes, and
identity probes. Per-key locking, unique staging, atomic promotion, complete
tree receipts, and a sealed input receipt make simultaneous same-key runs safe.
A hit verifies all of that state without rereading the source archive. Every
run receives its own small copied or reflinked source-include tree plus
generated headers, creates and verifies a fresh receipt for the published
copy, and requires that receipt to equal the authenticated cache receipt. It
compiles only against that run-local copy and never references mutable cache
files.
Thus retained evidence contains no full extracted R source or configure tree.
Its artifacts remain below `.local/checks/`, and it never reads or changes the
host R. On Linux, before header preparation, the gate receipts the exact
compiler closure that can affect syntax admission: invoked and canonical GCC
and Clang drivers, versions, targets and search paths, GCC specs and `cc1`,
their effective preprocessing plans, Clang's target configuration, resolved
dynamic libraries, and every default or explicit header tree used by the gate:
GCC builtin/fixed headers, the local sysroot, the explicit toolchain include
tree, and the Clang resource tree. It verifies that receipt again before final
sealing instead of traversing the complete multi-gigabyte toolchain. Before
compilation, Clang's raw lexer
audits every shipped C source and header,
including inactive preprocessor branches, and rejects the legacy object-layout
and binding identifiers forbidden by Writing R Extensions, as well as either
spelling of the C token-pasting operator. The audit is
implemented by `scripts/environment/audit-public-api-tokens` and its report is
part of the retained run. The gate authenticates its snapshot manifest before
using it, receipts the complete package source and run-artifact trees,
reverifies both at completion, and writes a checksum-protected completion
record binding those receipts, the retained harness and helpers, the audit,
header-cache ledger and receipts, compilers, and release count. A failed or
interrupted run has no valid completion record and records `status=failed`.
Strict syntax admissions are
independent per translation unit and therefore use the resource-aware compiler
batch runner. GCC and Clang share one live ceiling and, when that ceiling is
greater than one, execute as balanced concurrent lanes rather than each
claiming the full limit. Schema-4 lane plans bind the exact admission report,
retained runner, timeout tool, compiler and wrapper identities, authenticated
util-linux `setsid`, and the count plus NUL-framed SHA-256 of the complete
compiler argument vector. The latter proves the strict warning profile and
exact run-local include paths rather than merely the compiler executable. Its
source-order task ledger binds every translation-unit hash, the invoked,
link-target, and canonical compiler identities, exit and timeout state, and
the hashes of both the compiler log and its separate supervisor-only log. The
wave inventory hashes both exact lane ledgers and their inventories. It also
retains deterministic input-order aggregates and reports failure only after
every task in both compiler batches has completed.
`PARADOX_API_JOBS` may conservatively lower, but never raise, the derived job
ceiling. Header preparation uses its own retained light-test ceiling and
supervised process groups; compilation recomputes a lowering-only live ceiling
for every R release. The parent retains an exact row for every header task plus
an authenticated cleanup-retry ledger, and a bounded four-release compiler
matrix records all task failures before one aggregate failure is reported.

`scripts/build-valgrind-r` builds a second, unoptimized R 4.6.1 below
`.local/r-valgrind/4.6.1` from the pinned source, with reference BLAS,
Valgrind instrumentation level 2, memory profiling, and only local compilers
and libraries. It is deliberately not selected by normal activation. Set
`PARADOX_BUILD_JOBS` to lower its automatically derived build parallelism; a
value above the current safe CPU/RAM ceiling is rejected. The build retains the
decision in `resource-jobs.tsv`. A verified cache hit performs no scheduling
probe or rebuild; a real rebuild computes and stages its exact report
immediately before removing the old build. A configuration mismatch fails
instead of silently reusing a stale build tree.
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
copies and tree-verifies the already sealed strict-GCC candidate installation,
then runs the complete registered-routine and reviewed-hazard inventory once
under `gctorture2(10)`. `valgrind` refuses to build prerequisites implicitly;
it validates the dedicated R, exact package closure, level-2 instrumentation,
and project-local reference BLAS, installs the candidate once for that R, and
reuses the source archive already sealed by the frozen native run instead of
rebuilding package or vignette inputs, then runs the same complete inventory
under Valgrind. Valgrind then executes the exact six-file analyzer-sensitive
subset (at least 70 blocks and 650 passing expectations) with four reviewed
expensive scopes skipped; GCT relies on its complete probes and deterministic
rooting regressions. Both modes bind the semantic result to the exact current
DSO before execution and reverify it immediately before sealing. They do not
repeat the full functional corpus, examples, vignettes, manuals, or `R CMD
check` surfaces already owned by the frozen native and documentation gates.
Under the Valgrind state lock, the complete R source,
installed R, and dependency-library trees are authenticated at exactly the
pre/post execution boundaries; cheap non-following metadata ledgers cover the
interval, and the ordinary toolchain's already complete receipt is matched
byte-for-byte instead of being traversed again. Retained Valgrind stage
wrappers force exact
command-line-only options, empty `VALGRIND_OPTS`, the receipted loader debug
object, and complete suppressed-zero logs. Python bytecode and caches are
disabled or redirected into the disposable mode work tree. The ordinary
toolchain and dependency library receive full content receipts at stage start
and final postflight; non-following type/mode/size/time/device/inode/link
metadata protects every intermediate mode boundary without rereading all file
contents. `rchk` refuses implicit image pulls and verifies the authenticated
800,000-state bcheck cache before entering the exact image digest through
`scripts/podman-local`. The container directly installs and extracts bitcode
from the writable run-local copy of the frozen source; it never invokes the
image bcheck wrapper. All three analyzers run serially with an exact 20-GiB
`RLIMIT_AS`, and the host admits the mode only after the separate 20-GiB-budget,
16-GiB-reserve resource check. The complete bcheck report and ordered
block/rationale policy must match the source snapshot, maacheck must be empty,
and fficheck must inspect exactly 61 registered functions. All commands,
source, cache identity, metadata, limits, tool versions, logs, and reports
remain below `.local/checks/<run-id>`.

## Frozen release workflow

The exact external Windows/Rtools and Apple ARM64 handoff for the frozen 2.0.0
candidate is in `design/portability-ci.md`. It deliberately requires explicit
authorization before publishing the single CI-only tag and dispatching the
workflow. Never substitute a push of local `main`, a broad `--tags` push, the
benchmark companion, or the custom local release-ref namespace. Retain the
SHA-bound run metadata and logs, then record the platform job conclusions and
retained metadata/log/workflow hashes in `design/release-2.0.0.md`; do not
repeat byte-identical local release matrices just because the remote matrix
includes Linux jobs.

Release evidence starts only after all intended package files are committed and
a full Git ref is fixed on that commit. Keep the primary checkout on that exact
clean commit only while commands such as the differential and benchmark drivers
snapshot its current worktree. Compatibility and documentation gates instead
authenticate the frozen detached source, so excluded harness/report work may
continue in the primary checkout. A final native, public-R API, and memory
sequence uses new run IDs:

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

The memory command is shown beside the source run that it consumes, but in a
new release execute it only after the focused/full consumer gates are green;
do not spend Valgrind/rchk time on a candidate already rejected by consumers.

`--mode all` on `native-check` means all strict compiler, static-analysis,
symbol, ASan, and UBSan modes. With `--tests full`, its strict-GCC mode also
runs the complete skip-on-CRAN test corpus once, one clean `R CMD check
--as-cran`, and a test/example/vignette/manual-free depends-only check with
forced Suggests disabled. Both checks must end at exact `Status: OK`; the
strict-Clang and sanitizer DSOs run the complete native probe inventory instead
of repeating the R corpus.
`--mode all` on `memory-check` means gctorture,
the dedicated instrumented-R Valgrind gate, and rchk. The memory gate must use
the passed native run that retained the same frozen source; an API-only run is
not a valid `--source-run`. Any later package-source change requires a new
commit/ref and new native, API, memory, compatibility, documentation, and
performance evidence. Final checks consume that frozen candidate and
content-addressed caches whose byte-affecting keys still verify; they do not
rebuild or resnapshot the moving primary checkout merely because a report or
verifier changed.

Real supported-runtime evidence is retained separately from header-only API
compilation. Run it against the same frozen full ref as the release gates:

```sh
scripts/test-runtime-matrix --runtime all \
  --source-ref refs/paradox-release/candidate-YYYYMMDDTHHMMSSZ \
  --run-id release-runtime-matrix-YYYYMMDDTHHMMSSZ
scripts/verify-runtime-matrix-evidence \
  --run-id release-runtime-matrix-YYYYMMDDTHHMMSSZ
```

Each real runtime builds a source archive, installs it into an absent
run-specific library, executes a focused `r_api_compat.c` behavior probe, and
runs the authenticated public/characterization/regression and supported-native
source scope described above with `NOT_CRAN=false` (so the deliberately
expensive GC-torture jobs may skip). At least 4,900 clean expectations must run;
the exact staged files, testthat-reported files, exclusions, parsed whole-file
guards, reported skip blocks/reasons, and all counts and hashes are retained
and verified. Source provenance operations use the authenticated
repository-local Git 2.55.0 with replacements, grafts, alternate object stores,
info attributes, global/system attributes, and mutable tar umasks disabled or
rejected. The canonical byte-reproducible Git tar, Git executable identity,
and an exact pre-execution source-tree receipt are retained and reverified after testing and
again from a fresh archive extraction by the evidence verifier. The full source
ref must still resolve to the recorded commit and tree. The DSO audit proves
the exact version-specific public R symbol set: R 4.3 must not link any R
4.5/4.6 accessor, while R 4.5 must link the direct closure and
evaluated-binding accessors but no R 4.6 binding/attribute accessor. Complete
source, build,
library, logs, dependency inventory, compiler identity, bootstrap receipt,
commands, and source-test scope are tree-receipted and completion-sealed below
`.local/checks/<run-id>/runtime-matrix/`. Any later source change requires a
fresh matrix run along with every other frozen release gate.

While the primary checkout is still exactly the clean candidate, run the full
pinned differential inventory and verify its sealed directory before consumer
work begins:

```sh
compat/differential/run --baseline-ref 06091b5b64a78807d332ec95c5cdc1aaac5899b9
Rscript --vanilla compat/verify-repository-evidence.R \
  "$(cat .local/tmp/current-release-differential-run.txt)"
```

Every difference must either be absent or match both fingerprints and the
reason in `compat/differential/expected-differences.tsv`; a dirty differential
run is diagnostic only and cannot become release or benchmark evidence.

Before installing the frozen candidate, prepare the pinned source-package
hard dependency closure with a separate unique evidence ID:

```sh
reverse_dependency_run=release-reverse-dependencies-YYYYMMDDTHHMMSSZ
dependency_library="$PARADOX_ROOT/.local/compat/R/library-dependencies"
Rscript --vanilla compat/install-reverse-dependency-dependencies.R \
  --root "$PARADOX_ROOT" --max-priority 1 \
  --dependency-library "$dependency_library" \
  --run-id "$reverse_dependency_run"
```

This stage authenticates the pinned CRAN/Bioconductor target sources, resolves
only `Depends`, `Imports`, and `LinkingTo` through retained synthetic
dependencies-only roots, rejects installing or updating paradox, replays the
SHA-256-bearing pak lock, and seals its endpoints and inputs under
`.local/compat/runs/<ID>/reverse-dependency-dependencies-priority-<N>/`.
An existing paradox used to build a transitive dependency is allowed only as a
protected `installed` lock row and must remain byte-identical. `--plan-only`
requires a pre-existing selected dependency library and must leave it
unchanged. The disposable fixture gate is
`scripts/environment/test-reverse-dependency-preparation.R`; its real harness
success and injected post-lock failure/retry runs verify cleanup without ever
using the live shared dependency library.

Install the consumer-test candidate only after its run ID and full ref, commit,
and tree are exported, and only into the absent run-specific library reserved
beside the dependency-preparation stage. The authoritative command shape and
portable content-sentinel handling are in
`compat/README.md`; `compat/install-candidate` takes candidate library,
dependency library, and source worktree in that order. Installation and the
consumer/documentation gates require the clean detached candidate source,
full ref, commit, and tree all to identify the candidate; the primary checkout
may contain unrelated later work. Differential and benchmark drivers are the
exceptions and require exact candidate `HEAD` because they snapshot the primary
checkout. Replacement refs, grafts, alternate object stores, external archive
attributes, hidden index flags, and inherited repository-altering `GIT_*`
inputs are forbidden. The
sealed schema-2 receipt binds `PARADOX_CANDIDATE_RUN_ID`, both canonical library
paths, the dependency-library content hash, and the exact installer and Git
authenticator. Repository checks reuse that candidate run ID because their
dependency stage is its sibling; reverse-dependency and documentation checks
use new evidence run IDs while retaining the original candidate run ID in the
environment. That same authenticated candidate is then used for:

- `compat/install-reverse-dependency-dependencies.R`, with named root,
  priority, dependency-library, and unique run-ID options; its successful
  sealed stage supplies the hard source-package closure before candidate
  installation and reverse checks;
- `compat/install-repository-test-dependencies.R`, with positional root,
  maximum priority, and dependency library plus the mandatory named
  `--run-id`; its retained ledger is written below the matching
  `.local/compat/runs/<run-id>/repository-dependencies-priority-<N>/` stage;
- `compat/test-reverse-dependencies.R`, with named options for the root,
  priority, candidate/dependency libraries, full ref/commit/tree, portable
  candidate content hash, clean detached `--candidate-source`, and a new
  reverse-run ID. Its default tracked runner uses content-addressed consumer
  installs, two protected-library content passes per actual stage, cheap
  metadata wave boundaries, and sealed append-only acceptance with `--resume`.
  Immediately before every wave it retains a fresh
  `scripts/environment/resource-jobs consumer --report` decision, runs at most
  that lowering-only limit, normally disables nested make/CMake, testthat,
  `parallel`/`future`, BLAS, and OpenMP parallelism, waits for every sibling,
  and lets only the parent seal and accept rows. Its `mlr3` R CMD check child
  alone exposes the row's two-CPU allocation for five worker-contract controls;
  installation, outer workers, build/test pools, BLAS, and Rcpp stay at one;
  plan-only and its synthetic self-test perform no protected-library content
  pass;
- `compat/test-repositories.R`, whose positional interface is root, maximum
  priority, candidate library, and dependency library, plus mandatory named
  `--run-id`, optional `--plan-only` or `--verify`, candidate source/origin and
  repository-selection controls, a task deadline, and a lowering-only `--jobs`
  limit; priority zero also receives the reviewed `library-mlr3verse-core`
  through `PARADOX_CONSUMER_EXTRA_LIBS`. Its default
  resumable runner recomputes the same lowering-only consumer ceiling before
  each wave, executes rows in independent supervised `Rscript` processes with
  isolated homes, temporary trees, and caches, takes exactly one protected
  pre/post boundary per wave, seals the complete wave before parent-only
  promotion, and resumes a partially promoted sealed wave without rerunning
  successful rows. Nested work is capped at one except for a receipt-bound
  `mlr3` worker-contract exception that exposes exactly two CPUs while make,
  CMake, testthat, BLAS, and Rcpp remain at one;
- `compat/test-documentation`, with the same candidate/dependency libraries,
  the clean detached `--candidate-source`, the mlr3verse core and
  `library-documentation-extra-final3` as two explicit, repeated
  `--extra-library` arguments, a new run ID, and `--scope all`. The second
  overlay supplies `gt` and its locked `V8`/`bigD`/`juicyjuice` closure for the
  mandatory website benchmark;
  this also runs the pinned mbo_config and reviewed documentation migration
  workloads; and
- `benchmarks/release`, with the sealed full-inventory differential run,
  authenticated candidate library, repeated dependency-library options,
  miesmuschel library, and a new output directory. This release wrapper always
  runs the complete workload inventory and both focused consumer processes.

Repository dependency and test ledgers are retained only below their unique
run directories. The checked-in priority-zero ledgers are historical fixtures;
release harnesses must never update files under `compat/`. Repository,
reverse-dependency, and documentation stages use deterministic manifests
authenticated by `metadata/completion.seal`. Verify repository and
documentation stages with `compat/verify-repository-evidence.R`; verify the
reverse stage's row-bound composite seal, structured counts, two-pass ledger,
and retained install-cache receipts with
`compat/verify-reverse-dependency-evidence.R` before citing it.

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
