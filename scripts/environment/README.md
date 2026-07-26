# Native validation harness

The unattended top-level entry point is now:

```sh
scripts/verify doctor
scripts/verify plan --profile focused
scripts/verify run --profile focused
```

It orders the existing drivers as a weighted, cache-aware DAG and uses either
proved per-worker Podman/Docker cgroups or one proved aggregate systemd cgroup
before unlocking parallel coarse tasks. On this rootless-Podman/cgroup-v1
machine, the reviewed installed aggregate launcher is the default for
`doctor`, `plan`, and `run`; task resource claims are admission reservations
inside its hard process-tree ceiling. This file continues to document the
low-level native drivers and their retained evidence contracts. See
`verification/README.md` for one-time installation, containment proofs,
failure policies, development caching, change-aware ordering, resume, and the
explicit non-release `--best-effort` fallback.

The outer scheduler discovers CPU, parent-cgroup live RAM, aggregate PID, and
disk budgets rather than assuming this development host's size. It keeps
separate physical/cgroup/filesystem/PID capacity ceilings so temporary
pressure waits rather than becoming a permanent block. Static platform and
machine constraints fail visibly in the plan. Scalable tasks
declare a reviewed minimum and desired ceiling; peers receive their minima
first and spare CPU/RAM is assigned in information order. A per-user
machine-wide lock prevents two checkouts from independently admitting the full
host budget. Static fit applies the RAM fraction to physical/parent-cgroup
capacity, while live admission recomputes it from current availability; low
startup availability therefore waits without freezing later capacity.
Attempt results are immutable, each
resume records the current host/engine invocation, and retained successes must
match their numbered result, log, and original invocation; revalidated tasks
also invalidate their descendants. Scheduler-origin blocks do not cancel independent
branches or influence execution-failure priority. Each attempt enters through `verify-task-entry`, which reconstructs
ordinary activation after establishing private HOME/tmp/runtime state.
Prepared consumer tasks explicitly request `activate-compat-system` there, so
hard-container and host best-effort behavior cannot silently differ. Every
hard-containment probe uses that same worker UID and proves the read-only
root/nested-writable bind pattern. Per-worker mode additionally proves cgroup
and sacrificial OOM enforcement; aggregate mode authenticates the dedicated
system service and proves the real Podman payload remains in it.

The generic `release-core` profile stops at the native/API/runtime/differential
foundation. The combined memory driver below remains a direct source-bound gate
because its rchk branch already launches the pinned analyzer container; it must
be split before it can be safely represented as an outer contained task.
Actual prepared Paradox-2 reverse-dependency and documentation gates are
available separately and together through `prepared-reverse`,
`prepared-documentation`, and `prepared-release-compat`; their synthetic
fixtures remain preflights rather than substitutes.

Run the harness only after activating the repository-contained toolchain:

```sh
. scripts/activate
scripts/native-check --mode strict-gcc --mode strict-clang --tests focused
```

`resource-jobs` computes reviewed parallel ceilings from online CPUs, affinity,
cgroup v1/v2 CPU and memory constraints, and currently available RAM. Its
`--report` mode is evidence-friendly; `--max-jobs` can only lower the result.
It fails closed when even one job would invade the reserved memory headroom.
Inside a top-level verification worker it also converts
`PARADOX_VERIFY_ASSIGNED_CPUS` and
`PARADOX_VERIFY_ASSIGNED_MEMORY_MIB` into a cooperative job-count ceiling, so
several aggregate-contained coarse tasks cannot each rediscover and spend the
complete service envelope.
The conservative profiles are: `compile`, one CPU and 1024 MiB per job with a
16-job cap; `api-compile`, one CPU and 768 MiB per job; `light-test`, one CPU
and 2048 MiB per job with a 16-job cap; and `consumer`, two CPUs and 8192 MiB
per job with a four-job cap and 16384 MiB minimum reserve. These are admission
budgets, not measured peaks. The serial `rchk` profile admits one analyzer only
with a 20480 MiB address-space budget and at least 16384 MiB retained for the
host; the analyzer also receives an independent hard `RLIMIT_AS`.
`run-compiler-batch` executes isolated compiler admissions under that ceiling,
keeps deterministic input-order aggregates, and waits for the complete batch
before failing. Its schema-4 plan binds the admission report, authenticated
util-linux `setsid`, and the count plus NUL-framed SHA-256 of the complete
compiler argument vector. Each source-order task-ledger row binds the source hash,
invoked/link-target/canonical compiler identity, retained wrapper and Bash,
exit and timeout state, and hashes of both the compiler log and the separate
supervisor-only log. The wave inventory hashes both exact compiler-lane ledgers
and their admission-bound plans; the API parent also retains exact header-task
and authenticated cleanup-retry ledgers before compiling. Their fast
adversarial tests are:

```sh
scripts/environment/test-resource-jobs
scripts/environment/test-compiler-batch
scripts/environment/test-verification-economy
```

The last command is a fast static preflight for the long harnesses. It rejects
repeated tree traversals, verifier-dependent cache keys, execution before
content authentication, misplaced final seals, first-failure test batches, and
functional ledgers that are not bound to the loaded DSO. The broader
`test-validation-hardening` invokes it before constructing its slower tamper
fixtures, so a scheduling or ordering regression fails early. That harness
constructs each valid focused/full native-evidence fixture once, then restores
independent cached copies for its mutations; adding a tamper case must not
regenerate the same valid fixture from scratch.

The GitHub portability workflow has a separate always-run completion job in
addition to its platform matrix. Keep its raw-log and aggregate-result guards
covered by the cheap structural/adversarial tests:

```sh
Rscript scripts/environment/test-portability-workflow.R "$PARADOX_ROOT" general
Rscript scripts/environment/test-portability-ci-evidence-verifier.R
"$PARADOX_ROOT/.local/tools/bin/actionlint" \
  "$PARADOX_ROOT/.github/workflows/r-cmd-check.yml"
```

The release-only direct-child companion may reduce the matrix to macOS ARM64
and Windows x86-64 and pin the immutable candidate checkout, but it retains the
completion job. Its offline evidence must contain three successful REST jobs
(both platform rows plus completion) and exactly two platform artifacts.

The offline public-R-API gate uses `r-api-header-cache` to avoid retaining a
roughly 424 MiB extraction in every run. On a cache miss the helper verifies
the pinned archive, configures R in a supervised private process group, copies
the complete `src/include` tree and generated `Rconfig.h`/`Rversion.h`, then
deletes all source/build staging before it receipts and atomically promotes the
small entry. The schema 3 key and receipts bind every configure input, the
archive, platform, toolchain lock, the executing helper and tree-receipt helper,
the fixed configure shell, and the complete reviewed configure/make command
inventory. That inventory records present and absent commands, selected paths,
bounded link chains, executable bytes, and identity probes. Same-key builders
serialize with `flock`; interrupted staging is removed on exit or by the next
lock owner. A hit verifies the canonical input, seal, and full header tree
without hashing the archive. Compilation never reads the cache directly: each
retained run gets an independent copy or reflink, creates and verifies a fresh
receipt for the actual published output, requires it to equal the authenticated
cache receipt, and binds both receipts into completion evidence.

On Linux, `compiler-identity.R` similarly avoids an indiscriminate whole-
toolchain hash. It receipts the invoked and canonical GCC and Clang drivers,
their version/target/search state, GCC specs and `cc1`, resolved dynamic
libraries, their effective preprocessing plans, Clang's target configuration,
and every default or explicit header tree used by the gate (GCC builtin/fixed,
local sysroot, explicit toolchain include, and Clang resource) before header
preparation, then verifies the exact closure again before final sealing.

`scripts/native-check --help` lists the independent modes. There is no default
mode. `--mode static` runs the two strict compiler installs, a GCC `-fanalyzer`
package build, Clang 22's static analyzer over every C translation unit,
cppcheck, and the native registration/ELF audit. The Clang mode retains an
individual plist and command log for each source and fails on either textual
warnings or a nonempty diagnostics array. `--mode all` additionally runs ASan
and UBSan as two separate builds. Functional tests are deliberately owned by
one selected DSO (strict GCC when it is present); the other compiler and
sanitizer builds dynamically exercise every ordinary registered routine in the
reviewed coverage manifest and four allocation/callback hazards through
`run-native-probes.R`. The compile-time row-name-rooting fixture belongs only
to its dedicated instrumented build. The gate discovers and compares names and
arities exactly; prose never substitutes for the current manifest. Every such
probe-mode DSO also runs a bounded analyzer-sensitive corpus covering
adversarial storage, materialize-once ALTREP, Domain kernels, allocating-entry
GCT, ParamSet quantile/trafo GCT, graph value transactions, and canonical
collection construction. That corpus sets `NOT_CRAN=false`, retains an
independently verified ledger, and requires the exact reviewed analyzer file
manifest and `NOT_CRAN` skip policy discovered from current source. `--tests
probes` runs this fast combined
inventory in every selected executable mode. `--tests full` includes tests
normally skipped on CRAN by setting `NOT_CRAN=true` and
then runs one ordinary `--as-cran` check plus a test-free depends-only check;
focused tests select files whose names contain `characterization`, `native`, or
`regression`. The functional owner emits a structured per-test ledger. Its
trusted verifier requires the exact selected file inventory, zero failures,
errors, and warnings, a nonempty result with passing expectations, and evidence
that every source scope using `skip_on_cran()` was admitted by `NOT_CRAN=true`
rather than silently skipped. Exact source discovery is the coverage authority;
there is no historical count floor to become stale. Outside the bounded
analyzer corpus no Linux skip is accepted implicitly.
On Linux, that one focused/full corpus uses independent file-level R workers
under the `resource-jobs light-test` ceiling (lowerable with
`PARADOX_NATIVE_TEST_JOBS`). Every worker starts with a unique home, temp, and
cache tree, loads only the installed mode library with `load_package = "none"`,
and proves the candidate DSO hash before and after its task. The coordinator
waits for the complete bounded batch, publishes results in source-file order,
and binds a deterministic `*-workers.tsv` task ledger into the semantic test
ledger. Nested make/CMake, testthat, `parallel`/`future`, BLAS, OpenMP, and
related numerical pools are forced to one. Each isolated task has a 30-minute
deadline; timeout and interruption use bounded TERM/KILL cleanup of the whole
task group plus token-marked nested sessions. The terminal row always records
requested jobs, effective jobs, and
the scheduler backend; the trusted verifier joins the requested count to the
retained `light-test` decision and the effective count to every worker row. A
task-token watchdog removes nested callr/processx descendants if the
coordinator is interrupted. The two ConfigSpace files share one exclusive
worker after the ordinary wave so they cannot race reticulate's managed Python
state. Direct runner invocations default to serial; non-Linux systems,
including Darwin, retain an explicit requested-many/effective-one fallback
until an equally strong native process-tree primitive is available. Analyzer,
probe, sanitizer, GCT, and Valgrind scopes remain serial.

Ordinary `R CMD INSTALL` compilation uses a separately retained
`resource-jobs compile` decision and may be lowered with
`PARADOX_NATIVE_COMPILE_JOBS`; nested runtime/test make and numerical-library
parallelism remains capped at one. GCC `-fanalyzer` installation stays serial
because its per-translation-unit peak memory does not fit the ordinary compile
profile without a separate measured budget. Before sealing, the native gate
requires the exact ordered install/functional decision inventory, validates
every report's profile arithmetic and lowering-only ceiling, and joins the
functional report to its test ledger. Missing, duplicate, raised, reordered,
or rehashed-but-inconsistent decisions are rejected.
`scripts/environment/test-native-test-batch` is the cheap runner regression: a
two-failure corpus must execute both blocks plus a successful sibling,
atomically retain the complete batch and terminal aggregate, and only then
return a failing status. It also proves genuine worker overlap, deterministic
ledger order, wrong-DSO rejection, exact effective-worker policy, the exclusive
ConfigSpace lane, bounded timeout and TERM-ignoring-worker cleanup, and cleanup
of active workers plus marked grandchildren after coordinator SIGTERM. The same
command runs a compact generated clean corpus through the real ledger writer
and trusted verifier, including admitted `skip_on_cran()` scopes; its size is a
runner fixture, not a coverage baseline.

Cppcheck uses its exhaustive analysis level on the actual Linux/C99 package
configuration. It deliberately does not force every imagined preprocessor
configuration in R's public headers: doing so invents self-referential values
for API macros such as `NORET` and produces header syntax errors unrelated to
any package build. The strict Linux builds and cross-platform CI cover the
real compiler configurations separately.

Each invocation creates a fresh `.local/checks/<run-id>/`; each selected build
is installed once inside that run and then reused by its tests or probes. The
input is the tracked plus non-ignored untracked state
of the current worktree. The snapshotter explicitly rejects `.git`, `.local`,
and `.cache`, preserves tracked deletions, hashes every regular file before and
after copying, and aborts if Git state or file membership changes during the
copy. The run retains the immutable source tree, manifest and its SHA-256,
HEAD/diff/status, source archive, compiler versions, replayable command log,
per-mode library, compiled DLL, analysis output, and result status. R startup,
temporary directories, caches, target libraries, and Makevars are all set to
project- or run-local paths. Python bytecode is disabled and redirected below
the disposable mode cache, so reticulate cannot mutate the shared dependency
library. Commands do not consult user R startup files. Probe, focused, and
compile-only runs do not authenticate or put TinyTeX on `PATH`; the relatively
expensive archive/tree authentication is owned only by `--tests full`, whose
CRAN-style documentation surfaces actually use it.

`--source-run <prior-id>` instead replays a prior run's retained source while
the live worktree is changing. The replay helper rejects unexpected files and
verifies the prior manifest, every file hash and mode, link target, and tracked
deletion before and after copying. The new run retains the original Git
provenance and manifest unchanged, plus a `source-origin.txt` link to the prior
run; build and analysis outputs always go to the new run directory.

A successful run also retains an ordered status/hash row for every selected
tool, complete source- and modes-tree receipts, and a completion seal created
before `result.txt`. The seal binds the source manifest and built archive to
the exact mode evidence, harness/helper, toolchain, run policy, tools, and
commands. Release memory gates accept only schema-2 native runs with all six
static modes, compare the retained harness/helper to trusted current copies,
replay the source manifest against the tree, and reject any missing, added, or
modified evidence.

The strict profiles use C99 and turn a broad warning set into errors. They do
not globally suppress compiler diagnostics. The only accepted R-specific
exceptions therefore remain the narrow source pragmas around the public R
header fixed-base enum and the registration ABI's required `DL_FUNC` casts.
The conda R `Makeconf` includes a linker-only option in `CPPFLAGS`; profiles
replace that with the equivalent compile-only preprocessor flags instead of
disabling Clang's unused-command-line warning.

The GCC analyzer profile intentionally uses `-O0` for path fidelity and omits
`_FORTIFY_SOURCE`: glibc itself emits a preprocessor warning when fortification
is requested without optimization, which the warning-as-error analyzer gate
correctly refuses. Both strict compiler builds and all runtime profiles retain
fortification; this exception affects analysis only, never a shipped DLL.

The sanitizer results have deliberately limited scope:

- ASan instruments and links the package DSO with Clang. Because the pinned R
  executable is unsanitized, the runner places Clang's project-local ASan
  runtime first with `LD_PRELOAD`. Leak detection is disabled because the R
  4.6.1 extension manual documents process-lifetime allocations retained by R;
  address errors remain fatal.
- UBSan instruments only the package DSO and dynamically links Clang's
  standalone runtime with a project-local rpath, as recommended for a package
  check under an unaltered R. Floating-point division by zero is excluded to
  preserve R's IEC 60559 semantics. Undefined behavior is fatal.
- ASan and UBSan are never combined. The matching R 4.6.1 manual warns of
  library conflicts with Clang 17 and later, and describes package-only UBSan
  as the successful setup. Every sanitizer run records
  `release_gate_complete=false`: neither mode exercises R's own native code as
  an instrumented runtime would. A separately built sanitizer-enabled R would
  be a distinct validation scope; these package-only results do not claim that
  coverage.

The symbol mode requires dynamic lookup to be disabled, forced registered
symbols, a one-to-one mapping between `.Call` registrations and `C_*`
namespace bindings, no literal-string `.Call` in shipped R, and no dynamically
exported package symbols except `R_init_paradox`. Paradox has no unload hook:
the capsule rewrite retains no process-global package state to release. On
Linux the symbol mode also rejects an
executable stack and text relocations and requires GNU RELRO metadata.

The bounded analyzer executable is a separate cached prerequisite.
`scripts/fetch-reference-sources` authenticates the exact
`gaborcsardi/rchk` commit
`56b621a4e7112246d7b640bee6219ee9c6eb4bf8` and tree
`d08d5b88ab6c469ac1b6d9c4beeece322e223ac5`; after the pinned rchk image is
local, `scripts/prepare-bounded-rchk-bcheck` compiles only `bcheck` inside that image
with explicit Clang 14, LLVM 14, 800,000 bcheck states, and 1,000,000 allocator
states. It never runs package analysis. The atomically published
`.local/rchk-bounded-bcheck/<input-key>` entry retains the immutable source and
complete receipt, image and tool identities, exact command/environment and
macros, full build log, binary hash/mode, payload receipt, and seal. A cache hit
and `--verify` are build-free and read-only with respect to the cache; an
invalid existing entry is reported rather than replaced. Exercise the same
publication verifier plus re-receipted tamper cases with
`scripts/environment/test-bounded-rchk-bcheck-cache`.

`scripts/memory-check --source-run NATIVE_RUN_ID --mode rchk` verifies that
cache and mounts its bcheck read-only. It does not call the image's `rchk.sh`,
image bcheck, or upstream
`check_package.sh`: a small retained driver performs
`R CMD INSTALL --libs-only --no-test-load` with WLLVM, extracts the candidate
DSO bitcode, and runs bounded
bcheck plus the image-pinned maacheck and fficheck directly against the same
R/package bitcode. Each analyzer is wrapped by `/usr/bin/prlimit` with soft and
hard `RLIMIT_AS` set to exactly 21474836480 bytes. Before starting the serial
container, `resource-jobs rchk` must admit one 20480-MiB job while reserving at
least 16384 MiB for the host. Rootless Podman memory flags are unsupported on
this cgroup-v1 host and are not trusted as a limit.

The frozen source supplies `policy.tsv`, `blocks.tsv`, and `rationales.tsv`
under `environment/rchk-bcheck-policy/`. It binds the analyzer identity, exact
ordered Function blocks and UP/PB counts, and one reviewed analyzer-model
rationale for each block. Its bcheck semantic digest sorts length-prefixed
records and therefore ignores only diagnostic ordering within a Function
block; it still binds the exact multiset of full diagnostics to function names
plus the exact error and analysis-summary lines. Each run independently seals
the raw reports in its tool-status receipt. Any semantic diagnostic drift or
package-local `ERROR:` is fatal; this is not a wildcard suppression. maacheck
must be byte-empty, and fficheck must report `R_init_paradox`, the exact
dynamically discovered registered routine inventory, and exactly one
registration call. Before any selected memory mode starts, an rchk-enabled run
requires the frozen source's exact three ordinary policy files to be
byte-identical to the trusted current policy directory; replay enforces the
same boundary. Regenerate all function/state/semantic-report inventories from
the frozen candidate; only the authenticated bounded-analyzer executable cache
may be reused without recompilation.

The Valgrind branch of `scripts/memory-check` separates fast sealed
receipt/runtime validation from expensive content traversal. Its ordinary
toolchain receipt must exactly match the instrumented-R build receipt, so that
multi-gigabyte tree is not reread by a nested verifier. Under the shared state
lock, the R source, installed R prefix, and dedicated package library are each
content-verified once before and once after execution, with a sealed
three-stage `start`/`post-before`/`post-after` metadata ledger covering the
interval. Source archives are authenticated when a build or install consumes
them; cache-hit and memory verification instead authenticate the installed
trees and retained pinned digests without repeatedly hashing unused archives.

The opt-in Linux consumer dependency overlay is independent of this native
harness. `scripts/environment/test-compat-system` performs its fast structural
gate: exact lock shape, prefix-free relocation, generated Makevars and receipt
verification, seals, and fail-closed tamper/symlink cases. Full installed-state
verification is `scripts/bootstrap-compat-system --verify`; run it only after
ordinary activation and before sourcing `scripts/activate-compat-system`.
`scripts/environment/test-compat-system-installed` hard-links the three
installed package inventories into a disposable checkout and exercises
provision, offline, verification, receipt repair, activation, and active and
inactive evidence binding without changing the source prefixes. It also runs
`scripts/environment/test-reverse-activation-contract`, whose tracked
`reverse-activation-probe.R` checks the same R-level command predicates used by
the reverse-dependency gate. The contract fixture starts with hostile command
shims, requires the exact managed prefix TinyTeX, Quarto, toolchain,
`.local/bin`, P1, GEO, and proves repeated overlay activation is byte-for-byte
stable before ordinary activation removes the overlay again. A source-only
audit worktree can exercise an already installed checkout with
`PARADOX_COMPAT_TEST_INSTALLED_ROOT=/absolute/checkout`.
`scripts/environment/test-activation-isolation` starts a clean shell with
hostile inherited XDG/ccache temporary paths and proves ordinary activation
repairs both to plain repository-local directories, including the required
mode-0700 XDG runtime root.

## Real supported-R runtime matrix

Header compilation cannot prove that versioned public-API paths behave
correctly inside the actual R interpreter. The opt-in runtime matrix therefore
uses the authenticated `environment/runtime-matrix.tsv` registry to provision
exact conda environments for R 3.6.3, R 4.3.3, and R 4.5.2 from their
SHA-256 explicit locks. It separately compiles shipped source against the
R 3.6.0 headers:

```sh
scripts/bootstrap-runtime-matrix
scripts/bootstrap-runtime-matrix --verify
scripts/environment/test-runtime-matrix
scripts/environment/test-runtime-matrix-installed all
```

The fast fixture checks registry, shell/R syntax, lock shape, complete-tree tamper
detection, and outward-link refusal. The installed fixture starts hostile
clean shells and proves every activation repairs injected R libraries, startup
files, compiler/linker/pkg-config inputs, caches, temporary paths, and XDG
runtime state. It also re-sources activation to prove idempotence. No matrix
command reads or mutates `.local/compat/R/library-dependencies` or either
consumer system prefix. R 3.6.3 additionally uses the exact
`environment/runtime-r-3.6.3-packages.lock` source closure in a separately
receipted repository-local library; it does not write to the immutable prefix,
host R, HOME, or a user library.
R 3.6 still executes the atomic ALTREP tests. Its sole ALTREP capability
exclusion is the adversarial list fixture: R did not expose VECSXP ALTREP
classes until R 4.3, so the corresponding production branch cannot arise on
that interpreter.

`scripts/test-runtime-matrix --help` describes the retained execution gate.
For each selected actual interpreter it archives a committed source ref,
builds and installs paradox into a fresh stage library, runs the focused
public-R-API facade probe and an authenticated supported source-test scope, and
audits undefined DSO symbols against that release's allowed accessor set. All
supported interpreters stage the complete discovered public, characterization,
regression, and native source suite. The header-only
`environment/runtime-matrix-pre46-exclusions.tsv` authenticates that there are
no remaining pre-R-4.6 implementation exclusions. The coordinator validates
that zero-row contract from the exact extracted candidate before admitting
any build/install worker, and each old-runtime runner reuses the same
validator. R 3.6--4.5 use the ATTRIB and
FORMALS backports documented in Writing R Extensions rather than evaluating R
inspection shims. R 3.6--4.1 use a cold `base::exists()` path only for optional
absence checks and an old-only `R_HasFancyBindings()` receipt-scan exception;
required binding reads remain allocation-free. The two ConfigSpace
files that stop at their absent-reticulate guard remain staged and are audited
separately through `environment/runtime-matrix-whole-file-skips.tsv`, including
the old file's preceding available `callr` guard; they are not silently treated
as executed result files. Every result-block skip title and reason is likewise
derived from the current block-scoped `skip_on_cran()` source and matched
against `environment/runtime-matrix-result-skips.tsv` for every runtime. The
suite must be clean and nonempty; file, context, support, and skip counts are
joined to their retained inventories instead of frozen prose floors. The stage
retains the exact scope ledger, staged source copies, testthat-reported
inventory, skip ledgers, counts, and hashes.
The coordinator also runs `mbo-config-fixtures` before resource admission.
That helper joins the candidate's snapshot and organization-review manifests,
reads the two upgrade fixtures from the selected immutable Git commit rather
than its worktree, and publishes one read-only shared bundle. All stages set
`PARADOX_MBO_CONFIG_ROOT` to that bundle's `common/` directory. R >= 4.0 runs
the complete direct and recursive ParamSet-family migration assertions. R 3.6
proves the precise fail-closed active-binding limitation plus current-object
and standalone legacy Domain/Condition paths; Paradox-1 ParamSet-family R6
shells themselves use active bindings, so their practical migration requires
R >= 4.0.
The top-level receipt and each stage's digest claims are replayed by
`verify-runtime-matrix-evidence`.
Committed-source reads and archives use the authenticated project-local Git
with replacements and unreviewed object/attribute inputs rejected, global and
system attributes disabled, and the tar umask pinned. Its identity and
canonical byte-reproducible archive are retained, and the full source ref must
continue to resolve to its recorded commit and tree. A pre-execution source
receipt is reverified after the run and against a fresh archive extraction
during evidence verification. Conda R may retain a
nonexistent build-farm directory in `R CMD config NM`; the audit checks that
its tool basename agrees with the activated compiler hook, then invokes and
records the authenticated runtime-prefix binary instead of following that
escaped path. All artifacts are
sealed beneath `.local/checks/<run-id>/runtime-matrix`; verify a completed run
with `scripts/verify-runtime-matrix-evidence --run-id <run-id>`. The retained
command and input copies make rerunning a later frozen candidate a change only
to `--source-ref` and `--run-id`.
