# Native validation matrix

This is the release matrix, not a list of checks that may be claimed without
their retained logs. All commands run after `. scripts/activate`, against a
run-local source snapshot and library below `.local/checks/`.
`--source-run <prior-id>` can replay a fully hash- and membership-verified
retained source tree into a fresh run when concurrent worktree edits must not
alter the validation input; its original Git provenance remains attached.

## Compiler and package checks

- Default local GCC build and the complete testthat suite with `NOT_CRAN=true`.
- GCC 14 and Clang 22 C17 builds with `-Werror`, conversion, format, prototype,
  declaration, shadowing, pointer-arithmetic, cast-alignment, and strict
  pedantic warnings enabled. The only registration warning suppressed is the
  function-pointer cast required by R's documented `R_CallMethodDef` idiom.
- A separate GCC `-fanalyzer` package build, a direct Clang 22 static-analyzer
  pass over every C translation unit, cppcheck, and a registered-routine/symbol
  audit. The Clang pass uses the strict package include/diagnostic flags and
  retains one plist and command log per source; both textual findings and
  nonempty plist diagnostics are fatal. Cppcheck uses exhaustive analysis for
  the selected C17 build configuration without forcing impossible branches in
  the public R headers.
- A full strict-GCC native run executes the complete testthat inventory once
  with `NOT_CRAN=true`, performs one `R CMD check --as-cran` on the built source
  archive, and runs a test/example/vignette/manual-free depends-only check with
  forced Suggests disabled. Both package checks must finish with exact `Status:
  OK`; their complete check trees and command logs are sealed with the other
  native-mode evidence. Strict Clang, ASan, and UBSan execute the registered
  routine/hazard probe inventory plus the exact six-file analyzer-sensitive
  subset against their exact DSOs instead of repeating the functional corpus.
- Linux, Windows/Rtools, and macOS checks in CI. No native-architecture flags
  or x86 intrinsics are allowed in the shipping build.
- An offline source-API gate compiles every translation unit with both strict
  compilers against the pinned R 4.3.0, 4.4.0, 4.5.2, and 4.6.1 headers. Before
  compilation, Clang's raw lexer scans every shipped C source and header,
  including inactive preprocessor branches, and rejects the legacy closure,
  environment, binding, and raw-attribute identifiers that Writing R
  Extensions classifies as non-API. Comments, string literals, and documented
  APIs such as `ANY_ATTRIB` and `SHALLOW_DUPLICATE_ATTRIB` remain allowed. The
  raw-token policy also rejects both `##` and its `%:%:` digraph spelling, so
  forbidden identifiers cannot be assembled by token pasting. The snapshot
  manifest is authenticated before use. Full R extraction and configure trees
  are cache-miss-only staging. Header-cache schema 3 binds the archive, exact
  configure environment/arguments and fixed shell, platform, toolchain lock,
  executing cache and tree-receipt helpers, and the complete reviewed
  configure/make command inventory, including absent commands and selected
  executable paths, link chains, bytes, and identities. Per-key locking and
  atomic promotion publish only the receipted source-include tree plus
  `Rconfig.h` and `Rversion.h`. Cache hits authenticate that small tree without
  rereading the archive. Each run independently copies or reflinks the tree,
  creates and verifies a fresh receipt for its published copy, requires that
  receipt to equal the cache receipt, and therefore retains no full R
  source/build tree.
  On Linux a separate two-boundary compiler-closure receipt covers the invoked
  and canonical GCC and Clang drivers, their target/search/version state, GCC
  specs and `cc1`, effective preprocessing plans, Clang's target configuration,
  resolved dynamic libraries, and all default or explicit header trees (GCC
  builtin/fixed, local sysroot, explicit toolchain include, and Clang resource)
  without hashing the complete toolchain. Resource-aware
  GCC and Clang lanes share one admission ceiling. Their exact source-order
  task ledgers bind source hashes, compiler and wrapper identities, exit and
  timeout state, and both compiler and supervisor-only log hashes. Schema-4
  plans additionally bind authenticated util-linux `setsid` and the count plus
  NUL-framed SHA-256 of each complete argument vector, including strict flags
  and run-local include paths. Both lanes and all four release rows finish and
  are inventoried before the matrix reports task failures. Header preparation
  has its own exact result and authenticated cleanup-retry ledgers.
  Complete package-source and run-artifact trees are receipted and reverified
  at completion. A checksum-protected completion record binds those receipts,
  the retained harness/helpers, cache ledger, audit report, compiler inventory,
  and release count. Failed or interrupted invocations cannot produce passing
  evidence.

The ordinary 2.0.0 DSO currently has 61 registered `.Call` routines. The
release native gates treat `src/init.c`, `NAMESPACE`, and
`environment/native-routine-coverage.tsv` as an exact set rather than accepting
that number as a floor; every ordinary entry needs a direct probe. The
compile-time row-name-rooting fixture is registered only in its dedicated
instrumented build and is not an ordinary-DSO entry. Dynamic lookup remains
disabled, and the symbol gate accepts exactly the two lifecycle exports
`R_init_paradox` and `R_unload_paradox`.

## Verification-economy audit

All durations, memory figures, and historical inventory counts in this section
are harness-design measurements from named development stages. They explain why
the retained gates are structured as they are; they are not release evidence
for the final 2.0.0 source. Only receipts produced from the frozen candidate may
support a release claim.

The historical Candidate 4 native run took 2 h 46 min 42 s. Individual package
installs took roughly 15--25 seconds; compilation was not the bottleneck. The
same 26--34 minute functional corpus was executed under strict GCC, strict
Clang, ASan, and UBSan, in addition to a redundant all-tests check. The revised
matrix retains one authoritative complete functional run and uses exact-DSO
native probes for the other compiler/instrumentation builds. A release-native
run is therefore expected to be dominated by one roughly 28-minute corpus and
one roughly 3-minute CRAN-style check, rather than about 2 h 25 min of repeated
behavioral tests.

The reviewed analyzer-subset harness baseline contained six exact native-state
files, 70 test blocks, and 659 passing expectations. It runs with
`NOT_CRAN=false`, so four especially pathological nested-torture scopes are
skipped by exact reviewed title/reason while ordinary native mutation,
lifetime, domain, collection-state, and R6-surface behavior still executes.
The measured plain run was about nine seconds. Missing required skips, new
skips, file drift, and coverage below 70 blocks or 650 passes are fatal.

The old Candidate 4 GCT attempt was still inside its duplicated whole-package
surface when it was terminated after about 2 h 18 min; the harness also planned
a second explicit torture-vignette surface afterward. Against that stage's
fixed DSO, the replacement GCT inventory dynamically covered all 34 routines
registered at that development stage and four reviewed hazards, with zero failures and a
verified activation transition,
in 143.74 seconds (92,928 KiB peak RSS). This is the intended trade: functional
breadth is owned once by the native suite, while each compiler, sanitizer, and
memory runtime gets fast attribution to every native entry point and the known
allocation/reentrancy hazards. Those counts and timings explain the harness
redesign; they are not evidence for the larger frozen 2.0.0 candidate. Current
release evidence must match the complete ordinary registration inventory in
`environment/native-routine-coverage.tsv` exactly.

The intermediate non-following metadata fingerprints measured 0.98 seconds for
the complete local toolchain and 0.31 seconds for the dependency library. Full
content receipts remain mandatory at stage start and final postflight, but are
not recomputed around every mode or consumer row.

The former nested Valgrind verifier amplified the same mistake: with Valgrind
selected, the ordinary toolchain could be traversed six times, while the
instrumented R source, installed prefix, and package library were each
traversed four times. The memory gate now separates sealed structure/runtime
validation from content validation. It shares the byte-identical complete
toolchain receipt already created by the ordinary gate, holds the instrumented
state lock, and traverses each of the other three protected trees exactly once
before and once after execution. Metadata fingerprints cover the interval.
Source and package archives are authenticated when a build or installation
consumes them; a no-op bootstrap authenticates the installed runtime trees and
their pinned archive digests without rereading unused archives.

The old receipts also included the complete verifier script hash as though it
were a byte-affecting build input. A reporting-only verifier edit therefore
forced a 2.9 GB R toolchain rebuild and then reinstalled all 29 packages. The
schema-2 build keys omit verifier identity, while retained run evidence still
binds the verifier that made each decision. The one-time authenticated schema
migration took 24.52 seconds for R and 25.87 seconds for the package library;
the measured combined no-op package bootstrap took 25.30 seconds and preserved
both R and package timestamps. No compilation or installation occurred.

The former repository matrix was worse at consumer scale. A successful 34-row
priority-two run could traverse the candidate library 72 times, the 2.2 GB
dependency library 73 times, and the candidate package 70 times--more than
160 GB of dependency-library reads alone. Plan-only mode still performed full
hashes, an interruption lost every completed row, and consumer-local builds
could repeat. The tracked repository and reverse-dependency defaults now use
only start/final content passes, cheap path-bound metadata at row boundaries,
authenticated content-addressed install caches, and append-only sealed rows.
`--resume` accepts already sealed rows and quarantines only an interrupted or
invalid row. Bounded consumer commands retain complete failure counts rather
than stopping at the first test error.

The isolated reverse-runner self-test completed in 8.854 seconds (peak RSS was
not separately captured). It executed no real consumers and no multi-gigabyte
hashes, while
instrumentation proved exactly two complete passes over each small protected
fixture, zero passes in plan/self-test mode, sealed-row reuse, interrupted-row
quarantine, atomic launch/result/completion receipts, cache authentication,
complete test-batch policy, two-worker overlap, retained successful siblings,
bounded descendant cleanup, and one-winner cache promotion. The
matching repository self-test completed in 78.75 seconds (152,636 KiB peak RSS)
and exercised real external-worker overlap and descendant cleanup, complete
wave collection after one worker failed, failed-row-only resume, sealed-wave
promotion, environment isolation, exact nested CMake/`parallel`/`future`
controls with receipt-tamper rejection, and the same two-boundary economy
contract.

Independent compiler admissions now use a resource-aware batch ceiling rather
than fixed `-j1` or an unbounded logical-CPU count. On the 32-logical-CPU,
approximately 51-GiB-available development host, the reviewed API-compile
profile selects 30 jobs while reserving two CPUs and about 13 GiB. The policy
also reads affinity and cgroup v1/v2 ancestor limits, keeps at least 12 GiB and
25% of available RAM free, and permits only downward operator overrides.
`compile` budgets one CPU and 1024 MiB per job with a 16-job cap;
`api-compile` budgets one CPU and 768 MiB; `light-test` budgets one CPU and 2048
MiB with a 16-job cap; and `consumer` budgets two CPUs and 8192 MiB with a
four-job cap and 16384 MiB minimum reserve. These are conservative admission
budgets rather than observed peak-memory claims. The synthetic resource test
covers both cgroup layouts, Darwin `vm_stat`, and malformed limits. The
compiler-batch test proves that six tasks and two failures are all retained in
exact source-order task ledgers before the batch returns nonzero. Heavyweight
consumers use separate
`Rscript` processes in waves of at most four, recompute the live ceiling before
every wave, and force nested make, CMake, testthat, `parallel`, `future`, BLAS,
and OpenMP parallelism to one.
The parent waits for every sibling and seals the complete wave before promoting
rows in deterministic order; interruption terminates each worker's process
tree. Sanitizer, GCT, Valgrind, and rchk execution remain serialized because
their shared runtime state and peak-memory behavior do not justify a concurrent
schedule. Heavy top-level gates also run one at a time: their live reports are
point-in-time decisions, not a machine-wide lease, and the host has no swap.

The deterministic compact-row-name rooting regression has a separate
content-addressed instrumented build. Its measured cold build was 23.73 seconds
and a verified cache hit, including the behavioral driver, was 3.03 seconds.
Negative compile-time variants independently fail at the store local root, the
collection local root, and the collection carrier root, so this fast cached
gate is discriminating rather than merely a green torture run.

The expanded functional-ledger and isolated-worker self-test completed in
59.78 seconds (142,964 KiB peak RSS). It proves real worker overlap,
deterministic collation, exclusive ConfigSpace execution, wrong-DSO rejection,
strict supervisor identity checks, bounded per-task timeout and
interrupted-descendant cleanup, exact worker-count enforcement, complete
two-failure batching, and a real
58-file, 580-block, 1,740-expectation ledger (including 27 `NOT_CRAN` scopes)
through the trusted verifier. It also guards against control-character
sanitization that could corrupt retained test/file identities without changing
test status.

## R heap and native memory

- GC torture reuses the tree-verified strict-GCC installation sealed by the
  frozen native run and dynamically calls every registered routine plus the
  reviewed allocation/callback hazards under `gctorture2(10)`. The result
  records the activation/restoration transition and is checked against the
  exact DSO hash both immediately and before final sealing. The matching
  Writing R Extensions manual identifies GC torture as the primary dynamic way
  to expose missing `PROTECT` calls. Functional tests, examples, vignettes, and
  manuals remain in their existing native/documentation gates instead of being
  repeated under an analyzer; deterministic instrumented regressions remain
  required for known lifetime defects whose unsafe window cannot be guaranteed
  by ordinary torture scheduling.
- AddressSanitizer and UndefinedBehaviorSanitizer are separate jobs. Current R
  documentation warns of runtime-library conflicts when combining them with
  Clang 17 or newer. ASan compiles and links with
  `-fsanitize=address -fno-omit-frame-pointer`; package-wide R runs disable
  leak and allocation-mismatch reports from unrelated runtime libraries.
  UBSan uses `-fsanitize=undefined -fno-sanitize=float-divide-by-zero` and makes
  the first report fatal with stack traces.
- Valgrind uses a separate, unoptimized, debug-symbol R 4.6.1 built from the
  pinned source with reference BLAS and
  `--with-valgrind-instrumentation=2`. Release OpenBLAS is not used for this
  gate because the R manual notes that optimized BLAS can create irrelevant
  Valgrind reports. Reuse the source archive sealed by the frozen native run,
  install the frozen candidate once for this R, and run the complete
  registered-routine/hazard inventory with full leak checking,
  detailed output for definite, indirect, and possible losses, a complete
  all-kind summary, origin tracking, disabled default suppressions, and a
  nonzero error exit. The pinned Ubuntu
  loader debug package is extracted locally; its archive, matching host loader,
  build ID, `.gnu_debuglink`, mirrored debug object, and complete installed tree
  are receipted without invoking apt or changing `/usr`. A `/bin/true` smoke
  proves that Valgrind can install its mandatory loader redirections. A vanilla
  instrumented-R baseline then runs with the same memory options. Stage-specific
  retained wrappers invoke exact Valgrind 3.27.1 with every option on the real
  command line and `--command-line-only=yes`; `VALGRIND_OPTS` is empty and rc
  files are rejected before and after the gate. Fork children are silent until
  exec, preventing header-only logs, and non-actionable DWARF variable-location
  parsing is disabled. Every per-process log must contain a complete heap/leak
  result and end at exactly one suppressed-zero error summary. Definite,
  indirect, possible, and suppressed leak bytes must be zero; R's summarized
  still-reachable memory may be nonzero. Thus absent, header-only, truncated,
  suppressed, or trailing-junk logs cannot pass. A cheap `/bin/true` smoke and
  instrumented-R baseline diagnose infrastructure separately. Valgrind also
  runs the exact six-file analyzer subset so native behavior receives more
  than entry-point smoke coverage, but it does not repeat the complete
  functional package check.
- rchk complements the dynamic tools with source-pinned bounded bcheck and the
  maacheck/fficheck executables from the exact content-addressed R-hub image.
  `scripts/prepare-bounded-rchk-bcheck` builds bcheck once from
  `gaborcsardi/rchk` commit
  `56b621a4e7112246d7b640bee6219ee9c6eb4bf8`, tree
  `d08d5b88ab6c469ac1b6d9c4beeece322e223ac5`, with an 800,000-state limit;
  the immutable, content-addressed cache and all byte-affecting inputs are
  authenticated before analysis. The offline container never invokes the
  image's `rchk.sh`, image bcheck, or upstream `check_package.sh`. Its retained
  driver performs a direct WLLVM libraries-only install of the frozen
  candidate, extracts the DSO bitcode, and runs all three analyzers directly on
  the same pinned R and package bitcode.

  Each analyzer process receives exact soft and hard
  `RLIMIT_AS=21474836480`-byte limits through `/usr/bin/prlimit`. The host first
  admits one serial rchk job with a 20480-MiB budget and at least 16384 MiB in
  reserve. Rootless Podman on the development host's cgroup-v1 setup warns that
  `--memory` is unsupported, so container memory flags are not a release safety
  control. The analyzer limits, resource report, executable identities, DSO and
  bitcode hashes, tool statuses, and reports are retained.

  The source-bound policy hashes every complete analyzer report and its analyzer
  identity. For bcheck it additionally binds the ordered Function-block
  inventory, exact per-block UP/PB counts, and one used, reviewed rationale for
  each block. Only those exact analyzer-model limitations are accepted; any
  report, block, count, or rationale drift fails. Package-local `ERROR:` text is
  always fatal, while only the exact three reviewed base-R state-budget errors
  and two exact objdump warning forms are infrastructure exceptions. maacheck
  must be byte-empty. fficheck must identify `R_init_paradox`, report exactly 61
  functions, and find exactly one `R_registerRoutines` call. Thus the gate does
  not pretend every bcheck UP/PB diagnostic is actionable, but it also cannot
  hide a new one behind a broad suppression.

  Before the constructor decomposition, bcheck exhausted its per-function state
  budget in the literal `ps()` path. The post-refactor prefreeze review completed
  854 functions and 41,293 states without a package state-exhaustion error. This
  historical measurement justifies the split and bounded budget; only a
  policy-matching run over the frozen candidate is final release evidence.

`scripts/memory-check --source-run ID --mode gct|valgrind|rchk` implements
these gates. It has no default mode, accepts only a fully manifest-verified
snapshot retained below `.local/checks/ID`, and creates a fresh run containing
the copied source, any applicable source-archive hash, commands, isolated
homes/libraries, tool and image provenance, logs, and explicit scope records.
It never builds the instrumented R, installs its packages, or pulls auxiliary
images as a side effect; those are separate, auditable preparation steps.
The independent replay contract and its adversarial tests are documented in
[Memory evidence replay](memory-evidence-replay.md).

Memory runs accept only a successful `native-check` run with metadata schema 2,
the `release-memory-v1` static policy, focused or full tests, and all six
strict/static modes enabled. Before its exit trap may record success,
`native-check` creates an ordered per-tool status receipt, complete deterministic
source- and modes-tree receipts, and a non-circular completion seal. The seal
binds the source manifest and built archive to the mode evidence, harness,
receipt helper, toolchain lock, run metadata, tools, commands, tests, and static
policy. The memory validator rejects duplicate fields, compares retained
harness/helpers with trusted current copies, replays the source manifest
against the actual source, verifies both trees and every mode artifact, and
retains identical proofs before and after copying. Thus a failed, partial, or
evidence-substituted native run is not a release-memory input.

Full tests may provision Python through reticulate/uv. Before the modes tree is
sealed, `native-check` records and removes only each mode's generated cache and
temporary directory; these contain absolute interpreter convenience symlinks
and architecture-specific build scratch, not validation evidence. Installed
libraries, profiles, binaries, artifacts, and complete logs remain in the
receipt. This keeps successful evidence relocatable while the receipt helper
continues to reject every surviving absolute or escaping symlink.

The instrumented R and its 29-package library have deterministic receipts
outside both installed prefixes. The R receipt binds the checksummed R archive,
an exact archive/source-tree comparison, the complete local toolchain tree and
lock, compiler/build-tool/Valgrind binaries and headers, configuration and
level-2 build evidence, and the complete installed R tree. The library receipt
binds the exact manifest and every named archive, the R receipt seal, package
versions and `Built` fields, and its complete installed tree. Creation uses a
fresh library or R prefix and atomically selects the receipt; verification is
performed from scratch by the standalone verifier. During the Valgrind gate,
sealed receipt structure and runtime semantics are checked at both boundaries,
the already authenticated ordinary toolchain receipt must match the R-build
receipt byte-for-byte, and the R source, installed prefix, and dependency
library each receive exactly one complete content verification before and
after execution. An exclusive shared state lock plus an exact three-row
(`start`, `post-before`, `post-after`) metadata ledger covers the interval. The
retained helper copies, receipts, and metadata ledger are included in the run
artifacts.

Build, package installation, snapshot replay, and memory-check subprocesses
start from `env -i` with only repository-local homes, caches, libraries,
compilers, and tools restored. This prevents user profiles, R libraries,
compiler include paths, sanitizer preloads, `VALGRIND_OPTS`, or container
connection variables from weakening a gate. The rchk mode additionally proves
that its current Podman wrapper and configuration equal the static source run,
validates the installed Podman receipt and wrapper-managed tree before and
after the container, and retains those receipts with the result. That check
authenticates container provenance and isolation; memory containment comes from
the explicit analyzer `RLIMIT_AS` and host resource admission, not unsupported
rootless Podman memory flags.
The ordinary R toolchain and complete dependency library also receive full-tree
receipts before checked code is built or loaded and one complete content
reverification at final postflight. Intermediate mode boundaries compare a
non-following metadata digest including path, type, mode, size, mtime, ctime,
device, inode, link count, and symlink target. A final memory-run seal binds the
complete modes tree (including logs/artifacts), copied source tree and manifest,
native source proofs, ordinary-input receipt, harness, commands, and selected
modes before its own exit trap can record success. Shared instrumented-R locks
are opened append-only and checked by inode after opening, preventing lock-path
substitution from truncating or splitting coordination.

## Compatibility and adversarial gates

- The complete package suite, exact normalized upstream differential gate,
  and priority-zero/one consumer suites run after every native representation
  change. Expected upstream bug-fix deltas pin fingerprints of both complete
  normalized results; a changed delta fails.
- Deterministic boundary matrices cover empty vectors and sets, absent versus
  present `NULL`, reordered names, duplicate tags, integer/double storage,
  NA/NaN/infinities, tolerance boundaries, special values, unknown Domain
  classes, callbacks, dependencies, serialization, cloning, and data.table
  mutation.
- Native entry points are also called with deliberately corrupt private tables
  in R-level tests. They must raise a package error before reading an invalid
  type or length; malformed private writes remain unsupported but must not
  segfault.
- An isolated namespace-lifecycle regression constructs native-backed objects,
  unloads the namespace and DSO, forces collection, and reloads twice. It
  verifies that the DSO disappears and native-backed construction succeeds in
  each fresh lifetime. This behavioral check complements, but does not replace,
  code review of every paired preserve/release path.
- Stateful test ALTREP classes vary `Length`, `Elt`, and materialization results
  between accesses and may run a closure that replaces a previously inspected
  parent cell before forcing collection. Crash-oriented cases run in isolated
  subprocesses under ASan and GC torture. They cover every native path that
  sizes then emits, retains a vector pointer, or extracts children across an
  allocating or reentrant boundary; ordinary compact ALTREP inputs separately
  verify compatible fallback or one-pass materialization.
- The adversarial ALTREP constructors, GC-finalizer mutator, and unreachable
  string-size boundary probe intentionally live in the shipped DSO. R package
  tests execute the installed release library, and compiling a different
  test-only library would leave the actual CRAN artifact untested at precisely
  these lifetime boundaries. They are registered, hidden C symbols with
  forced-symbol lookup and have no exported R wrapper; this is an access
  boundary, not a secrecy claim, because a caller can still obtain registered
  routines deliberately. Their `test_*` registration names and namespace-only
  `C_test_*` bindings are a private validation ABI: external use is unsupported
  and may change without deprecation. Production paths never call them, they
  retain no process-global R objects, and the finalizer fixture clears its
  external pointer after one mutation.
- Any consumer failure that expresses a reusable contract first becomes a
  package regression test before the implementation is changed.

## Performance gates

Discretionary performance work is frozen for 2.0.0. A new optimization enters
release convergence only for a measured, release-relevant bottleneck whose gain
justifies invalidating source-dependent correctness evidence. Final benchmarks
run after all correctness gates, alone, against the same frozen Git candidate;
authenticated caches remain reusable when their byte-affecting keys match.

Benchmarks compare the pinned upstream installation and candidate in separate
processes. They record revision, R/compiler versions, CPU governor, system load,
input sizes, warmups, iterations, elapsed distributions, allocations, and GC.
The shared host is noisy, so only paired relative results from an otherwise
idle window are accepted. Synthetic microbenchmarks guide local work; release
claims require workloads extracted from bbotk, mlr3tuning, mlr3pipelines,
mlr3mbo, miesmuschel, and the mlr3 website benchmark corpus.
