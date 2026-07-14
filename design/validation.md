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
- A full strict-GCC native run performs `R CMD check --as-cran` on the built
  source archive, a second check with all skip-on-CRAN tests enabled, and a
  depends-only check with forced Suggests disabled. Each must finish with exact
  `Status: OK`; its complete check tree and command log are sealed with the
  other native-mode evidence.
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
  manifest is authenticated before use; complete source and artifact trees are
  receipted and reverified at completion. A checksum-protected completion
  record binds those receipts, the retained harness/helper, audit report,
  generated-header inventory, compiler inventory, and release count. Failed or
  interrupted invocations cannot produce passing evidence.

## R heap and native memory

- `R CMD check --use-gct` with `_R_CHECK_GCT_N_=10`, followed by focused
  `gctorture(TRUE)` tests around every allocating native entry point. The
  matching Writing R Extensions manual identifies this as the primary way to
  expose missing `PROTECT` calls. The check output must retain evidence of both
  the requested `gctorture2(10)` examples setting and GC torture in the package
  test startup file. R 4.6 does not apply that examples/tests injection to the
  vignette builder, so the gate also extracts the authenticated source archive
  and runs `tools::buildVignettes()` with both `R_GCTORTURE=10` and an explicit
  `gctorture2(10)` call. The log must contain the two activation markers.
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
  Valgrind reports. Run both focused native tests and `R CMD check
  --use-valgrind` with full leak checking, detailed output for definite,
  indirect, and possible losses, a complete all-kind summary, origin tracking,
  disabled default suppressions, and a nonzero error exit. The pinned Ubuntu
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
  suppressed, or trailing-junk logs cannot pass. The check omits manuals and vignettes so
  its library stays the exact instrumented hard/test dependency closure; the
  ordinary GC-torture check retains the vignette surface.
- rchk complements the dynamic tools with bcheck, maacheck, and fficheck in the
  exact content-addressed R-hub image. It runs with networking disabled and no
  implicit pull, through the repository-local Podman store, on a writable copy
  of the verified source. The analyzed DSO and the bcheck, maacheck, and
  fficheck reports are copied back through a writable host results mount and
  hashed. Reports cached or generated by the image's install helper are first
  deleted. Upstream `check_package.sh` then runs separately for each tool under
  Bash `-e -o pipefail`; an ordered host-side receipt binds every status 0 to
  the exact copied report hash. `ERROR:`, `[UP]`, `[PB]`, and `WARNING`
  diagnostics are fatal; bcheck
  must report a positive analyzed-function count, fficheck must inspect a
  positive function count, recognize `R_init_paradox`, and report exactly one
  checked call to `R_registerRoutines`.

`scripts/memory-check --source-run ID --mode gct|valgrind|rchk` implements
these gates. It has no default mode, accepts only a fully manifest-verified
snapshot retained below `.local/checks/ID`, and creates a fresh run containing
the copied source, any applicable source-archive hash, commands, isolated
homes/libraries, tool and image provenance, logs, and explicit scope records.
It never builds the instrumented R, installs its packages, or pulls auxiliary
images as a side effect; those are separate, auditable preparation steps.

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
performed both before and after the Valgrind gate while an exclusive shared
state lock prevents the preparation scripts from changing either tree. The
retained helper copies and receipts are included in the run artifacts.

Build, package installation, snapshot replay, and memory-check subprocesses
start from `env -i` with only repository-local homes, caches, libraries,
compilers, and tools restored. This prevents user profiles, R libraries,
compiler include paths, sanitizer preloads, `VALGRIND_OPTS`, or container
connection variables from weakening a gate. The rchk mode additionally proves
that its current Podman wrapper and configuration equal the static source run,
validates the installed Podman receipt and wrapper-managed tree before and
after the container, and retains those receipts with the result.
The ordinary R toolchain and complete dependency library also receive full-tree
receipts before checked code is built or loaded. They are reverified around
each selected memory mode and at completion. A final memory-run seal binds the
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
- Stateful test ALTREP classes vary `Length`, `Elt`, and materialization results
  between accesses and may run a closure that replaces a previously inspected
  parent cell before forcing collection. Crash-oriented cases run in isolated
  subprocesses under ASan and GC torture. They cover every native path that
  sizes then emits, retains a vector pointer, or extracts children across an
  allocating or reentrant boundary; ordinary compact ALTREP inputs separately
  verify compatible fallback or one-pass materialization.
- Any consumer failure that expresses a reusable contract first becomes a
  package regression test before the implementation is changed.

## Performance gates

Benchmarks compare the pinned upstream installation and candidate in separate
processes. They record revision, R/compiler versions, CPU governor, system load,
input sizes, warmups, iterations, elapsed distributions, allocations, and GC.
The shared host is noisy, so only paired relative results from an otherwise
idle window are accepted. Synthetic microbenchmarks guide local work; release
claims require workloads extracted from bbotk, mlr3tuning, mlr3pipelines,
mlr3mbo, miesmuschel, and the mlr3 website benchmark corpus.
