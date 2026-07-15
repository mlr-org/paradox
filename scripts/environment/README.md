# Native validation harness

Run the harness only after activating the repository-contained toolchain:

```sh
. scripts/activate
scripts/native-check --mode strict-gcc --mode strict-clang --tests focused
```

`scripts/native-check --help` lists the independent modes. There is no default
mode. `--mode static` runs the two strict compiler installs, a GCC `-fanalyzer`
package build, Clang 22's static analyzer over every C translation unit,
cppcheck, and the native registration/ELF audit. The Clang mode retains an
individual plist and command log for each source and fails on either textual
warnings or a nonempty diagnostics array. `--mode all` additionally runs ASan
and UBSan as two separate builds. `--tests full` includes tests normally
skipped on CRAN by setting `NOT_CRAN=true`; focused tests select files whose
names contain `characterization`, `native`, or `regression`.

Cppcheck uses its exhaustive analysis level on the actual Linux/C17 package
configuration. It deliberately does not force every imagined preprocessor
configuration in R's public headers: doing so invents self-referential values
for API macros such as `NORET` and produces header syntax errors unrelated to
any package build. The strict Linux builds and cross-platform CI cover the
real compiler configurations separately.

Each invocation creates a fresh `.local/checks/<run-id>/` and never reuses an
installed candidate. The input is the tracked plus non-ignored untracked state
of the current worktree. The snapshotter explicitly rejects `.git`, `.local`,
and `.cache`, preserves tracked deletions, hashes every regular file before and
after copying, and aborts if Git state or file membership changes during the
copy. The run retains the immutable source tree, manifest and its SHA-256,
HEAD/diff/status, source archive, compiler versions, replayable command log,
per-mode library, compiled DLL, analysis output, and result status. R startup,
temporary directories, caches, target libraries, and Makevars are all set to
project- or run-local paths. Commands do not consult user R startup files.

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

The strict profiles use C17 and turn a broad warning set into errors. They do
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
  an instrumented runtime would. A separately built sanitizer-enabled R is a
  distinct future validation gate.

The symbol mode requires dynamic lookup to be disabled, forced registered
symbols, a one-to-one mapping between `.Call` registrations and `C_*`
namespace bindings, no literal-string `.Call` in shipped R, and no dynamically
exported package symbols except `R_init_paradox`. On Linux it also rejects an
executable stack and text relocations and requires GNU RELRO metadata.

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

Header compilation cannot prove that fallback paths behave correctly inside
the actual R interpreter. The opt-in runtime matrix therefore provisions
exact conda environments for R 4.3.3 and R 4.5.2 from the SHA-256 explicit
locks in `environment/runtime-r-*-linux-64.lock`:

```sh
scripts/bootstrap-runtime-matrix
scripts/bootstrap-runtime-matrix --verify
scripts/environment/test-runtime-matrix
scripts/environment/test-runtime-matrix-installed all
```

The fast fixture checks shell/R syntax, lock shape, complete-tree tamper
detection, and outward-link refusal. The installed fixture starts hostile
clean shells and proves both activations repair injected R libraries, startup
files, compiler/linker/pkg-config inputs, caches, temporary paths, and XDG
runtime state. It also re-sources activation to prove idempotence. No matrix
command reads or mutates `.local/compat/R/library-dependencies` or either
consumer system prefix.

`scripts/test-runtime-matrix --help` describes the retained execution gate.
For each selected actual interpreter it archives a committed source ref,
builds and installs paradox into a fresh stage library, runs the focused
public-R-API facade probe and an authenticated supported source-test scope, and
audits undefined DSO symbols against that release's allowed accessor set. Both
old interpreters stage all public, characterization, regression, and compatible
native tests, while the 14 R-4.6-binding-admission implementation contexts in
`environment/runtime-matrix-pre46-exclusions.tsv` are explicitly retained as
excluded. The current inventory is 71 discovered files, 57 executed files,
and 14 exclusions, with a 4,900-expectation clean floor. The two ConfigSpace
files that stop at their absent-reticulate guard remain staged and are audited
separately through `environment/runtime-matrix-whole-file-skips.tsv`, including
the old file's preceding available `callr` guard; they are not silently treated
as executed result files. Every result-block skip title and reason is likewise
matched against `environment/runtime-matrix-result-skips.tsv` (six on R 4.3.3,
seven on R 4.5.2). The stage retains the exact scope ledger, staged source
copies, testthat-reported inventory, skip ledgers, counts, and hashes.
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
