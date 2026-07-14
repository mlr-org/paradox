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
