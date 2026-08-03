# Paradox 2 portability and CI policy

This policy applies to the contract-first candidate. Historical workflow runs
for the superseded compatibility-first source are not accepted evidence.

## Supported baseline

- R >= 3.6;
- portable ISO C99 through the toolchain selected by R;
- Linux x86-64, Windows x86-64, and macOS Apple-silicon ARM64;
- data.table >= 1.18.4 as an outward interoperability dependency;
- public R C APIs available at the selected supported runtime, except for the
  exact versioned old-runtime raw-attribute, coherent closure-snapshot,
  stored-binding, and promise compatibility entries described below.

The source must not rely on GNU-only C behavior, x86 floating-point details,
unaligned access, little-endian layout, pointer ordering, `long` width,
architecture-specific vector instructions, or private data.table APIs. Use
`R_xlen_t`, checked conversions, R's NA/NaN predicates, and portable math.
Compiler extensions selected inside recent public R headers are external to
that source contract: `src/paradox.h` explicitly includes the public
`R_ext/Complex.h` required for `Rcomplex` and lexically isolates only R's
build-selected fixed-base enum and anonymous-structure diagnostics. The same
pedantic warnings remain errors for every Paradox declaration.

## Local supported-runtime matrix

Repository-local pinned prefixes exercise R 3.6.3, R 4.0.5, R 4.1.3,
R 4.2.3, R 4.3.3, R 4.4.3, and R 4.5.2
independently of the current R 4.6.1 prefix owned by the full native lane.
Pinned R 3.6.0, 4.0.0, and
4.2.0 headers are explicit compile/API axes, so support is not inferred only
from endpoint runtimes and every old-R preprocessor transition is compiled:

```sh
scripts/bootstrap-runtime-matrix
scripts/bootstrap-runtime-matrix --verify
scripts/test-runtime-matrix --runtime all \
  --source-ref refs/paradox-release/candidate-YYYYMMDDTHHMMSSZ \
  --run-id release-runtime-YYYYMMDDTHHMMSSZ
scripts/verify-runtime-matrix-evidence \
  --run-id release-runtime-YYYYMMDDTHHMMSSZ
```

The seven runtime stages use isolated libraries and caches and may overlap only when
the memory-aware resource report admits the workers. Nested make, testthat,
parallel/future, BLAS, and OpenMP pools remain one.
Each stage runs the complete main source suite with `NOT_CRAN=true`; exact
evidence rejects CRAN-policy skips while retaining genuine runtime-capability
skips.

Micromamba's ordinary hard-link mode is forbidden for these persistent
prefixes. Provisioning uses `--always-copy`, and the prefix receipt refuses
every regular file with a link count other than one. Content hashes alone are
insufficient when a writable extracted-package cache and several sealed
prefixes share an inode: a later in-place write can invalidate all of them
simultaneously. Existing prefixes from the former policy are not auto-deleted
or silently resealed; they must be deliberately reprovisioned from their exact
locks. The additional disk use buys an actual prefix immutability boundary.

The offline header compiler gate's schema-4 cache deliberately depends on the
already mandatory, fully receipted R 3.6.3 runtime prefix for its locked
libcurl 7.86 closure. R 3.6.0, 4.0.0, and 4.2.0 therefore execute their
unmodified major-7/minimum/header/link/HTTPS configure admission; no
`r_cv_have_curl*` result is seeded. A strict direct compile/link/run proof also
requires the selected header and runtime to be the same major-7 version with
HTTPS. The complete 12,899-member runtime-prefix receipt is verified before
every old-header cache lookup and again after a cache-miss build. Its helper,
manifest, receipt/seal, runtime lock, selected `curl-config`, effective flags,
probe source, and result are cache-key inputs. R 3.6.0 and 4.0.0 alone append
configure-only `-Wno-error=implicit-function-declaration`: their historical
bzip2 and curl HTTPS probes omit `<stdlib.h>` (and the bzip2 probe also omits
`<string.h>`), which GCC 14 otherwise rejects before those real test programs
can run. Package compilation retains the full strict-warning policy, and R
4.2.0 receives no warning adaptation.

R 3.6.3 through R 4.2.3 use exact conda runtime locks plus separately
authenticated source-package libraries from their
`environment/runtime-r-*-packages.lock` files. Those snapshot-compatible
closures include the package imports, test framework, and focused test support
without changing either prefix or host libraries. Their cache identities bind
only selected build inputs (runtime artifact fields, exact locks, authenticated
prefix content, and the explicit install schema); verification-tool provenance
is resealed without reinstalling the closure. The R 4.0.5 stage covers the
active-binding accessor introduced in R 4.0 while retaining the pre-R-4.2
optional-binding and element-setter branches. R 4.3 has no authenticated conda-forge
R-4.3 build of data.table 1.18.4. That stage therefore authenticates the source row in
`environment/r-packages-linux-64.lock`, copies the cached exact archive into its
retained inputs, installs it into the fresh stage library, verifies the resolved
version, and then builds Paradox. R 4.4.3 and R 4.5.2 already contain the exact
minimum.
This overlay never changes the host, persistent runtime prefix, or package
minimum.

Whenever R 3.6.3 is selected, it also consumes a separate
`declared-floor` profile from the same sealed source-library manager. Its exact
six-package lock is the five direct `DESCRIPTION` minima plus `digest` 0.6.39.
The candidate tarball is installed into a fresh candidate-only library against
that closure. One bounded smoke authenticates every installed package identity
and dependency namespace origin, plus the candidate Paradox DLL origin and
registration, and exercises representative imported paths. This does not
repeat the complete package suite. Its cache key, complete tree receipt, and
stage handoff make the floor proof reusable and source-bound. Runtime
activation removes an ambient `R_DEFAULT_PACKAGES` value so operator startup
policy cannot preload a reviewed dependency.

Only runtime bootstrap may provision this persistent floor closure. Bootstrap
provisions it together with R 3.6.3 and verifies it in read-only `--verify`
mode; the matrix coordinator is a receipt consumer and must not download,
build, repair, or rewrite the cache. Retained stages likewise replace the live
checkout's developer `Renviron`, `Rprofile`, and `Makevars` paths with the
detached authenticated copies admitted as trusted inputs before any build.
The binding includes `R_BUILD_ENVIRON`, `R_CHECK_ENVIRON`, and
`R_INSTALL_ENVIRON`; an inherited command-specific environment file may not
alter build, install, or check after source admission.

A complete seven-runtime selection additionally performs one sealed
R 4.0.5-to-R 3.6.3 serialization handoff after all stages pass. The producer
serializes a representative current-v2 BASE/COLLECTION/SHADOW graph; the
consumer loads, exercises, mutates, and reserializes it. Exact stage package
and DSO paths, shared topology, closure/attribute reachability, dormant and
named-`NULL` values, callbacks, transformations, fixture bytes, round-trip
bytes, logs, receipts, and stage seals are independently replayed. A partial
matrix records no cross-runtime claim.

Each stage must discover the current test inventory, run supported contract
tests, audit the DSO's undefined R symbols against the public API plus the exact
exception ledger, and seal source, build,
library, inputs, commands, logs, compiler, package, and session information.
One source-derived trusted-input manifest covers every helper, registry/policy
file, package lock, and prefix-repair lock; it is authenticated before and
after the complete worker wave and bound into top-level and per-stage evidence.
The declared-minimum package check uses `--no-tests`, because the separately
receipted complete-source stage already executes that exact suite once. Its
locked local test library deliberately omits exactly `reticulate`, `rmarkdown`,
`mlr3learners`, and `e1071`; therefore its sole admissible check exception is
child exit zero, one exact missing-Suggests dependency NOTE, no other
NOTE/WARNING/ERROR/halt, and one final `Status: 1 NOTE`.
R 3.6's Unix compiled-code checker predates the configured `NM` lookup and
calls the literal command `nm`. Only for this check, the stage prepends the
authenticated compiler-family target-tool directory already present in the
sealed prefix. It first verifies the installed alias's exact relative target
and resolved inode against the target-prefixed `NM`; no worker-global tool or
duplicated launcher is part of the contract.
Historical fixed file/skip/expectation counts are removed; a narrow exclusion
must name a current unsupported runtime capability and be independently
validated.

## R API discipline and non-forcing compatibility facade

Shipped C compiles against the pinned R 3.6.0, 4.0.0, 4.2.0, 4.3.0, 4.4.0,
4.5.2, and development headers. Version adapters live in
`src/r_api_compat.c` and may
select equivalent APIs, but may not select a different semantic engine. For
older supported headers, the facade contains one exact, ledgered `ATTRIB`
traversal through R 4.5, because no earlier public API enumerates raw stored
attributes without expanding compact `row.names`. Before R 4.5, one ledgered
`FORMALS`, one `R_ClosureExpr`, and one `CLOENV` accessor capture a coherent,
allocation-free closure generation for transformation callback admission and
recursive migration; the two historical function-like macros are called with
expansion suppressed. All three exceptions compile out at R 4.5. A directly
reached bytecode object alone uses the cold public
`as.function.default()`/`body()` bridge without execution on old R.
R 4.5 uses public `ANY_ATTRIB` for the common no-attribute query; R 4.6 uses
public `R_mapAttrib`, `R_getAttribCount`, and `R_hasAttrib` throughout. The
hot attribute branches do not evaluate R-level `attributes()` or call
data.table code.
Code also must not assume that an R API predicate has the same signed integer
typedef in every supported header. A predicate retained in C state is
normalized by an explicit truth comparison (for example, `Rf_isObject(x) !=
FALSE`); the old-header GCC/Clang matrix rejects implicit signedness
conversions.

The remaining post-3.6 API differences are public and local: fresh raw/complex
destinations use public vector access before the element-setter declarations
appear, and the documented default `identical()` flags are represented by
their old-header value before `IDENT_USE_CLOENV` is named. Collection parameter
reads pass their already admitted, rooted core directly to the shared params
loader, eliminating the temporary environment and any need for `R_NewEnv()`.
These adapters are inline or remove work; they do not slow the current path.
The old-Windows source contract also avoids MSVCRT-dependent `%lld`, `%I64`,
and `j`/`z`/`t` integer-length formats. Long-vector diagnostic positions are
safe to render through `%.0f` because `R_XLEN_T_MAX` is at most 2^52 and
therefore exactly representable as a double. The strict source/header gate
rejects flags, widths, precision, or positional variants of those formats
before compilation.

The graph walker authenticates an imports-environment boundary by two public
facts together: an ordinary scalar raw `name` attribute beginning `imports:`
and `R_BaseNamespace` as the direct parent. A display name alone is never a
boundary, because an ordinary user environment can spoof it. This avoids both
silently skipping user graphs and depending on R's internal environment
layout.

The only exceptions to the public-API rule are the exact versioned entries
needed for raw attribute and coherent old-R closure inspection and to inspect
a stored environment binding or already reached promise without forcing it.
Every occurrence is
centralized and count-ledgered. R 3.6--4.5 has no public non-forcing classifier for one
binding. The public R-level `substitute()` workaround is non-forcing but
insufficient for simultaneous generation/TuneToken receipt scans and recursive
object-graph migration: it returns a promise expression, not an unambiguous
binding-kind/generation receipt.
The runtime matrix therefore exercises the registered native plain-binding
classifier with realized and delayed literal logical, NULL, language, symbol,
environment, closure, and external-pointer values. In particular, a realized
language object or symbol must remain a direct value while a promise carrying
an expression of the same type remains delayed; expression/type shape must
never stand in for a binding-type query.
The pinned R 4.5 Writing R Extensions manual explicitly says that detailed
delayed-binding information is unavailable in the API. The R 4.6 manual labels
`R_GetBindingType` experimental and continues to classify
`Rf_findVarInFrame` as too low-level for the API; R 4.6 `tools` includes the
latter in its warned non-API symbols. Thus this exception is a reviewed
supported-runtime compromise, not a CRAN allowlist justification.
`R_getVar` is public beginning with R 4.5, but it can force a delayed binding
and is therefore not a sound replacement until R 4.6's classifier has first
proved that the cell is a direct or already-forced value. The R 3.6--4.5 DSO
inventories consequently forbid `R_getVar`. The current raw-token contract has
three reviewed call sites, all following `R_GetBindingType`; its DSO inventory
has one undefined-symbol row.

The printable simple-Domain renderer also has one explicit policy boundary.
R >= 4.5 uses the documented `Rf_GetOption1` to snapshot `scipen`; R 3.6--4.4
uses public `base::getOption()` through the compatibility facade instead of
calling that release's then-undocumented native entry point. Both paths feed
one bounded native renderer. The runtime inventories forbid `Rf_GetOption1`
through R 4.4 and require one occurrence from R 4.5 onward, authenticating the
compile-time selection without duplicating rendering logic.

R 3.6--4.1 also lacks the public `R_existsVarInFrame()`. Optional, cold
absence-tolerant lookup uses `base::exists(..., inherits = FALSE)`, whose
implementation does not invoke active bindings; mandatory admitted ordinary
bindings stay on the allocation-free classifier. On newer R, the ordinary
frame path remains allocation-free. Callers keep one conservative rooting
proof across all supported facade branches because hostile class metadata can
allocate during admission, and recognized callback-backed user databases are
rejected before binding inspection. The terminal optional receipt scan
cannot evaluate or allocate. It therefore uses the header-declared/exported
`R_HasFancyBindings()` only on R 3.6--4.1 to reject a fancy frame before
reading a stored cell. This is an exact old-runtime exception, not a general
environment-layout API. The facade first rejects
`UserDefinedDatabase` environments with the public inheritance predicate R
itself uses: their callback-backed object table is outside the supported
ordinary-frame boundary, and old `R_HasFancyBindings()` assumes a hash-vector
layout that they do not have.

Consequently, the R < 4.6 branch in `src/r_api_compat.c` calls the
header-declared/exported `Rf_findVarInFrame` once. Only R < 4.5 may inspect a
returned `PROMSXP` through the header-declared/exported `R_PromiseExpr`,
`PRENV`, and `PRVALUE`; it never forces the promise. R 4.5 compiled-code policy
classifies those accessors as non-API, so recursive migration fails closed on a
reached promise and requests R 4.0--4.4 or R >= 4.6. Ordinary callback
factories can retain formal promises in lexical call frames, even when a
formal is already forced or unused; this is therefore an exact R 4.5
recursive-migration limitation rather than merely a `delayedAssign()` edge
case. Ordinary ParamSet operations remain supported.
R >= 4.6 uses the documented experimental binding classifier plus delayed/
forced-binding expression/environment accessors and does not compile the
`Rf_findVarInFrame` path. The public dots API covers `...`. R 4.5 headers still
declare all three detached-promise accessors, but that release's compiled-code
policy classifies them as non-API; source gating and DSO inventories therefore
exclude them beginning with R 4.5. On R >= 4.6, a `PROMSXP` reached outside a
binding/dots cell is opaque.

R 3.6 exposes neither `R_ActiveBindingFunction()` nor an equivalent R
accessor. Direct and recursive legacy ParamSet-family migration fail closed
when active-binding inspection is required, with an instruction to load and
migrate the object under R >= 4.0. Paradox-1 ParamSet-family R6 shells
themselves contain active bindings, so practical ParamSet/Collection object and
graph migration requires R >= 4.0. Current Paradox-2 operations and idempotent
current-object conversion remain supported on R 3.6. Exact built-in current
shells also retain recursive traversal through their authenticated capsule;
their package active facades are opaque because unsupported in-place
replacement cannot be distinguished when exact shell receipts are preserved.
Such a binding is never invoked. Additive shells and modifications that fail
exact authentication instead fail closed. Standalone legacy Domain/Condition
conversion and graphs without arbitrary active bindings remain supported.

`environment/r-api-exceptions.tsv` is the exact ledger. It records each
exceptional symbol, source, raw-token count, version branch, and rationale; the
pinned-header and real-runtime matrices exercise both sides. These symbols are
not CRAN-allowlisted. R 3.6--4.1 inventories additionally contain the exact
old-only `R_HasFancyBindings` scan entry. R 3.6--4.4 DSO inventories contain
the ledgered old binding/promise set. R 4.5 retains only the old binding
selector and must exclude `R_getVar`, `R_PromiseExpr`, `PRENV`, and `PRVALUE`;
R >= 4.6 must instead exclude `Rf_findVarInFrame` and include the paired
`R_GetBindingType`/`R_getVar` entries selected by the crawler.
The
facade does not permit another internal API, caller, forced promise, or
different semantic engine.
The exact R 4.5.2 stage additionally invokes that runtime's own
`tools:::check_compiled_code()` on the installed package. Its retained,
manifest-bound receipt must report zero issues. This catches the policy list
actually communicated by R 4.5 rather than assuming that the explicit branch
inventory is a complete CRAN-policy model.
`native-check --mode symbols`, the old-runtime matrix, and the retained-evidence
validator derive their expected undefined-symbol inventories from that
versioned ledger; do not retain the superseded one-symbol hard-coded policy.

The registered routine table has fixed arities and dynamic lookup disabled.
Symbol audits reject too-new R APIs in older-runtime DSOs, any unledgered
internal R API, any binding/promise count/path/version-branch mismatch, and any
private data.table symbols. Direct probes exercise each registered routine on
valid and malformed inputs.

R did not expose VECSXP ALTREP classes before R 4.3. The package's adversarial
list-ALTREP fixture is compiled and run only where that facility exists;
atomic ALTREP fixtures still execute on R 3.6. Production list-ALTREP behavior
is not skipped on old R—the corresponding object kind cannot exist there, so
the semantic branch is vacuous.

## Floating-point portability

Comparisons, tolerances, quantiles, exponential transformations, and integer
conversion receive cross-architecture boundary tests. Do not require fused
multiply-add contraction or a particular intermediate rounding result. Avoid
tests that compare stochastic floating output bit-for-bit when the public
contract is tolerance-based, but keep exact tests for deterministic categorical
selection, integer bounds, NA/NaN/Inf classification, and RNG state restoration.

Apple ARM64 previously exposed an FMA-sensitive defect in different source.
Fresh candidate evidence must exercise the focused floating-point boundary
tests and real hardware CI. The historical correction is guidance, not
evidence.

## GitHub portability workflow

The release workflow checks the exact frozen candidate ref on at least:

- Windows release R, x86-64, with the matching Rtools;
- Windows R 3.6.3, x86-64, with exact Rtools35 GCC 4.9.3;
- macOS release R on a pinned Apple-silicon runner label;
- Linux release/development R as appropriate for package-check integration.

Every platform job records checked-out ref/commit/tree and verifies that
provenance before building. Shell and R steps use failure-propagating wrappers:
R errors, failed `R CMD check`, missing logs, or a terminal status outside the
one exact old-Windows exception below must terminate the job nonzero. The
matrix has `fail-fast: false`, so every required
platform runs. The exact old-Windows toolchain is a separate fail-fast job, so
its narrow source-build contract cannot be hidden inside or weakened by the
ordinary current-R matrix. A separate always-run completion job waits for both
the entire matrix and that old-Windows job and rejects either aggregate
`needs` result unless it is exactly `success`; the offline acceptance verifier
additionally enumerates and requires success from each individual REST job and
required step. The completion gate must not use an
R expression such as
`if (Sys.getenv(...) )` without strict logical parsing; the earlier false-green
`missing value where TRUE/FALSE needed` incident is a permanent harness
regression case.

CI uploads source/check artifacts and logs even on failure, but artifact upload
steps use `if: always()` without masking the preceding command's conclusion.
The acceptance verifier checks job conclusions, log terminal status, source
identity, artifact hashes, and required manifests rather than trusting the
workflow's top-level green mark alone.

The R 3.6.3/Rtools35 job installs no current-index binary or solved dependency
set. A reviewed PowerShell driver selects the seven-package runtime closure from
`environment/runtime-r-3.6.3-packages.lock`, downloads a locked primary or
fallback source URL, verifies every SHA-256 before use, and installs in explicit
dependency order into a fresh short-path library. Locked `digest` 0.6.39 is a
sentinel for the ordinary old-toolchain path: its authenticated source uses
C++11 constructs without declaring `CXX_STD`, while exact R 3.6.3 Windows
already defines default `CXX` as `g++ -std=gnu++11`. There is therefore no
dependency workaround. Before each of the toolchain, dependency-closure, and
candidate phases, one checked-in PowerShell helper clears the complete reviewed
R/compiler/make/package-flag environment, binds R's site/user environment,
profile, and Makevars inputs to six empty read-only files, and installs fresh
phase-specific home and temporary directories. It selects a fresh empty site
library and an explicit user library for the phase, while preserving
`R_KEEP_PKG_SOURCE=yes`. Build, check, and install command environments are
also pinned to the authenticated empty environment file.
Clearing means deleting each `Env:` item with `Remove-Item`, not assigning
`$null`: PowerShell's binder may coerce `$null` to an empty string, and current
.NET can retain that empty environment entry. The first hosted old-Windows
attempts exposed this deterministically when hostile `R_HOME` survived the
reset; those runs are harness failures and must not be retried at the old SHA.
`R_PKG_CXX_STD` is consequently absent throughout the
complete closure. The helper emits one read-only exact-policy receipt per
phase; all three are retained in the artifact and independently replayed, so
neither a user Makevars file nor a self-reported isolation banner can satisfy
the gate. Retained evidence also requires one distinct ordinary compiler
command containing exactly one
`-std=gnu++11` for each of digest's four reviewed translation units. It also
binds exact `C:/Rtools/mingw_64/bin/g++.exe` and exact first line
`g++.exe (x86_64-posix-seh, Built by MinGW-W64 project) 4.9.3`. The offline
verifier independently replays the evidence and raw install log. This proves
the unmodified R 3.6.3 default rather than creating a package-wide C++ mode;
Paradox retains its portable-C99 contract. The exact source basis is
`R-3.6.3.tar.gz`, SHA-256
`89302990d8e8add536e12125ec591d6951022cf8475861b3690bc8bf1cefaa8f`,
whose `src/gnuwin32/fixed/etc/Makeconf` supplies that default. The driver then
builds Paradox once,
installs and loads the resulting archive, requires strict registered native
routines with dynamic lookup disabled, authenticates the x86-64 PE DLL with
Rtools35 `objdump`, and exercises construction, dormant values, detached
data.table facades, native grid generation, serialization, and both returned
and thrown native formatted diagnostics. The latter is the real old-MSVCRT
probe for the failure-only `vsnprintf()` path; valid-only smoke cannot establish
that contract. Its bounded
runtime-import-only `R CMD check` deliberately uses `--no-tests --no-examples
--ignore-vignettes --no-manual`: the local authenticated R 3.6 stage owns complete
old-runtime behavior, while this hosted lane uniquely proves the old Windows
compiler/linker/loader ABI. R 3.6's `_R_CHECK_DEPENDS_ONLY_` changes execution
libraries but does not suppress its dependency-inventory NOTE. The driver also
sets the documented R 3.6 switch `_R_CHECK_RD_XREFS_=false`: the exact bounded
ABI closure intentionally omits the external `lhs` and `spacefillr`
documentation targets, while the complete local and current hosted checks own
cross-reference coverage. Therefore this
lane requires child exit zero and exactly one final `Status: 1 NOTE`, with the
package-dependency section naming exactly the nine unavailable direct Suggests
and no other NOTE, WARNING, ERROR, or halt. Current Windows and macOS still
require exact final `Status: OK`. A source-build, smoke, or bounded-status
failure is a release blocker; there is no binary fallback, dependency downgrade,
or continue-on-error path.

The exact hosted-R configuration was independently extracted from
`https://cran-archive.r-project.org/bin/windows/base/old/3.6.3/R-3.6.3-win.exe`
(size 86,429,120; SHA-256
`881ace1f1a7dd550845283a65f4597456877ef3c3e129e2c3cbb92a67be7892c`).
Its `etc/x64/Makeconf` has SHA-256
`22313a8b8be1e8fc85c7366dcb20c95280f3693f1fa74f4ecf48f69b20f56df8`
and exact `CXX = $(BINPREF)g++ -std=gnu++11 $(M_ARCH)`. An earlier audit
looked at the R 3.6.0 minimum-API source tree and invoked raw G++98 outside
`R CMD INSTALL`; that did not model this hosted runtime and its proposed
digest override was discarded. Do not reintroduce it.

The reviewed Rtools35 identity was reproduced on 2026-07-27. The canonical
[CRAN Rtools history](https://cran.r-project.org/bin/windows/Rtools/history.html)
identifies Rtools35 as the frozen R 3.3--3.6 toolchain; the pinned `setup-r`
action downloads
`https://cran.rstudio.com/bin/windows/Rtools/Rtools35.exe`. Those downloaded
bytes have size 108,622,512 and SHA-256
`18fd63bb9c903e1f9bfce5c9a2600bd24213295760592dbc130b73a61e9d9414`.
Extraction and independent hashing established the exact executable identities
required live and in retained evidence:

| executable | SHA-256 |
|---|---|
| `mingw_64/bin/gcc.exe` | `2d415b0fd5eacb43268e2ddf080b50f706d9fa2465b1e32d04f54ce936fac3da` |
| `mingw_64/bin/g++.exe` | `0d3d581bca702c777fc045a2fe69696e5979d86e819efe2350e2ac43f33f2b7f` |
| `mingw_64/bin/objdump.exe` | `cbf5f996ef759be73502387c9d1296176f8bb7b6320b63cfb61371f7a98e7b59` |
| `bin/make.exe` | `ce462e4ca812718a077ae4b67ebec0bd2df0e7a3bc1e31897e40895023e13c72` |

The embedded GCC package version is exactly
`x86_64-posix-seh, Built by MinGW-W64 project`, which owns the complete GCC/G++
first lines required above. Checking all four binary digests prevents a
pre-existing `C:\Rtools` with merely plausible paths and banners from passing.

## Windows-specific checks

- strict registration and DLL loading with no unresolved symbols;
- C99 compilation under the selected Rtools GCC;
- path, encoding, line-ending, temporary-directory, and file-lock behavior;
- no POSIX-only shipped code or shell dependency in package execution;
- serialization and data.table facade behavior;
- complete package check with examples/tests/vignettes relevant to Windows.

The last item belongs to the current-R Windows row. The Rtools35 row is the
separate compile/link/load/smoke and bounded-check proof described above; it
does not duplicate the complete old-R test suite.

## macOS ARM64-specific checks

- Apple Clang C99 warning-clean compile and link;
- ARM64 floating/quantile/tolerance boundary suite;
- alignment-safe object access and checked integer widths;
- no x86-only compiler flags or intrinsic assumptions;
- current serialization, callbacks, stable/base ALTREP materialization, hostile
  custom-ALTREP safety (no replay or Paradox-caused crash/corruption),
  direct-assignment rejection versus the sole `set_values(.values=)` shell
  snapshot, construction-time typed-special ALTREP materialization,
  operation-time live-table ALTREP rejection, pointer-S4 and ParamUty
  base-`identical()` boundaries, exact BASE Object-token receipts and sealed
  search capabilities,
  ordinary non-ALTREP/non-S4 structural shell rejection, documented tables with
  one shared strict suffix classifier, no prefix-induced ordinary-shell copy,
  ALTREP-snapshot canonicalization without dispatch, malformed
  class-vector rejection, suffix-classified top-level
  ALTREP shells, owned names
  under reentry, strict cache carriers, count-only ordinary/stable-ALTREP row
  names, zero-column Design semantics, semantic ALTREP columns, data.table
  facades, and package check;
- no temporary source-tree detritus that makes a check falsely dirty.

## Sanitizers and analyzers

The package-DSO ASan and UBSan profiles use the reviewed Clang toolchain and
remain separate; strict warning builds cover both GCC and Clang. GCT and
Valgrind focus on R object lifetime; rchk covers protection, allocation, and
registration statically. Long-vector/arithmetic and corrupt capsule tests are
platform-neutral requirements, not Linux-only analyzer extras.

No sanitizer flag or analyzer suppression may weaken ordinary semantics. Every
suppression is source- and tool-version-specific, minimal, documented, and
reviewed. Zero exit without parsed reports is insufficient.

The contained worker userland is also sanitizer input because the exact
repository-local R and Clang runtime are mounted into it. Before strict builds,
an ASan-selected native run must start that exact R with the production preload
and complete an XDR serialization round-trip; its exact success record is
completion-bound and independently replayed. A libc/interceptor mismatch fails
closed. It is corrected by changing the immutable worker image, not by a host
fallback, retry, disabled interceptor, or relaxed container isolation. The
July 2026 glibc 2.42 worker failure occurred in R lazy loading before Paradox
was loaded and therefore supplies neither positive nor negative package
evidence.

## Remote-write boundary

Agents may prepare workflow changes, local tags/refs only when explicitly
authorized, and PR-ready downstream commits. They do not push branches, publish
tags, dispatch remote workflows, or open PRs. The user performs each remote
write manually. After publication, agents may read and verify the resulting CI
run and artifacts.

The replacement-candidate renderer assumes every executable helper has already
converged in the immutable candidate. Its direct child changes exactly
`.github/workflows/r-cmd-check.yml`, while both hosted jobs still authenticate
the inherited old-Windows installer's exact `100644` tree entry. This keeps the
parent/diff proof minimal without weakening helper provenance.

The active immutable candidate is
`refs/paradox-release/candidate-20260803T131049Z`, commit
`a0a9ff3e05b535068392e0c20442ad9794f3b824`, tree
`49079e12a816542fe8d8d6a0a2290a757a558b41`. It includes the converged
installer, bounded R 3.6 Rd-xref policy, and portable old-GCC initializer. Its
local release-core, combined-memory, dual-axis compatibility, and mandatory
documentation gates pass at their stated boundaries.

The locally frozen direct child is
`refs/paradox-release/portability-harness-da5a500`, commit
`da5a500936d00f7bc4c44989258d5bc385082252`, tree
`ad699de13d8f2df1b08530db4cf3a6a08d3a7693`, with local tag
`paradox-2.0.0-ci-a0a9ff3-harness-da5a500`. Its sole parent is `a0a9ff3` and
its sole changed path is `.github/workflows/r-cmd-check.yml`; that rendered
workflow's SHA-256 is
`6e09fa7d068886c05c0d1643b49fe8f48cbd7dec49a7e1ee757c0649088ebb18`.
The inherited old-Windows installer remains exact `100644` Git blob
`3b420d542dc5a1ae5506380543159cdea84517fa`. General and release-mode
structural validation, deterministic renderer tests, helper-blob adversary,
offline evidence-verifier fixtures, documentation-economy checks, and
actionlint all pass locally. Hosted execution remains pending; the only
current manual publication and dispatch procedure is recorded in
`compat/downstream-pr-handoff.md`. Agents must not publish or dispatch it.

Hosted branch run `30807900755`, at remote head
`dfc1a2f8fbf52c569563b48842b2f45be480a5c9`, passed all current Linux,
Windows, and macOS jobs. Only exact R 3.6.3/Rtools35 job `91667437793` failed,
with the aggregate job failing consequently. That result predates the active
candidate's repair: R 3.6 now disables only the redundant Rd-cross-reference
check so the bounded closure retains exactly its intended missing-Suggests
NOTE, and the upgrade walker now uses the portable C99 scalar initializer.
Retrying the old branch or any superseded companion cannot execute those
repairs. No hosted result below transfers to `a0a9ff3`; only a fresh run whose
head is `da5a500936d00f7bc4c44989258d5bc385082252` can close hosted portability.

For now-superseded candidate
`f27776ee1eca5d964945aa53d14d0ec7947dccbf`, the first locally validated
direct-child companion was
`refs/paradox-release/portability-harness-198e838`, commit
`198e838566579806d6c3bd48e1327c293473257b`, tree
`0ff93580b05e1e37994d301922f77490b2a5bb81`. It changes only
`.github/workflows/r-cmd-check.yml`; rendered workflow SHA-256 is
`5a60156cb79e403dcc38fceeb075d223a4397dab4a74989b36a745ccadfebec0`.
Its exact candidate and companion tags are remote. Its local structural checks
passed, but its hosted result below makes it immutable failed-harness history.

Hosted run `30793059118` executed that exact `198e838` companion. Current
Windows x86-64 and macOS ARM64 passed, but the exact R 3.6.3 job failed in
toolchain preflight before building or loading Paradox. Pinned `setup-r` puts
`C:\R\bin` on `PATH`; R 3.6's top-level Windows `Rfe.exe` launcher reconstructs
a `cmd.exe` command, does not escape embedded double quotes, and cannot
faithfully relay the workflow's multiline `Rscript.exe -e` operand. R therefore
received an empty expression and stopped with `option '-e' requires a non-empty
argument`. The missing receipts, artifact, and aggregate failure are expected
consequences of that primary stop. This is deterministic harness evidence;
retrying `198e838` cannot test a repair.

The first direct-x86-64 correction was frozen under tooling `a2af703` and
companion `ff3b510`. Hosted run `30803703541` executed that exact companion.
Current Windows x86-64 and macOS ARM64 both passed their full package checks.
The old-Windows job `91654062556` stopped in toolchain preflight at `PATH does
not select exact R 3.6 x86-64 executables`. Both literal
`C:\R\bin\x64\R.exe` and `Rscript.exe` had passed the preceding file checks;
the retained log does not expose which `Get-Command` result differed. No R
process, compiler, package build, DLL load, or Paradox test ran. Provenance,
artifact, and aggregate failures are consequential. This is another
deterministic harness result; retrying unchanged `ff3b510` cannot test a repair.

The final old-Windows policy makes name discovery non-authoritative. Every
phase invokes the exact installed `C:\R\bin\x64` applications directly, and
the locked installer admits those two paths as mandatory parameters before it
does any work. Rtools executables are likewise addressed through their exact
authenticated paths and byte hashes. All workflow and installer R expressions
are fresh UTF-8-without-BOM files, executed by direct `Rscript.exe`, and removed
in `finally`; no multiline `-e` operand or PowerShell native-argument mode is
involved. The ordinary PATH still starts with the reviewed R/Rtools directories
for R's own child processes, while every harness launch remains explicit.
Structural tests inventory all such calls, reject name-based R/Rscript/Rtools
discovery and `-e`, bind construction/status/cleanup, and adversarially mutate
the architecture, provenance, direct-path, and helper-blob contracts. The full
helper and all four old-Windows workflow blocks also parse with zero errors
under official portable PowerShell 7.6.4.

Final validation for superseded payload `f27776e` is frozen at
`refs/paradox-release/portability-tooling-20260803T103512Z`, commit
`812e5abef05c86f743425f6d984fb146c2827434`, tree
`e99604f05c10db57406773383d2e0a73746a139c`. Its historical immutable companion is
`refs/paradox-release/portability-harness-582eba8`, commit
`582eba86e7a05428f63608272c1c6c6e11a894f4`, tree
`e5ba13f476b4997fea4dbaf5d60369a058ad024c`, with tag
`paradox-2.0.0-ci-f27776e-harness-582eba8`. It is the candidate's sole child
and changes exactly `.github/workflows/r-cmd-check.yml` and
`scripts/environment/install-hosted-r36-windows.ps1`. The candidate excludes
both top-level roots through `.Rbuildignore`, so the companion is
package-facing-source identical to `f27776e`. The workflow and helper SHA-256
values are respectively
`a7d55d3f3df1543753fbea37424dd3702d8c8de187cb213354f5580db4f35f44`
and
`283e3450e47337f3fc121d6e103c1c0a0ad66ab4cab63b8c42caa57ab174381b`;
the helper is exact `100644` Git blob
`3b420d542dc5a1ae5506380543159cdea84517fa`. Both jobs check out the companion
at depth two, prove its exact sole parent and two-path diff, and authenticate
that complete helper tree entry before execution. General/release validation,
deterministic rendering, helper-blob adversary, exact committed blobs,
CI-evidence fixtures, documentation economy, and actionlint pass.

Hosted run `30807910809` later proved that companion's current Windows/macOS
rows and exact old-Windows build/install/load/smoke path. Its old-Windows check
failed only because the bounded closure produced the expected missing-Suggests
NOTE plus a redundant Rd-cross-reference NOTE. `582eba8` is therefore also
immutable failed-harness history and must not be retried. The active replacement
is the `a0a9ff3`/`da5a500` candidate-companion pair above; obsolete `f27776e`,
`582eba8`, `198e838`, or `ff3b510` publication and dispatch commands must not be
executed.

## Acceptance

Portability is accepted only when the local R matrix, pinned-header/symbol and
exact exception-ledger audits, current Windows x86-64, exact
R 3.6.3/Rtools35 Windows x86-64, and real macOS ARM64 rows all name the same
candidate source and their independent verifiers pass. The release evidence
inventory is four successful REST jobs and three platform artifacts: current
Windows, old Windows, macOS, and the aggregate completion job. Any source change
affecting C, registration, R wrappers, tests, build configuration, or
portability harness reopens the corresponding rows.
